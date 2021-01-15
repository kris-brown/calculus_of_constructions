from abc import ABCMeta, abstractmethod
import re
import copy
from dataclasses import dataclass, field
from itertools import chain, count
from lark import Lark, Tree, Token
from lark.reconstruct import Reconstructor

from typing import (Optional as O, List as L, Set as S, Tuple as T, Dict as D,
                    Any, Iterator as I, Union as U)


def Tok(name: str, kind: str = 'CNAME') -> Token:
    '''For some reason, typechecker thinks Token can't take a str'''
    return Token(kind, name)  # type: ignore


grammar = r"""
    ?start: term

    ?toplevel:
     | def
     | itype

    def : "def " CNAME " = " term

    itype : "Inductive " CNAME " " (typed " ")* ": " term " :=" icon* "."
    icon : "\n| " CNAME " : " term

    ?term:
     | sort
     | lam
     | pi
     | arr
     | app
     | match
     | var
     | fix

    sort :
     | "Set" -> set
     | "Prop" -> prop
     | "Type" -> type
     | "Type" INT -> typeint

    var : CNAME
    typed : "(" var ": " term ")"

    lam : "λ" (" "typed)+  " => " term
    pi : "(∀" (" " typed)+  ", " term ")"

    arr:  "(" term (" → " term)+ ")"

    app : "(" term (" " term)* ")"

    match : "match " term " as " CNAME " return " term " with" matchcase+ "."
    matchcase : "\n| " term " => " term


    fix : "fix " CNAME ": " term " :=" fixcase+ "."
    fixcase: "\n| " term " => " term
    %import common.INT
    %import common.CNAME
    WS: (" " | "\n")+
    %ignore WS


"""
general_parser = Lark(grammar, maybe_placeholders=False)
reconstructor = Reconstructor(general_parser)


class Term(metaclass=ABCMeta):
    '''
    Abstract syntax - actually a 'pseudo-term' b/c it represents
    terms, types, kinds, ...

    Terms can be:
    - Sort
    - Var
    - Abstraction
        - Lambda
        - Pi
    - Application
    '''

    def __str__(self) -> str:
        return re.sub(r'Type\s(\d)', r'Type\1', str(
            Reconstructor(general_parser).reconstruct(self.to_lark())))

    @abstractmethod
    def _eq(self, other: 'Term', varpairs: L[T[str, str]] = None) -> bool:
        '''Helper function for equality modulo alpha-equivalence'''
        raise NotImplementedError

    def __eq__(self, other: object) -> bool:
        '''Equality modulo alpha-equivalence'''
        assert isinstance(other, Term), (other, type(other))
        return self._eq(other)

    @abstractmethod
    def freevars(self, fv: S[str] = None) -> S[str]:
        raise NotImplementedError

    @abstractmethod
    def sub(self, var: str, term: 'Term') -> 'Term':
        '''Substitute variable for value'''
        raise NotImplementedError

    @abstractmethod
    def __iter__(self) -> I['Term']:
        raise NotImplementedError

    @classmethod
    def term_from_lark(cls, tree: Tree) -> 'Term':
        if tree.data == 'match':
            return Match.from_lark(tree)
        elif tree.data == 'app':
            return App.from_lark(tree)
        elif tree.data == 'var':
            return Var.from_lark(tree)
        elif tree.data == 'lam':
            return Lam.from_lark(tree)
        elif tree.data in ['prop', 'set', 'type', 'typeint']:
            return Sort.from_lark(tree)
        elif tree.data == 'pi':
            return Pi.from_lark(tree)
        elif tree.data == 'fix':
            return Fix.from_lark(tree)
        elif tree.data == 'arr':
            return arr_from_lark(tree)
        else:
            raise NotImplementedError(tree.data)

    @classmethod
    def parse(cls, s: str) -> 'Term':
        return cls.term_from_lark(general_parser.parse(s))

    @abstractmethod
    def to_lark(self) -> Tree:
        raise NotImplementedError

    @property
    def t(self) -> 'Term':
        '''Cast type to Term to help typechecker'''
        return self


@dataclass(eq=False, unsafe_hash=True, frozen=True)
class Sort(Term):
    sym: str

    def __post_init__(self) -> None:
        assert self.sym in [
            'Prop', 'Set'] or self.sym.isdigit() and int(
                self.sym) > 0, self.sym

    def _eq(self, other: Term, _: Any = None) -> bool:
        return isinstance(other, Sort) and self.sym == other.sym

    def freevars(self, fv: S[str] = None) -> S[str]:
        return fv or set()

    def sub(self, var: str, term: Term) -> Term:
        return self

    def __iter__(self) -> I[Term]:
        return iter([self])

    def to_lark(self) -> Tree:
        if self == Set:
            return Tree('set', [])
        elif self == Prop:
            return Tree('prop', [])
        elif self == Type:
            return Tree('type', [])
        else:
            return Tree('typeint', [Tok(self.sym, "INT")])

    @classmethod
    def from_lark(cls, t: Tree) -> 'Sort':
        if t.data == 'prop':
            return Prop
        elif t.data == 'set':
            return Set
        elif t.data == 'type':
            assert not t.children
            return Type
        elif t.data == 'typeint':
            x, = t.children
            assert isinstance(x, Token), t
            return Sort(x.value)
        else:
            raise ValueError(t.data)


Set = Sort('Set')
Prop = Sort('Prop')  # Impredicative
Type = Sort('1')


def lookup(key: str, dic: L[T[str, str]]) -> O[str]:
    for k, v in dic:
        if key == k:
            return v
    return None


@dataclass(eq=False, unsafe_hash=True, frozen=True)
class Var(Term):
    sym: str

    def _eq(self, other: 'Term', varpairs: L[T[str, str]] = None) -> bool:
        vp = varpairs or []
        if not isinstance(other, Var):
            return False
        else:
            maybe = lookup(self.sym, vp)
            if isinstance(maybe, str):
                return maybe == other.sym
            else:
                maybe = lookup(other.sym, vp)
                if isinstance(maybe, str):
                    return False
                else:
                    return self.sym == other.sym

    def freevars(self, fv: S[str] = None) -> S[str]:
        return set([] if self.sym in (fv or set()) else [self.sym])

    def sub(self, var: str, term: Term) -> Term:
        return term if self.sym == var else self

    def __iter__(self) -> I[Term]:
        return iter([self])

    def to_lark(self) -> Tree:
        return Tree('var', [Tok(self.sym)])

    @classmethod
    def from_lark(cls, t: Tree) -> 'Var':
        assert t.data == 'var'
        child, = t.children
        assert isinstance(child, Token)
        return Var(child.value)


def arg_tree(v: str, t: Term) -> Tree:
    '''Make an argument for abstractions'''
    return Tree('typed', [Tree('var', [Tok(v)]), t.to_lark()])


def arg_untree(tree: Tree, data: str = 'typed') -> T[str, Term]:
    '''Inverse to arg_tree'''
    assert tree.data == data
    k, v = tree.children
    assert isinstance(k, Tree)
    assert isinstance(v, Tree)
    return Var.from_lark(k).sym, Term.term_from_lark(v)


class Abs(Term, metaclass=ABCMeta):
    '''Abstraction - common structure of lambda and Pi'''
    sym: str
    t1: Term
    t2: Term

    def _eq(self, other: 'Term', varpairs: L[T[str, str]] = None) -> bool:
        '''Critical step for equality modulo alpha-equivalence'''
        vp = varpairs or []
        if isinstance(other, type(self)):
            # The types of the bound variables have to agree
            if self.t1._eq(other.t1, vp):
                # If bound varname is the same, then the return val must be too
                # otherwise, add var1 -> v2ar to our list of equivalences
                vp_ = vp if self.sym == other.sym else [(self.sym, other.sym)]
                return self.t2._eq(other.t2, vp_ + vp)
        return False

    def freevars(self, fv: S[str] = None) -> S[str]:
        fv_ = fv or set()
        s2 = self.t2.freevars(fv_)
        s2.discard(self.sym)  # self.sym is BOUND
        return set.union(self.t1.freevars(fv_), s2)

    def sub(self, var: str, term: Term) -> Term:
        return self if self.sym == var else type(self)(  # type: ignore
            self.sym, self.t1.sub(var, term), self.t2.sub(var, term))

    def __iter__(self) -> I[Term]:
        return chain(iter([self]), iter(self.t1), iter(self.t2))


@dataclass(frozen=True, unsafe_hash=True, eq=False)
class Lam(Abs):
    sym: str
    t1: Term
    t2: Term

    def to_lark(self) -> Tree:
        args, ret = self.get_args()
        args_: L[U[str, Tree]] = [
            arg_tree(v, t) if True else '' for v, t in args]
        return Tree('lam', args_ + [ret.to_lark()])

    def get_args(self) -> T[L[T[str, Term]], Term]:
        if isinstance(self.t2, Lam):
            a, r = self.t2.get_args()
            return [(self.sym, self.t1)] + a, r
        else:
            return [(self.sym, self.t1)], self.t2

    @classmethod
    def from_lark(cls, tree: Tree) -> 'Lam':
        assert tree.data == 'lam'
        args = []
        for child in tree.children[:-1]:
            assert isinstance(child, Tree)
            args.append(arg_untree(child))
        last = tree.children[-1]
        assert isinstance(last, Tree)
        ret = Lams(Term.term_from_lark(last), **dict(args))
        assert isinstance(ret, Lam)
        return ret


def Lams(_lastone_: Term, **args: Term) -> Term:
    '''
    Construct a (nested) Lambda term with multiple arguments:
        e.g. `λ (a: A) (b: B), tail`
    '''
    res = _lastone_
    for var, arg in reversed(list(args.items())):
        res = Lam(var, arg, res)
    return res


@dataclass(eq=False, unsafe_hash=True, frozen=True)
class Pi(Abs):
    sym: str
    t1: Term
    t2: Term

    def args(self) -> T[L[T[str, Term]], Term]:
        '''
        Factor repeated Pis while unconcerned if arguments are used
        '''
        if isinstance(self.t2, Pi):
            a, r = self.t2.args()
            return [(self.sym, self.t1)] + a, r
        else:
            return [(self.sym, self.t1)], self.t2

    def get_args(self, prev_dep: bool = None) -> T[L[T[str, Term]], Term]:
        '''
        Factor repeated Pis while concerned to distinguish arrows from pis
        '''
        is_dep = Var(self.sym) in self.t2
        prev_dep_ = is_dep if prev_dep is None else prev_dep
        if is_dep == prev_dep_ and isinstance(self.t2, Pi):
            a, r = self.t2.get_args(is_dep)
            return [(self.sym, self.t1)] + a, r
        else:
            return [], self

    def to_lark(self) -> Tree:
        is_dep = Var(self.sym) in self.t2
        args, r = self.get_args()
        if not args:
            args, r = [(self.sym, self.t1)], self.t2
        if is_dep:
            return Tree('pi', [
                *[arg_tree(v, t) for v, t in args], r.to_lark()])
        else:
            return Tree('arr', [*[t.to_lark() for _, t in args], r.to_lark()])

    @classmethod
    def from_lark(cls, tree: Tree) -> 'Pi':
        assert tree.data == 'pi'
        args = [arg_untree(x) for x in tree.children[:-1]]  # type: ignore
        t2 = tree.children[-1]
        assert isinstance(t2, Tree)
        ret = Pis(Term.term_from_lark(t2), **dict(args))
        assert isinstance(ret, cls)
        return ret


def Pis(_lastone_: Term, **args: Term) -> Term:
    '''
    Construct a (nested) Pi term with multiple arguments:
        e.g. `Π (a: A) (b: B), tail`
    '''
    res = _lastone_
    for var, arg in reversed(list(args.items())):
        res = Pi(var, arg, res)
    return res


def Fun(t1: Term, t2: Term) -> Pi:
    '''Create a type `t1 → t2`'''
    for i in count():
        if Var('_%d' % i) not in t2:
            return Pi('_%d' % i, t1, t2)
    raise ValueError()


def Funs(*args: Term) -> Term:
    '''Create a type `A → B → C → D` with `Funs(A,B,C,D)`'''
    assert args
    res = args[-1]
    for arg in reversed(args[:-1]):
        res = Fun(arg, res)
    return res


def arr_from_lark(tree: Tree) -> Term:
    args = [Term.term_from_lark(x) for x in tree.children]  # type: ignore
    return Funs(*args)


@dataclass(eq=False, unsafe_hash=True, frozen=True)
class App(Term):
    t1: Term
    t2: Term

    def _eq(self, other: Term, varpairs: L[T[str, str]] = None) -> bool:
        if isinstance(other, App):
            if self.t1._eq(other.t1, varpairs):
                if self.t2._eq(other.t2, varpairs):
                    return True
        return False

    def __str__(self) -> str:
        return '({} {})'.format(self.t1, self.t2)

    def __iter__(self) -> I[Term]:
        return chain(iter([self]), iter(self.t1), iter(self.t2))

    def freevars(self, fv: S[str] = None) -> S[str]:
        fv_ = fv or set()
        return set.union(self.t1.freevars(fv_), self.t2.freevars(fv_))

    def sub(self, var: str, term: Term) -> Term:
        return App(self.t1.sub(var, term), self.t2.sub(var, term))

    def args(self) -> L[Term]:
        '''
        For a term with multiple args applied to it,
        get the original term being applied to,
        e.g. `(f(g))(h)` -> `(f, [g, h])`
        '''
        if isinstance(self.t1, App):
            return self.t1.args() + [self.t2]
        else:
            return [self.t1, self.t2]

    def to_lark(self) -> Tree:
        return Tree('app', [x.to_lark() for x in self.args()])

    @classmethod
    def from_lark(cls, tree: Tree) -> 'App':
        args = [Term.term_from_lark(x) for x in tree.children]  # type: ignore
        ret = Apps(*args)
        assert isinstance(ret, App)
        return ret


def Apps(*args: Term) -> Term:
    '''From `f` and `[g,h,i]` get `((f(g))(h))(i)`'''
    res = args[0]
    if len(args) > 1:
        for arg in args[1:]:
            res = App(res, arg)
    return res


@dataclass(eq=False, unsafe_hash=True, frozen=True)
class LetIn(Term):
    '''
    A `Let x=X:t1, y=Y:t2 in Z` expression.

    Derivable from applications and lambdas but convenient to have.

    {x+y | x: 0::Int, y: 1::Int} = {y+x | x: 1::Int, y: 0::Int}
    = {x+y | y: 0::Int, x: 1::Int} = {z | z: x+y :: Int}
    '''
    subst: T[T[str, Term, Term], ...]
    sub_into: Term

    def desugar(self) -> Term:
        res = self.sub_into
        for k, v, _ in self.subst:
            res = res.sub(k, v)
        return res

    def _eq(self, other: Term, varpairs: L[T[str, str]] = None) -> bool:
        return self.desugar()._eq(other, varpairs)

    def freevars(self, fv: S[str] = None) -> S[str]:
        return self.desugar().freevars(fv)

    def sub(self, var: str, term: Term) -> Term:
        return self.desugar().sub(var, term)

    def __iter__(self) -> I[Term]:
        return iter(self.desugar())

    def to_lark(self) -> Tree:
        raise NotImplementedError

    @classmethod
    def from_lark(cls, t: Tree) -> 'LetIn':
        raise NotImplementedError


def letin(t: Term, **subst: T[Term, Term]) -> LetIn:
    return LetIn(tuple([(k, v[0], v[1]) for k, v in subst.items()]), t)


def subs(t: Term, **subst: Term) -> Term:
    res = copy.deepcopy(t)
    for k, v in subst.items():
        res = t.sub(k, v)
    return res


@ dataclass(frozen=True)
class IType:
    name: str
    arity: Term
    pars: T[T[str, Term], ...] = field(default_factory=tuple)

    def __str__(self) -> str:
        return self.name

    @ classmethod
    def mk(cls, name: str, arity: Term, pars: D[str, Term]) -> 'IType':
        return cls(name, arity, tuple(pars.items()))


@ dataclass(frozen=True)
class ITypeDecl:
    name: str
    arity: Term
    pars: D[str, Term] = field(default_factory=dict)
    decls: D[str, Term] = field(default_factory=dict)

    @classmethod
    def from_lark(cls, tree: Tree) -> 'ITypeDecl':
        assert tree.data == 'itype'
        pars: D[str, Term] = {}
        decls: D[str, Term] = {}
        name = tree.children[0]
        assert isinstance(name, Token)
        i = 0
        for i, child in enumerate(tree.children[1:-1]):
            assert isinstance(child, Tree)
            if child.data == 'typed':
                k, v = arg_untree(child)
                pars[k] = v
            else:
                break

        arity = tree.children[i + 1]
        assert isinstance(arity, Tree)

        for icon in tree.children[i + 2:]:
            assert isinstance(icon, Tree)
            assert icon.data == 'icon'
            iname, ival = icon.children
            assert isinstance(iname, Token)
            assert isinstance(ival, Tree)
            decls[iname.value] = Term.term_from_lark(ival)

        return ITypeDecl(name.value, Term.term_from_lark(arity),
                         pars, decls)

    def to_lark(self) -> Tree:
        '''
        Some inductive types depend on other inductive types
        '''
        icontrees = [Tree('icon', [Tok(cd), v.to_lark()])
                     for cd, v in self.decls.items()]
        # for condecl, condecl_Term in self.decls.items():
        #     condeclargs = list(condecl.args.items())
        #     pars = []

        #     for i, (k, v) in enumerate(condeclargs):
        #         is_dep = k in set.union(
        #             it_term.freevars(), *[
        #                 a[1].freevars() for a in condeclargs[i:]],
        #             *[cd.freevars() for cd in condecl.indices])
        #         pars.append(arg_tree(k, v) if is_dep else v.to_lark())

        #     icontrees.append(
        #         Tok(condecl.name),
        #         condecl_term.to_lark()]))

        return Tree('itype', [Tok(self.name), *[
            arg_tree(v, t) for v, t in self.pars.items()],
            self.arity.to_lark(), *icontrees])


@ dataclass(frozen=True)
class ICon:
    itype: str
    name: str
    args: T[T[str, Term], ...]
    res: Term  # resulting term type

    def __str__(self) -> str:
        return '%s.%s' % (self.itype, self.name)


# @dataclass(frozen=True)
# class MatchCase:
#     cons: str
#     ret: Term
#     args: T[Term, ...] = field(default_factory=tuple)

#     def __iter__(self) -> I[Term]:
#         return iter([self.ret, *self.args])

#     def term(self) -> Term:
#         return Apps(Var(self.cons), *self.args)

#     @classmethod
#     def from_lark(cls, tree: U[str, Tree]) -> 'MatchCase':
#         assert isinstance(tree, Tree)
#         assert tree.data == 'matchcase'
#         pat, ret = tree.children
#         assert isinstance(pat, Tree)
#         assert isinstance(ret, Tree)
#         if pat.data == 'app':
#             app = App.from_lark(pat)
#             appargs = app.args()
#             cons_, args = appargs[0], tuple(appargs[1:])
#             assert isinstance(cons_, Var)
#             cons = cons_.sym
#         elif pat.data == 'var':
#             cons, args = Var.from_lark(pat).sym, ()
#         else:
#             raise ValueError()
#         return MatchCase(cons, Term.term_from_lark(ret), args)

#     def sub(self, v: str, t: Term) -> 'MatchCase':
#         return MatchCase(self.cons, self.ret.sub(v, t),
#                          tuple(a.sub(v, t) for a in self.args))

#     def freevars(self, fv: S[str] = None) -> S[str]:
#         return set.union(self.ret.freevars(fv),
#                          *[a.freevars(fv) for a in self.args])


@dataclass(eq=False, unsafe_hash=True, frozen=True)
class Match(Term):
    '''
    ```
    match m as x return (P x) with
    | (c1 x11 ... ) => f1
    | ...
    | (cn xn1...) => fn
    end
    ```

    The return type may be different in different branches, so we
    bind the input to a variable (`x`, above) and then parameterize
    the return type by that variable (it could go unused).


    For type checking, there are some concerns:
     - one way to make sure that the match expression completely covers
       all possible inputs is to require syntactically that each constructor
       is used exactly once (with completely general arguments). This is
       very inconvenient to write what we want in practice
     - There are algorithms for improving the type checking with partial
       covering (auto-proving that the uncovered cases are inaccessible):
       https://www.asc.ohio-state.edu/pollard.4/type/readings/new-elim-cic.pdf

    We'll err on the side of convenience over caution right now, so no
    restrictions are made on the match conditions
    '''
    term: Term
    sym: str
    ret: Term
    cases: T[T[Term, Term], ...]

    def _eq(self, other: 'Term', _: L[T[str, str]] = None) -> bool:
        if isinstance(other, Match):
            return self.ret == other.ret and self.cases == other.cases
        return False

    def sub(self, v: str, t: Term) -> Term:
        ret = self.ret if v == self.sym else self.ret.sub(v, t)
        cases = [(c1, c2) if v == self.sym else (c1.sub(v, t), c2.sub(v, t))
                 for (c1, c2) in self.cases]
        return Match(self.term.sub(v, t), self.sym, ret, tuple(cases))

    def freevars(self, fv: S[str] = None) -> S[str]:
        return set.union(self.term.freevars(fv), self.ret.freevars(fv),
                         *[set.union(c1.freevars(fv), c2.freevars(fv))
                           for c1, c2 in self.cases])

    def __iter__(self) -> I[Term]:
        return chain(iter([self, self.ret]), *[
            chain(iter(c1), iter(c2)) for c1, c2 in self.cases])

    def to_lark(self) -> Tree:
        cs = [Tree('matchcase', [c1.to_lark(), c2.to_lark()])
              for c1, c2 in self.cases]
        v = Tok(self.sym)
        return Tree('match', [self.term.to_lark(), v, self.ret.to_lark(), *cs])

    @classmethod
    def from_lark(cls, tree: Tree) -> 'Match':
        assert tree.data == 'match'
        assert isinstance(tree.children[0], Tree)
        assert isinstance(tree.children[1], Token)
        assert isinstance(tree.children[2], Tree)
        cases = []
        for child in tree.children[3:]:
            assert isinstance(child, Tree)
            assert child.data == 'matchcase'
            c1, c2 = child.children
            assert isinstance(c1, Tree) and isinstance(c2, Tree)
            cases.append((Term.term_from_lark(
                c1), Term.term_from_lark(c2)))
        return Match(Term.term_from_lark(tree.children[0]),
                     tree.children[1].value,
                     Term.term_from_lark(tree.children[2]), tuple(cases))


# @dataclass(unsafe_hash=True, frozen=True)
# class FixCase:
#     ret: Term
#     arg: Term

#     def __iter__(self) -> I[Term]:
#         return chain(iter([self.ret]), iter(self.args))

#     def sub(self, v: str, t: Term) -> 'FixCase':
#         return FixCase(self.ret.sub(v, t),
#                        tuple([a.sub(v, t) for a in self.args]))

#     def freevars(self, fv: S[str] = None) -> S[str]:
#         return set.union(self.ret.freevars(fv),
#                          *[a.freevars(fv) for a in self.args])

#     def to_lark(self) -> Tree:
#         return Tree('fixcase', [*[a.to_lark() for a in self.args],
#                                 self.ret.to_lark()])

#     @classmethod
#  def from_lark(cls, tree: Tree) -> 'FixCase':
#  assert tree.data == 'fixcase'
#  terms = [Term.term_from_lark(x) for x in tree.children]  # type: ignore
#  return FixCase(terms[-1], tuple(terms[:-1]))


@dataclass(eq=False, unsafe_hash=True, frozen=True)
class Fix(Term):
    '''
    Each case needs to identify an argument that is structurally decreasing
    '''
    name: str
    typ: Term
    cases: T[T[Term, Term], ...]

    def _eq(self, other: 'Term', _: L[T[str, str]] = None) -> bool:
        if isinstance(other, Fix):
            return all([self.name == other.name, self.typ == other.typ,
                        self.cases == other.cases])
        return False

    def sub(self, v: str, t: Term) -> Term:
        cases = [(c1.sub(v, t), c2.sub(v, t)) for c1, c2 in self.cases]
        return Fix(self.name, self.typ.sub(v, t), tuple(cases))

    def freevars(self, fv: S[str] = None) -> S[str]:
        return set.union(self.typ.freevars(fv),
                         *[set.union(c1.freevars(fv), c2.freevars(fv))
                           for c1, c2 in self.cases])

    def __iter__(self) -> I[Term]:
        return chain(iter([self, self.typ]), *[
            iter(case) for case in self.cases])

    def to_lark(self) -> Tree:
        return Tree('fix', [Tok(self.name), self.typ.to_lark(),
                            *[Tree('fixcase', [c1.to_lark(), c2.to_lark()])
                              for c1, c2 in self.cases]])

    @classmethod
    def from_lark(cls, tree: Tree) -> 'Fix':
        assert tree.data == 'fix'
        assert isinstance(tree.children[0], Token)
        assert isinstance(tree.children[1], Tree)
        cases = []
        for x in tree.children[2:]:
            assert isinstance(x, Tree)
            assert x.data == 'fixcase'
            c1, c2 = x.children
            assert isinstance(c1, Tree) and isinstance(c2, Tree)
            cases.append((Term.term_from_lark(c1), Term.term_from_lark(c2)))
        return Fix(tree.children[0].value, Term.term_from_lark(
            tree.children[1]), tuple(cases))
