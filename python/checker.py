from typing import Dict as D, List as L, Iterator as I, Optional as O, Set as S
import re
import copy
from dataclasses import dataclass, field
from itertools import chain

from lark import Lark, Tree, Token
from lark.reconstruct import Reconstructor

from term import (Term, Sort, Var, Match, App, Lam,
                  Pis, Apps, Pi, LetIn, Prop, Set, general_parser,
                  reconstructor, Type, subs, Fix, ITypeDecl, letin)

# Helper functions


def merge(d1: D[str, Term], d2: D[str, Term]) -> O[D[str, Term]]:
    '''
    Merge dictionaries (with error being None) and give error
    value when there is a conflict
    '''
    res = copy.deepcopy(d1)
    for k, v in d2.items():
        if k in res:
            if res[k] != v:
                return None
        else:
            res[k] = v
    return res


def rm_ws(t: Tree) -> None:
    '''Remove WS from parse tree'''
    rm = []
    for i, c in enumerate(t.children):
        if isinstance(c, Token) and c.type == 'WS':
            rm.append(i)
    for i in reversed(rm):
        del t.children[i]
    for c in t.children:
        if isinstance(c, Tree):
            rm_ws(c)

# Main class


@dataclass(order=True)
class TypeChecker:
    # Store the decls
    idecls: D[str, ITypeDecl] = field(default_factory=dict)
    # Compute the type of each itype and constructor+destructor
    itypes: D[str, Term] = field(default_factory=dict)
    icons: D[str, Term] = field(default_factory=dict)

    # Standard recursor
    irecs: D[str, Term] = field(default_factory=dict)

    # Match type to its constructors
    # Default context of definitions
    defs: D[str, Term] = field(default_factory=dict)

    def cons(self, it: str) -> L[str]:
        return list(self.idecls[it].decls)

    def __iter__(self) -> I[str]:
        return iter(chain(self.itypes.keys(), self.icons.keys(),
                          self.defs.keys()))

    def __getitem__(self, key: str) -> Term:
        if key in self.itypes:
            return self.itypes[key]
        elif key in self.icons:
            return self.icons[key]
        elif key in self.defs:
            return self.defs[key]
        else:
            raise KeyError(key)

    def add_type(self, it: ITypeDecl) -> None:
        assert it.name not in self
        # the type of the constructor
        self.idecls[it.name] = it
        self.itypes[it.name] = Pis(it.arity, **dict(it.pars))
        for icon, iconterm in it.decls.items():
            self.icons[icon] = Pis(iconterm, **dict(it.pars))

        if False:
            self.irecs[it.name] = self._make_recursor(it)

    def _make_recursor(self, it: ITypeDecl) -> None:
        raise NotImplementedError

    def add_def(self, k: str, v: Term) -> None:
        assert not isinstance(v, Var), v
        if k in self.defs:
            raise ValueError("Already defined")
        else:
            self.defs[k] = v

    def showdecl(self, it: str) -> str:
        decl = self.idecls[it].to_lark()
        newparser = Lark(general_parser.source_grammar.replace(
            "?start: term", "?start: itype"),)
        return str(Reconstructor(newparser, term_subs={r'Type (\d)': r'Type\1'}
                                 ).reconstruct(decl))

    def show_def(self, k: str) -> str:
        return 'def {} = {}'.format(k, self.show(self.defs[k]))

    def show(self, t: Term) -> str:
        tree = t.to_lark()
        return re.sub(r'Type\s(\d)', r'Type\1',
                      str(reconstructor.reconstruct(tree)))

    def positive_check(self) -> None:
        '''
        Check every argument to every inductive type constructor
        '''
        def _positive_check(it: Var, t: Term) -> None:
            '''
            Recurse through the term structure. If 'neg' is true, then we have
            entered a negative region and any observation of the inductive
            type is forbidden
            '''
            if isinstance(t, Pi):
                assert it not in t.t1, (it, t)
                _positive_check(it, t.t2)
            elif isinstance(t, App):
                _positive_check(it, t.t1)
                _positive_check(it, t.t2)
                args = t.args()
                for arg in args[1:]:
                    assert it not in arg
            elif isinstance(t, Lam):
                _positive_check(it, t.t1)
                _positive_check(it, t.t2)

        for itn, it in self.idecls.items():
            for decl in it.decls.values():
                if isinstance(decl, Pi):
                    args, ret = decl.args()
                    ctx: D[str, Term] = {}
                    for k, v in args:
                        v = subs(v, **ctx)
                        _positive_check(Var(it.name), v)
                        ctx[k] = v

    def judge(self, t: Term, ctx: D[str, Term] = None) -> Term:
        ctx_ = ctx or {}
        if isinstance(t, Sort):
            if t.sym in ['Prop', 'Set']:
                return Type
            else:
                return Sort(str(int(t.sym) + 1))
        elif isinstance(t, Var):
            if t.sym in self.defs:
                return self.judge(self.defs[t.sym])
            # check all three possible sources of name -> type info
            res = ctx_.get(t.sym, self.itypes.get(
                t.sym, self.icons.get(t.sym)))
            if isinstance(res, Term):
                return res
            else:
                err = '{} not found in \n\titypes: {}\n\ticons:{}\n\tctx:{}'
                raise KeyError(err.format(*[
                    x.keys() for x in [self.itypes, self.icons, ctx_]]))
        elif isinstance(t, Fix):
            # todo: structural termination checking
            # todo: make sure each case has a type that matches self.typ
            return t.typ
        elif isinstance(t, Lam):
            return Pi(t.sym, t.t1, self.judge(t.t2, {**ctx_, t.sym: t.t1}))
        elif isinstance(t, Pi):
            assert isinstance(t.t1, Sort), '%s must be sort' % repr(t.t1)
            t2 = self.judge(t.t2, {**ctx_, t.sym: t.t1})
            assert isinstance(t2, Sort), t2
            # breakpoint()
            rules = {(Prop, Prop): Prop,  # implication
                     (Set, t2): t2,  # assume t2 is type: 1st order predicates
                     (Set, Set): Set,  # functions
                     (Set, Prop): Prop,  # quantification over sets
                     }
            res = rules.get((t.t1, t2))
            if res:
                return res
            else:
                if t.t1.sym.isdigit:
                    if t2.sym.isdigit:
                        # higher order function
                        return Sort(str(max(int(t.t1.sym), int(t2.sym))))
                    else:
                        err = "Predicative calculus of Inductive constructions"
                        err += "does not allow (Type, Set)"
                        assert t2 != Set, err
                        assert t2 == Prop  # quantification over predicates
                        return Prop
                else:
                    # impredicative rule: (Type_i, Prop)
                    assert t2 == Prop
                    return Prop
        elif isinstance(t, App):
            t1 = self.beta(t.t1)
            assert isinstance(t1, Pi), t1
            return t1.t2.sub(t1.sym, t.t2)
        elif isinstance(t, LetIn):
            return self.judge(t.desugar())
        elif isinstance(t, Match):
            return t.ret.sub(t.sym, t.term)
        else:
            raise TypeError(type(t))

    def beta(self, t: Term) -> Term:
        '''Convert term to beta-iota-normal form.'''
        tnext = self._beta(t)

        while tnext != t:
            t = tnext
            tnext = self._beta(t)
        return t

    def get_def(self, v: Term) -> Term:
        '''
        If a term is a variable referencing a def, return the def
        (otherwise, no change)
        '''
        if isinstance(v, Var):
            if v.sym in self.defs:
                return self.defs[v.sym]
        return v

    def _beta(self, t: Term, fixes: S[str] = None) -> Term:
        '''
        Apply a single beta-iota-normalization step
        Warning: you can't just naively expand a definition
        when you see a variable, since fixpoints would lead
        to an infinite loop.
        '''

        fix_ = fixes or set()

        if isinstance(t, (Sort, Var)):
            return t if t.sym in fix_ else self.get_def(t)

        elif isinstance(t, Fix):
            new_fix = set.union(fix_, set([t.name]))
            return Fix(t.name, self._beta(t.typ, new_fix), tuple([
                (self._beta(c1, new_fix), self._beta(c2, new_fix))
                for c1, c2 in t.cases]))

        elif isinstance(t, App):

            if isinstance(t.t1, Var):
                t1 = t.t1 if t.t1.sym in fix_ else self.get_def(t.t1)
            else:
                t1 = t.t1

            if isinstance(t1, Fix):
                for pat, fres in t1.cases:
                    fmatch = self.unify(t.t2, pat, {})
                    if fmatch is not None:
                        return subs(fres, **fmatch)
                raise ValueError("No pattern matches the argument")
            elif isinstance(t1, Lam):
                return t1.t2.sub(t1.sym, t.t2)
            else:
                return App(self._beta(t.t1, fix_), self._beta(t.t2, fix_))

        elif isinstance(t, (Pi, Lam)):
            return type(t)(t.sym, self._beta(t.t1, fix_),
                           self._beta(t.t2, fix_))

        elif isinstance(t, LetIn):
            return self._beta(t.desugar(), fix_)

        elif isinstance(t, Match):
            if isinstance(t.term, App):
                args_ = t.term.args()
                root, args = args_[0], args_[1:]
            elif isinstance(t.term, Var):
                root, args = t.term, []
            if isinstance(root, Var) and root.sym in self.icons:
                for pat, ret in t.cases:
                    res = self.unify(Apps(root, *args), pat, {})
                    if res is not None:  # potential match
                        return subs(ret, **res)
                raise ValueError("Match expression did not fully cover cases")

            return Match(self._beta(t.term, fix_), t.sym,
                         self._beta(t.ret, fix_),
                         tuple((self._beta(c1, fix_), self._beta(c2, fix_))
                               for c1, c2 in t.cases))
        else:
            raise TypeError(type(t), t)

    def unify(self, x: Term, pat: Term, match: O[D[str, Term]] = None
              ) -> O[D[str, Term]]:
        if match is None:
            return None

        if isinstance(pat, Var):  # either actual var or inductive type/con
            if pat.sym in self:
                test = isinstance(x, Var) and x.sym == pat.sym
                return match if test else None
            else:
                return merge(match, {pat.sym: x})
        elif isinstance(pat, Sort):
            return match if x == pat else None
        elif isinstance(pat, App):
            if isinstance(x, App):
                m1 = self.unify(x.t1, pat.t1, match)
                m2 = self.unify(x.t2, pat.t2, match)
                if m1 is not None and m2 is not None:
                    return merge(m1, m2)
            return None
        else:
            raise NotImplementedError

    def to_file(self, pth: str) -> None:
        decls = [*[self.showdecl(x)
                   for x in self.itypes.keys()],
                 *[self.show_def(k) for k in self.defs]]
        txt = '\n\n'.join(decls)
        with open(pth, 'w') as f:
            f.write(txt)
        print(txt)

    def from_file(self, pth: str) -> None:
        '''Add definitions / inductive types from a file'''
        newparser = Lark(general_parser.source_grammar.replace(
            "?start: term", r'?start: (toplevel WS)+'),)
        with open(pth, 'r') as f:
            contents = newparser.parse(f.read())
        rm_ws(contents)
        for content in contents.children:
            assert isinstance(content, Tree), content
            if content.data == 'def':
                k, v = content.children
                assert isinstance(k, Token), k
                assert isinstance(v, Tree), v
                self.add_def(k, Term.term_from_lark(v))
            elif content.data == 'itype':
                self.add_type(ITypeDecl.from_lark(content))
            else:
                raise ValueError(content.data)

    def let(self, t: Term, **subst: Term) -> LetIn:
        '''
        Create let ... in ... expr without having to specify the types of the
        arguments
        '''
        return letin(t, **{k: (self.judge(v), v) for k, v in subst.items()})

    def eq(self, t1: Term, t2: Term) -> bool:
        '''Beta equality'''
        return self.beta(t1) == self.beta(t2)
