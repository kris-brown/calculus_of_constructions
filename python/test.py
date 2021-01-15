import pytest
import copy

from term import (Prop, ITypeDecl, Type, Sort, Set, Fun, Lam, Var, Funs, Pis,
                  Pi, Apps, App, Term)
from checker import TypeChecker
from base import (base, True_, False_, OR, AND, vA,
                  vB, vN, true, inl, Not, Eq, refl, vX, Nat, zero, succ, le_n,
                  Le, bnot, ff, tt, Bool, length, nilA, mk_list, two)

##############
# Unit tests #
##############


def test_alpha_equiv() -> None:
    '''Terms that are alpha equivalent are treated as equal'''
    t1 = Lam('a', Prop, Var('a'))
    t2 = Lam('b', Prop, Var('b'))
    assert t1 == t2


def test_basepos() -> None:
    '''
    No inductive defs in base fail positivity check, and we can force a failure
    '''
    base.positive_check()
    base_ = copy.deepcopy(base)
    t2 = Var('True2')
    base_.add_type(
        ITypeDecl('True2', Prop, decls=dict(con=Fun(Fun(t2, Bool), t2))))
    with pytest.raises(AssertionError):
        base_.positive_check()


def test_pi() -> None:
    '''Cannot create a function from a non-sort'''
    with pytest.raises(AssertionError):
        base.judge(Pi('x', Nat, Fun(vX, Nat)))  # Nat is not a sort


def test_app() -> None:
    '''Cannot apply to a non-pi'''
    with pytest.raises(AssertionError):
        base.judge(App(Prop, Prop))
    with pytest.raises(AssertionError):
        base.judge(App(base.let(vX, x=bnot), tt))


def test_beta() -> None:
    '''Test beta normalization'''
    for x in [Prop, Sort('2'), vX, Lam('x', Type, vX), ]:
        assert base.beta(x) == x

    for k, v in {
        App(Lam('x', Type, vX), vA): vA,
        App(Lam('x', Type, vX), App(Lam('B', Type, vB), vA)): vA,
        App(bnot, ff): tt,
        App(bnot, tt): ff,
        Apps(length, vA, nilA): zero,
        Apps(length, vA, mk_list(vA, vX, vX)): two

    }.items():
        assert base.eq(k, v)


def test_judge() -> None:
    '''Terms in base have the expected type'''

    for k, v in {
        # Sorts
        Prop: Type,
        Set: Type,
        Type: Sort('2'),
        Sort('2'): Sort('3'),

        # Truth values
        False_: Prop,
        True_: Prop,
        true: True_,
        ff: Bool,
        tt: Bool,
        bnot: Fun(Bool, Bool),

        # Propositional logic
        OR: Funs(Prop, Prop, Prop),
        inl:
            Pis(Fun(vA, Apps(OR, vA, vB)), A=Prop, B=Prop),
        AND: Funs(Prop, Prop, Prop),
        Not: Fun(Prop, Prop),

        # Data structures
        Eq: Pis(Prop, A=Type, x=vA, y=vA),
        refl: Pis(Apps(Eq, vA, vX, vX), A=Type, x=vA),
        Nat: Set,
        zero: Nat,
        succ: Fun(Nat, Nat),

        le_n: Pi('n', Nat, Apps(Le, vN, vN)),


    }.items():
        assert base.eq(base.judge(k), v), (k, base.judge(k), v)


def test_text_equal() -> None:
    '''The python API base should be equal to the parsing of the data file'''
    base2 = TypeChecker()
    base2.from_file('../data/base.txt')
    for k, itype in base.itypes.items():
        assert base2.itypes[k] == itype
    for k, icon in base.icons.items():
        assert base2.icons[k] == icon
    for k, d in base.defs.items():
        assert base2.defs[k] == d


def test_print_parse_roundtrip() -> None:
    '''There should be no change when printing then parsing'''
    for itype in base.itypes.values():
        assert Term.parse(base.show(itype)) == itype
    for icon in base.icons.values():
        assert Term.parse(base.show(icon)) == icon
    for d in base.defs.values():
        assert Term.parse(base.show(d)) == d

##################
# Property tests #
##################


def beta_idempotent() -> None:
    '''Test that beta(beta(x)) == beta(x)'''
    pass  # NOT IMPLEMENTED YET
