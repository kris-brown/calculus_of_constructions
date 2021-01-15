from term import (ITypeDecl, Prop, Pi, Pis, Var, Type, App, Set,
                  Lam, Apps, Match, Fun, Funs, Fix,
                  Lams, Sort, Term)
from checker import TypeChecker
'''
Define common terms (e.g. Not) and inductive types
and add the latter to a base type checker
'''

base = TypeChecker()

# Inductive types

(True_, true, False_, Not,
 Nat, zero, succ, add,
 List, nil, cons, Length, length,
 one, two, three,
 Le, le_n, le_s,
 Vect,
 OR, inl, inr,
 AND, and_mk,
 Bool, tt, ff, bnot,
 Eq, refl,
 Map) = map(
    Var, ['True', 'true_mk', 'False', 'Not',
          'Nat', 'zero', 'succ', 'add',
          'List', 'nil', 'cons', 'Length', 'length',
          'one', 'two', 'three',
          'le', 'le_n', 'le_s',
          'vector',
          'OR', 'inl', 'inr',
          'AND', 'and_mk',
          'Bool', 'tt', 'ff', 'bnot',
          'Eq', 'refl',
          "Map"])

vA, vB, vN, vX, vF, vL, vP, vM = map(Var, 'ABnxflPm')

Sn = App(succ, vN)
ListA, nilA, consA = [App(x, vA) for x in [List, nil, cons]]
vHead, vTail = map(Var, ['head', 'tail'])
consHeadTail = Apps(consA, vHead, vTail)

for x in [
    ITypeDecl('True', Prop, decls=dict(true_mk=True_)),

    ITypeDecl('False', Prop),

    ITypeDecl('Bool', Set, decls=dict(tt=Bool, ff=Bool)),

    ITypeDecl('OR', Prop, dict(A=Prop, B=Prop), dict(
        inl=Fun(vA, Apps(OR, vA, vB)),
        inr=Fun(vB, Apps(OR, vA, vB)))),

    ITypeDecl('AND', Prop, dict(A=Prop, B=Prop), dict(
        and_mk=Funs(vA, vB, Apps(AND, vA, vB)))),

    ITypeDecl("Eq", Pi('a', vA, Prop), dict(A=Type, x=vA),
              dict(refl=Apps(Eq, vA, vX, vX))),

    ITypeDecl('Nat', Set, decls=dict(zero=Nat, succ=Fun(Nat, Nat))),

    ITypeDecl('le', Fun(Nat, Prop), dict(n=Nat),
              dict(le_n=Apps(Le, vN, vN),
                   le_S=Pis(Apps(Le, vN, Var('m')), m=Nat))),

    ITypeDecl('List', Type, dict(A=Type), dict(
        nil=ListA,
        cons=Funs(vA, ListA, ListA))),

    ITypeDecl('Length', Funs(ListA, Nat, Prop), dict(A=Type), dict(
        lnil=Apps(Length, nilA, zero),
        lcons=Pis(Apps(Length, Apps(cons, vA, Var('a'), Var('l'), Sn)),
                  a=vA, l=ListA, n=Nat, _=Apps(Length, vL, vN)))),

    ITypeDecl('vector', Pi('_', Nat, Type), dict(A=Type),
              dict(Vnil=Apps(Vect, vA, zero),
                   Vcons=Pis(Apps(Vect, vA, Sn),
                             a=vA, v=Apps(
                       Vect, vA, vN)))),
]:
    base.add_type(x)


for name, val in dict(
    type2=Sort('2'),

    one=App(succ, zero),

    two=App(succ, one),

    three=App(succ, two),

    natrec=Lams(
        Fix('NatRec', Pi('n', Nat, App(vP, vN)), (
            (zero, Var('p0')),
            (Sn, App(Var('pn'), vN)))),
        P=Fun(Nat, Prop),
        p0=App(vP, zero),
        pn=Pi('n', Nat, Fun(App(vP, vN), App(vP, Sn)))),

    map=Lams(Fix("Map", Fun(ListA, App(List, vB)),
                 ((nilA, App(nil, vB)),
                  (consHeadTail,
                   Apps(App(cons, vB),
                        App(vF, vA),
                        App(Map, vTail),),
                   ))),
             f=Fun(vA, vB)),

    add=Lam('m', Nat, Fix("add", Fun(Nat, Nat), (
        (zero, vM),
        (Sn, App(succ, Apps(add, vN, vM)))))),

    length=Lam('A', Type, Fix("length", Fun(ListA, Nat), (
        (nilA, zero,),
        (consHeadTail, App(succ, Apps(length, vA, vTail)))))),

    Not=Lam('A', Prop, Pi('A', Prop, False_)),

    bnot=Lam('x', Bool, Match(vX, 'x', Bool,
                              ((tt, ff), (ff, tt)))),

    isZero=Lam('x', Nat, Match(Var('x'), 'x', Fun(Nat, Bool), (
        (zero, True_),
        (Sn, False_))))

).items():

    base.add_def(name, val)


def mk_list(typ: Term, *elems: Term) -> Term:
    '''Create a list of type `typ`'''
    ret = App(nil, typ).t
    for t in reversed(elems):
        ret = Apps(cons, typ, t, ret)
    return ret


if __name__ == '__main__':
    if True:
        print("\n\n", *[base.showdecl(x)
                        for x in base.itypes.keys()],
              *[base.show_def(k) for k in base.defs],
              sep='\n\n')
