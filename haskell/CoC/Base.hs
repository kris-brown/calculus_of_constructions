module CoC.Base (true', false', bool, tt, ff, bnot, not', nat, zero, suc, add, one, two, three, list, nil', cons', length', indlength, vHead, vTail, le, leN, leS, vect, or', inl, inr, and', andMk, eq, refl, map', base, vA, vB, vN, vX, vF, vL, vP, vM, mkList, nilA, consA, consHeadTail) where

import CoC.Checker (TypeChecker (..))
import CoC.Term (Const (..), ITypeDecl (..), Sort (..), Term (..), apps, fun, funs, lams, pis, prop, set, type')
import Data.Map (Map, fromList)
import Data.Text (Text)

[true', false', bool, nat, list, indlength, le, vect, or', and', eq] = map (C I) ["True", "False", "bool", "Nat", "List", "IndLength", "le", "Vector", "OR", "AND", "Eq"]

[tt, ff, zero, suc, nil', cons', leN, leS, inl, inr, andMk, refl] = map (C Ic) ["tt", "ff", "zero", "succ", "nil", "cons", "le_n", "le_s", "inl", "inr", "andMk", "refl"]

[bnot, not', add, one, two, three, length', map'] = map (C D) ["bnot", "not", "add", "one", "two", "three", "length", "map"]

[vA, vB, vN, vX, vF, vL, vP, vM, vHead, vTail] = map Var ["A", "B", "n", "x", "f", "l", "P", "m", "head", "tail"]

sn = App suc vN

sn' = App suc (C WC "n")

[listA, nilA, consA] = map (`App` vA) [list, nil', cons']

consHeadTail = apps [consA, C WC "head", C WC "tail"]

mkList :: Term -> [Term] -> Term
mkList typ args = apps $ foldl f [App nil' typ] args
  where
    f xs x = apps [cons', typ, x] : xs

ityps =
  [ ITypeDecl "True" prop [] [("true_mk", true')],
    ITypeDecl "False" prop [] [],
    ITypeDecl
      "Bool"
      set
      []
      [("tt", bool), ("ff", bool)],
    ITypeDecl
      "OR"
      prop
      [("A", prop), ("B", prop)]
      [ ("inl", fun vA (apps [or', vA, vB])),
        ("inr", fun vB (apps [or', vA, vB]))
      ],
    ITypeDecl
      "AND"
      prop
      [("A", prop), ("B", prop)]
      [("and_mk", funs [vA, vB, apps [prop, vA, vB]])],
    ITypeDecl
      "Eq"
      (Pi "a" vA prop)
      [("A", type'), ("x", vA)]
      [("refl", apps [eq, vA, vX, vX])],
    ITypeDecl
      "Nat"
      set
      []
      [("zero", nat), ("succ", fun nat nat)],
    ITypeDecl
      "le"
      (fun nat prop)
      [("n", nat)]
      [ ("le_n", apps [le, vN, vN]),
        ( "le_s",
          Pi
            "m"
            nat
            ( fun
                (apps [le, vN, vM])
                (apps [le, App suc vN, App suc vM])
            )
        )
      ],
    ITypeDecl
      "List"
      type'
      [("A", type')]
      [("nil", listA), ("cons", funs [vA, listA, listA])],
    ITypeDecl
      "IndLength"
      (funs [listA, nat, prop])
      [("A", type')]
      [ ("lnil", apps [indlength, nilA, zero]),
        ( "lcons",
          pis
            [ ("a", vA),
              ("l", listA),
              ("n", nat)
            ]
            ( fun
                (apps [indlength, Var "l", vN])
                (apps [indlength, apps [consA, Var "a", Var "l"], App suc vN])
            )
        )
      ],
    ITypeDecl
      "Vector"
      (fun nat type')
      [("A", type')]
      [ ("vnil", apps [vect, vA, zero]),
        ( "vcons",
          Pi "n" nat $
            funs [vA, apps [vect, vA, vN], apps [vect, vA, sn]]
        )
      ]
  ]

dfns :: Map Text Term
dfns =
  fromList
    [ ("type2", S (Type 2)),
      ("one", App suc zero),
      ("two", App suc one),
      ("three", App suc two),
      ( "natrec",
        lams
          [ ("P", fun nat prop),
            ("p0", App vP zero),
            ("pn", Pi "n" nat (fun (App vP vN) (App vP sn)))
          ]
          ( Fix
              "NatRec"
              (Pi "n" nat (App vP vN))
              [ (zero, Var "p0"),
                (sn, App (Var "pn") vN)
              ]
          )
      ),
      ( "map",
        Lam
          "f"
          (fun vA vB)
          ( Fix
              "Map"
              (fun listA (App list vB))
              [ (nilA, App nil' vB),
                ( consHeadTail,
                  apps
                    [ App cons' vB,
                      App vF vHead,
                      App F vTail
                    ]
                )
              ]
          )
      ),
      ( "add",
        Lam
          "m"
          nat
          ( Fix
              "Add"
              (fun nat nat)
              [ (zero, vM),
                (sn', App suc (App F vN))
              ]
          )
      ),
      ( "length",
        Lam
          "A"
          type'
          ( Fix
              "Length"
              (fun listA nat)
              [ (nilA, zero),
                (consHeadTail, App suc (App F vTail))
              ]
          )
      ),
      ("not", Lam "A" prop (Pi "A" prop false')),
      ( "bnot",
        Lam
          "x"
          bool
          ( Match
              vX
              "x"
              bool
              [ (tt, ff),
                (ff, tt)
              ]
          )
      ),
      ( "isZero",
        Lam
          "x"
          nat
          ( Match
              vX
              "x"
              (fun nat prop)
              [ (zero, true'),
                (sn', false')
              ]
          )
      )
    ]

base :: TypeChecker
base = TypeChecker ityps dfns

true' :: Term
false' :: Term
bool :: Term
list :: Term
tt :: Term
ff :: Term
bnot :: Term
not' :: Term
nat :: Term
zero :: Term
suc :: Term
add :: Term
one :: Term
two :: Term
three :: Term
nil' :: Term
cons' :: Term
length' :: Term
indlength :: Term
vHead :: Term
vTail :: Term
le :: Term
leN :: Term
leS :: Term
vect :: Term
or' :: Term
inl :: Term
inr :: Term
and' :: Term
andMk :: Term
eq :: Term
refl :: Term
map' :: Term
vA :: Term
vB :: Term
vN :: Term
vX :: Term
vF :: Term
vL :: Term
vP :: Term
vM :: Term
sn :: Term
listA :: Term
nilA :: Term
consA :: Term
consHeadTail :: Term
ityps :: [ITypeDecl]
sn' :: Term
