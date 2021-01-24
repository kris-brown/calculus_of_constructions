import CoC.Base (base, bnot, bool, eq, false', ff, inl, le, leN, length', mkList, nat, not', or', refl, suc, three, tt, vA, vB, vN, vX, zero)
import CoC.Checker (judge, subDef)
import CoC.Parse (parseFile, parseLam, typed)
import CoC.Term (Sort (..), Term (..), apps, beta, fun, funs, pis, prop, set, type')
import Data.Either (isRight)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?))
import qualified Text.Parsec as Parsec

betas :: [TestTree]
betas =
  map
    f
    [ (App (Lam "x" type' vX) vA, vA),
      (App (Lam "x" type' vX) (App (Lam "B" type' vB) vA), vA),
      (App bnot ff, tt),
      (App bnot tt, ff),
      (apps [length', vA, mkList vA []], zero),
      (apps [length', vA, mkList vA [vX, vA, vX]], three)
    ]
  where
    f (x1, x2) =
      testCase (concat ["beta ", show x1, " = ", show x2]) $
        assertEqual
          ""
          (beta $ subDef base x2)
          (beta (subDef base x1))

judges :: [TestTree]
judges =
  map
    f
    [ (prop, type'),
      (set, type'),
      (type', S (Type 1)),
      (false', prop),
      (tt, bool),
      (bnot, fun bool bool),
      (or', funs [prop, prop, prop]),
      (inl, pis [("A", prop), ("B", prop)] $ fun vA $ apps [or', vA, vB]),
      (not', fun prop prop),
      (eq, pis [("A", type'), ("x", vA), ("y", vA)] prop),
      (refl, pis [("A", type'), ("x", vA)] $ apps [eq, vA, vX, vX]),
      (nat, set),
      (zero, nat),
      (suc, fun nat nat),
      (leN, Pi "n" nat $ apps [le, vN, vN])
    ]
  where
    f (x1, x2) =
      testCase (concat ["judge ", show x1, " = ", show x2]) $
        assertEqual
          ""
          (subDef base x2)
          (judge base $ subDef base x1)

unitTests :: TestTree
unitTests =
  testGroup
    "UnitTests"
    $ [ testCase "parseTyped" $
          isRight (Parsec.parse typed "test" "(A: Prop)") @? "",
        testCase "parseLam" $
          isRight (Parsec.parse parseLam "test" "λ  (x: Prop) (y: Prop)  =>  Prop")
            @? "",
        testCase
          "parseFile"
          $ (not . null <$> parseFile "../data/base.txt") @? "",
        testCase
          "alpha_equiv"
          $ assertEqual
            ""
            (Lam "A" prop vA)
            (Lam "B" prop vB)
      ]
      ++ betas
      ++ judges

main :: IO ()
main = do
  Test.Tasty.defaultMain unitTests

--  hspec propertyTests
-- x == y is the same as y == x