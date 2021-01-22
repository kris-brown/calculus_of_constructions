module Checker (TypeChecker (..), writeFile, subDef, judge) where

import Data.Bifunctor (bimap)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import qualified System.IO
import Term (Const (..), ITypeDecl (..), Sort (..), Term (..), beta, pis, sub)
import Prelude hiding (writeFile)

data TypeChecker = TypeChecker {itypes :: [ITypeDecl], defs :: Map Text Term}

---------------
-- Checking  --
---------------
-- Look up term corresponding to the inductive type
idefs :: TypeChecker -> Map Text Term
idefs (TypeChecker it _) = M.fromList $ map f it
  where
    f (ITypeDecl n ar par _) = (n, pis par ar)

-- Look up the type of the constructors
icondefs :: TypeChecker -> Map Text Term
icondefs (TypeChecker it _) = M.fromList $ concatMap f it
  where
    f (ITypeDecl _ _ par cs) = map (g par) cs
    g pars (conName, conType) = (conName, pis pars conType)

judge :: TypeChecker -> Term -> Term
judge tc t = judge' M.empty Nothing tc (beta $ subDef tc t)

judge' :: Map Text Term -> Maybe Term -> TypeChecker -> Term -> Term
judge' ctx fix tc = \case
  (S Prop) -> S $ Type 0
  (S Set) -> S $ Type 0
  (S (Type i)) -> S $ Type $ i + 1
  (C D _) -> error "judge' should be called after defs have been sub'd"
  (C I i) -> idefs tc M.! i
  (C Ic i) -> icondefs tc M.! i
  (C WC _) -> error "judge' should not be called on patterns"
  F -> fromJust fix -- fixpoints get added to context
  (Var v) -> ctx M.! v
  (Pi v t r) -> case (t, judge' (M.insert v t ctx) fix tc r) of
    (S Prop, S Prop) -> S Prop -- implication
    (S Set, S s2) -> S s2 -- type of first order predicates (when s2 is prop) or functions (when s2 is Set) or higher order predicates (s2 is type i)
    (S (Type i), S (Type j)) -> S $ Type $ max i j
    (S x, S y) -> error $ "Predicative calculus of inductive constructions forbids " ++ show x ++ show y
    _ -> error "Pi type's source type and type of target  must be sorts"
  (Lam v t r) -> Pi v t $ judge' (M.insert v t ctx) fix tc r
  (App a b) -> case judge' ctx fix tc a of
    Pi v _ r -> sub v b r
    _ -> error "Cannot apply something to a non-lambda"
  (Match _ _ a _) -> a
  (Fix _ t _) -> t

----------
-- Misc --
----------
-- Recursively replace all defined constants with lookups from defs
subDef :: TypeChecker -> Term -> Term
subDef tc t = let t' = subDef' tc t in if t == t' then t else subDef tc t'

subDef' :: TypeChecker -> Term -> Term
subDef' tc = \case
  (C D x) -> defs tc M.! x -- fail if not found
  (App x y) -> App (sub' x) (sub' y)
  F -> F
  (Lam x y z) -> Lam x (sub' y) (sub' z)
  (Pi x y z) -> Pi x (sub' y) (sub' z)
  (Match t v r cs) -> Match (sub' t) v (sub' r) $ mapCases cs
  (Fix t r cs) -> Fix t (sub' r) $ mapCases cs
  (S x) -> S x
  (Var x) -> Var x
  (C x y) -> C x y
  where
    mapCases = map (bimap sub' sub')
    sub' = subDef' tc

--------------
-- Printing --
--------------
toDef :: (Text, Term) -> String
toDef (n, v) = "def " ++ unpack n ++ " = " ++ show v

writeFile :: TypeChecker -> FilePath -> IO ()
writeFile (TypeChecker it ds) fp = System.IO.writeFile fp $ intercalate "\n\n" ((show <$> it) ++ map toDef (M.toList ds))
