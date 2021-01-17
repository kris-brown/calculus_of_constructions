module Lib
  ( Sort (..),
    Term (..),
  )
where

import Data.Set (Set, singleton)
import qualified Data.Set as S
import Data.Text (Text)

--import qualified Data.Text as T

data Sort = Prop | Set | Type Int deriving (Eq, Ord, Show)

data Term = Var Text | S Sort | Pi Text Term Term | Lam Text Term Term | App Term Term

instance Eq Term where
  Var x == Var y = x == y
  S x == S y = x == y
  Pi v x y == Pi w a b = undefined
  Lam v x y == Lam w a b = undefined
  App x y == App a b = undefined
  _ == _ = False

freevars :: Term -> Set Text
freevars =
  let abstract x y z = undefined
   in \case
        (S _) -> S.empty
        (Var x) -> singleton x
        (Pi v x y) -> abstract v x y
        (Lam v x y) -> abstract v x y
        (App x y) -> S.union (freevars x) (freevars y)
