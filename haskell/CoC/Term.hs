module CoC.Term (Sort (..), Const (..), Term (..), ITypeDecl (..), PiLike (..), apps, lams, pis, fun, appArgs, piArgs, lamArgs, funs, prop, set, type', sub, free, beta, beta', unify, termType, zipPat) where

-----------------------

import Control.Monad (foldM)
import Data.Bifunctor (bimap)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
--import Data.Maybe (fromJust)
import Data.Set (Set, singleton)
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import Data.Tuple (swap)

-----------------------
-- Sorts are the types of types
data Sort = Prop | Set | Type Int deriving (Eq, Ord, Show)

data Const
  = D -- Mark as a definition that will be substituted
  | I -- An inductive type (constructor), e.g. the `List` of `List Int`
  | Ic -- A constructor for instances of an inductive type
  | WC -- Wildcard for pattern matching
  deriving (Eq, Ord, Show)

data Term
  = Var {varName :: Text}
  | F -- Reference a fixpoint within the body of a declaration
  | S {getSort :: Sort}
  | C {constType :: Const, cName :: Text}
  | Pi {piV :: Text, piVtype :: Term, piVterm :: Term}
  | Lam {lamV :: Text, lamVtype :: Term, lamVterm :: Term}
  | App {t1 :: Term, t2 :: Term}
  | Fix {fixName :: Text, fixType :: Term, fixCases :: [(Term, Term)]}
  | Match {matchTerm :: Term, matchVar :: Text, matchType :: Term, matchCases :: [(Term, Term)]}

--Helper type for pretty printing
data PiLike = Pi' {piArgs' :: [(Text, Term)], piTerm :: PiLike} | Arr {arrArgs :: [PiLike]} | T {unT :: Term}

-- helper type for  alpha equivalence checking
-- A mapping of bound variables and a set of variables that have been bound
-- (which can be bigger than the values of the mapping, e.g. x-> y and then later x->z)
type AEQ = (Map Text Text, Set Text)

-- Declare an inductive type
data ITypeDecl = ITypeDecl
  { itName :: Text,
    itArity :: Term,
    itParams :: [(Text, Term)],
    itConstructors :: [(Text, Term)]
  }
  deriving (Eq, Ord)

-----------------------
prop :: Term
prop = S Prop

set :: Term
set = S Set

type' :: Term
type' = S (Type 0)

-----------------------
-- Common operations --
-----------------------
instance Ord Term where
  compare x y = undefined x y

-- Manipulating the AEQ data structure
addAEQ :: Text -> Text -> AEQ -> AEQ
addAEQ k v a@(m, s) = if k == v then a else (M.insert k v m, S.insert v s)

addInjectiveAEQ :: Text -> Text -> AEQ -> Maybe AEQ
addInjectiveAEQ k v a@(m, _) = if k `M.member` m then Just $ addAEQ k v a else Nothing

empAEQ :: AEQ
empAEQ = (M.empty, S.empty)

aeqCases :: AEQ -> [(Term, Term)] -> [(Term, Term)] -> Bool
aeqCases aeq xs ys = length xs == length ys && all f (zip xs ys)
  where
    f ((x, a), (y, b)) = case aeqMatch aeq (x, y) of
      Nothing -> False
      Just aeq' -> aeqEq aeq' (a, b)

-- Try to unify modulo alpha equivalence.
-- This is only intended for the patterns of match/fixpoint exprs
-- So it is only defined for
-- Return nothing if cannot unify
-- Any remaining variable pairs added to result AEQ
-- E.g. vector n (cons h t) and (vector m (cons a t)) -> {n->m,h->a}
-- and  vector n (cons h t) and (vector m lst) -> Nothing
aeqMatch :: AEQ -> (Term, Term) -> Maybe AEQ
aeqMatch aeq@(m, s) = \case
  (S x, S y) -> if x == y then Just aeq else Nothing
  (C a x, C b y) -> if (a, x) == (b, y) then Just aeq else Nothing
  (Var x, Var y) -> if x == y then Just aeq else Nothing
  (C WC x, Var y) -> case M.lookup x m of
    Nothing -> if y `notElem` s then addInjectiveAEQ x y aeq else Nothing
    Just x' -> Just $ addAEQ x' y aeq
  (x@App {}, y@App {}) -> foldM aeqMatch aeq (zip (appArgs x) (appArgs y))
  _ -> error "Have a non S/C/Var/App in a case pattern"

aeqEq :: AEQ -> (Term, Term) -> Bool
aeqEq aeq@(m, s) = \case
  (S x, S y) -> x == y
  (F, F) -> True
  (C a x, C b y) -> (a, x) == (b, y)
  (Var x, Var y) -> case M.lookup x m of
    Nothing -> y `notElem` s && x == y
    Just x' -> x' == y
  (Pi v x y, Pi w a b) -> aeqAbs v x y w a b
  (Lam v x y, Lam w a b) -> aeqAbs v x y w a b
  (App x y, App a b) -> aeqEq aeq (x, a) && aeqEq aeq (y, b)
  (Fix v x y, Fix w a b) -> aeqEq aeq (x, a) && aeqCases (addAEQ v w aeq) y b
  (Match t v r cs, Match t' v' r' cs') -> aeqEq aeq (t, t') && aeqEq aeq (r, r') && aeqCases (addAEQ v v' aeq) cs cs'
  _ -> False
  where
    aeqAbs v x y w a b = aeqEq aeq (x, a) && aeqEq (addAEQ v w aeq) (y, b)

instance Eq Term where
  x == y = aeqEq empAEQ (x, y)

--------------
-- Printing --
--------------

-- Helper function for Show
cases :: [(Term, Term)] -> String
cases = concatMap (\(x, y) -> "\n| " ++ show x ++ " => " ++ show y)

-- Check if Pi argument is used in the subsequent terms
toPiLike :: Term -> PiLike
toPiLike = \case
  p@Pi {} -> toPiLike' $ swap $ piArgs p
  _ -> error ""

toPiLike' :: (Term, [(Text, Term)]) -> PiLike
toPiLike' (_, []) = error ""
toPiLike' (ret, [(v, t)]) = if v `elem` free ret then Pi' [(v, t)] (T ret) else Arr [T t, T ret]
toPiLike' (ret, (v, t) : vts) = case (toPiLike' (ret, vts), v `elem` free (pis vts ret)) of
  (Arr xs, True) -> Pi' [(v, t)] (Arr xs)
  (Arr xs, False) -> Arr (T t : xs)
  (Pi' xs ret', True) -> Pi' ((v, t) : xs) ret'
  (Pi' xs ret', False) -> Arr [T t, Pi' xs ret']
  (T _, _) -> error ""

paren :: String -> String
paren x = concat ["(", x, ")"]

instance Show PiLike where
  show (T t) = show t
  show (Arr xs) = intercalate " -> " $ map show xs
  show (Pi' args ret) = concat ["∀", concatMap (\(a, b) -> paren $ unpack a ++ ": " ++ show b) args, ", " ++ show ret]

instance Show Term where
  show (Var vName) = unpack vName
  show (C _ x) = unpack x
  show F = "<fix>"
  show (S Prop) = "Prop"
  show (S Set) = "Set"
  show (S (Type i)) = "Type" ++ if i == 0 then "" else show i
  show p@Pi {} = show $ toPiLike p
  show p@Lam {} = let (args, ret) = lamArgs p in concat ["(λ", concatMap (\(a, b) -> " (" ++ show a ++ ": " ++ show b ++ ")") args, " => ", show ret, ")"]
  show x@App {} = let args = appArgs x in paren $ unwords (map show args)
  show (Fix name ret cs) = concat ["fix ", unpack name, ":", show ret, cases cs, "."]
  show (Match t var r cs) = concat ["match ", show t, " as ", unpack var, " return ", show r, cases cs, "."]

instance Show ITypeDecl where
  show (ITypeDecl name arity params constructors) = concat ["Inductive ", unpack name, " ", concat ((\(k, v) -> paren $ concat [unpack k, ": ", show v]) <$> params), ": ", show arity, " :=", concat ((\(k, v) -> concat ["\n| ", unpack k, " : ", show v]) <$> constructors), "."]

termType :: Term -> String
termType = \case
  S _ -> "Sort"
  Var {} -> "Var"
  Fix {} -> "Fix"
  Match {} -> "Match"
  App {} -> "App"
  Lam {} -> "Lam"
  Pi {} -> "Pi"
  F -> "FixRef"
  C I _ -> "IType"
  C Ic _ -> "ITypeCon"
  C D _ -> "Def"
  C WC _ -> "Wildcard"

-----------------------
-- Variables and sub --
-----------------------

free :: Term -> Set Text
free = free' S.empty

-- helper function for getting free variables
free' :: Set Text -> Term -> Set Text
free' fv =
  \case
    S _ -> S.empty
    F -> S.empty
    C _ _ -> S.empty
    Var x -> if x `elem` fv then S.empty else singleton x
    Pi v x y -> abstract v x y
    Lam v x y -> abstract v x y
    App x y -> S.union (free' fv x) (free' fv y)
    Fix _ t cs -> S.unions $ free' fv t : freeCases fv cs
    Match t v r cs -> S.unions $ free' fv t : free' fv r : freeCases (S.insert v fv) cs
  where
    abstract x0 y0 z0 = S.union (free' fv y0) (free' (S.insert x0 fv) z0)
    freeCases fv' cs = map (freeCases' fv') cs
    freeCases' fv' (pattrn, retrn) = free' fv' retrn `S.difference` free pattrn

-- Apply a substitution to a term
subs :: Term -> Map Text Term -> Term
subs = M.foldlWithKey (\a k b -> sub k b a)

-- Substitute variable with a term
sub :: Text -> Term -> Term -> Term
sub var tsub =
  \case
    s@S {} -> s
    x@C {} -> x
    F -> F
    v@(Var x) -> if x == var then tsub else v
    p@(Pi v x y) -> if v == var then p else Pi v (sub' x) (sub' y)
    l@(Lam v x y) -> if v == var then l else Lam v (sub' x) (sub' y)
    (App x y) -> App (sub' x) (sub' y)
    (Fix n x cs) -> Fix n (sub' x) (bm cs)
    (Match t v r cs) -> Match (sub' t) v (sub' r) (bm cs)
  where
    sub' = sub var tsub
    bm = map (bimap sub' sub')

-- Replace F with the fixpoint definition
subFix :: Term -> Term -> Term
subFix f@Fix {} = \case
  x@S {} -> x
  x@C {} -> x
  x@Var {} -> x
  F -> f
  Lam v t r -> Lam v (sf t) (sf r)
  Pi v t r -> Pi v (sf t) (sf r)
  App a b -> App (sf a) (sf b)
  fx@Fix {} -> fx
  Match t v r cs -> Match (sf t) v (sf r) (map (fmap sf) cs)
  where
    sf = subFix f
subFix _ = error "subfix called w/o fix as first arg"

---------------------------------
-- Flattening and unflattening --
---------------------------------
-- Create a multiargument Pi term
pis :: [(Text, Term)] -> Term -> Term
pis [] t = t
pis [(v, x)] y = Pi v x y
pis ((v, x) : args) y = Pi v x (pis args y)

-- Create a multiargument Lam term
lams :: [(Text, Term)] -> Term -> Term
lams [] t = t
lams [(v, x)] y = Lam v x y
lams ((v, x) : args) y = Lam v x (lams args y)

-- Apply multiple arguments to a function
apps :: [Term] -> Term
apps [] = error "empty arg list for apps"
apps [x] = x
apps l = App (apps (init l)) (last l)

-- Create function type (using hopefully-free variable names for the arguments of the Pi type)
funs :: [Term] -> Term
funs [] = error "empty arg list for funs"
funs [x] = x
funs l = pis (zip (map (\z -> pack $ "_x" ++ show z) [(1 :: Int) ..]) (init l)) (last l)

fun :: Term -> Term -> Term
fun x y = funs [x, y]

-- Flatten nested lambdas: λ x:X =>(λ y:Y => z) -> ([(x,X),(y,Y)], z)
lamArgs :: Term -> ([(Text, Term)], Term)
lamArgs (Lam v vType t) = let (a, b) = lamArgs t in ((v, vType) : a, b)
lamArgs x = ([], x)

-- Flatten nested pis: ∀x:X, (∀y:Y, z) -> ([(x,X),(y,Y)], z)
piArgs :: Term -> ([(Text, Term)], Term)
piArgs (Pi v vType t) = let (a, b) = piArgs t in ((v, vType) : a, b)
piArgs x = ([], x)

-- Flatten nested applications: (f x) y -> [f, x, y]
appArgs :: Term -> [Term]
appArgs (App x y) = appArgs x ++ [y]
appArgs z = [z]

-- Beta-iota-reduce (evaluate) a term to normal form
-- Apply beta reductions until convergence
beta :: Term -> Term
beta t =
  let t' = beta' t
   in if t == t' then t else beta t'

-- Single beta-iota-reduce reduction step
beta' :: Term -> Term
beta' = \case
  x@S {} -> x
  x@Var {} -> x
  F -> F
  x@C {} -> x
  (App (Lam v _ r) y) -> sub v y r
  (App f@(Fix _ _ cs) y) -> subFix f $ findCase y cs
  x@(App a b) -> case appArgs x of
    (h@(C I _) : t) -> apps $ h : map beta' t
    (h@(C Ic _) : t) -> apps $ h : map beta' t
    (F : t) -> apps $ F : map beta' t
    _ -> App (beta' a) (beta' b)
  (Lam v t r) -> Lam v (beta' t) (beta' r)
  (Pi v t r) -> Pi v (beta' t) (beta' r)
  (Fix v t cs) -> Fix v (beta' t) (map (fmap beta') cs)
  Match t v r cs -> case appArgs t of
    ((C Ic _) : _) -> findCase t cs
    _ -> Match (beta' t) v (beta' r) (map (fmap beta') cs)
  where
    findCase _ [] = error "incomplete pattern"
    findCase x ((pat, ret) : cs) = case unify M.empty (pat, x) of
      Nothing -> findCase x cs
      Just m -> subs ret m

unify :: Map Text Term -> (Term, Term) -> Maybe (Map Text Term)
unify ctx = \case
  (S x, S y) -> if x == y then Just ctx else Nothing
  (Var x, Var y) -> if x == y then Just ctx else Nothing
  (C a x, C b y) -> if (a, x) == (b, y) then Just ctx else Nothing
  (C WC x, y) -> case M.lookup x ctx of
    Nothing -> Just $ M.insert x y ctx
    Just y' -> if y == y' then Just ctx else Nothing
  (x@App {}, y@App {}) -> foldM unify M.empty $ zipPat (appArgs x) (appArgs y)
  _ -> Nothing

-- Zip App pattern with App term
-- [a,b,c,d] == [a, apps [b,c,d]] etc.
-- Do this transformation to make the # of arguments match, if possible
zipPat :: [Term] -> [Term] -> [(Term, Term)]
zipPat [] _ = error "zipPat empty"
zipPat _ [] = error "zipPat empty"
zipPat [x] y = [(x, apps y)]
zipPat (h : t) (a : b) = (h, a) : zipPat t b