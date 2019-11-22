module Process where

import qualified Data.List as L
import Data.Set (Set, (\\)); import qualified Data.Set as S
import Data.Map (Map); import qualified Data.Map as M
import Data.Semigroup
import qualified Data.Foldable as F
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Functor.Classes
import Data.Functor.Compose
import Control.Monad.State
import Control.Applicative

import Data.SBV

type Var = Int

data Process
  = Halt
  | New Var Process
  | Send Var Var Process
  | Recv Var Var Process
  | Process :|: Process
  | Process :+: Process
  | Loop Process
  | Match Var [(Var, Process)]
  deriving (Eq, Ord, Show)

makeBaseFunctor ''Process

pattern Match' x ys zs <- Match x (L.unzip -> (ys, zs))

(∪) :: Ord a => Set a -> Set a -> Set a
(∪) = S.union

-- Generic fold over variables
foldVars :: Monoid m => (Var -> m) -> Process -> m
foldVars m = cata $ \case
  HaltF -> mempty
  NewF x my -> m x <> my
  SendF x y mz -> m x <> m y <> mz
  RecvF x y mz -> m x <> m y <> mz
  mx :|:$ my -> mx <> my
  mx :+:$ my -> mx <> my
  LoopF mx -> mx
  MatchF x (L.unzip -> (ys, mzs)) -> mconcat (m x : map m ys ++ mzs)

-- Smallest variable v such that {v + 1, v + 2, ..} are all unused
maxUsed :: Process -> Var
maxUsed = getMax . foldVars Max

-- Used variables
uv :: Process -> Set Var
uv = foldVars S.singleton

-- Rename bound variables for unique bindings
ub :: Process -> Process
ub p = go M.empty p `evalState` maxUsed p where
  go σ = \case
    Halt -> return Halt
    New x p -> do x' <- gen; New x' <$> go (M.insert x x' σ) p
    Send s d p -> Send (app σ s) (app σ d) <$> go σ p
    Recv d s p -> do d' <- gen; Recv d' (app σ s) <$> go (M.insert d d' σ) p
    p :|: q -> liftA2 (:|:) (go σ p) (go σ q)
    p :+: q -> liftA2 (:+:) (go σ p) (go σ q)
    Loop p -> Loop <$> go σ p
    Match x yps -> Match x <$> traverse (traverse (go σ)) yps
  app σ x = M.findWithDefault x x σ
  gen = modify' succ *> get

-- Annotate every subprocess with some value
type AnnoF a f = Compose ((,) a) f
pattern AnnoF t p = Compose (t, p)
pattern Anno t p = Fix (Compose (t, p))
anno :: (Base Process a -> a) -> Process -> Fix (AnnoF a (Base Process))
anno f = cata $ \ p ->
  let ann x = Anno x p in
  case p of
    HaltF -> ann (f HaltF)
    NewF x (Anno a _) -> ann (f (NewF x a))
    SendF s d (Anno a _) -> ann (f (SendF s d a))
    RecvF d s (Anno a _) -> ann (f (RecvF d s a))
    Anno a _ :|:$ Anno b _ -> ann (f (a :|:$ b))
    Anno a _ :+:$ Anno b _ -> ann (f (a :+:$ b))
    LoopF (Anno a _) -> ann (f (LoopF a))
    MatchF x arms -> ann (f (MatchF x (map (\ (y, Anno a _) -> (y, a)) arms)))

-- Free variables
fvF :: Base Process (Set Var) -> Set Var
fvF = \case
  HaltF -> S.empty
  NewF x vs -> S.delete x vs
  SendF s d vs -> S.insert s (S.insert d vs)
  RecvF d s vs -> S.insert s (S.delete d vs)
  vs :|:$ ws -> vs ∪ ws
  vs :+:$ ws -> vs ∪ ws
  LoopF vs -> vs
  MatchF x (L.unzip -> (xs, vss)) -> S.fromList (x : xs) ∪ F.fold vss

fv :: Process -> Set Var
fv = cata fvF

-- Label every node with free variables
type FVProcess = Fix (AnnoF (Set Var) (Base Process))
pattern AHalt vs = Anno vs HaltF
pattern ANew vs x p = Anno vs (NewF x p)
pattern ASend vs s d p = Anno vs (SendF s d p)
pattern ARecv vs d s p = Anno vs (RecvF d s p)
pattern ABoth vs p q = Anno vs (p :|:$ q)
pattern AEither vs p q = Anno vs (p :+:$ q)
pattern ALoop vs p = Anno vs (LoopF p)
pattern AMatch vs x arms = Anno vs (MatchF x arms)

fvAnno :: Process -> FVProcess
fvAnno = anno fvF

(∉) :: Ord a => a -> Set a -> Bool
(∉) = S.notMember

-- New sinking
sinkNews :: FVProcess -> FVProcess
sinkNews = cata $ \case
  AnnoF vs (NewF x (Anno ws p)) | x ∉ ws -> Anno vs p
  AnnoF vs (NewF x (ANew ws y p)) -> ANew vs y (ANew ws x p)
  AnnoF vs (NewF x (ASend _ s d p)) | x /= s && x /= d -> ASend vs s d (ANew vs x p)
  AnnoF vs (NewF x (ARecv _ d s p)) | x /= s && x /= d -> ARecv vs s d (ANew vs x p)
  AnnoF vs (NewF x (ABoth ws p@(Anno xs _) q)) | x ∉ xs -> ABoth vs p (ANew vs x q)
  AnnoF vs (NewF x (ABoth ws p q@(Anno xs _))) | x ∉ xs -> ABoth vs (ANew vs x p) q
  AnnoF vs (NewF x (AEither ws p q)) -> AEither vs (ANew vs x p) (ANew vs x q)
  -- NewF x (p :+: q) -> New x p :|: New x q
  -- NewF x (Match' y zs ps) | x /= y && x `L.notElem` zs -> Match y (zip zs (New x <$> ps))
  -- p -> embed p

-- -------------------- From here on, assume UB --------------------

-- Expected number of forks that happen in a variable's lifetime
forks :: Var -> Process -> Double
forks x = \case
  Halt -> 0
  New _ p -> forks x p
  Send _ _ p -> forks x p
  Recv _ _ p -> forks x p
  p :|: q -> (if x `S.member` fv q then 1 else 0) + forks x p + forks x q
  p :+: q -> forks x p / 2 + forks x q / 2
  Loop p -> 100 * forks x p
  Match' _ _ ps -> maximum (0 : (forks x <$> ps))

-- The variables in a process, sorted by forks
sortedVars :: Process -> [Var]
sortedVars p = L.sortOn (`forks` p) (S.toList (uv p))

-- Interference constraint
type Constraint = Set Var

-- Collect interference constraints
constraints :: Process -> Set Constraint
constraints p = fv p `S.insert` case p of
  Halt -> S.empty
  New _ p -> constraints p
  Send _ _ p -> constraints p
  Recv _ _ p -> constraints p
  p :|: q -> constraints p ∪ constraints q
  p :+: q -> constraints p ∪ constraints q
  Loop p -> constraints p
  Match' _ _ ps -> foldMap constraints ps
