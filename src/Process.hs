module Process where

import Data.Fix
import Data.Bifunctor
import qualified Data.List as L
import Data.Set (Set, (\\)); import qualified Data.Set as S
import Data.Map (Map); import qualified Data.Map as M
import Data.Semigroup
import Control.Monad.State
import Control.Applicative

type Var = Int

data ProcessF a
  = HaltF
  | NewF Var a
  | SendF Var Var a
  | RecvF Var Var a
  | BothF a a
  | EitherF a a
  | LoopF a
  | MatchF Var [(Var, a)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Process = Fix ProcessF
pattern Halt = Fix HaltF
pattern New x p = Fix (NewF x p)
pattern Send src dst p = Fix (SendF src dst p)
pattern Recv dst src p = Fix (RecvF dst src p)
pattern p :|: q = Fix (BothF p q)
pattern p :+: q = Fix (EitherF p q)
pattern Loop p = Fix (LoopF p)
pattern Match x arms = Fix (MatchF x arms)
pattern Match' x ys ps <- Fix (MatchF x (L.unzip -> (ys, ps)))
{-# COMPLETE Halt, New, Send, Recv, (:|:), (:+:), Match, Loop #-}
{-# COMPLETE Halt, New, Send, Recv, (:|:), (:+:), Match', Loop #-}

(∪) :: Ord a => Set a -> Set a -> Set a
(∪) = S.union

-- Free variables
fv :: Process -> Set Var
fv = \case
  Halt -> S.empty
  New x p -> S.delete x (fv p)
  Send src dst p -> S.fromList [src, dst] ∪ fv p
  Recv dst src p -> S.insert src (S.delete dst (fv p))
  p :|: q -> fv p ∪ fv q
  p :+: q -> fv p ∪ fv q
  Loop p -> fv p
  Match x (L.unzip -> (xs, ps)) -> S.fromList (x : xs) ∪ foldMap fv ps

-- Generic fold over variables
foldVars :: Monoid m => (Var -> m) -> Process -> m
foldVars m = cata $ \case
  HaltF -> mempty
  NewF x my -> m x <> my
  SendF x y mz -> m x <> m y <> mz
  RecvF x y mz -> m x <> m y <> mz
  BothF mx my -> mx <> my
  EitherF mx my -> mx <> my
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

-- New sinking
sinkNews :: Process -> Process
sinkNews = cata $ \case
  NewF x p | x `S.notMember` fv p -> p
  NewF x (New y p) -> New y (New x p)
  NewF x (Send s d p) | x /= s && x /= d -> Send s d (New x p)
  NewF x (Recv d s p) | x /= s && x /= d -> Recv d s (New x p)
  NewF x (p :|: q) | x `S.notMember` fv p -> p :|: New x q
  NewF x (p :|: q) | x `S.notMember` fv q -> New x p :|: q
  NewF x (p :+: q) -> New x p :|: New x q
  NewF x (Match' y zs ps) | x /= y && x `L.notElem` zs -> Match y (zip zs (New x <$> ps))
  p -> Fix p

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

-- The variables in a process, sorted by forks. Assumes UB
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
