module Process where

import qualified Data.List as L
import Data.Set (Set, (\\)); import qualified Data.Set as S
import Data.Map (Map); import qualified Data.Map as M
import Data.Semigroup
import qualified Data.Foldable as F
import Data.Functor
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Functor.Classes
import Data.Functor.Compose
import Control.Monad.State
import Control.Monad.Writer.Strict
import Control.Applicative
import Text.Show.Deriving

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
deriveShow1 ''ProcessF

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

unanno :: Fix (AnnoF a (Base Process)) -> Process
unanno = hoist (snd . getCompose)

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

-- Fixed point computation
fixed :: (a -> Writer Any a) -> a -> a
fixed f x = if p then fixed f y else y where (y, Any p) = runWriter (f x)

-- -------------------- From here on, assume UB --------------------

-- New sinking
sinkNews :: FVProcess -> FVProcess
sinkNews = fixed . fix $ \ go -> \case
  ANew vs x (Anno ws p) | x ∉ ws -> go (Anno vs p)
  ANew vs x (ANew _ y p@(Anno ws _)) ->
    ANew vs y <$> do tick; go . ANew (ws \\ x) x =<< go p
  ANew vs x (ASend _ s d p@(Anno ws _)) | x /= s && x /= d ->
    ASend vs s d <$> do tick; go . ANew (ws \\ x) x =<< go p
  ANew vs x (ARecv _ d s p@(Anno ws _)) | x /= s ->
    ARecv vs s d <$> do tick; go . ANew (ws \\ x) x =<< go p
  ANew vs x (ABoth _ p@(Anno ps _) q@(Anno qs _)) | x ∉ ps ->
    do tick; ABoth vs <$> go p <*> (go . ANew (qs \\ x) x =<< go q)
  ANew vs x (ABoth _ p@(Anno ps _) q@(Anno qs _)) | x ∉ qs ->
    do tick; ABoth vs <$> (go . ANew (ps \\ x) x =<< go p) <*> go q
  ANew vs x (AEither ws p@(Anno ps _) q@(Anno qs _)) ->
    do tick; AEither vs <$> (go . ANew (ps \\ x) x =<< go p) <*> (go . ANew (qs \\ x) x =<< go q)
  ANew vs x (AMatch _ y arms) | x `L.notElem` (y : map fst arms) ->
    do tick; AMatch vs y <$> mapM (mapM (\ p@(Anno ps _) -> go . ANew (ps \\ x) x =<< go p)) arms <* tick
  p -> tell (Any False) $> p
  where
    tick = tell (Any True)
    ret x = tick $> x
    s \\ x = S.delete x s

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
type Constraint = (Var, Var)

clique :: Set Var -> Set Constraint
clique xs = S.fromList [(x, y) | x:ys <- L.tails (S.toList xs), y <- ys]

-- Collect interference constraints
constraints :: FVProcess -> Set Constraint
constraints = fold $ \case
  AnnoF (clique -> vs) HaltF -> vs
  AnnoF (clique -> vs) (NewF _ ps) -> vs ∪ ps
  AnnoF (clique -> vs) (SendF _ _ ps) -> vs ∪ ps
  AnnoF (clique -> vs) (RecvF _ _ ps) -> vs ∪ ps
  AnnoF (clique -> vs) (ps :|:$ qs) -> vs ∪ ps ∪ qs
  AnnoF (clique -> vs) (ps :+:$ qs) -> vs ∪ ps ∪ qs
  AnnoF (clique -> vs) (LoopF ps) -> vs ∪ ps
  AnnoF (clique -> vs) (MatchF _ (map snd -> ps)) -> vs ∪ F.fold ps
