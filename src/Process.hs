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

-- -------------------- Utils --------------------

(∪) :: Ord a => Set a -> Set a -> Set a
(∪) = S.union

(∈) :: Ord a => a -> Set a -> Bool
(∈) = S.member

(∉) :: Ord a => a -> Set a -> Bool
(∉) = S.notMember

-- Fixed point computation
fixed :: (a -> Writer Any a) -> a -> a
fixed f x = if p then fixed f y else y where (y, Any p) = runWriter (f x)

-- -------------------- Annotate every subprocess with some value --------------------

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

-- Label every node
type AProcess a = Fix (AnnoF a (Base Process))
pattern AHalt a = Anno a HaltF
pattern ANew a x p = Anno a (NewF x p)
pattern ASend a s d p = Anno a (SendF s d p)
pattern ARecv a d s p = Anno a (RecvF d s p)
pattern ABoth a p q = Anno a (p :|:$ q)
pattern APick a p q = Anno a (p :+:$ q)
pattern ALoop a p = Anno a (LoopF p)
pattern AMatch a x arms = Anno a (MatchF x arms)
pattern AMatch' a x ys ps <- Anno a (MatchF x (L.unzip -> (ys, ps)))

-- -------------------- Variables --------------------

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
    Send s d p -> Send (σ ! s) (σ ! d) <$> go σ p
    Recv d s p -> do d' <- gen; Recv d' (σ ! s) <$> go (M.insert d d' σ) p
    p :|: q -> liftA2 (:|:) (go σ p) (go σ q)
    p :+: q -> liftA2 (:+:) (go σ p) (go σ q)
    Loop p -> Loop <$> go σ p
    Match x yps -> Match x <$> traverse (traverse (go σ)) yps
  σ ! x = M.findWithDefault x x σ
  gen = modify' succ *> get

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

-- -------------------- Liveness (from here on, assume UB) --------------------

type FVProcess = AProcess (Set Var)
pattern FV ws p <- ((\ p -> (p, p)) -> (p, Anno ws _))

fvAnno :: Process -> FVProcess
fvAnno = anno fvF

-- New sinking
sinkNews :: FVProcess -> FVProcess
sinkNews = fixed . fix $ \ go ->
  let rec x ps p = do tell (Any True); go . ANew (S.delete x ps) x =<< go p in
  \case
    ANew vs x (Anno ps p) | x ∉ ps -> go (Anno vs p)
    ANew vs x (ANew _ y (FV ps p)) -> ANew vs y <$> rec x ps p
    ANew vs x (ASend _ s d (FV ps p)) | x /= s && x /= d -> ASend vs s d <$> rec x ps p
    ANew vs x (ARecv _ d s (FV ps p)) | x /= s -> ARecv vs s d <$> rec x ps p
    ANew vs x (ABoth _ (FV ps p) (FV qs q)) | x ∉ ps -> ABoth vs <$> go p <*> rec x qs q
    ANew vs x (ABoth _ (FV ps p) (FV qs q)) | x ∉ qs -> ABoth vs <$> rec x ps p <*> go q
    ANew vs x (APick ws (FV ps p) (FV qs q)) -> APick vs <$> rec x ps p <*> rec x qs q
    ANew vs x (AMatch _ y arms) | x `L.notElem` (y : map fst arms) ->
      AMatch vs y <$> mapM (mapM (\ (FV ps p) -> rec x ps p)) arms
    p -> tell (Any False) $> p

-- -------------------- Register allocation --------------------

-- Interference constraint
data Constraint = Var :/=: Var deriving (Eq, Ord, Show)

clique :: Set Var -> Set Constraint
clique xs = S.fromList [x :/=: y | x : ys <- L.tails (S.toList xs), y <- ys]

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

-- Expected number of forks that happen in a variable's lifetime
forks :: Var -> FVProcess -> Double
forks x = \case
  AHalt _ -> 0
  ANew _ _ p -> forks x p
  ASend _ _ _ p -> forks x p
  ARecv _ _ _ p -> forks x p
  ABoth _ p (FV qs q) -> (if x ∈ qs then 1 else 0) + forks x p + forks x q
  APick _ p q -> forks x p / 2 + forks x q / 2
  ALoop _ p -> 100 * forks x p
  AMatch' _ _ _ ps -> maximum (0 : map (forks x) ps)

-- The variables in a process, sorted by forks
sortedVars :: FVProcess -> [Var]
sortedVars p = L.sortOn (`forks` p) (S.toList (uv (unanno p)))

varName :: Var -> String
varName x = "x" ++ show x

type Register = Int

registers :: [Register]
registers = [0, 1, 2, 3, 4]

regName :: Register -> String
regName = \case
  0 -> "rbx"
  1 -> "r12"
  2 -> "r13"
  3 -> "r14"
  4 -> "r15"
  n -> "spill" ++ show n

