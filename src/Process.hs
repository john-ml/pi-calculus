module Process where

import qualified Data.List as L
import Data.Set (Set, (\\)); import qualified Data.Set as S
import Data.Map.Strict (Map); import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified Data.Foldable as F
import Data.Functor
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Functor.Classes
import Data.Functor.Compose
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Trans.Maybe
import Control.Applicative
import Text.Show.Deriving

import Data.SBV
import qualified Data.SBV.Internals as SBVI

import Data.String (IsString (..))
import Data.DList (DList); import qualified Data.DList as D

type Var = Word64

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
forks x = fix $ \ go -> \case
  AHalt _ -> 0
  ANew _ _ p -> go p
  ASend _ _ _ p -> go p
  ARecv _ _ _ p -> go p
  ABoth _ p (FV qs q) -> (if x ∈ qs then 1 else 0) + go p + go q
  APick _ p q -> go p / 2 + go q / 2
  ALoop _ p -> 100 * go p
  AMatch' _ _ _ ps -> maximum (0 : map go ps)

-- The variables in a process, sorted in increasing order by forks
sortedVars :: FVProcess -> [Var]
sortedVars p = L.sortOn (`forks` p) (S.toList (uv (unanno p)))

varName :: Var -> String
varName x = "x" ++ show x

data Register = Rbx | R12 | R13 | R14 | R15 | Spill Word64 deriving (Eq, Ord, Show)
type Alloc = Map Var Register

intOfReg :: Register -> Word64
intOfReg = \case Rbx -> 0; R12 -> 1; R13 -> 2; R14 -> 3; R15 -> 4; Spill n -> n + 5

regOfWord64 :: Word64 -> Register
regOfWord64 = \case 0 -> Rbx; 1 -> R12; 2 -> R13; 3 -> R14; 4 -> R15; n -> Spill (n - 5)

spill :: Word64 -> Register
spill n = Spill (n + 5)

asgts :: Provable a => a -> Symbolic (Maybe Alloc)
asgts m = liftIO (sat m) >>= \case
  model@(SatResult (Satisfiable _ _)) ->
    return . Just
      . M.mapKeys varOfString
      $ regOfWord64 . SBVI.fromCV <$> getModelDictionary model
  _ -> return Nothing
  where
    varOfString ('x':n) = read n :: Word64

tryAlloc ::
  [Var] -> [Var] -> Word64 -> Set Constraint ->
  Symbolic (Maybe Alloc)
tryAlloc spillable unspillable maxSpills interference = asgts $ do
  let vars' = spillable ++ unspillable
  vars <- M.fromList . zip vars' <$> mapM (exists . varName) vars'
  mapM_ (\ (x :/=: y) -> constrain $ vars M.! x ./= vars M.! y) interference
  mapM_ (\ x -> constrain $ vars M.! x .< literal (5 + maxSpills)) spillable
  mapM_ (\ x -> constrain $ vars M.! x .< literal 5) unspillable

bsearchM :: (Monad m, Integral a) => (a -> m (Maybe b)) -> a -> a -> a -> m (Maybe (a, b))
bsearchM f lo mid hi = go' lo mid hi =<< f mid where
  rec lo hi = go lo mid hi =<< f mid where mid = (lo + hi) `div` 2
  go lo mid _ m | lo == mid = return $ (mid,) <$> m
  go lo mid hi m = go' lo mid hi m
  go' _ mid hi Nothing = rec mid hi
  go' lo mid _ (Just r) = rec lo mid >>= \case
    Just r' -> return $ Just r'
    Nothing -> return $ Just (mid, r)

allocWith :: [Var] -> Set Constraint -> IO (Maybe Alloc)
allocWith vars interference = runSMT . runMaybeT $ do
  -- 1) Find the smallest number of spills needed.
  (spills, r) <- MaybeT $ bsearchM spilling 0 (guessSpill $ length vars) (1 + length vars)
  -- 2) Find the smallest number of spill slots needed.
  (_, r) <- MaybeT $ bsearchM (shrinking spills) 0 (guessSlot $ maxSp) maxSp
  return r
  where
    -- Maximum spill slots
    maxSp = 10
    -- Initial guess for number of spills.
    guessSpill x = x `div` 10
    -- Initial guess for number of slots.
    guessSlot x = (x `div` 10) `max` 2
    spilling spills =
      let (sp, unsp) = L.genericSplitAt spills vars in
      tryAlloc sp unsp maxSp interference
    shrinking spills slots =
      let (sp, unsp) = L.genericSplitAt spills vars in
      tryAlloc sp unsp slots interference

alloc :: FVProcess -> IO Alloc
alloc p = allocWith (sortedVars p) (constraints p) >>= \case
  Nothing -> error $ "Register allocation failed."
  Just m -> return m

-- -------------------- Code generation utils --------------------

type Str = DList Char -- For efficient catenation

-- Indentation and allocation as input
type Code = Reader (Alloc, Str) Str
deriving instance Semigroup a => Semigroup (Reader r a)
deriving instance Monoid a => Monoid (Reader r a)

show' :: Show a => a -> Str
show' = D.fromList . show

show'' :: Show a => a -> Code
show'' = pure . show'

runCode :: Alloc -> Code -> String
runCode alloc c = D.toList $ c `runReader` (alloc, "")

instance IsString Code where fromString = pure . D.fromList

indent :: Code -> Code
indent = local (fmap ("  " <>))

line :: Str -> Code
line l = reader $ \ (_, s) -> s <> l <> "\n"

line' :: Code -> Code
line' l = reader $ \ r@(_, s) -> s <> runReader l r <> "\n"

-- -------------------- Code generation --------------------

varG :: Var -> Code
varG x = (M.! x) . fst <$> ask >>= \case
  Rbx -> pure "rbx"
  R12 -> pure "r12"
  R13 -> pure "r13"
  R14 -> pure "r14"
  R15 -> pure "r15"
  Spill n -> pure $ "spill" <> show' n

procG :: Str -> Code -> Code
procG name body = F.fold
  [ line ("void " <> name <> "(void) {")
  , indent body
  , line "}"
  ]

spillProcG :: Str -> Set Var -> Code -> Code
spillProcG name spilled body = procG name $ F.fold
  [ line "gt_ch *rsp = gt_self()->rsp;"
  , F.fold (zipWith mkSpill [0..] (S.toAscList spilled))
  , body
  , line $ "asm (\"addq $" <> show' spillBytes <> ", %%rsp\\t\\n\" : : : \"rsp\");"
  ]
  where
    mkSpill offset x =
      line' $ "gt_ch " <> varG x <> " = rsp[" <> show'' offset <> "];"
    spillBytes = 16 * ((S.size spilled + 1) `div` 2)

mainG :: Code -> Code
mainG body = F.fold
  [ line "void main(void) {"
  , indent $ F.fold
      [ line "gt_init();"
      , body
      , line "gt_exit(0);"
      ]
  , line "}"
  ]

-- codegen :: 
