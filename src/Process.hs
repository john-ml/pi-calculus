module Process where

import qualified Data.List as L
import Data.Set (Set, (\\)); import qualified Data.Set as S
import Data.Map.Strict (Map); import qualified Data.Map.Strict as M
import Data.Semigroup
import qualified Data.Foldable as F
import Data.Bifunctor
import Data.Functor
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Functor.Classes
import Data.Functor.Compose
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Trans.Maybe
import Control.Applicative
import Text.Show.Deriving

import Data.SBV
import qualified Data.SBV.Internals as SBVI

import Data.String (IsString (..))
import Data.DList (DList); import qualified Data.DList as D

import Data.Char
import Data.Void
import Text.Megaparsec (ParsecT, MonadParsec)
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified System.Process as P

type Var = Word64

data ForeignExp' a
  = Atom a
  | Call String [ForeignExp' a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type ForeignExp = ForeignExp' Var

data Process
  = Halt
  | New Var Process
  | Send Var Var Process
  | Recv Var Var Process
  | Eval Var ForeignExp Process
  | Do ForeignExp Process
  | Process :|: Process
  | Process :+: Process
  | Loop Process
  | Match Var [(Var, Process)]
  | Foreign String Process
  deriving (Eq, Ord, Show)

makeBaseFunctor ''Process
deriveShow1 ''ProcessF

pattern Match' x ys zs <- Match x (L.unzip -> (ys, zs))

-- -------------------- Utils --------------------

for :: [a] -> (a -> b) -> [b]
for xs f = map f xs

for2 :: [a] -> [b] -> (a -> b -> c) -> [c]
for2 xs ys f = zipWith f xs ys

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
    EvalF x e (Anno a _) -> ann (f (EvalF x e a))
    DoF e (Anno a _) -> ann (f (DoF e a))
    Anno a _ :|:$ Anno b _ -> ann (f (a :|:$ b))
    Anno a _ :+:$ Anno b _ -> ann (f (a :+:$ b))
    LoopF (Anno a _) -> ann (f (LoopF a))
    MatchF x arms -> ann (f (MatchF x (map (\ (y, Anno a _) -> (y, a)) arms)))
    ForeignF body (Anno a _) -> ann (f (ForeignF body a))

anno2 ::
  (Base Process a -> a) -> (Base Process b -> b) ->
  Process -> Fix (AnnoF (a, b) (Base Process))
anno2 f g = cata $ \ p ->
  let ann x y = Anno (f x, g y) p in
  case p of
    HaltF -> ann HaltF HaltF
    NewF x (Anno (a, b) _) -> ann (NewF x a) (NewF x b)
    SendF s d (Anno (a, b) _) -> ann (SendF s d a) (SendF s d b)
    RecvF d s (Anno (a, b) _) -> ann (RecvF d s a) (RecvF d s b)
    EvalF x e (Anno (a, b) _) -> ann (EvalF x e a) (EvalF x e b)
    DoF e (Anno (a, b) _) -> ann (DoF e a) (DoF e b)
    Anno (a1, b1) _ :|:$ Anno (a2, b2) _ -> ann (a1 :|:$ a2) (b1 :|:$ b2)
    Anno (a1, b1) _ :+:$ Anno (a2, b2) _ -> ann (a1 :+:$ a2) (b1 :+:$ b2)
    LoopF (Anno (a, b) _) -> ann (LoopF a) (LoopF b)
    MatchF x arms ->
      ann
        (MatchF x (map (\ (y, Anno (a, _) _) -> (y, a)) arms))
        (MatchF x (map (\ (y, Anno (_, b) _) -> (y, b)) arms))
    ForeignF body (Anno (a, b) _) -> ann (ForeignF body a) (ForeignF body b)

unanno :: Fix (AnnoF a (Base Process)) -> Process
unanno = hoist (snd . getCompose)

-- Label every node
type AProcess a = Fix (AnnoF a (Base Process))
pattern AHalt a = Anno a HaltF
pattern ANew a x p = Anno a (NewF x p)
pattern ASend a s d p = Anno a (SendF s d p)
pattern ARecv a d s p = Anno a (RecvF d s p)
pattern AEval a x e p = Anno a (EvalF x e p)
pattern ADo a e p = Anno a (DoF e p)
pattern ABoth a p q = Anno a (p :|:$ q)
pattern APick a p q = Anno a (p :+:$ q)
pattern ALoop a p = Anno a (LoopF p)
pattern AMatch a x arms = Anno a (MatchF x arms)
pattern AMatch' a x ys ps <- Anno a (MatchF x (L.unzip -> (ys, ps)))
pattern AForeign a body p = Anno a (ForeignF body p)

-- -------------------- Variables --------------------

-- Generic fold over variables
foldVars :: Monoid m => (Var -> m) -> Process -> m
foldVars m = cata $ \case
  HaltF -> mempty
  NewF x my -> m x <> my
  SendF x y mz -> m x <> m y <> mz
  RecvF x y mz -> m x <> m y <> mz
  EvalF x e my -> m x <> foldMap m e <> my
  DoF e my -> foldMap m e <> my
  mx :|:$ my -> mx <> my
  mx :+:$ my -> mx <> my
  LoopF mx -> mx
  MatchF x (L.unzip -> (ys, mzs)) -> mconcat (m x : map m ys ++ mzs)
  ForeignF _ mx -> mx

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
    Eval x e p -> do x' <- gen; Eval x' ((σ !) <$> e) <$> go (M.insert x x' σ) p
    Do e p -> Do ((σ !) <$> e) <$> go σ p
    p :|: q -> liftA2 (:|:) (go σ p) (go σ q)
    p :+: q -> liftA2 (:+:) (go σ p) (go σ q)
    Loop p -> Loop <$> go σ p
    Match x yps -> Match (σ ! x) <$> mapM (\ (y, p) -> (σ ! y,) <$> go σ p) yps
    Foreign body p -> Foreign body <$> go σ p
  σ ! x = M.findWithDefault x x σ
  gen = modify' succ *> get

-- Free variables
fvF :: Base Process (Set Var) -> Set Var
fvF = \case
  HaltF -> S.empty
  NewF x vs -> S.delete x vs
  SendF s d vs -> S.insert s (S.insert d vs)
  RecvF d s vs -> S.insert s (S.delete d vs)
  EvalF x e vs -> foldMap S.singleton e ∪ S.delete x vs
  DoF e vs -> foldMap S.singleton e
  vs :|:$ ws -> vs ∪ ws
  vs :+:$ ws -> vs ∪ ws
  LoopF vs -> vs
  MatchF x (L.unzip -> (xs, vss)) -> S.fromList (x : xs) ∪ F.fold vss
  ForeignF _ vs -> vs

fv :: Process -> Set Var
fv = cata fvF

-- -------------------- Liveness (from here on, assume UB) --------------------

-- We want to annotate each AST node with a set of free variables + whether
-- the process executes arbtirary C code (using Eval) during its lifetime.
type AnnProcess = AProcess (Set Var, Any)
pattern FV ws p <- ((\ p -> (p, p)) -> (p, Anno (ws, _) _))
pattern Evals b p <- ((\ p -> (p, p)) -> (p, Anno (_, Any b) _))
pattern Anns a p <- ((\ p -> (p, p)) -> (p, Anno a _))

evalF :: Base Process Any -> Any
evalF = \case
  HaltF -> Any False
  NewF _ p -> p
  SendF _ _ p -> p
  RecvF _ _ p -> p
  EvalF _ _ _ -> Any True
  DoF _ _ -> Any True
  -- We fork the 2nd process, so *this* process's Eval-ness doesn't depend on it
  p :|:$ _ -> p
  -- We could choose either, so we depend on both
  p :+:$ q -> p <> q
  LoopF p -> p
  MatchF _ (L.unzip -> (_, ps)) -> F.fold ps
  ForeignF _ p -> p

fvAnno :: Process -> AnnProcess
fvAnno = anno2 fvF evalF

-- New sinking
sinkNews :: AnnProcess -> AnnProcess
sinkNews = fixed . fix $ \ go ->
  let rec' x a p = go . ANew (first (S.delete x) a) x =<< go p in
  let rec x a p = do tell (Any True); rec' x a p in
  \case
    ANew vs x (Anno (ps, _) p) | x ∉ ps -> go (Anno vs p)
    ANew vs x (ANew _ y (Anns a p)) -> ANew vs y <$> rec' x a p
    ANew vs x (ASend _ s d (Anns a p)) | x /= s && x /= d -> ASend vs s d <$> rec x a p
    ANew vs x (ARecv _ d s (Anns a p)) | x /= s -> ARecv vs s d <$> rec x a p
    ANew _ x (AEval (ws, b) y e (Anns ap@(ps, _) p)) | x ∉ (ws S.\\ ps) -> AEval (ws, b) y e <$> rec x ap p
    ANew _ x (ADo (ws, b) e (Anns ap@(ps, _) p)) | x ∉ (ws S.\\ ps) -> ADo (ws, b) e <$> rec x ap p
    ANew vs x (ABoth _ (FV ps p) (Anns aq q)) | x ∉ ps -> ABoth vs <$> go p <*> rec x aq q
    ANew vs x (ABoth _ (Anns ap p) (FV qs q)) | x ∉ qs -> ABoth vs <$> rec x ap p <*> go q
    ANew vs x (APick _ (Anns ap p) (Anns aq q)) -> APick vs <$> rec x ap p <*> rec x aq q
    ANew vs x (AMatch _ y arms) | x `L.notElem` (y : map fst arms) ->
      AMatch vs y <$> mapM (mapM (\ (Anns ap p) -> rec x ap p)) arms
    ANew vs x (AForeign _ body (Anns ap p)) -> AForeign vs body <$> rec x ap p
    p -> tell (Any False) $> p

-- -------------------- Register allocation --------------------

-- Interference constraint
data Constraint = Var :/=: Var deriving (Eq, Ord, Show)

clique :: Set Var -> Set Constraint
clique xs = S.fromList [x :/=: y | x : ys <- L.tails (S.toList xs), y <- ys]

-- Collect interference constraints
constraints :: AnnProcess -> Set Constraint
constraints = go S.empty where
  go s = \case
    AHalt (clique' -> vs) -> vs
    ANew (clique' -> vs) _ p -> vs ∪ go s p
    ASend (clique' -> vs) _ _ p -> vs ∪ go s p
    ARecv (clique' -> vs) _ _ p -> vs ∪ go s p
    AEval (clique' -> vs) _ _ p -> vs ∪ go s p
    ADo (clique' -> vs) _ p -> vs ∪ go s p
    ABoth (clique' -> vs) p q -> vs ∪ go s p ∪ go s q
    APick (clique' -> vs) p q -> vs ∪ go s p ∪ go s q
    -- The tricky one: whatever is live now must still be live throughout body
    ALoop (clique'' -> (s', vs)) p -> vs ∪ go (s ∪ s') p 
    AMatch' (clique' -> vs) _ _ ps -> vs ∪ foldMap (go s) ps
    AForeign _ _ ps -> go s ps
    where
      clique' a = snd $ clique'' a
      clique'' (xs, _) = (xs, clique $ s ∪ xs)

-- Expected number of forks that happen in a variable's lifetime
forks :: Var -> AnnProcess -> Double
forks x = fix $ \ go -> \case
  AHalt _ -> 0
  ANew _ _ p -> go p
  ASend _ _ _ p -> go p
  ARecv _ _ _ p -> go p
  AEval _ _ _ p -> go p
  ADo _ _ p -> go p
  ABoth _ p (FV qs q) -> (if x ∈ qs then 1 else 0) + go p + go q
  APick _ p q -> go p / 2 + go q / 2
  ALoop _ p -> 100 * go p
  AMatch' _ _ _ ps -> maximum (0 : map go ps)
  AForeign _ _ ps -> go ps

-- The variables in a process, sorted in increasing order by forks
sortedVars :: AnnProcess -> [Var]
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

alloc' :: Bool -> AnnProcess -> IO Alloc
alloc' True p = allocWith (sortedVars p) (constraints p) >>= \case
  Nothing -> error $ "Register allocation failed."
  Just m -> return m
alloc' False p = return . M.fromList $ zip (S.toList . uv $ unanno p) (Spill <$> [0..])

alloc :: AnnProcess -> IO Alloc
alloc = alloc' True

-- -------------------- C code formatting utils --------------------

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

-- -------------------- Code generation utils --------------------

varG :: Var -> Code
varG x = (M.! x) . fst <$> ask >>= \case
  Rbx -> pure "rbx"
  R12 -> pure "r12"
  R13 -> pure "r13"
  R14 -> pure "r14"
  R15 -> pure "r15"
  Spill n -> pure $ "spill" <> show' n

declG :: Str -> Var -> Code
declG ty x = (M.! x) . fst <$> ask >>= \case
  Spill _ -> pure ty <> " " <> varG x
  _ -> varG x

procG :: Code -> Code -> Code
procG name body = F.fold
  [ line' ("void " <> name <> "(void) {")
  , indent body
  , indent $ line "asm (\"jmp gt_stop\\t\\n\");"
  , line "}"
  ]

spillProcG :: Set Var -> Code -> Code -> Code
spillProcG spilled name body = procG name $ F.fold
  [ line "gt_ch *rsp = (gt_ch *)gt_self()->rsp + 1;"
  , F.fold . for2 [0..] (S.toAscList spilled) $ \ offset x ->
      line' $ "gt_ch " <> varG x <> " = rsp[" <> show'' offset <> "];"
  , body
  ]

mainG :: Code -> Code
mainG body = F.fold
  [ line "int main(void) {"
  , indent $ F.fold
    [ line "gt_init();"
    , body
    , line "gt_exit(0);"
    ]
  , line "}"
  ]

-- -------------------- Code generation --------------------

type Gen =
  ReaderT Alloc -- Result of allocation
  (StateT Word64 -- Fresh names for helper functions
  (Writer Code)) -- Maintain helper functions generated along the way

gensym :: Str -> Gen Str
gensym name = ("var_" <>) . (name <>) . show' <$> get <* modify' succ

newG :: Var -> Code
newG x = line' $ declG "gt_ch" x <> " = gt_chan();"

sendG :: Var -> Var -> Code
sendG s d = line' $ "gt_write(" <> varG d <> ", " <> varG s <> ");"

recvG :: Var -> Var -> Code
recvG d s = line' $ declG "gt_ch" d <> " = gt_read(" <> varG s <> ");"

foreignExpG :: ForeignExp -> Code
foreignExpG = \case
  Atom x -> varG x
  Call f xs ->
    pure (D.fromList f) <> "(" <>
      F.fold (L.intersperse "," (foreignExpG <$> xs)) <> ")"

evalG :: Var -> ForeignExp -> Code
evalG x e = line' $ declG "gt_ch" x <> " = " <> foreignExpG e <> ";"

doG :: ForeignExp -> Code
doG e = line' $ foreignExpG e <> ";"

bothG :: AnnProcess -> Set Var -> AnnProcess -> Gen Code
bothG p qs q = do
  f <- gensym "f"
  t <- gensym "t"
  rsp <- gensym "rsp"
  p' <- gen p
  q' <- gen q
  alloc <- ask
  let (spilled, unspilled) = S.partition (wasSpilled alloc) qs
  let pG = if S.null spilled then procG else spillProcG spilled
  tell $ pG (pure f) q'
  return $ F.fold
    [ line $ "gt_t " <> t <> " = " <> call f spilled
    , F.fold . for (S.toAscList unspilled) $ \ v ->
        line' $ pure t <> "->" <> varG v <> " = " <> varG v <> ";"
    , if not $ S.null spilled
      then line $ "gt_ch *" <> rsp <> " = ((gt_ch *)" <> t <> "->rsp) + 1;"
      else ""
    , F.fold . for2 [0..] (S.toAscList spilled) $ \ offset v ->
        line' $ pure rsp <> "[" <> show'' offset <> "] = " <> varG v <> ";"
    , p'
    ]
  where
    call :: Str -> Set Var -> Str
    call f spilled
      | S.null spilled = "gt_go(" <> f <> ", " <> show' (stackSize spilled q) <> ");"
      | otherwise = "gt_go_alloca(" <>
          f <> ", " <> show' (spillSize spilled) <> ", " <>
          show' (stackSize spilled q) <> ");"

    wasSpilled :: Alloc -> Var -> Bool
    wasSpilled alloc q =
      case alloc M.!? q of
        Just (Spill _) -> True
        _ -> False

    spillSize :: Set Var -> Int
    spillSize spilled = 16 * ((S.size spilled + 1) `div` 2)
    
    stackSize :: Set Var -> AnnProcess -> Int
    stackSize spilled = \case
      Evals True _ -> 0x100000 + spillSize spilled
      Evals False _ -> 64 + spillSize spilled

gen :: AnnProcess -> Gen Code
gen = \case
  AHalt _ -> pure ""
  ANew _ x p -> (newG x <>) <$> gen p
  ASend _ s d p -> (sendG s d <>) <$> gen p
  ARecv _ d s p -> (recvG d s <>) <$> gen p
  AEval _ x e p -> (evalG x e <>) <$> gen p
  ADo _ e p -> (doG e <>) <$> gen p
  ABoth _ p (FV qs q) -> bothG p qs q
  APick _ p q -> do
    p' <- gen p
    q' <- gen q
    return $ F.fold
      [ line "if (rand() & 1) {"
      , indent p'
      , line "} else {"
      , indent q'
      , line "}"
      ]
  ALoop _ p -> do
    p' <- gen p
    return $ F.fold
      [ line "for (;;) {"
      , indent p'
      , line "}"
      ]
  AMatch _ x yps -> do
    let x' = varG x
    yps' <- forM yps $ \ (y, p) -> (varG y, ) <$> gen p
    return $ F.fold
      [ line $ "if (0) {}"
      , F.fold . for yps' $ \ (y', p') -> F.fold
        [ line' $ "else if (" <> x' <> " == " <> y' <> ") {"
        , indent p'
        , line "}"
        ]
      ]
  AForeign _ body p -> do
    tell . foldMap (line . D.fromList) $ lines body
    gen p

genTop :: AnnProcess -> Gen Code
genTop (FV vs p) = do
  tell $ line "#include <stdlib.h>"
  tell $ line "#include \"runtime.c\""
  mainG <$> gen (ABoth (vs, Any False) (AHalt (S.empty, Any False)) p)

runGen :: Alloc -> Gen Code -> String
runGen alloc m =
  let (main, helpers) = runWriter $ m `runReaderT` alloc `evalStateT` 0 in
  runCode alloc (helpers <> main)

-- -------------------- AST Compilation --------------------

codeGen' :: Bool -> Process -> IO String
codeGen' sinking p = do
  let p' = (if sinking then sinkNews else id) . fvAnno $ ub p
  a <- alloc p'
  return $ runGen a (genTop p')

codeGen :: Process -> IO String
codeGen = codeGen' True

-- -------------------- Parsing utils --------------------

newtype PError = PError String deriving (Eq, Ord)

type Parser = ParsecT PError String (State (Map String Word64))

instance P.ShowErrorComponent PError where
  showErrorComponent (PError s) = s

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

tryAll :: (Foldable f, MonadParsec e s m) => f (m a) -> m a
tryAll = foldr ((<|>) . P.try) empty

symbols :: [String] -> Parser String
symbols = tryAll . fmap symbol

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = P.between (symbol "{") (symbol "}")

-- -------------------- Parsing --------------------

keywords :: [String]
keywords = ["new", "all", "any", "loop", "match", "foreign", "do"]

word :: Parser String
word = do
  s <- lexeme $ some (alphaNumChar <|> char '_')
  guard . not $ s `elem` keywords
  return s

varP' :: Bool -> Parser Var
varP' strict = do
  x <- word
  (M.!? x) <$> get >>= \case
    Nothing | strict ->
      P.customFailure . PError $ "Variable not in scope: " ++ x
    Nothing -> do
      n <- fromIntegral . M.size <$> get
      modify' (M.insert x n)
      return n
    Just n -> return n

varP :: Parser Var = varP' True

bindP :: Parser Var = varP' False

haltP :: Parser Process = Halt <$ symbol "."

contP :: Parser Process = P.try haltP <|> symbol ";" *> procP

newP :: Parser Process = symbol "new" >> mkNew <$> some bindP <*> contP where
  mkNew xs p = foldr New p xs

sendP :: Parser Process = Send <$> varP <* symbol "->" <*> varP <*> contP

recvP :: Parser Process = Recv <$> bindP <* symbol "<-" <*> varP <*> contP

binopP :: String -> (Process -> Process -> Process) -> Parser Process
binopP keyword op = symbol keyword >> mk <$> braces (many procP) where
  mk = \case
    [] -> Halt
    [p] -> p
    x:xs -> L.foldl' op x xs

anyP :: Parser Process = binopP "any" (:+:)

allP :: Parser Process = binopP "all" (:|:)

loopP :: Parser Process = symbol "loop" >> Loop <$> procP

matchP :: Parser Process
matchP = symbol "match" >> Match <$> varP <*> braces (many armP) where
  armP = (,) <$> varP <* symbol "=>" <*> procP

foreignP :: Parser Process
foreignP = symbol "foreign" >> symbol "{" >> Foreign <$> suffix 0 <*> procP where
  suffix n = (++) <$> P.takeWhileP Nothing nonBrace <*> bodyP n
  bodyP n = tryAll
    [ (++) <$> string "{" <*> suffix (n + 1)
    , string "}" >>
        if n == 0
        then sc $> ""
        else (\ x y -> "}" ++ x ++ y) <$> spaces <*> suffix (n - 1)
    ]
  nonBrace = \case
    '{' -> False
    '}' -> False
    _ -> True
  spaces = P.takeWhileP Nothing isSpace

foreignExpP :: Parser ForeignExp
foreignExpP = Call <$> word <*> many argP where
  argP = P.try (parens foreignExpP) <|> (Atom <$> varP)

evalP :: Parser Process
evalP = Eval <$> bindP <* symbol "<~" <*> foreignExpP <*> contP

doP :: Parser Process
doP = symbol "do" >> Do <$> foreignExpP <*> contP

procP :: Parser Process
procP = tryAll
  [ -- Try stuff that starts with keywords first...
    newP, doP, anyP, allP, loopP, matchP, foreignP
  , -- ...before the stuff with arrows in them
    sendP, recvP, evalP
  ]

parse' :: String -> String -> Either String Process
parse' fname s =
  first P.errorBundlePretty
    $ P.runParserT (procP <* P.eof) fname s `evalState` M.empty

parse :: String -> Either String Process
parse s = parse' "" s

parseFile :: FilePath -> IO (Either String Process)
parseFile f = parse' f <$> readFile f

-- -------------------- Compilation to C --------------------

transpile :: String -> IO (Either String String)
transpile s = mapM codeGen (parse s)

transpileFile :: FilePath -> IO (Either String String)
transpileFile f = parseFile f >>= \case
  Left err -> return $ Left err
  Right p -> Right <$> codeGen p

-- -------------------- Full compilation --------------------

compile :: String -> FilePath -> FilePath -> IO ()
compile s cOut binOut = transpile s >>= \case
  Left err -> putStrLn err
  Right c -> do
    writeFile cOut c
    let flags = ["-O2", "-g", "-I", "runtime", "runtime/gt_switch.s", cOut, "-o", binOut]
    P.createProcess (P.proc "gcc" flags)
    return ()

compileFile :: FilePath -> FilePath -> FilePath -> IO ()
compileFile piIn cOut binOut = do
  s <- readFile piIn
  compile s cOut binOut
