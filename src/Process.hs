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
    Match x yps -> Match (σ ! x) <$> mapM (\ (y, p) -> (σ ! y,) <$> go σ p) yps
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
  let rec' x ps p = go . ANew (S.delete x ps) x =<< go p in
  let rec x ps p = do tell (Any True); rec' x ps p in
  \case
    ANew vs x (Anno ps p) | x ∉ ps -> go (Anno vs p)
    ANew vs x (ANew _ y (FV ps p)) -> ANew vs y <$> rec' x ps p
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

spillSize :: Set Var -> Int
spillSize spilled = 16 * ((S.size spilled + 1) `div` 2)

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

bothG :: FVProcess -> Set Var -> FVProcess -> Gen Code
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
    call f spilled
      | S.null spilled = "gt_go(" <> f <> ", 0x100000);" -- TODO: stack size
      | otherwise = "gt_go_alloca(" <>
          f <> ", " <>
          show' (spillSize spilled) <> ", " <>
          "0x100000);"
    wasSpilled alloc q =
      case alloc M.!? q of
        Just (Spill _) -> True
        _ -> False

gen :: FVProcess -> Gen Code
gen = \case
  AHalt _ -> pure ""
  ANew _ x p -> (newG x <>) <$> gen p
  ASend _ s d p -> (sendG s d <>) <$> gen p
  ARecv _ d s p -> (recvG d s <>) <$> gen p
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

genTop :: FVProcess -> Gen Code
genTop (FV vs p) = do
  tell $ line "#include <stdlib.h>"
  tell $ line "#include \"runtime.c\""
  mainG <$> gen (ABoth vs (AHalt S.empty) p)

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
keywords = ["new", "all", "any", "loop", "match"]

word :: Parser String
word = do
  s <- lexeme $ some alphaNumChar
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

loopP :: Parser Process = symbol "loop" >> Loop <$> braces procP

matchP :: Parser Process
matchP = symbol "match" >> Match <$> varP <*> braces (many armP) where
  armP = (,) <$> varP <* symbol "=>" <*> procP

procP :: Parser Process
procP = tryAll [newP, sendP, recvP, anyP, allP, loopP, matchP]

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
    let flags = ["-I", "runtime", "runtime/gt_switch.s", cOut, "-o", binOut]
    P.createProcess (P.proc "gcc" flags)
    return ()

compileFile :: FilePath -> FilePath -> FilePath -> IO ()
compileFile piIn cOut binOut = do
  s <- readFile piIn
  compile s cOut binOut
