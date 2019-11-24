import Process
import Data.SBV
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Trans
import Control.Monad.State.Strict
import qualified Text.Megaparsec as P

println :: Show a => a -> IO ()
println x = do print x; putStrLn ""

testAlloc p = do
  let p' = sinkNews . fvAnno $ ub p
  println =<< alloc p'

testAlloc' p = do
  let p' = fvAnno $ ub p
  println =<< alloc p'

testSMT :: Symbolic ()
testSMT = do
  SatResult r@(Satisfiable c m) <- liftIO . sat $ do
    x :: SWord64 <- exists "x"
    y :: SWord64 <- exists "y"
    constrain $ x ./= y
    constrain $ x .< 10
    constrain $ y .< 10
  liftIO . println $ getModelDictionary r

printGen :: Process -> IO ()
printGen p = putStrLn =<< codeGen p

printParse :: String -> IO ()
printParse s = either putStrLn println $ parse s

printTranspile :: String -> IO ()
printTranspile s = either putStrLn putStrLn =<< transpile s

printTranspileFile :: FilePath -> IO ()
printTranspileFile f = either putStrLn putStrLn =<< transpileFile f

main = do
  println 0
  println . fvAnno . New 0 . New 1 . Send 0 2 . Send 1 2 $ Halt
  println . sinkNews . fvAnno . New 0 . New 1 . Send 0 2 . Send 1 2 $ Halt
  let p = New 0 . New 1 . Send 0 2 . New 0 . Send 1 2 . Send 0 2 $ Halt
  println $ ub p
  println . sinkNews . fvAnno $ ub p
  println . unanno . sinkNews . fvAnno $ ub p
  println . constraints . fvAnno $ ub p
  println . constraints . sinkNews . fvAnno $ ub p
  let q = New 0 . New 1 . Send 2 2 . Send 2 2 . Send 2 3 . Send 1 2 . Send 0 2 $ Halt
  println . fvAnno $ ub q
  println . sinkNews . fvAnno $ ub q
  println . unanno . sinkNews . fvAnno $ ub q
  println . constraints . fvAnno $ ub q
  println . constraints . sinkNews . fvAnno $ ub q
  testAlloc q
  runSMT testSMT
  let p = New 0 . New 1 . New 2 . New 3 . New 4 . New 5
            . Send 0 6 . Send 1 6 . Send 2 6 . Send 3 6 . Send 4 6 . Send 5 6
            $ Halt
  testAlloc' p
  testAlloc p
  putStrLn . runCode (M.fromList [(0, Spill 3), (1, Spill 4)])
    $ mainG (spillProcG (S.fromList [0, 1]) "f" "")
  printGen p
  printGen . New 1 $ New 0 (Loop (Recv 2 0 Halt) :|: Loop (Send 1 0 Halt))
  printParse "new x."
  let s = "new x y z; x <- y; match x { y => x -> y.\nz => y -> z. }"
  printParse s
  printTranspile s
  printTranspile "new x; match x <- y"
  printParse "new x;\nnew y;\nloop {\nx -> y.\n}\n"
  printTranspileFile "examples/loop.pi"
  printTranspileFile "examples/loop_faulty.pi"
  compileFile "examples/loop.pi" "examples/out/loop.c" "examples/out/loop"
  compileFile "examples/deadlock.pi" "examples/out/deadlock.c" "examples/out/deadlock"
