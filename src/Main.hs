import Process
import Data.SBV
import qualified Data.Set as S
import Control.Monad.Trans

println :: Show a => a -> IO ()
println x = do print x; putStrLn ""

testAlloc p = do
  let p' = sinkNews . fvAnno $ ub p
  println =<< alloc (S.toList (uv (unanno p'))) (constraints p')

testAlloc' p = do
  let p' = fvAnno $ ub p
  println =<< alloc (S.toList (uv (unanno p'))) (constraints p')

testSMT :: Symbolic ()
testSMT = do
  SatResult r@(Satisfiable c m) <- liftIO . sat $ do
    x :: SWord64 <- exists "x"
    y :: SWord64 <- exists "y"
    constrain $ x ./= y
    constrain $ x .< 10
    constrain $ y .< 10
  liftIO . print $ getModelDictionary r

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
