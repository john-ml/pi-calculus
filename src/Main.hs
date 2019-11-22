import Process

println :: Show a => a -> IO ()
println x = do print x; putStrLn ""

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
