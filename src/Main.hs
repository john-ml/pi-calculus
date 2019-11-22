import Process

main = do
  print 0
  print . fvAnno . New 0 . New 1 . Send 0 2 . Send 1 2 $ Halt
  print . sinkNews . fvAnno . New 0 . New 1 . Send 0 2 . Send 1 2 $ Halt
  let p = New 0 . New 1 . Send 0 2 . New 0 . Send 1 2 . Send 0 2 $ Halt
  print $ ub p
  -- print . constraints . fvAnno $ ub p
  print . sinkNews . fvAnno $ ub p
  print . sinkNews . fvAnno $ ub p
  let q = New 0 . New 1 . Send 2 2 . Send 2 2 . Send 2 3 . Send 1 2 . Send 0 2 $ Halt
  print . sinkNews . fvAnno $ ub q
  -- print . constraints . ub . fvAnno $ sinkNews p
