import Process

main = do
  print . sinkNews . New 0 . New 1 . Send 0 2 . Send 1 2 $ Halt
  print . ub . New 0 . New 1 . Send 0 2 . New 0 . Send 1 2 . Send 0 2 $ Halt
