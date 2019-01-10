
import Control.Concurrent

main = do
  done <- newEmptyMVar
  forkIO (do putStrLn "I'm one thread!"
             putMVar done "Done!")
  second <- forkIO (do threadDelay 50000
                       xx <- getLine
                       putStrLn "I'm another thread!")
  
  msg <- takeMVar done
  xx <- getLine
  killThread second
  putStrLn msg
  
  
  
  
  