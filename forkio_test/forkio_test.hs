
import Control.Concurrent

main = do forkIO $ (print . sum) [1..10000000]
          forkIO $ print $ "Hello"
          _ <- getLine
          print $ "Ferdig"
          

