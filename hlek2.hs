import Data.List
import System.IO

hello 0 _ = do
    putStrLn ("------------------------------")
    
hello n name = do
    putStrLn ("Hello " ++ name)
    hello (n-1) name

hello10 = hello 10
