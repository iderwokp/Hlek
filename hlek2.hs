import Data.List
import System.IO

hello 0 _ = do
    putStrLn ("------------------------------")
    
hello n name = do
    putStrLn ("Hello " ++ name)
    hello (n-1) name

hello10 = hello 10


-- En prime-generator. Ikke ment til å være mest effektiv, men ment til å vise div. ting i Haskell
primes = filterPrime [2..] 
  where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, x `mod` p /= 0]