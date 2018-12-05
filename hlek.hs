import Data.List
import System.IO

-- :set prompt "ghci> "

doubleMe x = x + x


fib = 1:1:[a + b | (a, b) <- zip fib (tail fib)]
fib30 = fib !! 30

rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24] 

