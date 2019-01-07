import Data.List
import System.IO

-- :set prompt "ghci> "

doubleMe x = x + x

----------------------------------------------------------
-- fib :: RealFloat  => [a]
fib = 1.0:1.0:[a + b | (a, b) <- zip fib (tail fib)]
fib30 = fib !! 30

-- Bedre fib?
fibs = 0:scanl (+) 1 fibs


fibdiv' :: RealFloat a => [a] -> [a]
fibdiv' [] = []
fibdiv' [e] = []
fibdiv' (x:xs) = [head xs/x] ++ fibdiv' xs


-----------------------------------------------------
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24] 
------------------------------------------------------------------------
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  
-------------------------------------

bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  

-------------------------------------------------------------

bmiTell2 :: (RealFloat a) => a -> a -> String  
bmiTell2 weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"  

-------------------------------------------------

bmiTell3 :: (RealFloat a) => a -> a -> String  
bmiTell3 weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  

--------------------------------------------------

bmiTell4 :: (RealFloat a) => a -> a -> String  
bmiTell4 weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  

-----------------------------------

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  
----------------------------------------------------------------------------------
describeList2 :: [a] -> String  
describeList2 xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  
--------------------------------------------------------------------
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  

---------------------------------------------------------

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

--------------------------------------------





