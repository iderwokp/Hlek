import Data.Monoid  
import Control.Monad.Writer


applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)  


type Food = String  
type Price = Sum Int  
  
addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)  


logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Gonna multiply these two"]
    return (a*b)  

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
   | b == 0 = do
     tell ["Finished with " ++ show a]
     return a
   | otherwise = do
     tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
     gcd' b (a `mod` b)


-- mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)


--------------------------------------------------------

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
      tell ["Keeping " ++ show x]
      return True
    | otherwise = do
      tell [show x ++ " is too large, throwing it away"]
      return False


-- fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
-- mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]

------------------------------------------

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

-- powerset [1,2,3]