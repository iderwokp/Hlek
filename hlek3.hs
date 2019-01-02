-- import Data.Char  
-- import Data.List 
import Control.Applicative

a = [(+ 10), (* 4), (/ 2)] <*> [1,2,3,4]
b = [(+),(*)] <*> [1,2] <*> [3,4]  
c = (++) <$> ["ha","heh","hmm"] <*> ["?","!","."] 

-- Disse to gjør det samme
d1 = [ x*y | x <- [2,5,10], y <- [8,10,11]] 
d2 = (*) <$> [2,5,10] <*> [8,10,11]  
-- Samme, men med filter
d3filter = filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11] 


myAction :: IO String  
myAction = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b 


myAction2 :: IO String  
myAction2 = (++) <$> getLine <*> getLine  

main2 = do  
    a <- (++) <$> getLine <*> getLine  
    putStrLn $ "The two lines concatenated turn out to be: " ++ a  



k = (+) <$> (+3) <*> (*100) $ 5

l = (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5  

m = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100] 
n = getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  


-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
-- liftA2 f a b = f <$> a <*> b  
lif = liftA2 (:) (Just 3) (Just [4]) 


sequenceA' :: (Applicative f) => [f a] -> f [a]  
sequenceA' [] = pure []  
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs  

-- Gjør det samme:
sequenceA2 :: (Applicative f) => [f a] -> f [a]  
sequenceA2 = foldr (liftA2 (:)) (pure [])  

s1 = sequenceA' [Just 3, Just 2, Just 1] 
s2 = sequenceA2 [Just 3, Just 2, Just 1] 

s3 = sequenceA' [[1,2,3],[4,5,6]]

-- Disse to gjør det samme
m1 = map (\f -> f 7) [(>4),(<10),odd]  
s4 = sequenceA [(>4),(<10),odd] 7 

s5 = sequenceA [getLine, getLine, getLine]  

z1 = [(+1),(*100),(*5)] <*> [1,2,3]  

z2 = getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3] 
-- z3 = ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3] 
