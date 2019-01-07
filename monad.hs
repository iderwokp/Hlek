import Control.Monad

-- Disse gjør samme
sevensOnly1 = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)  

sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x  

sevensOnly2 = [ x | x <- [1..50], '7' `elem` show x ] 




cool_eller = guard (5 > 2) >> return "cool" :: [String]



---------------------------------------

type KnightPos = (Int,Int) 

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')  


in3 :: KnightPos -> [KnightPos]  
in3 start = do   
    first <- moveKnight start  
    second <- moveKnight first  
    moveKnight second  

-- Gjør det samme som den over
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight  

canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  

