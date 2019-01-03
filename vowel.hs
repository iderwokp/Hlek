

ll = length.filter (`elem` "aeiouy")
-- Brukes slik: ll "navn"

vow_len :: String -> int
vow_len [] = 0
vow_len s = length. isVow 
    

isVow :: [a]-> [Bool]
isVow [] = [False]
isVow (x:xs) = x `elem` "aeiouy" :isVow xs 