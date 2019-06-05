-- import Data.Strings
import Data.Char
import Control.Monad
-- import Data.String.Utils
-- import Data.Text(pack, unpack, replace)

romDigit ::Char-> Int
-- romDigit [] = 0
romDigit s 
    | s == 'I' = 1
    | s == 'V' = 5
    | s == 'X' = 10
    | s == 'L' = 50
    | s == 'C' = 100
    | s == 'D' = 500
    | s == 'M' = 1000
    | otherwise  = 0



getThunk :: [Char] -> [Char]
getThunk [] = []
getThunk [x] = [x]
getThunk (x:y:xs) 
    |romDigit x <= romDigit y = x : getThunk (y:xs)
    |otherwise = [x]



thunks :: String -> [String]
thunks [] = []
thunks xs = getThunk xs : thunks ( drop ( length $ getThunk xs )  xs ) 

calcThunks :: [Char] -> Int
calcThunks [] = 0
calcThunks [x] = romDigit x
calcThunks xs = calc (reverse xs)
    where calc [] = 0
          calc [a] = romDigit a
          calc (a:b:ys) 
                       | romDigit a > romDigit b = romDigit a - romDigit b - calc ys
                       | otherwise = romDigit a + romDigit b + calc ys

romToInt :: String -> Int
romToInt xs = sum $ map calcThunks $ thunks $ map toUpper xs 

-------- Med monad ---------------------------------------------------------------------------

romDigit' ::Char-> Maybe Int
romDigit' s 
    | s == 'I' = Just 1
    | s == 'V' = Just 5
    | s == 'X' = Just 10
    | s == 'L' = Just 50
    | s == 'C' = Just 100
    | s == 'D' = Just 500
    | s == 'M' = Just 1000
    | otherwise  = Nothing


getThunk' :: [Char] -> Maybe [Char]
getThunk' [] = Just []
getThunk' [x] = Just [x]
getThunk' (x:y:xs) 
    |romDigit' x <= romDigit' y = x : getThunk' (y:xs)
    |otherwise = Just [x]

thunks' :: String -> Maybe [String]
thunks' [] = Just []
thunks' xs = getThunk' xs : thunks' ( drop ( length $ getThunk' xs )  xs )

calcThunks' :: [Char] -> Maybe Int
calcThunks' Just  [] = 0
calcThunks' [x] = romDigit' x
calcThunks' xs = calc (reverse xs)
    where calc [] = 0
          calc [a] = romDigit' a
          calc (a:b:ys)
                       | romDigit' a > romDigit' b = romDigit' a - romDigit' b - calc ys
                       | otherwise = romDigit' a + romDigit' b + calc ys

romToInt' :: String -> Maybe Int
romToInt' xs = sum $ map calcThunks' $ thunks' $ map toUpper xs



----------------------------Andre veien ---------------------------------------------------

digitToRom :: Int -> String
digitToRom 0 = []
digitToRom x
    | x >= 1000 = 'M' : digitToRom (x - 1000)
    | x >= 500  = 'D' : digitToRom (x - 500)
    | x >= 100  = 'C' : digitToRom (x - 100)
    | x >= 50   = 'L' : digitToRom (x - 50)
    | x >= 10   = 'X' : digitToRom (x - 10)
    | x >= 5    = 'V' : digitToRom (x - 5)
    | x >= 1    = 'I' : digitToRom (x - 1)

digitToRom' :: Int -> [String]
digitToRom' 0 = []
digitToRom' x
    | x >= 1000 = "M" : digitToRom' (x - 1000)
    | x >= 500  = "D" : digitToRom' (x - 500)
    | x >= 100  = "C" : digitToRom' (x - 100)
    | x >= 50   = "L" : digitToRom' (x - 50)
    | x >= 10   = "X" : digitToRom' (x - 10)
    | x >= 5    = "V" : digitToRom' (x - 5)
    | x >= 1    = "I" : digitToRom' (x - 1)



replaceStr :: String -> String -> String -> String
replaceStr [] old new = []
replaceStr str old new = loop str
  where
    loop [] = []
    loop str =
      let (prefix, rest) = splitAt n str
      in
        if old == prefix                -- found an occurrence?
        then new ++ loop rest           -- yes: replace it
        else head str : loop (tail str) -- no: keep looking
    n = length old


intToRom :: Int -> String
intToRom x = do 
    let x1 = replaceStr (digitToRom x) "VIIII" "IX"  
    let x2 = replaceStr (x1) "IIII" "IV"
    let x3 = replaceStr (x2) "LXXXX" "XC"  
    let x4 = replaceStr (x3) "XXXX" "XL"  
    let x5 = replaceStr (x4) "DCCCC" "CM"  
    replaceStr (x5) "CCCC" "CD"    

makeRomanList :: [Int] -> [String]
makeRomanList [] = []
makeRomanList (x:xs) = intToRom x : makeRomanList xs

