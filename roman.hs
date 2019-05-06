
import Data.Char
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
romToInt xs = sum $ map calcThunks $ thunks (map toUpper xs)

intToRom :: Int -> String
intToRom 0 = []
intToRom x
    | x >= 1000 = 'M' : intToRom (x - 1000)
    | x >= 500  = 'D' : intToRom (x - 500)
    | x >= 100  = 'C' : intToRom (x - 100)
    | x >= 50   = 'L' : intToRom (x - 50)
    | x >= 10   = 'X' : intToRom (x - 10)
    | x >= 5    = 'V' : intToRom (x - 5)
    | x >= 1    = 'I' : intToRom (x - 1)

intToRom' :: Int -> [String]
intToRom' 0 = []
intToRom' x
    | x >= 1000 = "M" : intToRom' (x - 1000)
    | x >= 500  = "D" : intToRom' (x - 500)
    | x >= 100  = "C" : intToRom' (x - 100)
    | x >= 50   = "L" : intToRom' (x - 50)
    | x >= 10   = "X" : intToRom' (x - 10)
    | x >= 5    = "V" : intToRom' (x - 5)
    | x >= 1    = "I" : intToRom' (x - 1)



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



integToRom x = do 
    replaceStr (intToRom x) "VIIII" "IX"  
    replaceStr (intToRom x) "IIII" "IV"
    replaceStr (intToRom x) "XIIII" "IL"  
    replaceStr (intToRom x) "IIII" "IX"  
    replaceStr (intToRom x) "VIIII" "IX"  
    replaceStr (intToRom x) "VIIII" "IX"    

