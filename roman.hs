
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
romToInt xs = sum $ map calcThunks $ thunks xs

    
