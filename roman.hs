
romToInt ::Char-> Int
-- romToInt [] = 0
romToInt s 
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
    |romToInt x <= romToInt y = x : getThunk (y:xs)
    |otherwise = [x]

thunks :: String -> [String]
thunks [] = []
thunks xs = getThunk xs : thunks ( drop ( length $ getThunk xs )  xs ) 




    
