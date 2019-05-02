
romToInt :: String -> Int
romToInt [] = 0
romToInt s 
    | s == "I" = 1
    | s == "V" = 5
    | s == "X" = 10
    | s == "L" = 50
    | s == "C" = 100
    | s == "D" = 500
    | s == "M" = 1000
    | otherwise  = 0

    
