


llll [] = []
llll [a] = [a]
llll (x:xs) = [x] ++ [0] ++ llll xs

