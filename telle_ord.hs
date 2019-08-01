
telle::Eq a =>(Int, a)-> [a]-> [(Int,a)]
telle (i, s) []  = [(i, s)]
telle (i, s) (x:xs)
    | x == s = telle (i+1, s) xs 
    | x /= s = (i,s) : telle (1, x) xs 

tuple_sort :: [(Int, a)] -> [(Int, a)]
tuple_sort [] = []
tuple_sort ((i, s):xs) = tuple_sort sm ++ [(i, s)] ++ tuple_sort la
      where
        sm = [(ii, ss) | (ii, ss) <- xs, ii<=i]
        la = [(ii, ss) | (ii, ss) <- xs, ii>i]
        

--  let talt = tuple_sort . tail . telle (0, ' ') . sort $ tekst



-- ordteller:: [String] -> [(Int, String)]
-- ordteller xs = tail $ telle xs (0," ")

-- Main Data.Char Data.List> let setn = "Hei på deg din snei hei hei hva ligger på deg"
-- *Main Data.Char Data.List> let ords = words setn
-- *Main Data.Char Data.List> let sords = sort ords
-- *Main Data.Char Data.List> ordteller sords
-- [(1,"Hei"),(2,"deg"),(1,"din"),(2,"hei"),(1,"hva"),(1,"ligger"),(2,"p\229"),(1,"snei")]


 