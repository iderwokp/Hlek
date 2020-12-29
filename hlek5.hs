



-- Tail rekursiv fakt funksjon

fakthelp :: (Eq t, Num t) => t -> t -> t
fakthelp n acc 
         | n == 1 = acc
         | otherwise = fakthelp (n-1) (n*acc)

fakt :: (Eq t, Num t) => t -> t
fakt n = fakthelp n 1

fakt' :: (Eq p, Num p) => p -> p
fakt' n
      | n == 1 = 1
      |otherwise = n*fakt' (n-1)
