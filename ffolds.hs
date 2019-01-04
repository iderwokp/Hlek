import qualified Data.Foldable as F 
import Data.Monoid





data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  

instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r  


testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )  


-- Bruk:
-- ghci> F.foldl (+) 0 testTree  
-- 42  
-- ghci> F.foldl (*) 1 testTree  
-- 64800  

-- Sjekke om det finnes en node med 3 i treet:
-- getAny $ F.foldMap (\x -> Any $ x == 3) testTree  