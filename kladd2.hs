
import Control.Monad

fa :: (Num a, Eq a) => a -> Maybe a
fa 2 = Nothing
fa 4 = Nothing
fa x = Just (x+x)

fb :: (Num a, Eq a) => a -> Maybe a
fb 4 = Nothing
fb 16 = Nothing
fb x = Just (2*x)

ga :: (Num a, Eq a) => a -> [a]
ga 2 = [2]
ga x = [x*x]

gb :: (Num a, Eq a) => a -> [a]
gb 16 = [16]
gb x = [x*x*x]


