
import Control.Monad

fa :: (Num a, Eq a) => a -> Maybe a
fa 2 = Nothing
fa x = Just x

fb :: (Num a, Eq a) => a -> Maybe a
fb 4 = Nothing
fb x = Just x

ga :: (Num a, Eq a) => a -> [a]
ga 2 = [2]
ga x = [x*x]

gb :: (Num a, Eq a) => a -> [a]
gb 16 = [16]
gb x = [x*x*x]


