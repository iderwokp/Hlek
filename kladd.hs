import Control.Monad


enDeltPa' :: (Fractional a, Eq a) => a ->  a
--enDeltPa' 0.0 = Nothing
enDeltPa' t =  (1 / (t*(t+1)))

oppgaven' :: (Num a, Fractional a, Eq a) =>  a ->  [a]
--oppgaven' Nothing = [Nothing]
oppgaven' x = enDeltPa' x : oppgaven' (x+1)



enDeltPa :: (Fractional a, Eq a) => a -> Maybe a
enDeltPa 0.0 = Nothing
enDeltPa t = Just (1 / (t*(t+1)))

oppgaven :: (Num a, Fractional a, Eq a) =>  Maybe a ->  [Maybe a]
oppgaven Nothing = [Nothing]
oppgaven x = enDeltPa x : oppgaven (addJust(x + 1))

addJust :: Num a => Maybe a -> a -> Maybe a
addJust (Just x) y = Just (x+y)
