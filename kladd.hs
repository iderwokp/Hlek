import Control.Monad


enDeltPa' :: (Fractional a, Eq a) => a ->  a
--enDeltPa' 0.0 = Nothing
enDeltPa' t =  (1 / (t*(t+1)))

oppgaven' :: (Num a, Fractional a, Eq a) =>  a ->  [a]
--oppgaven' Nothing = [Nothing]
oppgaven' x = enDeltPa' x : oppgaven' (x+1)



enDeltPa :: (Fractional a, Eq a) => a -> Maybe a
enDeltPa (-1.0) = Nothing
enDeltPa 0.0 = Nothing
enDeltPa t = Just (1 / (t*(t+1)))

oppgaven :: (Num a, Fractional a, Eq a) =>  a ->  [Maybe a]
-- oppgaven Nothing = [Nothing]
oppgaven x = enDeltPa x : oppgaven (x + 1)


-- fmap sum $ sequence  (take 20 x)
