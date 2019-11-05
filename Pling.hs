import Control.Monad
import Control.Applicative




data Pling a = Enkel a | Dobbel a | Trippel a | Uff deriving (Show, Eq)

instance Functor Pling where
    fmap f (Enkel a) = Enkel (f a)
    fmap f (Dobbel a) = Dobbel (f a)
    fmap f (Trippel a)  = Trippel  (f a)
    fmap f Uff       = Uff

