
data Vektor2d = Vektor2d {x::Double, y::Double} Deriving(Show, Eq)

v2dx :: Vektor2d -> Double
v2dx Vektor2d(x _) = x

v2dy :: Vektor2d -> Double
v2dy Vektor2d(_ y) = y

v2dAdd :: (Num t) => Vektor2d t -> Vektor2d t -> Vektor2d t
v2dAdd (Vektor2d x y) (Vektor2d x1 y1) =  Vektor2d (x+x1) (y+y1)




