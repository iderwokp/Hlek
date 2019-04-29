

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

kvot :: (Fractional a) => [a] -> [a]
kvot [] = []
kvot [y] = []
kvot (x:y:ys) = y/x : kvot (y:ys)


-- *Main> map (Circle (Point 0 0)) [20, 30, 40]
-- [Circle (Point 0.0 0.0) 20.0,Circle (Point 0.0 0.0) 30.0,Circle (Point 0.0 0.0) 40.0]
-- *Main> map area $ map (Circle (Point 0 0)) [20, 30, 40]
-- [1256.6371,2827.4333,5026.5483]
-- *Main> kvot . map area $ map (Circle (Point 0 0)) [20, 30, 40]
-- [2.25,1.7777778]


