

factor n = [x | x <- [1..n], n `mod` x == 0]

prime p = factor p == [1,p]

