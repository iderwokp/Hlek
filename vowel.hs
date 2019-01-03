import Data.Char

ll = length.filter (`elem` "aeiouy")
-- Brukes slik: ll "navn"

vow_len :: String -> Int
vow_len [] = 0
vow_len s =   length $ vows s
    

vows :: [Char]-> [Char]
vows [] = []
vows s = filter ( `elem` "aeiouyæøå" ) $ map toLower s

kons_len :: String -> Int
kons_len [] = 0
kons_len s =   length $ kons s

kons :: [Char]-> [Char]
kons [] = []
kons s = filter (`notElem` "aeiouyæøå" ) $ map toLower s