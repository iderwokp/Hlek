-- Diverse måter å lese filer
import System.IO

main = do
    handle <- openFile "lala.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
    
    
-- Med withFile
main = do
    withFile "lala.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
        
-- Med withFile og brackets
withFile name mode f = bracket (openFile name mode)
    (\handle -> hClose handle)
    (\handle -> f handle)
    

-- Med readFile (filen blir automatisk åpnet og lukket)
main = do
    contents <- readFile "lala.txt"
    putStr contents


import Data.Char
main = do
    contents <- readFile "lala.txt"
    writeFile "lala.txt" (map toUpper contents)
