import Data.Char  
import Data.List 

--  reve = do line <- fmap reverse getLine  
--          putStrLn $ "You said " ++ line ++ " backwards!"  
--          putStrLn $ "Yes, you really said " ++ line ++ " backwards!"  


main2 = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
           putStrLn line  