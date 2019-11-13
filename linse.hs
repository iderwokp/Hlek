
import Data.Fixed
import Numeric

data Linse = Linse 
    {
         sphere :: Float
        ,cylinder :: Float
        ,axis :: Float 
    }
        deriving(Eq)
        
-- data Vector2d 
mPI = 3.14159265 :: Float

degToRad :: Float -> Float
degToRad deg = deg*mPI/180.0

radToDeg :: Float -> Float
radToDeg rad = 180.0*rad/mPI
        
convCyl :: Linse -> Linse
convCyl (Linse sf cy ax) = 
                   let sph = sf+cy;
                       axe = sanitizeAxe (ax - 90);
                       cyl = (-1) * cy
                   in (Linse sph cyl axe)

sanitizeAxe :: Float -> Float
sanitizeAxe x | x<0 = sanitizeAxe (x+180)
              | otherwise = x `mod'` 180
            
lt1 = Linse (-5) (-1) 45
lt2 = Linse (5.25) (-1.5) 45
lt3 = Linse (-5) (-2.25) 145
lt4 = Linse (-5.25) (-1.5) 60
lt5 = Linse (-5) (-0.5) 120
lt6 = Linse (5.25) (-1.5) 170
linser = [lt1,lt2,lt3,lt4,lt5,lt6]
planLinse = Linse 0 0 0

instance Show Linse where
    show (Linse s c a) = (showFFloat (Just 2) s "") ++"/" ++ (showFFloat (Just 2) c "" )++ "x" ++ (showFFloat (Just 0) a "")-- "Rectangle with base " ++ show w ++ " and height " ++ show h

linseAdd :: Linse -> Linse -> Linse
linseAdd l1@(Linse s1 c1 aa1) l2@(Linse s2 c2 aa2) = do
                         
                         let a1 = sanitizeAxe aa1
                         let a2 = sanitizeAxe aa2 
                         let rad_a = degToRad $ abs $ a2 - a1
                         let teller = c2 * sin (2*rad_a)
                         let nevner = c1 + c2 * cos(2*rad_a)
                         let th_rad | nevner == 0 = 0 | otherwise = atan(teller/nevner)/2
                         let s = c1*(sin(th_rad)*sin(th_rad))+c2*(sin(rad_a-th_rad)*sin(rad_a-th_rad))
                         let c = c1+c2-2*s;
                         let akse = radToDeg th_rad + a1
                         let sp = s+s1+s2
                         --if (sanitizeAxe akse) <= 90
                            -- then convCyl(Linse sp c (sanitizeAxe akse))
                            -- else Linse sp c (sanitizeAxe akse)
                         Linse sp c (sanitizeAxe akse)
                         
                         
                         
addLinser :: Linse -> Linse -> Linse
addLinser  l1@(Linse s1 c1 a1) l2@(Linse s2 c2 a2) 
                         | sanitizeAxe a1 < sanitizeAxe a2 = linseAdd l2 l1
                         | otherwise = linseAdd l1 l2
                         
--(:+:) :: Linse -> Linse -> Linse
--infixl 5 :+:
(<+>) = addLinser
--l1 :+: l2 = addLinser l1 l2
 
                         
                         