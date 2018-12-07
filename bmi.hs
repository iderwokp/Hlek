bmi :: Double -> Double -> String
bmi w h
    | w/h^2 <= 18.2 = show (w/h^2) ++ "  ........Tynn"
    | w/h^2 <= 25 = show (w/h^2) ++ "  .......Normal"
    | w/h^2 <= 30 = show (w/h^2) ++ "  .........Feit"
    | otherwise = show (w/h^2) ++ "  .........Kval"


bmicalc :: [(String, Double, Double)] -> [String]
bmicalc xs = [navn | (navn, w, h) <- xs, let bmi2 = w/h^2, bmi2>25.0]
 