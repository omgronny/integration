module Functions where

f1 :: Double -> Double
f1 x = 1 / (sqrt x)

f2 :: Double -> Double
f2 x = (sin x) + (cos x)

f3 :: Double -> Double
f3 x = 1 / (x^2 - 1)

variant :: Double -> Double  
variant a = 3 * a^3 - 2 * a^2 - 7 * a - 8

func_num :: String -> (Double -> Double)
func_num n | n == "1"  = f1
           | n == "2"  = f2
           | n == "3"  = f3
           | otherwise = variant