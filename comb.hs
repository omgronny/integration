fibonacci :: Integer -> Integer
fibonacci n | n == 0     = 0
            | n == 1     = 1

            | n > 1      =  let 

                helper a1 a2 k n | k < n = helper (a1+a2) a1 (k+1) n
                                 | otherwise = a1 + a2
                
                in helper 0 1 1 n

            | n == -1    = 1
            | n == -2    = -1

            | otherwise  = let

                helper_neg a1 a2 k n | k > n      = helper_neg (a1+a2) a1 (k-1) n
                                     | otherwise  = (a1 + a2)
                in helper_neg 0 1 (-1) n

max3 a b c = 
    let max2 a b = max a b in
    (max (max2 a b) c)
    
roots a b c = 
    let {
        d = sqrt (b^2 - 4*a*c);
        x1 = (-b + d) / (2 * a);
        x2 = (-b - d) / (2 * a);
    } in (x1,x2)
    
roots' :: Double -> Double -> Double -> (Double, Double)
roots' a b c =
    let
        d = sqrt (b^2 - 4*a*c)
        x1 = (-b + d) / aTwice
        x2 = (-b - d) / aTwice
        aTwice = 2 * a
    in (x1, x2)

imperative :: (Int, Int) -> Int
imperative a = (fst a)^2 + (snd a)^2


seqA :: Integer -> Integer
seqA n | n == 0    = 1
       | n == 1    = 2 
       | n == 2    = 3
       | otherwise = let
            helper a0 a1 a2 k n | k < n     = helper a1 a2 (s a0 a1 a2) (k+1) n
                                | otherwise = (s a0 a1 a2)
            in helper 1 2 3 3 n
                
                where s a0 a1 a2 = (a2 + a1 - 2*a0)


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | (abs x) > 9     = loop (abs x) 0 0
              | otherwise = ((abs x), 1)
    where 
        loop :: Integer -> Integer -> Integer -> (Integer, Integer)
        loop x s c | (abs x) > 0 = loop (x `div` 10) (s + (x `mod` 10)) (c + 1)
                   | otherwise   = (s,c)

trap :: (Double -> Double) -> Double -> Double -> Double -> Double
trap f a b n = h*(((f a) + (f b) / 2) + (su f n 1 0))
    where
        su :: (Double -> Double) -> Double -> Double -> Double -> Double
        su f n k res | k < (n - 1) = (su f n (k + 1) (res + (f (a + k*h))))
                     | otherwise   = res
        h = ((b - a) / n)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = trap f a b 100000

main = putStrLn "Hello, World!"






