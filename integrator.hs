module Integrator where

-- метод 1) Трапеций
trap :: (Double -> Double) -> Double -> Double -> Double -> Double
trap f a b n = h*(((f a) + (f b) / 2) + (su f n 1 0))
    where
        su :: (Double -> Double) -> Double -> Double -> Double -> Double
        su f n k res | n > 100000  = (1/0)
                     | k < (n - 1) = (su f n (k + 1) (res + (f (a + k*h))))
                     | otherwise   = res
        h = ((b - a) / n)

-- метод 2) Симпсона
simpson :: (Double -> Double) -> Double -> Double -> Double -> Double
simpson f a b n = smp f 0 a (a + h) 0

    where

        smp :: (Double -> Double) -> Double -> Double -> Double -> Double -> Double
        smp f sum x0 x1 i | i < n     = smp f ( sum + (f x0) + 4 * (f (x0 + h/2)) + (f x1)) (x0 + h) (x1 + h) (i + 1)
                          | otherwise = (h / 6) * sum

        h = ((b - a) / n)

integration :: ((Double -> Double) -> Double -> Double -> Double -> Double) -> (Double -> Double) -> Double -> Double -> Double -> Double -> [Double]
integration method f a b fault n =
    if abs (((method f a b (n / 2)) - (method f a b n)) / (2^4 - 1)) < fault then
        [method f a b n, n]
    else
        integration method f a b fault (n + n)