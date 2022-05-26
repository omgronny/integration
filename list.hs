module List where

get_by_index :: [String] -> Integer -> String
get_by_index l i = hlp l i 0
    where
        hlp :: [String] -> Integer -> Integer -> String
        hlp l i k | k < i = hlp (tail l) i (k + 1)
                  | otherwise = head l
                  
get_by_index_double :: [Double] -> Integer -> Double
get_by_index_double l i = hlp l i 0
    where
        hlp :: [Double] -> Integer -> Integer -> Double
        hlp l i k | k < i = hlp (tail l) i (k + 1)
                  | otherwise = head l

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'