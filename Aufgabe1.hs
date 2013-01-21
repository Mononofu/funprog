import Data.List

-- binomial coefficient
nCr :: Integer -> Integer -> Integer
nCr 0 k = 0
nCr k 0 = 1
nCr n k = nCr (n-1) (k-1) * n `div` k
 
katNumber :: Integer -> Integer
katNumber 0 = error "Argument ungueltig"
katNumber n = (nCr (2*k) k) `div` (k+1)
  where k = n - 1

sumPowers :: Integer -> Integer -> Integer
sumPowers n k 
  | k < 0       = -1
  | otherwise   = sum $ map (^ k) [1..n]

shrink :: Char -> String -> String
shrink c (x:x':xs) 
  -- if we have the same char twice, only use it once (if the one we shrink by)
  | c == x && x == x'  = shrink c (x':xs)
  | otherwise          = x : shrink c (x':xs)
shrink _ xs = xs


stretch :: Char -> Integer -> String -> String
stretch c n (x:xs) 
  -- replace the char to stretch by n times that char
  | c == x    = genericTake n $ repeat c ++ stretch c n xs
  | otherwise = x : stretch c n xs
stretch _ _ xs = xs


-- False = Lampe aus, True = Lampe an
waechter :: Integer -> Bool
waechter = odd . length . factors
  where factors n = [ x | x <- [1..n], n `mod` x == 0]
