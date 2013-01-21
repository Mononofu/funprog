\begin{code}


module Aufgabe3 where

type NegaBinary = String


dropZeroes :: NegaBinary -> NegaBinary
dropZeroes n = case dropWhile (== '0') n of
  [] -> "0"
  xs -> xs


extract :: String -> NegaBinary
extract = dropZeroes . filter (`elem` ['1', '0'])


nb2dec :: NegaBinary -> Integer
nb2dec n = foldl f 0 n
  where  f acc '1' = acc * (-2) + 1
         f acc '0' = acc * (-2)


balance :: Integer -> NegaBinary -> NegaBinary
balance b n = dropZeroes $ reverse $ bal b (reverse n) [(-2) ^ i | i <- [0..]]
  where bal 0 xs _            = xs
        bal b [] (n:ns)       = '1' : bal (b - n) [] ns
        bal b ('0':xs) (n:ns) = '1' : bal (b - n) xs ns
        bal b ('1':xs) (n:ns) = '0' : bal (b + n) xs ns


nbIncr :: NegaBinary -> NegaBinary
nbIncr n = balance 1 n


nbDecr :: NegaBinary -> NegaBinary
nbDecr n = balance (-1) n


nbIsPos :: NegaBinary -> Bool
nbIsPos n = (length n) `mod` 2 == 1


nbAbs :: NegaBinary -> NegaBinary
nbAbs n 
  | nbIsPos n     = n       -- no change for already positive numbers
  | otherwise     = nbIncr $ nbAbs $ nbIncr n


nbPlus :: NegaBinary -> NegaBinary -> NegaBinary
nbPlus "0" n = n
nbPlus n "0" = n
nbPlus n m 
  | nbIsPos n   = nbPlus (nbDecr n) (nbIncr m)
  | otherwise   = nbPlus (nbIncr n) (nbDecr m)


nbTimes :: NegaBinary -> NegaBinary -> NegaBinary
nbTimes "0" _ = "0"
nbTimes _ "0" = "0"
nbTimes n m
  | nbIsPos n   = nbPlus m $ nbTimes (nbDecr n) m
  | nbIsPos m   = nbPlus n $ nbTimes (nbDecr m) n
  | otherwise   = nbTimes (nbAbs n) (nbAbs m)     -- both negative is positive anyway


\end{code}