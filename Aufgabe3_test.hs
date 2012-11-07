{-# LANGUAGE TemplateHaskell #-}

import Aufgabe3

import Test.QuickCheck
import Test.QuickCheck.All


n2i :: NegaBinary -> Integer
n2i n = foldl f 0 n
  where  f acc '1' = acc * (-2) + 1
         f acc '0' = acc * (-2)

i2n :: Integer -> NegaBinary
i2n i = dropZeroes $ reverse $ convert (-i)
  where 
    convert 0 = "0"
    convert i = case  i `mod` (-2) of
      (-1)  ->  '1' : convert (i `div` (-2))
      0     ->  '0' : convert (i `div` (-2))

prop_testConversion a = n2i (i2n a) == a

-- tests for extract and dropZeroes would basically just show the implementation,
-- so I can't give those

prop_testIncr a = n2i (nbIncr (i2n a)) == a + 1

prop_testDecr a = n2i (nbDecr (i2n a)) == a - 1

prop_testAbs a = n2i (nbAbs (i2n a)) == abs a

prop_testPlus a b = n2i (nbPlus (i2n a) (i2n b)) == a + b

prop_testTime a b = n2i (nbTimes (i2n a) (i2n b)) == a * b


-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)