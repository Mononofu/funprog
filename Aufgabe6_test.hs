{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List


import Aufgabe6

-- converts an integer to Nat
dec2nat :: Integer -> Nat
dec2nat 0 = Z
dec2nat i = S (dec2nat (i-1))

-- Na to integer
nat2dec :: Nat -> Integer
nat2dec Z = 0
nat2dec (S n) = 1 + nat2dec n

neg2dec :: NegaBinary -> Integer
neg2dec n = foldl f 0 n
  where  f acc '1' = acc * (-2) + 1
         f acc '0' = acc * (-2)


-- tests cases for Aufgabe 6.1
test8 = TestCase (assertEqual "" "VIII" (show (dec2nat 8)) )
test9 = TestCase (assertEqual "" "IX" (show (dec2nat 9)))
test19 = TestCase (assertEqual "" "XIX" (show (dec2nat 19)))
test39 = TestCase (assertEqual "" "XXXIX"(show (dec2nat 39)))
test84 = TestCase (assertEqual "" "LXXXIV" (show (dec2nat 84)))
test89 = TestCase (assertEqual "" "LXXXIX" (show (dec2nat 89)))
test98 = TestCase (assertEqual "" "XCVIII" (show (dec2nat 98)))
test99 = TestCase (assertEqual "" "IC" (show (dec2nat 99)))
test889 = TestCase (assertEqual "" "DCCCLXXXIX" (show (dec2nat 889)))
test1899 = TestCase (assertEqual "" "MDCCCIC" (show (dec2nat 1899)))
test4884 = TestCase (assertEqual "" "MMMMDCCCLXXXIV" (show (dec2nat 4884)))
test5999 = TestCase (assertEqual "" "MMMMMIM" (show (dec2nat 5999)))

-- official test 
testHundred = TestCase (assertEqual "" [] (let z = Z : [S x | x <- z] 
  in [i | (i,r) <- zip[10..99] ["X","XI","XII","XIII","XIV","XV","XVI","XVII",
    "XVIII","XIX","XX","XXI","XXII","XXIII","XXIV","XXV","XXVI","XXVII","XXVIII",
    "XXIX","XXX","XXXI","XXXII","XXXIII","XXXIV","XXXV","XXXVI","XXXVII","XXXVIII",
    "XXXIX","XL","XLI","XLII","XLIII","XLIV","XLV","XLVI","XLVII","XLVIII","IL",
    "L","LI","LII","LIII","LIV","LV","LVI","LVII","LVIII","LIX","LX","LXI","LXII",
    "LXIII","LXIV","LXV","LXVI","LXVII","LXVIII","LXIX","LXX","LXXI","LXXII",
    "LXXIII","LXXIV","LXXV","LXXVI","LXXVII","LXXVIII","LXXIX","LXXX","LXXXI",
    "LXXXII","LXXXIII","LXXXIV","LXXXV","LXXXVI","LXXXVII","LXXXVIII","LXXXIX",
    "XC","XCI","XCII","XCIII","XCIV","XCV","XCVI","XCVII","XCVIII","IC"],
    r /= show (z !! i)]) )

testSome = TestCase (assertEqual "" ["CD","CCIC","XCVIII","XCV","XIX","XXXVIII",
  "LXXXIV","LXXXIX","DCCCLXXXIX"]
  (let z = Z : [S x|x<-z] 
   in [show (z!!i)|i<-[400,299,98,95,19,38,84,89,889]]) )


tests = TestList [TestLabel "Test 8" test8, TestLabel "Test 9" test9,
                  TestLabel "Test 19" test19, TestLabel "Test 39" test39,
                  TestLabel "Test 84" test84, TestLabel "Test89" test89,
                  TestLabel "Test 98" test98, TestLabel "Test 99" test99,
                  TestLabel "Test 889" test889, TestLabel "Test 1899" test1899,
                  TestLabel "Test 4884" test4884, TestLabel "Test 5999" test5999,
                  TestLabel "Test 10.99" testHundred, TestLabel "Test some" testSome]


digit2num 'I' = 1
digit2num 'V' = 5
digit2num 'X' = 10
digit2num 'L' = 50
digit2num 'C' = 100
digit2num 'D' = 500
digit2num 'M' = 1000

roman2int r = parse (Just 0) $ reverse r
  where
    parse _ [] = 0
    parse highest (d:ds)
      | index < highest   = (-1 * digit2num d) + parse index ds
      | otherwise         = (digit2num d) + parse index ds
      where index       = findIndex (== d) romanDigits
            romanDigits = "IVXLCDM"

-- this only tests if the conversion keeps the correct number, not if the style
-- rules are followed
prop_roman_conversion (Na a) = a == roman2int (show (dec2nat a))

-- helper type to make sure arguments to test are always natural numbers
-- (ie >= 0)
newtype Na a = Na a
    deriving (Show)

instance (Integral a, Arbitrary a) => Arbitrary (Na a) where
    arbitrary = do
      a <- arbitrary
      return $! Na (a `mod` 20)   -- limited size because *, +, - are slow otherwise

-- helper type to make sure arguments to test are always positive numbers
-- (ie > 0)
newtype Pos a = Pos a
    deriving (Show)

instance (Integral a, Arbitrary a) => Arbitrary (Pos a) where
    arbitrary = do
      a <- arbitrary
      return $! Pos ((a `mod` 20) + 1)


prop_showRat (Na num) (Na den) = num == neg2dec negNum && den == neg2dec negDen
  where
    (negNum, (_:negDen)) = break (== '/') $ show (Rat (dec2nat num) (dec2nat den))


prop_rat2nf (Na num) (Pos den) =
  show (t2nf (Rat (dec2nat num) (dec2nat den))) ==
  show (t2nf (Rat (dec2nat (2*num)) (dec2nat (2*den))))


prop_rat2nf2 (Na num) (Pos den) = gcd (nat2dec n) (nat2dec d) == 1
  where (Rat n d) = t2nf (Rat (dec2nat num) (dec2nat den))


prop_natp2nf (Na a) (Na b) =
  t2nf (NP ((dec2nat a), (dec2nat b))) ==
  t2nf (NP ((dec2nat (10+a)), (dec2nat (10+b))))

prop_natp2nf2 (Na a) (Na b) = case t2nf (NP ((dec2nat a), (dec2nat b))) of
  NP (Z, _) -> True
  NP (_, Z) -> True
  _         -> False

prop_natp2nf3 (Na a) (Na b) = a - b == (nat2dec n) - (nat2dec m)
  where (NP (n, m)) = t2nf (NP ((dec2nat a), (dec2nat b)))

prop_rat_eq (Na num) (Na den) =
  (Rat (dec2nat num) (dec2nat den)) == (Rat (dec2nat (3*num)) (dec2nat (3*den)))

prop_rat_less (Pos num) (Pos den) =
  (Rat (dec2nat num) (dec2nat den)) < (Rat (dec2nat (3*num)) (dec2nat (den)))

prop_rat_greater (Pos num) (Pos den) =
  (Rat (dec2nat num) (dec2nat den)) > (Rat (dec2nat (num)) (dec2nat (4*den)))

prop_rat_fromInteger (Na n) =
  fromInteger n == (Rat (dec2nat n) (S Z))

prop_rat_add (Na n1) (Na n2) (Pos d1) (Pos d2) =
  (Rat (dec2nat n1) (dec2nat d1)) + (Rat (dec2nat n2) (dec2nat d2)) ==
  (Rat (dec2nat (n1*d2 + n2*d1)) (dec2nat (d1 * d2)))

prop_rat_sub (Na n1) (Na n2) (Pos d1) (Pos d2) =
  (Rat (dec2nat n1) (dec2nat d1)) - (Rat (dec2nat n2) (dec2nat d2)) ==
  (Rat (dec2nat (max 0 (n1*d2 - n2*d1))) (dec2nat (d1 * d2)))

prop_rat_mul (Na n1) (Na n2) (Pos d1) (Pos d2) =
  (Rat (dec2nat n1) (dec2nat d1)) * (Rat (dec2nat n2) (dec2nat d2)) ==
  (Rat (dec2nat (n1*n2)) (dec2nat (d1 * d2)))

main = do
  $(quickCheckAll)
  runTestTT tests
