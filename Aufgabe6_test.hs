{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.All


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

tests = TestList [TestLabel "Test 8" test8, TestLabel "Test 9" test9,
                  TestLabel "Test 19" test19, TestLabel "Test 39" test39,
                  TestLabel "Test 84" test84, TestLabel "Test89" test89,
                  TestLabel "Test 98" test98, TestLabel "Test 99" test99,
                  TestLabel "Test 889" test889, TestLabel "Test 1899" test1899,
                  TestLabel "Test 4884" test4884, TestLabel "Test 5999" test5999]

-- helper type to make sure arguments to test are always natural numbers
-- (ie >= 0)
newtype Na a = Na a
    deriving (Show)

instance (Integral a, Arbitrary a) => Arbitrary (Na a) where
    arbitrary = do
      a <- arbitrary
      return $! Na (a `mod` 20)

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
