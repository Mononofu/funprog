{-# LANGUAGE TemplateHaskell #-}

import Aufgabe4

import Test.QuickCheck
import Test.QuickCheck.All


-- helper type to make sure arguments to test are always natural numbers
-- (ie >= 0)
newtype Na a = Na a
    deriving (Show)

instance (Integral a, Arbitrary a) => Arbitrary (Na a) where
    arbitrary = do
      a <- arbitrary
      return $! Na (if a >= 0 then a else (-a))

-- helper type to make sure arguments to test are always positive numbers
-- (ie > 0)
newtype Pos a = Pos a
    deriving (Show)

instance (Integral a, Arbitrary a) => Arbitrary (Pos a) where
    arbitrary = do
      a <- arbitrary
      return $! Pos (if a > 0 then a else (-a + 1))


-- integer to nat
i2n :: Integer -> Nat
i2n 0 = Z
i2n n = S (i2n (n-1))

-- Na to integer
n2i :: Nat -> Integer
n2i Z = 0
n2i (S n) = 1 + n2i n

-- Pair to integer
np2i :: NatPair -> Integer
np2i (a, b) = n2i a - n2i b

prop_testNatAddition (Na a) (Na b) =
  n2i (plusNat (i2n a) (i2n b)) == a + b


prop_testNatSubtraction (Na a) (Na b) = a >= b ==> 
  n2i (minusNat (i2n a) (i2n b)) == a - b

prop_testNatSubtraction2 (Na a) (Na b) = a < b ==> 
  n2i (minusNat (i2n a) (i2n b)) == 0


prop_testNatMultiplication (Na a) (Na b) =
  n2i (timesNat (i2n a) (i2n b)) == a * b

prop_testNatDivision (Pos a) (Pos b) = 
  n2i (divNat (i2n a) (i2n b)) == a `div` b

prop_testNatModulo (Pos a) (Pos b) = 
  n2i (modNat (i2n a) (i2n b)) == a `mod` b


prop_testNatEq (Na a) (Na b) =
  eqNat (i2n a) (i2n b) == (a == b)

prop_testNatGr (Na a) (Na b) =
  grNat (i2n a) (i2n b) == (a > b)

prop_testNatLe (Na a) (Na b) =
  leNat (i2n a) (i2n b) == (a < b)

prop_testNatGrEq (Na a) (Na b) =
  grEqNat (i2n a) (i2n b) == (a >= b)

prop_testNatLeEq (Na a) (Na b) =
  leEqNat (i2n a) (i2n b) == (a <= b)



prop_testNPAddition (Na a) (Na b) (Na c) (Na d) = 
  np2i (plusNP (i2n a, i2n b) (i2n c, i2n d)) == (a-b) + (c-d)

prop_testNPSubtraction (Na a) (Na b) (Na c) (Na d) = 
  np2i (minusNP (i2n a, i2n b) (i2n c, i2n d)) == (a-b) - (c-d)

prop_testNPMultiplication (Na a) (Na b) (Na c) (Na d) = 
  np2i (timesNP (i2n a, i2n b) (i2n c, i2n d)) == (a-b) * (c-d)

prop_testNPDivision (Na a) (Na b) (Na c) (Na d) = c /= d ==>
  np2i (divNP (i2n a, i2n b) (i2n c, i2n d)) == (a-b) `div` (c-d)

prop_testNPModulo (Na a) (Na b) (Na c) (Na d) = c /= d ==>
  np2i (modNP (i2n a, i2n b) (i2n c, i2n d)) == (a-b) `mod` (c-d)


prop_testNPEq (Na a) (Na b) (Na c) (Na d) =
  eqNP (i2n a, i2n b) (i2n c, i2n d) == ((a-b) == (c-d))

prop_testNPGr (Na a) (Na b) (Na c) (Na d) =
  grNP (i2n a, i2n b) (i2n c, i2n d) == ((a-b) > (c-d))

prop_testNPLe (Na a) (Na b) (Na c) (Na d) =
  leNP (i2n a, i2n b) (i2n c, i2n d) == ((a-b) < (c-d))

prop_testNPGrEq (Na a) (Na b) (Na c) (Na d) =
  grEqNP (i2n a, i2n b) (i2n c, i2n d) == ((a-b) >= (c-d))

prop_testNPLeEq (Na a) (Na b) (Na c) (Na d) =
  leEqNP (i2n a, i2n b) (i2n c, i2n d) == ((a-b) <= (c-d))


-- quickCheckAll generates test cases for all 'prop_*' properties
main = $(quickCheckAll)