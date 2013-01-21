module Aufgabe6 where

data Nat = Z | S Nat

-- Na to integer
nat2int :: Nat -> Integer
nat2int Z = 0
nat2int (S n) = 1 + nat2int n

-- integer to nat
int2nat :: Integer -> Nat
int2nat 0 = Z
int2nat n = S (int2nat (n-1))


toRoman :: Integer -> String
toRoman 0 = ""
toRoman 49 = "IL"
toRoman 99 = "IC"
toRoman 499 = "ID"
toRoman 999 = "IM"
toRoman x | x >= 1000 = 'M' : toRoman (x - 1000)
toRoman x | x >= 100  = representDigit 'C' 'D' 'M' q ++ toRoman r
  where (q,r) = x `divMod` 100
toRoman x | x >= 10   = representDigit 'X' 'L' 'C' q ++ toRoman r
  where (q,r) = x `divMod` 10
toRoman x             = representDigit 'I' 'V' 'X' x

representDigit :: Char -> Char -> Char -> Integer -> String
representDigit one five ten digit =
  [[one],[one,one],[one,one,one],[one,five],[five],
  [five,one],[five,one,one],[five,one,one,one],[one,ten]] !! (fromInteger digit - 1)

instance Show Nat where
  show n = toRoman (nat2int n)


data RatNumbers = Rat Numerator Denominator
type Numerator = Nat
type Denominator = Nat

dropZeroes :: NegaBinary -> NegaBinary
dropZeroes n = case dropWhile (== '0') n of
  [] -> "0"
  xs -> xs

type NegaBinary = String
int2neg :: Integer -> NegaBinary
int2neg i = dropZeroes $ reverse $ convert (-i)
  where
    convert 0 = "0"
    convert i = case  i `mod` (-2) of
      (-1)  ->  '1' : convert (i `div` (-2))
      0     ->  '0' : convert (i `div` (-2))

instance Show RatNumbers where
  show (Rat num den) = (int2neg (nat2int num)) ++ "/" ++ (int2neg (nat2int den))

plusNat :: Nat -> Nat -> Nat
plusNat Z n = n
plusNat n Z = n
plusNat (S m) (S n) = plusNat m (S (S n))

minusNat :: Nat -> Nat -> Nat
minusNat Z _ = Z
minusNat n Z = n
minusNat (S m) (S n) = minusNat m n

timesNat :: Nat -> Nat -> Nat
timesNat Z _ = Z
timesNat _ Z = Z
timesNat (S m) n = plusNat n (timesNat m n)

eqNat :: Nat -> Nat -> Bool
eqNat Z Z = True
eqNat (S m) (S n) = eqNat m n
eqNat _ _ = False

instance Eq RatNumbers where
  (Rat m n) == (Rat p q) = (m `timesNat` q) `eqNat` (n `timesNat` p)

newtype NatP = NP (Nat,Nat) deriving Show



class Nf a where
  t2nf :: a -> a


instance Nf RatNumbers where
  t2nf (Rat Z _) = Rat Z (S Z)
  t2nf (Rat n Z) = Rat n Z
  t2nf (Rat num den) = Rat (int2nat (n `div` g)) (int2nat (d `div` g))
    where n = nat2int num
          d = nat2int den
          g = gcd n d


instance Nf NatP where
  t2nf (NP ((S m), (S n))) = t2nf (NP (m, n))
  t2nf n = n


plusNP :: NatP -> NatP -> NatP
plusNP (NP (m, n)) (NP (o, p)) = t2nf (NP (plusNat m o, plusNat n p))

minusNP :: NatP -> NatP -> NatP
minusNP (NP (m, n)) (NP (o, p)) = t2nf (NP (plusNat m p, plusNat n o))

instance Eq NatP where
   u == v = case minusNP u v of
    (NP (Z, Z)) -> True
    _           -> False

grNat :: Nat -> Nat -> Bool
grNat (S _) Z = True
grNat (S m) (S n) = grNat m n
grNat _ _ = False

instance Ord RatNumbers where
  (Rat m n) `compare` (Rat p q)
    | (m `timesNat` q) `grNat` (n `timesNat` p) = GT
    | (n `timesNat` p) `grNat` (m `timesNat` q) = LT
    | otherwise                                 = EQ


instance Num RatNumbers where
  (Rat m n) + (Rat p q) =
    t2nf (Rat ((m `timesNat` q) `plusNat` (n `timesNat` p)) (n `timesNat` q))
  (Rat m n) - (Rat p q) =
    t2nf (Rat ((m `timesNat` q) `minusNat` (n `timesNat` p)) (n `timesNat` q))
  (Rat m n) * (Rat p q) =
    t2nf (Rat (m `timesNat` p) (n `timesNat` q))
  abs n = t2nf n
  signum _ = t2nf (Rat (S Z) (S Z))
  fromInteger i = t2nf (Rat (int2nat (abs i)) (S Z))

