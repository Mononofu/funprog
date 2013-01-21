\begin{code}

module Aufgabe4 where

data Nat = Z | S Nat deriving Show

instance Eq Nat where
  n1 == n2 = eqNat n1 n2

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

divNat :: Nat -> Nat -> Nat
divNat _ Z = error "Invalid argument"
divNat Z _ = Z
divNat m n 
  | grEqNat m n = plusNat (S Z) (divNat (minusNat m n) n)
  | otherwise   = Z


divNatUp :: Nat -> Nat -> Nat
divNatUp _ Z = error "Invalid argument"
divNatUp Z _ = Z
divNatUp m n 
  | grEqNat m Z = plusNat (S Z) (divNatUp (minusNat m n) n)
  | otherwise   = Z


modNat :: Nat -> Nat -> Nat
modNat _ Z = error "Invalid argument"
modNat m n = minusNat m (timesNat n (divNat m n))


eqNat :: Nat -> Nat -> Bool
eqNat Z Z = True
eqNat (S m) (S n) = eqNat m n
eqNat _ _ = False

grNat :: Nat -> Nat -> Bool
grNat (S _) Z = True
grNat (S m) (S n) = grNat m n
grNat _ _ = False

leNat :: Nat -> Nat -> Bool
leNat m n = not $ grEqNat m n

grEqNat :: Nat -> Nat -> Bool
grEqNat m n 
  | eqNat m n = True
  | otherwise = grNat m n

leEqNat :: Nat -> Nat -> Bool
leEqNat m n = not $ grNat m n



type NatPair = (Nat,Nat)


mkCan :: NatPair -> NatPair
mkCan ((S m), (S n)) = mkCan (m, n)
mkCan p = p


plusNP :: NatPair -> NatPair -> NatPair
plusNP (m, n) (o, p) = mkCan (plusNat m o, plusNat n p) 

minusNP :: NatPair -> NatPair -> NatPair
minusNP (m, n) (o, p) = mkCan (plusNat m p, plusNat n o)

absNP :: NatPair -> Nat
absNP x@(m, n) 
  | posNP x     = minusNat m n
  | otherwise   = minusNat n m

-- manually expand x*y = (m-n)*(o-p) to see why this is true
timesNP :: NatPair -> NatPair -> NatPair
timesNP (m, n) (o, p) = mkCan (plusNat (timesNat m o) (timesNat n p),
                               plusNat (timesNat m p) (timesNat o n))

-- x / y
divNP :: NatPair -> NatPair -> NatPair
divNP x y
  | nullNP y                 = error "Invalid argument"
  | posNP x && posNP y ||   
    negNP x && negNP y       = (divNat (absNP x) (absNP y), Z)
  | otherwise                = (Z, divNatUp (absNP x) (absNP y))


modNP :: NatPair -> NatPair -> NatPair
modNP u v 
  | nullNP v       = error "Invalid argument"
  | otherwise      = mkCan $ minusNP u (timesNP v (divNP u v))

nullNP :: NatPair -> Bool
nullNP (m, n) = eqNat m n

posNP :: NatPair -> Bool
posNP n = grEqNP n (Z, Z)

negNP :: NatPair -> Bool
negNP = not . posNP

eqNP :: NatPair -> NatPair -> Bool
eqNP u v = case minusNP u v of
  (Z, Z) -> True
  _      -> False

grNP :: NatPair -> NatPair -> Bool
grNP u v = case minusNP u v of
  (S _, Z) -> True
  _        -> False

leNP :: NatPair -> NatPair -> Bool
leNP u v = case minusNP u v of
  (Z, S _) -> True
  _        -> False

grEqNP :: NatPair -> NatPair -> Bool
grEqNP u v = case minusNP u v of
  (Z, Z)   -> True
  (S _, Z) -> True
  _        -> False

leEqNP :: NatPair -> NatPair -> Bool
leEqNP u v = case minusNP u v of
  (Z, Z)   -> True
  (Z, S _) -> True
  _        -> False


\end{code}