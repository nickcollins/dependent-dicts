module Nat where

data Nat = Z | S Nat deriving (Eq, Show)

toInt Z     = 0
toInt (S n) = 1 + toInt n

instance Num Nat where
  n + m  = fromInteger (toInt n + toInt m)
  n * m  = fromInteger (toInt n * toInt m)
  abs    = fromInteger . abs . toInt
  signum = fromInteger . signum . toInt

  Z - m     = Z
  n - Z     = n
  S n - S m = n - m

  fromInteger i
    | i < 0     = undefined -- CRASH!
    | i == 0    = Z
    | otherwise = S (fromInteger (i - 1))

instance Ord Nat where
  compare n m = compare (toInt n) (toInt m)

instance Enum Nat where
  toEnum i = fromInteger (fromIntegral i)
  fromEnum = toInt

class Bij a where
  toNat :: a -> Nat
  fromNat :: Nat -> a

instance Bij Nat where
  toNat = id
  fromNat = id
