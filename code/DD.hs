module DD
  ( DD, Key(..)
  , empty, lookup, insert, destruct ) where

import Prelude hiding (lookup)
import Nat

class Key a where
  toNat   :: a -> Nat
  -- in Agda, fromNat is defined indirectly
  fromNat :: Nat -> a

newtype DD k v = DD [(Nat, v)] deriving (Show)

empty :: DD k v
empty = DD []

delta :: Nat -> Nat -> Nat
delta x y = y - x - 1

lookup :: Key k => DD k v -> k -> Maybe v
lookup (DD dd) k = lkup dd (toNat k)
  where
    lkup :: [(Nat, v)] -> Nat -> Maybe v
    lkup [] _ = Nothing
    lkup ((hx, hv) : t) x
      | x < hx  = Nothing
      | x == hx = Just hv
      | x > hx  = lkup t (delta hx x)

insert :: Key k => DD k v -> (k, v) -> DD k v
insert (DD dd) (k, v) = DD (ins dd (toNat k, v))
  where
    ins :: [(Nat, v)] -> (Nat, v) -> [(Nat, v)]
    ins [] (x, v) = [(x, v)]
    ins ((hx, hv) : t) (x, v)
      | x < hx  = (x, v) : (delta x hx, hv) : t
      | x == hx = (x, v) : t
      | x > hx  = (hx, hv) : ins t (delta hx x, v)

destruct :: Key k => DD k v -> Maybe ((k, v), DD k v)
destruct (DD []) = Nothing
destruct (DD [(x, v1)]) =
  Just ((fromNat x, v1), DD [])
destruct (DD ((x, v1) : (y, v2) : t)) =
  Just ((fromNat x, v1), DD ((x + y + 1, v2) : t))
