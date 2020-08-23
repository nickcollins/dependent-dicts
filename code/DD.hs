module DD
  (DD, empty, lookup, insert, uncons) where

import Prelude hiding (lookup)
import Nat  -- exposes Bij class

newtype DD k v = DD [(Nat, v)] deriving (Show)

empty :: DD k v
empty = DD []

delta :: Nat -> Nat -> Nat
delta x y = y - x - 1

lookup :: Bij k => DD k v -> k -> Maybe v
lookup (DD dd) k = look dd (toNat k)
  where
    look :: [(Nat, v)] -> Nat -> Maybe v
    look [] _ = Nothing
    look ((hx, hv) : t) x
      | x < hx  = Nothing
      | x == hx = Just hv
      | True    = look t (delta hx x)

insert :: Bij k => DD k v -> (k, v) -> DD k v
insert (DD dd) (k, v) = DD (ins dd (toNat k, v))
  where
    ins :: [(Nat, v)] -> (Nat, v) -> [(Nat, v)]
    ins [] (x, v) = [(x, v)]
    ins ((hx, hv) : t) (x, v)
      | x < hx  = (x, v) : (delta x hx, hv) : t
      | x == hx = (x, v) : t
      | True    = (hx, hv) : ins t (delta hx x, v)

uncons :: Bij k => DD k v -> Maybe ((k, v), DD k v)
uncons (DD []) = Nothing
uncons (DD [(x, v)]) = Just ((fromNat x, v), DD [])
uncons (DD ((x, v1) : (y, v2) : t)) =
  Just ((fromNat x, v1), DD ((x + y + 1, v2) : t))
