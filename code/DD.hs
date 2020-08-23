module DD (Dict, empty, lookup, insert, destruct) where

import Prelude hiding (lookup)
import Nat

empty    :: Dict k v
lookup   :: NatLike k => Dict k v -> k -> Maybe v
insert   :: NatLike k => Dict k v -> (k, v) -> Dict k v
destruct :: NatLike k => Dict k v -> Maybe ((k, v), Dict k v)

newtype Dict k v = DD (DeltaList v) deriving (Show)
type DeltaList v = [(Nat, v)]

empty = DD []

delta :: Nat -> Nat -> Nat
delta x y = y - x - 1

lookup (DD dd) k = lookup' dd (toNat k)
  where
    lookup' :: [(Nat, v)] -> Nat -> Maybe v
    lookup' [] _ = Nothing
    lookup' ((hx, hv) : t) x
      | x < hx    = Nothing
      | x == hx   = Just hv
      | otherwise = lookup' t (delta hx x)

insert (DD dd) (k, v) = DD (insert' dd (toNat k, v))
  where
    insert' :: [(Nat, v)] -> (Nat, v) -> [(Nat, v)]
    insert' [] (x, v) = [(x, v)]
    insert' ((hx, hv) : t) (x, v)
      | x < hx    = (x, v) : (delta x hx, hv) : t
      | x == hx   = (x, v) : t
      | otherwise = (hx, hv) : insert' t (delta hx x, v)

destruct (DD [])                              = Nothing
destruct (DD [(base, v1)])                    = Just ((fromNat base, v1), DD [])
destruct (DD ((base, v1) : (delta, v2) : dl)) = Just (kv1, DD (kv2 : dl))
                                                  where kv1 = (fromNat base, v1)
                                                        kv2 = (base + delta + 1, v2)
