module DD (Dict, empty, lookup, insert, destruct) where

import Prelude hiding (lookup)
import Nat

empty    :: Dict k v
lookup   :: NatLike k => k -> Dict k v -> Maybe v
insert   :: NatLike k => k -> v -> Dict k v -> Dict k v
destruct :: NatLike k => Dict k v -> Maybe ((k, v), Dict k v)

newtype Dict k v = DD (DeltaList v) deriving (Show)
type DeltaList v = [(Nat, v)]

empty = DD []

lookup k (DD []) = Nothing
lookup k (DD ((base, v) : dl)) = lookup_iter base ((0, v) : dl)
  where
    lookup_iter :: Nat -> DeltaList v -> Maybe v
    lookup_iter cur [] = Nothing
    lookup_iter cur ((delta, v) : dl)
      | toNat k < cur + delta  = Nothing
      | toNat k == cur + delta = Just v
      | otherwise              = lookup_iter (cur + delta + 1) dl

insert k v' (DD []) = DD [(toNat k, v')]
insert k v' (DD ((base, v) : dl))
  | toNat k < base  = DD ((toNat k, v') : (base - toNat k - 1, v) : dl)
  | toNat k == base = DD ((base, v') : dl)
  | otherwise       = DD ((base, v) : insert_iter (base + 1) dl)
      where
     -- insert_iter :: Nat -> DeltaList ^v -> DeltaList ^v
        insert_iter cur [] = [(toNat k - cur, v')]
        insert_iter cur ((delta, v) : dl)
          | toNat k < cur + delta  = (toNat k - cur, v') : (cur + delta - toNat k, v) : dl
          | toNat k == cur + delta = (delta, v') : dl
          | otherwise              = (delta, v) : insert_iter (cur + delta + 1) dl

destruct (DD [])                              = Nothing
destruct (DD [(base, v1)])                    = Just ((fromNat base, v1), DD [])
destruct (DD ((base, v1) : (delta, v2) : dl)) = Just (kv1, DD (kv2 : dl))
                                                  where kv1 = (fromNat base, v1)
                                                        kv2 = (base + delta + 1, v2)
