delta :: Nat -> Nat -> Nat
delta x y = y - x - 1

lookup :: NatLike k => Dict k v -> k -> Maybe v
lookup (DD dd) k = lookup' dd (toNat k)
  where
    lookup' :: [(Nat, v)] -> Nat -> Maybe v
    lookup' [] _ = Nothing
    lookup' ((hx, hv) : t) x
      | x < hx    = Nothing
      | x == hx   = Just hv
      | otherwise = lookup' t (delta hx x)

insert :: NatLike k => Dict k v -> (k, v) -> Dict k v
insert (DD dd) (k, v) = DD (insert' dd (toNat k, v))
  where
    insert' :: [(Nat, v)] -> (Nat, v) -> [(Nat, v)]
    insert' [] (x, v) = [(x, v)]
    insert' ((hx, hv) : t) (x, v)
      | x < hx    = (x, v) : (delta x hx, hv) : t
      | x == hx   = (x, v) : t
      | otherwise = (hx, hv) : insert' t (delta hx x, v)
