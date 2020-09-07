-- these definitions are inside a module that is
-- parameterized by the bij type-class and the key K

-- given that n < m, returns m - n - 1
delta : \altFAll\{n m\} \altRArr n < m \altRArr Nat
delta n<m = difference (n<m\altRArr1+n\ensuremath{\leq}m n<m)

-- insert/extend
_,,_ : \altFAll\{V\} \altRArr DD V \altRArr (K \altAnd V) \altRArr DD V
(DD dd') ,, (k , v) = DD (dd' ,,' (toNat k , v))
  where
  _,,'_ : \altFAll\{V\} \altRArr DD' V \altRArr (Nat \altAnd V) \altRArr DD' V
  [] ,,' (n , v) = (n , v) :: []
  ((hn , hv) :: t) ,,' (n , v)
    with <dec n hn
  ... | Inl n<hn       = (n , v) :: ((delta n<hn , hv) :: t)
  ... | Inr (Inl refl) = (n , v) :: t
  ... | Inr (Inr hn<n) = (hn , hv) :: (t ,,' (delta hn<n , v))

-- lookup
_\{\{_\}\} : \{V : Set\} \altRArr DD V \altRArr K \altRArr Maybe V
(DD dd') \{\{ k \}\} = dd' lkup (toNat k)
  where
  _lkup_ : \{V : Set\} \altRArr DD' V \altRArr Nat \altRArr Maybe V
  [] lkup n = None
  ((hn , ha) :: t) lkup n
    with <dec n hn
  ... | Inl n<hn       = None
  ... | Inr (Inl refl) = Some ha
  ... | Inr (Inr hn<n) = t lkup (delta hn<n)

destruct :: \{V : Set\} \altRArr DD V \altRArr Maybe ((K \altAnd V), DD K V)
destruct (DD []) = None
destruct (DD ((n , v) :: [])) =
  Some ((fromNat n , v), DD [])
destruct (DD ((n1 , v1) :: (n2 , v2) : t)) =
  Some ((fromNat n1 , v1), DD ((n1 + n2 + 1, v2) :: t))
