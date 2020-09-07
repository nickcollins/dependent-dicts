module Delta (Key : Set) \{\{bij : bij Key Nat\}\} where

...

data DD (V : Set) : Set where
  DD : DL V \altRArr DD V

DL : (V : Set) \altRArr Set
DL V = List (Nat \altAnd V)

delta
  : \altFAll\{n m\} \altRArr n < m \altRArr Nat
delta n<m =
  difference (n<m \altRArr 1+n\ensuremath{\leq}m n<m) -- i.e. m - n - 1

lookup
  : \{V : Set\} \altRArr DD V \altRArr K \altRArr Maybe V
lookup (DD dd) k =
  lkup dd (toNat k)
  where
    lkup : \{V : Set\} \altRArr DL V \altRArr Nat \altRArr Maybe V
    lkup [] n = None
    lkup ((hn, ha) :: t) n with <dec n hn
\twoChars{}... | Inl n<hn       = None
\twoChars{}... | Inr (Inl refl) = Some ha
\twoChars{}... | Inr (Inr hn<n) = lkup t (delta hn<n)

insert
  : \altFAll\{V\} \altRArr DD V \altRArr (K \altAnd V) \altRArr DD V
insert (DD dd) (k, v) =
  DD (ins dd (toNat k, v))
  where
    ins : \altFAll\{V\} \altRArr DL V \altRArr (Nat \altAnd V) \altRArr DD V
    ins (n, v) =
      (n, v) :: []
    ins ((hn, hv) :: t) (n, v) with <dec n hn
\twoChars{}... | Inl n<hn =
          (n, v) :: (delta n<hn, hv) :: t
\twoChars{}... | Inr (Inl refl) =
          (n, v) :: t
\twoChars{}... | Inr (Inr hn<n) =
          (hn, hv) :: ins t (delta hn<n, v)

destruct
  : \{V : Set\} \altRArr DD V \altRArr Maybe ((K \altAnd V), DD V)
destruct (DD []) = None
destruct (DD ((n, v) :: [])) =
  Some ((fromNat n, v), DD [])
destruct (DD ((n, v) :: (m, v') :: t)) =
  Some ((fromNat n, v), DD ((n + m + 1, v') :: t))
