module Delta (K : Set) \{\{bij : bij K Nat\}\} where

DL : (V : Set) \altRArr Set
DL V = List (Nat \altAnd V)

data DD (V : Set) : Set where
  DD : DL V \altRArr DD V

delta
  : \altFAll\{n m\} \altRArr n < m \altRArr Nat
delta n<m =
  difference (n<m\altRArr1+n\ensuremath{\leq}m n<m) -- i.e. m - n - 1

-- lookup
_\altLAng_\altRAng
  : \altFAll\{V\} \altRArr DD V \altRArr K \altRArr Maybe V
(DD dd) \altLAng k \altRAng =
  lkup dd (toNat k)
  where
    lkup : \altFAll\{V\} \altRArr DL V \altRArr Nat \altRArr Maybe V
    lkup [] n = None
    lkup ((hn , ha) :: t) n
      with <dec n hn
\fourChars{}... | Inl n<hn       = None
\fourChars{}... | Inr (Inl refl) = Some ha
\fourChars{}... | Inr (Inr hn<n) = lkup t (delta hn<n)

-- insert/extend
_,,_
  : \altFAll\{V\} \altRArr DD V \altRArr (K \altAnd V) \altRArr DD V
(DD dd) ,, (k , v) =
  DD (ins dd (toNat k , v))
  where
    ins : \altFAll\{V\} \altRArr DL V \altRArr (Nat \altAnd V) \altRArr DL V
    ins [] (n , v) = (n , v) :: []
    ins ((hn , hv) :: t) (n , v)
      with <dec n hn
\fourChars{}... | Inl n<hn =
          (n , v) :: (delta n<hn , hv) :: t
\fourChars{}... | Inr (Inl refl) =
          (n , v) :: t
\fourChars{}... | Inr (Inr hn<n) =
          (hn , hv) :: ins t (delta hn<n , v)

destruct
  : \altFAll\{V\} \altRArr DD V \altRArr Maybe ((K \altAnd V) \altAnd DD V)
destruct (DD []) = None
destruct (DD ((n , v) :: [])) =
  Some ((fromNat n , v) , DD [])
destruct (DD ((n , v) :: (m , v') :: t)) =
  Some ((fromNat n , v) , DD ((n + m + 1 , v') :: t))
