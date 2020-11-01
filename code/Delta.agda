module Delta (K : Set) \{\{bij : bij K Nat\}\} where

  DL : (V : Set) \altRArr Set
  DL V = List (Nat \altTimes V)

  data DD (V : Set) : Set where
    DD : DL V \altRArr DD V

  delta : \altFAll\{n m\} \altRArr n < m \altRArr Nat
  -- In Agda, almost any character can appear in an identifier.
  -- So 'n<m' is the variable name for a proof that 'n < m', whereas 'n<m\altRArr1+n\ensuremath{\leq}m' is a theorem.
  -- 'difference (n<m\altRArr1+n\ensuremath{\leq}m n<m)' evaluates to 'm - n - 1'
  delta n<m = difference (n<m\altRArr1+n\ensuremath{\leq}m n<m)

  -- lookup
  _\altLAng_\altRAng : \altFAll\{V\} \altRArr DD V \altRArr K \altRArr Maybe V
  (DD dd) \altLAng k \altRAng =
    lkup dd (toNat k)
    where
      lkup : \altFAll\{V\} \altRArr DL V \altRArr Nat \altRArr Maybe V
      lkup [] n = None
      lkup ((hn , ha) :: t) n
        with <dec n hn
      ... | Inl _          = None                 -- n < hn
      ... | Inr (Inl _)    = Some ha              -- n == hn
      ... | Inr (Inr hn<n) = lkup t (delta hn<n)  -- hn < n

  -- insert/extend
  _,,_ : \altFAll\{V\} \altRArr DD V \altRArr (K \altTimes V) \altRArr DD V
  (DD dd) ,, (k , v) =
    DD (ins dd (toNat k , v))
    where
      ins : \altFAll\{V\} \altRArr DL V \altRArr (Nat \altTimes V) \altRArr DL V
      ins [] (n , v) = (n , v) :: []
      ins ((hn , hv) :: t) (n , v)
        with <dec n hn
      ... | Inl n<hn       = (n , v) :: (delta n<hn , hv) :: t
      ... | Inr (Inl _)    = (n , v) :: t
      ... | Inr (Inr hn<n) = (hn , hv) :: ins t (delta hn<n , v)

  destruct : \altFAll\{V\} \altRArr DD V \altRArr Maybe ((K \altTimes V) \altTimes DD V)
  destruct (DD [])                         = None
  destruct (DD ((n , v) :: []))            = Some ((fromNat n , v) , DD [])
  destruct (DD ((n , v) :: (m , v') :: t)) = Some ((fromNat n , v) , DD ((n + m + 1 , v') :: t))
