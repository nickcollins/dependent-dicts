module Test where

import Nat
import DD

x |> f = f x

ins k v d = DD.insert d (k, v)

dict1 :: DD Nat String
dict1 =
  empty
    |> ins 6 "d"
    |> ins 3 "q"
    |> ins 1 "b"
    |> ins 3 "c"

test1 = do
  putStrLn $ show dict1
  mapM_ (\n -> putStrLn $ show $ (toInt n, DD.lookup dict1 n)) [0..10]
