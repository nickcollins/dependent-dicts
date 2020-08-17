module Test where

import Nat
import DD

x |> f = f x

dict1 :: Dict Nat String
dict1 =
  empty
    |> insert 6 "d"
    |> insert 3 "q"
    |> insert 1 "b"
    |> insert 3 "c"

test1 = do
  putStrLn $ show dict1
  mapM_ (\n -> putStrLn $ show $ (toInt n, DD.lookup n dict1)) [0..10]
