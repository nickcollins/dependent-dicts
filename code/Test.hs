module Test where

import Nat
import DD

x |> f = f x

test1 =
  let
    d :: Dict Nat String
    d =
      empty
        |> insert 6 "d"
        |> insert 3 "q"
        |> insert 1 "b"
        |> insert 3 "c"
  in
  do
    putStrLn $ show d
    mapM_ (\n -> putStrLn $ show $ (toInt n, DD.lookup n d)) [0..10]
