--Dado un par de números devuelve el mayor de estos.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use max" #-}
maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y then x else y