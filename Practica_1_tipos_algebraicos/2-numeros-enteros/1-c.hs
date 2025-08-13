{-
Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.
-}
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)