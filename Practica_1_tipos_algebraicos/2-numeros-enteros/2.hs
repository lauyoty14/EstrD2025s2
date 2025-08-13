-- Dado un número devuelve su sucesor
sucesor :: Int -> Int
sucesor n = n + 1

--Dados dos números devuelve su suma utilizando la operación +.
sumar :: Int -> Int -> Int
sumar x y = x + y

{-
Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.
-}
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)

--Dado un par de números devuelve el mayor de estos.
maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = max x y

{-De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas
las funciones del punto anterior-}
expresion1 = sumar (maxDelPar (divisionYResto 10 5)) (sucesor 7)
expresion2 = maxDelPar (divisionYResto (sucesor 9) (sumar 0 1))
expresion3 = sucesor (maxDelPar (divisionYResto (sumar 3 6) 1))
expresion4 = sumar (sucesor 5) (maxDelPar (divisionYResto 8 2))