{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
import Set
import Queue
import Stack

head' :: [a] -> a
head' (x:xs) = x
-- O(1) -- costo constante
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
--  O(1) -- costo constante
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
-- O(n) -- costo lineal donde n es el valor del argumento
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
-- O(n) -- costo lineal donde n es la longitud de la lista
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
-- O(n*m) -- costo cuadratico donde n es la longitud de la lista y m el valor maximo de los elementos de la lista
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs
-- O(n) -- costo lineal donde n es la longitud de la lista
sinRepetidos' :: Eq a => [a] -> [a]
sinRepetidos' [] = []
sinRepetidos' (x:xs) = if pertenece x xs
                        then sinRepetidos' xs
                        else x : sinRepetidos' xs
-- O(n^2) -- costo cuadratico donde n es la longitud de la lista
-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys
-- O(n) -- costo lineal donde n es la longitud de la primera lista
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs
-- O(n*m) -- costo cuadratico donde n es la longitud de la lista y m la longitud promedio de los strings en la lista
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs
-- O(n) -- costo lineal donde n es el valor del primer argumento
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs
-- O(n) -- costo lineal donde n es el valor del primer argumento
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)
-- O(n * m) -- costo cuadratico donde n es el valor del primer argumento y m la longitud de la lista
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)
-- O(n) -- costo lineal donde n es la longitud de la lista
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs = let m = minimo xs in m : ordenar (sacar m xs)
-- O(n^2) -- costo cuadratico donde n es la longitud de la lista

--------------------------------------------------------------------------------------------------------
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
              deriving Show

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertecen [] s = []
losQuePertenecen (x:xs) s = if belongs x s
                              then x : losQuePertenecen xs s
                              else losQuePertenecen xs s

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos xs = sinRepetidosAux xs emptyS 

sinRepetidosAux :: Eq a => [a] -> Set a -> [a]
sinRepetidosAux [] _ = []
sinRepetidosAux (x:xs) s = if belongs x s
                              then sinRepetidosAux xs s
                              else x : sinRepetidosAux xs (addS x s)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s ti td) = unionS s (unionS (unirTodos ti) (unirTodos td))

----------- 3 Queue (cola) --------------------------------------------------------------

lengthQ :: Queue a -> Int
lengthQ queue = if isEmptyQ queue
                then 0
                else 1 + lengthQ (dequeue queue)
-- O(n) -- costo lineal donde n es la cantidad de elementos en la cola

queueToList :: Queue a -> [a]
queueToList queue = if isEmptyQ queue
                   then []
                   else firstQ queue : queueToList (dequeue queue)
-- O(n) -- costo lineal donde n es la cantidad de elementos en la cola

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q1
                  then q2
                  else unionQ (dequeue q1) (enqueue (firstQ q1) q2)
-- O(n) -- costo lineal donde n es la cantidad de elementos en la primera cola

--------- 4 Stack (pila) --------------------------------------------------------------

apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs)
-- O(n) -- costo lineal donde n es la longitud de la lista

desapilar :: Stack a -> [a]
desapilar stack = if isEmptyS stack
                    then []
                    else top stack : desapilar (pop stack)
-- O(n) -- costo lineal donde n es la cantidad de elementos en la pila

insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 e stack = push e stack
insertarEnPos n e stack = push (top stack) (insertarEnPos (n-1) e (pop stack))
-- O(m) -- costo lineal donde m es el minimo entre n y la cantidad de