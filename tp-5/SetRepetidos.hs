{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module SetRepetidos where

data Set a = S [a] Int
-- INV Re : la lista tiene elementos repetidos y el Int es la cantidad de elementos

emptyS :: Set a
emptyS = S [] 0

addS :: Eq a => a -> Set a -> Set a
addS x (S xs n) = S (x:xs) (length (quitarRepetidos (x:xs)))
-- O(n) -- costo lineal donde n es la longitud de la lista

quitarRepetidos :: Eq a => [a] -> [a]
quitarRepetidos [] = []
quitarRepetidos (x:xs) = if pertenece x xs
                          then quitarRepetidos xs
                          else x : quitarRepetidos xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs n) = pertenece x xs

sizeS :: Eq a => Set a -> Int
sizeS (S xs n) = n

-- Borra un elemento del conjunto.
removeS :: Eq a => a -> Set a -> Set a
removeS x (S xs n) = if pertenece x xs
                        then S (sacar x xs) (n-1)
                        else S xs n

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs n) (S ys m) = S (xs ++ ys) (longitud (quitarRepetidos (xs ++ ys)))
-- O(n) -- costo lineal donde n es la longitud de la lista resultante

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

setToList :: Eq a => Set a -> [a]
setToList (S xs n) = quitarRepetidos xs

