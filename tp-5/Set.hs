module Set 
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) 
where

data Set a = S [a] Int 
-- INV Re : la lista no tiene elementos repetidos y el Int es la cantidad de elementos de la lista
            deriving Show
            
-- Crea un conjunto vacío.
emptyS :: Set a
emptyS = S [] 0

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS x (S xs n) = if pertenece x xs
                    then S xs n
                    else S (x:xs) (n+1)
-- O(n) -- costo lineal donde n es la longitud de la lista

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs n) = pertenece x xs

-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (S xs n) = n

-- Borra un elemento del conjunto.
removeS :: Eq a => a -> Set a -> Set a
removeS x (S xs n) = if pertenece x xs
                        then S (sacar x xs) (n-1)
                        else S xs n

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs _) (S ys _) = setSinRepetidos (xs ++ ys)
-- O(n) -- costo lineal donde n es la longitud de la lista resultante 

setSinRepetidos :: Eq a => [a] -> Set a
setSinRepetidos xs = S (sinRepetidos xs) (longitud (sinRepetidos xs))

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto
setToList :: Eq a => Set a -> [a]
setToList (S xs n) = xs