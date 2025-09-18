module Queue2 (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
    where

data Queue a = Q [a]
-- INV Re : la lista representa una cola donde el primer elemento de la lista es el frente de la cola
            deriving Show

emptyQ :: Queue a
emptyQ = Q []
-- Crea una cola vacía.

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs
-- Dada una cola indica si la cola está vacía.

enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (x:xs)
-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- O(1) -- costo constante

firstQ :: Queue a -> a
firstQ Q xs = last xs 
-- Dada una cola devuelve el primer elemento de la cola.
-- O(n) -- costo lineal donde n es la longitud de la lista

dequeue :: Queue a -> Queue a
dequeue Q xs = Q (last xs)
-- Dada una cola la devuelve sin su ultimo elemento
-- O(n) -- costo lineal donde n es la longitud de la lista

