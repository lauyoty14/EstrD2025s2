{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Queue 
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
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
enqueue x (Q xs) = Q (xs ++ [x])
-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- O(1) -- costo constante

firstQ :: Queue a -> a
firstQ (Q (x:xs)) = x
-- Dada una cola devuelve el primer elemento de la cola.
-- O(1) -- costo constante

dequeue :: Queue a -> Queue a
dequeue (Q (x:xs)) = Q xs
-- Dada una cola la devuelve sin su primer elemento
-- O(1) -- costo constante
