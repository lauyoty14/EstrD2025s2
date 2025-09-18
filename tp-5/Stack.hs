{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}


data Stack a = Stack [a]
-- INV Re : la lista representa una pila donde el primer elemento de la lista es el tope de la pila
            deriving Show

emptyS :: Stack a
emptyS = Stack []
-- Crea una pila vacía.

isEmptyS :: Stack a -> Bool
isEmptyS (Stack xs) = null xs
-- Dada una pila indica si está vacía.

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)
-- Dados un elemento y una pila, agrega el elemento a la pila.

top :: Stack a -> a
top (Stack (x:xs)) = x
-- Dada un pila devuelve el elemento del tope de la pila.

pop :: Stack a -> Stack a
pop (Stack (x:xs)) = Stack xs
-- Dada una pila devuelve la pila sin el primer elemento.

lenS :: Stack a -> Int
lenS (Stack xs) = length xs
-- Dada la cantidad de elementos en la pila.
-- Costo: constante.
