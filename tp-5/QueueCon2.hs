

data Queue2 a = Q [a] [a]

emptyQ :: Queue a
emptyQ = Q [] []
-- Crea una cola vacía.

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q [] []) = True
isEmptyQ _ = False
-- Dada una cola indica si la cola está vacía.

enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs ys) = Q xs (x:ys)
-- Dados un elemento y una cola, agrega ese elemento a la cola.

firstQ :: Queue a -> a
firstQ (Q [] ys) = head (last ys)
firstQ (Q (x:xs) ys) = x
-- Dada una cola devuelve el primer elemento de la cola.

dequeue :: Queue a -> Queue a
dequeue (Q [] ys) = Q (tail ys) []
dequeue (Q (x:xs) ys) = Q xs ys
-- Dada una cola la devuelve sin su primer elemento

--En la versión con una sola lista, una de las operaciones (enqueue o dequeue) siempre cuesta O(n).
--En esta versión con dos listas, todas las operaciones cuestan O(1) amortizado:
--Cada elemento se mueve como máximo una vez de bs a fs, por lo que el costo total repartido es constante.