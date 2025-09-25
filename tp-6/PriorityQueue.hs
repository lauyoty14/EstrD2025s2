module PriorityQueue(PriorityQueue, emptyPQ, isEmptyPQ
, insertPQ, findMinPQ, deleteMinPQ) where

import Data.List (delete)

data PriorityQueue a = Pq [a]

emptyPQ :: PriorityQueue a
emptyPQ = Pq []
-- costo O(1) -- 
--Propósito: devuelve una priority queue vacía.

isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (Pq as) = null as
-- costo O(1) --
--Propósito: indica si la priority queue está vacía.

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ a (Pq as) = Pq (a:as)
-- costo O(1) ---
--Propósito: inserta un elemento en la priority queue.

findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (Pq as) = if(null as)
                    then error "no hay minimo en lista vacia"
                    else minimoDe as
-- costo 0(n) donde n es el tamaño de la lista --
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (Pq as) = Pq (delete (minimoDe as) as)
-- costo O(n) donde n es el tamaño de la lista -- 
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía

minimoDe :: Ord a => [a] -> a 
minimoDe [] = error "error : no se puede hacer el minimo de lista vacia"
minimoDe [x] = x
minimoDe (x:xs) = minimoEntre x (minimoDe xs)

minimoEntre :: Ord a => a -> a -> a
minimoEntre x y = if (x > y)
                  then x 
                  else y