import Data.IntMap (insert, split)
import Data.Sequence (Seq(Empty))
-- ejercicio 1 : heapsort es de costo O(n log n) siendo n la cantidad de elementos de la lista
-- ejercicio 2
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST _ EmptyT = False
belongsBST x (NodeT y ti td) =  if(x == y)
                                then True
                                else if (x < y)
                                    then belongsBST x ti
                                    else belongsBST x td                         
-- Propósito: dado un BST dice si el elemento pertenece o no al árbol.
-- Costo: O(log N)

insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x EmptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if (x == y)
                              then NodeT y ti td 
                              else if (x < y)
                                then NodeT y (insertBST x ti) td 
                                else NodeT y ti (insertBST x td)
-- Propósito: dado un BST inserta un elemento en el árbol.
-- Costo: O(log N)

deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST x EmptyT = EmptyT
deleteBST x (NodeT y ti td) = if (x == y)
                              then rearmarBST ti td 
                              else if (x < y) 
                                then NodeT y (deleteBST x ti) td 
                                else NodeT y ti (deleteBST x td)
-- Propósito: dado un BST borra un elemento en el árbol.
-- Costo: O(log N)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
 -- PRECOND: ambos árboles son BSTs
rearmarBST EmptyT td = td
rearmarBST ti td = NodeT (maxBST ti) (delMaxBST ti) td

maxBST :: Tree a -> a
maxBST (NodeT x _ EmptyT) = x -- PRECOND: no es vacío
maxBST (NodeT _ _ td) = maxBST td

delMaxBST :: Tree a -> Tree a
delMaxBST (NodeT _ ti EmptyT) = ti -- PRECOND: no es vacío
delMaxBST (NodeT x ti td) = NodeT x ti (delMaxBST td)

splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST EmptyT = error "no hay minimo elemento"
splitMinBST (NodeT x ti td) = (minBST ti , delMinBST ti)
-- Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- Costo: O(log N)

minBST :: Tree a -> a
minBST (NodeT x EmptyT _) = x -- PRECOND : el tree no es vacio
minBST (NodeT _ ti _) = minBST ti

delMinBST :: Tree a -> Tree a
delMinBST (NodeT _ EmptyT td) = td -- PRECOND: no es vacío
delMinBST (NodeT x ti td) = NodeT x (delMinBST ti) td

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST EmptyT = error "no hay maximo elemento"
splitMaxBST (NodeT x ti td) = (maxBST td, delMaxBST td)
-- Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- Costo: O(log N)
