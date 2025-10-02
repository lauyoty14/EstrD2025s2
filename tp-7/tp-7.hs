-- ejercicio 1 : heapsort es de costo O(n log n) siendo n la cantidad de elementos de la lista
-- ejercicio 2
data TreeBST a = EmptyTB | NodeTB a (TreeBST a) (TreeBST a)

belongsBST :: Ord a => a -> TreeBST a -> Bool
belongsBST _ EmptyTB = False
belongsBST x (NodeTB y ti td) =  if(x == y)
                                then True
                                else if (x < y)
                                    then belongsBST x ti
                                    else belongsBST x td                         
-- Propósito: dado un BST dice si el elemento pertenece o no al árbol.
-- Costo: O(log N)

insertBST :: Ord a => a -> TreeBST a -> TreeBST a
insertBST x EmptyTB = NodeT x EmptyTB EmptyTB
insertBST x (NodeT y ti td) = if (x == y)
                              then NodeTB y ti td 
                              else if (x < y)
                                then NodeTB y (insertBST x ti) td 
                                else NodeTB y ti (insertBST x td)
-- Propósito: dado un BST inserta un elemento en el árbol.
-- Costo: O(log N)

deleteBST :: Ord a => a -> TreeBST a -> TreeBST a
deleteBST x EmptyTB = EmptyTB
deleteBST x (NodeTB y ti td) = if (x == y)
                              then rearmarBST ti td 
                              else if (x < y) 
                                then NodeTB y (deleteBST x ti) td 
                                else NodeTB y ti (deleteBST x td)
-- Propósito: dado un BST borra un elemento en el árbol.
-- Costo: O(log N)

rearmarBST :: Ord a => TreeBST a -> TreeBST a -> TreeBST a
 -- PRECOND: ambos árboles son BSTs
rearmarBST EmptyTB td = td
rearmarBST ti td = NodeTB (maxBST ti) (delMaxBST ti) td

maxBST :: TreeBST a -> a
maxBST (NodeTB x _ EmptyTB) = x -- PRECOND: no es vacío
maxBST (NodeTB _ _ td) = maxBST td

delMaxBST :: TreeBST a -> TreeBST a
delMaxBST (NodeTB _ ti EmptyTB) = ti -- PRECOND: no es vacío
delMaxBST (NodeTB x ti td) = NodeTB x ti (delMaxBST td)

splitMinBST :: Ord a => TreeBST a -> (a, TreeBST a)
splitMinBST EmptyTB = error "no hay minimo elemento"
splitMinBST (NodeTB x ti td) = (minBST ti , delMinBST ti)
-- Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- Costo: O(log N)

minBST :: TreeBST a -> a
minBST (NodeTB x EmptyTB _) = x -- PRECOND : el tree no es vacio
minBST (NodeTB _ ti _) = minBST ti

delMinBST :: TreeBST a -> TreeBST a
delMinBST (NodeTB _ EmptyTB td) = td -- PRECOND: no es vacío
delMinBST (NodeTB x ti td) = NodeTB x (delMinBST ti) td

splitMaxBST :: Ord a => TreeBST a -> (a, TreeBST a)
splitMaxBST EmptyTB = error "no hay maximo elemento"
splitMaxBST (NodeTB x ti td) = (maxBST td, delMaxBST td)
-- Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- Costo: O(log N)
 
esBST :: TreeBST a -> Bool
esBST EmptyTB = True
esBST (NodeTB x ti td) = esMayorAlArbol x ti && esMenorAlArbol x td
-- Propósito: indica si el árbol cumple con los invariantes de BST.
-- Costo: O(N 2)

esMayorAlArbol :: Ord a -> a -> TreeBST a -> Bool
esMayorAlArbol x EmptyTB = True 
esMayorAlArbol x (NodeTB y ti td) = x > y && esMayorAlArbol x ti && esMayorAlArbol x td

esMenorAlArbol :: Ord a -> a -> TreeBST a -> Bool
esMenorAlArbol x EmptyTB = True
esMenorAlArbol x (NodeTB y ti td) = x < y && esMenorAlArbol x ti && esMenorAlArbol x td

elMaximoMenorA :: Ord a => a -> TreeBST a -> Maybe a
elMaximoMenorA x treeBST = 
                case maximoMenor x treeBST of
                    Just v -> v 
                    Nothing -> error "no hay un maximo menor"   
-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
-- elemento dado.
-- Costo: O(log N )

maximoMenor :: Ord a => a -> TreeBST a -> Maybe a
maximoMenor x (NodeTB y ti td) = if y < x && y > maximoMenor x ti 
                                 then Just y
                                 else maximoMenor x ti 
