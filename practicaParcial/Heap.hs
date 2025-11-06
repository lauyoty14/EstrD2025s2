module Heap (
    Heap,
    emptyH,     -- O(1)
    isEmptyH,   -- O(1)
    insertH,    -- O(log n)
    findMinH,   -- O(1)
    deleteMinH, -- O(log n)
    mergeH,     -- O(log n)
    toListH     -- O(n log n)
) where
-- Implementación básica de Heap mínima (min-heap binomial)

data Heap a = E | N a (Heap a) (Heap a)
    deriving (Eq, Show)

emptyH :: Heap a
emptyH = E

isEmptyH :: Heap a -> Bool
isEmptyH E = True
isEmptyH _ = False

-- Inserta un elemento (O(log n))
insertH :: Ord a => a -> Heap a -> Heap a
insertH x h = mergeH (N x E E) h

-- Devuelve el mínimo
findMinH :: Ord a => Heap a -> a
findMinH E = error "findMinH: heap vacía"
findMinH (N x _ _) = x

-- Elimina el mínimo
deleteMinH :: Ord a => Heap a -> Heap a
deleteMinH E = error "deleteMinH: heap vacía"
deleteMinH (N _ l r) = mergeH l r

-- Une dos heaps (O(log n))
mergeH :: Ord a => Heap a -> Heap a -> Heap a
mergeH E h = h
mergeH h E = h
mergeH h1@(N x l1 r1) h2@(N y l2 r2)
    | x <= y    = N x (mergeH r1 h2) l1
    | otherwise = N y (mergeH h1 r2) l2

-- Convierte a lista ordenada (para depurar o imprimir)
toListH :: Ord a => Heap a -> [a]
toListH h
    | isEmptyH h = []
    | otherwise  = findMinH h : toListH (deleteMinH h)
