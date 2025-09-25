import PriorityQueue

heapSort :: Ord a => [a] -> [a]
heapSort xs = pqToList (listToPQ xs)

listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ [] = emptyPQ
listToPQ (x:xs) = insertPQ x (listToPQ xs)

pqToList :: Ord a => PriorityQueue a -> [a]
pqToList pq = if (isEmptyPQ pq)
              then []
              else pqToList (deleteMinPQ pq) ++ [(findMinPQ pq)]

----------------------------------------------------------------------------------------------------------------------------------------------------
            
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = valoresDe (keys map) map 
-- Propósito: obtiene los valores asociados a cada clave del map

valoresDe :: Eq k => [k] -> Map k v -> [Maybe v]
valoresDe [] map = []
valoresDe (k:ks) map = lookupM k m : valoresDe ks map

------------------------------------------------------------------------------------------------------------------------------------

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] map = True
todasAsociadas (k:ks) map = estaEnLaLista k (keys map) && todasAsociadas ks map
--Propósito: indica si en el map se encuentran todas las claves dadas.

estaEnLaLista :: Eq a => a -> [a] -> Bool
estaEnLaLista _ [] = False
estaEnLaLista k (x:xs) = k == x || estaEnLaLista k xs

------------------------------------------------------------------------------------------------------------------------------------

listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap (kv:kvs) = claveValorToMap kv (listToMap kvs) 
-- Propósito: convierte una lista de pares clave valor en un map

claveValorToMap :: Eq k => (k,v) -> Map k v -> Map k v
claveValorToMap (k,v) map = assocM k v map

------------------------------------------------------------------------------------------------------------------------------------

mapToList :: Eq k => Map k v -> [(k, Maybe v)]
mapToList emptyM = []
mapToList map = mapToListAux (keys map) map
-- Propósito: convierte un map en una lista de pares clave valor

mapToListAux :: Eq k => [k] -> Map k v -> [(k, Maybe v)]
mapToListAux [] _ = []
mapToListAux (k:ks) map = (k , lookupM k map) : mapToListAux ks map 

------------------------------------------------------------------------------------------------------------------------------------

agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq (kv:kvs) = agruparEqAux kv (agruparEq kvs)
--Propósito: dada una lista de pares clave valor, agrupa los valores de los 
--pares que compartan la misma clave

agruparEqAux :: Eq k => (k, v) -> Map k [v] -> Map k [v]
agruparEqAux (k, v) map = assocM k (valoresDeLaClave k map) map

valoresDeLaClave :: Eq k => k -> Map k v -> [v]
valoresDeLaCLave k emptyM = []
valoresDeLaClave k map = lookupM k map : valoresDeLaClave k (deleteM k map)
