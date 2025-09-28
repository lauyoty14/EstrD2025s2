import PriorityQueue
import Distribution.Simple (VersionInterval)

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
-- O(n) donde n es la cantidad de claves del map

valoresDe :: Eq k => [k] -> Map k v -> [Maybe v]
valoresDe [] map = []
valoresDe (k:ks) map = lookupM k m : valoresDe ks map

------------------------------------------------------------------------------------------------------------------------------------

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] map = True
todasAsociadas (k:ks) map = estaEnLaLista k (keys map) && todasAsociadas ks map
--Propósito: indica si en el map se encuentran todas las claves dadas.
-- O(n * m) donde n es la cantidad de claves dadas y m la cantidad de claves del map

estaEnLaLista :: Eq a => a -> [a] -> Bool
estaEnLaLista _ [] = False
estaEnLaLista k (x:xs) = k == x || estaEnLaLista k xs

------------------------------------------------------------------------------------------------------------------------------------

listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap (kv:kvs) = claveValorToMap kv (listToMap kvs) 
-- Propósito: convierte una lista de pares clave valor en un map
-- O(n * n) donde n es el tamaño de la lista dada 

claveValorToMap :: Eq k => (k,v) -> Map k v -> Map k v
claveValorToMap (k,v) map = assocM k v map

------------------------------------------------------------------------------------------------------------------------------------

mapToList :: Eq k => Map k v -> [(k, Maybe v)]
mapToList emptyM = []
mapToList map = mapToListAux (keys map) map
-- Propósito: convierte un map en una lista de pares clave valor
-- O(n * n) donde n es la cantidad de claves valor del map

mapToListAux :: Eq k => [k] -> Map k v -> [(k, Maybe v)]
mapToListAux [] _ = []
mapToListAux (k:ks) map = (k , lookupM k map) : mapToListAux ks map 

------------------------------------------------------------------------------------------------------------------------------------

agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq (kv:kvs) = agruparEqAux kv (agruparEq kvs)
--Propósito: dada una lista de pares clave valor, agrupa los valores de los 
--pares que compartan la misma clave
-- O(n) donde n es la cantidad de pares clave valor de la lista

agruparEqAux :: Eq k => (k, v) -> Map k [v] -> Map k [v]
agruparEqAux (k, v) m = 
    case lookup k m of 
        Just vs -> assocM k (v:vs) m
        Nothing -> assocM k [v] m

------------------------------------------------------------------------------------------------------------------------------------

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le
-- suma uno a cada número asociado con dichas claves.
-- O(n * n) donde n es la cantidad de claves de la lista dada
incrementar [] m = m
incrementar (k:ks) m = 
    case lookupM k m of 
        Just v -> incrementar ks (assocM k (v + 1) m)
        Nothing -> incrementar ks (assocM k 1 m)

------------------------------------------------------------------------------------------------------------------------------------

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el 
-- segundo. Si una clave del primero existe en el segundo, es reemplazada por la
-- del primero.
-- O(n * n) donde es la cantidad de claves del primer map
mergeMaps emptyM m2 = m2
mergeMaps m1 m2 =
    let k = head (keys m1)
    in case lookupM k m1 of 
        Just v -> mergeMaps (deleteM k m1) (assocM k v m2)
        Nothing -> mergeMaps (deleteM k m1) m2

------------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 4 -- 
-- 1)
data Map k v = Map [(k, v)]

emptyM :: Map k v
emptyM = Map []
-- Propósito: devuelve un map vacío

assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (Map kv) = if (laClavePertenece k kv)
                      then Map (remplazarKV k v kv)
                      else Map ((k,v) : kv)
-- Propósito: agrega una asociación clave-valor al map.

lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (Map kv) = valorDeEn k kv
-- Propósito: encuentra un valor dado una clave.

deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (Map kv) = if (laClavePertenece k kv)
                     then Map (sacarAsociacionDeClave k kv)
                     else Map kv
-- Propósito: borra una asociación dada una clave.

keys :: Map k v -> [k]
keys (Map kvs) = clavesDe kvs
-- Propósito: devuelve las claves del map.

laClavePertenece :: Eq k => k -> [(k, v)] -> Bool
laClavePertenece k [] = False
laClavePertenece k (kv:kvs) = claveEstaEn k kv || laClavePertenece k kvs

claveEstaEn :: Eq k => k -> (k, v) -> Bool
claveEstaEn clave (k, v) = clave == k 

remplazarKV :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
remplazarKV _ _ [] = []
remplazarKV k v (kv:kvs) = if (claveEstaEn k kv)
                           then (k, v) : kvs
                           else remplazarKV k v kvs
 
valorDeEn :: Eq k => k -> [(k, v)] -> Maybe v
valorDeEn _ [] = Nothing
valorDeEn k (kv:kvs) = if (claveEstaEn k kv)
                       then valorDe kv
                       else valorDeEn k kvs

valorDe :: (k, v) -> Maybe v 
valorDe (_ , v) = Just v

sacarAsociacionDeClave :: Eq k => k -> [(k, v)] -> [(k, v)]
sacarAsociacionDeClave _ [] = []
sacarAsociacionDeClave k (kv:kvs) = if(claveEstaEn k kv)
                                    then kvs 
                                    else kv : sacarAsociacionDeClave k kvs

clavesDe :: [(k, v)] -> [k]
clavesDe [] = []
clavesDe (kv:kvs) = (claveDe kv) : clavesDe kvs

claveDe :: Eq k => (k, v) -> k
claveDe (k, _) = k

------------------------------------------------------------------------------------------------------------------------------------

-- 2)

data Map k v = Map [(k, v)]

emptyM :: Map k v
emptyM = Map []
-- Propósito: devuelve un map vacío

assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (Map kv) = Map ((k,v) : kv)
-- Propósito: agrega una asociación clave-valor al map.

lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (Map []) = Nothing
lookupM k (Map kv) = valorDeLaClaveEn k kv
-- Propósito: encuentra un valor dado una clave.

valorDeLaClaveEn :: Eq k => k -> [(k, v)] -> Maybe v
valorDeLaClaveEn k (kv:kvs) = if claveEstaEn k kv
                              then valorDe kv
                              else valorDeLaClaveEn k kvs

deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (Map kv) = if laClavePertenece k kv
                     then Map (sacarAsociacionDeClave k kv)
                     else Map kv
-- Propósito: borra una asociación dada una clave.

sacarAsociacionDeClave :: Eq k => k -> [(k, v)] -> [(k, v)]
sacarAsociacionDeClave _ [] = []
sacarAsociacionDeClave k (kv:kvs) = if claveEstaEn k kv
                                    then kvs 
                                    else kv : sacarAsociacionDeClave k kvs

keys :: Map k v -> [k]
keys (Map kvs) = clavesDe kvs
-- Propósito: devuelve las claves del map.

clavesDe :: [(k, v)] -> [k]
clavesDe [] = []
clavesDe (kv:kvs) = (claveDe kv) : clavesDe kvs

claveDe :: Eq k => (k, v) -> k
claveDe (k, _) = k

----------------------------------------------------------------------------------------------------------------------------------------

-- 3)
data Map k v = Map [k] [v]

emptyM :: Map k v
emptyM = Map [] []
-- Propósito: devuelve un map vacío

assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (Map ks vs) = Map (k:ks) (v:vs) 
-- Propósito: agrega una asociación clave-valor al map.
-- O(1) --

lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (Map [] _) = Nothing
lookupM k (Map ks vs) = if primeroDe ks == k
                        then Just (primeroDe vs) 
                        else lookupM k (Map (sinElPrimero ks) (sinElPrimero vs))
-- Propósito: encuentra un valor dado una clave.
-- O(n) donde n es el tamaño de la lista de llaves

primeroDe :: [x] -> x 
primeroDe [] = error "no se puede dar el primero de una lista vacia"
primeroDe (x:_) = x

sinElPrimero :: [x] -> [x]
sinElPrimero [] = []
sinElPrimero (x:xs) = xs 

deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (Map ks vs) = if primeroDe ks == k
                        then Map (sinElPrimero ks) (sinElPrimero vs)
                        else case deleteM k (sinElPrimero ks) (sinElPrimero vs) of 
                            Map ks' vs' -> Map (primeroDe ks : ks') (primeroDe vs : vs')
-- Propósito: borra una asociación dada una clave.
-- O(n) donde n es el tamaño de la lista de llaves

keys :: Map k v -> [k]
keys (Map ks vs) = ks
-- Propósito: devuelve las claves del map.
-- O(1) --

---------------------------------------------------------------------------------------------------------------------------------------

indexar :: [a] -> Map Int a
indexar xs = indexarDesde 0 xs
-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
-- su posición en la lista.

indexarDesde :: Int -> [a] -> Map Int a
indexarDesde _ []     = emptyM
indexarDesde n (x:xs) = assocM n x (indexarDesde (n+1) xs)

ocurrencias :: String -> Map Char Int
ocurrencias []     = emptyM
ocurrencias (c:cs) =
    case lookupM c (ocurrencias cs) of
        Just n  -> assocM c (n+1) (ocurrencias cs)
        Nothing -> assocM c 1     (ocurrencias cs)

