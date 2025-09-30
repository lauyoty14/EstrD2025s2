import Map
data MultiSet a = MS (Map a Int)

emptyMS :: MultiSet a
emptyMS = MS emptyM
-- Propósito: denota un multiconjunto vacío.
-- O(1) --

addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMs x (MS m) =
    case lookupM x m of
        Just n -> MS (assocM x (n+1) m)
        Nothing -> MS (assocM x 1 m) 
-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
-- multiconjunto.
-- O(n) donde n es la cantidad de elementos del map que tiene que recorrer para buscar el valor dado 

ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS x (MS m) =
    case lookupM x m of
        Just n -> n
        Nothing -> 0
-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
-- elemento en el multiconjunto.
-- O(n) donde N es la cantidad de elementos en la lista del map dentro del multiSet 

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS (MS m1) (MS m2) = MS (unionMaps m1 m2)
-- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
-- ambos multiconjuntos.
-- O(n*n) donde n es la cantidad de claves en el mapa del primer multiSet

unionMaps :: Ord a => Map a Int -> Map a Int -> Map a Int
unionMaps m1 m2 = case keys m1 of
    []     -> m2
    (k:ks) -> case lookupM k m2 of
                 Just n  -> assocM k (n + valueAt k m1) (unionMaps (deleteM k m1) m2)
                 Nothing -> assocM k (valueAt k m1)     (unionMaps (deleteM k m1) m2)

valueAt k m = case lookupM k m of
                 Just n  -> n
                 Nothing -> 0

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
intersectionMS (MS m1) (MS m2) = MS (intersectionMaps m1 m2)
-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común.
-- O(n²) donde n es la cantidad de llaves que tiene el map del primer multiSet

intersectionMaps :: Ord a => Map a Int -> Map a Int -> Map a Int
intersectionMaps m1 m2 = 
    case keys m1 of
        [] -> emptyM
        (k:ks) -> case lookupM k m2 of
                    Just n2 -> let n1 = valueAt k m1
                                in assocM k (min n1 n2) (intersectionMaps (deleteM k m1) m2)
                    Nothing -> intersectionMaps (delete k m1) m2

multiSetToList :: MultiSet a -> [(a, Int)]
multiSetToList (MS m) = mapToList m
-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.
-- O(n²) donde n es la cantidad de keys en el mapa del multiSet

mapToList :: Map a Int -> [(a, Int)]
mapToList m = case keys m of
                [] -> []
                (k:ks) -> (k, valueAt k m) : mapToList (deleteM k m)
