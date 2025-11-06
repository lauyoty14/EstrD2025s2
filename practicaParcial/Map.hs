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