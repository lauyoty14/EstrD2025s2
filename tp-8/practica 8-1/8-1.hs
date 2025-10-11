data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible

-- Sector
crearS :: SectorId -> Sector -- O(1)
sectorId :: Sector -> SectorId -- O(1)
componentesS :: Sector -> [Componente] -- O(1)
tripulantesS :: Sector -> Set Nombre -- O(1)
agregarC :: Componente -> Sector -> Sector -- O(1)
agregarT :: Nombre -> Sector -> Sector -- O(log T)

-- Tripulante
crearT :: Nombre -> Rango -> Tripulante -- O(1)
asignarS :: SectorId -> Tripulante -> Tripulante -- O(log S)
sectoresT :: Tripulante -> Set SectorId -- O(1)
nombre :: Tripulante -> String -- O(1)
rango :: Tripulante -> Rango -- O(1)

-- MaxHeap
emptyH :: MaxHeap a -- O(1)
isEmptyH :: MaxHeap a -> Bool -- O(1)
insertH :: a -> MaxHeap a -> MaxHeap a -- O(log M)
maxH :: MaxHeap a -> a -- O(1)
deleteMaxH :: MaxHeap a -> MaxHeap a -- O(log M)

-- Set
emptyS :: Set a -- O(1)
addS :: a -> Set a -> Set a -- O(log N)
belongsS :: a -> Set a -> Bool -- O(log N)
unionS :: Set a -> Set a -> Set a -- O(N log N)
setToList :: Set a -> [a] -- O(N)
sizeS :: Set a -> Int

-- Map
emptyM :: Map k v -- O(1)
assocM :: k -> v -> Map k v -> Map k v -- O(log K)
lookupM :: k -> Map k v -> Maybe v -- O(log K)
deleteM :: k -> Map k v -> Map k v -- O(log K)
domM :: Map k v -> [k]



data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

construir :: [SectorId] -> Nave
construir [] = N emptyM emptyM emptyH
construir sectores = N (añadirSectores sectores) emptyM emptyH
-- Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores 
-- de sectores.
-- Eficiencia: O(S)

añadirSectores :: [SectorId] -> Map SectorId Sector
añadirSectores [] = emptyM
añadirSectores (si:sis) = assocM si (crearS si) (añadirSectores sis)

ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N mapS mapT mht) = 
    let newTrip = crearT n r
    in N mapS (assocM n newTrip mapT) (insertH newTrip mht)
-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
-- Eficiencia: O(log T)

sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N _ mapT _) = 
    case lookupM n mapT of 
        Nothing -> error "el tripulante no esta en la nave"
        Just t -> sectoresT t
-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
-- Eficiencia: O(log M)

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector sid (N mapS mapT mht) = 
    case lookupM sid mapS of 
        Nothing -> error "no hay un sector con ese id"
        Just s -> (tripulantesS s ,componentesS s)
-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese 
-- sector.
-- Precondición: Existe un sector con dicho id.
-- Eficiencia: O(log S)

tripulantesN :: Nave -> [Tripulante] 
tripulantesN (N _ _ emptyH) = [] 
tripulantesN (N _ _ mht) = (maxH mht) : tripulantesN (N emptyM emptyM (deleteMaxH mht)) 
-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor. 
-- Eficiencia: O(T log T) donde T es la cantidad de tripulantes del heap

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector [] _ nave = nave
agregarASector comps si (N mapS mapT mht) = 
    case lookupM si mapS of    
        Nothing -> error "no hay un sector con ese id en la nave"
        Just s -> 
            let newSect = agregarComponentesA comps s
            in N (assocM si newSect mapS) mapT mht
-- Propósito: Asigna una lista de componentes a un sector de la nave.
-- Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.

agregarComponentesA :: [Componente] -> Sector -> Sector
agregarComponentesA [] s = s 
agregarComponentesA (c:cs) s = agregarComponentesA cs (agregarC c s)
-- eficiencia : O(C) siendo C la cantidad de componentes en la lista

asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n si (N mapS mapT mht) = 
    case lookupM n mapT of 
        Nothing -> error "no hay un tripulante con ese nombre en la nave"
        Just t -> 
            case lookupM si mapS of 
                Nothing -> error "no hay un sector con ese id en la nave"
                Just s -> 
                    let sectorNuevo = agregarT n s 
                        tripulanteNuevo = asignarS si t
                        newMapT = assocM n tripulanteNuevo mapT
                        newMapS = assocM si sectorNuevo mapS
                        heapSinT = heapSin mht tripulanteNuevo 
                        newHeap = insertH tripulanteNuevo heapSinT
                        in N newMapS newMapT newHeap 
-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen.
-- Eficiencia: O(log S + log T + T log T)

heapSin :: MaxHeap Tripulante -> Tripulante -> MaxHeap Tripulante
heapSin heap t = if isEmptyH heap
                    then emptyH
                else if nombre (maxH heap) == nombre t 
                    then deleteMaxH heap 
                else insertH (maxH heap) (heapSin (deleteMaxH heap) t)

-----------------------------------------------------------------------------
sectores :: Nave -> Set SectorId
sectores nave = sectoresNoVaciosDe (tripulantesN nave) nave
-- Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
-- Eficiencia : O( T ∗ (N log N + log T)) siendo T la cantidad de tripulantes
-- en la nave y N el tamaño del set

sectoresNoVaciosDe :: [Tripulantes] -> Nave -> Set SectorId
sectoresNoVaciosDe [] _ = emptyS 
sectoresNoVaciosDe (t:ts) nave = unionS (sectoresAsignados (nombre t) nave) 
                                        (sectoresNoVaciosDe ts nave)
-- Eficiencia : O(T * (N log N + log M)) siendo T la cantidad de tripulantes
-- en la lista, N la cantidad de elementos del set y M la cantidad de tripulantes
-- en la nave

sinSectoresAsignados :: Nave -> [Tripulante]
sinSectoresAsignados nave = tripulantesSinSectores (tripulantesN nave) nave
-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
-- eficiencia : (T log T + T) donde T es la cantidad de tripulantes de la nave 

tripulantesSinSectores :: [Tripulante] -> Nave -> [Tripulante]
tripulantesSinSectores [] _ = []
tripulantesSinSectores (t:ts) nave = if size (sectoresT t) == 0
                                        then t : tripulantesSinSectores ts nave
                                     else tripulantesSinSectores ts nave
-- eficiencia : O (T) donde T es la cantidad de tripulantes en la lista

barriles :: Nave -> [Barril]
barriles nave = barrilesDe (SetToList (sectores nave)) nave
-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.

barrilesDe :: [SectorId] -> Nave -> [Barril]
barrilesDe [] _ = []
barrilesDe (sec:secs) nave = 
    let sector = datosDelSector sec nave 
        componentes = second sector
        in (barrilDe componentes) : barrilesDe secs nave 
-- Coste O(S * (log S + log C)) donde S es la cantidad de Sectores y C la cantidad de
-- componentes de dicho sector

second :: (Set Nombre, [Componente]) -> [Componente]
second (_ , componentes) = componentes
-- Coste O(1) 

barrilDe :: [Componente] -> [Barril]
barrilDe [] = []
barrilDe (comp:comps) = if esAlmacen comp 
                        then (barril comp) : barrilDe comps
                        else barrilDe comps
-- Coste O(C) siendo C la cantidad de componentes

esAlmacen :: Componente -> Bool 
esAlmacen (Almacen _) = True
esAlmacen _ = False
-- Coste O(1)

barril :: Componente -> [Barril]
barril (Almacen barriles) = barriles
barril _ = []
-- Coste O(1)