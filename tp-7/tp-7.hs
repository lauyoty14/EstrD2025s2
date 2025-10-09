import Control.Applicative (Const)
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

esBST :: Tree a -> Bool
esBST EmptyT = False
esBST (NodeT x ti td) = esBST ti && esBST td && 
                        esMayorATodosLosDe x ti && esMenorATodosLosDe x td
-- Propósito: indica si el árbol cumple con los invariantes de BST.
-- Costo: O(N2)

esMayorATodosLosDe :: a -> Tree a -> Bool
esMayorATodosLosDe _ EmptyT = True
esMayorATodosLosDe x (NodeT y ti td) = x > y && esMayorATodosLosDe x ti && esMayorATodosLosDe x td

esMenorATodosLosDe :: a -> Tree a -> Bool 
esMenorATodosLosDe _ EmptyT = True
esMenorATodosLosDe x (NodeT y ti td) = x < y && esMenorATodosLosDe x ti && esMenorATodosLosDe x td

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA x (NodeT y ti td) = if (x > y)
                                   then case (elMaximoMenorA x td) of
                                    Nothing -> Just y
                                    Just v -> Just v
                                   else elMaximoMenorA x ti
-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
-- elemento dado.
-- Costo: O(log N)

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA _ EmptyT = Nothing 
elMinimoMayorA x (NodeT y ti td) = if (x < y)
                                   then case (elMinimoMayorA x ti) of
                                    Nothing -> Just y
                                    Just v -> Just v
                                   else elMinimoMayorA x td
-- Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
-- elemento dado.
-- Costo: O(log N)

balanceado :: Tree a -> Bool
balanceado EmptyT = False
balanceado (NodeT x ti td) = abs(height ti - height td) <= 1 && balanceado ti && balanceado td
-- Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
-- nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
-- Costo: O(N2)

----------------------------------------------------------------------------------------------------------------------------------------
-- ejercicio 3

data Map k v = Map [(k, v)]

emptyM :: Map k v
--Costo: O(1). ya que solo devuelve un mapa vacio
assocM :: Ord k => k -> v -> Map k v -> Map k v
-- Costo: O(k). ya que dada la clave que nos dan recorre todas las claves del map para buscar si esta ya existe en el map
lookupM :: Ord k => k -> Map k v -> Maybe v
-- Costo: O(K). ya que recorre todas las claves del map para buscar la clave dada y devolverlo
deleteM :: Ord k => k -> Map k v -> Map k v
-- Costo: O(K). ya que recorre todas las claves del map para encontrar la dada y sacar el valor de dicha clave
keys :: Map k v -> [k]
-- Costo: O(K). ya que recorre y devuelve todas las claves del map

----------------------------------------------------------------------------------------------------------------------------------------
-- ejercicio 4
type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)
-- inv : 
--  * todos los empleados del primer map deben tener asigando como clave un numero de cuil en el segundo map
--  * no hay un empleado sin ningun sector asigando
--  * SectorId es un numero mayor o igual a 1
--  * Cuil es un Cuil valido


-- empleado 

consEmpleado :: CUIL -> Empleado
-- Propósito: construye un empleado con dicho CUIL.
-- Costo: O(1) 
cuil :: Empleado -> CUIL
-- Propósito: indica el CUIL de un empleado.
-- Costo: O(1)
incorporarSector :: SectorId -> Empleado -> Empleado
-- Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
-- Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
sectores :: Empleado -> [SectorId]
-- Propósito: indica los sectores en los que el empleado trabaja.
-- Costo: O(S)

-- empresa

consEmpresa :: Empresa
consEmpresa = ConsE emptyM emptyM 
-- Propósito: construye una empresa vacía.
-- Costo: O(1)

buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE _ mapE) = case (lookupM c mapE) of 
                                  Nothing -> error "el empleado con el cuil dado no esta en la empresa"
                                  Just v -> v
-- Propósito: devuelve el empleado con dicho CUIL.
-- Precondición: el CUIL es de un empleado de la empresa.
-- Costo: O(log E) donde E es la cantidad de claves de empleado de la empresa, es coste logaritmo ya que utiliza la funcion lookupM 
-- de ese mismo coste 

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
empleadosDelSector sectorId (ConsE mapS _) = case (lookupM sectorId mapS) of 
                                  Nothing -> error "el empleado con el cuil dado no esta en la empresa"
                                  Just v -> v
-- Propósito: indica los empleados que trabajan en un sector dado.
-- Costo: O(log S + E) donde es log S por que buscamos el sectorId en el map y es E ya que devolvemos la lista de empresas de ese sector

todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE _ mapE) = keys mapE
-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E) es E donde E es la lista de empleados ya que utilizamos la funcion keys que recorre la lista de claves de el mapa de empleados

todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE mapS _) = keys mapS
-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S) es S donde S es la lista de sectoresId de la empresa, ya que utilizamos la funcion keys que recorre la lista de claves del map

agregarSector :: SectorId -> Empresa -> Empresa
agregarSector sectorId (ConsE mapS mapE) = ConsE (assocM sectorId emptyS) mapE
-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(log S) es log de S donde S es la cantidad de claves de los sectores en la empresa, ya que utilizamos la funcion assocM para 
-- añadir el nuevo sector

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado sectoresIds cuil (ConsE mapS mapE) = 
  let empleado = incorporarSectores sectoresIds (consEmpleado cuil)
      nuevosSectores = añadirEmpleadoALosSectoresEn empleado sectoresIds mapS
      nuevoMapE = assocM cuil empleado mapE
  in ConsE nuevosSectores nuevoMapE
-- Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el
-- CUIL dado.
-- Costo: O(S (log S + log K + log N) + log E)
--  * incorporarSectores : O(log S)
--  * consEmpleado : O(1)
--  * añadirEmpleadoALosSectoresEn : O(S (log S + log N)
--  * assocM : O(log E)

añadirEmpleadoALosSectoresEn :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
añadirEmpleadoALosSectoresEn _ [] mapS = mapS
añadirEmpleadoALosSectoresEn emp (si:sis) mapS = 
  let setEmpleado = case (lookupM si mapS) of
                        Nothing -> emptyS
                        Just s -> s
      nuevoSet = addS emp setEmpleado
      nuevoMapS = assocM si nuevoSet mapS
  in añadirEmpleadoALosSectoresEn emp sis nuevoMapS
-- Costo : O(S (log S + log N)
--  * lookupM : O(log S) donde S es la cantidad de sectores en el map
--  * addS : O(log N) donde N es la cantidad de empleados de ese sector
--  * assocM : O(log S)

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector si cuil (ConsE mapS mapE) = ConsE mapS (assocM cuil (incorporarSector si (buscarPorCUIL cuil (ConsE mapS mapE))) mapE)
-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Costo: (log S + log E)
--  * assocM : O(log E) donde E es la cantidad de empleados en el map de la empresa
--  * incorporarSector : O(log S) donde S es la cantidad de sectores en el map de la empresa
--  * buscarPorCuil : O(log E) donde E es la cantidad de empleados en el map de la empresa

borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado cuil (ConsE mapS mapE) = ConsE (sacarEmpleadoDeLosSectoresEn cuil (keys mapS) mapS) (deleteM cuil mapE)
-- Propósito: elimina al empleado que posee dicho CUIL.
-- Costo: O(S + S*(log S + (E log E)) + log E) donde E es la cantidad de empleados del map

sacarEmpleadoDeLosSectoresEn :: CUIL -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
sacarEmpleadoDeLosSectoresEn _ [] mapS = mapS
sacarEmpleadoDeLosSectoresEn cuil (si:sis) mapS = 
  let setEmpleado = case (lookupM si mapS) of
                        Nothing -> emptyS
                        Just s -> s
      nuevoSet = removerEmpleado cuil setEmpleado
      nuevoMapS = assocM si nuevoSet mapS
  in sacarEmpleadoDeLosSectoresEn cuil sis nuevoMapS
-- Costo : O(S (log S + N log N))
--  * lookupM : O(log S) donde S es la cantidad de sectores en el map
--  * removerEmpleado : O(E) donde E es la cantidad de empleados del set
--  * assocM : O(log S)

removerEmpleado :: CUIL -> Set Empleado -> Set Empleado
removerEmpleado cuil setE = listToSet (filtrarEmpleados cuil (setToList setE))
-- costo O(N log N)
--  * listToSet : O(N log N)
--  * filtrarEmpleados : O(N)
--  * setToList : O(N) 

listToSet :: [Empleado] -> Set Empleado
listToSet []     = emptyS
listToSet (e:es) = addS e (listToSet es)
-- costo O(N log N) donde N es la cantidad de empleados en la lista

filtrarEmpleados :: CUIL -> [Empleado] -> [Empleado]
filtrarEmpleados cuil [] = []
filtrarEmpleados cuilBuscado (emp:emps) = if cuilBuscado == (cuil emp)
                                   then filtrarEmpleados cuilBuscado emps
                                   else emp : filtrarEmpleados cuilBuscado emps
-- costo O(N) donde N es la cantidad de empleados en la lista