programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun p1 p2 org = if programaronJuntas org p1 p2
                                then intersection (programasDe org p1) (programasDe org p2)
                             else emptyS
-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en 
-- las que las personas programaron juntas
-- Costo O(N log N + C log C + log P) donde N son la cantidad de programas del primer programador
-- C la cantidad total de programas y P la cantidad de personas del organizador
--  * intersection : O(N log N)
--  * programaronJuntas : (log P + C log C)
--  * programasDe : O(log P)

esUnGranHacker :: Organizador -> Persona -> Bool
esUnGranHacker org per = esAutorDeTodosLos per org (todosLosProgramas org)
-- Propósito: denota verdadero si la persona indicada aparece como autor de todos los 
-- programas del organizador
-- Costo : O (C log P) donde C es la cantidad de programas y P la cantidad de personas 
--  *todosLosProgramas : O(C)
--  *esAutorDeTodosLos : O(C * (log N + log P))

esAutorDeTodosLos :: Persona -> Organizador -> [Checksum] -> Bool 
esAutorDeTodosLos _ _ [] = True
esAutorDeTodosLos p org (c:cs) = belongs c (programasDe org p) &&
                                 esAutorDeTodosLos p org cs
-- Costo : O(C * (log N + log P)) donde C es la cantidad de checksum, N el tamaño del set
-- y P la cantidad de programas de la persona 
--  * belongs : O(log N)
--  * programasDe : O(log P) 

---------------------------------------------------------------------------------------------

-- data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
-- INV 
--    * toda persona en el Map de checksum tiene que estar en el map persona y vicebersa
--    * el codigo checksum debe ser mayor o igual a 1
--    * no debe existir ningun checksum o persona que no este referenciado en el otro
--    * no hay conjuntos vacios, ningun programa o persona debe estar asociada a un cojunto vacio
--    * los sets internos de cada map no deben tener duplicados 

nuevo :: Organizador
nuevo = MkO emptyM emptyM
-- Propósito: Un organizador vacío.
-- Eficiencia: O(1)

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO mapC mapP) c set = 
   case lookupM c mapC of 
      Nothing -> 
         if isEmptyS set 
            then error "el set de desarrolladores es vacio"
         else MkO (assocM c set mapC) (actualizarPersonas c (set2List set) mapP)
      Just v -> error "el idenficador del programa ya fue usado previamente"
-- Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el 
-- conjunto de personas autores de dicho programa.
-- Precondición: el identificador del programa que se agrega no fue usado previamente en el 
-- organizador, y el Set de personas no está vacío.
-- Eficiencia: no hay ninguna garantía de eficiencia.

actualizarPersonas :: CheckSum -> [Persona] -> Map Persona (Set Checksum) -> Map Persona (Set Checksum) 
actualizarPersonas _ [] mapP = mapP
actualizarPersonas c (p:ps) mapP =
   case lookupM p mapP of
      Nothing -> assocM p (addS c emptyS) (actualizarPersonas c ps mapP) 
      Just v -> assocM p (addS c (lookupM v mapP)) (actualizarPersonas c ps mapP)
-- Costo : O (P (Log M + Log N)) siendo P la cantidad de personas de la lista, M la cantidad
-- de personas en el map y N la cantidad de Checksums en la lista de la persona
--    * lookupM : Log M
--    * assocM : Log M
--    * addS : Log N 

todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MkO mapC mapP) = domM mapC
-- Propósito: denota una lista con todos y cada uno de los códigos identificadores de
-- programas del organizador.
-- Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.

autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MkO mapC _) c = 
   case lookupM c mapC of 
      Nothing -> error "no hay un programa con ese id"
      Just ps -> ps
-- Propósito: denota el conjunto de autores que aparecen en un programa determinado.
-- Precondición: el Checksum debe corresponder a un programa del organizador.
-- Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.

programasDe :: Organizador -> Persona -> Set Checksum
programasDe (MkO _ mapP) p = 
   case lookupM p mapP of 
      Nothing -> error "no existe esa persona"
      Just v -> v
-- Propósito: denota el conjunto de programas en los que participó una determinada persona.
-- Precondición: la persona debe existir en el organizador.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas org p1 p2 = 
   not (isEmptyS (intersection (programasDe org p1) (programasDe org p2))) 
-- Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de 
-- algún software en común.
-- Precondición: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas 
-- que aparecen en todos los programas del organizador, y C la cantidad total de programas.

nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona (MkO _ mapP) p = 
   case lookupM p mapP of 
      Nothing -> error "no existe esa persona"
      Just v -> sizeS v
-- Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en
-- los que aparece.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador

----------------------------------------------------------------------------------------------

data Organizador = MkO (Map Checksum (Set Persona))
                         (Map Persona (Set Checksum))
                         (Maybe (Checksum, Int))
-- Inv 
--    * toda persona en el Map de checksum tiene que estar en el map persona y vicebersa
--    * el codigo checksum debe ser mayor o igual a 1
--    * no debe existir ningun checksum o persona que no este referenciado en el otro
--    * no hay conjuntos vacios, ningun programa o persona debe estar asociada a un cojunto vacio
--    * los sets internos de cada map no deben tener duplicados 
--    * el maybe debe coincideir con algun programa del map Checksum que tenga mas personas en
--       su set

elMayorPrograma :: Organizador -> Maybe Checksum
elMayorPrograma (MkO _ _ Nothing) = Nothing
elMayorPrograma (MkO _ _ maybe) = fst maybe
-- Propósito: recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota
-- Nothing si no puede devolver un programa.
-- Eficiencia: O(1) en peor caso.

fst :: (Checksum, Int) -> Checksum
fst (c , _) = c

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO mapC mapP maybeMax) c set =
   case lookupM c mapC of
      Just _  -> error "El identificador del programa ya fue usado previamente"
      Nothing ->
         if isEmptyS set
            then error "El conjunto de desarrolladores es vacío"
            else
               let nuevosMapC = assocM c set mapC
                   nuevosMapP = actualizarPersonas c (setToList set) mapP
                   nuevoMax   = actualizarMax maybeMax c (sizeS set)
               in MkO nuevosMapC nuevosMapP nuevoMax
-- Propósito: Agrega un programa con el Checksum indicado y su conjunto de autores.
-- Precondición: El identificador no fue usado previamente y el conjunto de personas no está vacío.
-- Eficiencia: O(P * log P + log C), donde P es la cantidad de autores y C la de programas.


actualizarPersonas :: CheckSum -> [Persona] -> Map Persona (Set Checksum) -> Map Persona (Set Checksum) 
actualizarPersonas _ [] mapP = mapP
actualizarPersonas c (p:ps) mapP =
   case lookupM p mapP of
      Nothing -> assocM p (addS c emptyS) (actualizarPersonas c ps mapP) 
      Just v -> assocM p (addS c (lookupM v mapP)) (actualizarPersonas c ps mapP)
-- Costo : O (P (Log M + Log N)) siendo P la cantidad de personas de la lista, M la cantidad
-- de personas en el map y N la cantidad de Checksums en la lista de la persona
--    * lookupM : Log M
--    * assocM : Log M
--    * addS : Log N 

actualizarPersonas :: Maybe (Checksum, Int) -> Map Checksum (Set Persona) -> Maybe (Checksum, Int)
actualizarPersonas maybe (Map c set) = if (second maybe) > sizeS set
                                          then maybe
                                       else Maybe (c , (sizeS set))