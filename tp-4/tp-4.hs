{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use foldr" #-}
data Pizza = Prepizza
    | Capa Ingrediente Pizza deriving (Show)
data Ingrediente = Salsa
    | Queso
    | Jamon
    | Aceitunas Int deriving (Show)

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

---------------------------------------------------------------------------------------------------------

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

---------------------------------------------------------------------------------------------------------

sacarJamon :: Pizza -> Pizza
sacarJamon (Capa Jamon p) = p
sacarJamon (Capa i p) = Capa i (sacarJamon p)

---------------------------------------------------------------------------------------------------------

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa Salsa p) = tieneSoloSalsaYQueso p
tieneSoloSalsaYQueso (Capa Queso p) = tieneSoloSalsaYQueso p
tieneSoloSalsaYQueso (Capa i p) = False

---------------------------------------------------------------------------------------------------------

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa (Aceitunas n) p) = Capa (Aceitunas (n*2)) p
duplicarAceitunas (Capa i p) = Capa i (duplicarAceitunas p)

---------------------------------------------------------------------------------------------------------

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps

---------------------------------------------------------------------------------------------------------

pizza1 = Capa Salsa (Capa Queso Prepizza)
pizza2 = Capa Salsa (Capa Queso (Capa Jamon (Capa (Aceitunas 3) Prepizza)))
pizza3 = Prepizza

data Dir = Izq | Der deriving (Show)
data Objeto = Tesoro | Chatarra deriving (Show)
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre
    | Bifurcacion Cofre Mapa Mapa

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnElCofre c
hayTesoro (Bifurcacion c mapaI mapaD) = hayTesoroEnElCofre c || hayTesoro mapaI
                                      || hayTesoro mapaD

hayTesoroEnElCofre :: Cofre -> Bool
hayTesoroEnElCofre (Cofre (ob:obs)) = objetoEsIgualA ob Tesoro ||
                                      hayTesoroEnElCofre (Cofre obs)
hayTesoroEnElCofre (Cofre []) = False


objetoEsIgualA :: Objeto -> Objeto -> Bool
objetoEsIgualA Tesoro Tesoro = True
objetoEsIgualA Chatarra Chatarra = True
objetoEsIgualA _ _ = False

--------------------------------------------------------------------------------------

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin cofre) = hayTesoroEnElCofre cofre
hayTesoroEn [] (Bifurcacion cofre _ _) = hayTesoroEnElCofre cofre
hayTesoroEn (dir:dirs) mapa = hayTesoroEn dirs (darUnPaso dir mapa)

darUnPaso :: Dir -> Mapa -> Mapa
darUnPaso Izq (Bifurcacion _ mapaI _) = mapaI
darUnPaso Der (Bifurcacion _ _ mapaD) = mapaD
darUnPaso _ _ = Fin (Cofre [])

--------------------------------------------------------------------------------------------------------

caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofre) = []
caminoAlTesoro (Bifurcacion cofre mapaI mapaD) = if hayTesoroEnElCofre cofre
                                                  then []
                                                  else if hayTesoro mapaI
                                                       then Izq : caminoAlTesoro mapaI
                                                       else Der : caminoAlTesoro mapaD

--------------------------------------------------------------------------------------------------------

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion _ mapaI mapaD) = caminoMasLargoEntre
                                                    (Izq : caminoDeLaRamaMasLarga mapaI)
                                                    (Der : caminoDeLaRamaMasLarga mapaD)

caminoMasLargoEntre :: [Dir] -> [Dir] -> [Dir]
caminoMasLargoEntre c1 c2 = if length c1 >= length c2
                            then c1
                            else c2

--------------------------------------------------------------------------------------------------------

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel mapa = tesorosPorNivelAux mapa (alturaDelMapa mapa)

tesorosPorNivelAux :: Mapa -> Int -> [[Objeto]]
tesorosPorNivelAux (Fin _) 0 = []
tesorosPorNivelAux (Fin cofre) n = [tesorosEnElCofre cofre] ++ tesorosPorNivelAux (Fin (Cofre [])) (n-1)
tesorosPorNivelAux (Bifurcacion cofre mapaI mapaD) n = [tesorosEnElCofre cofre] ++
                                                        combinarListas (tesorosPorNivelAux mapaI (n-1))
                                                        (tesorosPorNivelAux mapaD (n-1))

tesorosEnElCofre :: Cofre -> [Objeto]
tesorosEnElCofre (Cofre (ob:objs)) = if objetoEsIgualA ob Tesoro
                                   then ob : tesorosEnElCofre (Cofre objs)
                                   else tesorosEnElCofre (Cofre objs)
tesorosEnElCofre (Cofre []) = []

combinarListas :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
combinarListas [] ys = ys
combinarListas xs [] = xs
combinarListas (x:xs) (y:ys) = (x ++ y) : combinarListas xs ys


alturaDelMapa :: Mapa -> Int
alturaDelMapa (Fin _) = 1
alturaDelMapa (Bifurcacion _ mapaI mapaD) = 1 + max (alturaDelMapa mapaI) (alturaDelMapa mapaD)

--------------------------------------------------------------------------------------------------------

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _) = [[]]
todosLosCaminos (Bifurcacion _ mapaI mapaD) = agregarDireccion Izq (todosLosCaminos mapaI) ++
                                               agregarDireccion Der (todosLosCaminos mapaD)

agregarDireccion :: Dir -> [[Dir]] -> [[Dir]]
agregarDireccion _ [] = []
agregarDireccion dir (x:xs) = (dir:x) : agregarDireccion dir xs

-------------------------------------------------------------------------------------------------------

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving (Show)
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving (Show)
data Sector = S SectorId [Componente] [Tripulante] deriving (Show)
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving (Show)
data Nave = N (Tree Sector) deriving (Show)

sectores :: Nave -> [SectorId]
sectores (N tree) = sectoresDe tree

sectoresDe :: Tree Sector -> [SectorId]
sectoresDe EmptyT = []
sectoresDe (NodeT s ti td) = sectorIdDe s : sectoresDe ti ++ sectoresDe td 

sectorIdDe :: Sector -> SectorId
sectorIdDe (S id _ _) = id

-------------------------------------------------------------------------------------------------------

poderDePropulsion :: Nave -> Int
poderDePropulsion (N tree) = poderDePropulsionDe tree

poderDePropulsionDe :: Tree Sector -> Int
poderDePropulsionDe EmptyT = 0 
poderDePropulsionDe (NodeT s ti td) = poderDePropulsionDelSector s + 
                                     poderDePropulsionDe ti + 
                                     poderDePropulsionDe td

poderDePropulsionDelSector :: Sector -> Int
poderDePropulsionDelSector (S _ componentes _) = poderDePropulsionDeComponentes componentes

poderDePropulsionDeComponentes :: [Componente] -> Int
poderDePropulsionDeComponentes [] = 0
poderDePropulsionDeComponentes (c:cs) = poderDePropulsionDeComponente c + 
                                      poderDePropulsionDeComponentes cs

poderDePropulsionDeComponente :: Componente -> Int
poderDePropulsionDeComponente (Motor n) = n
poderDePropulsionDeComponente _ = 0

--------------------------------------------------------------------------------------------------------

barriles :: Nave -> [Barril]
barriles (N tree) = barrilesDe tree

barrilesDe :: Tree Sector -> [Barril]
barrilesDe EmptyT = []
barrilesDe (NodeT s ti td) = barrilesDelSector s ++ barrilesDe ti ++ barrilesDe td

barrilesDelSector :: Sector -> [Barril]
barrilesDelSector (S _ componentes _) = barrilesDeComponente componentes

barrilesDeComponente :: [Componente] -> [Barril]
barrilesDeComponente [] = []
barrilesDeComponente (c:cs) = barrilesDeComponenteAux c ++ barrilesDeComponente cs

barrilesDeComponenteAux :: Componente -> [Barril]
barrilesDeComponenteAux (Almacen barriles) = barriles
barrilesDeComponenteAux _ = []

--------------------------------------------------------------------------------------------------------

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector nuevosComponentes idSector (N tree) = N (agregarASectorEnTree nuevosComponentes idSector tree)

agregarASectorEnTree :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorEnTree _ _ EmptyT = EmptyT
agregarASectorEnTree nuevosComponentes idSector (NodeT s ti td) = if sectorIdDe s == idSector
                                                                   then NodeT (agregarComponentesASector nuevosComponentes s) ti td
                                                                   else NodeT s (agregarASectorEnTree nuevosComponentes idSector ti)
                                                                                (agregarASectorEnTree nuevosComponentes idSector td)

agregarComponentesASector :: [Componente] -> Sector -> Sector
agregarComponentesASector nuevosComponentes (S id componentes tripulantes) = S id (componentes ++ nuevosComponentes) tripulantes

--------------------------------------------------------------------------------------------------------

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA tripulante ids (N tree) = N (asignarTripulanteATree tripulante ids tree)

asignarTripulanteATree :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteATree _ _ EmptyT = EmptyT
asignarTripulanteATree tripulante ids (NodeT s ti td) = if algunIdDelSectorEs ids s
                                                        then NodeT (asignarTripulanteASector tripulante s) 
                                                                    (asignarTripulanteATree tripulante ids ti) 
                                                                    (asignarTripulanteATree tripulante ids td)
                                                        else NodeT s (asignarTripulanteATree tripulante ids ti) 
                                                                    (asignarTripulanteATree tripulante ids td)

asignarTripulanteASector :: Tripulante -> Sector -> Sector
asignarTripulanteASector tripulante (S id componentes tripulantes) = S id componentes (tripulante:tripulantes)

algunIdDelSectorEs :: [SectorId] -> Sector -> Bool
algunIdDelSectorEs (id:ids) sector = sectorIdDe sector == id || algunIdDelSectorEs ids sector
algunIdDelSectorEs [] _ = False

--------------------------------------------------------------------------------------------------------

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados tripulante (N tree) = sectoresAsignadosEnTree tripulante tree

sectoresAsignadosEnTree :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosEnTree _ EmptyT = []
sectoresAsignadosEnTree tripulante (NodeT s ti td) = if tripulanteEnSector tripulante s
                                                     then sectorIdDe s : sectoresAsignadosEnTree tripulante ti ++
                                                                      sectoresAsignadosEnTree tripulante td
                                                     else sectoresAsignadosEnTree tripulante ti ++
                                                          sectoresAsignadosEnTree tripulante td

tripulanteEnSector :: Tripulante -> Sector -> Bool
tripulanteEnSector tripulante (S _ _ tripulantes) = tripulanteEsEnLista tripulante tripulantes

tripulanteEsEnLista :: Tripulante -> [Tripulante] -> Bool
tripulanteEsEnLista tripulante (t:ts) = tripulante == t || tripulanteEsEnLista tripulante ts
tripulanteEsEnLista _ [] = False

--------------------------------------------------------------------------------------------------------

tripulantes :: Nave -> [Tripulante]
tripulantes (N tree) = tripulantesEnTree tree

tripulantesEnTree :: Tree Sector -> [Tripulante]
tripulantesEnTree EmptyT = []
tripulantesEnTree (NodeT s ti td) = tripulantesDelSector s ++ tripulantesEnTree ti ++ tripulantesEnTree td

tripulantesDelSector :: Sector -> [Tripulante]
tripulantesDelSector (S _ _ tripulantes) = tripulantes

--------------------------------------------------------------------------------------------------------

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
            | Explorador Nombre [Territorio] Lobo Lobo
            | Cría Nombre
data Manada = M Lobo

cazador1 :: Lobo
cazador1 = Cazador "hunter" ["conejo"] explorador1 explorador2 cazador2
cazador2 :: Lobo
cazador2 = Cazador "hunter2" ["zorro", "ardilla"] (Cría "cria5") (Cría "cria6") (Cría "cria7")
explorador1 :: Lobo
explorador1 = Explorador "explorer1" ["territorio1", "territorio2", "territorio3"] (Cría "cria1") (Cría "cria2")
explorador2 :: Lobo
explorador2 = Explorador "explorer2" ["territorio3", "territorio4"] (Cría "cria3") (Cría "cria4")
manada :: Manada
manada = M cazador1

buenaCaza :: Manada -> Bool
buenaCaza (M lobo) = totalDePresas lobo > totalDeCriasDe lobo

totalDePresas :: Lobo -> Int
totalDePresas (Cría _) = 0
totalDePresas (Explorador _ _ lobo1 lobo2) = totalDePresas lobo1 + totalDePresas lobo2
totalDePresas (Cazador _ presas lobo1 lobo2 lobo3) = length presas + totalDePresas lobo1 +
                                                    totalDePresas lobo2 + totalDePresas lobo3

totalDeCriasDe :: Lobo -> Int
totalDeCriasDe (Cría _) = 1
totalDeCriasDe (Explorador _ _ lobo1 lobo2) = totalDeCriasDe lobo1 + totalDeCriasDe lobo2
totalDeCriasDe (Cazador _ _ lobo1 lobo2 lobo3) = totalDeCriasDe lobo1 + totalDeCriasDe lobo2 +
                                              totalDeCriasDe lobo3

--------------------------------------------------------------------------------------------------------

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaDe lobo

elAlfaDe :: Lobo -> (Nombre, Int)
elAlfaDe (Cría nombre) = (nombre, 0)
elAlfaDe (Explorador nombre _ lobo1 lobo2) = elAlfaMayor (nombre, 0) (elAlfaMayor (elAlfaDe lobo1) (elAlfaDe lobo2))
elAlfaDe (Cazador nombre presas lobo1 lobo2 lobo3) = elAlfaMayor (elAlfaMayor (nombre, length presas) (elAlfaDe lobo1))
                                                        (elAlfaMayor (elAlfaDe lobo2) (elAlfaDe lobo3))

elAlfaMayor :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elAlfaMayor (nombre1, presas1) (nombre2, presas2) = if presas1 >= presas2
                                 then (nombre1, presas1)
                                 else (nombre2, presas2)

--------------------------------------------------------------------------------------------------------

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron territorio (M lobo) = losQueExploraronEl territorio lobo

losQueExploraronEl :: Territorio -> Lobo -> [Nombre]
losQueExploraronEl _ (Cría _) = []
losQueExploraronEl territorio (Explorador nombre territorios lobo1 lobo2) = if pertenece territorio territorios
                                    then nombre : (losQueExploraronEl territorio lobo1 ++ losQueExploraronEl territorio lobo2)
                                    else losQueExploraronEl territorio lobo1 ++ losQueExploraronEl territorio lobo2
losQueExploraronEl territorio (Cazador _ _ lobo1 lobo2 lobo3) = losQueExploraronEl territorio lobo1 ++
                                                                losQueExploraronEl territorio lobo2 ++
                                                                losQueExploraronEl territorio lobo3

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

--------------------------------------------------------------------------------------------------------

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobo) = juntar (exploradoresPorTerritorioDe lobo) []

exploradoresPorTerritorioDe :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioDe (Cría _) = []
exploradoresPorTerritorioDe (Explorador nombre territorios l1 l2) = combinarTerritorios territorios nombre
                                                                    ++ exploradoresPorTerritorioDe l1
                                                                    ++ exploradoresPorTerritorioDe l2
exploradoresPorTerritorioDe (Cazador _ _ l1 l2 l3) = exploradoresPorTerritorioDe l1
                                                    ++ exploradoresPorTerritorioDe l2
                                                    ++ exploradoresPorTerritorioDe l3

combinarTerritorios :: [Territorio] -> Nombre -> [(Territorio, [Nombre])]
combinarTerritorios [] _ = []
combinarTerritorios (t:ts) n = (t, [n]) : combinarTerritorios ts n

-- inserta cada par en la lista acumulada
juntar :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
juntar [] acumulado = acumulado
juntar ((t,ns):xs) acumulado = juntar xs (insertar t ns acumulado)

-- si el territorio ya está, agrega los nombres, sino lo crea
insertar :: Territorio -> [Nombre] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
insertar t ns [] = [(t,ns)]
insertar t ns ((t2,ns2):resto) = if t == t2
                               then (t2, ns2 ++ ns) : resto
                               else (t2, ns2) : insertar t ns resto

--------------------------------------------------------------------------------------------------------

cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
cazadoresSuperioresDe nombre (M lobo) = superiores nombre lobo

superiores :: Nombre -> Lobo -> [Nombre]
superiores _ (Cría _) = []
superiores buscado (Explorador _ _ l1 l2) = superiores buscado l1 ++ superiores buscado l2
superiores buscado (Cazador nombre _ l1 l2 l3) = if contiene buscado l1 || contiene buscado l2 || contiene buscado l3
                                                then nombre : (superiores buscado l1 ++ superiores buscado l2 ++ superiores buscado l3)
                                                else superiores buscado l1 ++ superiores buscado l2 ++ superiores buscado l3
-- dice si un lobo está en este subárbol
contiene :: Nombre -> Lobo -> Bool
contiene n (Cría nombre) = n == nombre
contiene n (Explorador nombre _ l1 l2) = n == nombre || contiene n l1 || contiene n l2
contiene n (Cazador nombre _ l1 l2 l3) = n == nombre || contiene n l1 || contiene n l2 || contiene n l3


--------------------------------------------------------------------------------------------------------




-- Ejemplos de objetos---------------------------------------------------------------------------------
objeto1 :: Objeto
objeto1 = Tesoro

objeto2 :: Objeto
objeto2 = Chatarra

-- Ejemplos de cofres
cofreVacio :: Cofre
cofreVacio = Cofre []

cofreConTesoro :: Cofre
cofreConTesoro = Cofre [Tesoro]

cofreConChatarra :: Cofre
cofreConChatarra = Cofre [Chatarra, Chatarra]

cofreMixto :: Cofre
cofreMixto = Cofre [Chatarra, Tesoro, Chatarra]

-- Ejemplos de mapas
mapaSimple :: Mapa
mapaSimple = Fin cofreConTesoro

mapaSinTesoro :: Mapa
mapaSinTesoro = Fin cofreConChatarra

mapaBifurcado :: Mapa
mapaBifurcado = Bifurcacion cofreVacio (Fin cofreConTesoro) (Fin cofreConChatarra)

mapaGrande :: Mapa
mapaGrande = Bifurcacion cofreVacio
                (Bifurcacion cofreConChatarra (Fin cofreConTesoro)
                (Bifurcacion cofreConTesoro (Fin cofreConChatarra) (Fin cofreConTesoro)))
                (Fin cofreConChatarra)

-- ejemplos de naves ------
naveVacia :: Nave
naveVacia = N EmptyT

naveConSectores :: Nave
naveConSectores = N (NodeT (S "A1" [LanzaTorpedos] ["Alice"]) 
                        (NodeT (S "B2" [Motor 5, Almacen [Comida, Oxigeno]] ["Bob", "Charlie"]) 
                            EmptyT 
                            (NodeT (S "C3" [Almacen [Torpedo, Combustible]] ["David"]) EmptyT EmptyT))
                        (NodeT (S "D4" [Motor 10] ["Eve"]) EmptyT EmptyT))

naveCompleta :: Nave
naveCompleta = N (NodeT (S "Central" [LanzaTorpedos, Motor 20] ["Captain"])
                    (NodeT (S "Engineering" [Motor 15, Almacen [Combustible, Oxigeno]] ["Engineer1", "Engineer2"])
                        (NodeT (S "Storage" [Almacen [Comida, Comida, Torpedo]] ["Loader"]) EmptyT EmptyT)
                        (NodeT (S "Crew Quarters" [Almacen [Oxigeno]] ["Crew1", "Crew2"]) EmptyT EmptyT))
                    (NodeT (S "Bridge" [LanzaTorpedos] ["Pilot"])
                        EmptyT
                        (NodeT (S "Observation" [Almacen [Comida]] ["Observer"]) EmptyT EmptyT)))

-- Ejemplos de lobos
lobo1 :: Lobo
lobo1 = Cría "Luna"

lobo2 :: Lobo
lobo2 = Cría "Niebla"

lobo3 :: Lobo
lobo3 = Explorador "Rastro" ["Bosque del Norte", "Cueva Helada"] lobo1 lobo2

lobo4 :: Lobo
lobo4 = Cría "Sombra"

lobo5 :: Lobo
lobo5 = Cazador "Colmillo" ["Liebre", "Ciervo"] lobo3 lobo4 lobo2

-- Ejemplo de manada
manada1 :: Manada
manada1 = M lobo5
