{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use :" #-}
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