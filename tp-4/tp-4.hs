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

data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre
    | Bifurcacion Cofre Mapa Mapa

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnElCofre c
hayTesoro (Bifurcacion c mapaI mapaD) = hayTesoroEnElCofre c || hayTesoro mapaI 
                                      || hayTesoro mapaD

hayTesoroEnElCofre :: Cofre -> Bool
hayTesoroEnElCofre (Cofre (ob:obs)) = (objetoEsIgualA ob Tesoro) || 
                                      (hayTesoroEnElCofre (Cofre obs))
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

-- Ejemplos de objetos
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
mapaGrande = Bifurcacion cofreMixto
                (Bifurcacion cofreConChatarra (Fin cofreVacio) (Fin cofreConTesoro))
                (Fin cofreConChatarra)