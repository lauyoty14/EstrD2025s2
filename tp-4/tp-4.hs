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