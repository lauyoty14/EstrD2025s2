data Color = Azul | Rojo deriving (Show)
data Celda = Bolita Color Celda | CeldaVacia deriving (Show)

-- Ejercicio 1: 
-- 1.1
nroBolitas :: Color -> Celda -> Int 
nroBolitas _ CeldaVacia = 0
nroBolitas c1 (Bolita c2 celda) = unoSiSonElMismoColor c1 c2 + nroBolitas c1 celda

unoSiSonElMismoColor :: Color -> Color -> Int
unoSiSonElMismoColor Azul Azul = 1
unoSiSonElMismoColor Rojo Rojo = 1
unoSiSonElMismoColor _ _ = 0

-------------------------------------------------------------------------------------------------------

poner :: Color -> Celda -> Celda
poner c CeldaVacia = Bolita c CeldaVacia
poner c1 (Bolita c2 celda) = Bolita c2 (Bolita c1 CeldaVacia)

sacar :: Color -> Celda -> Celda
sacar c1 CeldaVacia = CeldaVacia
sacar c1 (Bolita c2 celda) = if sonColoresIguales c1 c2 
                             then CeldaVacia
                             else sacar c1 celda

sonColoresIguales :: Color -> Color -> Bool
sonColoresIguales Azul Azul = True
sonColoresIguales Rojo Rojo = True
sonColoresIguales _ _ = False

-------------------------------------------------------------------------------------------------------

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 c celda = Bolita c celda 
ponerN n c celda = ponerN (n-1) c (Bolita c celda)

celdaRoja = ponerN 0 Rojo CeldaVacia 