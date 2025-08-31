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
ponerN 0 _ celda = celda
ponerN n c celda = ponerN (n-1) c (poner c celda)

-------------------------------------------------------------------------------------------------------
-- 1.2
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada camino) = hayTesoro camino
hayTesoro (Cofre objetos camino) = if hayTesoroEnElCofre objetos 
                                    then True
                                    else hayTesoro camino

hayTesoroEnElCofre :: [Objeto] -> Bool
hayTesoroEnElCofre [] = False
hayTesoroEnElCofre (obj:objetos) = if esElObjeto Tesoro obj 
                                    then True
                                    else hayTesoroEnElCofre objetos

esElObjeto :: Objeto -> Objeto -> Bool
esElObjeto Cacharro Cacharro = True
esElObjeto Tesoro Tesoro = True
esElObjeto _ _ = False
-------------------------------------------------------------------------------------------------------

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Nada camino) = 1 + pasosHastaTesoro camino
pasosHastaTesoro (Cofre objetos camino) = if hayTesoroEnElCofre objetos 
                                          then 0 
                                          else 1 + pasosHastaTesoro camino

-------------------------------------------------------------------------------------------------------

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False
hayTesoroEn 0 (Cofre objetos _) = hayTesoroEnElCofre objetos
hayTesoroEn 0 (Nada _) = False
hayTesoroEn n (Cofre _ camino) = hayTesoroEn (n-1) camino
hayTesoroEn n (Nada camino) = hayTesoroEn (n-1) camino

-------------------------------------------------------------------------------------------------------

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _ = True
alMenosNTesoros _ Fin = False
alMenosNTesoros n (Cofre objetos camino) = if hayTesoroEnElCofre objetos 
                                            then alMenosNTesoros (n- cantidadDeTesorosEnElCofre objetos) camino
                                            else alMenosNTesoros n camino
alMenosNTesoros n (Nada camino) = alMenosNTesoros n camino

cantidadDeTesorosEnElCofre :: [Objeto] -> Int
cantidadDeTesorosEnElCofre [] = 0  
cantidadDeTesorosEnElCofre (obj:objetos) = if esElObjeto Tesoro obj 
                                            then 1 + cantidadDeTesorosEnElCofre objetos
                                            else cantidadDeTesorosEnElCofre objetos

-------------------------------------------------------------------------------------------------------
-- Precondicion n <= m
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre _ _ Fin = 0 
cantTesorosEntre 0 m camino = cantidadDeTesorosEnElCofreDelCamino m camino
cantTesorosEntre n m (Nada camino) = if n > 0 
                                      then cantTesorosEntre (n-1) (m-1) camino
                                      else cantTesorosEntre n (m-1) camino
cantTesorosEntre n m (Cofre objetos camino) = if n > 0 
                                              then cantTesorosEntre (n-1) (m-1) camino
                                              else cantidadDeTesorosEnElCofre objetos + cantTesorosEntre n (m-1) camino

cantidadDeTesorosEnElCofreDelCamino :: Int -> Camino -> Int
cantidadDeTesorosEnElCofreDelCamino 0 (Cofre objetos _) = cantidadDeTesorosEnElCofre objetos
cantidadDeTesorosEnElCofreDelCamino 0 (Nada _) = 0
cantidadDeTesorosEnElCofreDelCamino n (Cofre objetos camino) = cantidadDeTesorosEnElCofre objetos + 
                                                               cantidadDeTesorosEnElCofreDelCamino (n-1) camino
cantidadDeTesorosEnElCofreDelCamino n (Nada camino) = cantidadDeTesorosEnElCofreDelCamino (n-1) camino

camino1 = Nada (Cofre [Cacharro] (Nada (Cofre [Tesoro] Fin)))
camino2 = Cofre [Tesoro] Fin
camino3 = Nada (Cofre [Tesoro,Tesoro] Fin)
camino4 = Nada (Cofre [Tesoro] (Nada (Cofre [Tesoro] Fin)))