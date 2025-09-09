{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use guards" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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
poner c1 (Bolita c2 celda) = Bolita c1 (Bolita c2 CeldaVacia)

sacar :: Color -> Celda -> Celda
sacar c1 CeldaVacia = CeldaVacia
sacar c1 (Bolita c2 celda) = if sonColoresIguales c1 c2
                             then CeldaVacia
                             else Bolita c2 (sacar c1 celda)

sonColoresIguales :: Color -> Color -> Bool
sonColoresIguales Azul Azul = True
sonColoresIguales Rojo Rojo = True
sonColoresIguales _ _ = False

-------------------------------------------------------------------------------------------------------

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ celda = celda
ponerN n c celda = Bolita c (ponerN (n-1) c celda)

-------------------------------------------------------------------------------------------------------
-- 1.2
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada camino) = hayTesoro camino
hayTesoro (Cofre objetos camino) = hayTesoroEnElCofre objetos || hayTesoro camino

hayTesoroEnElCofre :: [Objeto] -> Bool
hayTesoroEnElCofre [] = False
hayTesoroEnElCofre (obj:objetos) = esElObjeto Tesoro obj || hayTesoroEnElCofre objetos

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

---- 2. Tipos arbóreos ------
-- 2.1. Árboles binarios
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving (Show)

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x i d) = x + sumarT i + sumarT d

-----------------------------------------------------------------------------------------------------------------------------------------------

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ i d) = 1 + sizeT i + sizeT d

-----------------------------------------------------------------------------------------------------------------------------------------------

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x i d) = NodeT (2*x) (mapDobleT i) (mapDobleT d)

---------------------------------------------------------------------------------------------------------------------------------------------

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT = False
perteneceT x (NodeT y i d) = (x == y) || (perteneceT x i || perteneceT x d)

---------------------------------------------------------------------------------------------------------------------------------------------

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x EmptyT = 0
aparicionesT x (NodeT y i d) = if x == y
                               then 1 + aparicionesT x i + aparicionesT x d
                               else aparicionesT x i + aparicionesT x d

---------------------------------------------------------------------------------------------------------------------------------------------

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x i d) = leaves i ++ leaves d

---------------------------------------------------------------------------------------------------------------------------------------------

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ i d) = 1 + maximoEntre (heightT i) (heightT d)

maximoEntre :: Int -> Int -> Int
maximoEntre a b = if a > b
                  then a
                  else b

---------------------------------------------------------------------------------------------------------------------------------------------

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x i d) = NodeT x (mirrorT d) (mirrorT i)

---------------------------------------------------------------------------------------------------------------------------------------------

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x EmptyT EmptyT) = [x]
toList (NodeT x EmptyT d) = [x] ++ toList d
toList (NodeT x i d) = toList i ++ [x] ++ toList d

---------------------------------------------------------------------------------------------------------------------------------------------

levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN 0 (NodeT x i d) = [x]
levelN n (NodeT x i d) = levelN (n-1) i ++ levelN (n-1) d

---------------------------------------------------------------------------------------------------------------------------------------------

listPerLevel :: Tree a -> [[a]]
listPerLevel t = listPerLevelDesde 0 t

listPerLevelDesde :: Int -> Tree a -> [[a]]
listPerLevelDesde n t = if esVacio (levelN n t)
                        then []
                        else levelN n t : listPerLevelDesde (n+1) t

esVacio :: [a] -> Bool
esVacio [] = True
esVacio _  = False

---------------------------------------------------------------------------------------------------------------------------------------------

ramaMasLarga :: Tree a -> [a]
ramaMasLarga (NodeT x i d) = if heightT i > heightT d
                             then x : elementosDeLaRama i
                             else x : elementosDeLaRama d

elementosDeLaRama :: Tree a -> [a]
elementosDeLaRama EmptyT = []
elementosDeLaRama (NodeT x i d) = x : elementosDeLaRama i ++ elementosDeLaRama d

----------------------------------------------------------------------------------------------------------------------------------------------

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x i d) = [[x]] ++ antePoner x (todosLosCaminos i) ++ antePoner x (todosLosCaminos d)

antePoner :: a -> [[a]] -> [[a]]
antePoner _ [] = []
antePoner x (y:ys) = (x:y) : antePoner x ys

----------------------------------------------------------------------------------------------------------------------------------------------

data ExpA = Valor Int
            | Sum ExpA ExpA
            | Prod ExpA ExpA
            | Neg ExpA deriving (Show)

eval :: ExpA -> Int
eval (Valor x) = x
eval (Sum e1 e2) = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Neg e1) = (-1) * eval e1

simplificar :: ExpA -> ExpA
simplificar (Valor x) = Valor x
simplificar (Sum x y) = simplificarSuma (simplificar x) (simplificar y)
simplificar (Prod x y) = simplificarProd (simplificar x) (simplificar y)
simplificar (Neg x) = simplificarNeg (simplificar x)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Valor 0) y = y 
simplificarSuma x (Valor 0) = x
simplificarSuma x y = Sum x y

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) _ = Valor 0
simplificarProd _ (Valor 0) = Valor 0
simplificarProd (Valor 1) y = y
simplificarProd y (Valor 1) = y
simplificarProd x y = Prod x y

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg x) = x
simplificarNeg x = Neg x


-- ejemplos --
arbol1 :: Tree Int
arbol1 = NodeT 1 (NodeT 2 arbol2 arbol4) (NodeT 3 EmptyT arbol3)

arbol2 :: Tree Int
arbol2 = NodeT 1 (NodeT 2 arbol3 EmptyT) (NodeT 3 EmptyT EmptyT)

arbol3 :: Tree Int
arbol3 = NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)

arbol4 :: Tree Int
arbol4 = NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)
