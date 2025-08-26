{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use max" #-}
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores [] = []

yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien _ _ = False

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = yTambien x (conjuncion xs)

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ b = b

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = oBien x (disyuncion xs)

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = oBien (e == x) (pertenece e xs)

unoSi :: Bool -> Int
unoSi False = 0
unoSi _ = 1

apariciones :: Eq a => a -> [a] -> Int
apariciones a [] = 0
apariciones a (x:xs) = unoSi (a == x) + apariciones a xs

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x:xs) = if x < n 
                       then x : losMenoresA n xs 
                       else losMenoresA n xs

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (xs:xss) = if longitud xs > n 
                                then xs : lasDeLongitudMayorA n xss 
                                else lasDeLongitudMayorA n xss

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar xs [] = xs
agregar xs ys = xs ++ ys 

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y then x else y

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos (x:xs) (y:ys) = maxDelPar (x,y) : zipMaximos xs ys
