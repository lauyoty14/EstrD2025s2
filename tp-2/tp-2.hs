{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use camelCase" #-}
--- 1 ---
--1)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
--2)
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
--3)
sucesores :: [Int] -> [Int]
sucesores [] = []
--4)
yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien _ _ = False

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = yTambien x (conjuncion xs)
--5)
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ b = b

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = oBien x (disyuncion xs)
--6)
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss
--7)
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = oBien (e == x) (pertenece e xs)
--8)
unoSi :: Bool -> Int
unoSi False = 0
unoSi _ = 1

apariciones :: Eq a => a -> [a] -> Int
apariciones a [] = 0
apariciones a (x:xs) = unoSi (a == x) + apariciones a xs
--9)
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x:xs) = if x < n
                       then x : losMenoresA n xs
                       else losMenoresA n xs
--10)
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (xs:xss) = if longitud xs > n
                                then xs : lasDeLongitudMayorA n xss
                                else lasDeLongitudMayorA n xss
--11)
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e
--12)
agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar xs [] = xs
agregar xs ys = xs ++ ys
--13)
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]
--14)
maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y then x else y

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos (x:xs) (y:ys) = maxDelPar (x,y) : zipMaximos xs ys

--15)
minimo :: Ord a => a -> [a] -> a
minimo m [] = m
minimo m (x:xs) = if x < m then x else minimo m xs

elMinimo :: Ord a => [a] -> a
elMinimo [] = error "No hay elementos"
elMinimo (x:xs) = minimo x xs

--- 2 ---

--1)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--2)
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n > 0
                    then n : cuentaRegresiva (n-1)
                    else []

--3)
repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (n-1) e

--4)
losPrimeros :: Int -> [a] -> [a]
losPrimeros _ [] = []
losPrimeros 0 _ = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

--5)
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs = xs
sinLosPrimeros n [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

--- 3 --- 
data Persona = Persona String Int

--1)
esMayorA :: Persona -> Int -> Bool
esMayorA (Persona _ edad) n = edad > n

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (x:xs) = if esMayorA x n
                    then x : mayoresA n xs
                    else mayoresA n xs

edad :: Persona -> Int
edad (Persona _ e) = e

sumaDeEdades :: [Persona] -> Int
sumaDeEdades [] = 0
sumaDeEdades (x:xs) = edad x + sumaDeEdades xs

promedioEdad :: [Persona] -> Int
promedioEdad [x] = edad x
promedioEdad (x:xs) = div (edad x + sumaDeEdades xs) (longitud xs + 1)

elMayorEntre :: Persona -> Persona -> Persona
elMayorEntre p1 p2 = if edad p1 > edad p2 then p1 else p2

elMasViejo :: [Persona] -> Persona
elMasViejo [x] = x
elMasViejo (x:xs) = elMayorEntre x (elMasViejo xs)

--2)
data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ ps) = longitud ps

-------------------------------------------------------------------------------------------------------
tipoEsIgualATipo :: TipoDePokemon -> TipoDePokemon -> Bool
tipoEsIgualATipo Agua Agua = True
tipoEsIgualATipo Fuego Fuego = True
tipoEsIgualATipo Planta Planta = True
tipoEsIgualATipo _ _ = False

unoSiEsDelTipo :: TipoDePokemon -> Pokemon -> Int
unoSiEsDelTipo tipo (ConsPokemon t _) = if tipoEsIgualATipo tipo t then 1 else 0

cantPokemonDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDeTipo _ [] = 0
cantPokemonDeTipo t (x:xs) = unoSiEsDelTipo t x + cantPokemonDeTipo t xs

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ ps) = cantPokemonDeTipo t ps
-------------------------------------------------------------------------------------------------------

tipoSuperaATipo :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSuperaATipo Agua Fuego = True
tipoSuperaATipo Fuego Planta = True
tipoSuperaATipo Planta Agua = True
tipoSuperaATipo _ _ = False

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ []) (ConsEntrenador _ _) = 0
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ _) (ConsEntrenador _ []) = 0
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ ps) (ConsEntrenador _ ps2) = 