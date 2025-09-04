{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Distribution.Simple.Program.Ar (createArLibArchive)
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use max" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use min" #-}
{-# HLINT ignore "Use foldl" #-}
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
sucesores (x:xs) = (x+1) : sucesores xs
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
zipMaximos [] ys = ys
zipMaximos xs [] = xs
zipMaximos (x:xs) (y:ys) = maxDelPar (x,y) : zipMaximos xs ys

--15)
minimo :: Ord a => a -> a -> a
minimo a b = if a < b 
                  then a 
                  else b

elMinimo :: Ord a => [a] -> a
elMinimo [] = error "la lista no tiene minimo"
elMinimo (x:xs) = if null xs 
                  then x
                  else minimo x (elMinimo xs)

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
data Persona = Persona String Int deriving Show

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
tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (ConsPokemon tipo _) = tipo

tipoSuperaATipo :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSuperaATipo Agua Fuego = True
tipoSuperaATipo Fuego Planta = True
tipoSuperaATipo Planta Agua = True
tipoSuperaATipo _ _ = False

superaA :: Pokemon -> Pokemon -> Bool
superaA pk1 pk2 = tipoSuperaATipo (tipoDePokemon pk1) (tipoDePokemon pk2)

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ ps) (ConsEntrenador _ ps2) =
    cantPokemonDe_LeGananATodosLosDe_ (todosLosDe_De_ t ps) ps2

cantPokemonDe_LeGananATodosLosDe_ :: [Pokemon] -> [Pokemon] -> Int
cantPokemonDe_LeGananATodosLosDe_ [] _ = 0
cantPokemonDe_LeGananATodosLosDe_ (p:ps) ps2 = if leGanaATodosLosDe_ p ps2
                                            then longitud ps + 1
                                            else 0

leGanaATodosLosDe_ :: Pokemon -> [Pokemon] -> Bool
leGanaATodosLosDe_ p [] = True
leGanaATodosLosDe_ p (x:xs) = superaA p x && leGanaATodosLosDe_ p xs

todosLosDe_De_ :: TipoDePokemon -> [Pokemon] -> [Pokemon]
todosLosDe_De_ t [] = []
todosLosDe_De_ t (x:xs) = if tipoEsIgualATipo (tipoDePokemon x) t
                          then x : todosLosDe_De_ t xs
                          else todosLosDe_De_ t xs
-------------------------------------------------------------------------------------------------------
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = tieneAlMenosUnPokemonDeCadaTipo ps

tieneAlMenosUnPokemonDeCadaTipo :: [Pokemon] -> Bool
tieneAlMenosUnPokemonDeCadaTipo [] = False
tieneAlMenosUnPokemonDeCadaTipo ps = tieneAlMenosUnoDe Fuego ps &&
                                     tieneAlMenosUnoDe Agua ps &&
                                     tieneAlMenosUnoDe Planta ps

tieneAlMenosUnoDe :: TipoDePokemon -> [Pokemon] -> Bool
tieneAlMenosUnoDe t (p:ps) = tipoEsIgualATipo t (tipoDePokemon p) ||
                               tieneAlMenosUnoDe t ps
tieneAlMenosUnoDe _ [] = False

ash = ConsEntrenador "Ash" [ConsPokemon Fuego 1, ConsPokemon Fuego 2, ConsPokemon Planta 3]
misty = ConsEntrenador "Misty" [ConsPokemon Planta 4, ConsPokemon Planta 5, ConsPokemon Planta 6]

--------------------------------------------------------------------------------------------------------
--3)

data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa xs) = proyectosSinRepetir (proyectosDeLosRoles xs)

proyectosDeLosRoles :: [Rol] -> [Proyecto]
proyectosDeLosRoles [] = []
proyectosDeLosRoles (x:xs) = proyectosDeElRol x ++ proyectosDeLosRoles xs

proyectosDeElRol :: Rol -> [Proyecto]
proyectosDeElRol (Developer _ p) = [p]
proyectosDeElRol (Management _ p) = [p] 

proyectosSinRepetir :: [Proyecto] -> [Proyecto]
proyectosSinRepetir [] = []
proyectosSinRepetir (x:xs) = if proyectoPerteneceAlaLista x (proyectosSinRepetir xs)
                             then proyectosSinRepetir xs
                             else x : proyectosSinRepetir xs

proyectoPerteneceAlaLista :: Proyecto -> [Proyecto] -> Bool
proyectoPerteneceAlaLista _ [] = False 
proyectoPerteneceAlaLista m (p:ps) = proyecto_EsIgualA m p || proyectoPerteneceAlaLista m ps  

proyecto_EsIgualA :: Proyecto -> Proyecto -> Bool
proyecto_EsIgualA (ConsProyecto m) (ConsProyecto n) = m == n

nombreDelProyecto :: Proyecto -> String
nombreDelProyecto (ConsProyecto m) = m

--------------------------------------------------------------------------------------------------------

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = cantDevSeniorQueTrabajanEnLosProyectos rs ps

cantDevSeniorQueTrabajanEnLosProyectos :: [Rol] -> [Proyecto] -> Int
cantDevSeniorQueTrabajanEnLosProyectos [] _ = 0
cantDevSeniorQueTrabajanEnLosProyectos (r:rs) ps = 
    let condicion = esDevSenior r && trabajaEnAlgunoDeLosProyectos r ps  in 
                     unoSi condicion + cantDevSeniorQueTrabajanEnLosProyectos rs ps

esDevSenior :: Rol -> Bool
esDevSenior (Developer s _) = esSenior s
esDevSenior _ = False

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _ = False

trabajaEnAlgunoDeLosProyectos :: Rol -> [Proyecto] -> Bool
trabajaEnAlgunoDeLosProyectos (Developer _ p) ps = proyectoPerteneceAlaLista p ps
trabajaEnAlgunoDeLosProyectos (Management _ p) ps = proyectoPerteneceAlaLista p ps

--------------------------------------------------------------------------------------------------------
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] (ConsEmpresa rs) = 0
cantQueTrabajanEn ps (ConsEmpresa rs) = cantDeLosQueTrabajanEn ps rs

cantDeLosQueTrabajanEn :: [Proyecto] -> [Rol] -> Int
cantDeLosQueTrabajanEn _ [] = 0
cantDeLosQueTrabajanEn ps (r:rs) = if trabajaEnAlgunoDeLosProyectos r ps
                                   then 1 + cantDeLosQueTrabajanEn ps rs
                                   else cantDeLosQueTrabajanEn ps rs

--------------------------------------------------------------------------------------------------------

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = asignadosPorCadaProyecto rs  

asignadosPorCadaProyecto :: [Rol] -> [(Proyecto, Int)]
asignadosPorCadaProyecto [] = []
asignadosPorCadaProyecto (r:rs) = agregarRol (proyectosDeElRol r) (asignadosPorCadaProyecto rs)

agregarRol :: [Proyecto] -> [(Proyecto, Int)] -> [(Proyecto, Int)]
agregarRol [] ap = ap
agregarRol (p:ps) ap = agregarRol ps (agregarProyectoA p ap)

agregarProyectoA :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
agregarProyectoA p ((pa,i) : ps) = if proyecto_EsIgualA p pa
                                   then (p, i+1) : ps
                                   else (pa,i) : agregarProyectoA p ps
agregarProyectoA p [] = [(p,1)]

--------------------------------------------------------------------------------------------------------