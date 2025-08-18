-- Dado un número devuelve su sucesor
sucesor :: Int -> Int
sucesor n = n + 1

--Dados dos números devuelve su suma utilizando la operación +.
sumar :: Int -> Int -> Int
sumar x y = x + y

{-
Dado dos números, devuelve un par donde la primera componente es la división del
primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
provista por Haskell.
-}
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)

--Dado un par de números devuelve el mayor de estos.
maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y then x else y

{-De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas
las funciones del punto anterior-}
expresion1 = sumar (maxDelPar (divisionYResto 10 5)) (sucesor 7)
expresion2 = maxDelPar (divisionYResto (sucesor 9) (sumar 0 1))
expresion3 = sucesor (maxDelPar (divisionYResto (sumar 3 6) 1))
expresion4 = sumar (sucesor 5) (maxDelPar (divisionYResto 8 2))

{-
Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
las siguientes funciones
-}
data Dir = Norte | Sur | Este | Oeste deriving (Show, Eq)

--a) Dada una dirección devuelve su opuesta.
opuesta :: Dir -> Dir
opuesta d = 
    case d of
        Norte -> Sur
        Sur   -> Norte
        Este  -> Oeste
        Oeste -> Este

--b) Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==
iguales :: Dir -> Dir -> Bool
iguales d1 d2 = 
    case (d1, d2) of
        (Norte, Norte) -> True
        (Sur, Sur) -> True
        (Este, Este) -> True
        (Oeste, Oeste) -> True
        _ -> False

{-
c) Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
la siguiente dirección a Oeste. ¾Posee una precondición esta función? ¾Es una función
total o parcial? ¾Por qué?
-}
siguiente :: Dir -> Dir
siguiente d = 
    case d of
        Norte -> Este
        Este  -> Sur
        Sur   -> Oeste
        Oeste -> error "No hay siguiente dirección a Oeste"
-- La función `siguiente` tiene una precondición: no se puede pasar Oeste como argumento, 
-- ya que no tiene una siguiente dirección. Es una función parcial porque no está definida para todos 
-- los valores posibles del tipo `Dir`, específicamente no está definida para Oeste.

{-
Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones:
-}
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving (Show, Eq, Ord)

{- 
a) Devuelve un par donde la primera componente es el primer día de la semana, y la
segunda componente es el último día de la semana. Considerar definir subtareas útiles
que puedan servir después.
-}
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)
primerDia :: DiaDeSemana
primerDia = Lunes
ultimoDia :: DiaDeSemana
ultimoDia = Domingo

-- b) Dado un día de la semana indica si comienza con la letra M
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

{-
c) Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).
-}
diaSiguiente :: DiaDeSemana -> DiaDeSemana
diaSiguiente Lunes = Martes
diaSiguiente Martes = Miercoles
diaSiguiente Miercoles = Jueves
diaSiguiente Jueves = Viernes
diaSiguiente Viernes = Sabado
diaSiguiente Sabado = Domingo
diaSiguiente Domingo = Lunes

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = d1 == diaSiguiente d2

--d) Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio d = d /= primerDia && d /= ultimoDia
                     
{-
Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina
las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
ya definidas en Haskell):
-}

--a) Dado un booleano, si es True devuelve False, y si es False devuelve True. En Haskell ya está 
--definida como not.
not' :: Bool -> Bool
not' False = True
not' _ = False

{-
b) Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True.
Esta función NO debe realizar doble pattern matching.
Nota: no viene implementada en Haskell
-}
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

{-
c) Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como \&\&.
-}
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

{-
d) Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como ||.
-}
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ True = True
oBien _ _ = False

{-
Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:
-}
data Persona = P String Int deriving (Show, Eq)

--Dada una persona devuelve su nombre.
nombre :: Persona -> String
nombre (P n _) = n

--Dada una persona devuelve su edad.
edad :: Persona -> Int
edad (P _ e) = e

--Aumenta en uno la edad de la persona.
crecer :: Persona -> Persona
crecer (P n e) = P n (e + 1)

--Dados un nombre y una persona, devuelve una persona con la edad de la persona y el nuevo nombre.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombre (P n e) = P nombre e

--Dadas dos personas indica si la primera es mayor que la segunda
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P n1 e1) (P n2 e2) = e1 > e2

--Dadas dos personas devuelve a la persona que sea mayor
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P n1 e1) (P n2 e2) = if e1 > e2 then P n1 e1 else P n2 e2

{-
Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego definir las
siguientes funciones:
-}
data TipoDePokemon = Agua | Fuego | Planta deriving (Show, Eq)
data Pokemon = Pokemon TipoDePokemon Int deriving (Show, Eq)
data Entrenador = Entrenador String Pokemon Pokemon deriving (Show, Eq)

--Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
superaA :: Pokemon -> Pokemon -> Bool
superaA (Pokemon t _) (Pokemon t2 _) =
    case (t, t2) of
        (Agua, Fuego) -> True
        (Fuego, Planta) -> True
        (Planta, Agua) -> True
        _ -> False

esDeTipo :: TipoDePokemon -> Pokemon -> Bool
esDeTipo t (Pokemon t2 _) = t == t2

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (Entrenador _ p1 p2) = if esDeTipo tipo p1 then 1 else 0 + if esDeTipo tipo p2 then 1 else 0

--Dado un par de entrenadores, devuelve a sus Pokémon en una lista
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (Entrenador _ p1 p2, Entrenador _ p3 p4) = [p1, p2, p3, p4]

--a)Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo :: a -> a
loMismo x = x

--b)Dado un elemento de algún tipo devuelve el número 7.
siempreSiete :: a -> Int
siempreSiete x = 7

--c)Dadas una tupla, invierte sus componentes. Por qué existen dos variables de tipo diferentes?
swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)

--2. Responda la siguiente pregunta: Por qué estas funciones son polimórficas?
--Porque pueden tomar argumentos de cualquier tipo.

{-
Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no
utilizar las funciones que ya vienen con Haskell):
-}

--a)Dada una lista de elementos, si es vacía devuelve True, sino devuelve False. Definida en 
--Haskell como null.
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia (_:_) = False

--b)Dada una lista devuelve su primer elemento.
--Definida en Haskell como head.
elPrimero :: [a] -> a
elPrimero (x:_) = x

--Dada una lista devuelve esa lista menos el primer elemento.
--Definida en Haskell como tail.
--Nota: tener en cuenta que el constructor de listas es :
sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) = xs

--Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--lista, y la segunda componente es esa lista pero sin el primero.
splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)

