import GHC.RTS.Flags (TickyFlags(TickyFlags))
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
opuesto :: Dir -> Dir
opuesto d = 
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
    deriving (Show)

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
numeroDia :: DiaDeSemana -> Int
numeroDia Lunes     = 1
numeroDia Martes    = 2
numeroDia Miercoles = 3
numeroDia Jueves    = 4
numeroDia Viernes   = 5
numeroDia Sabado    = 6
numeroDia Domingo   = 7

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = numeroDia d1 > numeroDia d2

--d) Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
esPrimerDia :: DiaDeSemana -> Bool
esPrimerDia Lunes = True
esPrimerDia _ = False

esUltimoDia :: DiaDeSemana -> Bool
esUltimoDia Domingo = True
esUltimoDia _ = False

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio d = not (esPrimerDia d) && not (esUltimoDia d)
                     
{-
Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina
las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
ya definidas en Haskell):
-}

--a) Dado un booleano, si es True devuelve False, y si es False devuelve True. En Haskell ya está 
--definida como not.
negar :: Bool -> Bool
negar False = True
negar _ = False

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
yTambien True b = b
yTambien _ _ = False

{-
d) Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como ||.
-}
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ b = b

{-
Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:
-}
data Persona = P String Int deriving (Show)

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
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

--Dadas dos personas devuelve a la persona que sea mayor
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2 then p1 else p2

{-
Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego definir las
siguientes funciones:
-}
data TipoDePokemon = Agua | Fuego | Planta deriving (Show)
data Pokemon = Pokemon TipoDePokemon Int deriving (Show)
data Entrenador = Entrenador String Pokemon Pokemon deriving (Show)

--Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
tipoSuperaATipo :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSuperaATipo Agua Fuego = True
tipoSuperaATipo Fuego Planta = True
tipoSuperaATipo Planta Agua = True
tipoSuperaATipo _ _ = False

tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (Pokemon tipo _) = tipo

superaA :: Pokemon -> Pokemon -> Bool
superaA pk1 pk2 = tipoSuperaATipo (tipoDePokemon pk1) (tipoDePokemon pk2)
  
tieneMismoTipoQue :: TipoDePokemon -> TipoDePokemon -> Bool
tieneMismoTipoQue Agua Agua = True
tieneMismoTipoQue Fuego Fuego = True
tieneMismoTipoQue Planta Planta = True
tieneMismoTipoQue _ _ = False

unoSiEsDelTipo :: TipoDePokemon -> Pokemon -> Int
unoSiEsDelTipo tipo (Pokemon t _) = if tieneMismoTipoQue tipo t then 1 else 0

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (Entrenador _ p1 p2) = 
    unoSiEsDelTipo tipo p1 + unoSiEsDelTipo tipo p2

--Dado un par de entrenadores, devuelve a sus Pokémon en una lista
pokemones :: Entrenador -> [Pokemon]
pokemones (Entrenador _ p1 p2) = [p1, p2]

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = pokemones e1 ++ pokemones e2

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
elPrimero [] = error "No hay elementos en la lista"

--Dada una lista devuelve esa lista menos el primer elemento.
--Definida en Haskell como tail.
--Nota: tener en cuenta que el constructor de listas es :
sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) = xs
sinElPrimero [] = error "No hay elementos en la lista"

--Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--lista, y la segunda componente es esa lista pero sin el primero.
splitHead :: [a] -> (a, [a])
splitHead [] = error "No hay elementos en la lista"
splitHead xs = (elPrimero xs, sinElPrimero xs)
