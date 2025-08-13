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
