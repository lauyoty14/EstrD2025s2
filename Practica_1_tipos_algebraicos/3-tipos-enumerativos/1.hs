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


