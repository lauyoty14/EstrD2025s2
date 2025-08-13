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