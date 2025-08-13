{-
Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones:
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}

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

vienesDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vienesDespues d1 d2 = d1 == diaSiguiente d2

--d) Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio d = d /= primerDia && d /= ultimoDia
                     

