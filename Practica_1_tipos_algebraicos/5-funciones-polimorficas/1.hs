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