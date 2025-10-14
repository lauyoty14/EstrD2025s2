siguientesN :: Busqueda -> Int -> [( String , Int)]
siguientesN _ 0 = [] 
siguientesN busc i = 
    let res = siguiente busc 
    in case frs res of 
        Nothing -> []
        Just v -> let buscSinProd = sec res
                    in v : siguienteN buscSinProd (i-1)
-- Costo O(N * (P * log P + log A)) donde N es la cantidad de productos dada, P cantidad de productos en la busqueda y A la cantidad de atributos
--  *siguiente : O(P * log P + log A)
--  *frs : O(1)
--  *sec : O(1)

frs :: (Maybe (String, Int), Busqueda) -> Maybe (String, Int)
frs (m, _) = m

sec :: ((String, Int) , Busqueda) -> Busqueda
sec (_, y) = y

registrar :: String -> Int -> Map String Int -> Busqueda -> Busqueda
registrar nombre precio attrs (B productos filtros) =
  case lookupM nombre productos of
    Just _  -> B productos filtros  -- ya existe, no se modifica
    Nothing ->
      let attrsConPrecio = assocM "precio" precio attrs
      in if aplicaTodosLos attrsConPrecio filtros
           then B (assocM nombre attrsConPrecio productos) filtros
           else B productos filtros
-- Costo O(F * log a + log p) donde F es la cantidad de filtros, a la cantidad de atributos y p la cantidad de productos

-- verifica que todos los filtros se cumplan
aplicaTodosLos :: Map String Int -> [Filtro] -> Bool
aplicaTodosLos _ [] = True
aplicaTodosLos attrs (f:fs) = aplica f attrs && aplicaTodosLos attrs fs
-- Costo O(F * log a) donde F es la cantidad de filtros y A la cantidad de atributos

filtrar :: Filtro -> Busqueda -> Busqueda
filtrar f (B productos filtros) = B (aplicarATodosLos f (kesy productos) productos) (f:filtros)
-- Costo O(P * (log P + log A)) donde P es la cantidad de productos y A la cantidad de atributos

aplicarATodosLos :: Filtro -> [String] -> Map String (Map String Int) -> Map String (Map String Int)
aplicarATodosLos _ [] productos = productos
aplicarATodosLos f (p:ps) productos =
    case lookupM p productos of
        Nothing -> aplicarATodosLos f ps productos
        Just atbs -> 
            if aplica f atbs
                then aplicarATodosLos f ps productos
                else aplicarATodosLos f ps (deleteM p productos)
-- Costo O(P * (log P + log A)) donde P es la cantidad de productos y A la cantidad de atributos