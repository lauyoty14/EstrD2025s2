import Map 
import Heap 

type Categoria = String
type CaminoJ = [Categoria]
type Producto = String

data MVTree = MVT (Heap (Int, Producto)) (Map Categoria MVTree)
  deriving (Eq, Show)

subCategoriaDe :: CaminoJ -> MVTree -> [Categoria]
subCategoriaDe [] (MVT _ m) = keysM m
subCategoriaDe (c:cs) (MVT _ m) =
    case lookupM c m of
        Nothing -> error "camino invalido"
        Just tree -> subCategoriaDe cs tree

registrarVenta :: Producto -> CaminoJ -> MVTree -> MVTree
registrarVenta p (c:cs) (MVT h m) =
    case lookupM c m of
        Nothing -> error "camino invalido"
        Just tree -> MVT (registrarVentaH p h) (assocM c (registrarVenta p cs tree) m)
registrarVenta p [] (MVT h m) = MVT (registrarVentaH p h) m

registrarVentaH :: Producto -> Heap (Int, Producto) -> Heap (Int, Producto)
registrarVentaH p h = 
    if isEmptyH h 
      then insertH (1, p) h
      else if (snd (findMaxH h) == p)
        then insertH (fst (findMaxH h) + 1, p) (deleteMaxH h)
      else inserH (findMax h) (registrarVentaH p (deleteMaxH h))


-- Definimos algunos productos
producto1 = "Producto A"
producto2 = "Producto B"
producto3 = "Producto C"

-- Creamos un heap con productos
heapEjemplo = MVT (insertarEnHeap (1, producto1) 
                  (insertarEnHeap (2, producto2) emptyHeap))
                  emptyM

-- Creamos un árbol MVTree
arbolEjemplo = MVT heapEjemplo (assocM "Categoria1" (MVT emptyHeap emptyM) 
                                (assocM "Categoria2" (MVT emptyHeap emptyM) 
                                emptyM))

-- Árbol MVTree vacío
arbolVacio = MVT emptyHeap emptyM


