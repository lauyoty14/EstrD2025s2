/* 
Diseñar e implementar un tad
para representar un producto de un inventario, 
junto con funpciones básicas para manejarlo.

Estructura del Dato:
Un Producto estará compuesto por:
- nombre (cadena de caracteres, AKA String)
- enStock (un valor booleano: true si hay stock, false si no lo hay)

Funciones a Implementar:

crearP,roducto(string nombre, bool enStock):
Propósito: Crea un nuevo producto.
Retorna: Un Producto con los datos proporcionados.

otenerNombre(Producto p):
Propósito: Devuelve el nombre del producto.
Retorna: Una cadena de caracteres.

tieneStock(Producto p):
Propósito: Indica si el producto tiene stock.
Retorna: true si hay stock, false en caso contrario.
                                                                          
mostrarProducto(Producto p):                                                       
Propósito: Muestra la información de un producto por consola.
*/
#include <iostream>
#include "producto.h"
using namespace std;

Producto crearProducto(string nombre, bool enStock){
    Producto p;
    p.nombre = nombre;
    p.enStock = enStock;
    return p;
}
//Propósito: Crea un nuevo producto.
//Retorna: Un Producto con los datos proporcionados.

string obtenerNombre(Producto p){
    return p.nombre;
}
//Propósito: Devuelve el nombre del producto.
//Retorna: Una cadena de caracteres.

bool tieneStock(Producto p){
    return p.enStock;
}
//Propósito: Indica si el producto tiene stock.
//Retorna: true si hay stock, false en caso contrario.

void mostrarProducto(Producto p){
    cout << "Nombre: " << p.nombre << ", En Stock: " << (p.enStock ? "true" : "false") << endl;
}                      
//Propósito: Muestra la información de un producto por consola.

