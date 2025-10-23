#include <iostream>
#include "producto.cpp"
using namespace std;

int main() {
    Producto p1 = crearProducto("Laptop", true);
    Producto p2 = crearProducto("Mouse", false);

    mostrarProducto(p1);
    mostrarProducto(p2);

    cout << "El producto " << obtenerNombre(p1) << (tieneStock(p1) ? " tiene stock." : " no tiene stock.") << endl;
    cout << "El producto " << obtenerNombre(p2) << (tieneStock(p2) ? " tiene stock." : " no tiene stock.") << endl;

    return 0;
}

