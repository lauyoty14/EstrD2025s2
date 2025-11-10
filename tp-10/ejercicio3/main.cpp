#include "arrayList.cpp"
#include <iostream>
using namespace std;

int main() {
    ArrayList a = newArrayList();

    for (int i = 1; i <= 16; i++) {
        add(i, a);
    }

    cout << "Cantidad de elementos de a: " << lengthAL(a) << endl;
    cout << "El 2do elemento de a es: " << get(1, a) << endl;

    ArrayList b = newArrayListWith(20);
    add(10, b);
    set(0, 5, b);
    
    cout << "Seteamos 5 en la lista b: " << get(0, b) << endl;

    resize(10, a);
    cout << "Cantidad de a despues del resize: " << lengthAL(a) << endl;

    add(20, a);
    cout << "El ultimo elemento de a es: " << get(lengthAL(a) - 1, a) << endl;

    remove(a);
    cout << "Despues de borrar, cantidad: " << lengthAL(a) << endl;

    ArrayList c = newArrayList();
    for (int i = 1; i <= 10; i++) add(i, c);

    cout << "Lista c (1 al 10): sumatoria = " << sumatoria(c) << endl;

    sucesores(c);
    cout << "Despues de sucesores, sumatoria = " << sumatoria(c) << endl;

    cout << "¿Pertenece 5? " << (pertenece(5, c) ? "Si" : "No") << endl;
    cout << "¿Pertenece 100? " << (pertenece(100, c) ? "Si" : "No") << endl;

    add(5, c);
    cout << "Apariciones de 5: " << apariciones(5, c) << endl;
    cout << "Minimo de a: " << minimo(c) << endl;

    ArrayList d = newArrayList();
    for (int i = 20; i < 25; i++) add(i, d);

    ArrayList e = append(c, d);
    cout << "Lista e (c + d): cantidad = " << lengthAL(e) << endl;
    cout << "Primer elemento c: " << get(0, e) << endl;
    cout << "Ultimo elemento c: " << get(lengthAL(e) - 1, e) << endl;

}
