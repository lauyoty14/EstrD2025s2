#include "arrayList.h"

ArrayList newArrayList(){
    ArrayList a = new ArrayListSt;
    a->cantidad = 0;
    a->capacidad = 16;
    a->elementos = new int[a->capacidad];
    return a;
}
//Crea una lista con 0 elementos.
//Nota: empezar el array list con capacidad 16.

ArrayList newArrayListWith(int capacidad){
    ArrayList a = new ArrayListSt;
    a->cantidad = 0;
    a->elementos = new int[a->cantidad];
    a->capacidad = capacidad;
}
//Crea una lista con 0 elementos y una capacidad dada por parámetro.

int lengthAL(ArrayList xs){
   return xs->cantidad; 
}
//Devuelve la cantidad de elementos existentes.

int get(int i, ArrayList xs){
    return xs->elementos[i];
}
//Devuelve el iésimo elemento de la lista.

void set(int i, int x, ArrayList xs){
    xs->elementos[i] = x;
}
//Reemplaza el iésimo elemento por otro dado.

void resize(int capacidad, ArrayList xs) {
    int* nuevo = new int[capacidad];
    int hasta = (xs->cantidad < capacidad) ? xs->cantidad : capacidad;

    for (int i = 0; i < hasta; i++) {
        nuevo[i] = xs->elementos[i];
    }

    delete[] xs->elementos;
    xs->elementos = nuevo;
    xs->capacidad = capacidad;

    if (xs->cantidad > capacidad) {
        xs->cantidad = capacidad;
    }
}
//Decrementa o aumenta la capacidad del array.
//Nota: en caso de decrementarla, se pierden los elementos del final de la lista.

void add(int x, ArrayList xs) {
    if (xs->cantidad == xs->capacidad) {
        resize(xs->capacidad + 1, xs);
    }
    xs->elementos[xs->cantidad] = x;
    xs->cantidad++;
}
//Agrega un elemento al final de la lista.

void remove(ArrayList xs) {
    if (xs->cantidad > 0) {
        xs->cantidad--;
    }
}
//Borra el último elemento de la lista.

int sumatoria(ArrayList xs){
    int suma = 0;
    for(int i = 0; i < xs->cantidad; i++){
        suma += xs->elementos[i];
    }
    return suma;
}
//Devuelve la suma de todos los elementos.

void sucesores(ArrayList xs){
    for(int i = 0; i < xs->cantidad; i++){
        xs->elementos[i] += 1;
    }
}
//Incrementa en uno todos los elementos.

bool pertenece(int x, ArrayList xs){
    int i = 0;
    while(i < xs->cantidad && xs->elementos[i] != x){
        i++;
    }
    return xs->elementos[i] == x;
}
//Indica si el elemento pertenece a la lista.

int apariciones(int x, ArrayList xs){
    int contador = 0;
    for (int i = 0; i < xs->cantidad; i++){
        if (xs->elementos[i] == x){
            contador += 1;
        }
    }
    return contador;
}
//Indica la cantidad de elementos iguales a x

ArrayList append(ArrayList xs, ArrayList ys){
    ArrayList n = newArrayListWith(xs->cantidad + ys->cantidad);
    n->cantidad = xs->cantidad + ys->cantidad;

    for (int i = 0; i < xs->cantidad; i++){
        n->elementos[i] = xs->elementos[i];
    }

    for (int i = 0; i < ys->cantidad; i++){
        n->elementos[xs->cantidad + i] = ys->elementos[i];
    }

    return n;
}
//Crea una nueva lista a partir de la primera y la segunda (en ese orden).

int minimo(ArrayList xs){
    int minimo = xs->elementos[0];
    for (int i = 1; i < xs->cantidad; i++){
        if (minimo > xs->elementos[i]){
            minimo = xs->elementos[i];
        }
    }
    return minimo;
}
//Devuelve el elemento más chico de la lista.
