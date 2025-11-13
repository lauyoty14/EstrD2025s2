#include "set.h"
#include "linkedList.h"

Set emptyS(){
    Set s = new SetSt;
    s->cantidad = 0;
    s->primero = NULL;
    
    return s;
}
//Crea un conjunto vacío.
// Costo: O(1)

bool isEmptyS(Set s){
    return (s->cantidad == 0);
}
//Indica si el conjunto está vacío.
// Costo: O(1)

bool belongsS(int x, Set s){
    NodoS* actual = s->primero;

    while (actual != NULL) {
        if (actual->elem == x) {
            return true;
        }
        actual = actual->siguiente;
    }

    return false;
}
//Indica si el elemento pertenece al conjunto.
// Costo: O(n)

void AddS(int x, Set s){
    if (!belongsS(x, s)){
        NodoS* nuevoNodo = new NodoS;
        nuevoNodo->elem = x;
        nuevoNodo->siguiente = s->primero;
        s->primero = nuevoNodo;
        s->cantidad++;
    }
}
//Agrega un elemento al conjunto.
// Costo: O(n)

void RemoveS(int x, Set s){
    if (belongsS(x, s)){
        NodoS* actual = s->primero;
        NodoS* anterior = NULL;

        while (actual != NULL && actual->elem != x){
            anterior = actual;
            actual = actual->siguiente;
        }

        if (actual != NULL) {
            if (anterior == NULL){
                s->primero = actual->siguiente;
            }
            else{
                anterior->siguiente = actual->siguiente;
            }
            delete actual;
            s->cantidad--;
        }
    }
}
//Quita un elemento dado.
// Costo: O(n)

int sizeS(Set s){
    return s->cantidad;
}
//Devuelve la cantidad de elementos.
// Costo: O(1)

LinkedList setToList(Set s){
    LinkedList lista = new LinkedListSt;
    lista->primero = NULL;
    lista->cantidad = 0;
    lista->ultimo = NULL;

    NodoS* actual = s->primero;

    while (actual != NULL) {
        NodoL* nuevoNodo = new NodoL;
        nuevoNodo->elem = actual->elem;
        nuevoNodo->siguiente = NULL;

        if (lista->primero == NULL) {
            lista->primero = nuevoNodo;
            lista->ultimo = nuevoNodo;
        } else {
            lista->ultimo->siguiente = nuevoNodo;
            lista->ultimo = nuevoNodo;
        }

        lista->cantidad++;
        actual = actual->siguiente;
    }

    return lista;
}
//Devuelve una lista con los lementos del conjunto.
// Costo: O(n)

void DestroyS(Set s){
    NodoS* actual = s->primero;
    NodoS* siguienteNodo;

    while (actual != NULL) {
        siguienteNodo = actual->siguiente;
        delete actual;
        actual = siguienteNodo;
    }

    delete s;
}
//Libera la memoria ocupada por el conjunto.
// Costo: O(n)
