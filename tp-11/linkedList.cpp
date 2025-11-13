#include "linkedList.h"

LinkedList nil(){
    LinkedList l = new LinkedListSt;
    l->cantidad = 0;
    l->primero = NULL; 

    return l;
}
//Crea una lista vacía.
//Costo : O(1)

bool isEmpty(LinkedList xs){
    return xs->cantidad == 0;
}
//Indica si la lista está vacía.
//Costo : O(1)

int head(LinkedList xs){
    return xs->primero->elem;
}
//Devuelve el primer elemento
//Costo : O(1)

void Cons(int x, LinkedList xs){
    NodoL* n = new NodoL;
    n->elem = x;
    n->siguiente = xs->primero;
    xs->primero = n;
    xs->cantidad++;
}
//Agrega un elemento al principio de la lista.
//Costo : O(1)

void Tail(LinkedList xs){
    NodoL* n = xs->primero;
    xs->primero = xs->primero->siguiente;
    xs->cantidad--;
    delete n;
}
//Quita el primer elemento.
//Costo : O(1)

int length(LinkedList xs){
    return xs->cantidad;
}
//Devuelve la cantidad de elementos
//Costo : O(1)

void Snoc(int x, LinkedList xs){
    NodoL* n = new NodoL;
    n->elem = x;
    n->siguiente = NULL;
    
    if (xs->primero == NULL){
        xs->primero = n;
    } else {
        NodoL* actual = xs->primero;
        while (actual->siguiente != NULL){
            actual = actual->siguiente;
        }
        actual->siguiente = n;
    }

    xs->cantidad++;
}
//Agrega un elemento al final de la lista
//Costo : O(n) siendo n la cantidad de elementos de la lista

ListIterator getIterator(LinkedList xs){
    ListIterator ixs = new IteratorSt;
    ixs->current = xs->primero;

    return ixs;
}
//Apunta el recorrido al primer elemento.
// Costo: O(1)
// PRECOND: xs no es NULL

int current(ListIterator ixs){
    return ixs->current->elem;
}
//Devuelve el elemento actual en el recorrido.
// PRECOND: el iterador no está al final (ixs->current != NULL)
// Costo: O(1)

void SetCurrent(int x, ListIterator ixs){
    ixs->current->elem = x;
}
//Reemplaza el elemento actual por otro elemento.
// PRECOND: el iterador no está al final (ixs->current != NULL)
// Costo: O(1)

void Next(ListIterator ixs){
    ixs->current = ixs->current->siguiente;
}
//Pasa al siguiente elemento.
// PRECOND: el iterador no está al final (ixs->current != NULL)
// Costo: O(1)

bool atEnd(ListIterator ixs){
    return ixs->current == NULL;
}
//Indica si el recorrido ha terminado.
// Costo: O(1)

void DisposeIterator(ListIterator ixs){
    delete ixs;
}
//Libera la memoria ocupada por el iterador.
// Costo: O(1)

void DestroyL(LinkedList xs){
    NodoL* actual = xs->primero;
    NodoL* siguiente;

    while (actual != NULL){
        siguiente = actual->siguiente;
        delete actual;
        actual = siguiente;
    }

    delete xs;
}
//Libera la memoria ocupada por la lista.
// Costo: O(n) siendo n la cantidad de elementos de la lista

void Append(LinkedList xs, LinkedList ys){
    if (ys->primero == NULL) { return;}

    if(xs->primero == NULL) {
        xs->primero = ys->primero;
        xs->ultimo = ys->ultimo;
    } else {
        xs->ultimo->siguiente = ys->primero;
        xs->ultimo = ys->ultimo;
    }

    xs->cantidad += ys->cantidad;

    delete(ys);
}
//Agrega todos los elementos de la segunda lista al final de los de la primera.
//La segunda lista se destruye.