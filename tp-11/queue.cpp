#include "queue.h"

Queue emptyQ(){
    Queue q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;

    return q;
}
//Crea una cola vacía.
//Costo: O(1).

bool isEmptyQ(Queue q){
    return q->cantidad == 0;
}
//Indica si la cola está vacía.
//Costo: O(1).

int firstQ(Queue q){
    return q->primero->elem;
}
//Devuelve el primer elemento.
// PRECOND: la cola no está vacía.
//Costo: O(1).

void Enqueue(int x, Queue q){
    NodoQ* n = new NodoQ;
    n->elem = x;
    n->siguiente = NULL;

    if (q->ultimo != NULL){
        q->ultimo->siguiente = n;
    } else {
        q->primero = n;
    }
    q->ultimo = n;
    q->cantidad++;
}
//Agrega un elemento al final de la cola.
// PRECOND: la cola no está vacía.
//Costo: O(1).

void Dequeue(Queue q){
    NodoQ* n = q->primero;
    q->primero = q->primero->siguiente;
    q->cantidad--;

    if( q->primero == NULL ){
        q->ultimo = NULL;
    }

    delete n;
}
//Quita el primer elemento de la cola.
//Costo: O(1).

int lengthQ(Queue q){
    return q->cantidad;
}
//Devuelve la cantidad de elementos de la cola.
//Costo: O(1).

void MergeQ(Queue q1, Queue q2){
    if (q2->primero == NULL) { return; }

    if (q1->primero == NULL) {
        q1->primero = q2->primero;
        q1->ultimo = q2->ultimo;
    } else {
        q1->ultimo->siguiente = q2->primero;
        q1->ultimo = q2->ultimo;
    }
    q1->cantidad += q2->cantidad;

    // Liberar la memoria de la estructura de la cola q2
    delete q2;
}
//Anexa q2 al final de q1, liberando la memoria inservible de q2 en el proceso.
//Nota: Si bien se libera memoria de q2, no necesariamente la de sus nodos.
//Costo: O(1).

void DestroyQ(Queue q){
    NodoQ* actual = q->primero;
    NodoQ* siguiente;

    while (actual != NULL){
        siguiente = actual->siguiente;
        delete actual;
        actual = siguiente;
    }

    delete q;
}
//Libera la memoria ocupada por la cola.
//Costo: O(n).