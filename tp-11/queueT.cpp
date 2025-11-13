#include "queueT.h"

QueueT emptyQT() {
    QueueT q = new QueueTreeSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
    return q;
}

bool isEmptyQT(QueueT q) { return q->cantidad == 0; }

Tree firstQT(QueueT q) { return q->primero->elem; }

void EnqueueQT(Tree x, QueueT q) {
    NodoQT* n = new NodoQT;
    n->elem = x;
    n->siguiente = NULL;
    if (q->ultimo != NULL) q->ultimo->siguiente = n;
    else q->primero = n;
    q->ultimo = n;
    q->cantidad++;
}

void DequeueQT(QueueT q) {
    NodoQT* n = q->primero;
    q->primero = q->primero->siguiente;
    q->cantidad--;
    if (q->primero == NULL) q->ultimo = NULL;
    delete n;
}

void DestroyQT(QueueT q) {
    NodoQT* actual = q->primero;
    while (actual != NULL) {
        NodoQT* sig = actual->siguiente;
        delete actual;
        actual = sig;
    }
    delete q;
}