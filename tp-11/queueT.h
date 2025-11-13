#include "nodeT.cpp"

struct NodoQT {
    Tree elem;
    NodoQT* siguiente;
};

struct QueueTreeSt {
    int cantidad;
    NodoQT* primero;
    NodoQT* ultimo;
};

typedef QueueTreeSt* QueueT;