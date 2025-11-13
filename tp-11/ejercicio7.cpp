#include "nodeT.cpp"
#include "arraylist.cpp"

int sumarT(Tree t){
    if (isEmptyT(t)){
        return 0;
    } else {
        return rootT(t) + sumarT(left(t)) + sumarT(right(t));
    }
}
//Dado un árbol binario de enteros devuelve la suma entre sus elementos.
// Costo: O(n)

int sizeT(Tree t){
    if (isEmptyT(t)){
        return 0;
    } else {
        return 1 + sizeT(left(t)) + sizeT(right(t));
    }
}
//Dado un árbol binario devuelve su cantidad de elementos, es decir, 
//el tamaño del árbol (size en inglés).
// Costo: O(n)

bool perteneceT(int e, Tree t){
    if (isEmptyT(t)){
        return false;
    } else if (rootT(t) == e){
        return true;
    } else {
        return perteneceT(e, left(t)) || perteneceT(e, right(t));
    }
}
//Dados un elemento y un árbol binario devuelve True si existe un elemento 
//igual a ese en el árbol.
// Costo: O(n)

int aparicionesT(int e, Tree t){
    int contador = 0;

    if (isEmptyT(t)){
        return 0;
    } else if (rootT(t) == e){
        contador += 1;
        return contador + aparicionesT(e, left(t)) + aparicionesT(e, right(t));
    } else {
        return aparicionesT(e, left(t)) + aparicionesT(e, right(t));
    }
}
//Dados un elemento e y un árbol binario devuelve la cantidad de elementos
//del árbol que son iguales a e.
// Costo: O(n)

int heightT(Tree t){
    if (isEmptyT(t)){
        return 0;
    } else {
        int alturaIzq = heightT(left(t));
        int alturaDer = heightT(right(t));

        if (alturaIzq > alturaDer){
            return 1 + alturaIzq;
        } else {
            return 1 + alturaDer;
        }
    }
}
//Dado un árbol devuelve su altura.
// Costo: O(n)

ArrayList toList(Tree t){
    if (isEmptyT(t)){
        return newArrayList();
    } else {
        ArrayList leftList = toList(left(t));
        ArrayList rightList = toList(right(t));

        add(rootT(t), leftList);
        ArrayList resultado = append(leftList, rightList);
    }
}
//Dado un árbol devuelve una lista con todos sus elementos.
// Costo: O(n²) si append es O(n), o O(n) si append es O(1)

ArrayList leaves(Tree t){
    if (isEmptyT(t)){
        return newArrayList();
    } else if (isEmptyT(left(t)) && isEmptyT(right(t))){
        ArrayList lista = newArrayList();
        add(rootT(t), lista);
        return lista;
    } else {
        ArrayList leftLeaves = leaves(left(t));
        ArrayList rightLeaves = leaves(right(t));

        return append(leftLeaves, rightLeaves);
    }
}
//Dado un árbol devuelve los elementos que se encuentran en sus hojas.

ArrayList levelN(int n, Tree t){
    if (isEmptyT(t)){
        return newArrayList();
    } else if (n == 0){
        ArrayList lista = newArrayList();
        add(rootT(t), lista);
        return lista;
    } else {
        ArrayList leftLevel = levelN(n - 1, left(t));
        ArrayList rightLevel = levelN(n - 1, right(t));
        return append(leftLevel, rightLevel);
    }
}
//Dados un número n y un árbol devuelve una lista con los nodos de nivel n.