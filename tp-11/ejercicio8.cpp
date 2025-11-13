#include "queueT.cpp"
#include "arrayList.cpp"

int sumarT(Tree t){
    if (isEmptyT(t)){
        return 0;
    } else {
        QueueT q = emptyQT();
        EnqueueQT(t, q);
        int suma = 0;

        while (!isEmptyQT(q)){
            Tree actual = firstQT(q);
            DequeueQT(q);

            suma += rootT(actual);

            if (!isEmptyT(left(actual))){
                EnqueueQT(left(actual), q);
            }
            if (!isEmptyT(right(actual))){
                EnqueueQT(right(actual), q);
            }
        }

        DestroyQT(q);
        return suma;
    }
}
//Dado un árbol binario de enteros devuelve la suma entre sus elementos.

int sizeT(Tree t){
    if (isEmptyT(t)){
        return 0;
    } else {
        QueueT q = emptyQT();
        EnqueueQT(t, q);
        int contador = 0;

        while (!isEmptyQT(q)){
            Tree actual = firstQT(q);
            DequeueQT(q);
            contador++;

            if (!isEmptyT(left(actual))){
                EnqueueQT(left(actual), q);
            }
            if (!isEmptyT(right(actual))){
                EnqueueQT(right(actual), q);
            }
        }
        DestroyQT(q);
        return contador;
    }
}
//Dado un árbol binario devuelve su cantidad de elementos, es decir, el 
//tamaño del árbol (size en inglés).

bool perteneceT(int e, Tree t){
    if (isEmptyT(t)){
        return false;
    } else {
        QueueT q = emptyQT();
        EnqueueQT(t, q);
        bool encontrado = false;

        while (!isEmptyQT(q) && !encontrado){
            Tree actual = firstQT(q);
            DequeueQT(q);

            if (rootT(actual) == e){
                encontrado = true;
            } else {
                if (!isEmptyT(left(actual))){
                    EnqueueQT(left(actual), q);
                }
                if (!isEmptyT(right(actual))){
                    EnqueueQT(right(actual), q);
                }
            }
        }
        DestroyQT(q);
        return encontrado;
    }
}
//Dados un elemento y un árbol binario devuelve True si existe un elemento 
//igual a ese en el árbol.

int aparicionesT(int e, Tree t){
    if (isEmptyT(t)){
        return 0;
    } else {
        QueueT q = emptyQT();
        EnqueueQT(t, q);
        int contador = 0;

        while (!isEmptyQT(q)){
            Tree actual = firstQT(q);
            DequeueQT(q);

            if (rootT(actual) == e){
                contador++;
            }

            if (!isEmptyT(left(actual))){
                EnqueueQT(left(actual), q);
            }
            if (!isEmptyT(right(actual))){
                EnqueueQT(right(actual), q);
            }
        }
        DestroyQT(q);
        return contador;
    }
}
//Dados un elemento e y un árbol binario devuelve la cantidad de elementos 
//del árbol que son iguales a e.

ArrayList toList(Tree t){
    if (isEmptyT(t)){
        return newArrayList();
    } else {
        QueueT q = emptyQT();
        EnqueueQT(t, q);
        ArrayList lista = newArrayList();

        while (!isEmptyQT(q)){
            Tree actual = firstQT(q);
            DequeueQT(q);

            add(rootT(actual), lista);

            if (!isEmptyT(left(actual))){
                EnqueueQT(left(actual), q);
            }
            if (!isEmptyT(right(actual))){
                EnqueueQT(right(actual), q);
            }
        }
        DestroyQT(q);
        return lista;
    }
}
//Dado un árbol devuelve una lista con todos sus elementos.
