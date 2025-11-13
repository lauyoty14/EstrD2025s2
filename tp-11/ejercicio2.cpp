#include "linkedList.cpp"

int sumatoria(LinkedList xs){
    int total = 0;
    ListIterator ixs = getIterator(xs);
    
    while (!atEnd(ixs)){
        total += current(ixs);
        Next(ixs);
    }

    DisposeIterator(ixs);
    return total;
}
//Devuelve la suma de todos los elementos
// Costo: O(n) siendo n la cantidad de elementos de la lista

void Sucesores(LinkedList xs){
    ListIterator ixs = getIterator(xs);
    
    while (!atEnd(ixs)){
        SetCurrent(current(ixs) + 1, ixs);
        Next(ixs);
    }

    DisposeIterator(ixs);
}
//Incrementa en uno todos los elementos.

bool pertenece(int x, LinkedList xs){
    ListIterator ixs = getIterator(xs);
    
    while (!atEnd(ixs) && !ixs->current->elem == x){
        Next(ixs);
    }

    DisposeIterator(ixs);
    return ixs->current->elem == x;
}
//Indica si el elemento pertenece a la lista
// Costo: O(n)

int apariciones(int x, LinkedList xs){
    int apariciones = 0;
    ListIterator ixs = getIterator(xs);

    while(!atEnd(ixs)){
        if(current(ixs) == x){
            apariciones += 1;
        }
        Next(ixs);
    }

    DisposeIterator(ixs);
    return apariciones;
}
//Indica la cantidad de elementos iguales a x.
// Costo: O(n)

int minimo(LinkedList xs){
    ListIterator ixs = getIterator(xs);
    int minimo = current(ixs);
    Next(ixs);

    while(!atEnd(ixs)){
        if (ixs->current->elem < minimo){
            minimo = ixs->current->elem;
        }
        Next(ixs);
    }

    DisposeIterator(ixs);
    return minimo;
}
//Devuelve el elemento más chico de la lista
// PRECOND: xs no vacía


LinkedList copy(LinkedList xs){
    ListIterator ixs = getIterator(xs);
    LinkedList nueva = nil();
    nueva->cantidad = xs->cantidad;

    while(!atEnd(ixs)){
        Snoc(current(ixs), nueva);
        Next(ixs);
    }

    DisposeIterator(ixs);
    return nueva;    
}
//Dada una lista genera otra con los mismos elementos, en el mismo orden.
//Nota: notar que el costo mejoraría si Snoc fuese O(1), cómo podría serlo?
// Costo: O(n²) si Snoc es O(n), o O(n) si Snoc es O(1)

void Append(LinkedList xs, LinkedList ys){
    ListIterator iys = getIterator(ys);

    while(!atEnd(iys)){
        Snoc(current(iys), xs);
        Next(iys);
    }

    DisposeIterator(iys);
    DestroyL(ys);
}
//Agrega todos los elementos de la segunda lista al final de los de la primera.
//La segunda lista se destruye.
//Nota: notar que el costo mejoraría si Snoc fuese O(1), ¾cómo podría serlo?
// Costo: O(n * m) si Snoc es O(n); O(n + m) si Snoc es O(1)

