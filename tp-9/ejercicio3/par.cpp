#include <iostream>
#include "par.h"
using namespace std;

// Propósito: construye un par
Par consPar(int x, int y){
    Par p;
    p.x = x;
    p.y = y;
    return p;
}

// Propósito: devuelve la primera componente
int fst(Par p){
    return p.x;
}

// Propósito: devuelve la segunda componente
int snd(Par p){
    return p.y;
}

// Propósito: devuelve la mayor componente
int maxDelPar(Par p){
    if (p.x > p.y){
        return p.x;
    } else {
        return p.y;
    }
}

// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p){
    Par r; 
    r.x = p.y;
    r.y = p.x;
    return r;
}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
// Precondición: m != 0
Par divisionYResto(int n, int m){
    Par r; 
    r.x = n / m;
    r.y = n % m;
    return r;
}
