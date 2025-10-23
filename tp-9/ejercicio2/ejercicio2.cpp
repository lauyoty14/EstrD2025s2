#include <iostream>
using namespace std;
// ejercicio 2
//1. 
// Precondición: c1 < c2
// Proposito : muestra cada caracter entre c1 y c2 inclusive
void printFromTo(char c1, char c2) {
    for(int i = 0; c1 + i <= c2; i++) {
        cout << c1 + i << ", ";
    }
    cout << endl;
}
// printFromTo('a', 'd'); // salida: a b c d
// Usa solo una variable local (c) → memoria constante (O(1)).
// No se puede optimizar más.

//2. 
// Precondición: n >= 0
// Proposito : devuelve el factorial de n 
int fc(int n) {
    int x = 1;
    while(n > 0) {
        x = x * n;
        n--;
    }
    return x;
}
// fc(5);   // devuelve 120
// Solo variable x → O(1).
// Es la versión más eficiente posible sin recursión.

//3. 
// Proposito : devuelve la suma de los enteros entre n y m
// Precondición: n <= m
int ft(int n, int m) {
    if (n == m) {
        return n;
    }
    return n + ft(n+1, m);
}
// ft(2, 5);   // devuelve 14 → 2+3+4+5
// Versión recursiva: O(m - n) (un frame por llamada).

// Se puede optimizar con un bucle iterativo o fórmula aritmética.
int ft(int n, int m) {
    int suma = 0;
    for (int i = n; i <= m; i++) {
        suma += i;
    }
    return suma;
}
// Versión iterativa: O(1) → más eficiente.
