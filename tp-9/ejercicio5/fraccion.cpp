#include "fraccion.h"

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador){
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f){
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f){
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f){
    return (float)f.numerador / (float)f.denominador;
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2){
    Fraccion resultado;
    resultado.numerador = f1.numerador * f2.numerador;
    resultado.denominador = f1.denominador * f2.denominador;
    return resultado;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p){
    int a = p.numerador;
    int b = p.denominador;
    int menor = (a < b) ? a : b;

    for (int i = menor; i > 1; i--){
        if (a % i == 0 && b % i == 0){
            a = a / i;
            b = b / i;
            break;
        }
    }

    Fraccion resultado;
    resultado.numerador = a;
    resultado.denominador = b;
    return resultado;
}

// Propósito: devuelve la fracción resultante de sumar las fracciones
Fraccion sumF(Fraccion f1, Fraccion f2){
    Fraccion resultado;
    resultado.numerador = (f1.numerador * f2.denominador) + (f2.numerador * f1.denominador);
    resultado.denominador = f1.denominador * f2.denominador;
    return resultado;
}