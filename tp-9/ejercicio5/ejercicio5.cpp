#include "fraccion.cpp"
#include <iostream>
using namespace std;

int main()
{
    Fraccion f = consFraccion(1, 2);
    Fraccion f2 = consFraccion(3, 4);
    Fraccion f3 = multF(f, f2);

    cout << "Ejercicio 5 - Fracciones" << endl;
    cout << "------------------------" << endl;
    cout << "fraccion: " << "numerador " << numerador(f) << " denominador " << denominador(f) << endl;
    cout << "division: " << division(f) << endl;
    cout << "multiplicacion: " << "numerador " << numerador(f3) << " denominador " << denominador(f3) << endl;
    cout << "suma: " << "numerador " << numerador(sumF(f, f2)) << " denominador " << denominador(sumF(f, f2)) << endl;
    cout << "fraccion simplificada de la suma: " << "numerador " << numerador(simplificada(sumF(f, f2))) << " denominador " << denominador(simplificada(sumF(f, f2))) << endl;
}
