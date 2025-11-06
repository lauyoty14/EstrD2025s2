#include <iostream>
#include "pokemon.h"
#include "entrenador.h"
using namespace std;

int main(){
    Pokemon p1 = consPokemon("agua");
    Pokemon p2 = consPokemon("fuego");
    Pokemon p3 = consPokemon("planta");

    cout << "Tipo de p1: " << tipoDePokemon(p1) << ", Energia: " << energia(p1) << "%" << endl;
    cout << "Tipo de p2: " << tipoDePokemon(p2) << ", Energia: " << energia(p2) << "%" << endl;
    cout << "Tipo de p3: " << tipoDePokemon(p3) << ", Energia: " << energia(p3) << "%" << endl;

    perderEnergia(30, p1);
    cout << "Despues de perder energia, Energia de p1: " << energia(p1) << "%" << endl;

    cout << "¿p1 supera a p2? " << (superaA(p1, p2) ? "Si" : "No") << endl;
    cout << "¿p2 supera a p3? " << (superaA(p2, p3) ? "Si" : "No") << endl;
    cout << "¿p3 supera a p1? " << (superaA(p3, p1) ? "Si" : "No") << endl;

    Pokemon pokemonesEntrenador1[2] = {p1, p2};
    Entrenador e1 = consEntrenador("Ash", 2, pokemonesEntrenador1);

    Pokemon pokemonesEntrenador2[2] = {p2, p3};
    Entrenador e2 = consEntrenador("Misty", 2, pokemonesEntrenador2);

    cout << "Nombre del entrenador 1: " << nombreDeEntrenador(e1) << endl;
    cout << "Cantidad de pokémon del entrenador 1: " << cantidadDePokemon(e1) << endl;
    cout << "Cantidad de pokémon de tipo 'fuego' del entrenador 1: " 
         << cantidadDePokemonDe("fuego", e1) << endl;

    cout << "¿El entrenador 1 le gana a todos los pokémon del entrenador 2? " 
         << (leGanaATodos(e1, e2) ? "Si" : "No") << endl;

    return 0;
}