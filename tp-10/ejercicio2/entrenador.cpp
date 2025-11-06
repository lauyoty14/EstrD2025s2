#include <iostream>
#include "entrenador.h"
#include "pokemon.h"
using namespace std;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemons){
    Entrenador e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantPokemon = cantidad;
    e->pokemon = new Pokemon[cantidad];

    for (int i = 0; i < cantidad; i++) {
        e->pokemon[i] = pokemons[i];
    }

    return e;
}
//Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese 
//tamaño, devuelve un entrenador.

string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}
//Devuelve el nombre del entrenador.

int cantidadDePokemon(Entrenador e){
    return e->cantPokemon;
}
//Devuelve la cantidad de pokémon que posee el entrenador

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    int contador = 0;
    for (int i = 0; i < e->cantPokemon; i++){
        if (tipoDePokemon(e->pokemon[i]) == tipo){
            contador++;
        }
    }
    return contador;
}
//Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.

Pokemon pokemonNro(int i, Entrenador e){
    return e->pokemon[i-1];
}
//Devuelve el pokémon número i de los pokémon del entrenador.
//Precondición: existen al menos i − 1 pokémon

bool leGanaATodos(Entrenador e1, Entrenador e2) {
    // Recorremos todos los pokémon del segundo entrenador
    for (int i2 = 0; i2 < e2->cantPokemon; i2++) {
        bool algunoLeGana = false;

        // Buscamos si hay al menos uno en e1 que le gane
        for (int i1 = 0; i1 < e1->cantPokemon; i1++) {
            if (tipoSuperaATipo(
                    tipoDePokemon(pokemonNro(i1 + 1, e1)),   // i+1 para respetar tu función
                    tipoDePokemon(pokemonNro(i2 + 1, e2)))) {
                algunoLeGana = true;
                break; // no hace falta seguir buscando
            }
        }

        // Si no hay ninguno que le gane a este, entonces e1 no le gana a todos
        if (!algunoLeGana)
            return false;
    }

    // Si llegamos hasta acá, e1 le gana a todos
    return true;
}
//Dados dos entrenadores, indica si, para cada pokémon del segundo 
//entrenador, el primero posee al menos un pokémon que le gane.
