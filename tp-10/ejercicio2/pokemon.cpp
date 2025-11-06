#include <iostream>
#include "pokemon.h"
using namespace std;

Pokemon consPokemon(TipoDePokemon tipo){
    Pokemon p = new PokeSt;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}
//Dado un tipo devuelve un pokémon con 100 % de energía.

TipoDePokemon tipoDePokemon(Pokemon p){
    return p->tipo;
}
//Devuelve el tipo de un pokémon.

int energia(Pokemon p){
    return p->vida;
}
//Devuelve el porcentaje de energía

void perderEnergia(int energia, Pokemon p){
    p->vida -= energia;
    if (p->vida < 0){
        p->vida = 0;
    }
}
//Le resta energía al pokémon.

bool superaA(Pokemon p1, Pokemon p2){
    return tipoSuperaATipo(p1->tipo, p2->tipo);
}
//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
//a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.

bool tipoSuperaATipo(TipoDePokemon t1, TipoDePokemon t2){
    if ((t1 == "agua" && t2 == "fuego") ||
        (t1 == "fuego" && t2 == "planta") ||
        (t1 == "planta" && t2 == "agua")){
            return true;
    } else {
        return false;
    }
}