#include "persona.cpp"

int main() {
    PersonaSt* juan = consPersona("Juan", 25);
    PersonaSt* maria = consPersona("Maria", 30);

    cout << "Nombre de Juan: " << nombre(juan) << ", Edad: " << edad(juan) << endl;
    cout << "Nombre de Maria: " << nombre(maria) << ", Edad: " << edad(maria) << endl;
    crecer(juan);
    cout << "Despues de crecer, Edad de Juan: " << edad(juan) << endl;
    cout << "Es Maria mayor que juan? " << (esMayorQueLaOtra(maria, juan) ? "Si" : "No") << endl;
    cout << "La persona mayor entre juan y maria es: " << nombre(laQueEsMayor(juan, maria)) << endl;
}
