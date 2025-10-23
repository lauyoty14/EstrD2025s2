#include <iostream>
using namespace std;

// VERSION ITERATIVA
// 1. 
// Propósito: imprime n veces un string s.
void IprintN(int n, string s){
    for (int i = 0; i < n; i++){
        cout << s << endl;
    }
}

// 2. 
// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void IcuentaRegresiva(int n){
    for (int i = n; i >= 0; i--){
        cout << i << " " << endl;
    }
}

//3. 
// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void IdesdeCeroHastaN(int n){
    for (int i = 0; i <= n; i++){
        cout << i << " " << endl;
    }
}

// 4. 
// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int Imult(int n, int m){
    int resultado = 0;
    for (int i = 0; i < m; i++){
        resultado += n;
    }
    return resultado;
}

// 5. 
// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
void IprimerosN(int n, string s){
    for (int i = 0; i < n; i++){
        cout << s[i] << " " << endl;
    }
}

// 6. 
// Propósito: indica si un char c aparece en el string s.
bool Ipertenece(char c, string s){
    for (int i = 0; i < s.length(); i++){
        if (s[i] == c){
            return true;
        }
    }
    return false;
}

// 7. 
// Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int Iapariciones(char c, string s){
    int contador = 0;
    for (int i = 0; i < s.length(); i++){
        if (s[i] == c){
            contador++;
        }
    }
    return contador;
}

// VERSION RECURSIVA 
// 1. 
// Propósito: imprime n veces un string s.
void RprintN(int n, string s){
    if (n > 0){
        cout << s << endl;
        RprintN(n - 1, s);
    }
}
// 2. 
// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void RcuentaRegresiva(int n){
    if (n >= 0){
        cout << n << " " << endl;
        RcuentaRegresiva(n - 1);
    }
}

//3. 
// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void RdesdeCeroHastaN(int n){
    if (n >= 0){
        RdesdeCeroHastaN(n - 1);
        cout << n << " " << endl;
    }
}

// 4. 
// Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int Rmult(int n, int m){
    if (m == 0){
        return 0;
    } else {
        return n + Rmult(n, m - 1);
    }
}

// 5. 
// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
void RprimerosN(int n, string s){
    if (n > 0){
        RprimerosN(n - 1, s);
        cout << s[n - 1] << " " << endl;
    }
}

// 6. 
// Propósito: indica si un char c aparece en el string s.
bool Rpertenece(char c, string s){
    while (s.length() > 0){
        if (s[0] == c){
            return true;
        } else {
            s = s.substr(1);
            return Rpertenece(c, s);
        }
    }
}

// 7. 
// Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int Rapariciones(char c, string s){
    while (s.length() > 0){
        if (s[0] == c){
            return 1 + Rapariciones(c, s.substr(1));
        } else {
            return Rapariciones(c, s.substr(1));
        }
    }
}

int main() {
    // Puedes agregar código aquí para probar las funciones si lo deseas.
    
    IprintN(3, "Hola");
    IcuentaRegresiva(5);
    IdesdeCeroHastaN(5);
    Imult(4, 3);
    IprimerosN(4, "Ejemplo");
    cout << "pertenece: " << Ipertenece('e', "Ejemplo") << endl;
    cout << "apariciones: " << Iapariciones('e', "Ejemplo") << endl;
    
    
    RprintN(3, "Hola");
    RcuentaRegresiva(5);
    RdesdeCeroHastaN(5);
    cout << "4 * 3 = " << Rmult(4, 3) << endl;
    RprimerosN(4, "Ejemplo"); 
    cout << "pertenece: " << Rpertenece('e', "Ejemplo") << endl;
    cout << "apariciones: " << Rapariciones('e', "Ejemplo") << endl;
}