#include "heapsBinarias.cpp"
#include <iostream>


int main() {
    int elems[] = {5, 3, 8, 1, 6, 2};
    BinHeap h = crearHeap(elems, 6);

    std::cout << "Minimo: " << findMin(h) << std::endl; // → 1

    DeleteMin(h);
    std::cout << "Nuevo minimo: " << findMin(h) << std::endl; // → 2

    InsertH(0, h);
    std::cout << "Minimo tras insertar 0: " << findMin(h) << std::endl; // → 0

    DestroyHeap(h);
}
