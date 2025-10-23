#include "par.cpp"

int main(int argc, char const *argv[])
{
    Par p = consPar(10, 4);

    int first = fst(p);
    int second = snd(p);
    int maximum = maxDelPar(p);
    Par swapped = swap(p);
    Par divAndRes = divisionYResto(20, 6);

    cout << "First: " << first << endl;
    cout << "Second: " << second << endl;
    cout << "Maximum: " << maximum << endl;
    cout << "Swapped: (" << swapped.x << ", " << swapped.y << ")" << endl;
    cout << "Division y resto de (20, 6): (" << divAndRes.x << ", " << divAndRes.y << ")" << endl;
}
