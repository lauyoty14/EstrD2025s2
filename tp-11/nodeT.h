#include <iostream>
using namespace std;

struct NodeT {
    int elem;
    NodeT* left;
    NodeT* right;
};

typedef NodeT* Tree;