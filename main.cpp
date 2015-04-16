#include "henk.h"

#include <iostream>

using namespace henk;

int main() {
    auto kind = new Const(nullptr, "kind");
    auto int_ = new Const(kind, "int");
    auto abs = new Abs(int_, "id", "x");
    abs->close(abs->var()); // lambda x.x
    abs->dump();
    std::cout << std::endl;
}
