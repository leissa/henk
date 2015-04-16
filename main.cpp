#include "henk.h"

#include <iostream>

using namespace henk;

int main() {
    auto kind = new Const(nullptr, "kind");
    kind->dump();
    std::cout << std::endl;
    //auto abs = new Abs(nullptr);
    //abs->close(abs->var()); // lambda x.x
}
