#include "henk.h"

#include <iostream>

namespace henk {

/*
 * dump
 */

void Const::dump() const {
    std::cout << name() << ": ";
    if (type_)
        type_->dump();
    else
        std::cout << "box";
}

}
