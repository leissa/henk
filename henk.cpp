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

void Var::dump() const {
    std::cout << name() << ": ";
    type()->dump();
}

void Body::dump_body() const {
    std::cout << var()->name() << ". ";
    body()->dump();
}

void Abs::dump() const {
    std::cout << "λ";
    dump_body();
}

void Pi::dump() const {
    std::cout << "Π";
    dump_body();
}

}
