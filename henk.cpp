#include "henk.h"

#include <iostream>

namespace henk {

/*
 * dump
 */

void Const::dump() const {
    std::cout << name();
}

void Var::dump() const {
    std::cout << name();
}

void Body::dump_body() const {
    var()->dump();
    std::cout << ": ";
    var()->type()->dump();
    std::cout << ". ";
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
