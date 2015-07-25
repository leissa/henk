#include "henk.h"
#include "hash.h"

#include <iostream>

namespace henk {

size_t AnnotatedExpr::vhash() const {
    return hash_combine(hash_begin(gid()), type()->gid());
}
/*
size_t Const::vhash() const {
    return hash_value(type()->gid());
}

size_t VarIntr::vhash() const {
    return hash_begin(type()->gid());
}*/

size_t IntValueConst::vhash() const {
    return hash_begin(value());
}

size_t BoolValueConst::vhash() const {
    return hash_begin(value() ? 2 : 1);
}

size_t PrimConst::vhash() const {
    return hash_begin(gid());//name());
}

size_t VarOcc::vhash() const {
    return hash_begin(gid());//introduced_by()->hash();
}

size_t Body::vhash() const {
    return hash_begin(var()->hash());
    // body is a nullptr just after construction
    // but we want to hash it after being closed... 
  //  return hash_combine(var()->hash(), body()->hash());
}

size_t App::vhash() const {
    return  hash_combine(apply()->hash(), arg()->hash());
}

}
