#include "henk.h"
#include "hash.h"

#include <iostream>

namespace henk {

size_t AnnotatedExpr::vhash() const {
    return /*hash_combine(hash_begin(name),*/ type()->vhash();//);
}

size_t IntValueConst::vhash() const {
    return hash_begin(value());
}

size_t BoolValueConst::vhash() const {
    return hash_begin(value() ? 1 : 0);
}

size_t PrimConst::vhash() const {
    return 1;//hash_begin(name());
}

size_t VarOcc::vhash() const {
    return introduced_by()->vhash();
}

size_t Body::vhash() const {
    return hash_combine(var()->vhash(), body()->vhash());
}

size_t App::vhash() const {
    return hash_combine(apply()->vhash(), arg()->vhash());
}

}
