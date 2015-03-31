#include "henk.h"

using namespace henk;

int main() {
    auto abs = new Abs(nullptr);
    abs->set_body(abs->var()); // lambda x.x
}
