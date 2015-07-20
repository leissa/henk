#include "world.h"

#include <iostream>

using namespace henk;

int main() {
    auto world = new World();
    world->show_prims(std::cout);
    std::cout << std::endl;   
    auto type_lam = //const_cast<Lam*>(
        world->mk_lam("a", world->prim_consts.at("*"));
    //);
    auto id_lam = world->mk_lam("x", type_lam->var());
    auto x_occ = world->mk_varOcc(id_lam);
   // auto app = world->mkApp(x_occ, x_occ);
    id_lam->close(x_occ);
    type_lam->close(id_lam);
    world->dump(type_lam);
    std::cout << std::endl;
    
    //--------------------------------------
    // f (forall b. b -> b)
    // where f: forall a. a -> Int
    // should fail
    auto f = world->mk_pi("α", world->prim_consts.at("*"));
    f->close(world->mk_function_type(
        f->var(), world->prim_consts.at("Int")
        )
    );
    auto forallb = world->mk_pi("β", world->prim_consts.at("*"));
    forallb->close(world->mk_function_type(
        forallb->var(), forallb->var()
        )
    );
    world->dump(f);
    std::cout << std::endl;
    world->dump(forallb);
    std::cout << std::endl;
    auto app = world->mk_app(f, forallb);
    world->dump(app);
    std::cout << std::endl;
    auto apptype = world->typecheck(app);
    world->dump(apptype);
    /*
    try {
        auto atype = world->typecheck(type_lam);
        std::cout << std::endl;
        world->dump(atype);
    } catch(const std::exception& e)
    {
        std::cout << "sth wrong: " << e.what() << std::endl;
    }*/
    std::cout << std::endl;
}
