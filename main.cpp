#include "world.h"

#include <iostream>

using namespace henk;

int main() {
    auto world = new World();
    world->show_prims(std::cout);
    std::cout << std::endl;   
/*    auto type_lam = //const_cast<Lam*>(
        world->mk_lam("a", world->prim_consts.at("*"));
    //);
    auto id_lam = world->mk_lam("x", type_lam->var());
    auto x_occ = world->mk_varOcc(id_lam);
   // auto app = world->mkApp(x_occ, x_occ);
    id_lam->close(x_occ);
    type_lam->close(id_lam);
    world->dump(type_lam);
    std::cout << std::endl;
    
    try {
        auto atype = world->typecheck(type_lam);
        std::cout << std::endl;
        world->dump(atype);
    } catch(const std::exception& e)
    {
        std::cout << "sth wrong: " << e.what() << std::endl;
    }*/
    //--------------------------------------
    // f (forall b. b -> b)
    // where f: forall a. a -> Int
    // should fail
    auto f = const_cast<Pi*>(world->mk_pi("α", world->prim_consts.at("*")));
    f->close(world->mk_function_type(
        f->var(), world->prim_consts.at("Int")
        )
    );
    auto forallb = const_cast<Pi*>(world->mk_pi("β", world->prim_consts.at("*")));
    forallb->close(world->mk_function_type(
        forallb->var(), forallb->var()
        )
    );
  /*  world->dump(f);
    std::cout << std::endl;
    world->dump(forallb);
    std::cout << std::endl;
    auto app = world->mk_app(f, forallb);
    world->dump(app);
    std::cout << std::endl;
    auto apptype = world->typecheck(app);
    world->dump(apptype);*/
    
    // U = lambda _ . Int
    // (lambda x: (U sth). 42) (Int)
    
    auto u = const_cast<Lam*>(world->mk_lam("y", world->prim_consts.at("*")));
    u->close(world->prim_consts.at("Int"));
    auto lam = const_cast<Lam*>(world->mk_lam("x", world->mk_app(
        u, world->prim_consts.at("Bool")
        )
    ));
    lam->close(/*world->prim_consts.at("*"));//*/world->mk_int(42));
    auto app = world->mk_app(lam, world->mk_int(33));//prim_consts.at("Int"));
    world->dump(app);
    auto tapp = world->typecheck(app);
    std::cout << "\n\n";
    world->dump(tapp);
    std::cout << std::endl;
}
