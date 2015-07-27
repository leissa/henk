#include "world.h"

#include <iostream>

using namespace henk;

Lam* mk_poly_id(World* world, std::string tvar, std::string var) {
    auto type_lam = const_cast<Lam*>(
        world->mk_lam(tvar, world->prim_consts.at("*"))
    );
  //  std::cout << "fsdfsd! " << std::endl;
    auto id_lam = const_cast<Lam*>(world->mk_lam(var, type_lam->var()));
    auto x_occ = world->mk_varOcc(id_lam);
 //   std::cout << "nbvcx " << std::endl;
    id_lam = const_cast<Lam*>(world->close_body(id_lam, x_occ)->as<Lam>());
//  std::cout << "urwe! " << std::endl;
    type_lam = const_cast<Lam*>(world->close_body(type_lam, id_lam)->as<Lam>());
    return type_lam;
}

void test1(World* world) {
    auto type_lam = mk_poly_id(world, "a", "x");

    auto atype = world->typecheck(type_lam);
    std::cout << std::endl;
    world->dump(atype);
}

void test2(World* world) {
     // U = lambda _ . Int
    // (lambda x: (U sth). 42) (Int)
    
    auto u = const_cast<Lam*>(world->mk_lam("y", world->prim_consts.at("*")));
    u = const_cast<Lam*>(world->close_body(u, world->prim_consts.at("Int"))->as<Lam>());
    auto lam = const_cast<Lam*>(world->mk_lam("x", world->mk_app(
        u, world->prim_consts.at("Bool")
        )
    ));
    lam = const_cast<Lam*>(world->close_body(lam, world->mk_int(42))->as<Lam>());
    auto app = world->mk_app(lam, world->mk_int(33));//prim_consts.at("Int"));
    world->dump(app);
    auto tapp = world->typecheck(app);
    std::cout << " : ";
    world->dump(tapp);
}

void test3(World* world) {
     // f (forall b. b -> b)
    // where f: forall a. a -> Int
    // should fail
    auto f = const_cast<Pi*>(world->mk_pi("α", world->prim_consts.at("*")));
    f = const_cast<Pi*>(world->close_body(f, world->mk_function_type(
        f->var(), world->prim_consts.at("Int")
        )
    )->as<Pi>());
    auto forallb = const_cast<Pi*>(world->mk_pi("β", world->prim_consts.at("*")));
    forallb = const_cast<Pi*>(world->close_body(forallb, world->mk_function_type(
        forallb->var(), forallb->var()
        )
    )->as<Pi>());
    world->dump(f);
    std::cout << std::endl;
    world->dump(forallb);
    std::cout << std::endl;
    auto app = world->mk_app(f, forallb);
    world->dump(app);
    std::cout << std::endl;
    try {
        auto apptype = world->typecheck(app);
        world->dump(apptype);
    } catch (std::runtime_error& e) {
        std::cout << "typecheck error: " << e.what() << std::endl;
    }
}

void test4(World* world) {
    auto i42 = world->mk_int(42);
    auto i42prim = world->mk_int(42);
    std::cout << i42 << " vs " << i42prim << std::endl;
    
    auto id1 = mk_poly_id(world, "α", "x");
    world->dump(id1);
    std::cout << std::endl;
    auto id2 = mk_poly_id(world, "β", "y");
    world->dump(id2);
    std::cout << "\nin memory: id1 = " << id1 << ", id2 = " << id2 << std::endl;
}

int main() {
    auto world = new World();
    world->show_prims(std::cout);
    std::cout << std::endl;   
    test4(world);
    std::cout << std::endl << "world has expressions: " << std::endl;
    world->show_expressions();
    std::cout << std::endl;
}
