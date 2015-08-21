#include "world.h"

using namespace henk;

Def mk_poly_id(World* world, std::string tvar, std::string var) {
    auto type_lam = world->mk_lam(tvar, world->get_prim_const("*"));
    auto id_lam = world->mk_lam(var, type_lam.abs_var());
    auto x_occ = world->mk_var_occ(id_lam);
    id_lam.close_abs(x_occ);
    type_lam.close_abs(id_lam);

    return type_lam;
}

void test1(World* world) {
    auto type_lam = mk_poly_id(world, "a", "x");

    auto atype = world->typecheck(type_lam);
    std::cout << std::endl;
    world->dump(type_lam);
    std::cout << " : ";
    world->dump(atype);
}

void test2(World* world) {
     // u = lambda y:* . Int
    // (lambda x: (u Bool). 42) (Int)
    
    auto u = world->mk_lam("y", world->get_prim_const("*"));

    u.close_abs(world->get_prim_const("Int"));

    auto lam = world->mk_lam("x", world->mk_app(
        u, world->get_prim_const("Bool")
        )
    );
    lam.close_abs(world->mk_int(42));

    auto app = world->mk_app(lam, world->mk_int(33));//get_prim_const("Int"));
    world->dump(app);
    auto tapp = world->typecheck(app);
    std::cout << " : ";
    world->dump(tapp);
}

void test3(World* world) {
     // f (forall b. b -> b)
    // where f: forall a. a -> Int
    // should fail
    auto f = world->mk_pi("α", world->get_prim_const("*"));

    f.close_abs(world->mk_fun_type(
            world->mk_var_occ(f), world->get_prim_const("Int")
        )
    );
    auto forallb = world->mk_pi("β", world->get_prim_const("*"));
    forallb.close_abs(world->mk_fun_type(
        world->mk_var_occ(forallb), world->mk_var_occ(forallb)
        )
    );
    std::cout << "f = ";
    world->dump(f);
    std::cout << std::endl;
    std::cout << "g = ";
    world->dump(forallb);
    
    std::cout << std::endl;
    auto app = world->mk_app(f, forallb);
    std::cout << "f g = ";
    world->dump(app);
    std::cout << std::endl;
    try {
        std::cout << "f g : ";
        auto apptype = world->typecheck(app);
        world->dump(apptype);
    } catch (std::runtime_error& e) {
        std::cout << "typecheck error: " << e.what() << std::endl;
    }
}

void test4(World* world) {
    auto i42 = world->mk_int(42);
    auto i42prim = world->mk_int(42);
    std::cout << "created two numbers 42 and their physical addresses are ";
    std::cout << *i42 << " and " << *i42prim << std::endl;
    assert(*i42 == *i42prim && "number 42 have different addresses");
    
    auto id1 = mk_poly_id(world, "α", "x");
    std::cout << "id1 = ";
    world->dump(id1);
    std::cout << std::endl;
    std::cout << "id2 = ";
    auto id2 = mk_poly_id(world, "β", "y");
    world->dump(id2);
    std::cout << "\nin memory: id1 = " << *id1 << ", id2 = " << *id2 << std::endl;
}

int main(int argc, char* argv[]) {
    auto world = new World();
    world->show_prims(std::cout);
    std::cout << std::endl;
    if(argc == 2) {
        std::string arg = argv[1];
        switch(std::stoi(arg)) {
            case 1: test1(world); break;
            case 2: test2(world); break;
            case 3: test3(world); break;
            case 4: test4(world); break;
            default: throw std::runtime_error("wrong number of test case");
        }
    }
    std::cout << std::endl << "world has expressions: " << std::endl;
    world->show_expressions();
    std::cout << std::endl;
    delete world;
    
}
