#include "world.h"

using namespace henk;

Lambda poly_id(World* world, std::string tvar, std::string var) {
    auto type_lambda = world->lambda(tvar, world->get_prim_const("*"));
    auto id_lambda = world->lambda(var, type_lambda->var());
    id_lambda->close(id_lambda->var());
    type_lambda->close(id_lambda);

    return type_lambda;
}

void test1(World* world) {
    auto type_lambda = poly_id(world, "a", "x");

    auto atype = type_lambda->inftype();
    std::cout << std::endl;
    type_lambda.dump();
    std::cout << " : ";
    atype.dump();
    std::cout << std::endl;
}

void test2(World* world) {
     // u = lambda y:* . Int
    // (lambda x: (u Bool). 42) (Int)
    
    auto u = world->lambda("y", world->get_prim_const("*"));
    u->close(world->get_prim_const("Int"));
    
    std::cout << "CAREFUL NOW" << std::endl;
    auto lambda = world->lambda("x", world->app(
        u, world->get_prim_const("Bool")
        )
    );
    std::cout << "LEAK BEFORE" << std::endl;
    lambda->close(world->literal(42));
    
    world->add_external(lambda);
    world->cleanup();
    //world->show_expressions();

    auto app = world->app(lambda, world->literal(33));//get_prim_const("Int"));
    std::cout << app->non_reduced_repr() << " reduced to ";
    app.dump();
    auto tapp = app->inftype();
    std::cout << " : ";
    tapp.dump();
    std::cout << std::endl;
}

void test3(World* world) {
     // f (forall b. b -> b)
    // where f: forall a. a -> Int
    // should fail
    auto f = world->pi("α", world->get_prim_const("*"));

    f->close(world->fun_type(
            f->var(), world->get_prim_const("Int")
        )
    );
    auto forallb = world->pi("β", world->get_prim_const("*"));
    forallb->close(world->fun_type(
        forallb->var(), forallb->var()
        )
    );
    std::cout << "f = ";
    f.dump();
    std::cout << " : ";
    f->inftype().dump();
    std::cout << std::endl;
    std::cout << "g = ";
    forallb.dump();
    std::cout << " : ";
    forallb->inftype().dump();
    
    std::cout << std::endl;
    auto app = world->app(f, forallb);
    std::cout << "f g = ";
    app.dump();
    std::cout << std::endl;
    
    std::cout << "f g : ";
    auto apptype = app->inftype();
    apptype.dump();
    std::cout << std::endl;

}

void test4(World* world) {
    auto i42 = world->literal(42);
    auto i42prim = world->literal(42);
    std::cout << "created two numbers 42 and their physical addresses are ";
    std::cout << *i42 << " and " << *i42prim << std::endl;
    assert(*i42 == *i42prim && "number 42 have different addresses");
    
    auto id1 = poly_id(world, "α", "x");
    std::cout << "id1 = ";
    id1.dump();
    std::cout << std::endl;
    std::cout << "id2 = ";
    auto id2 = poly_id(world, "β", "y");
    id2.dump();
    std::cout << "\nin memory: id1 = " << *id1 << ", id2 = " << *id2 << std::endl;
    assert(*id1 == *id2 && "id functions have different addresses");
}

int main(int argc, char* argv[]) {
    auto world = new World();
    world->show_prims(std::cout);
    std::cout << std::endl;
    if (argc == 2) {
        std::string arg = argv[1];
        switch(std::stoi(arg)) {
            case 1: test1(world); break;
            case 2: test2(world); break;
            case 3: test3(world); break;
            case 4: test4(world); break;
            default: throw std::runtime_error("wrong number of test case");
        }
    } else
        throw std::runtime_error("give number of test case from 1 to 4");
    std::cout << std::endl << "world has expressions: " << std::endl;
    world->show_expressions();
    std::cout << std::endl;
    delete world;
    
}
