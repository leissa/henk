#include <memory>

#include "world.h"

using namespace henk;

Lambda poly_id(World& world, std::string tvar, std::string var) {
    auto type_lambda = world.lambda(world.get_prim_const("*"), tvar);
    auto id_lambda = world.lambda(type_lambda->var(), var);
    id_lambda->close(id_lambda->var());
    type_lambda->close(id_lambda);

    return type_lambda;
}

void test1(World& world) {
    auto type_lambda = poly_id(world, "a", "x");

    auto atype = type_lambda->type();
    std::cout << std::endl;
    type_lambda.dump();
    std::cout << " : ";
    atype.dump();
    std::cout << std::endl;
}

void test2(World& world) {
     // u = lambda y:* . Int
    // (lambda x: (u Bool). 42) (Int)
    
    auto u = world.lambda(world.get_prim_const("*"), "y");
    u->close(world.get_prim_const("Int"));
    
    auto lambda = world.lambda(world.app(u, world.get_prim_const("Bool")), "x");
    lambda->close(world.literal(42));
    
    world.add_external(lambda);
    world.cleanup();
    //world.show_expressions();

    auto app = world.app(lambda, world.literal(33));//get_prim_const("Int"));
    std::cout << app->non_reduced_repr() << " reduced to ";
    app.dump();
    auto tapp = app->type();
    std::cout << " : ";
    tapp.dump();
    std::cout << std::endl;
}

void test3(World& world) {
     // f (forall b. b -> b)
    // where f: forall a. a -> Int
    // should fail in predicative system
    auto f = //world.pi("α", world.get_prim_const("*"));
        world.lambda(world.get_prim_const("*"), "α");
    auto inf = world.lambda(f->var(), "x");
    inf->close(world.literal(42));
    f->close(inf);

    /*f->close(world.fun_type(
            f->var(), world.get_prim_const("Int")
        )
    );*/
    auto forallb = world.pi(world.get_prim_const("*"), "β");
    forallb->close(world.fun_type(forallb->var(), forallb->var()));
    std::cout << "f = ";
    f.dump();
    std::cout << " : ";
    f->type().dump();
    std::cout << std::endl;
    std::cout << "g = ";
    forallb.dump();
    std::cout << " : ";
    forallb->type().dump();
    
    std::cout << std::endl;
    auto app = world.app(f, forallb);
    std::cout << "f g = ";
    app.dump();
    std::cout << std::endl;
    
    std::cout << "f g : ";
    auto apptype = app->type();
    apptype.dump();
    std::cout << std::endl;

}

void test4(World& world) {
    auto i42 = world.literal(42);
    auto i42prim = world.literal(42);
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

void test5(World& world) {
    auto singl = world.tuple({world.literal(42)});
    singl.dump();
    std::cout << ": ";
    singl->type().dump();
    auto p = world.tuple({world.literal(23), singl});
    std::cout << std::endl;
    p.dump();
    std::cout << ": ";
    p->type().dump();
    
    auto p2 = world.extract(p, 1);
    std::cout << std::endl;
    p2.dump();
}

void test6(World& world) {
    auto i42 = world.literal(42);
    auto i8 = world.literal(8);
  /*  auto plus = world->lambda("p", 
        world->tuple(std::vector<Def> {world->get_prim_const("Int"), world->get_prim_const("Int")}));
    auto primpl = world->get_primop("+");
    auto dummypl = world->dummy(plus, world->get_prim_const("Int"));
    dummypl->put_body(primpl);
    plus->close(dummypl);*/
    auto plus = world.get_primop("+");
    plus.dump();
    auto r = world.app(plus, world.tuple(std::vector<Def> {i42, i8}));
    std::cout << std::endl;
    r.dump();
}

void test7(World& world) {
    auto dint = world.get_prim_const("Int");
    auto plus = world.get_primop("+");
    auto l1 = world.lambda(dint, "x");
    l1->close(world.app(plus, 
        world.tuple(std::vector<Def> {l1->var(), world.literal(7)})));
    auto l2 = world.lambda(dint, "y");
    l2->close(world.app(plus, world.tuple(std::vector<Def>{world.app(plus, 
        world.tuple(std::vector<Def>{world.literal(3), world.literal(4)})), l2->var()})));
    l1.dump();
    std::cout << std::endl;
    l2.dump();
    
    assert(l1 == l2 && "lambdas differ");
}

int main(int argc, char* argv[]) {
    std::unique_ptr<World> world(new World());
    world->dump_prims(std::cout);
    std::cout << std::endl;

    switch (argc) {
        case 1:
            test1(*world);
            test2(*world);
            test3(*world);
            test4(*world);
            test5(*world);
            test6(*world);
            test7(*world);
            break;
        case 2:
            switch (std::atoi(argv[1])) {
                case 1: test1(*world); break;
                case 2: test2(*world); break;
                case 3: test3(*world); break;
                case 4: test4(*world); break;
                case 5: test5(*world); break;
                case 6: test6(*world); break;
                case 7: test7(*world); break;
                default: 
                    throw std::runtime_error("wrong number of test case");
            }
            break;
        default:
            throw std::runtime_error("give number of test case from 1 to 7");
    }

    //std::cout << "\n\nworld has expressions: " << std::endl;
    //world->dump();
    std::cout << std::endl;
}
