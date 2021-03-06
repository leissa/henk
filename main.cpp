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
    std::cout << "\n===  test1  ===\n\n"; 
    auto type_lambda = poly_id(world, "a", "x");

    auto atype = type_lambda->type();
    type_lambda.dump(std::cout);
    std::cout << " : ";
    atype.dump();
}

void test2(World& world) {
    std::cout << "\n===  test2  ===\n\n";
     // u = lambda y:* . Int
    // (lambda x: (u Bool). 42) (Int)
    
    auto u = world.lambda(world.get_prim_const("*"), "y");
    u->close(world.get_prim_const("Int"));
    
    auto lambda = world.lambda(world.app(u, world.get_prim_const("Bool")), "x");
    lambda->close(world.literal(42));
    
    world.add_external(lambda);
    world.cleanup();
    //world.show_expressions();

    auto app = world.app(lambda, world.literal(33));
    std::cout << "reducing 33: " << std::endl;
    lambda->dump();
    app->dump();
}

void test3(World& world) {
    std::cout << "\n===  test3  ===\n\n";
     // f (forall b. b -> b)
    // where f: forall a. a -> Int
    // should fail in predicative system
    auto f = //world.pi("α", world.get_prim_const("*"));
        world.lambda(world.get_prim_const("*"), "α");
    auto inf = world.lambda(f->var(), "x");
    inf->close(world.literal(42));
    f->close(inf);
    
    auto forallb = world.pi(world.get_prim_const("*"), "β");
    forallb->close(world.fun_type(forallb->var(), forallb->var()));
    std::cout << "f = ";
    f.dump(std::cout);
    std::cout << " : ";
    f->type().dump();
    std::cout << "g = ";
    forallb.dump(std::cout);
    std::cout << " : ";
    forallb->type().dump();
    
    auto app = world.app(f, forallb);
    std::cout << "f g = ";
    app.dump();
    
    std::cout << "f g : ";
    auto apptype = app->type();
    apptype.dump();

}

void test4(World& world) {
    std::cout << "\n===  test4  ===\n\n";
    auto i42 = world.literal(42);
    auto i42prim = world.literal(42);
    std::cout << "created two numbers 42 and their physical addresses are ";
    std::cout << *i42 << " and " << *i42prim << std::endl;
    assert(*i42 == *i42prim && "number 42 have different addresses");
    
    auto id1 = poly_id(world, "α", "x");
    std::cout << "id1 = ";
    id1.dump(std::cout);
    std::cout << std::endl;
    std::cout << "id2 = ";
    auto id2 = poly_id(world, "β", "y");
    id2.dump(std::cout);
    std::cout << "\nin memory: id1 = " << *id1 << ", id2 = " << *id2 << std::endl;
    assert(*id1 == *id2 && "id functions have different addresses");
}

void test5(World& world) {
    std::cout << "\n===  test5  ===\n\n";
    auto singl = world.tuple({world.literal(42)});
    singl.dump(std::cout);
    std::cout << ": ";
    singl->type().dump();
    auto p = world.tuple({world.literal(23), singl});
    p.dump(std::cout);
    std::cout << ": ";
    p->type().dump();
    auto p2 = world.extract(p, 1);
    p2.dump();
}

void test6(World& world) {
    std::cout << "\n===  test6  ===\n\n";
    auto i42 = world.literal(42);
    auto i8 = world.literal(8);
    auto plus = world.get_primop("+");
    plus.dump();
    auto r = world.app(plus, world.tuple(std::vector<Def> {i42, i8}));
    r.dump();
}

void test7(World& world) {
    std::cout << "\n===  test7  ===\n\n";
    auto dint = world.get_prim_const("Int");
    auto plus = world.get_primop("+");
    auto l1 = world.lambda(dint, "x");
    l1->close(world.app(plus, 
        world.tuple(std::vector<Def> {l1->var(), world.literal(7)})));
    auto l2 = world.lambda(dint, "y");
    
    l2->close(world.app(plus, world.tuple(std::vector<Def>{world.app(plus, 
        world.tuple(std::vector<Def>{world.literal(3), world.literal(4)})), l2->var()})));
    l1.dump();
    l2.dump();
    assert(*l1 == *l2 && "lambdas differ");
}

void test8(World& world) {
    std::cout << "\n===  test8  ===\n\n";
    auto intt = world.get_prim_const("Int");
    auto R = world.abs_record( std::vector<std::pair<std::string, Def> >{std::make_pair("num", intt), 
        std::make_pair<std::string, Def>("fun", world.fun_type(intt, intt))} );
    R.dump();
    std::cout << "has type: "; R->type().dump();
    auto Rdup = world.abs_record( std::vector<std::pair<std::string, Def> >{std::make_pair("num", intt), 
        std::make_pair<std::string, Def>("fun", world.fun_type(intt, intt))} );
    assert(R != Rdup && "equal abstract records contradict nominative typing for them");
    
    auto l = world.lambda(intt, "x");
    l->close(l->var());
    
    auto r = world.inst_record(std::vector<std::pair<std::string, Def> >{
        std::make_pair<std::string, Def>("fun", l),
        std::make_pair<std::string, Def>("num", world.literal(42))
    }, R);
    r.dump(std::cout); std::cout << ": "; r->type().dump();
    
    auto rdup = world.inst_record(std::vector<std::pair<std::string, Def> >{
        std::make_pair<std::string, Def>("fun", l),
        std::make_pair<std::string, Def>("num", world.literal(42))
    }, R);
    assert(r != rdup && "equal instances of records contradict nominative typing for them");
    
    for(auto& f : R->get_fields()) {
        auto elem = world.inst_record_extract(r, f);
        auto type = world.abs_record_extract(R, f);
        std::cout << "at field " << f.label() << " r has ";
        elem.dump(std::cout); std::cout << " : "; type.dump();
        std::cout << "elem: "; elem->type().dump();
        std::cout << "type: "; type->type().dump();
    }
    std::cout << std::endl;
    
    auto r2 = world.inst_record(std::vector<std::pair<std::string, Def> >{
        std::make_pair<std::string, Def>("fun", l),
        std::make_pair<std::string, Def>("blah", world.literal(42))
    }, R);
    r2.dump(std::cout); std::cout << ": "; r2->type().dump();
    
    std::cout << std::endl;
    
    auto r3 = world.inst_record(std::vector<std::pair<std::string, Def> >{
        std::make_pair<std::string, Def>("fun", l),
        std::make_pair<std::string, Def>("num", l)
    }, R);
    r3.dump(std::cout); std::cout << ": "; r3->type().dump();
}

void test9(World& world) {
    std::cout << "\n===  test9  ===\n\n";
    auto intt = world.get_prim_const("Int");
    auto star = world.get_prim_const("*");
    auto sig = world.sigma(star, "x");
    sig->close(world.fun_type(sig->var(), sig->var()));
    sig.dump(std::cout); std::cout << ": "; sig->type().dump();
    
    auto l = world.lambda(intt, "y");
    l->close(l->var());
    auto p = world.pair(intt, l, sig);
    p.dump(std::cout); std::cout << ": "; p->type().dump();
    std::cout << "first = ";
    auto fst = world.first(p); fst.dump(std::cout);
    std::cout << ": "; fst->type().dump();
    std::cout << "second = ";
    auto snd = world.second(p); snd.dump(std::cout);
    std::cout << ": "; snd->type().dump();
    
    auto booll = world.get_prim_const("Bool");
    auto l2 = world.lambda(booll, "z");
    l2->close(world.fun_type(booll, booll));
    l2.dump(std::cout); std::cout << ": "; l2->type().dump();
    auto p2 = world.pair(booll, l2, sig);
    p2.dump(std::cout); std::cout << ": "; p2->type().dump();
}

void test10(World& world) {
    std::cout << "\n===  test10  ===\n\n";
    auto intt = world.get_prim_const("Int");
    auto star = world.get_prim_const("*");
    auto sig = world.sigma(star, "x");
    sig->close(sig->var());
    sig.dump(std::cout); std::cout << ": "; sig->type().dump();
    
    auto p = world.pair(intt, world.literal(42), sig);
    p.dump(std::cout); std::cout << ": "; p->type().dump();
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
            test8(*world);
            test9(*world);
            test10(*world);
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
                case 8: test8(*world); break;
                case 9: test9(*world); break;
                case 10: test10(*world); break;
                default: 
                    throw std::runtime_error("wrong number of test case");
            }
            break;
        default:
            throw std::runtime_error("give number of test case from 1 to 9");
    }

    //std::cout << "\n\nworld has expressions: " << std::endl;
    //world->dump(std::cout);
    std::cout << std::endl;
}
