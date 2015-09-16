#include "world.h"

#include "thorin/util/queue.h"

#include <list>
#include <sstream>

using thorin::Array;
using thorin::ArrayRef;

namespace henk {

World::World()
    : gid_(0)
{
    auto botbox = cse(new BottomNode(*this, gid_++, "⬜ doesn't have a type"));
    auto box = cse(new VarNode(*this, gid_++, botbox, nullptr, "⬜"));
    auto star = cse(new VarNode(*this, gid_++, box, nullptr, "*"));
    auto pint = cse(new VarNode(*this, gid_++, star, nullptr, "Int"));
    auto pbool = cse(new VarNode(*this, gid_++, star, nullptr, "Bool"));
    auto botbox2 = cse(new BottomNode(*this, gid_++, "⬜⬜ doesn't have a type"));
    auto box2 = cse(new VarNode(*this, gid_++, botbox2, nullptr, "⬜⬜"));
    auto star2 = cse(new VarNode(*this, gid_++, box2, nullptr, "**"));
    auto dim = cse(new VarNode(*this, gid_++, box, nullptr, "D"));
    
    botbox->update_non_reduced_repr();
    box->update_non_reduced_repr();
    star->update_non_reduced_repr();
    pint->update_non_reduced_repr();
    pbool->update_non_reduced_repr();
    botbox2->update_non_reduced_repr();
    box2->update_non_reduced_repr();
    star2->update_non_reduced_repr();
    dim->update_non_reduced_repr();
    
    expressions_.insert(botbox); expressions_.insert(box);
    expressions_.insert(star);   expressions_.insert(pint);
    expressions_.insert(pbool);  expressions_.insert(botbox2);
    expressions_.insert(box2);   expressions_.insert(star2);
    expressions_.insert(dim);
    
    prim_consts["*"] = star;     prim_consts["**"] = star2;
    prim_consts["⬜"] = box;      prim_consts["⬜⬜"] = box2;
    prim_consts["Int"] = pint;   prim_consts["Bool"] = pbool;
    prim_consts["⊥ ⬜"] = botbox; prim_consts["⊥ ⬜⬜"] = botbox2;
    prim_consts["D"] = dim;
    
    wavy_arrow_rules[std::make_pair(star, star)]   = star;
    wavy_arrow_rules[std::make_pair(star, star2)]  = star2;
    wavy_arrow_rules[std::make_pair(star2, star)]  = star2;
    wavy_arrow_rules[std::make_pair(star2, star2)] = star2;
    
    // polymorphism
    wavy_arrow_rules[std::make_pair(box, star)]    = star2;
    wavy_arrow_rules[std::make_pair(box, star2)]   = star2;
    
    // type constructors
    wavy_arrow_rules[std::make_pair(box, box)]     = box;
    
    // dependent types
    wavy_arrow_rules[std::make_pair(star, box)]    = box;
    wavy_arrow_rules[std::make_pair(star2, box)]   = box2;
    wavy_arrow_rules[std::make_pair(star, box2)]   = box2;
    wavy_arrow_rules[std::make_pair(star2, box2)]  = box2;
    
    // tuples
    wavy_arrow_rules[std::make_pair(dim, dim)]     = star;
    wavy_arrow_rules[std::make_pair(dim, star)]    = star;
    wavy_arrow_rules[std::make_pair(dim, box)]     = box;
    wavy_arrow_rules[std::make_pair(dim, star2)]   = star2;
    wavy_arrow_rules[std::make_pair(dim, box2)]    = box2;
    wavy_arrow_rules[std::make_pair(star, dim)]    = star; // or dim?
    wavy_arrow_rules[std::make_pair(star2, dim)]   = star2; // or dim?
    wavy_arrow_rules[std::make_pair(box, dim)]     = box; // or dim?
    
    /* primitive operators */
    
    
    
    
    std::cout << "constructed world at " << this << std::endl;
}

World::~World() {
    std::cout << "deleting world at " << this << std::endl;
    for (auto& e : expressions_) {
        std::cout << "expr at " << e << std::endl;
        delete e;
    }
    for (auto& e : duplicates_) {
        std::cout << "dup expr at " << e << std::endl;
        delete e;
    }
}


/*
 * factory methods
 */

Lambda World::lambda(Def var_type, std::string name) {
    assert(var_type->is_closed() && "type of lambda variable is an unclosed term");
    size_t g = gid_;
    gid_ += 2; // world knows that Abs creates Var
    return cse(new LambdaNode(*this, g, var_type, name));
}

Pi World::pi(Def var_type, std::string name) {
    assert(var_type->is_closed() && "type of pi variable is an unclosed term");
    size_t g = gid_;
    gid_ += 2; // world knows that Abs creates Var
    return cse(new PiNode(*this, g, var_type, name));
}

Def World::app(Def fun, Def arg) {
    return cse_base(new AppNode(*this, gid_++, fun, arg, "app_"));
}

PrimLit World::literal(int value) { 
    return cse(new PrimLitNode(*this, gid_++, get_prim_const("Int"), value, "someint"));
}

Def World::tuple(ArrayRef<Def> elems) {
    if (elems.size() == 1)
        return elems[0];
    return cse_base(new TupleNode(*this, gid_++, elems.size(), "tuple", elems));
}

Pi World::fun_type(Def from, Def to) {
    auto npi = pi(from, "_");
    npi->close(to); // upon closing, cse should be fired automatically
    return npi; // so there's no need to call cse again
}

Def World::extract(Def def, size_t i) {
    if (auto tuple = def.isa<Tuple>())
        return app(tuple, projection(tuple->size(), i));
    else if (i > 0) {
        std::ostringstream msg;
        msg << "trying to extract " << i << "th element out of non-tuple value";
        return bottom(msg.str());
    } else
        return def;
}

Dim World::dimension(int n) {
    return cse(new DimNode(*this, gid_++, n));
}

Proj World::projection(int n, int m) {
    return cse(new ProjNode(*this, gid_++, n, m));
}

Bottom World::bottom(std::string info) {
    return cse(new BottomNode(*this, gid_++, info));
}
 
/*
 * Cleanup
 */

template<class T>
void World::unlink_and_unregister(T& exprs) {
    for (auto def : exprs) {
        if (!def->live_) {
            def->unregister_uses();
            def->unlink_representative();
        }
    }
}

template<class T>
void World::delete_garbage(T& exprs) {
    std::list<typename T::iterator> exprs_garbage;
    for (auto i = exprs.begin(); i != exprs.end(); ++i) {
        if (!((*i)->live_)) {
            exprs_garbage.push_back(i);
            delete *i;
        }
    }
    
    for (auto i : exprs_garbage)
        exprs.erase(i);
}

void World::cleanup() {
    std::queue<const DefNode*> queue;
    
    auto enqueue = [&] (const DefNode* def) {
        if (!def->live_) {
            def->live_ = true;
            queue.push(def);
        }
    };
    
    for (auto def : expressions_)
        def->live_ = false;
    for (auto def : duplicates_)
        def->live_ = false;
    for (auto kv : prim_consts)
        kv.second->live_ = true;
    for (auto edef : externals_) {
        edef->live_ = true;
        queue.push(edef);
    }
    
    while (!queue.empty()) {
        auto def = pop(queue);
        if (def->is_proxy()) {
            enqueue(def->representative_);
        } else {
            if (def->type_)
                enqueue(def->type_);
            
            if (auto v = def->isa<VarNode>()) {
                enqueue(v->type());
            } else for (auto op : def->ops_) {
                enqueue(op);
            }
        }
    }
    
    unlink_and_unregister(expressions_);
    unlink_and_unregister(duplicates_);
    
    delete_garbage(expressions_);
    delete_garbage(duplicates_);
}

/*
 * Putting expressions inside World
 */

const DefNode* World::cse_base(const DefNode* def) {
    def->update_non_reduced_repr();
    
    if (!def->is_closed())
        return def;
    
    auto type = def->typecheck();
    def->type_ = type;
    
    Def proxdef(def);
    proxdef->reduce();
    auto rdef = *proxdef;
    if (def != rdef) {
        rdef->type_ = type;
        delete def;
    }
    
    def = rdef;
    
    auto i = expressions_.find(def);
    if (i != expressions_.end() && *i != def) {
        delete def;
        def = *i;
    } else if (i != expressions_.end()) {
        // reduced already resides in expressions_
    } else {
        auto p = expressions_.insert(def);
        assert(p.second);
    }
    
    def->type_ = type; // unnecessary?
    return def;
}

void World::introduce(const DefNode* def)  {
    def->update_non_reduced_repr();
    
    auto type = def->typecheck();
    def->type_ = type;
    
    auto j = expressions_.find(def);
    if (j != expressions_.end()) {
        def->set_representative(*j);
        duplicates_.insert(def);
    } else {
        auto p = expressions_.insert(def);
        assert(p.second);
    }
}

/*
 * dump
 */

void World::dump(std::ostream& stream) const {
    stream << "expressions_:" << std::endl;
    for (auto e : expressions_) {
        Def(e).dump(stream);
        stream << " at " << e << std::endl;
    }
    stream << "duplicates_:" << std::endl;
    for (auto e : duplicates_) {
        Def(e).dump(stream);
        stream << " at " << e << std::endl;
    }
    stream << "externals: " << std::endl;
    for (auto e : externals_) {
        Def(e).dump(stream);
        stream << " at " << e << std::endl;
    }
}

void World::dump_prims(std::ostream& stream) const {
    stream << "prim consts: \n";
    for (auto& p : prim_consts) {
        stream << p.first << " at (" << p.second << ") : ";
        p.second->type().dump(stream); 
        stream << std::endl;
    } 
}

}
