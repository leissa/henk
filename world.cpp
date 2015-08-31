#include "world.h"

#include <list>
#include "thorin/util/queue.h"

namespace henk {

World::World()
    : gid_(0)
{
    auto botbox = new BottomNode(*this, gid_++, "⬜ doesn't have a type");
    auto box = new VarNode(*this, gid_++, botbox, nullptr, "⬜");
    auto star = new VarNode(*this, gid_++, box, nullptr, "*");
    auto pint = new VarNode(*this, gid_++, star, nullptr, "Int");
    auto pbool = new VarNode(*this, gid_++, star, nullptr, "Bool");
    auto botbox2 = new BottomNode(*this, gid_++, "⬜⬜ doesn't have a type");
    auto box2 = new VarNode(*this, gid_++, botbox2, nullptr, "⬜⬜");
    auto star2 = new VarNode(*this, gid_++, box2, nullptr, "**");
    
    botbox->update_non_reduced_repr();
    box->update_non_reduced_repr();
    star->update_non_reduced_repr();
    pint->update_non_reduced_repr();
    pbool->update_non_reduced_repr();
    botbox2->update_non_reduced_repr();
    box2->update_non_reduced_repr();
    star2->update_non_reduced_repr();
    
    expressions_.insert(botbox); expressions_.insert(box);
    expressions_.insert(star);   expressions_.insert(pint);
    expressions_.insert(pbool);  expressions_.insert(botbox2);
    expressions_.insert(box2);   expressions_.insert(star2);
    
    prim_consts["*"] = star;     prim_consts["**"] = star2;
    prim_consts["⬜"] = box;      prim_consts["⬜⬜"] = box2;
    prim_consts["Int"] = pint;   prim_consts["Bool"] = pbool;
    prim_consts["⊥ ⬜"] = botbox; prim_consts["⊥ ⬜⬜"] = botbox2;
    
    wavy_arrow_rules[std::make_pair(star, star)]   = star;
    wavy_arrow_rules[std::make_pair(star, star2)]  = star2;
    wavy_arrow_rules[std::make_pair(star2, star)]  = star2;
    wavy_arrow_rules[std::make_pair(star2, star2)] = star2;
    wavy_arrow_rules[std::make_pair(box, star)]    = star2;
    wavy_arrow_rules[std::make_pair(box, star2)]   = star2;
    wavy_arrow_rules[std::make_pair(box, box)]     = box;
    
    std::cout << "constructed world at " << this << std::endl;
}

World::~World() {
    std::cout << "deleting world at " << this << std::endl;
    for (auto& e : expressions_) {
        std::cout << "expr at " << e << std::endl;
        delete e;
    }
}


/*
 * Factory methods
 */

Lambda World::lambda(std::string name, Def var_type) {
    assert(var_type->is_closed() && "type of lambda variable is an unclosed term");
    size_t g = gid_;
    gid_ += 2; // world knows that Abs creates Var
    return cse(new LambdaNode(*this, g, var_type, name));
}

Pi World::pi(std::string name, Def var_type) {
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

Pi World::fun_type(Def from, Def to) {
    auto npi = pi("_", from);
    npi->close(to); // upon closing, cse should be fired automatically
    return npi; // so there's no need to call cse again
}

Bottom World::bottom(std::string info) {
    return cse(new BottomNode(*this, gid_++, info));
}


/*
 * Utility methods -- sorted alphabetically
 */

void World::cleanup() {
    std::queue<const DefNode*> queue;
    
    for (auto def : expressions_) {
        def->live_ = false;
    }
    
    for (auto edef : externals_) {
        edef->live_ = true;
        queue.push(edef);
    }
    
    while (!queue.empty()) {
        auto def = pop(queue);
        if (!def->live_) {
            def->live_ = true;
            if (auto v = def->isa<VarNode>()) {
                queue.push(v->type());
            }
            else for (auto op : def->ops_) {
                queue.push(op);
            }
        }
    }
    
    //std::queue<decltype(expressions_)::iterator> garbage; // why doesn't it work? :(
    std::list<thorin::HashSet<const DefNode*, ExprHash, ExprEqual>::iterator> garbage;
    for (auto i = expressions_.begin(); i != expressions_.end(); ++i) {
        if(!((*i)->live_)) {
            garbage.push_back(i);
            delete *i;
        }
    }
    for (auto i : garbage)
        expressions_.erase(i);
}

const DefNode* World::cse_base(const DefNode* def) {
    
    def->update_non_reduced_repr();
    
    if (!def->is_closed()) {
   //     std::cout << "in cse: putting unclosed def: ";
   //     dump(def);
        return def;
    }
    
    auto type = def->typecheck();
    def->inftype_ = type;
    
    Def proxdef(def);
    proxdef->reduce();
    auto rdef = *proxdef;
    if(def != rdef)
        delete def;
    
    def = rdef;
    
    auto i = expressions_.find(def);
  //  assert(i != expressions_.end() && "in cse reduced def is outside world");
    if (i != expressions_.end() && *i != def) {
        // here probably we want to do gid_-- or gid_-=2 depending on whether
        // def is Abs or not (or do nothing if gids don't need to be continuous)
      //  std::cout << "cse: found duplicate and deleteing: ";
      //  Def(def).dump();
     //   std::cout << std::endl;
        delete def;
        def = *i;
    } else if (i != expressions_.end()) {
      //  std::cout << "cse: reduced def already in expressions_ (physically): ";
     //   Def(def).dump();
      //  std::cout << " at " << def;
    //    std::cout << std::endl;
    } else {
      //  std::cout << "cse: brand new def: ";
      //  Def(def).dump();
      //  std::cout << " at " << def << std::endl;
        expressions_.insert(def);
    }
    
    def->inftype_ = type; // unnecessary?
    return def;
}

void World::introduce(const DefNode* def)  {
    
    def->update_non_reduced_repr();
    
    auto type = def->typecheck();
    def->inftype_ = type;
    
    auto j = expressions_.find(def);
    if (j != expressions_.end())
        def->set_representative(*j);
        
    expressions_.insert(def);
}

void World::show_expressions(std::ostream& stream) const {
    for (auto e : expressions_) {
        Def(e).dump(stream);
        stream << " at " << e << std::endl;
    }
    stream << "externals: " << std::endl;
    for (auto e : externals_) {
        Def(e).dump(stream);
        stream << " at " << e << std::endl;
    }
}

void World::show_prims(std::ostream& stream) const {
    stream << "prim consts: \n";
    for (auto& p : prim_consts) {
        stream << p.first << " at (" << p.second << ") : ";
        p.second->inftype().dump(stream); 
        stream << std::endl;
    } 
}

}

