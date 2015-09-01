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
    for (auto& e : duplicates_) {
        std::cout << "dup expr at " << e << std::endl;
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
    
    auto enqueue = [&] (const DefNode* def) {
        if(!def->live_) {
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
        if(def->is_proxy()) {
            enqueue(def->representative_);
        } else {
            if(def->inftype_)
                enqueue(def->inftype_);
            
            if (auto v = def->isa<VarNode>()) {
                enqueue(v->type());
            } else for (auto op : def->ops_) {
                enqueue(op);
            }
        }
    }
    
    for (auto def : expressions_) {
        if(!def->live_) {
            def->unregister_uses();
            def->unlink_representative();
        }
    }
    for (auto def : duplicates_) {
        if(!def->live_) {
            def->unregister_uses();
            std::cout << def << " has repr " << def->representative_ << std::endl;
            def->unlink_representative();
            std::cout << "after unlink it has " << def->representative_ << std::endl;
        }
    }
    
    std::cout << "in cleanup, garbage is:" << std::endl;
    //std::queue<decltype(expressions_)::iterator> garbage; // why doesn't it work? :(
    std::list<thorin::HashSet<const DefNode*, ExprHash, ExprEqual>::iterator> exprs_garbage;
    for (auto i = expressions_.begin(); i != expressions_.end(); ++i) {
        if(!((*i)->live_)) {
            Def(*i).dump();
            std::cout << " at " << *i << std::endl;
            exprs_garbage.push_back(i);
            delete *i;
        }
    }
    
    for (auto i : exprs_garbage)
        expressions_.erase(i);
    
    std::cout << "and duplicates_ garbage:" << std::endl;
    std::list<DefSet::iterator> dups_garbage;
    for (auto i = duplicates_.begin(); i != duplicates_.end(); ++i) {
        std::cout << " at " << *i << " : " << std::endl;
        if(!((*i)->live_)) {
            assert((*i)->representative_ == *i);
            Def(*i).dump();
            std::cout << " at " << *i << std::endl;
            dups_garbage.push_back(i);
            delete *i;
        }
    }
    
    for (auto i : dups_garbage)
        duplicates_.erase(i);
    
    std::cout << "done cleaning" << std::endl;
}

const DefNode* World::cse_base(const DefNode* def) {
    std::cout << "cse: def is ";
    Def(def).dump();
    std::cout << " at " << def << std::endl;
    def->update_non_reduced_repr();
    
    if (!def->is_closed()) {
        std::cout << "in cse: putting unclosed def: ";
        Def(def).dump();
        std::cout << " at " << def << std::endl;
        return def;
    }
    
    auto type = def->typecheck();
    def->inftype_ = type;
    
    Def proxdef(def);
    proxdef->reduce();
    std::cout << "after reduction node_= " << proxdef.node_ << std::endl;
    auto rdef = *proxdef;
    std::cout << "and repr at " << rdef << std::endl;
    if(def != rdef) {
        rdef->inftype_ = type;
        delete def;
    }
    
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
        auto p = expressions_.insert(def);
        assert(p.second);
    }
    
    def->inftype_ = type; // unnecessary?
    return def;
}

void World::introduce(const DefNode* def)  {
    
    def->update_non_reduced_repr();
    
    std::cout << "introduce ";
    Def(def).dump();
    std::cout << " at " << def << std::endl;
    
    auto type = def->typecheck();
    def->inftype_ = type;
    
    auto j = expressions_.find(def);
    if (j != expressions_.end()) {
        
        std::cout << "introduce ";
        Def(def).dump();
        std::cout << " found duplicate and setting repr to: ";
        std::cout << *j << std::endl;
        def->set_representative(*j);
        duplicates_.insert(def);
    } else {
        std::cout << "introduce: put to expressions_" << std::endl;
        auto p = expressions_.insert(def);
        assert(p.second);
    }
}

void World::show_expressions(std::ostream& stream) const {
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

void World::show_prims(std::ostream& stream) const {
    stream << "prim consts: \n";
    for (auto& p : prim_consts) {
        stream << p.first << " at (" << p.second << ") : ";
        p.second->inftype().dump(stream); 
        stream << std::endl;
    } 
}

}

