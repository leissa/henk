#include "world.h"

#include <sstream>

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
    std::cout << "deleting expressions_..." << std::endl;
    for (auto& e : expressions_) {
        std::cout << "at " << e << std::endl;
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

const DefNode* World::cse_base(const DefNode* def) {
    
    def->update_non_reduced_repr();
    
    if (!def->is_closed()) {
   //     std::cout << "in cse: putting unclosed def: ";
   //     dump(def);
        return def;
    }
    
    auto type = typecheck(def);
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
    
    auto type = typecheck(def);
    def->inftype_ = type;
    
    auto j = expressions_.find(def);
    if (j != expressions_.end())
        def->set_representative(*j);
        
    expressions_.insert(def);
}

// TODO make this a method of DefNode
void World::replace(Def olde, Def newe) const {
    (*olde)->set_representative(*newe);
}

void World::show_expressions(std::ostream& stream) const {
    for (auto e : expressions_) {
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

// TODO make this a virtual function in DefNode
// invariant: result of typecheck is reduced expression
Def World::typecheck(Def def) { // def may or may not be reduced
    assert(def->is_closed() && "typechecking non closed expr");
    
    if (auto bot = def.isa<Bottom>()) {
        return bot;
    }
    else if (auto int_value = def.isa<PrimLit>()) {
        return get_prim_const("Int");
    } else if (auto var = def.isa<Var>()) {
        return var->type();
    } else if (auto lambda = def.isa<Lambda>()) {
        auto body_type = lambda->body()->inftype();
        std::ostringstream nvarn;
        nvarn << lambda->var()->name();
        if (nvarn.str() != "_")
            nvarn << "'";
        auto res = pi(nvarn.str(), lambda->var()->inftype());
        auto body_type2 = body_type->reduce_but_dont_replace(
            lambda->var(), res->var()
        );
        res->close(body_type2);
       // typecheck(res);
        return res;
    } else if (auto pi = def.isa<Pi>()) {
        auto var_type = pi->var()->inftype();
        auto var_type_type = var_type->inftype();
        auto body_type = pi->body()->inftype();
        auto p = wavy_arrow_rules.find(std::make_pair(
            *var_type_type,
            *body_type)
        );
        if (p != wavy_arrow_rules.end()) {
            return p->second;
        }
        else {
            std::ostringstream msg;
            msg << "no wavy arrow rule for " << var_type_type->name();
            msg << " ⤳  " << body_type->name();
            return bottom(msg.str());
        }
    } else if (auto appl = def.isa<App>()) {
        auto funt = appl->fun()->inftype();
        auto argt = appl->arg()->inftype();
        if(auto pifunt = funt.isa<Pi>()) {
            if(pifunt->var()->inftype() == argt) {
                return pifunt->body()->reduce_but_dont_replace(
                    pifunt->var(), appl->arg()
                );
            } else {
                std::ostringstream msg;
                msg << "in application: (";
                appl->fun().dump(msg); msg << ") ("; appl->arg().dump(msg);
                msg << ") -- type of argument (";
                argt.dump(msg); msg << ") != type of fun's var (";
                pifunt->var()->inftype().dump(msg);
                msg << ")";
                return bottom(msg.str());
            }
        } else {
            std::ostringstream msg;
            msg << "in application: (";
            appl->fun().dump(msg); msg << ") ("; appl->arg().dump(msg);
            msg << ") -- type of fun is not Pi, but: ";
            funt.dump(msg);
            return bottom(msg.str());
        }
    } else {
        std::ostringstream msg;
        msg << "malformed expression in typecheck: ";
        throw std::runtime_error(msg.str());
    }
}

}

