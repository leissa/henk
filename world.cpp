#include "world.h"

#include <sstream>

namespace henk {

World::World()
    : gid_(0)
{
  /*  prim_consts_boxes_ = std::list<Def>();
    for (auto& kv : prim_consts) {
        prim_consts_boxes_.push_back(kv.second);
    }*/
    
    auto botbox = new BottomNode(*this, gid_++, "⬜ doesn't have a type");
    auto box = new VarNode(*this, gid_++, botbox, nullptr, "⬜");
    auto star = new VarNode(*this, gid_++, box, nullptr, "*");
    auto pint = new VarNode(*this, gid_++, star, nullptr, "Int");
    auto pbool = new VarNode(*this, gid_++, star, nullptr, "Bool");
    auto botbox2 = new BottomNode(*this, gid_++, "⬜⬜ doesn't have a type");
    auto box2 = new VarNode(*this, gid_++, botbox2, nullptr, "⬜⬜");
    auto star2 = new VarNode(*this, gid_++, box2, nullptr, "**");
    
    expressions_.insert(botbox); expressions_.insert(box);
    expressions_.insert(star); expressions_.insert(pint);
    expressions_.insert(pbool); expressions_.insert(botbox2);
    expressions_.insert(box2); expressions_.insert(star2);
    
    prim_consts["*"] = star;    prim_consts["**"] = star2;
    prim_consts["⬜"] = box;     prim_consts["⬜⬜"] = box2;
    prim_consts["Int"] = pint;  prim_consts["Bool"] = pbool;
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


/*
 * Utility methods -- sorted alphabetically
 */

const DefNode* World::cse_base(const DefNode* def) {
    if (!def->is_closed()) {
   //     std::cout << "in cse: putting unclosed def: ";
   //     dump(def);
   //     std::cout << "\n to garbage" << std::endl;
    //    garbage_.insert(def);
        //def->set_gid(gid_++);
        return def;
    }
    
    auto type = typecheck(def);
    def->inftype_ = type;
    
    Def proxdef(def);
    reduce(proxdef);
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
       // assert(*i != def);
        delete def;
        def = *i;
    } else if (i != expressions_.end()) {
      //  std::cout << "cse: reduced def already in expressions_ (physically): ";
     //   Def(def).dump();
      //  std::cout << " at " << def;
    //    std::cout << std::endl;
       // def->set_gid(gid_++);
      //  expressions_.insert(def);
    } else {
      //  std::cout << "cse: brand new def: ";
      //  Def(def).dump();
      //  std::cout << " at " << def << std::endl;
        expressions_.insert(def);
    }
    
    def->inftype_ = type;
    return def;
}

void World::introduce(const DefNode* def)  {
    
    auto type = typecheck(def);
    def->inftype_ = type;
    
    auto j = expressions_.find(def);
    if (j != expressions_.end())
        def->set_representative(*j);
        
    expressions_.insert(def);
}

void World::reduce(Def def)  { // should we allow non-closed exprs?
    assert(def->is_closed() && "unclosed def in reduce");

    auto node = *def;
    Def2Def map;
    node->set_representative(*reduce(def, map));
}

Def World::reduce_bot_dont_replace(Def def, Def oldd, Def newd) { // acts as substitution
    Def2Def map;
    map[*oldd] = *newd;
    return reduce(def, map);
}

void World::reduce(Def def, Def oldd, Def newd)  { // acts as substitution
    auto node = *def;
    Def2Def map;
    map[*oldd] = *newd;
    node->set_representative(*reduce(def, map));
}

// TODO make this a virtual function in DefNode
Def World::reduce(Def def, Def2Def& map)  {
   // std::cout << "reducing " << std::endl;
  //  dump(def);
  //  std::cout << std::endl;
    if (auto var = def->isa<VarNode>()) {
        auto i = map.find(var);
        if (i != map.end()) {
            return i->second;
        } else {
            return def;
        }
    } else if (auto abs = def->isa<AbsNode>()) { // TODO make sure we cannot fall into infinite loop
        // if body is already reduced and we create a new unnecessary abstraction
        auto i = map.find(*(abs->var()));
        if (i != map.end()) { // TODO looks broken to me // FIXED?
            map.erase(i);
            //return def;
        }
        std::ostringstream nvarn;
        nvarn << abs->var()->name();
        if (nvarn.str() != "_")
            nvarn << "'";
        auto ntype = reduce(abs->var().as<Var>()->type(), map);
        Abs nabs;
        if(abs->isa<LambdaNode>())
            nabs = lambda(nvarn.str(), ntype);
        else
            nabs = pi(nvarn.str(), ntype);
        // = lambda(nvarn.str(), ntype);
    //    std::cout << "in reduce, created new abs: ";
   //     dump(nabs);
 //       std::cout << std::endl;
        map[*(abs->var())] = *(nabs->var());
        auto nbody = reduce(abs->body(), map);
  //      std::cout << "and reduced its body to: ";
 //       dump(nbody);
 //       std::cout << std::endl;
        nabs->close(nbody);
        return nabs;
    } else if (auto appd = def.isa<App>()) {
        Def rfun = reduce(appd->fun(), map);
        Def rarg = reduce(appd->arg(), map);
        if (auto abs = rfun.isa<Abs>()) {
            map[*(abs->var())] = *rarg;
            return reduce(abs->body(), map);
        } else {
            // throw std::runtime_error("app of non-abs found in reduce");
            // it can happen that fun is not a lambda -- consider λx.λy.x y
            // FIXME infinite loop if fun and arg are already reduced
            // POTENTIAL FIX is_reduced_ flag ?
            if(*rfun != *(appd->fun()) || *rarg != *(appd->arg()))
                return app(rfun, rarg);
            else
                return def;
        }
    } else
        throw std::runtime_error("malformed def in reduce");
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
// assumption: subexpressions of def have already been typechecked
Def World::typecheck(Def def) { // def may or may not be reduced!
    assert(def->is_closed() && "typechecking non closed expr");
    
   // auto i = prim_rules_has_type.find(def);
  //  if (i != prim_rules_has_type.end())
   //     return i->second;

   /* for (auto& kv : prim_consts) {
        if (kv.second == def) {
            if (i == prim_rules_has_type.end()) {
                std::ostringstream msg;
                msg << "typechecking " << kv.second->name() << "  shouldn't happen" << std::endl;
                throw std::runtime_error(msg.str());
            }
        }
    }*/
    if (auto bot = def.isa<Bottom>()) {
        return bot;
    }
    if (auto int_value = def.isa<PrimLit>()) {
        return get_prim_const("Int");
    } else if (auto var = def.isa<Var>()) {
        //return var->inftype();
        return var->type(); // ?
    } else if (auto lambda = def.isa<Lambda>()) {
        auto body_type = lambda->body()->inftype();
        std::ostringstream nvarn;
        nvarn << lambda->var()->name();
        if (nvarn.str() != "_")
            nvarn << "'";
        auto res = pi(nvarn.str(), lambda->var()->inftype());
        auto body_type2 = reduce_bot_dont_replace(body_type, lambda->var(), res->var());
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
            throw std::runtime_error(msg.str());
        }
    } else if (auto appl = def.isa<App>()) {
        auto funt = appl->fun()->inftype().as<Pi>();
        auto argt = appl->arg()->inftype();
       // assert(funt->isa<PiNode> && "typecheck: application of non lambda");
        if(funt->var()->inftype() == argt) {
            auto res = reduce_bot_dont_replace(funt->body(), funt->var(), appl->arg());
            return res;
            
        } else
            throw std::runtime_error("typecheck: in app type of arg is wrong");
        
    } else {
        std::ostringstream msg;
        msg << "malformed expression: ";
        throw std::runtime_error(msg.str());
    }
}


#if 0 // LEGACY TYPECHECK
Def World::typecheck(Def def) { // def may or may not be reduced!
    assert(def->is_closed() && "typechecking non closed expr");
    
  //  std::cout << "typechecking: ";
  //  dump(def);
  //  std::cout << std::endl;
    
    auto i = prim_rules_has_type.find(def);
    if (i != prim_rules_has_type.end())
        return i->second;

    for (auto& kv : prim_consts) {
        if (kv.second == def) {
            if (i == prim_rules_has_type.end()) {
                std::ostringstream msg;
                msg << "typechecking " << kv.second->name() << "  shouldn't happen" << std::endl;
                throw std::runtime_error(msg.str());
            }
        }
    }
    if (auto int_value = def.isa<PrimLit>()) {
        return get_prim_const("Int");
    } else if (auto var = def.isa<Var>()) { // will probably typecheck such things many times
        // so maybe some caching? keepin inferred type in every DefNode after typechecking?
        
        // probably not use that line -- what if var is a star? then we typecheck box...
        //auto type_type = typecheck(var->type());
        
        //return var->type();
        return reduce(var->type());
    } else if (auto lambda = def.isa<Lambda>()) {
        // do we need to typecheck var?
        //auto var_type = typecheck_(lambda->var());
        auto body_type = typecheck(lambda->body());
        std::ostringstream nvarn;
        nvarn << lambda->var()->name();
        if (nvarn.str() != "_")
            nvarn << "'";
        auto res = pi(nvarn.str(), lambda->var()->type());//var_type);
    //            std::cout << "during typechecking lambda: ";
  //      dump(lambda);
  //      std::cout << "\nnclosed res type is: ";
  //      dump(res);
  //      std::cout << "\nand its unreduced body isgvvcvcc: ";
       // dump(body_type);
  //      std::cout << std::endl;
        auto body_type2 = reduce_bot_dont_replace(body_type, lambda->var(), res->var());
  //  std::cout << " AFTER REDCTIONJ" << std::endl;
       // auto res = pi_share_var(lambda->var());
      //  auto body_type2 = substitute(body_type, lambda->var(), varocc);//new VarOcc(res));
        res->close(body_type2);
        typecheck(res);
        return res;
    } else if (auto pi = def.isa<Pi>()) {
        auto var_type = typecheck(pi->var());
        auto var_type_type = typecheck(var_type);
      /*  std::cout << "type of ";
        dump(var_type);
        std::cout << "  is ";
        dump(var_type_type);
        std::cout << std::endl;*/
        auto body_type = typecheck(pi->body());
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
            throw std::runtime_error(msg.str());
        }
    } else if (auto appl = def.isa<App>()) {
        //throw std::runtime_error("bumped into app in typechecker");
        auto funt = typecheck(appl->fun());
        auto argt = typecheck(appl->arg());
        assert(funt->isa<PiNode> && "typecheck: application of non lambda");
        if(funt->var()->type() == argt) {
            auto funtnode = funt->as<PiNode>();
            auto res = reduce_bot_dont_replace(funtnode->body(), funtnode->var(), appl->arg());
            
        } else
            throw std::runtime_error("typecheck: in app type of arg is wrong");
        
    } else {
        std::ostringstream msg;
        msg << "malformed expression: ";
        throw std::runtime_error(msg.str());
    }
}
#endif

}

