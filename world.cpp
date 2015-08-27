#include "world.h"

#include <sstream>

namespace henk {

World::World()
    : gid_(0)
    , prim_consts {
        std::make_pair("*", new VarNode(*this, -1, nullptr, nullptr, "*")),
        std::make_pair("**", new VarNode(*this, -1, nullptr, nullptr, "**")),
        std::make_pair("⬜", new VarNode(*this, -1, nullptr, nullptr, "⬜")),
        std::make_pair("⬜⬜", new VarNode(*this, -1, nullptr, nullptr, "⬜⬜")),
        std::make_pair("Int", new VarNode(*this, -1, nullptr, nullptr, "Int")),
        std::make_pair("Bool", new VarNode(*this, -1, nullptr, nullptr, "Bool"))
    }
    , prim_rules_has_type {
        std::make_pair(prim_consts.at("*"), prim_consts.at("⬜")),
        std::make_pair(prim_consts.at("**"), prim_consts.at("⬜⬜")),
        std::make_pair(prim_consts.at("Int"), prim_consts.at("*")),
        std::make_pair(prim_consts.at("Bool"), prim_consts.at("*"))
    }
    , wavy_arrow_rules {
        std::make_pair(std::make_pair(prim_consts.at("*"), prim_consts.at("*")), prim_consts.at("*")),
        std::make_pair(std::make_pair(prim_consts.at("*"), prim_consts.at("**")), prim_consts.at("**")),
        std::make_pair(std::make_pair(prim_consts.at("**"), prim_consts.at("*")), prim_consts.at("**")),
        std::make_pair(std::make_pair(prim_consts.at("**"), prim_consts.at("**")), prim_consts.at("**")),
        std::make_pair(std::make_pair(prim_consts.at("⬜"), prim_consts.at("*")), prim_consts.at("**")),
        std::make_pair(std::make_pair(prim_consts.at("⬜"), prim_consts.at("**")), prim_consts.at("**")),
        std::make_pair(std::make_pair(prim_consts.at("⬜"), prim_consts.at("⬜")), prim_consts.at("⬜"))
    }
    , prim_consts_boxes_{}
{
    prim_consts_boxes_ = std::list<Def>();
    for (auto& kv : prim_consts) {
        prim_consts_boxes_.push_back(kv.second);
    }
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

App World::app(Def fun, Def arg) {
    return cse(new AppNode(*this, gid_++, fun, arg, "app_"));
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
    
    Def proxdef(def);
    reduce(proxdef);
    auto rdef = *proxdef;
    if(def != rdef)
        delete def;
    
    def = rdef;
    
    auto i = expressions_.find(def);
    if (i != expressions_.end() && *i != def) {
        // here probably we want to do gid_-- or gid_-=2 depending on whether
        // def is Abs or not (or do nothing if gids don't need to be continuous)
    //    std::cout << "in cse found duplicate and deleteing: ";
    //    dump(def);
    //    std::cout << std::endl;
        delete def;
        def = *i;
    } else {
     //   std::cout << "in cse bran new def: ";
    //    dump(def);
    //    std::cout << std::endl;
       // def->set_gid(gid_++);
        expressions_.insert(def);
    }
    
  //  reduce(def);
    return def;
}

void World::introduce(const DefNode* def) const {
    auto j = expressions_.find(def);
    if (j != expressions_.end())
        def->set_representative(*j);
    else
        expressions_.insert(def);
}

void World::reduce(Def def)  { // should we allow non-closed exprs?
    assert(def->is_closed() && "unclosed def in reduce");

    auto node = def.node();
    Def2Def map;
    node->set_representative(*reduce(def, map));
}

void World::reduce(Def def, Def oldd, Def newd)  { // acts as substitution
    auto node = def.node();
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
        } //else {
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
 //       std::cout << "WEL" << std::endl;
        return nabs;
        //}
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
    stream << "show expr: " << std::endl;
    for (auto e : expressions_) {
        Def(e).dump(stream);
        stream << " at " << e << std::endl;
    }
  //  stream << "garbage:" << std::endl;
  /*  for (auto e : garbage_) {
        Def(e).dump(stream);
        stream << " at " << e << std::endl;
    }*/
    stream << "prim consts:" << std::endl;
    for (auto& e: prim_consts_boxes_) {
        //dump(e, stream);
        stream << " at " << *e << std::endl;
    }
}

void World::show_prims(std::ostream& stream) const {
    stream << "prim consts: \n";
    for (auto& p : prim_consts) {
        stream << p.first << " at " << p.second << std::endl;
    }
    std::cout << "prim rules has type: \n";
    for (auto& p : prim_rules_has_type) {
        stream << p.first->name() << "(" << p.first << ") : ";
        stream << p.second->name() << "(" << p.second << ")" << std::endl;
    }  
}

Def World::typecheck(Def e) {
    //reduce(e);
    return typecheck_(e);
}

// TODO make this a virtual function in DefNode
Def World::typecheck_(Def def) { // assumption: e is reduced
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
        
        return var->type();
    } else if (auto lambda = def.isa<Lambda>()) {
        // do we need to typecheck var?
        //auto var_type = typecheck_(lambda->var());
        auto body_type = typecheck_(lambda->body());
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
        reduce(body_type, lambda->var(), res->var());
  //  std::cout << " AFTER REDCTIONJ" << std::endl;
       // auto res = pi_share_var(lambda->var());
      //  auto body_type2 = substitute(body_type, lambda->var(), varocc);//new VarOcc(res));
        res->close(body_type/*2*/);
        typecheck_(res);
        return res;
    } else if (auto pi = def.isa<Pi>()) {
        auto var_type = typecheck_(pi->var());
        auto var_type_type = typecheck_(var_type);
      /*  std::cout << "type of ";
        dump(var_type);
        std::cout << "  is ";
        dump(var_type_type);
        std::cout << std::endl;*/
        auto body_type = typecheck_(pi->body());
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
    } else if (auto app = def.isa<App>()) {
        throw std::runtime_error("bumped into app in typechecker");
    } else {
        std::ostringstream msg;
        msg << "malformed expression: ";
        throw std::runtime_error(msg.str());
    }
}

}

