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
    , garbage_{}
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
    std::cout << "deleting garbage_..." << std::endl;
    for (auto& e: garbage_) {
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

Pi World::pi_share_var(Def var) {
    return cse(new PiNode(*this, gid_++, var));
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
#if 0
bool World::are_expressions_equal(Def expr1, Def expr2) {
    reduce(expr1);
    reduce(expr2);
    return are_expressions_equal_(expr1, expr2);
}

// TODO make this a virtual function in DefNode
bool World::are_expressions_equal_(Def def1, Def def2) {
    assert((def1->is_closed() && def2->is_closed()) && "in are_expr_equal one is not closed");
    
    if (def1 == def2)
        return true;
    if (auto int1 = def1.isa<PrimLit>()) {
        if (auto int2 = def2.isa<PrimLit>())
            return int1->value() == int2->value();
    } else if (auto v1 = def1.isa<Var>()) {
        if (auto v2 = def2.isa<Var>())
            return v1 == v2 || v1->equiv_ == v2;
    } else if (auto abs1 = def1.isa<Abs>()) {
        if (auto abs2 = def2.isa<Abs>()) {
            abs1->var()->equiv_ = abs2;
            auto res = are_expressions_equal_(abs1->var()->as<VarNode>()->type(), 
                abs2->var()->as<VarNode>()->type()) &&
                are_expressions_equal_(abs1->body(), abs2->body());
            abs1->var()->equiv_ = nullptr;
            return res;
        }
    } else if (auto app1 = def1.isa<App>())
        throw std::runtime_error("bumped into app in are_expr_equal after reducing");
    else if (auto app2 = def2.isa<App>()) // how to merge those branches? "||" doesn't work
        throw std::runtime_error("bumped into app in are_expr_equal after reducing");

    return false;
}
#endif
const DefNode* World::cse_base(const DefNode* def) const {
    if (!def->is_closed()) {
        std::cout << "in cse: putting unclosed def: ";
        dump(def);
        std::cout << "\n to garbage" << std::endl;
        garbage_.insert(def);
        //def->set_gid(gid_++);
        return def;
    }
    auto i = expressions_.find(def);
    if (i != expressions_.end() && *i != def) {
        // here probably we want to do gid_-- or gid_-=2 depending on whether
        // def is Abs or not (or do nothing if gids don't need to be continuous)
        std::cout << "in cse found duplicate and deleteing: ";
        dump(def);
        std::cout << std::endl;
        delete def;
        def = *i;
    } else {
        std::cout << "in cse bran new def: ";
        dump(def);
        std::cout << std::endl;
       // def->set_gid(gid_++);
        expressions_.insert(def);
    }
    
  //  reduce(def);
    return def;
}

void World::dump(Def expr) const { dump(expr, std::cout); }

void World::dump(Def def, std::ostream& stream) const {
    if (!def) {
        stream << "'nullptr'";
    } else if (auto int_value = def.isa<PrimLit>()) {
        stream << int_value->value();
    } else if (auto var = def.isa<Var>()) {
        stream << var->name();
    } else if (auto lambda = def.isa<Lambda>()) {
        stream << "λ";
        dump_body(lambda, stream);
    } else if (auto pi = def.isa<Pi>()) {
        if(!pi->body()) {
            stream << "Π";
            dump_body(pi, stream);
        }
        else if (pi->var()->name() == "_" || !pi->body()->has_subexpr(pi->var())) {
            stream << "(";
            dump(pi->var().as<Var>()->type(), stream);
            stream << ") -> (";
            dump(pi->body(), stream);
            stream << ")";
        } else if (*(pi->var().as<Var>()->type()) == *(get_prim_const("*"))) {
            stream << "∀" << pi->var()->name() << ". ";
            dump(pi->body(), stream);
        } else {
            stream << "Π";
            dump_body(pi, stream);
        }  
    } else if (auto app = def.isa<App>()) {
        stream << "(";
        dump(app->fun(), stream);
        stream << ") (";
        dump(app->arg(), stream);
        stream << ")";
    }
}

void World::dump_body(Abs abs, std::ostream& stream) const {
    dump(abs->var(), stream);
    stream << ":";
    dump(abs->var()->type(), stream);
    stream << ". ";
    dump(abs->body(), stream);
}

void World::move_from_garbage(const DefNode* def) const {
    assert(def->is_closed() && "attemp to move unclosed def from garbage");
    auto i = garbage_.find(def);
    if (i != garbage_.end()) {
        garbage_.erase(i);
     //   std::cout << "will we find def?" << std::endl;
        auto j = expressions_.find(def);
    //    std::cout << "time has come" << std::endl;
        if (j != expressions_.end()) {
       //     std::cout << "moving from garbage: " << std::endl;
       //     dump(def);
       //     std::cout << "\n and found equivalent in exprs_" << std::endl;
            def->set_representative(*j);
        } else {
      //      std::cout << "moving from garbage: " << std::endl;
      //      dump(def);
      //      std::cout << "\n and it's brand new garbage" << std::endl;
            expressions_.insert(def);
        }
    }
    else
        throw std::runtime_error("move_from_garbage doesn't work");
}

void World::reduce(Def def) { // should we allow non-closed exprs?
    assert(def->is_closed() && "unclosed def in reduce");
    if(!def->is_reduced()) {
        auto node = def.node();
        Def2Def map;
        node->set_representative(reduce(def, map));
    }
}

void World::reduce(Def def, Def oldd, Def newd) {
    auto node = def.node();
    Def2Def map;
    map[*oldd] = *newd;
    node->set_representative(reduce(def, map));
}

// TODO make this a virtual function in DefNode
Def World::reduce(Def def, Def2Def& map) {
  //  std::cout << "reducing ";
  //  dump(def);
  //  std::cout << std::endl;
    if (auto var = def.isa<Var>()) {
        auto i = map.find(var);
        if (i != map.end()) {
            return i->second;
        } else {
            return def;
        }
    } else if (auto abs = def.isa<Abs>()) {
        auto i = map.find(abs->var());
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
  //      std::cout << "in reduce, created new abs: ";
   //     dump(nabs);
 //       std::cout << std::endl;
        map[abs->var()] = nabs->var();
        auto nbody = reduce(abs->body(), map);
  //      std::cout << "and reduced its body to: ";
 //       dump(nbody);
 //       std::cout << std::endl;
        nabs->close(nbody);
 //       std::cout << "WEL" << std::endl;
        return nabs;
        //}
    } else if (auto app = def.isa<App>()) {
        auto rfun = reduce(app->fun(), map);
        if (auto abs = rfun.isa<Abs>()) {
            auto rarg = reduce(app->arg(), map);
            map[*(abs->var())] = *rarg;
            return reduce(abs->body(), map);
        } else
            throw std::runtime_error("app of non-abs found in reduce");
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
        dump(e, stream);
        stream << " at " << e << std::endl;
    }
    stream << "garbage:" << std::endl;
    for (auto e : garbage_) {
        dump(e, stream);
        stream << " at " << e << std::endl;
    }
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

#if 0 // legacy code -- might be needed if general substitution turns out to be needed
Def World::substitute(Def bexpr, Def bvar, Def bnval) {
    auto expr = *bexpr; auto var = *bvar; auto nval = *bnval;
    if (expr == var) {
        return bnval;
    } else if (auto varocc = expr->isa<Var>()) {
        int a;
        if (varocc == var) {
            return bnval;
        } else {
            return bexpr;
        }
    } else if (auto abs = expr->isa<Abs>()) {
        if (abs->var() == var) {
            return bexpr;
        } else {
            std::ostringstream nvarn;
            nvarn << abs->var()->name();
            if (nvarn.str() != "_")
                nvarn << "'";
            auto ntype = substitute(abs->var()->type(), bvar, bnval);
            auto nlambda = lambda(nvarn.str(), ntype);
            auto nbody = substitute(lambda->body(), lambda->var(), nlambda->var());
            auto body_substituted = substitute(nbody, bvar, bnval);
            nlambda.close(body_substituted);
            return nlambda;
        }
    } else if (auto app = expr->isa<App>()) {
        throw std::runtime_exception("App in substitute!");
        auto napply = substitute(app->fun(), bvar, bnval);
        auto narg = substitute(app->arg(), bvar, bnval);
        return app(napply, narg);//new App(napply, narg);
    } else {
        throw std::runtime_exception("malformed expr in substitute");
        //return bexpr;
    }
}
#endif

#if 0 // legacy code -- if strong normalization turns out to be 'too much' then
      // will get back to whnf
void World::to_whnf(/*const Expr**/Def e) const {
    auto expr = *e;
    if (expr == nullptr) {
        return;
        //throw std::runtime_error("nullptr has no weak head normal form");
        // but may be useful?
    } else if (auto app = expr->isa<App>()) {
        auto f = app->fun();
        auto ff = *f; // done just to trigger path compression
        to_whnf(f);
        if (auto flambda = f->isa<Lambda>()) {
            auto nbody = substitute(flambda->body(), flambda->var(), app->arg());
            to_whnf(nbody);
            expr->set_representative(*nbody);
        }
        else
            throw std::runtime_error("app of non-lambdabda");
    }
}
#endif

Def World::typecheck(Def e) {
    reduce(e);
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
        
        auto res = pi(lambda->var()->name(), lambda->var()->type());//var_type);
                std::cout << "during typechecking lambda: ";
        dump(lambda);
        std::cout << "\nnclosed res type is: ";
        dump(res);
        std::cout << "\nand its unreduced body isgvvcvcc: ";
       // dump(body_type);
        std::cout << std::endl;
        reduce(body_type, lambda->var(), res->var());
    std::cout << " AFTER REDCTIONJ" << std::endl;
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

