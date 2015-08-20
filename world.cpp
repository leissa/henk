#include <iostream>
#include <sstream>

#include "world.h"
#include "henk.h"

namespace henk {

World::World()
    : gid_(0)
    , prim_consts {
        std::make_pair("*", new Var(this, -1, nullptr, nullptr, "*")),
        std::make_pair("**", new Var(this, -1, nullptr, nullptr, "**")),
        std::make_pair("⬜", new Var(this, -1, nullptr, nullptr, "⬜")),
        std::make_pair("⬜⬜", new Var(this, -1, nullptr, nullptr, "⬜⬜")),
        std::make_pair("Int", new Var(this, -1, nullptr, nullptr, "Int")),
        std::make_pair("Bool", new Var(this, -1, nullptr, nullptr, "Bool"))
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
    for(auto& kv : prim_consts) {
        prim_consts_boxes_.push_back(kv.second);
    }
    std::cout << "constructed world at " << this << std::endl;
}

World::~World() {
    std::cout << "deleting expressions_..." << std::endl;
    for(auto& e : expressions_) {
        delete e;
    }
    std::cout << "deleting garbage..." << std::endl;
    for(auto& e: garbage_) {
        std::cout << "at " << e << std::endl;
        delete e;
    }
}

void World::replace(Def olde, Def newe) const {
    (*olde)->set_representative(*newe);
}

void World::show_expressions(std::ostream& stream) const {
    for(auto e : expressions_) {
        dump(e, stream);
        stream << " at " << e << std::endl;
    }
    stream << "garbage:" << std::endl;
    for(auto e : garbage_) {
       // dump(e, stream);
        stream << " at " << e << std::endl;
    }
    stream << "prim consts:" << std::endl;
    for(auto& e: prim_consts_boxes_) {
        //dump(e, stream);
        stream << " at " << *e << std::endl;
    }
}

void World::move_from_garbage(const DefNode* def) const {
    auto i = garbage_.find(def);
    if(i != garbage_.end()) {
        garbage_.erase(i);
        auto j = expressions_.find(def);
        if(j != expressions_.end()) {
            def->set_representative(*j);
        }
        else {
            expressions_.insert(def);
        }
    }
    else
        throw std::runtime_error("move_from_garbage doesn't work");
}

Def World::cse(Def e) const {
    if(!e.is_closed()) {
        garbage_.insert(*e);
        //e->set_gid(gid_++);
        return e;
    }
    std::cout << "in cse_base, given box at " << &e << " and expr at " << *e << std::endl;
    auto expr = *e;
    auto i = expressions_.find(expr);
    if (i != expressions_.end() && *i != expr) {
        // here probably we want to do gid_-- or gid_-=2 depending on whether
        // expr is Abs or not
        delete expr;
        expr = *i;
    } else {
       // expr->set_gid(gid_++);
        auto p = expressions_.insert(expr);
    }
    
    return expr;
}

#if 0 // legacy code -- might be needed if general substitution turns out to be needed
Def World::substitute(/*const Expr**/Def bexpr, /*const VarIntr**/Def bvar, 
    /*const Expr**/Def bnval) {
    auto expr = *bexpr; auto var = *bvar; auto nval = *bnval;
    if(expr == var) {
        return bnval;
    }
    if(auto varocc = expr->isa<Var>()) {
        if(varocc == var) {
            return bnval;
        }
        else {
            return bexpr;
        }
    }
    else if(auto abs = expr->isa<Abs>()) {
        if(abs->var() == var) {
            return bexpr;
        }
        else {
            std::ostringstream nvarn;
            nvarn << abs->var()->name;
            if(nvarn.str() != "_")
                nvarn << "'";
            auto ntype = substitute(abs->var()->type(), bvar, bnval);
            auto nlam = mk_lam(nvarn.str(), ntype);
            auto nbody = substitute(Expression(lam->body(), this), 
                Expression(lam->var(), this), mk_varOcc_on_without_cse(nlam));
            auto body_substituted = substitute(nbody, bvar, bnval);
            nlam.close(body_substituted);
            return nlam;
        }
    }
    else if(auto app = expr->isa<App>()) {
        auto napply = substitute(Expression(app->apply(), this), bvar, bnval);
        auto narg = substitute(Expression(app->arg(), this), bvar, bnval);
        return mk_app(napply, narg);//new App(napply, narg);
    }
    else if(auto anne = expr->isa<AnnotatedExpr>()) {
        // should there be a substitution on anne->type() or not?
        return bexpr;
    }
    else {
        return bexpr;
    }
}
#endif

bool World::is_a_subexpression(Def bexpr, Def bsub) const {
    assert(!bsub.empty());
    
    if(bexpr.empty())
        return false;
    
    auto expr = *bexpr;
    auto sub = *bsub;
    if(expr == sub) {
        return true;
    }
    else if(auto var = expr->isa<Var>()) {
        return is_a_subexpression(var->type(), bsub);
    }
    else if(auto abs = expr->isa<Abs>()) {
        return is_a_subexpression(abs->var(), bsub) ||
            is_a_subexpression(abs->body(), bsub);
    }
    else if(auto app = expr->isa<App>()) {
        return is_a_subexpression(app->fun(), bsub) ||
            is_a_subexpression(app->arg(), bsub);
    }
    else 
        throw std::runtime_error("in is_a_subexpression malformed bexpr");
}



Def World::reduce(Def e, std::map<const DefNode*, const DefNode*>* M) const {
    auto expr = *e;
    if(auto var = expr->isa<Var>()) {
        auto i = M->find(var);
        if(i != M->end()) {
            return i->second;
        }
        else {
            return e; // expr
        }
    }
    else if(auto abs = expr->isa<Abs>()) {
        auto i = M->find(abs->var());
        if(i != M->end()) {
            return e;
        }
        else {
            std::ostringstream nvarn;
            nvarn << abs->var()->name;
            if(nvarn.str() != "_")
                nvarn << "'";
            auto ntype = reduce(abs->var()->as<Var>()->type(), M);
            auto nabs = mk_lam(nvarn.str(), ntype);
            (*M)[*(abs->var())] = *mk_var_occ(nabs);
            auto nbody = reduce(abs->body(), M);
            nabs.close_abs(nbody);
            return nabs;
        }
    }
    else if(auto app = expr->isa<App>()) {
        auto rfun = reduce(app->fun(), M);
        if(auto abs = (*rfun)->isa<Abs>()) {
            auto rarg = reduce(app->arg(), M);
            (*M)[*(abs->var())] = *rarg;
            return reduce(abs->body(), M);
        }
        else
            throw std::runtime_error("app of non-lambda found in reduce");
    }
}

void World::reduce(Def def) const { // should we allow non-closed exprs?
    auto defn = *def;
    auto M = new std::map<const DefNode*, const DefNode*>();
    defn->set_representative(reduce(def, M));
    delete M;
}

#if 0
void World::to_whnf(/*const Expr**/Def e) const {
    auto expr = *e;
    if(expr == nullptr) {
        return;
        //throw std::runtime_error("nullptr has no weak head normal form");
        // but may be useful?
    }
    else if(auto app = expr->isa<App>()) {
        auto f = app->fun();
        auto ff = *f; // done just to trigger path compression
        to_whnf(f);
        if(auto flam = f->isa<Lam>()) {
            auto nbody = substitute(flam->body(), flam->var(), app->arg());
            to_whnf(nbody);
            expr->set_representative(*nbody);
        }
        else
            throw std::runtime_error("app of non-lambda");
    }
}
#endif

bool World::are_expressions_equal(Def expr1, Def expr2) const {
    reduce(expr1);
    reduce(expr2);
    are_expressions_equal_(expr1, expr2);
}

bool World::are_expressions_equal_(Def expr1, Def expr2) const {
    if(!expr1.is_closed() || !expr2.is_closed())
        throw std::runtime_error("in are_expr_equal one is not closed");
    
    auto e1 = *expr1;
    auto e2 = *expr2;
    if(e1 == e2)
        return true;
    if(auto int1 = e1->isa<PrimLit>()) {
        if(auto int2 = e2->isa<PrimLit>()) {
            return int1->value() == int2->value();
        }
    }
    else if(auto v1 = e1->isa<Var>()) {
        if(auto v2 = e2->isa<Var>()) {
            return v1 == v2 || v1->equiv_ == v2;
        }
    }
    else if(auto abs1 = e1->isa<Abs>()) {
        if(auto abs2 = e2->isa<Abs>()) {
            abs1->var()->equiv_ = abs2;
            auto res = are_expressions_equal_(abs1->var()->as<Var>()->type(), 
                abs2->var()->as<Var>()->type()) &&
                are_expressions_equal_(abs1->body(), abs2->body());
            abs1->var()->equiv_ = nullptr;
            return res;
        }
    }
    else if(auto app1 = e1->isa<App>())
        throw std::runtime_error("bumped into app in are_expr_equal after reducing");
    else if(auto app2 = e2->isa<App>()) // how to merge those branches? "||" doesn't work
        throw std::runtime_error("bumped into app in are_expr_equal after reducing");
    else
        return false;
}

Def World::typecheck(Def e) {
    reduce(e);
    return typecheck_(e);
}

Def World::typecheck_(Def e) { // assumption: e is reduced
    if(!e.is_closed())
        throw std::runtime_error("typechecking non closed expr");
    auto expr = *e;
    if(expr == nullptr) {
        throw std::runtime_error("typechecking nullptr");
    }
    
    auto i = prim_rules_has_type.find(expr);
    if(i != prim_rules_has_type.end()) {
        return i->second;
    }
    for(auto& kv : prim_consts) {
        if(kv.second == expr) {
            if(i == prim_rules_has_type.end()) {
                std::ostringstream msg;
                msg << "typechecking " << kv.second->name << "  shouldn't happen" << std::endl;
                throw std::runtime_error(msg.str());
            }
        }
    }
    if(auto int_value = expr->isa<PrimLit>()) {
        return get_prim_const("Int");
    }
    else if(auto var = expr->isa<Var>()) { // will probably typecheck such things many times
        // so maybe some caching? keepin inferred type in every DefNode after typechecking?
        
        // probably not use that line -- what if var is a star? then we typecheck box...
        //auto type_type = typecheck(var->type());
        
        return var->type();
    }
    else if(auto lam = expr->isa<Lam>()) {
        // do we need to typecheck var?
       // auto var_type = typecheck_(lam->var());
        auto body_type = typecheck_(lam->body());
        
       // auto res = mk_pi(lam->var()->name(), var_type);
        auto res = mk_pi_share_var(lam->var());
        // oh, here we probably really need a real substitution...... ;(
        // but let's risk for now and make lam->var() a shared var between lam and new pi
      //  auto body_type2 = substitute(body_type, lam->var(), varocc);//new VarOcc(res));
        res.close_abs(body_type/*2*/);
        auto type_of_pi = typecheck_(res);
        return res;
    }
    else if(auto pi = expr->isa<Pi>()) {
        auto var_type = typecheck_(pi->var());
        auto var_type_type = typecheck_(var_type);
        auto body_type = typecheck_(pi->body());
        auto p = wavy_arrow_rules.find(std::make_pair(
            *var_type_type,
            *body_type)
        );
        if(p != wavy_arrow_rules.end()) {
            return p->second;
        }
        else {
            std::ostringstream msg;
            msg << "no wavy arrow rule for " << var_type_type->name;
            msg << " ⤳  " << body_type->name;
            throw std::runtime_error(msg.str());
        }
    }
    else if(auto app = expr->isa<App>()) {
        throw std::runtime_error("bumped into app in typechecker");
    }
    else {
        std::ostringstream msg;
        msg << "malformed expression: ";
        throw std::runtime_error(msg.str());
    }
}
    
void World::show_prims(std::ostream& stream) const {
    stream << "prim consts: \n";
    for(auto& p : prim_consts) {
        stream << p.first << " at " << p.second << std::endl;
    }
    std::cout << "prim rules has type: \n";
    for(auto& p : prim_rules_has_type) {
        stream << p.first->name << "(" << p.first << ") : ";
        stream << p.second->name << "(" << p.second << ")" << std::endl;
    }  
}

void World::dump(Def expr) const { dump(expr, std::cout); }

void World::dump(Def e, std::ostream& stream) const {
    auto expr = *e;
    if(expr == nullptr) {
        stream << "'nullptr'";
    }
    else if(auto int_value = expr->isa<PrimLit>()) {
        stream << int_value->value();
    }
    else if(auto var_occ = expr->isa<Var>()) {
        stream << var_occ->name;
    }
    else if(auto lam = expr->isa<Lam>()) {
        stream << "λ";
        dump_body(e, stream);
    }
    else if(auto pi = expr->isa<Pi>()) {
         if(pi->var()->name == "_" ||
            !is_a_subexpression(pi->body(), pi->var())
            ) {
            stream << "(";
            dump(pi->var()->as<Var>()->type(), stream);
            stream << ") -> (";
            dump(pi->body(), stream);
            stream << ")";
        }
        else if(*(pi->var()->as<Var>()->type()) == *(get_prim_const("*"))) {
            stream << "∀" << pi->var()->name << ". ";
            dump(pi->body(), stream);
        }
        else {
            stream << "Π";
            dump_body(e, stream);
        }  
    }
    else if(auto app = expr->isa<App>()) {
        stream << "(";
        dump(app->fun(), stream);
        stream << ") (";
        dump(app->arg(), stream);
        stream << ")";
    }
}

void World::dump_body(Def body, std::ostream& stream) const {
    dump(body.abs_var(), stream);
    stream << ":";
    dump(body.abs_var().var_type(), stream);
    stream << ". ";
    dump(body.abs_body(), stream);
}

Def World::mk_lam(std::string var_name, Def var_type) const {
    assert(var_type.is_closed() && "type of lambda variable is an unclosed term");
    size_t g = gid_;
    gid_ += 2; // world knows that Abs creates Var
    return cse(new Lam(this, g, var_type, var_name));
}

Def World::mk_pi(std::string var_name, Def var_type) const {
    assert(var_type.is_closed() && "type of pi variable is an unclosed term");
    size_t g = gid_;
    gid_ += 2; // world knows that Abs creates Var
    return cse(new Pi(this, g, var_type, var_name));
}

Def World::mk_pi_share_var(Def var) const {
    return cse(new Pi(this, gid_++, var));
}

Def World::mk_var_occ(Def introduced_by) const {
    return introduced_by->as<Abs>()->var();
}

Def World::mk_app(Def fun, Def arg) const {
    return cse(new App(this, gid_++, fun, arg, "app_"));
}

Def World::mk_int(int value) const { 
    return cse(new PrimLit(this, gid_++, get_prim_const("Int"), value, "someint"));
}

Def World::mk_fun_type(Def from, Def to) const {
    auto npi = mk_pi("_", from);
    npi.close_abs(to); // upon closing, cse should be fired automatically
    return npi; // so there's no need to call cse again
}

}

