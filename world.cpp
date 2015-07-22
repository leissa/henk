#include <iostream>
#include <sstream>

#include "world.h"
#include "henk.h"

namespace henk {
    
const Const* World::mk_const(const Expr* type, std::string name) { 
    auto nconst = new Const(type, std::move(name));
    auto hashed = expressions_.find(nconst);
    if(hashed != expressions_.end()) {
        delete nconst;
        return (*hashed)->as<Const>();
    }
    else {
        expressions_.insert(nconst);
        return nconst;
    }
}

const Lam* World::mk_lam(std::string var_name, const Expr* var_type) {
    auto nlam = new Lam(std::move(var_name), var_type);
    auto hashed = expressions_.find(nlam);
    if(hashed != expressions_.end()) {
        delete nlam;
        return (*hashed)->as<Lam>();
    }
    else {
        expressions_.insert(nlam);
        return nlam;
    }
}

const Pi* World::mk_pi(std::string var_name, const Expr* var_type) {
    auto npi = new Pi(std::move(var_name), var_type);
    auto hashed = expressions_.find(npi);
    if(hashed != expressions_.end()) {
        delete npi;
        return (*hashed)->as<Pi>();
    }
    else {
        expressions_.insert(npi);
        return npi;
    }
}

const App* World::mk_app(const Expr* appl, const Expr* arg) { 
    auto napp = new App(appl, arg);
    auto hashed = expressions_.find(napp);
    if(hashed != expressions_.end()) {
        delete napp;
        return (*hashed)->as<App>();
    }
    else {
        expressions_.insert(napp);
        return napp;
    }
}

const IntValueConst* World::mk_int(int value) {
    auto nval = new IntValueConst(value);
    auto hashed = expressions_.find(nval);
    if(hashed != expressions_.end()) {
        delete nval;
        return (*hashed)->as<IntValueConst>();
    }
    else {
        expressions_.insert(nval);
        return nval;
    }
}

const BoolValueConst* World::mk_bool(bool value) {
    auto nval = new BoolValueConst(value);
    auto hashed = expressions_.find(nval);
    if(hashed != expressions_.end()) {
        delete nval;
        return (*hashed)->as<BoolValueConst>();
    }
    else {
        expressions_.insert(nval);
        return nval;
    }
}

const Pi* World::mk_function_type(const Expr* from, const Expr* to) {
    auto npi = new Pi(to, "_", from);
    auto hashed = expressions_.find(npi);
    if(hashed != expressions_.end()) {
        delete npi;
        return (*hashed)->as<Pi>();
    }
    else {
        expressions_.insert(npi);
        return npi;
    }
}

const Expr* World::substitute(const Expr* expr, const VarIntr* var, const Expr* nval) {
    if(expr == var)
        return nval;
    if(auto var_occ = expr->isa<VarOcc>()) {
        if(var_occ->introduced_by()->var() == var) {
            return nval;
        }
    }
    else if(auto lam = expr->isa<Lam>()) {
        if(lam->var() == var) {
            return expr;
        }
        else {
            auto nlam = new Lam(lam->var()->name(), lam->var()->type());
            auto nbody = substitute(lam->body(), lam->var(), new VarOcc(nlam));
            auto body_substituted = substitute(nbody, var, nval);
            nlam->close(body_substituted);
            return nlam;
        }
    } // ugly copy-paste -- is it possible to coalesce this code?
    else if(auto pi = expr->isa<Pi>()) {
        if(pi->var() == var) {
            return expr;
        }
        else {
            auto npi = new Pi(pi->var()->name(), pi->var()->type());
            auto nbody = substitute(pi->body(), pi->var(), new VarOcc(npi));
            auto body_substituted = substitute(nbody, var, nval);
            npi->close(body_substituted);
            return npi;
        }
        
    }
    else if(auto app = expr->isa<App>()) {
        auto napply = substitute(app->apply(), var, nval);
        auto narg = substitute(app->arg(), var, nval);
        return new App(napply, narg);
    }
    else
        return expr;
}

bool World::is_a_subexpression(const Expr* expr, const Expr* sub) const {
    if(expr == sub) {
        return true;
    }
    else if(auto ann_expr = expr->isa<AnnotatedExpr>()) {
        return is_a_subexpression(ann_expr->type(), sub);
    }
    else if(auto var_occ = expr->isa<VarOcc>()) {
        return is_a_subexpression(var_occ->introduced_by()->var(), sub);
    }
    else if(auto body = expr->isa<Body>()) {
        return is_a_subexpression(body->var(), sub) ||
            is_a_subexpression(body->body(), sub);
    }
    else if(auto app = expr->isa<App>()) {
        return is_a_subexpression(app->apply(), sub) ||
            is_a_subexpression(app->arg(), sub);
    }
    else 
        return false;
}

const Expr* World::to_whnf(const Expr* expr) {
    if(expr == nullptr) {
        throw std::runtime_error("nullptr has no weak head normal form");
    }
    else if(auto app = expr->isa<App>()) {
        auto f = to_whnf(app->apply());
        if(auto flam = f->isa<Lam>()) {
            return to_whnf(
                substitute(flam->body(), flam->var(), app->arg())
            );
        }
        else
            throw std::runtime_error("app of non lambda");
    }
    else
        return expr;
}

bool World::are_expressions_equal(const Expr* expr1, const Expr* expr2) {
    auto e1 = to_whnf(expr1);
    auto e2 = to_whnf(expr2);
    // neither e1 nor e2 can be applications now

    if(auto int1 = e1->isa<IntValueConst>()) {
        if(auto int2 = e2->isa<IntValueConst>()) {
            return int1->value() == int2->value();
        }
    }
    else if(auto bool1 = e1->isa<BoolValueConst>()) {
        if(auto bool2 = e2->isa<BoolValueConst>()) {
            return bool1->value() == bool2->value();
        }
    }
    else if(auto prim1 = e1->isa<PrimConst>()) {
        if(auto prim2 = e2->isa<PrimConst>()) {
            return prim1 == prim2;
        }
    }
    else if(auto ann1 = e1->isa<AnnotatedExpr>()) {
        if(auto ann2 = e2->isa<AnnotatedExpr>()) {
            return are_expressions_equal(ann1->type(), ann2->type());
        }
    }
    else if(auto varo1 = e1->isa<VarOcc>()) {
        if(auto varo2 = e2->isa<VarOcc>()) {
            return are_expressions_equal(varo1->introduced_by(),
                varo2->introduced_by());
        }
    }
    else if(auto lam1 = e1->isa<Lam>()) {
        if(auto lam2 = e2->isa<Lam>()) {
            return are_expressions_equal(lam1->var(), lam2->var()) &&
                are_expressions_equal(lam1->body(), lam2->body());
        }
    }
    else if(auto pi1 = e1->isa<Pi>()) {
        if(auto pi2 = e2->isa<Pi>()) {
            return are_expressions_equal(pi1->var(), pi2->var()) &&
                are_expressions_equal(pi1->body(), pi2->body());
        }
    }
    return false;
}

const Expr* World::typecheck(const Expr* expr) {
    if(expr == nullptr) {
        throw std::runtime_error("typechecking nullptr");
    }
    else if(auto int_value = expr->isa<IntValueConst>()) {
        return prim_consts.at("Int");
    }
    else if(auto bool_value = expr->isa<BoolValueConst>()) {
        return prim_consts.at("Bool");
    }
    else if(auto prim_const = expr->isa<PrimConst>()) {
        auto p = prim_rules_has_type.find(prim_const);
        if(p != prim_rules_has_type.end()) {
            return p->second;
        }
        else {
            std::ostringstream msg;
            msg << "prim constant " << prim_const->name() << " has no type";
            throw std::runtime_error(msg.str());
        }
    }
    else if(auto ann_expr = expr->isa<AnnotatedExpr>()) {
        auto type_has_type = typecheck(ann_expr->type());
        return ann_expr->type();
    }
    else if(auto var_occ = expr->isa<VarOcc>()) {
        return typecheck(var_occ->introduced_by()->var());
    }
    else if(auto lam = expr->isa<Lam>()) {
        auto var_type = typecheck(lam->var());
        auto body_type = typecheck(lam->body());
        auto res = new Pi(lam->var()->name(), var_type);
        auto body_type2 = substitute(body_type, lam->var(), new VarOcc(res));
        res->close(body_type2);
        auto type_of_pi = typecheck(res);
        return res;
    }
    else if(auto pi = expr->isa<Pi>()) {
        auto var_type = typecheck(pi->var());
        auto var_type_type = to_whnf(typecheck(var_type));
        auto body_type = to_whnf(typecheck(pi->body()));
        auto p = wavy_arrow_rules.find(std::make_pair(
            var_type_type->as<PrimConst>(),
            body_type->as<PrimConst>())
        );
        if(p != wavy_arrow_rules.end()) {
            return p->second;
        }
        else {
            std::ostringstream msg;
            msg << "no wavy arrow rule for " << var_type_type->as<PrimConst>()->name();
            msg << " ⤳ " << body_type->as<PrimConst>()->name();
            throw std::runtime_error(msg.str());
        }
    }
    else if(auto app = expr->isa<App>()) {
        auto app_type = typecheck(to_whnf(app->apply()));
        if(auto app_type2 = app_type->isa<Pi>()) {
            auto arg_type = typecheck(app->arg());
            if(are_expressions_equal(arg_type, app_type2->var()->type())) {
                return substitute(app_type2->body(), app_type2->var(), app->arg());
            }
            else {
                std::ostringstream msg;
                msg << "argument types don't match in application: ";
                dump(arg_type, msg);
                msg << " (" << arg_type << ")";
                msg << " and ";
                dump(app_type2->var()->type(), msg);
                msg << " (" << app_type2->var()->type() << ")";
                throw std::runtime_error(msg.str());
            }
        }
        else {
            std::ostringstream msg;
            msg << "application of non-lambda expression: ";
            dump(app_type, msg);
            throw std::runtime_error(msg.str());
        }
    }
    else {
        std::ostringstream msg;
        msg << "malformed expression: ";
        dump(expr, msg);
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
        stream << p.first->name() << "(" << p.first << ") : ";
        stream << p.second->name() << "(" << p.second << ")" << std::endl;
    }  
}

void World::dump(const Expr* expr, std::ostream& stream) const {
    if(expr == nullptr) {
        throw std::runtime_error("dumping nullptr");
    }
    else if(auto int_value = expr->isa<IntValueConst>()) {
        stream << int_value->value();
    }
    else if(auto bool_value = expr->isa<BoolValueConst>()) {
        stream << bool_value->value() ? "true" : "false";
    }
    else if(auto prim_const = expr->isa<PrimConst>()) {
        stream << prim_const->name();
    }
    else if(auto ann_expr = expr->isa<AnnotatedExpr>()) {
        stream << ann_expr->name();
    }
    else if(auto var_occ = expr->isa<VarOcc>()) {
        dump(var_occ->introduced_by()->var(), stream);
    }
    else if(auto lam = expr->isa<Lam>()) {
        stream << "λ";
        dump_body(lam, stream);
    }
    else if(auto pi = expr->isa<Pi>()) {
         if(pi->var()->name() == "_" ||
            !is_a_subexpression(pi->body(), pi->var())
            ) {
            stream << "(";
            dump(pi->var()->type(), stream);
            stream << ") -> (";
            dump(pi->body(), stream);
            stream << ")";
        }
        else if(pi->var()->type() == prim_consts.at("*")) {
            stream << "∀" << pi->var()->name() << ". ";
            dump(pi->body(), stream);
        }
        else {
            stream << "Π";
            dump_body(pi, stream);
        }  
    }
    else if(auto app = expr->isa<App>()) {
        stream << "(";
        dump(app->apply(), stream);
        stream << ") (";
        dump(app->arg(), stream);
        stream << ")";
    }
}

void World::dump_body(const Body* body, std::ostream& stream) const {
    dump(body->var(), stream);
    std::cout << ":";
    dump(body->var()->type(), stream);
    std::cout << ". ";
    dump(body->body(), stream);
}

}

