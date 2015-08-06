#include <iostream>
#include <sstream>

#include "world.h"
#include "henk.h"

namespace henk {

World::World()
        : prim_consts {
            std::make_pair("*", new PrimConst("*")),
            std::make_pair("**", new PrimConst("**")),
            std::make_pair("⬜", new PrimConst("⬜")),
            std::make_pair("⬜⬜", new PrimConst("⬜⬜")),
            std::make_pair("Int", new PrimConst("Int")),
            std::make_pair("Bool", new PrimConst("Bool"))
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
    {
        prim_consts_boxes = std::list<Expression*>();
        // seems stupid but it's because we will never want to free primitives
        for(auto& kv : prim_consts) {
            prim_consts_boxes.push_back(new Expression(kv.second, this));
        }
    }

void World::removeExpr(const Expr* expr) {
        auto i = expressions_.find(expr);
     /*   bool is_a_prim = false;
        for(auto& kv : prim_consts) {
            if(kv.second == expr) {
                is_a_prim = true;
                break;
            }
        }*/
        if(i != expressions_.end() /*&& !is_a_prim*/) {
            std::cout << "deleting expr at " << *i << std::endl;
            expressions_.erase(i);
            delete expr;
            std::cout << "deleted succesfully" << std::endl;
        }
        else {
            throw std::runtime_error("attempt to free expr outside world");
        }
    }

Expression World::close_body(Expression abstraction, Expression body) {
    auto abs = (*abstraction)->as<Body>();
    std::ostringstream nvarn;
    nvarn << abs->var()->name() << "'";
    auto nabs = new Lam(nvarn.str(), abs->var()->type());
    nabs->close(substitute(*body, abs->var(), new VarOcc(nabs)));
    return cse(nabs);
}

/*
bool World::replace(Expression olde, Expression newe) {
    (*olde)->set_representative(*newe);
}*/

void World::show_expressions(std::ostream& stream) const {
    for(auto e : expressions_) {
        dump(e);
        std::cout << " at " << e << std::endl;
    }
}

const Expr* World::cse_base(const Expr* expr) {
    auto i = expressions_.find(expr);
    if (i != expressions_.end() && *i != expr) {
        std::cout << "in cse_base, now deleting " << expr << std::endl;
        delete expr;
        expr = *i;
    } else {
        expr->set_gid(gid_++);
        auto p = expressions_.insert(expr);
    }
    
    return expr;
}

const Expr* World::substitute(const Expr* expr, const VarIntr* var, const Expr* nval) {
    if(expr == var) {
        return nval;
    }
    if(auto var_occ = expr->isa<VarOcc>()) {
        if(var_occ->introduced_by()->var() == var) {
            return nval;
        }
        else {
            return expr;
        }
    }
    else if(auto lam = expr->isa<Lam>()) {
        if(lam->var() == var) {
            return expr;
        }
        else {
            std::ostringstream nvarn;
            nvarn << lam->var()->name() << "'";
            auto ntype = substitute(lam->var()->type(), var, nval);
            auto nlam = new Lam(nvarn.str(), ntype);
            std::cout << "in subst, created new lam at " << nlam << std::endl;
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
            std::ostringstream nvarn;
            nvarn << pi->var()->name() << "'";
            auto ntype = substitute(pi->var()->type(), var, nval);
            auto npi = new Pi(nvarn.str(), ntype);
            std::cout << "in subst, created new pi at " << npi << std::endl;
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
    else if(auto anne = expr->isa<AnnotatedExpr>()) {
        // should there be a substitution on anne->type() or not?
        return expr;
    }
    else {
        return expr;
    }
}

bool World::is_a_subexpression(const Expr* expr, const Expr* sub) {
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
        return expr;
        //throw std::runtime_error("nullptr has no weak head normal form");
        // but may be useful?
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
    if(e1 == e2)
        return true;
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
            return varo1->introduced_by() == varo2->introduced_by();
        }
    }
    else if(auto lam1 = e1->isa<Lam>()) {
        if(auto lam2 = e2->isa<Lam>()) {
            return are_expressions_equal(lam1->var()->type(), lam2->var()->type())
                && are_expressions_equal(lam1->body(),
                substitute(lam2->body(), lam2->var(), new VarOcc(lam1)));
        }
    }
    else if(auto pi1 = e1->isa<Pi>()) {
        if(auto pi2 = e2->isa<Pi>()) {
            return are_expressions_equal(pi1->var()->type(), pi2->var()->type())
                && are_expressions_equal(pi1->body(),
                substitute(pi2->body(), pi2->var(), new VarOcc(pi1)));
        }
    }
    else
        return false;
}

Expression World::typecheck(Expression e) {
    auto expr = *e;
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
                msg << " (" << arg_type << ")";
                msg << " and ";
                msg << " (" << app_type2->var()->type() << ")";
                throw std::runtime_error(msg.str());
            }
        }
        else {
            std::ostringstream msg;
            msg << "application of non-lambda expression: ";
            throw std::runtime_error(msg.str());
        }
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
        stream << p.first->name() << "(" << p.first << ") : ";
        stream << p.second->name() << "(" << p.second << ")" << std::endl;
    }  
}

void World::dump(const Expr* expr, std::ostream& stream) const {
    if(expr == nullptr) {
        stream << "'nullptr'";
    }
    if(auto int_value = expr->isa<IntValueConst>()) {
        stream << int_value->value();
    }
    if(auto bool_value = expr->isa<BoolValueConst>()) {
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

/*
void World::sdump(const Expr* expr, std::ostream& stream)  {
    if(expr == nullptr) {
        stream << "'nullptr'";
    }
    
    if(auto int_value = expr->isa<IntValueConst>()) {
        stream << int_value->value();
    }
    
    if(auto bool_value = expr->isa<BoolValueConst>()) {
        stream << bool_value->value() ? "true" : "false";
    }
    else if(auto prim_const = expr->isa<PrimConst>()) {
        stream << prim_const->name();
    }
    else if(auto ann_expr = expr->isa<AnnotatedExpr>()) {
        stream << ann_expr->name();
    }
    else if(auto var_occ = expr->isa<VarOcc>()) {
        sdump(var_occ->introduced_by()->var(), stream);
    }
    else if(auto lam = expr->isa<Lam>()) {
        stream << "λ";
        sdump_body(lam, stream);
    }
    else if(auto pi = expr->isa<Pi>()) {
         if(pi->var()->name() == "_" ||
            !is_a_subexpression(pi->body(), pi->var())
            ) {
            stream << "(";
            sdump(pi->var()->type(), stream);
            stream << ") -> (";
            sdump(pi->body(), stream);
            stream << ")";
        }
        else {
            stream << "Π";
            sdump_body(pi, stream);
        }  
    }
    else if(auto app = expr->isa<App>()) {
        stream << "(";
        sdump(app->apply(), stream);
        stream << ") (";
        sdump(app->arg(), stream);
        stream << ")";
    }
}

void World::sdump_body(const Body* body, std::ostream& stream)  {
    sdump(body->var(), stream);
    std::cout << ":";
    sdump(body->var()->type(), stream);
    std::cout << ". ";
    sdump(body->body(), stream);
}
* */

void World::dump_body(const Body* body, std::ostream& stream) const {
    dump(body->var(), stream);
    std::cout << ":";
    dump(body->var()->type(), stream);
    std::cout << ". ";
    dump(body->body(), stream);
}

Expression World::mk_const(Expression type, std::string name) { return Expression(cse(new Const(*type, std::move(name))), this); }
Expression World::mk_lam(std::string var_name, Expression var_type) { return Expression(cse(new Lam(std::move(var_name), *var_type)), this); }
Expression World::mk_pi(std::string var_name, Expression var_type){ return Expression(cse(new Pi(std::move(var_name), *var_type)), this); }
Expression World::mk_varOcc(Expression introduced_by) { return Expression(new VarOcc((*introduced_by)->as<Body>()), this); }
Expression World::mk_app(Expression appl, Expression arg) { return Expression(cse(new App(*appl, *arg)), this); }
Expression World::mk_int(int value) { return Expression(cse(new IntValueConst(value)), this); }
Expression World::mk_bool(bool value) { return Expression(cse(new BoolValueConst(value)), this); }
Expression World::mk_function_type(Expression from, Expression to) { return Expression(cse(new Pi(to, "_", from)), this); }

const Expr* Expression::deref() const {
    if(expr_ == nullptr)
        return nullptr;
    
    // TODO
    return expr_;
}

}

