#include <iostream>
#include <sstream>

#include "world.h"
#include "henk.h"

//#define DEBUG_MODE

namespace henk {

const Expr* World::substitute(const Expr* expr, const VarIntr* var, const Expr* nval) {
#ifdef DEBUG_MODE
        std::cout << "sbst val: ";
        dump(nval);
        std::cout << " for var: ";
        dump(var);
        std::cout << " in expr: ";
        dump(expr);
        std::cout << std::endl;
#endif
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
    } // ugly copy-paste -- is it possible to coealesce this code?
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
#ifdef DEBUG_MODE
    std::cout << "is "; dump(sub); std::cout << " at adr= " << sub;
    std::cout << " a subexpr of ";
    dump(expr); std::cout << " at adr= " << expr << "?" << std::endl;
#endif
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
#ifdef DEBUG_MODE
        std::cout << "tpch prim_const: " << std::endl;
        dump(prim_const);
        std::cout << std::endl;
#endif
        auto p = prim_rules_has_type.find(prim_const);
        if(p != prim_rules_has_type.end()) {
            return p->second;
        }
        else {
#ifdef DEBUG_MODE
        std::cout << "prim const notfound rule" << std::endl;
#endif
            std::ostringstream msg;
            msg << "prim constant " << prim_const->name() << " has no type";
            throw std::runtime_error(msg.str());
        }
    }
    else if(auto ann_expr = expr->isa<AnnotatedExpr>()) {
#ifdef DEBUG_MODE
        std::cout << "tpch ann_expr: " << std::endl;
        dump(ann_expr);
        std::cout << std::endl << "will now tpch in ann_expr: ";
        dump(ann_expr->type());
        std::cout << std::endl;
#endif
        auto type_has_type = typecheck(ann_expr->type());
        return ann_expr->type();
    }
    else if(auto var_occ = expr->isa<VarOcc>()) {
#ifdef DEBUG_MODE
        std::cout << "tpch var_occ: " << std::endl;
        dump(var_occ);
        std::cout << std::endl << "will now tpch in var_occ: ";
        dump(var_occ->introduced_by()->var());
        std::cout << std::endl;
#endif
        return typecheck(var_occ->introduced_by()->var());
    }
    else if(auto lam = expr->isa<Lam>()) {
#ifdef DEBUG_MODE
        std::cout << "tpch lam: " << std::endl;
        dump(lam);
        std::cout << std::endl << "will now tpch var type: ";
        dump(lam->var());
        std::cout << std::endl;
#endif
        auto var_type = typecheck(lam->var());
#ifdef DEBUG_MODE
        std::cout << std::endl << "will now tpch body type ";
        dump(lam->body());
        std::cout << std::endl;
#endif
        auto body_type = typecheck(lam->body());
        auto res = new Pi(lam->var()->name(), var_type);
        auto body_type2 = substitute(body_type, lam->var(), new VarOcc(res));
        res->close(body_type2);
#ifdef DEBUG_MODE
        std::cout << std::endl << "will now tpch pi as res " << std::endl;
        std::cout << "a res is " << res << std::endl;
        dump(res);
        std::cout << std::endl;
#endif
        auto type_of_pi = typecheck(res);
        return res;
    }
    else if(auto pi = expr->isa<Pi>()) {
#ifdef DEBUG_MODE
        std::cout << "tpch pi: " << std::endl;
        dump(pi);
        std::cout << std::endl;
#endif
        auto var_type = typecheck(pi->var());
        auto var_type_type = typecheck(var_type);
        auto body_type = typecheck(pi->body());
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
#ifdef DEBUG_MODE
        std::cout << "tpch app: " << std::endl;
        dump(app);
        std::cout << std::endl;
#endif
        auto app_type = typecheck(app->apply());
        if(auto app_type2 = app_type->isa<Pi>()) {
            auto arg_type = typecheck(app->arg());
            return substitute(app_type2->body(), app_type2->var(), app->arg());
        }
        else {
            std::ostringstream msg;
            msg << "application of non-lambda expression: ";
            dump(app_type, msg);
            throw std::runtime_error(msg.str());
        }
    }
    else {
#ifdef DEBUG_MODE
        std::cout << "malformed: " << std::endl;
        dump(expr);
        std::cout << std::endl;
#endif
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
        stream << ann_expr->name(); // << ":";
        //dump(ann_expr->type());
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

