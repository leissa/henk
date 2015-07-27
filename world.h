#ifndef HENK_WORLD_H
#define HENK_WORLD_H

#include <iostream>
#include <sstream>
#include <unordered_set>

#include "hash.h"
#include "henk.h"

namespace henk {

class NameSupply {
public:
    NameSupply()
        : name('a')
        , number(0)
    {}
    
    std::string get() {
        std::ostringstream s;
        if(number == 0) {
            if(name < 'z') {
                s << name;
                name++;
                return s.str();
            }
            else {
                name = 'a';
                number++;
                return "z";
            }
        }
        else {
            if(name < 'z') {
                s << name << number;
                name++;
                return s.str();
            }
            else {
                s << "a" << number;
                name = 'a';
                number++;
                return s.str();
            }
        }
    }
private:
    char name;
    int number;
};

class World {
public:
    std::map<std::string, const PrimConst*> prim_consts;
    std::map<const PrimConst*, const PrimConst*> prim_rules_has_type;
    std::map<std::pair<const PrimConst*, const PrimConst*>, const PrimConst*> wavy_arrow_rules;
    
    World()
        : prim_consts {
          //  std::make_pair("Term", new PrimConst("Term")),
          //  std::make_pair("MonoType", new PrimConst("MonoType")),
        //    std::make_pair("PolyType", new PrimConst("PolyType")),
            std::make_pair("*", new PrimConst("*")),
            std::make_pair("**", new PrimConst("**")),
            std::make_pair("⬜", new PrimConst("⬜")),
            std::make_pair("⬜⬜", new PrimConst("⬜⬜")),
            std::make_pair("Int", new PrimConst("Int")),
            std::make_pair("Bool", new PrimConst("Bool"))
        }
        , prim_rules_has_type {
           // std::make_pair(prim_consts.at("MonoType"), prim_consts.at("*")),
         //   std::make_pair(prim_consts.at("PolyType"), prim_consts.at("**")),
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
        , name_supply_(new NameSupply())
    {}
    
/*
 * Factory methods
 */     
    const Const* mk_const(const Expr* type, std::string name) { return cse(new Const(type, std::move(name))); }
    const Lam* mk_lam(std::string var_name, const Expr* var_type) { return cse(new Lam(std::move(var_name), var_type)); }
    const Pi* mk_pi(std::string var_name, const Expr* var_type){ return cse(new Pi(std::move(var_name), var_type)); }
    const VarOcc* mk_varOcc(const Body* introduced_by) { return new VarOcc(introduced_by); }
    const App* mk_app(const Expr* appl, const Expr* arg) { return cse(new App(appl, arg)); }
    const IntValueConst* mk_int(int value) { return cse(new IntValueConst(value)); }
    const BoolValueConst* mk_bool(bool value) { return cse(new BoolValueConst(value)); }
    const Body* close_body(/*const*/ Body* expr, const Expr* body) {
        // probably not the best way to do that
       // auto res = expressions_.erase(expr);
     /*   if(res) {
            std::cout << "deleted" << std::endl;
        }
        else {
            std::cout << "not deleted" << std::endl;
        }*/
        expr->close(body);
        return cse(expr);
    }
    
    // sugar
    const Pi* mk_function_type(const Expr* from, const Expr* to) { return cse(new Pi(to, "_", from)); }

/*
 * Utility methods
 */ 
    static const Expr* substitute(const Expr* expr, const VarIntr* var, const Expr* nval);

    static bool is_a_subexpression(const Expr* expr, const Expr* sub);

    static const Expr* to_whnf(const Expr* expr);
    static bool are_expressions_equal(const Expr* expr1, const Expr* expr2);
    const Expr* typecheck(const Expr* expr);
    
    void show_prims(std::ostream& stream) const;
    static void sdump(const Expr* expr) { sdump(expr, std::cout); }
    static void sdump(const Expr* expr, std::ostream& stream);
    void dump(const Expr* expr, std::ostream& stream) const;
    void dump(const Expr* expr) const { dump(expr, std::cout); }
    void show_expressions(std::ostream& stream) const;
    void show_expressions() const { show_expressions(std::cout); }

    size_t gid() const { return gid_; }
    
private:
    struct ExprHash { size_t operator () (const Expr* e) const { return e->hash(); } };
    struct ExprEqual { bool operator () (const Expr* e1, const Expr* e2) const { return are_expressions_equal(e1, e2); } };

    const Expr* cse_base(const Expr* expr);
    template<class T> const T* cse(const T* expr) { return cse_base(expr)->template as<T>(); }

    NameSupply* name_supply_;
    void dump_body(const Body* body, std::ostream& stream) const;
    static void sdump_body(const Body* body, std::ostream& stream) ;
    HashSet<const Expr*, ExprHash, ExprEqual> expressions_;
    size_t gid_; // global id for expressions
};

}

#endif
