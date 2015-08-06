#ifndef HENK_WORLD_H
#define HENK_WORLD_H

#include <iostream>
#include <sstream>
#include <unordered_set>
#include <list>

#include "hash.h"
#include "henk.h"

namespace henk {

class Expression;

//struct ExprHash;
struct ExprEqual;

class World {
public:
    friend class Expression;

    std::map<std::string, const PrimConst*> prim_consts;
    std::map<const PrimConst*, const PrimConst*> prim_rules_has_type;
    std::map<std::pair<const PrimConst*, const PrimConst*>, const PrimConst*> wavy_arrow_rules;
    
    World();
    /*    : prim_consts {
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
        prim_consts_boxes = std::list<Expression>();
        // seems stupid but it's because we will never want to free primitives
        for(auto& kv : prim_consts) {
            prim_consts_boxes.push_back(Expression(kv.second, this));
        }
    }*/
    
/*
 * Factory methods
 */     
    Expression mk_const(Expression type, std::string name);
    Expression mk_lam(std::string var_name, Expression var_type);
    Expression mk_pi(std::string var_name, Expression var_type);
    Expression mk_varOcc(Expression introduced_by);
    Expression mk_app(Expression appl, Expression arg);
    Expression mk_int(int value);
    Expression mk_bool(bool value);
    Expression close_body(Expression abstraction, Expression body);
    
    // sugar
    Expression mk_function_type(Expression from, Expression to);

/*
 * Utility methods
 */ 
    static const Expr* substitute(const Expr* expr, const VarIntr* var, const Expr* nval);

    static bool is_a_subexpression(const Expr* expr, const Expr* sub);

    static const Expr* to_whnf(const Expr* expr);
    static bool are_expressions_equal(const Expr* expr1, const Expr* expr2);
    Expression typecheck(Expression expr);
    
    void show_prims(std::ostream& stream) const;
    static void sdump(const Expr* expr) { sdump(expr, std::cout); }
    static void sdump(const Expr* expr, std::ostream& stream);
    void dump(const Expr* expr, std::ostream& stream) const;
    void dump(const Expr* expr) const { dump(expr, std::cout); }
    void show_expressions(std::ostream& stream) const;
    void show_expressions() const { show_expressions(std::cout); }

    size_t gid() const { return gid_; }
    
private:
    std::list<Expression*> prim_consts_boxes;

    struct ExprHash { size_t operator () (const Expr* e) const { return e->hash(); } };
    struct ExprEqual { 
        bool operator () (const Expr* e1, const Expr* e2) const { return are_expressions_equal(e1, e2); } 
    };

    const Expr* cse_base(const Expr* expr);
    template<class T> const T* cse(const T* expr) { return cse_base(expr)->template as<T>(); }
   // bool replace(Expression olde, Expression newe);

    void dump_body(const Body* body, std::ostream& stream) const;
    static void sdump_body(const Body* body, std::ostream& stream) ;
    HashSet<const Expr*, ExprHash, ExprEqual> expressions_;
    size_t gid_; // global id for expressions
    
    void removeExpr(const Expr* expr);
    void removeExprs(std::unordered_set<const Expr*> exprs) { for(auto e : exprs) removeExpr(e); }
};

struct ExprHash { size_t operator () (const Expr* e) const { return e->hash(); } };
struct ExprEqual { 
    World& world;
    ExprEqual(World& w) : world(w) {}
    bool operator () (const Expr* e1, const Expr* e2) const { return world.are_expressions_equal(e1, e2); } 
};

class Expression {
public:
    Expression(World* world)
        : expr_(nullptr)
        , world_(world)
    {}
    
    Expression(const Expr* expr)
        : expr_(expr)
        , world_(nullptr)
    {   
        expr_->increaseRefCount();
        std::cout << "increased refcount to " << expr_->refCount() << " of expr " << expr_ << std::endl;
         }
    
    Expression(const Expr* expr, World* world)
        : expr_(expr)
        , world_(world)
    { expr_->increaseRefCount();
        std::cout << "increased refcount to " << expr_->refCount() << " of expr " << expr_ << std::endl; }
    
    ~Expression() {
        if(!empty()) {
            expr_->decreaseRefCount();
            std::cout << "decreased refcount to " << expr_->refCount() << " of expr " << expr_ << std::endl;
         //   if(expr_->refCount() == 0) {
                world_->removeExprs(expr_->gatherUnusedCascading());
                //delete expr_;
       //     }
        }
    }
    
    void operator = (const Expression& other) {
        if(this != &other) {
            std::cout << "Expr assignment: inc " << other.expr() << ", dec " << expr_ << std::endl;
            auto oe = other.expr();
            oe->increaseRefCount();
            expr_->decreaseRefCount();
            expr_ = oe;
        }
    }
    
    Expression(const Expression& other) {
        std::cout << "copy construct " << other.expr() << std::endl;
        expr_ = other.expr();
        expr_->increaseRefCount();
    }
    
    Expression(Expression&& other) {
        std::cout << "move construct " << other.expr() << std::endl;
        expr_ = other.expr();
        expr_->increaseRefCount();
    }

    bool empty() const { return expr_ == nullptr; }
    const Expr* expr() const { return expr_; }
    const Expr* deref() const;
    const Expr* operator *() const { return deref(); }
    bool operator == (const Expr* other) const { return this->deref() == other; }
    operator const Expr*() const { return deref(); }
    const Expr* operator -> () const { return deref(); }
    
private:
    mutable const Expr* expr_;
    /*const*/ World* world_;
};

}

#endif
