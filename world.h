#ifndef HENK_WORLD_H
#define HENK_WORLD_H

#include <iostream>
#include <sstream>
#include <unordered_set>
#include <list>

#include "hash.h"
#include "henk.h"

namespace henk {

class World {
public:
    World();
    ~World();
    
/*
 * Factory methods
 */
    Def mk_lam(std::string var_name, Def var_type) const;
    Def mk_pi(std::string var_name, Def var_type) const;
    Def mk_pi_share_var(Def var) const;
    Def mk_var_occ(Def introduced_by) const;
    Def mk_app(Def appl, Def arg) const;
    Def mk_int(int value) const;
    Def mk_fun_type(Def from, Def to) const;

/*
 * Utility methods
 */ 
    Def typecheck(Def def);
    Def typecheck_(Def def);
   // Def substitute(Def expr, Def var, Def nval);

    bool is_a_subexpression(Def expr, Def sub) const;
    
    void reduce(Def def) const;
  //  void to_whnf(Def expr);
    Def reduce(Def e, std::map<const DefNode*, const DefNode*>* M) const;
    bool are_expressions_equal(Def expr1, Def expr2) const;
    bool are_expressions_equal_(Def expr1, Def expr2) const;
    
    void show_prims(std::ostream& stream) const;
    void dump(Def expr, std::ostream& stream) const;
    void dump(Def expr) const;
    void show_expressions(std::ostream& stream) const;
    void show_expressions() const { show_expressions(std::cout); }

    size_t gid() const { return gid_; }
    
    void move_from_garbage(const DefNode* def) const;
    std::map<std::string, const DefNode*> prim_consts;
    std::map<const DefNode*, const DefNode*> prim_rules_has_type;
    std::map<std::pair<const DefNode*, const DefNode*>, const DefNode*> wavy_arrow_rules;
    
    Def get_prim_const(std::string s) const {
        return prim_consts.at(s);
    }
    
private:
    std::list<Def> prim_consts_boxes_;
    mutable std::unordered_set<const DefNode*> garbage_;

    struct ExprHash { size_t operator () (const DefNode* e) const { return e->hash(); } };
    struct ExprEqual {
        bool operator () (const DefNode* e1, const DefNode* e2) const {
            if(e1->world_ == e2->world_)
                return e1->world_->are_expressions_equal(e1, e2);
            else
                throw std::runtime_error("testing for eq defs from different worlds");
        } 
    };
    
    Def cse(Def  e) const;
    //template<class T> const T* cse(const T* expr) { return cse_base(expr)->template as<T>(); }
    void replace(Def olde, Def newe) const;

    void dump_body(Def body, std::ostream& stream) const;
    mutable HashSet<const DefNode*, ExprHash, ExprEqual> expressions_;
    mutable size_t gid_; // global id for expressions
    
};



}

#endif
