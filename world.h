#ifndef HENK_WORLD_H
#define HENK_WORLD_H

#include <iostream>
#include <map>
#include <list>

#include "thorin/util/hash.h"

#include "henk.h"

namespace henk {

class World {
public:
    World();
    ~World();
    
/*
 * Factory methods
 */
    Lambda lambda(std::string var_name, Def var_type);
    Pi fun_type(Def from, Def to);
    Pi pi(std::string var_name, Def var_type);
    Pi pi_share_var(Def var);
    App app(Def appl, Def arg);
    PrimLit literal(int value);

/*
 * Utility methods
 */ 
    Def typecheck(Def def);
    bool are_expressions_equal(Def expr1, Def expr2);
    bool is_a_subexpression(Def expr, Def sub) const;
    void show_prims(std::ostream& stream) const;
    void dump(Def expr, std::ostream& stream) const;
    void dump(Def expr) const;
    void show_expressions(std::ostream& stream) const;
    void show_expressions() const { show_expressions(std::cout); }
    Def get_prim_const(std::string s) const { return prim_consts.at(s); }
    size_t gid() const { return gid_; }

protected:
    void dump_body(Abs abs, std::ostream& stream) const;
    Def typecheck_(Def def);
   // Def substitute(Def expr, Def var, Def nval);
    void reduce(Def);
    Def reduce(Def, Def2Def&);
    bool are_expressions_equal_(Def expr1, Def expr2);
    void replace(Def olde, Def newe) const;
    void move_from_garbage(const DefNode* def) const;
    template<class T> const T* cse(const T* def) { return cse_base(def)->template as<T>(); }

protected:
    const DefNode* cse_base(const DefNode*) const;

    struct ExprHash { size_t operator () (const DefNode* e) const { return e->hash(); } };
    struct ExprEqual {
        bool operator () (const DefNode* def1, const DefNode* def2) const {
            assert(&def1->world() == &def2->world_ && "testing for eq defs from different worlds");
            return def1->world().are_expressions_equal(def1, def2);
        } 
    };
    
    mutable size_t gid_; // global id for expressions
    std::map<std::string, const DefNode*> prim_consts;
    std::map<const DefNode*, const DefNode*> prim_rules_has_type;
    std::map<std::pair<const DefNode*, const DefNode*>, const DefNode*> wavy_arrow_rules;
    std::list<Def> prim_consts_boxes_;
    mutable DefSet garbage_;
    mutable thorin::HashSet<const DefNode*, ExprHash, ExprEqual> expressions_;
    
    friend class DefNode; // DefNode uses move_from_garbage(const DefNode*)
};

}

#endif
