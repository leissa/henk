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
    Def app(Def appl, Def arg);
    PrimLit literal(int value);
    
protected:
    Bottom bottom(std::string info);

public:
/*
 * Utility methods
 */ 
    void reduce(Def) ;
    Def typecheck(Def def);
    bool is_a_subexpression(Def expr, Def sub) const;
    void show_prims(std::ostream& stream) const;
    void show_expressions(std::ostream& stream) const;
    void show_expressions() const { show_expressions(std::cout); }
    Def get_prim_const(std::string s) const { return prim_consts.at(s); }
    size_t gid() const { return gid_; }

protected:
    Def typecheck_(Def def);
    Def substitute(Def expr, Def var, Def nval);
    void reduce(Def def, Def oldd, Def newd);
    Def reduce_bot_dont_replace(Def def, Def oldd, Def newd);
    Def reduce(Def, Def2Def&) ;
    void replace(Def olde, Def newe) const;
    void introduce(const DefNode* def) ;
    template<class T> const T* cse(const T* def) { return cse_base(def)->template as<T>(); }

protected:
    const DefNode* cse_base(const DefNode*) ;

    struct ExprHash { size_t operator () (const DefNode* e) const { return e->hash(); } };
    struct ExprEqual {
        bool operator () (const DefNode* def1, const DefNode* def2) const {
            assert(&def1->world() == &def2->world_ && "testing for eq defs from different worlds");
           // assert(def1->is_reduced() && def2->is_reduced() && "some def not reduced in ExprEqual");
            return def1->equal(*def2);
        } 
    };
    
    mutable size_t gid_; // global id for expressions
    std::map<std::string, const DefNode*> prim_consts;
    std::map<std::pair<const DefNode*, const DefNode*>, const DefNode*> wavy_arrow_rules;
    mutable thorin::HashSet<const DefNode*, ExprHash, ExprEqual> expressions_;
    
    friend class AbsNode; // AbsNode uses introduce(const DefNode*)
    friend class DefNode; // DefNode uses reduce(Def)
};

}

#endif
