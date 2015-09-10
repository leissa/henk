#ifndef HENK_WORLD_H
#define HENK_WORLD_H

#include <iostream>
#include <map>

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
    // one tuple <a> will be a, so that's why tuple returns Def and not Tuple
    /*Tuple*/Def tuple(std::vector<Def> components);
    Dim dimension(int n);
    Proj projection(int n, int m);
protected:
    Bottom bottom(std::string info);

public:
/*
 * Utility methods
 */ 
    void cleanup();
    void add_external(Lambda lambda) const { externals_.insert(lambda); }
    void remove_external(Lambda lambda) const { externals_.erase(lambda); }
    void dump_prims(std::ostream& stream) const;
    void dump(std::ostream& stream) const;
    void dump() const { dump(std::cout); }
    Def get_prim_const(std::string s) const { return prim_consts.at(s); }
    size_t gid() const { return gid_; }

protected:
    const DefNode* untuple(const TupleNode* tup);
    void introduce(const DefNode* def) ;
    template<class T> const T* cse(const T* def) { return cse_base(def)->template as<T>(); }
    const DefNode* cse_base(const DefNode*) ;
    
    template<class T>
    void unlink_and_unregister(T& expr);
    template<class T>
    void delete_garbage(T& exprs);
    
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
    mutable thorin::HashSet<const DefNode*, ExprHash, ExprEqual> externals_;
    mutable DefSet duplicates_;
    
    friend void AbsNode::close(Def) const; // uses introduce(DefNode*)
    friend Def PiNode::typecheck() const; // uses wavy_arrow_rules
    friend Def AppNode::typecheck() const; // uses bottom(std::string)
    friend Def TupleNode::typecheck() const; // uses bottom(std::string)
};

}

#endif
