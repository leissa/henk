#ifndef HENK_WORLD_H
#define HENK_WORLD_H

#include <functional>
#include <iostream>
#include <map>

#include "thorin/util/array.h"
#include "thorin/util/hash.h"

#include "henk.h"

namespace henk {

class World {
public:
    World();
    ~World();
    
    /*
     * factory methods
     */
    Lambda lambda(Def var_type, std::string var_name = "");
    Pi fun_type(Def from, Def to);
    Pi pi(Def var_type, std::string var_name = "");
    Sigma sigma(Def var_type, std::string var_name = "");
    Pair pair(Def first, Def second, Sigma ascribed_type);
    Def first(Pair p);
    Def second(Pair p);
    Def app(Def appl, Def arg);
    PrimLit literal(int value);
    Dim dimension(int n);
    /// 1-tuple "\<a\>" will be "a", so that's why tuple returns @p Def and not @p Tuple.
    Def tuple(thorin::ArrayRef<Def> elems);
    /// @p def is of type @p Def instead of @p Tuple because we can extract from non-tuples due to "<2> = 2".
    Def extract(Def def, size_t i);
    Dummy dummy(Abs abs, Def return_type, bool is_commutative = false, bool is_associative = false);
    AbsRecord abs_record(thorin::ArrayRef<std::pair<std::string, Def> > label2type);
    InstRecord inst_record(thorin::ArrayRef<std::pair<std::string, Def> > label2elem, AbsRecord ascribed_type);
    RecordDim record_dimension(AbsRecord of_record);
    Def inst_record_extract(InstRecord r, Field field);
    Def abs_record_extract(AbsRecord r, Field field);
    Proj projection(int n, int m);
    RecordProj record_projection(Field field);
    Bottom bottom(std::string info);

public:
/*
 * Utility methods
 */ 
    Lambda get_primop(std::string s) const { return prim_ops.at(s); }
    void cleanup();
    void add_external(Lambda lambda) const { externals_.insert(lambda); }
    void remove_external(Lambda lambda) const { externals_.erase(lambda); }
    void dump_prims(std::ostream& stream) const;
    void dump(std::ostream& stream) const;
    void dump() const { dump(std::cout); }
    Def get_prim_const(std::string s) const { return prim_consts.at(s); }
    size_t gid() const { return gid_; }

protected:
    std::pair<bool, Dummy> is_app_of_dummy(Def a) const;
    void introduce(const DefNode* def) ;
    template<class T> const T* cse(const T* def) { return cse_base(def)->template as<T>(); }
    const DefNode* cse_base(const DefNode*) ;
    
    template<class T> static void unlink_and_unregister(T& expr);
    template<class T> static void delete_garbage(T& exprs);
    
    struct ExprHash { size_t operator () (const DefNode* e) const { return e->hash(); } };
    struct ExprEqual {
        bool operator () (const DefNode* def1, const DefNode* def2) const {
            assert(&def1->world() == &def2->world_ && "testing for eq defs from different worlds");
            return def1->equal(*def2);
        } 
    };
    
    mutable size_t gid_; // global id for expressions
    std::map<std::string, const DefNode*> prim_consts;
    std::map<std::string, Lambda > prim_ops;
    std::map<std::pair<const DefNode*, const DefNode*>, const DefNode*> wavy_arrow_rules;
    mutable thorin::HashSet<const DefNode*, ExprHash, ExprEqual> expressions_;
    mutable thorin::HashSet<const DefNode*, ExprHash, ExprEqual> externals_;
    mutable DefSet duplicates_;
    
    friend void AbsNode::close(Def) const; // uses introduce(DefNode*)
    friend Def PiNode::typecheck() const; // uses pi_rules
    friend Def SigmaNode::typecheck() const;
    friend Def PairNode::typecheck() const;
    friend Def AppNode::typecheck() const; // uses bottom(std::string)
    friend Def TupleNode::typecheck() const; // uses bottom(std::string)
    friend Def AbsRecordNode::typecheck() const;
    friend Def InstRecordNode::typecheck() const;
};

}

#endif
