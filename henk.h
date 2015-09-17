#ifndef HENK_IR_H
#define HENK_IR_H

#include <functional>
#include <vector>

#include "thorin/util/cast.h"
#include "thorin/util/hash.h"

namespace henk {

class DefNode;
class World;

template<class T> class Proxy;
class DefNode;     typedef Proxy<DefNode>     Def;
class BottomNode;  typedef Proxy<BottomNode>  Bottom;
class VarNode;     typedef Proxy<VarNode>     Var;
class PrimLitNode; typedef Proxy<PrimLitNode> PrimLit;
class AbsNode;     typedef Proxy<AbsNode>     Abs;
class LambdaNode;  typedef Proxy<LambdaNode>  Lambda;
class PiNode;      typedef Proxy<PiNode>      Pi;
class TupleNode;   typedef Proxy<TupleNode>   Tuple;
class DimNode;     typedef Proxy<DimNode>     Dim;
class ProjNode;    typedef Proxy<ProjNode>    Proj;
class DummyNode;   typedef Proxy<DummyNode>   Dummy;
class AppNode;     typedef Proxy<AppNode>     App;

template<class T>
class Proxy {
public:
    typedef T BaseType;

    Proxy()
        : node_(nullptr)
    {}
    Proxy(const T* node)
        : node_(node)
    {}

    bool operator == (const Proxy<T>& other) const {
        assert(&(node()->world()) == &(other.node()->world()));
        // here it would be nice to have an assert stating that both
        // sides are already reduced
        
        return this->deref()->equal(*other.deref());
    }
    bool operator != (const Proxy<T>& other) const { return !(*this == other); }
    const T* representative() const { return node_->representative_->template as<T>(); }
    const T* node() const { assert(node_ != nullptr); return node_; }
    const T* deref() const;
    //const T* operator  * () const { return node()->is_unified() ? representative() : node(); }
    const T* operator  * () const { return deref(); }
    const T* operator -> () const { return *(*this); }
    
    bool is_empty() const { return node_ == nullptr; }
    void dump(std::ostream& stream) const;
    void dump() const { dump(std::cout); std::cout << std::endl; }
    
    // casts

    operator bool() const { return node_; }
    operator const T*() const { return deref(); }

    /// Automatic up-cast in the class hierarchy.
    template<class U> operator Proxy<U>() {
        static_assert(std::is_base_of<U, T>::value, "U is not a base type of T");
        return Proxy<U>((**this)->template as<T>());
    }

    /// Dynamic cast for @p Proxy.
    template<class U> Proxy<typename U::BaseType> isa() const {
        return Proxy<typename U::BaseType>((*this)->template isa<typename U::BaseType>());
    }

    /// Static cast for @p Proxy.
    template<class U> Proxy<typename U::BaseType> as() const {
        return Proxy<typename U::BaseType>((*this)->template as <typename U::BaseType>());
    }

private:
    const T* node_;
};

/**
 * References a user.
 * A \p Def u which uses \p Def d as i^th operand is a \p Use with \p index_ i of \p Def d.
 */
class Use {
public:
    Use() {}
    Use(size_t index, Def def)
        : index_(index)
        , def_(def)
    {}

    size_t index() const { return index_; }
    const Def& def() const { return def_; }
    operator Def() const { return def_; }
    const Def& operator -> () const { return def_; }

private:
    size_t index_;
    Def def_;
};

struct UseHash {
    size_t operator () (Use u) const;
};

struct UseEq {
    size_t operator () (Use use1, Use use2) const;
};

//------------------------------------------------------------------------------

template<class T>
struct GIDHash {
    size_t operator () (T n) const { return n->gid(); }
};

template<class T>
struct GIDEq {
    size_t operator () (T n1, T n2) const { return n1->gid() == n2->gid(); }
};

template<class To>
using DefMap  = thorin::HashMap<const DefNode*, To, GIDHash<const DefNode*>, GIDEq<const DefNode*>>;
using DefSet  = thorin::HashSet<const DefNode*, GIDHash<const DefNode*>, GIDEq<const DefNode*>>;
using Def2Def = DefMap<const DefNode*>;

//------------------------------------------------------------------------------

/// Base class for all @p Def%s.
class DefNode : public thorin::MagicCast<DefNode> {
protected:   
    DefNode(World& world, size_t gid, size_t size, std::string name);
    //DefNode(World& world, size_t gid, thorin::ArrayRef<Def> ops, std::string name);
    
    virtual ~DefNode() {}

    void unregister_use(size_t i) const;
    void unregister_uses() const;
    void unlink_representative() const;
    void set_representative(const DefNode* repr) const;
    void set_gid(size_t gid) const { const_cast<size_t&>(const_cast<DefNode*>(this)->gid_) = gid; }
    virtual size_t vhash() const = 0;
    virtual void update_non_reduced_repr() const;
    std::string __get_non_reduced_repr(const DefNode& def) const;
    virtual Def typecheck() const = 0;
    //virtual std::list<Def> flatten() const { return this; }
    void reduce() const;
    void reduce(Def oldd, Def newd) const; // acts as substitution
    virtual Def reduce(Def2Def& map) const = 0;
    Def reduce_but_dont_replace(Def oldd, Def newd) const; // acts as substitution
    // those '__reduce...' methods are a workaround to being able to call protected methods
    // from derived classes on objects not necessarily of their type, i.e.
    /*  class A : public DefNode {
     *      void foo() {
     *          DefNode* d = this->bar();
     *          // d->reduce(...); // fails
     *          __reduce(*d, ...); // works
     *      }
     *  }
     * // and: __reduce(const DefNode& def, ...) { def.reduce(...); }
     */ 
    Def __reduce_but_dont_replace(const DefNode& def, Def oldd, Def newd) const;
    Def __reduce(const DefNode& def, Def2Def& map) const; 
    
public:
    virtual DefSet free_vars() const;
    std::string non_reduced_repr() const { return non_reduced_repr_; }
    void dump() const;
    virtual void vdump(std::ostream& stream) const = 0;
    size_t hash() const { return hash_ == 0 ? hash_ = vhash() : hash_; }
    Def type() const { assert(!type_.is_empty()); return type_; }
    size_t size() const { return ops_.size(); }
    bool empty() const { return ops_.empty(); }
    void set_op(size_t i, Def def) const;
    void unset_op(size_t i);
    void unset_ops();
    const thorin::HashSet<Use, UseHash, UseEq>& uses() const { return uses_; }
    size_t num_uses() const { return uses().size(); }
    bool is_proxy() const { return representative_ != this; }
    bool has_uses() const;
    bool has_subexpr(Def) const;
    size_t gid() const { return gid_; }
    World& world() const { return world_; }
    thorin::ArrayRef<Def> ops() const { return ops_; }
    Def op(size_t i) const { return ops_[i]; }
    const std::string& name() const { return name_; }
    virtual bool is_closed() const = 0;
    bool equal(const DefNode& other) const;
    virtual bool eq(const DefNode& other, Def2Def& map) const;

protected:
    mutable std::string non_reduced_repr_;
    mutable const DefNode* representative_;
    World& world_;
    mutable thorin::Array<Def> ops_;
    mutable size_t gid_;
    mutable std::string name_;
    mutable thorin::HashSet<Use, UseHash, UseEq> uses_;
    mutable DefSet representative_of_;
    mutable size_t hash_ = 0;
    mutable bool live_ = false;
    // mutable const DefNode* equiv_ = nullptr; // hack-ptr useful when testing for equality
    mutable Def type_ = nullptr;
    
    template<class T> friend class Proxy;
    friend class World;
};

class AbsNode : public DefNode {
protected:
    AbsNode(World& world, size_t gid, Def var_type, std::string name);
    
    virtual ~AbsNode();
    
    virtual Def reduce(Def2Def& map) const override;
    void __update_non_reduced_repr_body(std::ostringstream& r) const;
    
public:
    virtual DefSet free_vars() const override;
    Var var() const { return op(0).as<Var>(); }
    Def body() const { return op(1); }
    void close(Def body) const;
    void dump_body(std::ostream& stream) const;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;

protected:
    virtual size_t vhash() const override;
    
    friend class World;
};

class LambdaNode : public AbsNode {
protected:
    LambdaNode(World& world, size_t gid, Def var_type, std::string name)
        : AbsNode(world, gid, var_type, name)
    {}
    
    virtual Def typecheck() const override;
    virtual void update_non_reduced_repr() const override;
    
public:
    virtual void vdump(std::ostream& stream) const override;

    friend class World;
};

class PiNode : public AbsNode {
protected:
    PiNode(World& world, size_t gid, Def var_type, std::string name)
        : AbsNode(world, gid, var_type, name)
    {}
    
    virtual Def typecheck() const override;
    virtual void update_non_reduced_repr() const override;
    
public:
    virtual void vdump(std::ostream& stream) const override;

    friend class World;
};

class TupleNode : public DefNode {
protected:
    TupleNode(World& world, size_t gid, size_t size, thorin::ArrayRef<Def> elems, std::string name);
    
    virtual Def typecheck() const override;
    virtual Def reduce(Def2Def& map) const override;
    virtual void update_non_reduced_repr() const override;
    
public:
    thorin::Array<Def> elem_types() const;
    virtual void vdump(std::ostream& stream) const override;
    virtual DefSet free_vars() const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;
    
protected:
    virtual size_t vhash() const override;
    
    friend class World;
};

// dimension -- used for typechecking tuples
class DimNode : public DefNode {
protected:
    DimNode(World& world, size_t gid, int n)
        : DefNode(world, gid, 0, "dimension")
        , n_(n)
    {}
    
    virtual void update_non_reduced_repr() const override;
    virtual Def typecheck() const override;
    virtual Def reduce(Def2Def& map) const override;
    
public:
    int n() const { return n_; }
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;

protected:
    virtual size_t vhash() const override;
    
    int n_;
    
    friend class World;
};

// projection -- used for extracting things from tuples
class ProjNode : public DefNode {
protected:
    ProjNode(World& world, size_t gid, int n, int m)
        : DefNode(world, gid, 0, "projection")
        , n_(n)
        , m_(m)
    {}
    
    virtual void update_non_reduced_repr() const override;
    virtual Def typecheck() const override;
    virtual Def reduce(Def2Def& map) const override;
    
public:
    int n() const { return n_; }
    int m() const { return m_; }
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;

protected:
    virtual size_t vhash() const override;
    
    int n_;
    int m_;
    
    friend class World;
};

class BottomNode : public DefNode {
protected:
    BottomNode(World& world, size_t gid, std::string info)
        : DefNode(world, gid, 0, "⊥")
        , info_(info)
    {}
    
    virtual Def typecheck() const override;
    virtual Def reduce(Def2Def& map) const override;
    
public:
    std::string info() const { return info_; }
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;
    
protected:
    virtual size_t vhash() const override;
    
    std::string info_;

    friend class World;
};

class VarNode : public DefNode {
protected:
    VarNode(World& world, size_t gid, Def type, Abs abs, std::string name)
        : DefNode(world, gid, 1, name)
        , abs_(abs)
    {
        set_op(0, type_ = type);
    }
    
    virtual Def typecheck() const override;
    virtual Def reduce(Def2Def& map) const override;
    
public:
    Abs abs() const { return abs_; }
    virtual void vdump(std::ostream& stream) const override;
    virtual DefSet free_vars() const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;
    
protected:
    virtual size_t vhash() const override;

    Abs abs_;

    friend class World;
    friend class AbsNode;
};

class PrimLitNode : public DefNode {
protected:
    PrimLitNode(World& world, size_t gid, Def type, int/*will become Box later on*/ value, std::string name)
        : DefNode(world, gid, 1, name)
        , value_(value)
    { set_op(0, type); }

    virtual Def typecheck() const;
    virtual Def reduce(Def2Def& map) const;
    
    virtual size_t vhash() const override;
    
public:
    virtual void vdump(std::ostream& stream) const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;
    Def type() const { return op(0); }
    int value() const { return value_; };
    virtual bool is_closed() const override;
    
private:
    int value_;
    
    friend class World;
};

class DummyNode : public DefNode {
protected:
    DummyNode(World& world, size_t gid, Def arg_type, Def return_type, 
        bool is_commutative, bool is_associative)
        : DefNode(world, gid, 0, "Dummy")
        , arg_type_(arg_type)
        , return_type_(return_type)
        , is_commutative_(is_commutative)
        , is_associative_(is_associative)
    {}
    
    size_t vhash() const;
    
    virtual Def typecheck() const;
    virtual void update_non_reduced_repr() const;
    virtual Def reduce(Def2Def& map) const;
    
public:
    Def arg_type() const { return arg_type_; }
    Def return_type() const { return return_type_; }
    bool is_reducable() const { return body_.operator bool(); }
    bool is_commutative() const { return is_commutative_; }
    bool is_associative() const { return is_associative_; }
    void put_body(std::function<Def(Def)> body) const { assert(!body_.operator bool() && "dummy already holds a body"); body_ = body; } 
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;

protected:
    Def arg_type_;
    Def return_type_;
    mutable std::function<Def(Def)> body_;
    bool is_commutative_;
    bool is_associative_;
    
    friend class World;
    friend class AppNode; // AppNode::reduce() uses body_
};

class AppNode : public DefNode {
protected:
    AppNode(World& world, size_t gid, Def fun, Def arg, std::string name);
    
    virtual size_t vhash() const override;
    virtual Def typecheck() const override;
    virtual void update_non_reduced_repr() const override;
    virtual Def reduce(Def2Def& map) const override;
    
public:
    virtual DefSet free_vars() const override;
    Def fun() const { return op(0); }
    Def arg() const { return op(1); }
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;

    friend class World;
};

//------------------------------------------------------------------------------

}

#endif
