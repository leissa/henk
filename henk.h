#ifndef HENK_IR_H
#define HENK_IR_H

#include <set>
#include <vector>

#include "thorin/util/cast.h"
#include "thorin/util/hash.h"

namespace henk {

class DefNode;
class World;

template<class T> class Proxy;
class DefNode;     typedef Proxy<DefNode>     Def;
class VarNode;     typedef Proxy<VarNode>     Var;
class PrimLitNode; typedef Proxy<PrimLitNode> PrimLit;
class AbsNode;     typedef Proxy<AbsNode>     Abs;
class LambdaNode;  typedef Proxy<LambdaNode>  Lambda;
class PiNode;      typedef Proxy<PiNode>      Pi;
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
        
        //return this->node()->unify() == other.node()->unify();
        return this->deref()->equal(*other.deref());
    }
    bool operator != (const Proxy<T>& other) const { return !(*this == other); }
    const T* representative() const { return node_->representative_->template as<T>(); }
    const T* node() const { assert(node_ != nullptr); return node_; }
    const T* deref() const;
    //const T* operator  * () const { return node()->is_unified() ? representative() : node(); }
    const T* operator  * () const { return deref(); }
    const T* operator -> () const { return *(*this); }
    bool is_empty () const { return node_ == nullptr; }
    
    void dump (std::ostream& stream) const;
    void dump () const { dump(std::cout); }
    
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

struct UseLT {
    inline bool operator () (Use use1, Use use2) const;
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
    DefNode(World& world, size_t gid, size_t size, std::string name)
        : representative_(this)
        , world_(world)
        , ops_(size)
        , gid_(gid)
        , name_(name)
        , uses_{}
    {}
    
    virtual ~DefNode() {}

    void unregister_use(size_t i) const;
    void unregister_uses() const;
    void resize(size_t n) { ops_.resize(n, nullptr); }
    void unlink_representative() const;
    void set_representative(const DefNode* repr) const;
    void set_gid(size_t gid) const { const_cast<size_t&>(const_cast<DefNode*>(this)->gid_) = gid; }
    virtual size_t vhash() const = 0;
    
public:
    virtual void dump (std::ostream& stream) const = 0;
    size_t hash() const { return hash_ == 0 ? hash_ = vhash() : hash_; }
 //   Def type() const { return type_.empty() ? type_ = typecheck(this) : type_; }
    size_t size() const { return ops_.size(); }
    bool empty() const { return ops_.empty(); }
    void set_op(size_t i, Def def) const; // weird constness?
    void unset_op(size_t i);
    void unset_ops();
    std::set<Use, UseLT> uses() const { return uses_; }
    bool is_proxy() const { return representative_ != this; }
    bool has_uses() const { return !uses_.empty(); }
    bool has_subexpr(Def) const;
    size_t num_uses() const { return uses().size(); }
    size_t gid() const { return gid_; }
    World& world() const { return world_; }
    std::vector<Def> ops() const { return ops_; }
    Def op(size_t i) const { assert(i < ops().size() && "index out of bounds"); return ops_[i]; }
    const std::string& name() const { return name_; }
    virtual bool is_closed() const = 0;
    //virtual bool is_reduced() const = 0;
    bool equal (const DefNode& other) const;
    virtual bool eq (const DefNode& other, Def2Def& map) const;

protected:
    mutable const DefNode* representative_;
    World& world_;
    mutable std::vector<Def> ops_;
    mutable size_t gid_;
    mutable std::string name_;
    mutable std::set<Use, UseLT> uses_; // TODO use HashSet
    mutable DefSet representative_of_;
    mutable size_t hash_ = 0;
    mutable bool live_ = false;
    mutable /*Def*/ const DefNode* equiv_ = nullptr; // hack-ptr useful when testing for equality
    mutable bool is_closed_;
 //   mutable Def type_ = nullptr;

    
    friend class World;
    template<class T> friend class Proxy;
};

class AbsNode : public DefNode {
protected:
    AbsNode(World& world, size_t gid, Def var_type, std::string name);
    AbsNode(World& world, size_t gid, Def var);
    
    virtual ~AbsNode();
    
public:
    virtual void dump (std::ostream& stream) const = 0;
    void dump_body (std::ostream& stream) const;
    Var var() const { return op(0).as<Var>(); }
    Def body() const { return op(1); }
    void close(Def body) const;
    virtual bool is_closed() const override;
    //virtual bool is_reduced() const override;
    virtual bool eq (const DefNode& other, Def2Def& map) const override;

private:
    size_t vhash() const;
    
    friend class World;
};

class LambdaNode : public AbsNode {
protected:
    LambdaNode(World& world, size_t gid, Def var_type, std::string name)
        : AbsNode(world, gid, var_type, name)
    {}
    
public:
    virtual void dump (std::ostream& stream) const;
    friend class World;
};

class PiNode : public AbsNode {
protected:
    PiNode(World& world, size_t gid, Def var_type, std::string name)
        : AbsNode(world, gid, var_type, name)
    {}
    
    PiNode(World& world, size_t gid, Def var)
        : AbsNode(world, gid, var)
    {}
    
public:
    virtual void dump (std::ostream& stream) const;
    friend class World;
};

class VarNode : public DefNode {
protected:
    VarNode(World& world, size_t gid, Def type, Def of_abs, std::string name)
        : DefNode(world, gid, 2, name)
    {
        set_op(0, type); 
        set_op(1, of_abs);
    }
    
public:
    virtual void dump (std::ostream& stream) const;
    Def type() const { return op(0); }
    Abs abs() const { return op(1).as<Abs>(); }
    virtual bool is_closed() const override;
   // virtual bool is_reduced() const override;
    virtual bool eq (const DefNode& other, Def2Def& map) const override;
    
private:
    size_t vhash() const;

    friend class World;
    friend class AbsNode;
};

class PrimLitNode : public VarNode {
protected:
    PrimLitNode(World& world, size_t gid, Def type, int/*will become Box later on*/ value, std::string name)
        : VarNode(world, gid, type, nullptr, name)
        , value_(value)
    {}
    
    size_t vhash() const;
    
public:
    virtual void dump (std::ostream& stream) const;
    int value() const { return value_; };
    virtual bool eq (const DefNode& other, Def2Def& map) const override;
    
private:
    int value_;
    
    friend class World;
};

class AppNode : public DefNode {
private:
    AppNode(World& world, size_t gid, Def fun, Def arg, std::string name);
    
    size_t vhash() const;
    
public:
    virtual void dump (std::ostream& stream) const;
    Def fun() const { return op(0); }
    Def arg() const { return op(1); }
    virtual bool is_closed() const override;
    //virtual bool is_reduced() const override;
    virtual bool eq (const DefNode& other, Def2Def& map) const override;

    friend class World;
};

//------------------------------------------------------------------------------

}

#endif
