#ifndef HENK_IR_H
#define HENK_IR_H

#include <set>
#include <unordered_set>
#include <vector>

#include "thorin/util/cast.h"

namespace henk {

class DefNode;
class World;

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

    bool empty() const { return node_ == nullptr; }
    bool operator == (const Proxy<T>& other) const {
        assert(&node()->world() == &other.node()->world());
        //return this->node()->unify() == other.node()->unify();
        return this->node()->deref() == other.node()->deref();
    }
    bool operator != (const Proxy<T>& other) const { return !(*this == other); }
    const T* representative() const { return node()->representative()->template as<T>(); }
    const T* node() const { assert(node_ != nullptr); return node_; }
    const T* deref() const;
    //const T* operator  * () const { return node()->is_unified() ? representative() : node(); }
    const T* operator  * () const { return deref(); }
    const T* operator -> () const { return *(*this); }
    /// Automatic up-cast in the class hierarchy.
    template<class U> operator Proxy<U>() {
        static_assert(std::is_base_of<U, T>::value, "U is not a base type of T");
        return Proxy<U>((**this)->template as<T>());
    }
    template<class U> Proxy<typename U::BaseType> isa() const {
        return Proxy<typename U::BaseType>((*this)->template isa<typename U::BaseType>());
    }
    template<class U> Proxy<typename U::BaseType> as() const {
        return Proxy<typename U::BaseType>((*this)->template as <typename U::BaseType>());
    }
    operator bool() const { return !empty(); }
    //Proxy<T> unify() const { return node()->unify()->template as<T>(); }

private:
    const T* node_;
};

class Def {
public:
    Def()
        : node_(nullptr)
    {}
    Def(const DefNode* node)
        : node_(node)
    {}

    bool empty() const { return node_ == nullptr; }
    const DefNode* node() const { return node_; }
    const DefNode* deref() const;
    const DefNode* operator *() const { return deref(); }
    bool operator == (const DefNode* other) const { return this->deref() == other; }
    operator const DefNode*() const { return deref(); }
    const DefNode* operator -> () const { return deref(); }
    bool is_closed() const;
    void close_abs(Def body) const;
    
    // getters for disassembling defs
    Def abs_var() const;
    Def abs_body() const;
    Def var_type() const;
    Def app_fun() const;
    Def app_arg() const;

private:
    mutable const DefNode* node_;
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
    operator const DefNode*() const { return def_; }
    const Def& operator -> () const { return def_; }

private:
    size_t index_;
    Def def_;
};

struct UseLT {
    inline bool operator () (Use use1, Use use2) const;
};


/// Base class for all @p Def%s.
class DefNode : public thorin::MagicCast<DefNode> {
protected:   
    DefNode(const World* world, size_t gid, size_t size, std::string name, bool closed = true)
        : representative_(this)
        , world_(world)
        , ops_(size)
        , gid_(gid)
        , name(name)
        , is_closed_(closed)
        , uses_{}
    {}
    
    virtual ~DefNode() {}

    void unregister_use(size_t i) const;
    void unregister_uses() const;
    void resize(size_t n) { ops_.resize(n, nullptr); }
    void unlink_representative() const;
    void set_representative(const DefNode* repr) const;
    size_t hash() const { return hash_ == 0 ? hash_ = vhash() : hash_; }
    void set_gid(size_t gid) const { const_cast<size_t&>(const_cast<DefNode*>(this)->gid_) = gid; }
    virtual size_t vhash() const = 0;
    
public:
 //   Def type() const { return type_.empty() ? type_ = typecheck(this) : type_; }
    size_t size() const { return ops_.size(); }
    bool empty() const { return ops_.empty(); }
    void set_op(size_t i, Def def) const; // weird constness?
    void unset_op(size_t i);
    void unset_ops();
    std::set<Use, UseLT> uses() const { return uses_; }
    bool is_proxy() const { return representative_ != this; }
    bool has_uses() const { return !uses_.empty(); }
    size_t num_uses() const { return uses().size(); }
    size_t gid() const { return gid_; }
    const World* world() const { return world_; }
    std::vector<Def> ops() const { return ops_; }
    Def op(size_t i) const { assert(i < ops().size() && "index out of bounds"); return ops_[i]; }
    bool is_closed() const { return is_closed_; }
    void update_closedness() const;

protected:
    mutable std::vector<Def> ops_;
    mutable std::set<Use, UseLT> uses_;
    mutable const DefNode* representative_;
    mutable std::unordered_set<const DefNode*> representative_of_;
    mutable size_t gid_;
    mutable size_t hash_ = 0;
    mutable const World* world_;
    mutable bool live_ = false;
    mutable /*Def*/ const DefNode* equiv_ = nullptr; // hack-ptr useful when testing for equality
    mutable bool is_closed_;
 //   mutable Def type_ = nullptr;

public:
    mutable std::string name;
    
    friend class World;
    friend class Def;
};

class Abs : public DefNode {
protected:
    Abs(const World* world, size_t gid, Def var_type, std::string name);
    Abs(const World* world, size_t gid, Def var);
    
public:
    Def var() const { return op(0); }
    Def body() const { return op(1); }
    void close(Def body) const;

private:
    size_t vhash() const;

    friend class World;
    friend class Def;
};

class Lambda : public Abs {
protected:
    Lambda(const World* world, size_t gid, Def var_type, std::string name)
        : Abs(world, gid, var_type, name)
    {}
    
    friend class World;
    friend class Def;
};

class Var : public DefNode {
protected:
    Var(const World* world, size_t gid, Def type, Def of_abs, std::string name)
        : DefNode(world, gid, 2, name)
        , type_(type)
    {
        set_op(0, type); 
        set_op(1, of_abs);
    }
    
public:
    Def type() const { return op(0); }
    Def of_abs() const { return op(1); }
    
private:
    size_t vhash() const;

    Def type_;

    friend class World;
    friend class Def;
    friend class Abs;
};

class PrimLit : public Var {
private:
    PrimLit(const World* world, size_t gid, Def type, int/*will become Box later on*/ value, std::string name)
        : Var(world, gid, type, nullptr, name)
        , value_(value)
    {}
    
    size_t vhash() const;
    
public:
    int value() const { return value_; };
    
private:
    int value_;
    
    friend class World;
    friend class Def;
};

class Pi : public Abs {
private:
    Pi(const World* world, size_t gid, Def var_type, std::string name)
        : Abs(world, gid, var_type, name)
    {}
    
    Pi(const World* world, size_t gid, Def var)
        : Abs(world, gid, var)
    {}
    
    friend class World;
    friend class Def;
};

class App : public DefNode {
private:
    App(const World* world, size_t gid, Def fun, Def arg, std::string name);
    
    size_t vhash() const;
    
public:
    Def fun() const { return op(0); }
    Def arg() const { return op(1); }

    friend class World;
    friend class Def;
};

}

#endif
