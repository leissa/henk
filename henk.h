#ifndef HENK_IR_H
#define HENK_IR_H

#include <functional>
#include <vector>
#include <map>
#include <set>

#include "thorin/util/cast.h"
#include "thorin/util/hash.h"

namespace henk {

class DefNode;
class World;

template<class T> class Proxy;
class DefNode;         typedef Proxy<DefNode>         Def;
class BottomNode;      typedef Proxy<BottomNode>      Bottom;
class VarNode;         typedef Proxy<VarNode>         Var;
class PrimLitNode;     typedef Proxy<PrimLitNode>     PrimLit;
class AbsNode;         typedef Proxy<AbsNode>         Abs;
class LambdaNode;      typedef Proxy<LambdaNode>      Lambda;
class PiNode;          typedef Proxy<PiNode>          Pi;
class TupleNode;       typedef Proxy<TupleNode>       Tuple;
class InstRecordNode;  typedef Proxy<InstRecordNode>  InstRecord;
class AbsRecordNode;   typedef Proxy<AbsRecordNode>   AbsRecord;
class DimNode;         typedef Proxy<DimNode>         Dim;
class RecordDimNode;   typedef Proxy<RecordDimNode>   RecordDim;
class ProjNode;        typedef Proxy<ProjNode>        Proj;
class RecordProjNode;  typedef Proxy<RecordProjNode>  RecordProj;
class SigmaNode;       typedef Proxy<SigmaNode>       Sigma;
class PairNode;        typedef Proxy<PairNode>        Pair;
class DummyNode;       typedef Proxy<DummyNode>       Dummy;
class AppNode;         typedef Proxy<AppNode>         App;
class Field;

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
    DefNode(World& world, size_t gid, thorin::ArrayRef<Def> ops, std::string name);
    
    virtual ~DefNode() {}

    void unlink_representative() const;
    void set_representative(const DefNode* repr) const;
    void set_gid(size_t gid) const { const_cast<size_t&>(const_cast<DefNode*>(this)->gid_) = gid; }
    virtual size_t vhash() const = 0;
    virtual Def typecheck() const = 0;
    virtual Def vreduce(Def2Def& map) const = 0;
    // '__reduce...' method is a workaround to being able to call protected methods
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
    Def __reduce(const DefNode& def, Def2Def& map) const; 
    
public:
    Def reduce(Def2Def map = {}, bool replace = true) const; // map says what to substitute with what
    DefSet free_vars() const;
    void dump() const;
    virtual void vdump(std::ostream& stream) const = 0;
    size_t hash() const { return hash_ == 0 ? hash_ = vhash() : hash_; }
    Def type() const { assert(!type_.is_empty()); return type_; }
    size_t size() const { return ops_.size(); }
    bool empty() const { return ops_.empty(); }
    void set_op(size_t i, Def def) const;
    bool is_proxy() const { return representative_ != this; }
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
    mutable const DefNode* representative_;
    World& world_;
    mutable thorin::Array<Def> ops_;
    mutable size_t gid_;
    mutable std::string name_;
    mutable DefSet representative_of_;
    mutable size_t hash_ = 0;
    mutable bool live_ = false;
    mutable Def type_ = nullptr;
    
    template<class T> friend class Proxy;
    friend class World;
};

class AbsNode : public DefNode {
protected:
    AbsNode(World& world, size_t gid, Def var_type, std::string name);
    
    virtual ~AbsNode();
    
    virtual Def vreduce(Def2Def& map) const override;
    virtual size_t vhash() const override;
    
public:
    Var var() const { return var_; }
    Def body() const { return op(0); }
    void close(Def body) const;
    void dump_body(std::ostream& stream) const;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;

protected:
    Var var_;
    friend class World;
};

class LambdaNode : public AbsNode {
protected:
    LambdaNode(World& world, size_t gid, Def var_type, std::string name)
        : AbsNode(world, gid, var_type, name)
    {}
    
    virtual Def typecheck() const override;
    
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
    
public:
    virtual void vdump(std::ostream& stream) const override;

    friend class World;
};

class TupleNode : public DefNode {
protected:
    TupleNode(World& world, size_t gid, thorin::ArrayRef<Def> elems, std::string name);
    
    virtual Def typecheck() const override;
    virtual Def vreduce(Def2Def& map) const override;
    virtual size_t vhash() const override;
    
public:
    thorin::Array<Def> elem_types() const;
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;

    friend class World;
};

class AbsRecordNode : public DefNode {
protected:
    AbsRecordNode(World& world, size_t gid, thorin::Array<std::pair<std::string, Def> > label2type, std::string name);
    
    virtual Def typecheck() const override;
    virtual Def vreduce(Def2Def& map) const override;
    virtual size_t vhash() const override;
    
public:
    thorin::Array<std::pair<std::string, Def> > label2type() const { return label2type_; }
    thorin::Array<Field> get_fields () const { return fields_; }
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;
    
protected:
    mutable thorin::Array<std::pair<std::string, Def> > label2type_; // aka lexi2reali
    mutable thorin::Array<Field> fields_;
    mutable std::vector<size_t> reali2lexi;
    
    friend class AppNode;
    friend class InstRecordNode;
    friend class World;
};

class InstRecordNode : public DefNode {
protected:
    InstRecordNode(World& world, size_t gid, thorin::Array<std::string> labels, 
        thorin::Array<Def> elems, AbsRecord ascribed_type, std::string name);
    
    virtual Def typecheck() const override;
    virtual Def vreduce(Def2Def& map) const override;
    virtual size_t vhash() const override;
    
public:
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;
    
protected:
    mutable AbsRecord ascribed_type_;
    mutable thorin::Array<std::string> labels_;
    mutable std::vector<size_t> lexi2reali;
    mutable std::vector<size_t> reali2lexi;
    
    friend class AppNode;
    friend class World;
};

class Field {
public:
    Field()
    {}
protected:
    Field(std::string l, size_t i, AbsRecord o)
        : label_(l)
        , index_(i)
        , owner_(o)
    {}
    
public:
    std::string label () const { return label_; }
    size_t index () const { return index_; }
    AbsRecord owner () const { return owner_; }
    
protected:
    std::string label_;
    size_t index_;
    AbsRecord owner_;
    
    friend class AbsRecordNode;
    friend class InstRecordNode;
    friend class World;
};

// dimension -- used for typechecking tuples
class DimNode : public DefNode {
protected:
    DimNode(World& world, size_t gid, int n)
        : DefNode(world, gid, {}, "dimension")
        , n_(n)
    {}
    
    virtual Def typecheck() const override;
    virtual Def vreduce(Def2Def& map) const override;
    virtual size_t vhash() const override;
        
public:
    int n() const { return n_; }
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;

protected:
    int n_;
    
    friend class World;
};

class RecordDimNode : public DefNode {
protected:
    RecordDimNode(World& world, size_t gid, AbsRecord of_record)
        : DefNode(world, gid, {}, "record dimension")
        , of_record_(of_record)
    {}
    
    virtual Def typecheck() const override;
    virtual Def vreduce(Def2Def& map) const override;
    virtual size_t vhash() const override;
        
public:
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;

protected:
    AbsRecord of_record_;
    
    friend class World;
};

// projection -- used for extracting things from tuples
class ProjNode : public DefNode {
protected:
    ProjNode(World& world, size_t gid, int n, int m)
        : DefNode(world, gid, {}, "projection")
        , n_(n)
        , m_(m)
    {}
    
    virtual Def typecheck() const override;
    virtual Def vreduce(Def2Def& map) const override;
    virtual size_t vhash() const override;
    
public:
    int n() const { return n_; }
    int m() const { return m_; }
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;

protected:
    int n_;
    int m_;
    
    friend class World;
};

class RecordProjNode : public DefNode {
protected:
    RecordProjNode(World& world, size_t gid, Field field)
        : DefNode(world, gid, {}, "projection")
        , field_(field)
    {}
    
    virtual Def typecheck() const override;
    virtual Def vreduce(Def2Def& map) const override;
    virtual size_t vhash() const override;
    
public:
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;
    Field field () const { return field_; }

protected:
    Field field_;
    
    friend class World;
};

class BottomNode : public DefNode {
protected:
    BottomNode(World& world, size_t gid, std::string info)
        : DefNode(world, gid, {}, "⊥")
        , info_(info)
    {}
    
    virtual Def typecheck() const override;
    virtual Def vreduce(Def2Def& map) const override;
    virtual size_t vhash() const override;
    
public:
    std::string info() const { return info_; }
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;
    
protected:
    std::string info_;

    friend class World;
};

class VarNode : public DefNode {
protected:
    VarNode(World& world, size_t gid, Def type, Abs abs, std::string name)
        : DefNode(world, gid, {}, name)
        , abs_(abs)
    {
        type_ = type;
    }
    
    virtual Def typecheck() const override;
    virtual Def vreduce(Def2Def& map) const override;
    virtual size_t vhash() const override;
    
public:
    Abs abs() const { return abs_; }
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;
    
protected:
    Abs abs_;

    friend class World;
    friend class AbsNode;
};

class SigmaNode : public AbsNode {
protected:
    SigmaNode(World& world, size_t gid, Def var_type, std::string name)
        : AbsNode(world, gid, var_type, name)
    {}
    
    virtual Def typecheck() const override;
    
public:
    virtual void vdump(std::ostream& stream) const override;

    friend class World;
};

class PairNode : public DefNode {
protected:
    PairNode(World& world, size_t gid, Def first, Def second, Sigma ascribed_type, std::string name)
        : DefNode(world, gid, { first, second }, name)
        , ascribed_type_(ascribed_type)
    {}
    
    virtual Def typecheck() const override;
    virtual Def vreduce(Def2Def& map) const override;
    virtual size_t vhash() const override;
    
public:
    Def first () const { return op(0); }
    Def second () const { return op(1); }
    virtual void vdump(std::ostream& stream) const override;
    virtual bool is_closed() const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;
    
protected:
    mutable Sigma ascribed_type_;
    
    friend class World;
};

class PrimLitNode : public DefNode {
protected:
    PrimLitNode(World& world, size_t gid, Def type, int/*will become Box later on*/ value, std::string name)
        : DefNode(world, gid, {}, name)
        , value_(value)
    {
        type_ = type;
    }

    virtual Def typecheck() const;
    virtual Def vreduce(Def2Def& map) const;
    virtual size_t vhash() const override;
    
public:
    virtual void vdump(std::ostream& stream) const override;
    virtual bool eq(const DefNode& other, Def2Def& map) const override;
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
        : DefNode(world, gid, {}, "Dummy")
        , arg_type_(arg_type)
        , return_type_(return_type)
        , is_commutative_(is_commutative)
        , is_associative_(is_associative)
    {}
    
    size_t vhash() const;
    
    virtual Def typecheck() const;
    virtual Def vreduce(Def2Def& map) const;
    
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
    AppNode(World& world, size_t gid, Def fun, Def arg, std::string name)
        : DefNode(world, gid, { fun, arg }, name)
    {}
    
    virtual size_t vhash() const override;
    virtual Def typecheck() const override;
    virtual Def vreduce(Def2Def& map) const override;
    
public:
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
