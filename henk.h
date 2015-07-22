#ifndef HENK_IR_H
#define HENK_IR_H

#include <map>
#include <memory>

#include "thorin/util/cast.h"

namespace henk {

class Lam;
class Pi;
class Body;
class World;

//------------------------------------------------------------------------------

/// Base class for all @p Expr%s.
class Expr : public thorin::MagicCast<Expr> {
protected:
    friend class World;
    
    Expr() {}
    virtual ~Expr() {}
    
    
   // virtual bool equal(const Expr* other) const;
    
public:
    virtual size_t vhash() const = 0;
   // mutable size_t hash = 0;
   // friend struct ExprHash;
  //  friend struct ExprEqual;
};

//------------------------------------------------------------------------------

class AnnotatedExpr : public Expr {
protected:
    friend class World;
    friend class Body;
    
    AnnotatedExpr(const Expr* type, std::string name)
        : type_(type)
        , name_(std::move(name))
    {}
    
    ~AnnotatedExpr() {}
public:
    const std::string& name() const { return name_; }
    const Expr* type() const { return type_; }
    size_t vhash() const;
protected:
    const Expr* type_;
    std::string name_;
    

};

class Const : public AnnotatedExpr {
protected:
    friend class World;
    Const(const Expr* type, std::string name)
        : AnnotatedExpr(type, std::move(name))
    {}
    
    ~Const() {}
    
};

class IntValueConst : public Expr {
protected:
    friend class World;
    IntValueConst(int value)
        : value_(value)
    {}
    
    ~IntValueConst() {}
    
public:
    int value() const { return value_; }
    size_t vhash() const;
protected:
    int value_;
    

};

class BoolValueConst : public Expr {
protected:
    friend class World;
    BoolValueConst(bool value)
        : value_(value)
    {}
    
    ~BoolValueConst() {}
    
public:
    bool value() const { return value_; }
    size_t vhash() const;
protected:
    bool value_;
    

};


class PrimConst : public Expr {
protected:
    friend class World;
    PrimConst(std::string name)
        : name_(name)
    {}
    
    ~PrimConst() {}
public:    
    const std::string& name() const { return name_; } 
    size_t vhash() const;
protected:
    std::string name_;
    

};

//------------------------------------------------------------------------------

/**
 *  Base class for Variable either bound by a lambda abstraction @p Abs
 *  or a @p Pi quantification, or not bound at all
 */
class VarIntr : public AnnotatedExpr {
protected:
    friend class World;
    VarIntr(const Expr* type, std::string name)
        : AnnotatedExpr(type, std::move(name))
        , owner_(nullptr)
    {}
    
    VarIntr(const Expr* type, std::string name, const Expr* ownerr)
        : AnnotatedExpr(type, std::move(name))
        , owner_(ownerr)
    {}
public:
    const Expr* owner() const { return owner_; }
    
protected:
    const Expr* owner_;
};

/// Variable of a lambda abstraction @p Abs.
class LamVar : public VarIntr {
protected:
    friend class World;
    friend class Lam;

    LamVar(const Expr* type, std::string name)
        : VarIntr(type, std::move(name))
    {}
    
    LamVar(const Expr* type, std::string name, const Expr* owner)
        : VarIntr(type, std::move(name), owner)
    {}
    
public:
    const Lam* lam() const { return owner()->as<Lam>(); }
};

/// Variable of a quantification @p Pi.
class PiVar : public VarIntr {
protected:
    friend class World;
    friend class Pi;

    PiVar(const Expr* type, std::string name)
        : VarIntr(type, std::move(name))
    {}
    
    PiVar(const Expr* type, std::string name, const Expr* owner)
        : VarIntr(type, std::move(name), owner)
    {}

public:
    const Pi* pi() const { return owner()->as<Pi>(); }
};

/// Variable occurrence - must be introduced by Lam or Pi beforehand
class VarOcc : public Expr {
protected:
    friend class World;
    VarOcc(const Body* introduced_by)
        : introduced_by_(introduced_by)
    {}

public:
    const Body* introduced_by() const { return introduced_by_; }
    size_t vhash() const;
protected:
    const Body* introduced_by_;
    

};

//------------------------------------------------------------------------------

/// Base class for lambda abstraction @p Abs and dependent product @p Pi.
class Body : public Expr {
protected:
    Body()
        : body_(nullptr)
    {}
    
    Body(const Expr* body)
        : body_(body)
    {}
    
    ~Body() {}
    
public:
    const VarIntr* var() const { return var_.get(); }
    const Expr* body() const { return body_; }

    void close(const Expr* body) { assert(body_ == nullptr); body_ = body; }
    size_t vhash() const;
protected:
    // not sure about that uniqueness
    std::unique_ptr<const VarIntr> var_;

    const Expr* body_;
    
    
};

/// Lambda abstraction
class Lam : public Body {
protected:
    friend class World;
    Lam(std::string var_name, const Expr* var_type)
    {
        var_.reset(new LamVar(var_type, std::move(var_name)));
    }
    
    Lam(const Expr* body, std::string var_name, const Expr* var_type)
        : Body(body)
    {
        var_.reset(new LamVar(var_type, std::move(var_name)));
    }
    
public:
    const LamVar* var() const { return Body::var()->as<LamVar>(); }
};

/// Dependent product
class Pi : public Body {
protected:
    friend class World;
    Pi(std::string var_name, const Expr* var_type)
    {
        var_.reset(new PiVar(var_type, std::move(var_name)));
    }
    
    Pi(const Expr* body, std::string var_name, const Expr* var_type)
        : Body(body)
    {
        var_.reset(new PiVar(var_type, std::move(var_name)));
    }

public:
    const PiVar* var() const { return Body::var()->as<PiVar>(); }
};

//------------------------------------------------------------------------------

/// Application
class App : public Expr {
protected:
    friend class World;
    App(const Expr* appl, const Expr* arg)
        : apply_(appl)
        , arg_(arg)
    {}
    
    ~App() {}

public:
    const Expr* apply() const { return apply_; }
    const Expr* arg() const { return arg_; }
    size_t vhash() const;
protected:
    const Expr* apply_;
    const Expr* arg_;
    

};

}

#endif
