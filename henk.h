#ifndef HENK_IR_H
#define HENK_IR_H

#include <map>
#include <memory>
#include <unordered_set>
#include <sstream>

#include "thorin/util/cast.h"

namespace henk {

class Lam;
class Pi;
class Body;
class World;
class Expr;

//------------------------------------------------------------------------------

/// Base class for all @p Expr%s.
class Expr : public thorin::MagicCast<Expr> {
protected:
    friend class World;
    
    Expr() 
        : gid_(-1)
        , refCount_(0)
        , representative_(this)
    {}
    
    Expr(size_t gid)
        : gid_(gid)
        , refCount_(0)
        , representative_(this)
    {}
    virtual ~Expr() {}
    
    std::unordered_set<const Expr*> uses() const { return uses_; }
    mutable std::unordered_set<const Expr*> uses_;

    
public:
    bool hasUses() const { return !uses_.empty(); }
    bool unregister_use(const Expr* user) const {
        uses_.erase(user);
        return true;
    }
    void register_use(const Expr* user) const {
        uses_.insert(user);
    }

    virtual std::unordered_set<const Expr*> gatherUnusedCascading() const = 0;

    size_t gid() const { return gid_; }
    size_t hash() const { return hash_ == 0 ? hash_ = vhash() : hash_; }
    
    void increaseRefCount() const { refCount_++; }
    
    void decreaseRefCount() const {
        std::ostringstream msg;
        msg << " of expr " << this;
      /*  if(refCount_ < 1) {
            throw std::runtime_error(msg.str());
        }*/
        //assert(refCount_ >= 1);
        refCount_--;
    }
    
    size_t refCount() const { return refCount_; }
    
protected:
    virtual size_t vhash() const = 0;
    
    void set_gid(size_t gid) const { const_cast<size_t&>(const_cast<Expr*>(this)->gid_) = gid; }
    const size_t gid_;
    mutable size_t hash_ = 0;
    mutable size_t refCount_;
    mutable const Expr* representative_;
    
    void set_representative(const Expr* e) const { representative_ = e; }
    const Expr* representative() { return representative_; }
};

//------------------------------------------------------------------------------

class AnnotatedExpr : public Expr {
protected:
    friend class World;
    friend class Body;
    
    AnnotatedExpr(const Expr* type, std::string name)
        : type_(type)
        , name_(std::move(name))
    { type_->register_use(this); }
    
public:
    const std::string& name() const { return name_; }
    const Expr* type() const { return type_; }
    
    std::unordered_set<const Expr*> gatherUnusedCascading() const {
        if(refCount() != 0 || hasUses()) {
            return std::unordered_set<const Expr*>();
        }
        else {
            type()->unregister_use(this);
            auto res = type()->gatherUnusedCascading();
            res.insert(this);
            return res;
        }
    }
    
protected:
    const Expr* type_;
    std::string name_;
    size_t vhash() const;

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
    
    std::unordered_set<const Expr*> gatherUnusedCascading() const {
        // never get rid of ints on the heap -- reasonable?
        return std::unordered_set<const Expr*>();
    }
    
protected:
    int value_;
    size_t vhash() const;

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
    
    std::unordered_set<const Expr*> gatherUnusedCascading() const {
        return std::unordered_set<const Expr*>();
    }
    
protected:
    bool value_;
    size_t vhash() const;

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
    
    std::unordered_set<const Expr*> gatherUnusedCascading() const {
        return std::unordered_set<const Expr*>();
    }
    
protected:
    std::string name_;
    size_t vhash() const;

};

//------------------------------------------------------------------------------

/**
 *  Base class for Variable either bound by a lambda abstraction @p Abs
 *  or a @p Pi quantification
 */
class VarIntr : public AnnotatedExpr {
protected:
    friend class World;
    VarIntr(const Expr* type, std::string name)
        : AnnotatedExpr(type, std::move(name))
        , owner_(nullptr)
    {}
    
    VarIntr(const Expr* type, std::string name, const Expr* owner)
        : AnnotatedExpr(type, std::move(name))
        , owner_(owner)
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
    
    std::unordered_set<const Expr*> gatherUnusedCascading() const {
        if(refCount() != 0 || hasUses()) {
            return std::unordered_set<const Expr*>();
        }
        else {
            var()->unregister_use(this);
            body()->unregister_use(this);
            auto vres = var()->gatherUnusedCascading();
            auto bres = body()->gatherUnusedCascading();
            vres.insert(bres.begin(), bres.end());
            vres.insert(this);
            return vres;
        }
    }

    friend class World;
    
protected:
    // not sure about that uniqueness
    std::unique_ptr<const VarIntr> var_;
    size_t vhash() const;
    const Expr* body_;
    void close(const Expr* body) { assert(body_ == nullptr); body_ = body; }
    
};

/// Variable occurrence - must be introduced by Lam or Pi beforehand
class VarOcc : public Expr {
protected:
    friend class World;
    VarOcc(const Body* introduced_by)
        : introduced_by_(introduced_by)
    { introduced_by_->var()->register_use(this); }

public:
    const Body* introduced_by() const { return introduced_by_; }
    
    std::unordered_set<const Expr*> gatherUnusedCascading() const {
        if(refCount() != 0 || hasUses()) {
            return std::unordered_set<const Expr*>();
        }
        else {
            introduced_by()->var()->unregister_use(this);
            auto res = introduced_by()->var()->gatherUnusedCascading();
            res.insert(this);
            return res;
        }
    }
    
protected:
    const Body* introduced_by_;
    size_t vhash() const;

};

//------------------------------------------------------------------------------

/// Lambda abstraction
class Lam : public Body {
protected:
    friend class World;
    Lam(std::string var_name, const Expr* var_type)
    {
        var_.reset(new LamVar(var_type, std::move(var_name)));
        var()->register_use(this);
    }
    
    Lam(const Expr* bbody, std::string var_name, const Expr* var_type)
        : Body(bbody)
    {
        var_.reset(new LamVar(var_type, std::move(var_name)));
        var()->register_use(this);
        body()->register_use(this);
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
        var()->register_use(this);
    }
    
    Pi(const Expr* bbody, std::string var_name, const Expr* var_type)
        : Body(bbody)
    {
        var_.reset(new PiVar(var_type, std::move(var_name)));
        var()->register_use(this);
        body()->register_use(this);
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
    { apply_->register_use(this); arg_->register_use(this); }
    
    ~App() {}

public:
    const Expr* apply() const { return apply_; }
    const Expr* arg() const { return arg_; }
    
    std::unordered_set<const Expr*> gatherUnusedCascading() const {
        if(refCount() != 0 || hasUses()) {
            return std::unordered_set<const Expr*>();
        }
        else {
            apply()->unregister_use(this);
            arg()->unregister_use(this);
            auto res1 = apply()->gatherUnusedCascading();
            auto res2 = arg()->gatherUnusedCascading();
            res1.insert(res2.begin(), res2.end());
            res1.insert(this);
            return res1;
        }
    }
    
protected:
    const Expr* apply_;
    const Expr* arg_;
    size_t vhash() const;

};

}

#endif
