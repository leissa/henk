#ifndef HENK_IR_H
#define HENK_IR_H

#include <memory>
#include <string>

#include "thorin/util/cast.h"

namespace henk {

class Abs;
class Pi;

//------------------------------------------------------------------------------

/// Base class for all @p Expr%s.
class Expr : public thorin::MagicCast<Expr> {
public:
    enum Sort {
        Term, Type, Kind, Box
    };

    Expr(Sort sort, std::string name)
        : sort_(sort)
        , name_(std::move(name))
    {}

    Sort sort() const { return sort_; }

    static Sort level_up(Sort sort) {
        auto result = Sort(int(sort)+1);
        assert(0 <= int(result) && int(result) <= Box);
        return result;
    }

    static Sort level_down(Sort sort) {
        auto result = Sort(int(sort)-1);
        assert(0 <= int(result) && int(result) <= Box);
        return result;
    }

    const std::string& name() const { return name_; }
    virtual void dump() const = 0;

private:
    std::string name_;

    Sort sort_;
};

//------------------------------------------------------------------------------

class Const : public Expr {
public:
    Const(const Expr* type, std::string name)
        : Expr(type ? level_up(type->sort()) : Box, std::move(name))
        , type_(type)
    {}

    const Expr* type() const { return type_; }
    virtual void dump() const;

private:
    const Expr* type_;
};

//------------------------------------------------------------------------------

/// Base class for Variable either bound by a lambda abstraction @p Abs or a @p Pi quantification.
class Var : public Expr {
protected:
    Var(const Expr* type, std::string name)
        : Expr(level_down(type->sort()), std::move(name))
        , type_(type)
    {}

public:
    const Expr* type() const { return type_; }
    const Expr* owner() const { return owner_; }

private:
    void dump_var() const;

    const Expr* type_;
    const Expr* owner_;
};

/// Variable of a lambda abstraction @p Abs.
class AbsVar : public Var {
public:
    AbsVar(const Expr* type, std::string name)
        : Var(type, std::move(name))
    {}

    const Abs* abs() const { return owner()->as<Abs>(); }
    virtual void dump() const;
};

/// Variable of a quantification @p Pi.
class PiVar : public Var {
public:
    PiVar(const Expr* type, std::string name)
        : Var(type, std::move(name))
    {}

    const Pi* pi() const { return owner()->as<Pi>(); }
    virtual void dump() const;
};

//------------------------------------------------------------------------------

/// Base class for lambda abstraction @p Abs and quantification @p Pi.
class BodyExpr : public Expr {
public:
    BodyExpr(Sort sort, std::string name)
        : Expr(sort, std::move(name))
        , body_(nullptr)
    {}

    const Var* var() const { return var_.get(); }
    const Expr* body() const { return body_; }
    void close(const Expr* body) { assert(body_ == nullptr); body_ = body; }

private:
    virtual void dump_body() const;

    const Expr* body_;

protected:
    std::unique_ptr<const Var> var_;
};

/// Abstraction
class Abs : public BodyExpr {
public:
    Abs(const Expr* type, std::string abs_name, std::string var_name)
        : BodyExpr(level_down(type->sort()), std::move(abs_name))
    {
        var_.reset(new AbsVar(type, std::move(var_name)));
    }

    const AbsVar* var() const { return var()->as<AbsVar>(); }
    virtual void dump() const;
};

/// Quantification
class Pi : public BodyExpr {
public:
    Pi(const Expr* type, std::string pi_name, std::string var_name)
        : BodyExpr(level_down(type->sort()), std::move(pi_name))
    {
        var_.reset(new PiVar(type, std::move(var_name)));
    }

    const PiVar* var() const { return var_->as<PiVar>(); }
    virtual void dump() const;
};

//------------------------------------------------------------------------------

/// Application
class App : public Expr {
public:
    const Expr* apply() const { return apply_; }
    const Expr* arg() const { return arg_; }
    virtual void dump() const;

private:
    const Expr* apply_;
    const Expr* arg_;
};

//------------------------------------------------------------------------------

}

#endif
