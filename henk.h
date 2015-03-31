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

    Expr(Sort sort)
        : sort_(sort)
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

private:
    std::string name_;
    Sort sort_;
};

//------------------------------------------------------------------------------

/// Base class for Variable either bound by a lambda abstraction @p Abs or a @p Pi quantification.
class Var : public Expr {
protected:
    Var(const Expr* type)
        : Expr(level_down(type->sort()))
        , type_(type)
    {}

public:
    const Expr* type() const { return type_; }
    const Expr* owner() const { return owner_; }

private:
    const Expr* type_;
    const Expr* owner_;
};

/// Variable of a lambda abstraction @p Abs.
class AbsVar : public Var {
public:
    AbsVar(const Expr* type)
        : Var(type)
    {}

    const Abs* abs() const { return owner()->as<Abs>(); }
};

/// Variable of a quantification @p Pi.
class PiVar : public Var {
public:
    PiVar(const Expr* type)
        : Var(type)
    {}

    const Pi* pi() const { return owner()->as<Pi>(); }
};

//------------------------------------------------------------------------------

/// Base class for lambda abstraction @p Abs and quantification @p Pi.
class BodyExpr : public Expr {
public:
    BodyExpr(Sort sort)
        : Expr(sort)
        , body_(nullptr)
    {}

    const Var* var() const { return var_.get(); }
    const Expr* body() const { return body_; }
    void set_body(const Expr* body) { assert(body_ == nullptr); body_ = body; }

private:
    const Expr* body_;

protected:
    std::unique_ptr<const Var> var_;
};

/// Abstraction
class Abs : public BodyExpr {
public:
    Abs(const Expr* type)
        : BodyExpr(level_down(type->sort()))
    {
        var_.reset(new AbsVar(type));
    }

    const AbsVar* var() const { return var()->as<AbsVar>(); }
};

/// Quantification
class Pi : public BodyExpr {
public:
    Pi(const Expr* type)
        : BodyExpr(level_down(type->sort()))
    {
        var_.reset(new PiVar(type));
    }

    const PiVar* var() const { return var_->as<PiVar>(); }
};

//------------------------------------------------------------------------------

/// Application
class App : public Expr {
public:
    const Expr* apply() const { return apply_; }
    const Expr* arg() const { return arg_; }

private:
    const Expr* apply_;
    const Expr* arg_;
};

//------------------------------------------------------------------------------

}

#endif
