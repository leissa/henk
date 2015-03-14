#ifndef HENK_IR_H
#define HENK_IR_H

#include <string>

namespace henk {

class Expr {
public:
    std::string name;
};

/// Abstraction
class Abs : public Expr {
public:
    const Expr* type() const { return type_; }

private:
    const Expr* type_;
};

/// Application
class App : public Expr {
public:
    const Expr* to() const { return to_; }
    const Expr* arg() const { return arg_; }

private:
    const Expr* to_;
    const Expr* arg_;
};

/// Quantification
class Pi : public Expr {
public:
    const Expr* to() const { return to_; }
    const Expr* arg() const { return arg_; }

private:
    const Expr* to_;
    const Expr* arg_;
};

}

#endif
