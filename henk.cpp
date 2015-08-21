#include "world.h"

namespace henk {

/* ----------------------------------------------------
 * Def
 * ------------------------------------------------- */

const DefNode* Def::deref() const {
    if (node_ == nullptr) return nullptr;

    auto target = node_;
    for (; target->is_proxy(); target = target->representative_)
        assert(target != nullptr);

    // path compression
    const DefNode* n = node_;
    while (n->representative_ != target) {
        auto representative = n->representative_;
        auto res = representative->representative_of_.erase(n);
        assert(res == 1);
        n->representative_ = target;
        auto p = target->representative_of_.insert(n);
        assert(p.second);
        n = representative;
    }

    return target;
}

bool Def::is_closed() const { return node_->is_closed(); }

void Def::close_abs(Def body) const {
    if(auto abs = node_->isa<Abs>()) {
        abs->close(body);
    }
    else
        throw std::runtime_error("attempt to close non-abstraction");
}

// getters for deassembling defs
Def Def::abs_var() const {
    if(auto abs = node_->isa<Abs>()) {
        return abs->var();
    }
    else
        throw std::runtime_error("in abs_var node_ is not Abs");
}

Def Def::abs_body() const {
    if(auto abs = node_->isa<Abs>()) {
        return abs->body();
    }
    else
        throw std::runtime_error("in abs_var node_ is not Abs");
}

Def Def::var_type() const {
    if(auto var = node_->isa<Var>()) {
        return var->type();
    }
    else
        throw std::runtime_error("in var_type node_ is not Var");
}

Def Def::app_fun() const {
    if(auto app = node_->isa<App>()) {
        return app->fun();
    }
    else
        throw std::runtime_error("in app_fun node_ is not App");
}

Def Def::app_arg() const {
    if(auto app = node_->isa<App>()) {
        return app->arg();
    }
    else
        throw std::runtime_error("in app_fun node_ is not App"); 
}

/* ----------------------------------------------------
 * Use
 * ------------------------------------------------- */
 
bool UseLT::operator () (Use use1, Use use2) const {
    auto gid1 = use1.def().node()->gid();
    auto gid2 = use2.def().node()->gid();
    return (gid1 < gid2 || (gid1 == gid2 && use1.index() < use2.index()));
}

/* ----------------------------------------------------
 * DefNode
 * ------------------------------------------------- */

void DefNode::set_op(size_t i, Def def) const { // weird constness?
    assert(!op(i) && "already set");
   // assert(def && "setting null pointer");
    if(!def) {
        // do nothing; we have to do this in order for var(nullptr, nullptr)
        // to work (that's how star and box are constructed, for instance
    }
    else {
        assert(i < size() && "index out of bounds");
        auto node = *def;
        ops_[i] = node;
        assert(def->uses_.count(Use(i, this)) == 0 && "sth uses sth else twice");
        auto p = node->uses_.emplace(i, this);
        assert(p.second);
    }
}

void DefNode::unregister_uses() const {
    for (size_t i = 0, e = size(); i != e; ++i)
        unregister_use(i);
}

void DefNode::unregister_use(size_t i) const {
    auto def = ops_[i].node();
    assert(def->uses_.count(Use(i, this)) == 1);
    def->uses_.erase(Use(i, this));
}

void DefNode::unlink_representative() const {
    if (is_proxy()) {
        auto num = representative_->representative_of_.erase(this);
        assert(num == 1);
    }
}

void DefNode::set_representative(const DefNode* repr) const {
    unlink_representative();
    representative_ = repr;
    repr->representative_of_.insert(this);
}

void DefNode::unset_op(size_t i) {
    assert(ops_[i] && "must be set");
    unregister_use(i);
    ops_[i] = nullptr;
}

void DefNode::unset_ops() {
    for (size_t i = 0, e = size(); i != e; ++i)
        unset_op(i);
}

void DefNode::update_closedness() const {
    bool closed = true;
    for(auto& d : ops()) {
        closed &= d->is_closed();
    }
    if(closed != is_closed()) {
        is_closed_ = closed;
        // closed terms need to be moved from garbage to expressions set
        world_->move_from_garbage(this);
        
        for(auto& u : uses()) {
            auto deff = u.def();
            (*deff)->update_closedness();
        }
    }
}

/* ----------------------------------------------------
 * Classes B where B : DefNode
 * ------------------------------------------------- */

Abs::Abs(const World* world, size_t gid, Def var_type, std::string name)
    : DefNode(world, gid, 2, name, false)
{
    set_op(0, new Var(world, gid + 1 /* gid ? */, var_type, this, name));
}

Abs::Abs(const World* world, size_t gid, Def var)
    : DefNode(world, gid, 2, "some abs", false)
{
    set_op(0, var);
}

void Abs::close(Def body) const {
    set_op(1, body);
    is_closed_ = body.is_closed();
    update_closedness();
}

App::App(const World* world, size_t gid, Def fun, Def arg, std::string name)
    : DefNode(world, gid, 2, name, fun->is_closed() && arg->is_closed())
{
    set_op(0, fun); set_op(1, arg);
}


size_t Var::vhash() const {
    return hash_combine(type()->gid(), of_abs()->gid());
}

size_t PrimLit::vhash() const {
    return hash_begin(value());
}

size_t Abs::vhash() const {
    return hash_combine(var()->gid(), body()->gid());
}

size_t App::vhash() const {
    return hash_combine(fun()->gid(), arg()->gid());
}

}
