#include <typeinfo>

#include "world.h"

#include "thorin/util/queue.h"

namespace henk {

using thorin::hash_combine;
using thorin::hash_begin;

/* ----------------------------------------------------
 * Proxy
 * ------------------------------------------------- */

template<class T>
const T* Proxy<T>::deref() const {
    if (node_ == nullptr) return nullptr;

    const DefNode* target = node_;
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

    return target->template as<T>();
}

// instantiate templates - but maybe we should place above method simply into the header
template class Proxy<DefNode>;
template class Proxy<VarNode>;
template class Proxy<PrimLitNode>;
template class Proxy<AbsNode>;
template class Proxy<LambdaNode>;
template class Proxy<PiNode>;
template class Proxy<AppNode>;

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
    if (!def) {
        // do nothing; we have to do this in order for var(nullptr, nullptr)
        // to work (that's how star and box are constructed, for instance)
    } else {
        assert(i < size() && "index out of bounds");
        auto node = *def;
        ops_[i] = node;
        assert(def->uses_.count(Use(i, this)) == 0 && "sth uses sth else more than once");
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

bool DefNode::has_subexpr(Def sub) const {
    assert(sub->is_closed() && "unclosed sub in has_subexpr");
    
    DefSet done;
    std::queue<Def> queue;

    auto enqueue = [&] (Def def) {
        if (def) {
            auto p = done.insert(def);
            if (p.second)
                queue.push(def);
        }
    };
    
    enqueue(this);
    while (!queue.empty()) {
        auto def = pop(queue);
        if (def->is_closed())
            if (def == sub)
                return true;

        for (auto op : ops_)
            enqueue(op);

    }

    return false;
}

/* ----------------------------------------------------
 * Classes B where B : DefNode
 * ------------------------------------------------- */

AbsNode::AbsNode(World& world, size_t gid, Def var_type, std::string name)
    : DefNode(world, gid, 2, name)
{
    set_op(0, new VarNode(world, gid + 1 /* gid ? */, var_type, this, name));
}

AbsNode::AbsNode(World& world, size_t gid, Def var)
    : DefNode(world, gid, 2, "some abs")
{
    set_op(0, var);
}

AbsNode::~AbsNode() { delete *var(); }

void AbsNode::close(Def body) const {
   // assert(body->is_closed() && "closing AbsNode with unclosed body");
    world_.reduce(body);
    std::cout << "will close ";
    world_.dump(this);
    std::cout << "  with  ";
    world_.dump(body);
    std::cout << std::endl;
    set_op(1, body);
    Def a = this;
    world_.reduce(a);
    *a;
    std::cout << "after repre ";
    world_.dump(a);
    world_.move_from_garbage(a);
}

AppNode::AppNode(World& world, size_t gid, Def fun, Def arg, std::string name)
    : DefNode(world, gid, 2, name)
{
    set_op(0, fun); set_op(1, arg);
}

/*
 * equal
 */
bool DefNode::equal (const DefNode& other) const {
  /*  std::cout << "are equal: ";
    world_.dump(this);
    std::cout << "  and  ";
    world_.dump(&other);
    std::cout << "  ?" << std::endl;
    world_.reduce(this);
    std::cout << "AAAAA" << std::endl;*/
    world_.reduce(this);
    world_.reduce(&other);
    assert(!other.isa<AppNode>() && "testing AppNode for equality contradicts" 
        " eager normalizing (reducing) policy");
    Def2Def map;
 //   std::cout << "will invoke eq now" << std::endl;
    bool res = this->eq(other, map);
   //             std::cout << "SZTOP" << std::endl;
    return res;
    //return this->eq(other, map);
}

bool DefNode::eq (const DefNode& other, Def2Def& map) const {
  //  std::cout << "DefNode::eq" << std::endl;
    return typeid(*this) == typeid(other);
}

bool VarNode::eq (const DefNode& other, Def2Def& map) const {
   // std::cout << "VarNode::eq" << std::endl;
    auto eqto = map[this];
    return DefNode::eq(other, map) && (this == &other || this == eqto);
}

bool PrimLitNode::eq (const DefNode& other, Def2Def& map) const {
  //  std::cout << "PrimLit::eq" << std::endl;
    return DefNode::eq(other, map) && value() == other.as<PrimLitNode>()->value();
}

bool AbsNode::eq (const DefNode& other, Def2Def& map) const {
  //  std::cout << "AbsNode::eq" << std::endl;
    if (DefNode::eq(other, map)) {
        auto aother = other.as<AbsNode>();
        map[this] = aother;
        //var()->equiv_ = aother->var();
        bool res = (var()->type()->eq(**(aother->var()->type()), map)) &&
            (body()->eq(**(aother->body()), map));
        map.erase(map.find(this));
        //var()->equiv_ = nullptr;
        return res;
    }
    return false;
}

bool AppNode::eq (const DefNode& other, Def2Def& map) const {
    throw std::runtime_error("testing AppNode for equality contradicts "
        " eager normalizing (reducing) policy");
}

/*
 * vhash
 */

size_t VarNode::vhash() const { return hash_combine(type() ? type()->gid() : 9, 5/*abs()->gid()*/); }
size_t PrimLitNode::vhash() const { return hash_combine(value(), 7); }
size_t AbsNode::vhash() const { return hash_combine(var()->hash(), body() ? body()->/*gid*/hash() : 13); }
size_t AppNode::vhash() const { return hash_combine(fun()->gid(), arg()->gid()); }

/*
 * is_closed
 */

bool AbsNode::is_closed() const { return body(); }
bool VarNode::is_closed() const { return type() ? type()->is_closed() : true; }
bool AppNode::is_closed() const { return fun()->is_closed() && arg()->is_closed(); }

/*
 * is_reduced
 */

bool AbsNode::is_reduced() const { return body()->is_reduced() && var()->is_reduced(); }
bool VarNode::is_reduced() const { return type() ? type()->is_reduced() : true; }
bool AppNode::is_reduced() const { return false; }

}
