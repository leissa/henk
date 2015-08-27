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
    if(this != repr)
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
        if (def->is_closed()) {
            if (def == sub) {
                return true;
            }
        }
        if(auto v = def->isa<VarNode>()) {
            enqueue(v->type());
        }
        else for (auto op : def->ops_) {
            enqueue(op);
        }
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
    assert(body->is_closed() && "closing AbsNode with unclosed body");
    world_.reduce(body);
 //   std::cout << "will close ";
 //   world_.dump(this);
 //   std::cout << "  with  ";
 //   world_.dump(body);
 //   std::cout << std::endl;
    set_op(1, body);
    world_.introduce(this);
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
   // assert(!other.isa<AppNode>() && "testing AppNode for equality contradicts" 
   //     " eager normalizing (reducing) policy");
 /*  std::cout << "are ";
   world_.dump(this);
   std::cout << "  and  ";
   world_.dump(&other);
   std::cout << "  equal? -- " << std::endl;
    Def2Def map;
   bool res = this->eq(other, map);
    if(res)
        std::cout << "yes!";
    else
        std::cout << "nope!";
    std::cout << std::endl;*/
    Def2Def map;
    
    return this->eq(other, map);
}

bool DefNode::eq (const DefNode& other, Def2Def& map) const {
   // std::cout << "DefNode::eq";
  //  bool res = typeid(*this) == typeid(other);
  //  std::cout << " " << res << std::endl;
    return typeid(*this) == typeid(other);
}

bool VarNode::eq (const DefNode& other, Def2Def& map) const {
   // std::cout << "VarNode::eq: ";
  //  world_.dump(this); std::cout << " at " << this << "  and  ";
  //  world_.dump(&other); std::cout << " at " << &other << std::endl;
    auto eqto = map[this];
  //  std::cout << "var at " << this << " and we know its eq to " << eqto << std::endl;
    return DefNode::eq(other, map) && (this == &other || eqto == &other);
}

bool PrimLitNode::eq (const DefNode& other, Def2Def& map) const {
  //  std::cout << "PrimLit::eq" << std::endl;
    return DefNode::eq(other, map) && value() == other.as<PrimLitNode>()->value();
}

bool AbsNode::eq (const DefNode& other, Def2Def& map) const {
  //  std::cout << "AbsNode::eq" << std::endl;
    if (DefNode::eq(other, map)) {
        auto aother = other.as<AbsNode>();
        map[*(this->var())] = *(aother->var());
  //      std::cout << "this = " << *(this->var()) << " and aother = " << *(aother->var()) << std::endl;
      //  auto eqto = map[this->var()];
     //   std::cout << "retrieving " << eqto << std::endl;
        //var()->equiv_ = aother->var();
        bool typeq = var()->type()->eq(**(aother->var()->type()), map);
 //       std::cout << "types: " << typeq << std::endl;
  //      std::cout << "body is at " << *body() << " and " << *(aother->body()) << std::endl;
        bool res = typeq && //(var()->type()->eq(**(aother->var()->type()), map)) &&
            (body()->eq(**(aother->body()), map));
        map.erase(map.find(*(this->var())));
        //var()->equiv_ = nullptr;
        return res;
    }
    return false;
}

bool AppNode::eq (const DefNode& other, Def2Def& map) const {
  //  std::cout << "AppNode::eq" << std::endl;
   // throw std::runtime_error("testing AppNode for equality contradicts "
    //    " eager normalizing (reducing) policy");
    assert(!this->isa<AbsNode>() && !other.isa<AbsNode>() && "an abstraction in fun position in AppNode::eq"
        " contradicts strong normalization (reduction) policy");
    return DefNode::eq(other, map)
        && fun()->eq(**(other.as<AppNode>()->fun()), map)
        && arg()->eq(**(other.as<AppNode>()->arg()), map);
}

/*
 * vhash
 */

size_t VarNode::vhash() const { return hash_combine(type() ? type()->/*gid*/hash() : 9, 5/*abs()->gid()*/); }
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
 * dump
 */

template<class T>
void Proxy<T>::dump (std::ostream& stream) const {
    if(is_empty())
        stream << "'nullptr'";
    else
        deref()->dump(stream);
}

void LambdaNode::dump (std::ostream& stream) const {
    stream << "λ";
    dump_body(stream);
}

void PiNode::dump (std::ostream& stream) const {
    if(body().is_empty()) {
        stream << "Π";
        dump_body(stream);
    } else if (var()->name() == "_" || !body()->has_subexpr(var())) {
        stream << "(";
        var().as<Var>()->type().dump(stream);
        stream << ") -> (";
        body().dump(stream);
        stream << ")";
    } else if (*(var().as<Var>()->type()) == *(world_.get_prim_const("*"))) {
        stream << "∀" << var()->name() << ". ";
        body().dump(stream);
    } else {
        stream << "Π";
        dump_body(stream);
    }  
}

void AbsNode::dump_body (std::ostream& stream) const {
    var().dump(stream);
    stream << ":";
    var()->type().dump(stream);
    stream << ". ";
    body().dump(stream);
}

void VarNode::dump (std::ostream& stream) const {
    stream << name();
}

void PrimLitNode::dump (std::ostream& stream) const {
    stream << value();
}

void AppNode::dump (std::ostream& stream) const {
    stream << "(";
    fun().dump(stream);
    stream << ") (";
    arg().dump(stream);
    stream << ")";
}

}
