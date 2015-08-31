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
template class Proxy<BottomNode>;
template class Proxy<VarNode>;
template class Proxy<PrimLitNode>;
template class Proxy<AbsNode>;
template class Proxy<LambdaNode>;
template class Proxy<PiNode>;
template class Proxy<AppNode>;

/* ----------------------------------------------------
 * Use
 * ------------------------------------------------- */

size_t UseHash::operator () (Use u) const { 
    return hash_combine(u.def().node()->gid(), u.index());
}

size_t UseEq::operator () (Use use1, Use use2) const {
    auto gid1 = use1.def().node()->gid();
    auto gid2 = use2.def().node()->gid();
    return (gid1 == gid2) && (use1.index() == use2.index());
}

/* ----------------------------------------------------
 * DefNode
 * ------------------------------------------------- */


void DefNode::update_non_reduced_repr() const {
    std::ostringstream r;
    Def(this).dump(r);
    non_reduced_repr_ = r.str();
}

Def DefNode::inftype() const { return inftype_.is_empty() ? inftype_ = typecheck() : inftype_; }

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
    if(this != repr) {
        repr->representative_of_.insert(this);
        repr->non_reduced_repr_ = non_reduced_repr();
    }
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

void AbsNode::close(Def body) const {
    assert(body->is_closed() && "closing AbsNode with unclosed body");
   // world_.reduce(body); // unnecessary?
    set_op(1, body);
    world_.introduce(this);
}

/*
 * constructors
 */

DefNode::DefNode(World& world, size_t gid, size_t size, std::string name)
    : representative_(this)
    , world_(world)
    , ops_(size)
    , gid_(gid)
    , name_(name)
{}

AbsNode::AbsNode(World& world, size_t gid, Def var_type, std::string name)
    : DefNode(world, gid, 2, name)
{
    auto v = new VarNode(world, gid + 1, var_type, this, name);
    v->update_non_reduced_repr();
    set_op(0, v);
}

AbsNode::AbsNode(World& world, size_t gid, Def var)
    : DefNode(world, gid, 2, "some abs")
{
    set_op(0, var);
}

AppNode::AppNode(World& world, size_t gid, Def fun, Def arg, std::string name)
    : DefNode(world, gid, 2, name)
{
    set_op(0, fun);
    set_op(1, arg);
}

/*
 * destructors
 */

AbsNode::~AbsNode() { delete *var(); }

/*
 * reduce
 */

Def DefNode::__reduce_but_dont_replace(const DefNode& def, Def oldd, Def newd) const {
    return def.reduce_but_dont_replace(oldd, newd);
}
 
Def DefNode::__reduce(const DefNode& def, Def2Def& map) const {
    return def.reduce(map);
}
 
void DefNode::reduce() const {
    assert(is_closed() && "unclosed def in reduce");
    Def2Def map;
    set_representative(*reduce(map));
}

void DefNode::reduce(Def oldd, Def newd) const {
    assert(is_closed() && "unclosed def in reduce");
    Def2Def map;
    map[*oldd] = *newd;
    set_representative(*reduce(map));
}

Def DefNode::reduce_but_dont_replace(Def oldd, Def newd) const {
    Def2Def map;
    map[*oldd] = *newd;
    return reduce(map);
}

Def AbsNode::reduce(Def2Def& map) const {
    auto i = map.find(*(var()));
    if (i != map.end()) { // TODO looks broken to me // FIXED?
        map.erase(i);
    }
    std::ostringstream nvarn;
    nvarn << var()->name();
    if (nvarn.str() != "_")
        nvarn << "'";
    auto ntype = __reduce(**var()->type(), map);
    Abs nabs;
    if(this->isa<LambdaNode>())
        nabs = world_.lambda(nvarn.str(), ntype);
    else
        nabs = world_.pi(nvarn.str(), ntype);
    map[*(var())] = *(nabs->var());
    auto nbody = __reduce(**body(), map);
    nabs->close(nbody);
    return nabs;
}

Def BottomNode::reduce(Def2Def& map) const {
    return this;
}

Def VarNode::reduce(Def2Def& map) const {
    auto i = map.find(this);
    if (i != map.end()) {
        return i->second;
    } else {
        return this;
    }
}

Def AppNode::reduce(Def2Def& map) const {
    Def rfun = __reduce(**fun(), map);
    Def rarg = __reduce(**arg(), map);
    if (auto abs = rfun.isa<Abs>()) {
        map[*(abs->var())] = *rarg;
        return __reduce(**abs->body(), map);
    } else {
        if(*rfun != *(fun()) || *rarg != *(arg()))
            return world_.app(rfun, rarg);
        else
            return this;
    }
}

/*
 * typecheck
 */ 

Def LambdaNode::typecheck() const {
    auto body_type = body()->inftype();
    std::ostringstream nvarn;
    nvarn << var()->name();
    if (nvarn.str() != "_")
        nvarn << "'";
    auto res = world_.pi(nvarn.str(), var()->inftype());
    auto body_type2 = __reduce_but_dont_replace(**body_type,
        var(), res->var()
    );
    res->close(body_type2);
    return res;
}

Def PiNode::typecheck() const {
    auto var_type = var()->inftype();
    auto var_type_type = var_type->inftype();
    auto body_type = body()->inftype();
    auto p = world_.wavy_arrow_rules.find(std::make_pair(
        *var_type_type,
        *body_type)
    );
    if (p != world_.wavy_arrow_rules.end()) {
        return p->second;
    }
    else {
        std::ostringstream msg;
        msg << "no wavy arrow rule for " << var_type_type->name();
        msg << " ⤳  " << body_type->name();
        return world_.bottom(msg.str());
    }
}

Def BottomNode::typecheck() const {
    return this;
}

Def VarNode::typecheck() const {
    return type();
}

Def AppNode::typecheck() const {
    auto funt = fun()->inftype();
    auto argt = arg()->inftype();
    if(auto pifunt = funt.isa<Pi>()) {
        if(pifunt->var()->inftype() == argt) {
            return __reduce_but_dont_replace(**pifunt->body(),
                pifunt->var(), arg()
            );
        } else {
            std::ostringstream msg;
            msg << "in application: (";
            fun().dump(msg); msg << ") ("; arg().dump(msg);
            msg << ") -- type of argument (";
            argt.dump(msg); msg << ") != type of fun's var (";
            pifunt->var()->inftype().dump(msg);
            msg << ")";
            return world_.bottom(msg.str());
        }
    } else {
        std::ostringstream msg;
        msg << "in application: (";
        fun().dump(msg); msg << ") ("; arg().dump(msg);
        msg << ") -- type of fun is not Pi, but: ";
        funt.dump(msg);
        return world_.bottom(msg.str());
    }
}


/*
 * equal
 */

bool DefNode::equal (const DefNode& other) const {
    Def2Def map;
    return this->eq(other, map);
}

bool DefNode::eq (const DefNode& other, Def2Def& map) const {
    return typeid(*this) == typeid(other);
}

bool BottomNode::eq (const DefNode& other, Def2Def& map) const {
    return this == &other;
}

bool VarNode::eq (const DefNode& other, Def2Def& map) const {
    auto eqto = map[this];
    return DefNode::eq(other, map) && (this == &other || eqto == &other);
}

bool PrimLitNode::eq (const DefNode& other, Def2Def& map) const {
    return DefNode::eq(other, map) && value() == other.as<PrimLitNode>()->value();
}

bool AbsNode::eq (const DefNode& other, Def2Def& map) const {
    if (DefNode::eq(other, map)) {
        auto aother = other.as<AbsNode>();
        map[*(this->var())] = *(aother->var());
        bool res = var()->type()->eq(**(aother->var()->type()), map) &&
            (body()->eq(**(aother->body()), map));
        map.erase(map.find(*(this->var())));
        return res;
    }
    return false;
}

bool AppNode::eq (const DefNode& other, Def2Def& map) const {
    assert(!this->isa<AbsNode>() && !other.isa<AbsNode>() && "an abstraction in fun position in AppNode::eq"
        " contradicts strong normalization (reduction) policy");
    return DefNode::eq(other, map)
        && fun()->eq(**(other.as<AppNode>()->fun()), map)
        && arg()->eq(**(other.as<AppNode>()->arg()), map);
}

/*
 * vhash
 */

size_t BottomNode::vhash() const { return hash_begin(101); }
size_t VarNode::vhash() const { return hash_combine(type() ? type()->hash() : 9, 5); }
size_t PrimLitNode::vhash() const { return hash_combine(value(), 7); }
size_t AbsNode::vhash() const { return hash_combine(var()->hash(), body() ? body()->hash() : 13); }
size_t AppNode::vhash() const { return hash_combine(fun()->gid(), arg()->gid()); }

/*
 * is_closed
 */

bool BottomNode::is_closed() const { return true; }
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

void BottomNode::dump (std::ostream& stream) const {
    stream << name() << " { " << info() << " }";
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
