#include <typeinfo>
#include <sstream>

#include "world.h"

#include "thorin/util/queue.h"

namespace henk {

using thorin::Array;
using thorin::ArrayRef;
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
template class Proxy<TupleNode>;
template class Proxy<DimNode>;
template class Proxy<ProjNode>;
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
        representative_ = this;
    }
}

void DefNode::set_representative(const DefNode* repr) const {
    unlink_representative();
    representative_ = repr;
    if (this != repr) {
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
        if (auto v = def->isa<VarNode>()) {
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

Array<Def> TupleNode::elem_types() const {
    Array<Def> result(size());
    for (size_t i = 0, e = size(); i != e; ++i)
        result[i] = op(i)->inftype();
    return result;
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

TupleNode::TupleNode(World& world, size_t gid, size_t size, std::string name, ArrayRef<Def> elems)
    : DefNode(world, gid, size, name)
{ 
    std::copy(elems.begin(), elems.end(), ops_.begin());
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
    assert(is_closed() && "unclosed def in reduce/subst");
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
    auto i = map.find(*var());
    if (i != map.end()) { // TODO looks broken to me // FIXED?
        map.erase(i);
    }
    std::ostringstream nvarn;
    nvarn << var()->name();
    if (nvarn.str() != "_")
        nvarn << "'";
    auto ntype = __reduce(**var()->type(), map);
    Abs nabs;
    if (this->isa<LambdaNode>())
        nabs = world_.lambda(nvarn.str(), ntype);
    else
        nabs = world_.pi(nvarn.str(), ntype);
    map[*var()] = *nabs->var();
    auto nbody = __reduce(**body(), map);
    nabs->close(nbody);
    return nabs;
}

Def TupleNode::reduce(Def2Def& map) const {
  //  if (ops_.size() == 1) {
   //     return __reduce(**ops_[0], map);
 //   } else {
        bool changed = false;
        std::vector<Def> nops;//(ops_.size());
        for (auto& d : ops_) {
            auto nd = __reduce(**d, map);
            nops.push_back(nd);
            changed &= *nd == *d;
        }
        if (changed) {
            return world_.tuple(nops);
        } else
            return this;
   // }
}

Def DimNode::reduce(Def2Def& map) const {
    return this;
}

Def ProjNode::reduce(Def2Def& map) const {
    return this;
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
    if (auto tup = rfun.isa<Tuple>()) {
        if (auto proj = rarg.isa<Proj>()) {
            return tup->op(proj->m());
        } else if (*rfun != *fun())
            return world_.app(rfun, rarg);
        else
            return this;
    } else if (auto abs = rfun.isa<Abs>()) {
        map[*abs->var()] = *rarg;
        return __reduce(**abs->body(), map);
    } else {
        if (*rfun != *fun() || *rarg != *arg())
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

Def TupleNode::typecheck() const {
    
    for(size_t i = 0; i < ops_.size(); ++i) {
        if (ops_[i]->isa<BottomNode>()) {
            std::ostringstream msg;
            msg << "tuple ";
            dump(msg); msg << " has bottom type as " << i << " elem";
            return world_.bottom(msg.str());
        }
    }
    
    auto t = world_.pi("i", world_.dimension(size()));
  //  std::vector<Def> comptypes;//(size());
  //  for (auto& d : ops_)
  //      comptypes.push_back(d->inftype());
    auto b = world_.app(world_.tuple(/*comptypes*/elem_types()), t->var());
    t->close(b);
    return t;
}

Def DimNode::typecheck() const {
    return world_.get_prim_const("D");
}

Def ProjNode::typecheck() const {
    return world_.dimension(n_);
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
    if (auto pifunt = funt.isa<Pi>()) {
        
        if (auto tup = fun().isa<Tuple>()) {
            if (auto argv = arg().isa<Var>()) {
                auto dim = world_.dimension(tup->size());
                Dim argtd;
                if ((/*auto*/ argtd = argv->inftype().isa<Dim>()) && dim == argtd) {
                   // if (dim == argv->inftype()) {
                        return world_.app(world_.tuple(tup->elem_types()), argv);
                  //  } 
                } else {
                    std::ostringstream msg;
                    msg << "in application: (";
                    fun().dump(msg); msg << ") ("; arg().dump(msg);
                    msg << ") -- arg is not Proj of " << tup->size() << "-tuple";
                    return world_.bottom(msg.str());
                }
                
            } else if (auto argp = arg().isa<Proj>()) {
                size_t m = argp->m();
                if (m < tup->size()) {
                    return tup->op(argp->m())->inftype();
                } else {
                    std::ostringstream msg;
                    msg << "in application: (";
                    fun().dump(msg); msg << ") ("; arg().dump(msg);
                    msg << ") -- projection outside bounds";
                    return world_.bottom(msg.str());
                }
            } else {
                std::ostringstream msg;
                msg << "in application: (";
                fun().dump(msg); msg << ") ("; arg().dump(msg);
                msg << ") -- argument is neither Var nor Proj";
                return world_.bottom(msg.str());
            }
        } else if (pifunt->var()->inftype() == argt) {
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
        map[*this->var()] = *aother->var();
        bool res = var()->type()->eq(**aother->var()->type(), map) &&
            (body()->eq(**aother->body(), map));
        map.erase(map.find(*this->var()));
        return res;
    }
    return false;
}

bool TupleNode::eq (const DefNode& other, Def2Def& map) const {
    if (DefNode::eq(other, map) && size() == other.size()) {
        bool compeq = true;
        //auto tother = other.as<TupleNode>();
        for (size_t i = 0; i < size(); ++i) {
            compeq &= ops_[i]->eq(**other.op(i), map);
        }
        return compeq;
    }
    return false;
}

bool DimNode::eq (const DefNode& other, Def2Def& map) const {
    return DefNode::eq(other, map) && n_ == other.as<DimNode>()->n_;
}

bool ProjNode::eq (const DefNode& other, Def2Def& map) const {
    return DefNode::eq(other, map) && n_ == other.as<ProjNode>()->n_
        && m_ == other.as<ProjNode>()->m_;
}

bool AppNode::eq (const DefNode& other, Def2Def& map) const {
    bool sametypes = DefNode::eq(other, map);
    if (!sametypes)
        return false;
    
    assert((!fun()->isa<AbsNode>() || fun()->isa<TupleNode>()) 
        && (!other.as<AppNode>()->fun()->isa<AbsNode>() || other.as<AppNode>()->fun()->isa<TupleNode>()) 
        && "an abstraction in fun position in AppNode::eq"
        " contradicts strong normalization (reduction) policy");
    // we have to give up on the assertion due to introduction of tuples
    // now `lam i:2^d. <a, b> i` is irreducible
    return DefNode::eq(other, map)
        && fun()->eq(**other.as<AppNode>()->fun(), map)
        && arg()->eq(**other.as<AppNode>()->arg(), map);
}

/*
 * vhash
 */

size_t BottomNode::vhash() const { return hash_begin(101); }
size_t VarNode::vhash() const { return hash_combine(type() ? type()->hash() : 9, 5); }
size_t PrimLitNode::vhash() const { return hash_combine(value(), 7); }
size_t AbsNode::vhash() const { return hash_combine(var()->hash(), body() ? body()->hash() : 13); }
size_t TupleNode::vhash() const {
    size_t r = hash_begin(42);
    for (auto& d: ops_) {
        hash_combine(r, d->hash());
    }
    return r;
}
size_t DimNode::vhash() const { return hash_combine(hash_begin(197), n_); }
size_t ProjNode::vhash() const { return hash_combine(hash_begin(73), hash_combine(n_, m_)); }
size_t AppNode::vhash() const { return hash_combine(fun()->gid(), arg()->gid()); }

/*
 * is_closed
 */

bool BottomNode::is_closed() const { return true; }
bool AbsNode::is_closed() const { return body(); }
bool VarNode::is_closed() const { return type() ? type()->is_closed() : true; }
bool TupleNode::is_closed() const {
    bool closed = true;
    for (auto& d : ops_) {
        closed &= d->is_closed();
    }
    return closed;
}
bool DimNode::is_closed() const { return true; }
bool ProjNode::is_closed() const { return true; }
bool AppNode::is_closed() const { return fun()->is_closed() && arg()->is_closed(); }

/*
 * update_non_reduced_repr
 */

std::string DefNode::__get_non_reduced_repr (const DefNode& def) const {
    return def.non_reduced_repr_;
}

void DefNode::update_non_reduced_repr() const {
    std::ostringstream r;
    Def(this).dump(r);
    non_reduced_repr_ = r.str();
}

void LambdaNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << "λ";
    __update_non_reduced_repr_body(r);
}

void PiNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << "Π";
    __update_non_reduced_repr_body(r);
}

void AbsNode::__update_non_reduced_repr_body (std::ostringstream& r) const {
    r << __get_non_reduced_repr(**var()) << ":";
    if (var()->type().is_empty())
        r << "'nullptr'";
    else
        r << __get_non_reduced_repr(**var()->type());
    r << ". ";
    if (body().is_empty())
        r << "'nullptr'";
    else
        r << __get_non_reduced_repr(**body());
    
    non_reduced_repr_ = r.str();
}

void TupleNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << "<";
    if (size() > 0)
        r << __get_non_reduced_repr(**ops_[0]);
    for (size_t i = 1; i < ops_.size(); ++i) {
        r << ", " << __get_non_reduced_repr(**ops_[i]);
    }
    r << ">";
    non_reduced_repr_ = r.str();
}

void DimNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << n_ << "ᵈ";
    non_reduced_repr_ = r.str();
}

void ProjNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << "proj{" << m_ << "_" << n_ << "}";
    non_reduced_repr_ = r.str();
}

void AppNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << "(";
    if (fun().is_empty())
        r << "'nullptr'";
    else 
        r << __get_non_reduced_repr(**fun()) << ") (";
    if (arg().is_empty())
        r << "'nullptr'";
    else
        r << __get_non_reduced_repr(**arg()) << ")";
    non_reduced_repr_ = r.str();
}


/*
 * dump
 */

void DefNode::vdump () const {dump(std::cout); std::cout << std::endl; }

void DefNode::dump () const { dump(std::cout); std::cout << std::endl; }

template<class T>
void Proxy<T>::dump (std::ostream& stream) const {
    if (is_empty())
        stream << "'nullptr'";
    else
        deref()->dump(stream);
}

void LambdaNode::dump (std::ostream& stream) const {
    stream << "λ";
    dump_body(stream);
}

void PiNode::dump (std::ostream& stream) const {
    if (body().is_empty()) {
        stream << "Π";
        dump_body(stream);
    } else if (var()->name() == "_" || !body()->has_subexpr(var())) {
        stream << "(";
        var().as<Var>()->type().dump(stream);
        stream << ") -> (";
        body().dump(stream);
        stream << ")";
    } else if (*var().as<Var>()->type() == *world_.get_prim_const("*")) {
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

void TupleNode::dump (std::ostream& stream) const {
    stream << "<";
    if (size() > 0)
        ops_[0].dump(stream);
    for (size_t i = 1; i < ops_.size(); ++i) {
        stream << ", ";
        ops_[i].dump(stream);
    }
    stream << ">";
}

void DimNode::dump(std::ostream& stream) const {
    stream << n_ << "ᵈ";
}

void ProjNode::dump(std::ostream& stream) const {
    stream << "proj{" << m_ << "_" << n_ << "}";
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
