#include <algorithm>
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
template class Proxy<SigmaNode>;
template class Proxy<PairNode>;
template class Proxy<TupleNode>;
template class Proxy<AbsRecordNode>;
template class Proxy<InstRecordNode>;
template class Proxy<DimNode>;
template class Proxy<RecordDimNode>;
template class Proxy<ProjNode>;
template class Proxy<RecordProjNode>;
template class Proxy<DummyNode>;
template class Proxy<AppNode>;


/* ----------------------------------------------------
 * DefNode
 * ------------------------------------------------- */

void DefNode::set_op(size_t i, Def def) const {
    assert(!op(i) && "already set");
   // assert(def && "setting null pointer");
    if (!def) {
        // do nothing; we have to do this in order for var(nullptr, nullptr)
        // to work (that's how star and box are constructed, for instance)
    } else {
        assert(i < size() && "index out of bounds");
        ops_[i] = *def;
    }
}

void DefNode::unlink_representative() const {
    if (is_proxy()) {
        auto num = representative_->representative_of_.erase(this);
        assert(num == 1);
    }
    representative_ = this;
}

void DefNode::set_representative(const DefNode* repr) const {
    unlink_representative();
    representative_ = repr;
    if (this != repr) {
        repr->representative_of_.insert(this);
        repr->non_reduced_repr_ = non_reduced_repr();
    }
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
            if (def == sub)
                return true;
        }

        for (auto op : def->ops_)
            enqueue(op);
    }

    return false;
}

void AbsNode::close(Def body) const {
    assert(body->is_closed() && "closing AbsNode with unclosed body");
    set_op(1, body);
    world_.introduce(this);
}

Array<Def> TupleNode::elem_types() const {
    Array<Def> result(size());
    for (size_t i = 0, e = size(); i != e; ++i)
        result[i] = op(i)->type();
    return result;
}

/*
 * constructors
 */

DefNode::DefNode(World& world, size_t gid, ArrayRef<Def> ops, std::string name)
    : representative_(this)
    , world_(world)
    , ops_(ops.size())
    , gid_(gid)
    , name_(name)
{
    for (size_t i = 0, e = ops.size(); i != e; ++i) {
        if (auto op = ops[i])
            set_op(i, ops[i]);
    }
}

AbsNode::AbsNode(World& world, size_t gid, Def var_type, std::string name)
    : DefNode(world, gid, { new VarNode(world, gid + 1, var_type, this, name), nullptr }, name)
{
    var()->update_non_reduced_repr();
}

TupleNode::TupleNode(World& world, size_t gid, ArrayRef<Def> elems, std::string name)
    : DefNode(world, gid, elems, name)
{}

AbsRecordNode::AbsRecordNode(World& world, size_t gid, thorin::Array<std::pair<std::string, Def> > label2type, std::string name)
    : DefNode(world, gid, {}, name)
    , label2type_{label2type}
    , fields_(label2type.size())
    , reali2lexi(label2type.size())
{
    size_t i = 0;
    for(auto& p : label2type) {
        fields_[i] = Field(p.first, i, this);
        ++i;
    }
    std::sort(fields_.begin(), fields_.end(), [] (const Field& f1, const Field& f2) {
        return f1.label() < f2.label();
    });
    
    for(size_t i = 0; i < label2type.size(); ++i) {
        reali2lexi[fields_[i].index()] = i;
    }
}

InstRecordNode::InstRecordNode(World& world, size_t gid, thorin::Array<std::string> labels, 
    thorin::Array<Def> elems, AbsRecord ascribed_type, std::string name)
    : DefNode(world, gid, elems, name)
    , ascribed_type_(ascribed_type)
    , labels_(labels)
    , reali2lexi(elems.size())
{
    std::vector<Field> lexlab;
    size_t i = 0;
    for(auto& l : labels) {
        lexlab.push_back(Field(l, i, nullptr));
        ++i;
    }
    std::sort(lexlab.begin(), lexlab.end(), [] (const Field& f1, const Field& f2) {
        return f1.label() < f2.label();
    });
    for(auto& f : lexlab) {
        lexi2reali.push_back(f.index());
    }
    
    for(size_t i = 0; i < lexi2reali.size(); ++i) {
        reali2lexi[lexi2reali[i]] = i;
    }
}

/*
 * destructors
 */

AbsNode::~AbsNode() { delete *var(); }

/*
 * free_vars
 */ 

DefSet DefNode::free_vars() const {
    return DefSet();
}

DefSet AbsNode::free_vars() const {
    auto r = body()->free_vars();
    r.erase(var());
    return r;
}

DefSet TupleNode::free_vars() const {
    DefSet r;
    for(auto& d : ops_) {
        auto fv = d->free_vars();
        r.insert(fv.begin(), fv.end());
    }
    return r;
}

DefSet InstRecordNode::free_vars() const {
    DefSet r;
    for(auto& d : ops_) {
        auto fv = d->free_vars();
        r.insert(fv.begin(), fv.end());
    }
    return r;
}

DefSet VarNode::free_vars() const {
    return DefSet { this };
}

DefSet PairNode::free_vars() const {
    auto r1 = first()->free_vars();
    auto r2 = second()->free_vars();
    r1.insert(r2.begin(), r2.end());
    return r1;
}

DefSet AppNode::free_vars() const {
    auto r1 = fun()->free_vars();
    auto r2 = arg()->free_vars();
    r1.insert(r2.begin(), r2.end());
    return r1;
}

/*
 * reduce
 */
 
Def DefNode::__reduce(const DefNode& def, Def2Def& map) const {
    return def.vreduce(map);
}

Def DefNode::reduce(Def2Def map, bool replace) const {
    assert(is_closed() && "unclosed def in reduce");
    auto res = vreduce(map);
    if(replace)
        set_representative(*res);
    
    return res;
}

Def AbsNode::vreduce(Def2Def& map) const {
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
        nabs = world_.lambda(ntype, nvarn.str());
    else if(this->isa<AbsNode>())
        nabs = world_.pi(ntype, nvarn.str());
    else if(this->isa<SigmaNode>())
        nabs = world_.sigma(ntype, nvarn.str());
    else
        assert(false && "AbsNode::vreduce has unknown Abs- node");
    map[*var()] = *nabs->var();
    auto nbody = __reduce(**body(), map);
    nabs->close(nbody);
    return nabs;
}

Def TupleNode::vreduce(Def2Def& map) const {
    bool changed = false;
    std::vector<Def> nops;
    for (auto& d : ops_) {
        auto nd = __reduce(**d, map);
        nops.push_back(nd);
        changed &= *nd == *d;
    }
    if (changed) {
        return world_.tuple(nops);
    } else
        return this;
}

Def PairNode::vreduce(Def2Def& map) const {
    bool changed = false;
    std::vector<Def> nops;
    for (auto& d : ops_) {
        auto nd = __reduce(**d, map);
        nops.push_back(nd);
        changed &= *nd == *d;
    }
    if (changed) {
        return world_.pair(nops[0], nops[1], ascribed_type_);
    } else
        return this;
}

Def AbsRecordNode::vreduce(Def2Def& map) const { return this; } // types should be already reduced?

Def InstRecordNode::vreduce(Def2Def& map) const {
    bool changed = false;
    thorin::Array<Def> nops(size());
    thorin::Array<std::pair<std::string, Def> > nl2d(size());
    size_t i = 0;
    for (auto& d : ops_) {
        auto nd = __reduce(**d, map);
        nl2d[i] = std::make_pair(labels_[i], nd);
        changed &= *nd == *d;
        ++i;
    }
    if (changed) {
        return world_.inst_record(nl2d, ascribed_type_);
    } else
        return this;
}

Def DimNode::vreduce(Def2Def&) const { return this; }
Def RecordDimNode::vreduce(Def2Def&) const { return this; }
Def ProjNode::vreduce(Def2Def&) const { return this; }
Def RecordProjNode::vreduce(Def2Def&) const { return this; }
Def BottomNode::vreduce(Def2Def&) const { return this; }

Def VarNode::vreduce(Def2Def& map) const {
    auto i = map.find(this);
    return i != map.end() ? i->second : this;
}

Def PrimLitNode::vreduce(Def2Def& map) const {
    return this;
}

Def DummyNode::vreduce(Def2Def& map) const {
    return this; // it's app of lambda that is responsible for reducing its damn dummy body! ;)
}

Def AppNode::vreduce(Def2Def& map) const {
    Def rfun = __reduce(**fun(), map);
    Def rarg = __reduce(**arg(), map);
    if (auto tup = rfun.isa<Tuple>()) {
        if (auto proj = rarg.isa<Proj>()) {
            return tup->op(proj->m());
        } else if (*rfun != *fun())
            return world_.app(rfun, rarg);
        else
            return this;
    } else if (auto absrec = rfun.isa<AbsRecord>()) {
        if (auto rproj = rarg.isa<RecordProj>()) {
            return absrec->label2type()[rproj->field().index()].second;
        } else if (*rfun != *fun())
            return world_.app(rfun, rarg);
        else
            return this;
    } else if (auto instrec = rfun.isa<InstRecord>()) {
        if (auto rproj = rarg.isa<RecordProj>()) {
            auto absreali = rproj->field().index();
            auto abslexi = rproj->field().owner()->reali2lexi[absreali];
            auto instreali = instrec->lexi2reali[abslexi];
            return instrec->op(instreali);
        } else if (*rfun != *fun())
            return world_.app(rfun, rarg);
        else
            return this;
    } else if (auto sig = rfun.isa<Pair>()) {
        if(rarg == world_.get_prim_const("𝟙").as<Def>())
            return sig->first();
        else if(rarg == world_.get_prim_const("𝟚").as<Def>())
            return sig->second();
        else if (*rfun != *fun())
            return world_.app(rfun, rarg);
        else
            return this;
    } else if (auto abs = rfun.isa<Abs>()) {
        if (auto dummyb = abs->body().isa<Dummy>()) {
            auto fv = rarg->free_vars();
            for(auto& kv : map) {
                fv.erase(kv.first);
            }
            if (/*rarg->free_vars()*/fv.empty() && dummyb->is_reducable()) {
                // TODO: triple-check if that makes sense :)
                auto f = dummyb->body_;
                return f(rarg);
            } else {
                goto cannotdumuchappnodereduce;                
            }
        } else {
            map[*abs->var()] = *rarg;
            return __reduce(**abs->body(), map);
        }
    } else {

cannotdumuchappnodereduce:
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
    auto body_type = body()->type();
    std::ostringstream nvarn;
    nvarn << var()->name();
    if (nvarn.str() != "_")
        nvarn << "'";
    auto res = world_.pi(var()->type(), nvarn.str());
    auto body_type2 = body_type->reduce({{var(), res->var()}}, false);
    res->close(body_type2);
    return res;
}

Def PiNode::typecheck() const {
    auto var_type = var()->type();
    auto var_type_type = var_type->type();
    auto body_type = body()->type();
    auto p = world_.wavy_arrow_rules.find(std::make_pair(
        *var_type_type,
        *body_type)
    );
    if (p != world_.wavy_arrow_rules.end()) {
        return p->second;
    }
    else {
        std::ostringstream msg;
        msg << "Π: no wavy_arrow rule for " << var_type_type->name();
        msg << " ⤳  " << body_type->name();
        return world_.bottom(msg.str());
    }
}

Def TupleNode::typecheck() const {
    for (size_t i = 0; i < ops_.size(); ++i) {
        if (ops_[i]->isa<BottomNode>()) {
            std::ostringstream msg;
            msg << "tuple ";
            vdump(msg); msg << " has bottom type as " << i << " elem";
            return world_.bottom(msg.str());
        }
    }
    
    auto t = world_.pi(world_.dimension(size()), "i");
    auto b = world_.app(world_.tuple(elem_types()), t->var());
    t->close(b);
    return t;
}

Def AbsRecordNode::typecheck() const {
    for (auto& p : label2type_)
        if (p.second->isa<BottomNode>()) {
            std::ostringstream msg;
            msg << "AbsRecord ";
            vdump(msg); msg << " has bottom type at field '" << p.first << "'";
            return world_.bottom(msg.str());
        }
    
    auto t = world_.pi(world_.record_dimension(this), "i");
    thorin::Array<std::pair<std::string, Def> > nl2t(label2type_.size());
    size_t i = 0;
    for (auto& p : label2type_) {
        nl2t[i] = std::make_pair(p.first, p.second->type());
        ++i;
    }
    
    auto b = world_.app(world_.abs_record(nl2t), t->var());
    t->close(b);
    return t;
}

Def InstRecordNode::typecheck() const {
    auto ASCT = ascribed_type_;
    for(auto& f : ASCT->get_fields()) {
        auto i = world_.record_projection(f);
        auto ascrt = world_.app(ascribed_type_, i);
        auto insti = lexi2reali[ASCT->reali2lexi[f.index()]];
        if(f.label() != labels_[insti]) {
            std::ostringstream msg;
            msg << "incorrect ascribtion: fields' names don't match --";
            msg << "in "; vdump(msg); msg << " # "; ASCT.dump(msg);
            return world_.bottom(msg.str());
        } else if(ascrt != op(insti)->type()) {
            std::ostringstream msg;
            msg << "incorrect ascribtion: types of fields don't match --";
            msg << "in "; vdump(msg); msg << " # "; ASCT.dump(msg);
            return world_.bottom(msg.str());
        }
    }
    
    //return ascribed_type_;
    // we need to adjust the type to be a Pi -- otherwise projections won't work!
    auto t = world_.pi(world_.record_dimension(ASCT), "i");
    t->close(world_.app(ASCT, t->var()));
    return t;
}

Def SigmaNode::typecheck() const {
    auto var_type = var()->type();
    auto var_type_type = var_type->type();
    auto body_type = body()->type();
    auto p = world_.wavy_arrow_rules.find(std::make_pair(
        *var_type_type,
        *body_type)
    );
    if (p != world_.wavy_arrow_rules.end()) {
        return p->second;
    }
    else {
        std::ostringstream msg;
        msg << "Σ: no wavy_arrow rule for " << var_type_type->name();
        msg << " ⤳  " << body_type->name();
        return world_.bottom(msg.str());
    }
}

Def PairNode::typecheck() const {
    Def redb = ascribed_type_->body()->reduce({{ascribed_type_->var(), first()}} , false);
    if(first()->type() != ascribed_type_->var()->type()) {
        std::ostringstream msg;
        msg << "incorrect ascribtion: first element's type (";
        first()->type().dump(msg); msg << ") differs from sigma's var type (";
        ascribed_type_->var()->type().dump(msg); msg << ")";
        return world_.bottom(msg.str());
    } else if(second()->type() != redb) {
        std::ostringstream msg;
        msg << "incorrect ascribtion: second element's type (";
        second()->type().dump(msg); msg << ") differs from sigma's body after substitution (";
        redb.dump(msg); msg << ")";
        return world_.bottom(msg.str());
    } else
        return ascribed_type_;
}

Def DimNode::typecheck() const {
    return world_.get_prim_const("D");
}

Def RecordDimNode::typecheck() const {
    return world_.get_prim_const("RD");
}

Def ProjNode::typecheck() const {
    return world_.dimension(n_);
}

Def RecordProjNode::typecheck() const {
    return world_.record_dimension(field_.owner());
}

Def BottomNode::typecheck() const {
    return this;
}

Def VarNode::typecheck() const {
    return type();
}

Def PrimLitNode::typecheck() const {
    return type();
}

Def DummyNode::typecheck() const {
    return arg_type_;
}

Def AppNode::typecheck() const {
    auto funt = fun()->type();
    auto argt = arg()->type();
    if (auto pifunt = funt.isa<Pi>()) {
        if (auto tup = fun().isa<Tuple>()) {
            if (auto argv = arg().isa<Var>()) {
                auto dim = world_.dimension(tup->size());
                Dim argtd;
                if ((/*auto*/ argtd = argv->type().isa<Dim>()) && dim == argtd) {
                   // if (dim == argv->type()) {
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
                    return tup->op(argp->m())->type();
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
        } else if(auto rec = fun().isa<AbsRecord>()) {
            if (auto argv = arg().isa<Var>()) {
                auto recdim = world_.record_dimension(rec);
                RecordDim argtd;
                if ((argtd = argv->type().isa<RecordDim>()) && recdim == argtd) {
                    thorin::Array<std::pair<std::string, Def> > nl2t(rec->label2type().size());
                    size_t i = 0;
                    for(auto& p : rec->label2type_) {
                        nl2t[i] = std::make_pair(p.first, p.second->type());
                        ++i;
                    }
                    
                    return world_.app(world_.abs_record(nl2t), argv);
                } else {
                    std::ostringstream msg;
                    msg << "in application: (";
                    fun().dump(msg); msg << ") ("; arg().dump(msg);
                    msg << ") -- arg is not correct RecordProj";
                    return world_.bottom(msg.str());
                }
                
            } else if (auto argp = arg().isa<RecordProj>()) {
                auto f = argp->field();
                if(f.owner() == rec) {
                    return rec->label2type_[f.index()].second->type();
                } else {
                    std::ostringstream msg;
                    msg << "in application: (";
                    fun().dump(msg); msg << ") ("; arg().dump(msg);
                    msg << ") -- arg is not RecordProj of same Record";
                    return world_.bottom(msg.str());
                }
            } else {
                std::ostringstream msg;
                msg << "in application: (";
                fun().dump(msg); msg << ") ("; arg().dump(msg);
                msg << ") -- argument is neither Var nor RecordProj";
                return world_.bottom(msg.str());
            }
        } else if(auto rec = fun().isa<InstRecord>()) {
            if (auto argv = arg().isa<Var>()) {
                auto recdim = world_.record_dimension(rec->ascribed_type_);
                RecordDim argtd;
                if ((argtd = argv->type().isa<RecordDim>()) && recdim == argtd) {
                    return world_.app(rec->ascribed_type_, argv);
                } else {
                    std::ostringstream msg;
                    msg << "in application: (";
                    fun().dump(msg); msg << ") ("; arg().dump(msg);
                    msg << ") -- arg is not correct RecordProj";
                    return world_.bottom(msg.str());
                }
                
            } else if (auto argp = arg().isa<RecordProj>()) {
                auto f = argp->field();
                if(f.owner() == rec->ascribed_type_) {
                    return rec->ascribed_type_->label2type_[f.index()].second;
                } else {
                    std::ostringstream msg;
                    msg << "in application: (";
                    fun().dump(msg); msg << ") ("; arg().dump(msg);
                    msg << ") -- arg is not RecordProj of same Record";
                    return world_.bottom(msg.str());
                }
            } else {
                std::ostringstream msg;
                msg << "in application: (";
                fun().dump(msg); msg << ") ("; arg().dump(msg);
                msg << ") -- argument is neither Var nor RecordProj";
                return world_.bottom(msg.str());
            }
        } else if (pifunt->var()->type() == argt) {
            return pifunt->body()->reduce({{pifunt->var(), arg()}}, false);
        } else {
            std::ostringstream msg;
            msg << "in application: (";
            fun().dump(msg); msg << ") ("; arg().dump(msg);
            msg << ") -- type of argument (";
            argt.dump(msg); msg << ") != type of fun's var (";
            pifunt->var()->type().dump(msg);
            msg << ")";
            return world_.bottom(msg.str());
        }
    } else if (auto sigt = funt.isa<Sigma>()) {
        if(argt == world_.get_prim_const("Σ¹").as<Def>()) {
            return sigt->var()->type();
        } else if(argt == world_.get_prim_const("Σ²").as<Def>()) {
            return sigt->body()->reduce(
                {{sigt->var(), world_.app(fun(), world_.get_prim_const("𝟙"))}}, false
            );
        } else {
            std::ostringstream msg;
            msg << "in application: (";
            fun().dump(msg); msg << ") ("; arg().dump(msg);
            msg << ") -- type of argument (";
            argt.dump(msg); msg << ") is neither Σ¹ nor Σ²";
            return world_.bottom(msg.str());
        }
    } else {
        std::ostringstream msg;
        msg << "in application: (";
        fun().dump(msg); msg << ") ("; arg().dump(msg);
        msg << ") -- type of fun is neither Pi, nor Sigma, but: ";
        funt.dump(msg);
        return world_.bottom(msg.str());
    }
}


/*
 * equal
 */

bool DefNode::equal(const DefNode& other) const {
    Def2Def map;
    return this->eq(other, map);
}

bool DefNode::eq(const DefNode& other, Def2Def& map) const {
    return typeid(*this) == typeid(other);
}

bool BottomNode::eq(const DefNode& other, Def2Def& map) const {
    return this == &other;
}

bool VarNode::eq(const DefNode& other, Def2Def& map) const {
    auto eqto = map[this];
    return DefNode::eq(other, map) && (this == &other || eqto == &other);
}

bool PrimLitNode::eq(const DefNode& other, Def2Def& map) const {
    return DefNode::eq(other, map) && value() == other.as<PrimLitNode>()->value();
}

bool AbsNode::eq(const DefNode& other, Def2Def& map) const {
    
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

bool TupleNode::eq(const DefNode& other, Def2Def& map) const {
    if (DefNode::eq(other, map) && size() == other.size()) {
        bool compeq = true;
        for (size_t i = 0; i < size(); ++i) {
            compeq &= ops_[i]->eq(**other.op(i), map);
        }
        return compeq;
    }
    return false;
}

bool PairNode::eq(const DefNode& other, Def2Def& map) const {
    return DefNode::eq(other, map) && ops_[0]->eq(**other.as<PairNode>()->first(), map)
        && ops_[1]->eq(**other.as<PairNode>()->second(), map)
        && ascribed_type_->eq(**other.as<PairNode>()->ascribed_type_, map);
}

bool AbsRecordNode::eq(const DefNode& other, Def2Def& map) const {
    return this == &other;
}

bool InstRecordNode::eq(const DefNode& other, Def2Def& map) const {
    return this == &other;
}

bool DimNode::eq(const DefNode& other, Def2Def& map) const {
    return DefNode::eq(other, map) && n_ == other.as<DimNode>()->n_;
}

bool RecordDimNode::eq(const DefNode& other, Def2Def& map) const {
    return DefNode::eq(other, map) && of_record_->eq(**other.as<RecordDimNode>()->of_record_, map);
}

bool ProjNode::eq(const DefNode& other, Def2Def& map) const {
    return DefNode::eq(other, map) && n_ == other.as<ProjNode>()->n_
        && m_ == other.as<ProjNode>()->m_;
}

bool RecordProjNode::eq(const DefNode& other, Def2Def& map) const {
    return DefNode::eq(other, map) && field_.index() == other.as<RecordProjNode>()->field_.index()
        && field_.owner()->eq(**other.as<RecordProjNode>()->field_.owner(), map);
}

bool DummyNode::eq(const DefNode& other, Def2Def& map) const {
    return this == &other;
}

bool AppNode::eq(const DefNode& other, Def2Def& map) const {
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
    for (auto& d: ops_)
        r = hash_combine(r, d->hash());
    return r;
}
size_t AbsRecordNode::vhash() const {
    size_t r = hash_begin(634);
    for (auto& kv : label2type_)
        r = hash_combine(r, kv.second->hash());
    return r;
}
size_t InstRecordNode::vhash() const {
    size_t r = hash_begin(108);
    for (auto& d : ops_)
        r = hash_combine(r, d->hash());
    return r;
}
size_t PairNode::vhash() const {
    return hash_combine(2357, hash_combine(first()->hash(), second()->hash()));
}
size_t DimNode::vhash() const { return hash_combine(hash_begin(197), n_); }
size_t RecordDimNode::vhash() const { return hash_begin(777); }
size_t ProjNode::vhash() const { return hash_combine(hash_begin(73), hash_combine(n_, m_)); }
size_t RecordProjNode::vhash() const { return hash_begin(931); }
size_t DummyNode::vhash() const { return hash_combine(hash_begin(55), hash_combine(arg_type()->hash(), return_type()->hash())); }
size_t AppNode::vhash() const { return hash_combine(fun()->hash(), arg()->hash()); }

/*
 * is_closed
 */

bool BottomNode::is_closed() const { return true; }
bool AbsNode::is_closed() const { return body(); }
bool VarNode::is_closed() const { return type() ? type()->is_closed() : true; }
bool PrimLitNode::is_closed() const { return type() ? type()->is_closed() : true; }
bool TupleNode::is_closed() const {
    bool closed = true;
    for (auto& d : ops_) {
        closed &= d->is_closed();
    }
    return closed;
}
bool PairNode::is_closed() const {
    return first()->is_closed() && second()->is_closed();
}
bool AbsRecordNode::is_closed() const {
    /*bool closed = true;
    for (auto& kv : label2type_)
        closed &= kv.second->is_closed();
    return closed;*/
    return true;
}
bool InstRecordNode::is_closed() const {
    bool closed = true;
    for (auto& d : ops_)
        closed &= d->is_closed();
    return closed;
}
bool DimNode::is_closed() const { return true; }
bool RecordDimNode::is_closed() const { return true; }
bool ProjNode::is_closed() const { return true; }
bool RecordProjNode::is_closed() const { return true; }
bool DummyNode::is_closed() const { return true; } // yes, true!
bool AppNode::is_closed() const { return fun()->is_closed() && arg()->is_closed(); }

/*
 * update_non_reduced_repr
 */

std::string DefNode::__get_non_reduced_repr(const DefNode& def) const {
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

void SigmaNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << "Σ";
    __update_non_reduced_repr_body(r);
}

void AbsNode::__update_non_reduced_repr_body(std::ostringstream& r) const {
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

void PairNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << "|" << __get_non_reduced_repr(**ops_[0]);
    r << ", " << __get_non_reduced_repr(**ops_[1]);
    r << "|";
    non_reduced_repr_ = r.str();
}

void AbsRecordNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << "AbsRecord{";
    size_t i = 0;
    for(auto& kv : label2type_) {
        if(i != 0) {
            r << "; ";
        }
        r << "'" << kv.first << "': " << __get_non_reduced_repr(**kv.second);
        ++i;
    }
    r << "}";
    non_reduced_repr_ = r.str();
}

void InstRecordNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << "InstRecord{";
    for(size_t i = 0; i < size(); ++i) {
        if(i != 0) {
            r << "; ";
        }
        r << "'" << labels_[i] << "': " << __get_non_reduced_repr(**ops_[i]);
    }
    r << "}";
    non_reduced_repr_ = r.str();
}

void DimNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << n_ << "ᵈ";
    non_reduced_repr_ = r.str();
}

void RecordDimNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << __get_non_reduced_repr(**of_record_) << "ᵈ";
    non_reduced_repr_ = r.str();
}

void ProjNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << "proj{" << m_ << "_" << n_ << "}";
    non_reduced_repr_ = r.str();
}

void RecordProjNode::update_non_reduced_repr() const {
    std::ostringstream r;
    /*size_t i = 0;
    r << "recproj{" << label_ << ", {";
    for (auto& s : labels_) {
        if(i != 0)
            r << ", ";
        r << s;
        ++i;
    }
    r << "}}";*/
    r << "recproj{" << field_.label() << "}";
    non_reduced_repr_ = r.str();
}

void DummyNode::update_non_reduced_repr() const {
    std::ostringstream r;
    r << "Dummy{(" << __get_non_reduced_repr(**arg_type());
    r << ") -> (" << __get_non_reduced_repr(**return_type()) << ")}";
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

template<class T>
void Proxy<T>::dump(std::ostream& stream) const {
    if (is_empty())
        stream << "'nullptr'";
    else
        deref()->vdump(stream);
}

void DefNode::dump() const { vdump(std::cout); std::cout << std::endl; }

void LambdaNode::vdump(std::ostream& stream) const {
    stream << "λ";
    dump_body(stream);
}

void PiNode::vdump(std::ostream& stream) const {
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

void SigmaNode::vdump(std::ostream& stream) const {
    if (body().is_empty()) {
        stream << "Σ";
        dump_body(stream);
    } else if (var().as<Var>()->type().as<Def>() == world_.get_prim_const("*").as<Def>()
        && var().as<Def>() == body().as<Def>()) {
        stream << "∃" << var()->name() << ". ";
        body().dump(stream);
    } else {
        stream << "Σ";
        dump_body(stream);
    }
}

void AbsNode::dump_body(std::ostream& stream) const {
    var().dump(stream);
    stream << ":";
    var()->type().dump(stream);
    stream << ". ";
    body().dump(stream);
}

void TupleNode::vdump(std::ostream& stream) const {
    stream << "<";
    if (size() > 0)
        ops_[0].dump(stream);
    for (size_t i = 1; i < ops_.size(); ++i) {
        stream << ", ";
        ops_[i].dump(stream);
    }
    stream << ">";
}

void PairNode::vdump(std::ostream& stream) const {
    stream << "|";
    ops_[0].dump(stream);
    stream << ", ";
    ops_[1].dump(stream);
    stream << "|";
}

void AbsRecordNode::vdump(std::ostream& stream) const {
    stream << "AbsRecord{";
    size_t i = 0;
    for(auto& kv : label2type_) {
        if(i != 0) {
            stream << "; ";
        }
        stream << "'" << kv.first << "': ";
        kv.second.dump(stream);
        ++i;
    }
    stream << "}";
}

void InstRecordNode::vdump(std::ostream& stream) const {
    stream << "InstRecord{";
   for(size_t i = 0; i < size(); ++i) {
        if(i != 0) {
            stream << "; ";
        }
        stream << "'" << labels_[i] << "': ";
        ops_[i].dump(stream);
    }
    stream << "}";
}

void DimNode::vdump(std::ostream& stream) const {
    stream << n_ << "ᵈ";
}

void RecordDimNode::vdump(std::ostream& stream) const {
    of_record_.dump(stream);
    stream << "ᵈ";
}

void ProjNode::vdump(std::ostream& stream) const {
    stream << "proj{" << m_ << "_" << n_ << "}";
}

void RecordProjNode::vdump(std::ostream& stream) const {
   /* size_t i = 0;
    stream << "recproj{" << label_ << ", {";
    for (auto& s : labels_) {
        if(i != 0)
            stream << ", ";
        stream << s;
        ++i;
    }
    stream << "}}";*/
    stream << "recproj{" << field_.label() << "}";
}

void BottomNode::vdump(std::ostream& stream) const {
    stream << name() << " { " << info() << " }";
}

void VarNode::vdump(std::ostream& stream) const {
    stream << name();
}

void PrimLitNode::vdump(std::ostream& stream) const {
    stream << value();
}

void DummyNode::vdump(std::ostream& stream) const {
    stream << "Dummy{(";
    arg_type().dump(stream);
    stream << ") -> (";
    return_type().dump(stream);
    stream << ")}";
}

void AppNode::vdump(std::ostream& stream) const {
    stream << "(";
    fun().dump(stream);
    stream << ") (";
    arg().dump(stream);
    stream << ")";
}

}
