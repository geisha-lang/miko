/// Give names to intermediate values

use std::collections::HashSet;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::ops::Deref;
use std::ops::DerefMut;

use crate::utils::*;
use crate::types::*;
use crate::syntax::form::*;
use crate::internal::*;

use crate::core::term::*;

type Direct = HashMap<Id, Id>;

#[derive(Debug)]
pub struct K<'i> {
    count: usize,
    env: HashMap<Id, Scheme>,
    global: HashMap<Id, P<FunDef>>,
    typedefs: HashMap<Id, P<TypeDef>>,
    current: Id,
    interner: &'i mut Interner,
    direct: Direct,
    /// ADT registry for pattern matching and constructor generation
    adt_registry: AdtRegistry,
}

macro_rules! with_var {
    ($env:expr, $var:expr, $val:expr => $act:block) => {{
        let o = $env.insert($var, $val);
        let ret = $act;
        $env.remove(&$var);
        if let Some(v) = o {
            $env.insert($var.to_owned(), v);
        }
        ret
    }};
}

impl<'i> K<'i> {
    /// Do transformation on a syntax module,
    /// generate core term representation
    pub fn go<I>(module: I,
                 interner: &mut Interner,
                 adt_registry: AdtRegistry)
                 -> (HashMap<Id, P<FunDef>>, HashMap<Id, P<TypeDef>>)
        where I: IntoIterator<Item = Def>
    {
        let current_tmp = interner.intern("");
        let mut runner = K {
            count: 0,
            env: HashMap::new(),
            global: HashMap::new(),
            typedefs: HashMap::new(),
            current: current_tmp,
            direct: HashMap::new(),
            interner,
            adt_registry,
        };
        let defs: Vec<_> = module.into_iter()
            .map(|def| {
                     runner.direct.insert(def.ident, def.ident);
                     def
                 })
            .collect();
        {
            let b = &mut runner;

            for def in defs {
                b.convert_def(def);
            }
        }

        let K { global, typedefs, .. } = runner;
        (global, typedefs)
    }

    /// Get a unique id, then increase the counter
    fn unique(&mut self) -> usize {
        self.count = self.count + 1;
        self.count
    }

    /// Generate a new temporary name
    fn fresh(&mut self) -> String {
        String::from(".tmp") + self.unique().to_string().as_str()
    }

    /// Generate a name for closure
    fn make_cls_name(&mut self, bound: &str) -> Id {
        let mut name = self.interner.trace(self.current).to_owned();
        name += ".inner.closure.";
        name += bound;
        name += self.unique().to_string().as_str();
        self.interner.intern(&name)
    }

    /// Convert a global definition to term
    fn convert_def(&mut self, def: Def) {
        let Def { ident, node, .. } = def;
        let name = self.interner.trace(ident).to_owned();
        match node {
            Item::Form(form) => {
                let form = *form;
                let Form { node, tag } = form;
                let ty = tag.ty;
                self.current = ident;

                match node {
                    Expr::Abs(lambda) => {
                        // We can ignore fvs (global definitions) there
                        // because of type check
                        let (ps, _, bd) = self.trans_lambda(lambda, false);
                        self.define_fn(ident, ty, ps, vec![], bd);
                    }
                    _ => unreachable!(),
                }
            }
            Item::Alg(ps, vs) => {
                let params = ps.into_iter().map(|id| self.interner.trace(id).to_owned()).collect();
                let d = Box::new(TypeDef::new(name.clone(), params, TypeKind::Algebra(vs)));
                self.typedefs.insert(ident, d);
            }
            Item::Alias(ps, t) => {
                let params = ps.into_iter().map(|id| self.interner.trace(id).to_owned()).collect();
                let d = Box::new(TypeDef::new(name.clone(), params, TypeKind::Alias(t)));
                self.typedefs.insert(ident, d);
            }
            Item::Concept { .. } => {
                // Concept definitions are processed during dictionary passing
                // For now, just record that this concept exists
                // The actual dictionary type will be generated later
            }
            Item::Instance { concept_name, type_args, constraints, methods } => {
                // Instance definitions will be transformed to dictionary values
                // For now, generate method implementations as functions
                for method_impl in methods {
                    // Each method implementation becomes a global function
                    // The name is mangled: instance_ConceptName_TypeArgs_methodName
                    let method_name_str = self.interner.trace(method_impl.name).to_owned();
                    let type_args_str: Vec<String> = type_args.iter()
                        .map(|t| t.to_string())
                        .collect();
                    let mangled_name = format!(
                        "instance_{}_{}_{}",
                        concept_name,
                        type_args_str.join("_"),
                        method_name_str
                    );
                    let mangled_id = self.interner.intern(&mangled_name);

                    self.current = mangled_id;

                    // Transform the method body
                    if let Expr::Abs(lambda) = method_impl.body.node.clone() {
                        let (ps, _, bd) = self.trans_lambda(lambda, false);
                        let ty = method_impl.body.tag.ty.clone();
                        self.define_fn(mangled_id, ty, ps, vec![], bd);
                    }
                }
            }
        }
    }

    /// Add a function in top level definitions
    fn define_fn<'c: 'b, 'b>(&'c mut self,
                             name: Id,
                             ty: Scheme,
                             params: Vec<VarDecl>,
                             free: Vec<VarDecl>,
                             body: TaggedTerm)
                             -> (Id, Vec<Id>) {
        let fun = FunDef::new(name, ty, params, free, body);
        let ent = self.global.entry(name).or_insert(P(fun));
        ((*ent).name(),
         (*ent)
             .fv()
             .iter()
             .map(|ref v| v.name())
             .collect())
    }


    /// Get free variables of a term
    fn fv<'a>(&mut self, source: &'a TaggedTerm) -> HashSet<Id> {
        use self::Term::*;
        match *source.body() {
            Term::Lit(_) => HashSet::new(),
            Var(ref v) => {
                let mut hs = HashSet::new();
                hs.insert(v.to_owned());
                return hs;
            }

            List(ref lst) |
            Block(ref lst) |
            ApplyDir(_, ref lst) => {
                lst.iter().fold(HashSet::new(), |mut res, v| {
                    res.extend(self.fv(v.deref()));
                    res
                })
            }

            // The new binding name should not be visible
            // in binding value.
            Let(ref var, ref val, ref exp) => {
                let mut res = self.fv(exp.deref());
                res.remove(&var.name());
                res.extend(&self.fv(val.deref()));
                res
            }
            MakeCls(ref var, ref cls, ref exp) => {
                let mut r = self.fv(exp.deref());
                r.extend(cls.deref().fv());
                r.remove(&var.name());
                r
            }
            ApplyCls(ref n, ref args) => {
                let mut r = args.iter().fold(HashSet::new(), |mut res, v| {
                    res.extend(self.fv(v.deref()));
                    res
                });
                r.extend(self.fv(n.deref()));
                r
            }
            Binary(_, ref lhs, ref rhs) => {
                let mut r = self.fv(lhs.deref());
                r.extend(self.fv(rhs.deref()));
                r
            }
            Unary(_, ref e) => self.fv(e.deref()),
            If(ref c, ref t, ref f) => {
                let mut r = self.fv(c.deref());
                r.extend(self.fv(t.deref()));
                r.extend(self.fv(f.deref()));
                r
            }
            Match(ref scrutinee, ref arms) => {
                let mut r = self.fv(scrutinee.deref());
                for (pat, _guard, body) in arms {
                    let mut arm_fv = self.fv(body.deref());
                    // Remove pattern-bound variables from free variables
                    Self::remove_pattern_bindings(&mut arm_fv, pat);
                    r.extend(arm_fv);
                }
                r
            }
            MakeData { ref fields, .. } => {
                fields.iter().fold(HashSet::new(), |mut res, v| {
                    res.extend(self.fv(v.deref()));
                    res
                })
            }
            GetTag(ref node) | GetField(ref node, _) => self.fv(node.deref()),
        }
    }

    /// Remove variables bound by a pattern from a set
    fn remove_pattern_bindings(fvs: &mut HashSet<Id>, pat: &crate::syntax::form::Pattern) {
        use crate::syntax::form::Pattern::*;
        match pat {
            Var(id) => { fvs.remove(id); }
            Wildcard | Lit(_) => {}
            Constructor(_, pats) => {
                for p in pats {
                    Self::remove_pattern_bindings(fvs, p);
                }
            }
        }
    }

    /// Add a variable in environment
    fn close_var(&mut self, var: Id, ty: Scheme) -> Option<Scheme> {
        self.env.insert(var, ty)
    }
    /// Remove a variable from environment
    fn release_var(&mut self, var: &Id) -> Option<Scheme> {
        self.env.remove(var)
    }

    /// Find if a variable is in environment
    fn find_var(&mut self, var: &Id) -> Option<&Scheme> {
        self.env.get(var)
    }

    /// Collect all variable bindings introduced by a pattern
    fn collect_pattern_bindings(&mut self, pattern: &Pattern) -> Vec<(Id, Scheme)> {
        match pattern {
            Pattern::Var(id) => {
                // Create a fresh type for pattern variable
                // In practice, this type should be constrained by the pattern context
                vec![(*id, Scheme::Slot)]
            }
            Pattern::Wildcard | Pattern::Lit(_) => vec![],
            Pattern::Constructor(_, sub_patterns) => {
                let mut bindings = vec![];
                for sub in sub_patterns {
                    bindings.extend(self.collect_pattern_bindings(sub));
                }
                bindings
            }
        }
    }

    /// Find ADT info by constructor name
    fn find_constructor_info(&self, ctor_name: &str) -> Option<(String, usize)> {
        for (adt_name, adt_info) in &self.adt_registry {
            for variant in &adt_info.variants {
                if variant.name == ctor_name {
                    return Some((adt_name.clone(), variant.tag));
                }
            }
        }
        None
    }

    /// Get parameters, free variables, function body term from lambda
    fn trans_lambda(&mut self, lambda: Lambda, cap_fv: bool) -> (Vec<VarDecl>, Vec<VarDecl>, TaggedTerm) {
        let params = lambda.param;
        let bd = *lambda.body;

        let (body_term, fvs) = {
            let mut present: Vec<Id> = Vec::new();
            let mut backup: Vec<(Id, Scheme)> = Vec::new();

            // Add parameter into env
            {
                let it = params.iter();
                for para in it {
                    let &VarDecl(ref n, ref t) = para;
                    let pname = n.to_owned();
                    if let Some(origin) = self.close_var(pname, t.to_owned()) {
                        backup.push((pname, origin));
                    }
                    present.push(pname);
                }
            }

            let body_term = self.transform(bd);
            let fvs = if cap_fv {
                let mut _fvs = self.fv(&body_term);
                for &VarDecl(ref n, _) in params.iter() {
                    _fvs.remove(n);
                }
                _fvs.iter().map(|vn| VarDecl(vn.to_owned(), self.env[vn].to_owned())).collect()
            } else { vec![] };
            // Reset env
            for bname in present {
                self.release_var(&bname);
            }
            for (bname, bty) in backup {
                self.close_var(bname, bty);
            }
            (body_term, fvs)
        };

        (params, fvs, body_term)
    }

    fn transform_list(&mut self, lst: Vec<P<Form>>) -> Vec<P<TaggedTerm>> {
        lst.into_iter().map(|f| Box::new(self.transform(*f))).collect()
    }

    /// Transform syntax form into core term
    fn transform(&mut self, form: Form) -> TaggedTerm {
        use self::Expr::*;
        let Form { node, tag: FormTag { ty: tform, .. } } = form;
        let t = match node {
            Lit(l) => Term::Lit(l),
            Var(n) => {
                // Check if this is a unit constructor (like None)
                let var_name = self.interner.trace(n).to_string();
                if let Some((adt_name, tag)) = self.find_constructor_info(&var_name) {
                    // Check if it's a unit constructor (no fields)
                    if let Some(adt_info) = self.adt_registry.get(&adt_name) {
                        let variant = adt_info.variants.iter().find(|v| v.name == var_name);
                        if let Some(v) = variant {
                            if v.field_types.is_empty() {
                                // Unit constructor - generate MakeData with no fields
                                return TaggedTerm::new(
                                    tform,
                                    Term::MakeData {
                                        type_name: adt_name,
                                        tag,
                                        fields: vec![],
                                        field_types: vec![],
                                    }
                                );
                            }
                        }
                    }
                }

                // A global definition should not be in scope env
                if self.find_var(&n).is_none() && tform.is_fn() {
                    if let Some(label) = self.direct.get(&n).map(|id| id.to_owned()) {
                        Term::Var(label)
                    } else {
                        // For global function name, make a closure
                        self.direct.insert(n, n);
                        Term::Var(n)
                    }
                } else {
                    Term::Var(n)
                }
            }
            List(e) => Term::List(self.transform_list(e)),
            Block(e) => Term::Block(self.transform_list(e)),
            Unary(op, e) => Term::Unary(op, Box::new(self.transform(*e))),
            If(cond, tr, fl) => {
                Term::If(Box::new(self.transform(*cond)),
                         Box::new(self.transform(*tr)),
                         Box::new(self.transform(*fl)))
            }
            Binary(op, left, right) => {
                Term::Binary(op, Box::new(self.transform(*left)), Box::new(self.transform(*right)))
            }
            Let(v, val, exp) => {
                let VarDecl(id, scm) = v.clone();

                let origin = self.close_var(id, scm.clone());
                let ret = match val.node {
                    Abs(lambda) => {
                        // Handle closure
                        let (cls_name, cls_fv) = {
                            let bound_str = self.interner.trace(id).to_owned();
                            let _cls_name = self.make_cls_name(bound_str.as_str());

                            // to check whether the closure could be call directly.
                            // this shit costs O(N^2) ?
                            let (ps, fv, bd) = with_var!(self.direct, id, _cls_name => {
                                // first assume it could recursive call it self directly
                                self.trans_lambda(lambda.clone(), true)
                            });

                            // if fv is empty, it actually is
                            if fv.is_empty() {
                                self.define_fn(_cls_name, scm, ps, fv, bd)
                            } else {
                                let (ps, fv, bd) = self.trans_lambda(lambda, true);
                                self.define_fn(_cls_name, scm, ps, fv, bd)
                            }
                        };

                        let cls_call_dir = cls_fv.is_empty();

                        let cls = Closure::new(cls_name, cls_fv);
                        let exp_term = if cls_call_dir {
                            with_var!(self.direct, id, cls_name => {
                                self.transform(*exp)
                            })
                        } else {
                            self.transform(*exp)
                        };

                        Term::MakeCls(v, Box::new(cls), Box::new(exp_term))
                    }
                    Var(var_id) => {
                        // Check if this is a constructor first
                        let var_name = self.interner.trace(var_id).to_string();
                        if self.find_constructor_info(&var_name).is_some() {
                            // Constructor - use the normal path
                            let val_term = self.transform(*val);
                            let exp_term = self.transform(*exp);
                            Term::Let(v, Box::new(val_term), Box::new(exp_term))
                        } else {
                            // Regular variable - original logic
                            let val_term = self.transform(*val);
                            let exp_term = if self.find_var(&var_id) == None {
                                let origin = if let Some(label) =
                                    self.direct.get(&var_id).map(|l| l.to_owned()) {
                                    self.direct.insert(var_id, label.to_owned())
                                } else {
                                    eprintln!("variable not fount: {}", self.interner.trace(var_id));
                                    panic!("variable not fount");
                                };
                                let exp_term = self.transform(*exp);
                                if let Some(o) = origin {
                                    self.direct.insert(var_id, o);
                                }
                                exp_term
                            } else {
                                self.transform(*exp)
                            };
                            Term::Let(v, Box::new(val_term), Box::new(exp_term))
                        }
                    }
                    _ => {
                        // Normal variable binding
                        let val_term = self.transform(*val);
                        let exp_term = self.transform(*exp);
                        Term::Let(v, Box::new(val_term), Box::new(exp_term))
                    }
                };
                if let Some(v) = origin {
                    self.close_var(id, v);
                }
                ret
            }

            Apply(callee, params) => {
                // Check if callee is a constructor
                if let Expr::Var(callee_id) = &callee.node {
                    let callee_name = self.interner.trace(*callee_id).to_string();
                    if let Some((adt_name, tag)) = self.find_constructor_info(&callee_name) {
                        // This is a constructor application - generate MakeData
                        let params_term = self.transform_list(params);
                        // Extract concrete field types from the transformed parameters
                        let field_types: Vec<Type> = params_term.iter()
                            .map(|t| t.ref_scheme().body().clone())
                            .collect();
                        return TaggedTerm::new(
                            tform,
                            Term::MakeData {
                                type_name: adt_name,
                                tag,
                                fields: params_term,
                                field_types,
                            }
                        );
                    }
                }

                // Regular function application
                let callee_term = self.transform(*callee);
                let params_term = self.transform_list(params);

                match callee_term.body() {
                    &Term::Var(n) => {
                        if let Some(label) = self.direct.get(&n) {
                            Term::ApplyDir(VarDecl(label.to_owned(), callee_term.ref_scheme().clone()),
                                           params_term)
                        } else {
                            Term::ApplyCls(Box::new(callee_term), params_term)
                        }
                    }
                    _ => Term::ApplyCls(Box::new(callee_term), params_term)
                }
            }

            // Match expression
            Match(ref scrutinee, ref arms) => {
                // Transform scrutinee
                let scrutinee_term = self.transform(*scrutinee.clone());

                // Transform match arms with proper pattern bindings
                let arms_term: Vec<_> = arms.iter().map(|arm| {
                    // Collect pattern bindings and add them to environment
                    let bindings = self.collect_pattern_bindings(&arm.pattern);

                    // Add bindings to environment
                    let mut backup: Vec<(Id, Scheme)> = Vec::new();
                    for (id, scheme) in &bindings {
                        if let Some(old) = self.close_var(*id, scheme.clone()) {
                            backup.push((*id, old));
                        }
                    }

                    // Transform body with bindings in scope
                    let body_term = self.transform(*arm.body.clone());

                    // Restore environment
                    for (id, _) in &bindings {
                        self.release_var(id);
                    }
                    for (id, scheme) in backup {
                        self.close_var(id, scheme);
                    }

                    (arm.pattern.clone(), arm.guard.clone(), Box::new(body_term))
                }).collect();

                Term::Match(Box::new(scrutinee_term), arms_term)
            }
            // Give anonymous lambda a name binding
            Abs(lambda) => {
                let ty = tform.clone();
                let tmp_id = {
                    let fr_name = self.fresh();
                    self.make_cls_name(fr_name.as_str())
                };
                let (ps, fv, bd) = self.trans_lambda(lambda, true);

                let (cls_name, cls_fv) = self.define_fn(tmp_id, ty.clone(), ps, fv, bd);
                let cls = Closure::new(cls_name, cls_fv);

                Term::MakeCls(VarDecl(tmp_id, ty.clone()),
                              Box::new(cls),
                              Box::new(TaggedTerm::new(ty, Term::Var(tmp_id))))
            }
        };
        TaggedTerm::new(tform, t)
    }
}
