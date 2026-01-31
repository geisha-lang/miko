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
use crate::typeinfer::mono::{InstantiationRegistry, Instantiation};

use crate::core::term::*;

type Direct = HashMap<Id, Id>;

/// Maps from original function name to the specialized name for a given call site
type SpecializationMap = HashMap<Name, HashMap<Vec<Type>, Name>>;

/// Information about a specialized function call
#[derive(Clone, Debug)]
struct SpecializedCall {
    mangled_name: Name,
    specialized_scheme: Scheme,
}

/// Check if a type is fully concrete (no type variables)
fn is_type_concrete(ty: &Type) -> bool {
    match ty {
        Type::Var(_) => false,
        Type::Con(_) | Type::Void => true,
        Type::Arr(p, r) => is_type_concrete(p) && is_type_concrete(r),
        Type::Prod(l, r) => is_type_concrete(l) && is_type_concrete(r),
        Type::Comp(base, arg) => is_type_concrete(base) && is_type_concrete(arg),
    }
}

/// Extract type arguments from a Comp type chain
/// E.g., Comp(Comp(Map, k), v) -> [k, v]
/// For a simple type like Con("Int"), returns empty vec
fn extract_type_args(ty: &Type) -> Vec<Type> {
    let mut args = Vec::new();
    let mut current = ty;
    while let Type::Comp(base, arg) = current {
        args.insert(0, arg.as_ref().clone());
        current = base.as_ref();
    }
    args
}

/// Unify two types, adding bindings to the substitution
/// This is a simple one-way unification: type variables in `pattern` are bound to concrete types in `concrete`
fn unify_types_into(pattern: &Type, concrete: &Type, subst: &mut HashMap<Name, Type>) {
    match (pattern, concrete) {
        (Type::Var(name), _) => {
            // Bind type variable to concrete type
            if !subst.contains_key(name) {
                subst.insert(name.clone(), concrete.clone());
            }
        }
        (Type::Arr(p1, r1), Type::Arr(p2, r2)) => {
            unify_types_into(p1, p2, subst);
            unify_types_into(r1, r2, subst);
        }
        (Type::Prod(l1, r1), Type::Prod(l2, r2)) => {
            unify_types_into(l1, l2, subst);
            unify_types_into(r1, r2, subst);
        }
        (Type::Comp(b1, a1), Type::Comp(b2, a2)) => {
            unify_types_into(b1, b2, subst);
            unify_types_into(a1, a2, subst);
        }
        _ => {}
    }
}

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
    /// Registry of instantiations for monomorphization
    instantiation_registry: InstantiationRegistry,
    /// Map from (function_name, type_args) to specialized function name
    specialization_map: SpecializationMap,
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
                 adt_registry: AdtRegistry,
                 instantiation_registry: InstantiationRegistry)
                 -> (HashMap<Id, P<FunDef>>, HashMap<Id, P<TypeDef>>)
        where I: IntoIterator<Item = Def>
    {
        let current_tmp = interner.intern("");

        // Build specialization map from instantiation registry
        let mut specialization_map: SpecializationMap = HashMap::new();
        for inst in &instantiation_registry.instantiations {
            let mangled = inst.mangled_name();
            specialization_map
                .entry(inst.function.clone())
                .or_insert_with(HashMap::new)
                .insert(inst.type_args.clone(), mangled);
        }

        let mut runner = K {
            count: 0,
            env: HashMap::new(),
            global: HashMap::new(),
            typedefs: HashMap::new(),
            current: current_tmp,
            direct: HashMap::new(),
            interner,
            adt_registry,
            instantiation_registry,
            specialization_map,
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

            // Generate specialized versions of polymorphic functions
            b.generate_specializations();
        }

        let K { global, typedefs, .. } = runner;
        (global, typedefs)
    }

    /// Generate specialized versions of polymorphic functions
    fn generate_specializations(&mut self) {
        // Collect functions that need specialization
        let functions_to_specialize: Vec<(String, Vec<Instantiation>)> = self.instantiation_registry
            .polymorphic_functions
            .keys()
            .filter_map(|fn_name| {
                let insts = self.instantiation_registry.get_instantiations(fn_name);
                if insts.is_empty() {
                    None
                } else {
                    Some((fn_name.clone(), insts.into_iter().cloned().collect()))
                }
            })
            .collect();


        // For each function that needs specialization, generate specialized versions
        for (fn_name, instantiations) in functions_to_specialize {
            // Get the original function definition
            let fn_id = self.interner.intern(&fn_name);
            if let Some(original_def) = self.global.get(&fn_id).cloned() {
                for inst in instantiations {
                    self.specialize_function(&original_def, &inst);
                }
            }
        }
    }

    /// Generate a specialized version of a function for a specific instantiation
    fn specialize_function(&mut self, original: &FunDef, inst: &Instantiation) {
        // Get the scheme from the original function definition
        // This has the actual type variable names used after inference
        let original_scheme = original.scheme();

        // Build substitution from type parameters to concrete types
        let type_params = original_scheme.type_params();
        if type_params.len() != inst.type_args.len() {
            return; // Mismatch, skip
        }

        let subst: HashMap<Name, Type> = type_params.iter()
            .cloned()
            .zip(inst.type_args.iter().cloned())
            .collect();

        // Create the specialized type by substituting type parameters
        let specialized_ty = apply_type_subst(original.ref_type(), &subst);
        let specialized_scheme = Scheme::Mono(specialized_ty);

        // Create the specialized function with mangled name
        let mangled_name = inst.mangled_name();
        let mangled_id = self.interner.intern(&mangled_name);

        // Build function rename map for recursive calls
        // Maps original function ID to specialized function ID
        let mut fn_rename = HashMap::new();
        fn_rename.insert(original.name(), mangled_id);

        // Create specialization context
        let ctx = SpecializeContext {
            type_subst: subst.clone(),
            fn_rename,
        };

        // Specialize parameters
        let specialized_params: Vec<VarDecl> = original.parameters().iter()
            .map(|p| {
                let VarDecl(id, scm) = p;
                let new_scm = apply_scheme_subst(scm, &subst);
                VarDecl(*id, new_scm)
            })
            .collect();

        // Specialize free variables
        let specialized_fv: Vec<VarDecl> = original.fv().iter()
            .map(|p| {
                let VarDecl(id, scm) = p;
                let new_scm = apply_scheme_subst(scm, &subst);
                VarDecl(*id, new_scm)
            })
            .collect();

        // Specialize body with context (including recursive call renaming)
        let specialized_body = specialize_term(original.body(), &ctx);

        // Create and register the specialized function
        let specialized_def = FunDef::new(
            mangled_id,
            specialized_scheme,
            specialized_params,
            specialized_fv,
            specialized_body,
        );

        // Add to direct call map
        self.direct.insert(mangled_id, mangled_id);
        self.global.insert(mangled_id, Box::new(specialized_def));
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

    /// Find a specialized function for a polymorphic call based on argument types
    /// Returns Some with the specialized function info if specialization is needed
    fn find_specialized_function(
        &self,
        fn_name: &str,
        args: &[P<TaggedTerm>],
        result_type: &Scheme
    ) -> Option<SpecializedCall> {
        // Get the function definition to access its actual scheme (with fresh type variables)
        let fn_id = self.interner.lookup(fn_name)?;
        let func_def = self.global.get(&fn_id)?;
        let actual_scheme = func_def.scheme();

        // Get type parameters from the actual scheme
        let type_params = actual_scheme.type_params();
        if type_params.is_empty() {
            return None; // Monomorphic function, no specialization needed
        }

        // Try to determine type arguments from argument types and result type
        let arg_types: Vec<Type> = args.iter()
            .map(|a| a.ref_scheme().body().clone())
            .collect();

        // Build a substitution by matching the actual function type against concrete types
        let subst = self.infer_type_args(actual_scheme, &arg_types, result_type.body())?;

        // Build the type args in order
        let type_args: Vec<Type> = type_params.iter()
            .filter_map(|p| subst.get(p).cloned())
            .collect();

        if type_args.len() != type_params.len() {
            return None; // Couldn't determine all type arguments
        }

        // Check if all type args are concrete
        if !type_args.iter().all(|t| is_type_concrete(t)) {
            return None;
        }

        // Create the instantiation and get mangled name
        let inst = Instantiation::new(fn_name.to_string(), type_args.clone());

        // Apply substitution to get specialized scheme
        let specialized_type = apply_type_subst(actual_scheme.body(), &subst);
        let specialized_scheme = Scheme::Mono(specialized_type);

        Some(SpecializedCall {
            mangled_name: inst.mangled_name(),
            specialized_scheme,
        })
    }

    /// Infer type arguments by matching polymorphic type against concrete argument and result types
    fn infer_type_args(
        &self,
        poly_scheme: &Scheme,
        arg_types: &[Type],
        result_type: &Type
    ) -> Option<HashMap<Name, Type>> {
        let mut subst = HashMap::new();

        // Get the function type from scheme
        let fn_type = poly_scheme.body();

        if let Type::Arr(param_type, ret_type) = fn_type {
            // Unify parameter types
            let param_types = param_type.prod_to_vec();
            for (expected, actual) in param_types.iter().zip(arg_types.iter()) {
                unify_types_into(*expected, actual, &mut subst);
            }

            // Unify result type
            unify_types_into(ret_type.as_ref(), result_type, &mut subst);
        }

        if subst.is_empty() {
            None
        } else {
            Some(subst)
        }
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
                                // Extract type arguments from the result type (e.g., List Int -> [Int])
                                let type_args = extract_type_args(tform.body());
                                return TaggedTerm::new(
                                    tform,
                                    Term::MakeData {
                                        type_name: adt_name,
                                        tag,
                                        fields: vec![],
                                        field_types: vec![],
                                        type_args,
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
                        // Extract type arguments from the result type (e.g., List Int -> [Int])
                        let type_args = extract_type_args(tform.body());
                        return TaggedTerm::new(
                            tform,
                            Term::MakeData {
                                type_name: adt_name,
                                tag,
                                fields: params_term,
                                field_types,
                                type_args,
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
                            // Check if this is a polymorphic function that needs specialization
                            let fn_name = self.interner.trace(*label).to_string();
                            if let Some(specialized) = self.find_specialized_function(&fn_name, &params_term, &tform) {
                                let specialized_id = self.interner.intern(&specialized.mangled_name);
                                // Make sure the specialized function is in direct call map
                                self.direct.insert(specialized_id, specialized_id);
                                Term::ApplyDir(
                                    VarDecl(specialized_id, specialized.specialized_scheme.clone()),
                                    params_term
                                )
                            } else {
                                Term::ApplyDir(VarDecl(label.to_owned(), callee_term.ref_scheme().clone()),
                                               params_term)
                            }
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

/// Apply a type substitution to a type
fn apply_type_subst(ty: &Type, subst: &HashMap<Name, Type>) -> Type {
    match ty {
        Type::Var(name) => {
            if let Some(concrete) = subst.get(name) {
                concrete.clone()
            } else {
                ty.clone()
            }
        }
        Type::Con(_) | Type::Void => ty.clone(),
        Type::Arr(p, r) => Type::Arr(
            Box::new(apply_type_subst(p, subst)),
            Box::new(apply_type_subst(r, subst)),
        ),
        Type::Prod(l, r) => Type::Prod(
            Box::new(apply_type_subst(l, subst)),
            Box::new(apply_type_subst(r, subst)),
        ),
        Type::Comp(base, arg) => Type::Comp(
            Box::new(apply_type_subst(base, subst)),
            Box::new(apply_type_subst(arg, subst)),
        ),
    }
}

/// Apply a type substitution to a scheme
fn apply_scheme_subst(scm: &Scheme, subst: &HashMap<Name, Type>) -> Scheme {
    match scm {
        Scheme::Mono(ty) => Scheme::Mono(apply_type_subst(ty, subst)),
        Scheme::Poly(vars, ty) => {
            // Don't substitute bound variables
            let filtered_subst: HashMap<Name, Type> = subst.iter()
                .filter(|(k, _)| !vars.contains(k))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            Scheme::Poly(vars.clone(), apply_type_subst(ty, &filtered_subst))
        }
        Scheme::PolyConstrained(vars, constraints, ty) => {
            let filtered_subst: HashMap<Name, Type> = subst.iter()
                .filter(|(k, _)| !vars.contains(k))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            Scheme::PolyConstrained(vars.clone(), constraints.clone(), apply_type_subst(ty, &filtered_subst))
        }
        Scheme::Slot => Scheme::Slot,
    }
}

/// Context for specialization: maps original function IDs to specialized IDs
struct SpecializeContext {
    type_subst: HashMap<Name, Type>,
    /// Maps original function ID to specialized function ID for recursive calls
    fn_rename: HashMap<Id, Id>,
}

/// Apply type substitution to a tagged term (specializes the term's types)
fn specialize_term(term: &TaggedTerm, ctx: &SpecializeContext) -> TaggedTerm {
    let specialized_scheme = apply_scheme_subst(term.ref_scheme(), &ctx.type_subst);
    let specialized_body = specialize_term_body(term.body(), ctx);
    TaggedTerm::new(specialized_scheme, specialized_body)
}

/// Apply type substitution to a term body
fn specialize_term_body(term: &Term, ctx: &SpecializeContext) -> Term {
    match term {
        Term::Lit(lit) => Term::Lit(lit.clone()),
        Term::Var(id) => {
            // Rename function reference if this is a recursive call
            let new_id = ctx.fn_rename.get(id).copied().unwrap_or(*id);
            Term::Var(new_id)
        }
        Term::Binary(op, l, r) => Term::Binary(
            *op,
            Box::new(specialize_term(l, ctx)),
            Box::new(specialize_term(r, ctx)),
        ),
        Term::Unary(op, e) => Term::Unary(
            *op,
            Box::new(specialize_term(e, ctx)),
        ),
        Term::List(items) => Term::List(
            items.iter().map(|i| Box::new(specialize_term(i, ctx))).collect(),
        ),
        Term::Block(items) => Term::Block(
            items.iter().map(|i| Box::new(specialize_term(i, ctx))).collect(),
        ),
        Term::Let(var, val, body) => {
            let VarDecl(id, scm) = var;
            let specialized_var = VarDecl(*id, apply_scheme_subst(scm, &ctx.type_subst));
            Term::Let(
                specialized_var,
                Box::new(specialize_term(val, ctx)),
                Box::new(specialize_term(body, ctx)),
            )
        }
        Term::If(c, t, f) => Term::If(
            Box::new(specialize_term(c, ctx)),
            Box::new(specialize_term(t, ctx)),
            Box::new(specialize_term(f, ctx)),
        ),
        Term::ApplyDir(var, args) => {
            let VarDecl(id, scm) = var;
            // Check if this function ID should be renamed (for recursive calls)
            let new_id = ctx.fn_rename.get(id).copied().unwrap_or(*id);
            let specialized_var = VarDecl(new_id, apply_scheme_subst(scm, &ctx.type_subst));
            Term::ApplyDir(
                specialized_var,
                args.iter().map(|a| Box::new(specialize_term(a, ctx))).collect(),
            )
        }
        Term::ApplyCls(callee, args) => Term::ApplyCls(
            Box::new(specialize_term(callee, ctx)),
            args.iter().map(|a| Box::new(specialize_term(a, ctx))).collect(),
        ),
        Term::MakeCls(var, cls, body) => {
            let VarDecl(id, scm) = var;
            let specialized_var = VarDecl(*id, apply_scheme_subst(scm, &ctx.type_subst));
            Term::MakeCls(
                specialized_var,
                cls.clone(), // Closure structure doesn't change
                Box::new(specialize_term(body, ctx)),
            )
        }
        Term::Match(scrutinee, arms) => Term::Match(
            Box::new(specialize_term(scrutinee, ctx)),
            arms.iter().map(|(pat, guard, body)| (
                pat.clone(),
                guard.clone(),
                Box::new(specialize_term(body, ctx)),
            )).collect(),
        ),
        Term::MakeData { type_name, tag, fields, field_types, type_args } => Term::MakeData {
            type_name: type_name.clone(),
            tag: *tag,
            fields: fields.iter().map(|f| Box::new(specialize_term(f, ctx))).collect(),
            field_types: field_types.iter().map(|t| apply_type_subst(t, &ctx.type_subst)).collect(),
            type_args: type_args.iter().map(|t| apply_type_subst(t, &ctx.type_subst)).collect(),
        },
        Term::GetTag(node) => Term::GetTag(Box::new(specialize_term(node, ctx))),
        Term::GetField(node, idx) => Term::GetField(Box::new(specialize_term(node, ctx)), *idx),
    }
}
