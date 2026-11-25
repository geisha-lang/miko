/// K-conversion, demand-driven transitive monomorphization, and static concept dispatch.

use std::collections::{HashMap, HashSet, VecDeque};
use std::ops::Deref;

use crate::core::term::*;
use crate::internal::*;
use crate::syntax::form::*;
use crate::typeinfer::mono::{Instantiation, InstantiationRegistry};
use crate::types::*;
use crate::utils::*;

type Direct = HashMap<Id, Id>;

fn is_type_concrete(ty: &Type) -> bool {
    match ty {
        Type::Var(_) => false,
        Type::Con(_) | Type::Void => true,
        Type::Arr(ps, r) => ps.iter().all(is_type_concrete) && is_type_concrete(r),
        Type::Tuple(es) => es.iter().all(is_type_concrete),
        Type::Comp(base, arg) => is_type_concrete(base) && is_type_concrete(arg),
    }
}

fn is_builtin_op_primitive(ty: &Type) -> bool {
    matches!(
        ty,
        Type::Con(n)
            if matches!(
                n.as_str(),
                "Int" | "Int64" | "Float" | "Bool" | "Char" | "String"
            )
    )
}

/// Default unconstrained phantom type variables (e.g. `b` in `Left(10) : Either Int ?b` or `a` in `Nothing : Maybe ?a`)
/// to `Type::Void` inside monomorphic contexts so monomorphization produces concrete types.
fn default_phantom_type(ty: &Type) -> Type {
    match ty {
        Type::Var(_) => Type::Void,
        Type::Con(_) | Type::Void => ty.clone(),
        Type::Arr(ps, r) => Type::Arr(
            ps.iter().map(default_phantom_type).collect(),
            Box::new(default_phantom_type(r)),
        ),
        Type::Tuple(es) => Type::Tuple(es.iter().map(default_phantom_type).collect()),
        Type::Comp(b, a) => Type::Comp(
            Box::new(default_phantom_type(b)),
            Box::new(default_phantom_type(a)),
        ),
    }
}

fn sanitize_phantom_vars_in_term(term: &mut TaggedTerm) {
    term.ty = Scheme::Mono(default_phantom_type(term.ty.body()));
    match &mut term.node {
        Term::Lit(_) | Term::Var(_) | Term::GlobalVar(_) => {}
        Term::Tuple(es) | Term::Block(es) => {
            for e in es {
                sanitize_phantom_vars_in_term(e);
            }
        }
        Term::MakeCls(VarDecl(_, scm), _, body) => {
            *scm = Scheme::Mono(default_phantom_type(scm.body()));
            sanitize_phantom_vars_in_term(body);
        }
        Term::ApplyCls(c, args) => {
            sanitize_phantom_vars_in_term(c);
            for a in args {
                sanitize_phantom_vars_in_term(a);
            }
        }
        Term::ApplyDir(VarDecl(_, scm), args) => {
            *scm = Scheme::Mono(default_phantom_type(scm.body()));
            for a in args {
                sanitize_phantom_vars_in_term(a);
            }
        }
        Term::Binary(_, l, r) => {
            sanitize_phantom_vars_in_term(l);
            sanitize_phantom_vars_in_term(r);
        }
        Term::Unary(_, e) | Term::GetTag(e) | Term::GetField(e, _) => {
            sanitize_phantom_vars_in_term(e);
        }
        Term::Let(VarDecl(_, scm), val, body) => {
            *scm = Scheme::Mono(default_phantom_type(scm.body()));
            sanitize_phantom_vars_in_term(val);
            sanitize_phantom_vars_in_term(body);
        }
        Term::If(c, t, f) => {
            sanitize_phantom_vars_in_term(c);
            sanitize_phantom_vars_in_term(t);
            sanitize_phantom_vars_in_term(f);
        }
        Term::While(c, b) => {
            sanitize_phantom_vars_in_term(c);
            sanitize_phantom_vars_in_term(b);
        }
        Term::Match(scrut, arms) => {
            sanitize_phantom_vars_in_term(scrut);
            for (_, g, b) in arms {
                if let Some(ref mut gt) = g {
                    sanitize_phantom_vars_in_term(gt);
                }
                sanitize_phantom_vars_in_term(b);
            }
        }
        Term::MakeData {
            fields,
            field_types,
            type_args,
            ..
        } => {
            for f in fields {
                sanitize_phantom_vars_in_term(f);
            }
            for ft in field_types {
                *ft = default_phantom_type(ft);
            }
            for ta in type_args {
                *ta = default_phantom_type(ta);
            }
        }
    }
}

fn extract_type_args(ty: &Type) -> Vec<Type> {
    let mut args = Vec::new();
    let mut current = ty;
    while let Type::Comp(base, arg) = current {
        args.push(arg.as_ref().clone());
        current = base.as_ref();
    }
    args.reverse();
    args
}

fn unify_types_into(pattern: &Type, concrete: &Type, subst: &mut HashMap<Name, Type>) {
    match (pattern, concrete) {
        (Type::Var(name), _) => {
            subst.entry(name.clone()).or_insert_with(|| concrete.clone());
        }
        (Type::Arr(ps1, r1), Type::Arr(ps2, r2)) if ps1.len() == ps2.len() => {
            for (p1, p2) in ps1.iter().zip(ps2.iter()) {
                unify_types_into(p1, p2, subst);
            }
            unify_types_into(r1, r2, subst);
        }
        (Type::Tuple(es1), Type::Tuple(es2)) if es1.len() == es2.len() => {
            for (e1, e2) in es1.iter().zip(es2.iter()) {
                unify_types_into(e1, e2, subst);
            }
        }
        (Type::Comp(b1, a1), Type::Comp(b2, a2)) => {
            unify_types_into(b1, b2, subst);
            unify_types_into(a1, a2, subst);
        }
        _ => {}
    }
}

fn match_instance_target(pattern: &Type, concrete: &Type, subst: &mut HashMap<Name, Type>) -> bool {
    match (pattern, concrete) {
        (Type::Var(v), t) => {
            if let Some(existing) = subst.get(v) {
                existing == t
            } else {
                subst.insert(v.clone(), t.clone());
                true
            }
        }
        (Type::Void, Type::Void) => true,
        (Type::Con(a), Type::Con(b)) => a == b,
        (Type::Comp(b1, a1), Type::Comp(b2, a2)) => {
            match_instance_target(b1, b2, subst) && match_instance_target(a1, a2, subst)
        }
        (Type::Tuple(es1), Type::Tuple(es2)) if es1.len() == es2.len() => es1
            .iter()
            .zip(es2.iter())
            .all(|(x, y)| match_instance_target(x, y, subst)),
        (Type::Arr(ps1, r1), Type::Arr(ps2, r2)) if ps1.len() == ps2.len() => {
            ps1.iter()
                .zip(ps2.iter())
                .all(|(x, y)| match_instance_target(x, y, subst))
                && match_instance_target(r1, r2, subst)
        }
        _ => false,
    }
}

#[derive(Debug)]
pub struct KOutput {
    pub functions: HashMap<Id, P<FunDef>>,
    pub globals: Vec<GlobalValDef>,
    pub externs: HashMap<Id, (Name, Type)>,
    pub typedefs: HashMap<Id, P<TypeDef>>,
}

#[derive(Debug)]
pub struct K<'i> {
    count: usize,
    env: HashMap<Id, Scheme>,
    global: HashMap<Id, P<FunDef>>,
    global_vals: Vec<GlobalValDef>,
    global_val_ids: HashSet<Id>,
    externs: HashMap<Id, (Name, Type)>,
    typedefs: HashMap<Id, P<TypeDef>>,
    current: Id,
    interner: &'i mut Interner,
    direct: Direct,
    /// Lifted closure functions created inside each parent function
    lifted_closures: HashMap<Id, Vec<Id>>,
    /// Lambda-lifted polymorphic local `let` functions: `local_id -> (lifted_fn_id, captured_fvs, lifted_scheme)`
    poly_lifted: HashMap<Id, (Id, Vec<VarDecl>, Scheme)>,
    adt_registry: AdtRegistry,
    concept_registry: ConceptRegistry,
    instance_registry: InstanceRegistry,
    concept_methods: HashMap<Name, (Name, usize)>,
    instantiation_registry: InstantiationRegistry,
    import_map: HashMap<Id, Id>,
}

macro_rules! with_var {
    ($env:expr, $var:expr, $val:expr => $act:block) => {{
        let o = $env.insert($var, $val);
        let ret = $act;
        $env.remove(&$var);
        if let Some(v) = o {
            $env.insert($var, v);
        }
        ret
    }};
}

impl<'i> K<'i> {
    pub fn go<I>(
        module: I,
        interner: &mut Interner,
        adt_registry: AdtRegistry,
        instantiation_registry: InstantiationRegistry,
    ) -> (HashMap<Id, P<FunDef>>, HashMap<Id, P<TypeDef>>)
    where
        I: IntoIterator<Item = Def>,
    {
        let out = Self::go_full(
            module,
            interner,
            adt_registry,
            HashMap::new(),
            Vec::new(),
            HashMap::new(),
            instantiation_registry,
            HashMap::new(),
        );
        (out.functions, out.typedefs)
    }

    pub fn go_with_imports<I>(
        module: I,
        interner: &mut Interner,
        adt_registry: AdtRegistry,
        instantiation_registry: InstantiationRegistry,
        import_map: HashMap<Id, Id>,
    ) -> (HashMap<Id, P<FunDef>>, HashMap<Id, P<TypeDef>>)
    where
        I: IntoIterator<Item = Def>,
    {
        let out = Self::go_full(
            module,
            interner,
            adt_registry,
            HashMap::new(),
            Vec::new(),
            HashMap::new(),
            instantiation_registry,
            import_map,
        );
        (out.functions, out.typedefs)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn go_full<I>(
        module: I,
        interner: &mut Interner,
        adt_registry: AdtRegistry,
        concept_registry: ConceptRegistry,
        instance_registry: InstanceRegistry,
        concept_methods: HashMap<Name, (Name, usize)>,
        instantiation_registry: InstantiationRegistry,
        import_map: HashMap<Id, Id>,
    ) -> KOutput
    where
        I: IntoIterator<Item = Def>,
    {
        let current_tmp = interner.intern("");
        let defs_vec: Vec<Def> = module.into_iter().collect();

        let mut global_val_ids = HashSet::new();
        for d in &defs_vec {
            if let Item::Form(ref f) = d.node {
                if !matches!(f.node, Expr::Abs(..)) {
                    global_val_ids.insert(d.ident);
                }
            }
        }

        let mut runner = K {
            count: 0,
            env: HashMap::new(),
            global: HashMap::new(),
            global_vals: Vec::new(),
            global_val_ids,
            externs: HashMap::new(),
            typedefs: HashMap::new(),
            current: current_tmp,
            direct: HashMap::new(),
            lifted_closures: HashMap::new(),
            poly_lifted: HashMap::new(),
            interner,
            adt_registry,
            concept_registry,
            instance_registry,
            concept_methods,
            instantiation_registry,
            import_map,
        };

        for def in &defs_vec {
            if def.is_form() && !runner.global_val_ids.contains(&def.ident) {
                runner.direct.insert(def.ident, def.ident);
            } else if let Item::Extern { ref link_name, ref ty } = def.node {
                runner.direct.insert(def.ident, def.ident);
                runner
                    .externs
                    .insert(def.ident, (link_name.clone(), ty.body().clone()));
            }
        }

        for def in defs_vec {
            runner.convert_def(def);
        }

        // Run demand-driven transitive worklist monomorphization & static concept resolution
        runner.run_monomorphization_worklist();

        KOutput {
            functions: runner.global,
            globals: runner.global_vals,
            externs: runner.externs,
            typedefs: runner.typedefs,
        }
    }

    // ========================================================================
    // Worklist Monomorphization + Static Concept Method Dispatch
    // ========================================================================

    fn run_monomorphization_worklist(&mut self) {
        let mut queue: VecDeque<Id> = VecDeque::new();
        let mut processed_fns: HashSet<Id> = HashSet::new();
        let mut created_specs: HashSet<Instantiation> = HashSet::new();

        // Seed worklist with all monomorphic top-level functions (sanitizing any phantom local type vars)
        let mono_ids: Vec<Id> = self
            .global
            .iter()
            .filter(|(_, fn_def)| {
                fn_def.scheme().type_params().is_empty() && is_type_concrete(fn_def.ref_type())
            })
            .map(|(&id, _)| id)
            .collect();
        for fn_id in mono_ids {
            if let Some(fn_def) = self.global.get_mut(&fn_id) {
                sanitize_phantom_vars_in_term(fn_def.body_mut());
            }
            queue.push_back(fn_id);
        }

        // First rewrite top-level global value initializers (all monomorphic)
        let mut new_pending_specs = Vec::new();
        let mut globals = std::mem::take(&mut self.global_vals);
        for g in &mut globals {
            g.ty = default_phantom_type(&g.ty);
            sanitize_phantom_vars_in_term(&mut g.init);
            self.rewrite_calls_in_term(&mut g.init, &mut new_pending_specs);
        }
        self.global_vals = globals;

        for inst in new_pending_specs {
            if created_specs.insert(inst.clone()) {
                if let Some(spec_ids) = self.materialize_specialization(&inst) {
                    for sid in spec_ids {
                        queue.push_back(sid);
                    }
                }
            }
        }

        // Process worklist until transitive closure is reached
        while let Some(fn_id) = queue.pop_front() {
            if !processed_fns.insert(fn_id) {
                continue;
            }
            let Some(mut fn_def) = self.global.remove(&fn_id) else {
                continue;
            };

            let mut discovered = Vec::new();
            self.rewrite_calls_in_term(fn_def.body_mut(), &mut discovered);
            self.global.insert(fn_id, fn_def);

            for inst in discovered {
                if created_specs.insert(inst.clone()) {
                    if let Some(spec_ids) = self.materialize_specialization(&inst) {
                        for sid in spec_ids {
                            queue.push_back(sid);
                        }
                    }
                }
            }
        }

        // Remove unspecialized polymorphic template functions (and their unspecialized inner closures)
        let poly_ids: Vec<Id> = self
            .global
            .iter()
            .filter(|(_, def)| {
                !def.scheme().type_params().is_empty() || !is_type_concrete(def.ref_type())
            })
            .map(|(&id, _)| id)
            .collect();
        for pid in poly_ids {
            self.global.remove(&pid);
        }
    }

    /// Resolve a concept method call `(method_name, arg_types, ret_type)` to `(impl_fn_name, impl_scheme, optional_inst)`.
    fn resolve_concept_call(
        &self,
        method_name: &str,
        arg_types: &[Type],
        ret_type: &Type,
    ) -> Option<(Name, Scheme, Option<Instantiation>)> {
        let (concept_name, param_idx) = self.concept_methods.get(method_name)?;
        let cinfo = self.concept_registry.get(concept_name)?;
        let (_, method_scm) = cinfo.methods.iter().find(|(m, _)| m == method_name)?;

        // Match method signature against concrete call site to find the concept's target type
        let mut call_sub = HashMap::new();
        let concrete_fn_ty = Type::Arr(arg_types.to_vec(), P(ret_type.clone()));
        unify_types_into(method_scm.body(), &concrete_fn_ty, &mut call_sub);

        let concept_tv = cinfo.type_params.get(*param_idx)?;
        let concrete_target_ty = call_sub.get(concept_tv)?;
        if !is_type_concrete(concrete_target_ty) {
            return None;
        }

        // Find matching instance in `instance_registry`
        for inst_info in &self.instance_registry {
            if &inst_info.concept_name != concept_name || inst_info.target_types.is_empty() {
                continue;
            }
            let mut inst_sub = HashMap::new();
            if match_instance_target(&inst_info.target_types[0], concrete_target_ty, &mut inst_sub)
            {
                let base_impl_name = inst_info.method_impls.get(method_name)?.clone();
                let base_impl_id = self.interner.lookup(&base_impl_name)?;
                let base_def = self.global.get(&base_impl_id)?;
                let base_scm = base_def.scheme();

                let mut full_sub = inst_sub;
                unify_types_into(base_scm.body(), &concrete_fn_ty, &mut full_sub);

                let tparams = base_scm.type_params();
                if tparams.is_empty() {
                    return Some((
                        base_impl_name,
                        Scheme::Mono(concrete_fn_ty),
                        None,
                    ));
                } else {
                    let type_args: Vec<Type> = tparams
                        .iter()
                        .filter_map(|p| full_sub.get(p).cloned())
                        .collect();
                    if type_args.len() == tparams.len() && type_args.iter().all(is_type_concrete) {
                        let spec_inst = Instantiation::new(base_impl_name, type_args);
                        return Some((
                            spec_inst.mangled_name(),
                            Scheme::Mono(concrete_fn_ty),
                            Some(spec_inst),
                        ));
                    }
                }
            }
        }
        None
    }

    fn resolve_poly_fn_call(
        &self,
        fn_name: &str,
        concrete_fn_ty: &Type,
    ) -> Option<(Name, Scheme, Instantiation)> {
        let fn_id = self.interner.lookup(fn_name)?;
        let func_def = self.global.get(&fn_id)?;
        let actual_scheme = func_def.scheme();
        let type_params = actual_scheme.type_params();
        if type_params.is_empty() {
            return None;
        }

        let mut subst = HashMap::new();
        unify_types_into(actual_scheme.body(), concrete_fn_ty, &mut subst);

        let type_args: Vec<Type> = type_params
            .iter()
            .filter_map(|p| subst.get(p).cloned())
            .collect();
        if type_args.len() != type_params.len() || !type_args.iter().all(is_type_concrete) {
            return None;
        }

        let inst = Instantiation::new(fn_name.to_string(), type_args);
        let spec_ty = apply_type_subst(actual_scheme.body(), &subst);
        Some((inst.mangled_name(), Scheme::Mono(spec_ty), inst))
    }

    /// Walk a concrete term, rewriting concept method calls & polymorphic calls/value references,
    /// and pushing newly discovered `Instantiation`s into `discovered`.
    fn rewrite_calls_in_term(&mut self, term: &mut TaggedTerm, discovered: &mut Vec<Instantiation>) {
        // Recurse into children first
        match &mut term.node {
            Term::Lit(_) | Term::GlobalVar(_) => {}
            Term::Var(ref mut id) => {
                // First-class polymorphic function value (e.g. `twice(id, 5)`)!
                let name = self.interner.trace(*id).to_string();
                let node_ty = term.ty.body().clone();
                if is_type_concrete(&node_ty) && !self.global.contains_key(id) {
                    if let Type::Arr(ref ps, ref r) = node_ty {
                        if let Some((mangled, spec_scm, opt_inst)) =
                            self.resolve_concept_call(&name, ps, r)
                        {
                            let new_id = self.interner.intern(&mangled);
                            self.direct.insert(new_id, new_id);
                            *id = new_id;
                            term.ty = spec_scm;
                            if let Some(inst) = opt_inst {
                                discovered.push(inst);
                            }
                            return;
                        }
                    }
                }
                if is_type_concrete(&node_ty) {
                    if let Some((mangled, spec_scm, inst)) =
                        self.resolve_poly_fn_call(&name, &node_ty)
                    {
                        let new_id = self.interner.intern(&mangled);
                        self.direct.insert(new_id, new_id);
                        *id = new_id;
                        term.ty = spec_scm;
                        discovered.push(inst);
                    }
                }
                // Note: if `Var(id)` is a direct top-level function used as a first-class value,
                // wrap it in an eta closure in `emit.rs` or right here!
                return;
            }
            Term::Tuple(es) | Term::Block(es) => {
                for e in es {
                    self.rewrite_calls_in_term(e, discovered);
                }
            }
            Term::MakeCls(_, _, body) => {
                self.rewrite_calls_in_term(body, discovered);
            }
            Term::ApplyCls(callee, args) => {
                self.rewrite_calls_in_term(callee, discovered);
                for a in args {
                    self.rewrite_calls_in_term(a, discovered);
                }
            }
            Term::ApplyDir(ref mut callee_decl, ref mut args) => {
                for a in args.iter_mut() {
                    self.rewrite_calls_in_term(a, discovered);
                }

                let fn_name = self.interner.trace(callee_decl.0).to_string();
                let arg_tys: Vec<Type> = args.iter().map(|a| a.ty.body().clone()).collect();
                let ret_ty = term.ty.body().clone();

                // 1. Check if this is a concept method call (unless shadowed by an explicit top-level `def`)
                if !self.global.contains_key(&callee_decl.0) {
                    if let Some((mangled, spec_scm, opt_inst)) =
                        self.resolve_concept_call(&fn_name, &arg_tys, &ret_ty)
                    {
                        let new_id = self.interner.intern(&mangled);
                        self.direct.insert(new_id, new_id);
                        *callee_decl = VarDecl(new_id, spec_scm);
                        if let Some(inst) = opt_inst {
                            discovered.push(inst);
                        }
                        return;
                    }

                    // Built-in fallback when calling `eq(a, b)` or `compare(a, b)` on primitives without a user `instance`
                    if arg_tys.len() == 2 && is_builtin_op_primitive(&arg_tys[0]) {
                        if fn_name == "eq" {
                            let a = args.remove(0);
                            let b = args.remove(0);
                            term.node = Term::Binary(BinOp::Eq, a, b);
                            return;
                        } else if fn_name == "compare" {
                            let a = args.remove(0);
                            let b = args.remove(0);
                            let bool_scm = Scheme::Mono(Type::Con("Bool".to_string()));
                            let int_scm = Scheme::Mono(Type::Con("Int".to_string()));
                            let lt_cond = Box::new(TaggedTerm::new(
                                bool_scm.clone(),
                                Term::Binary(BinOp::Lt, a.clone(), b.clone()),
                            ));
                            let eq_cond = Box::new(TaggedTerm::new(
                                bool_scm,
                                Term::Binary(BinOp::Eq, a, b),
                            ));
                            let neg_one =
                                Box::new(TaggedTerm::new(int_scm.clone(), Term::Lit(Lit::Int(-1))));
                            let zero =
                                Box::new(TaggedTerm::new(int_scm.clone(), Term::Lit(Lit::Int(0))));
                            let pos_one =
                                Box::new(TaggedTerm::new(int_scm.clone(), Term::Lit(Lit::Int(1))));
                            let inner_if =
                                Box::new(TaggedTerm::new(int_scm, Term::If(eq_cond, zero, pos_one)));
                            term.node = Term::If(lt_cond, neg_one, inner_if);
                            return;
                        }
                    }
                }

                // 2. Check if this is a polymorphic function call
                let concrete_fn_ty = Type::Arr(arg_tys, P(ret_ty));
                if let Some((mangled, spec_scm, inst)) =
                    self.resolve_poly_fn_call(&fn_name, &concrete_fn_ty)
                {
                    let new_id = self.interner.intern(&mangled);
                    self.direct.insert(new_id, new_id);
                    *callee_decl = VarDecl(new_id, spec_scm);
                    discovered.push(inst);
                }
            }
            Term::Binary(op, l, r) => {
                let bop = *op;
                self.rewrite_calls_in_term(l, discovered);
                self.rewrite_calls_in_term(r, discovered);

                let operand_ty = l.ty.body().clone();
                if is_type_concrete(&operand_ty) && !is_builtin_op_primitive(&operand_ty) {
                    let arg_tys = vec![operand_ty.clone(), operand_ty.clone()];
                    let ret_ty = term.ty.body().clone();
                    match bop {
                        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                            let mname = match bop {
                                BinOp::Add => "$op$plus",
                                BinOp::Sub => "$op$minus",
                                BinOp::Mul => "$op$star",
                                BinOp::Div => "$op$slash",
                                BinOp::Rem => "$op$rem",
                                _ => unreachable!(),
                            };
                            if let Some((mangled, spec_scm, opt_inst)) =
                                self.resolve_concept_call(mname, &arg_tys, &ret_ty)
                            {
                                let new_id = self.interner.intern(&mangled);
                                self.direct.insert(new_id, new_id);
                                if let Some(inst) = opt_inst {
                                    discovered.push(inst);
                                }
                                let args = vec![l.clone(), r.clone()];
                                term.node = Term::ApplyDir(VarDecl(new_id, spec_scm), args);
                            }
                        }
                        BinOp::Eq => {
                            if let Some((mangled, spec_scm, opt_inst)) =
                                self.resolve_concept_call("eq", &arg_tys, &ret_ty)
                            {
                                let new_id = self.interner.intern(&mangled);
                                self.direct.insert(new_id, new_id);
                                if let Some(inst) = opt_inst {
                                    discovered.push(inst);
                                }
                                let args = vec![l.clone(), r.clone()];
                                term.node = Term::ApplyDir(VarDecl(new_id, spec_scm), args);
                            }
                        }
                        BinOp::Ne => {
                            let bool_ty = Type::Con("Bool".to_string());
                            if let Some((mangled, spec_scm, opt_inst)) =
                                self.resolve_concept_call("eq", &arg_tys, &bool_ty)
                            {
                                let new_id = self.interner.intern(&mangled);
                                self.direct.insert(new_id, new_id);
                                if let Some(inst) = opt_inst {
                                    discovered.push(inst);
                                }
                                let eq_call = TaggedTerm::new(
                                    Scheme::Mono(bool_ty),
                                    Term::ApplyDir(
                                        VarDecl(new_id, spec_scm),
                                        vec![l.clone(), r.clone()],
                                    ),
                                );
                                term.node = Term::Unary(UnOp::Not, Box::new(eq_call));
                            }
                        }
                        BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                            let int_ty = Type::Con("Int".to_string());
                            if let Some((mangled, spec_scm, opt_inst)) =
                                self.resolve_concept_call("compare", &arg_tys, &int_ty)
                            {
                                let new_id = self.interner.intern(&mangled);
                                self.direct.insert(new_id, new_id);
                                if let Some(inst) = opt_inst {
                                    discovered.push(inst);
                                }
                                let cmp_call = TaggedTerm::new(
                                    Scheme::Mono(int_ty.clone()),
                                    Term::ApplyDir(
                                        VarDecl(new_id, spec_scm),
                                        vec![l.clone(), r.clone()],
                                    ),
                                );
                                let zero =
                                    TaggedTerm::new(Scheme::Mono(int_ty), Term::Lit(Lit::Int(0)));
                                term.node = Term::Binary(bop, Box::new(cmp_call), Box::new(zero));
                            }
                        }
                        _ => {}
                    }
                }
            }
            Term::Unary(UnOp::Neg, e) => {
                self.rewrite_calls_in_term(e, discovered);
                let operand_ty = e.ty.body().clone();
                if is_type_concrete(&operand_ty) && !is_builtin_op_primitive(&operand_ty) {
                    let arg_tys = vec![operand_ty.clone()];
                    if let Some((mangled, spec_scm, opt_inst)) =
                        self.resolve_concept_call("neg", &arg_tys, &operand_ty)
                    {
                        let new_id = self.interner.intern(&mangled);
                        self.direct.insert(new_id, new_id);
                        if let Some(inst) = opt_inst {
                            discovered.push(inst);
                        }
                        let args = vec![e.clone()];
                        term.node = Term::ApplyDir(VarDecl(new_id, spec_scm), args);
                    }
                }
            }
            Term::Unary(_, e) | Term::GetTag(e) | Term::GetField(e, _) => {
                self.rewrite_calls_in_term(e, discovered);
            }
            Term::Let(_, val, body) => {
                self.rewrite_calls_in_term(val, discovered);
                self.rewrite_calls_in_term(body, discovered);
            }
            Term::If(c, t, f) => {
                self.rewrite_calls_in_term(c, discovered);
                self.rewrite_calls_in_term(t, discovered);
                self.rewrite_calls_in_term(f, discovered);
            }
            Term::While(c, b) => {
                self.rewrite_calls_in_term(c, discovered);
                self.rewrite_calls_in_term(b, discovered);
            }
            Term::Match(scrutinee, arms) => {
                self.rewrite_calls_in_term(scrutinee, discovered);
                for (_, guard, body) in arms {
                    if let Some(ref mut g) = guard {
                        self.rewrite_calls_in_term(g, discovered);
                    }
                    self.rewrite_calls_in_term(body, discovered);
                }
            }
            Term::MakeData { fields, .. } => {
                for f in fields {
                    self.rewrite_calls_in_term(f, discovered);
                }
            }
        }
    }

    /// Generate a specialized `FunDef` (plus specialized copies of any lifted inner closures!)
    /// and return all newly created `Id`s so the worklist processes their bodies too.
    fn materialize_specialization(&mut self, inst: &Instantiation) -> Option<Vec<Id>> {
        let orig_id = self.interner.lookup(&inst.function)?;
        let original = self.global.get(&orig_id)?.clone();
        let original_scheme = original.scheme();
        let type_params = original_scheme.type_params();
        if type_params.len() != inst.type_args.len() {
            return None;
        }

        let subst: HashMap<Name, Type> = type_params
            .iter()
            .cloned()
            .zip(inst.type_args.iter().cloned())
            .collect();

        let mangled_name = inst.mangled_name();
        let mangled_id = self.interner.intern(&mangled_name);
        if self.global.contains_key(&mangled_id) {
            return Some(vec![mangled_id]);
        }

        let suffix = mangled_name
            .strip_prefix(&inst.function)
            .unwrap_or("")
            .to_string();

        // Also prepare renamed IDs for any closures lifted inside `orig_id`
        let mut fn_rename = HashMap::new();
        fn_rename.insert(orig_id, mangled_id);

        let inner_cls_ids = self
            .lifted_closures
            .get(&orig_id)
            .cloned()
            .unwrap_or_default();
        for &cid in &inner_cls_ids {
            let orig_cls_name = self.interner.trace(cid).to_string();
            let spec_cls_name = format!("{}{}", orig_cls_name, suffix);
            let spec_cls_id = self.interner.intern(&spec_cls_name);
            fn_rename.insert(cid, spec_cls_id);
        }

        let ctx = SpecializeContext {
            type_subst: subst.clone(),
            fn_rename,
        };

        let mut created = Vec::new();

        // Specialize any lifted closure functions first
        for cid in inner_cls_ids {
            if let Some(cls_def) = self.global.get(&cid).cloned() {
                let spec_cls_id = ctx.fn_rename[&cid];
                let spec_cls = self.build_specialized_fundef(&cls_def, spec_cls_id, &subst, &ctx);
                self.direct.insert(spec_cls_id, spec_cls_id);
                self.global.insert(spec_cls_id, Box::new(spec_cls));
                created.push(spec_cls_id);
            }
        }

        // Specialize the function itself
        let spec_def = self.build_specialized_fundef(&original, mangled_id, &subst, &ctx);
        self.direct.insert(mangled_id, mangled_id);
        self.global.insert(mangled_id, Box::new(spec_def));
        created.push(mangled_id);

        Some(created)
    }

    fn build_specialized_fundef(
        &self,
        original: &FunDef,
        new_id: Id,
        subst: &HashMap<Name, Type>,
        ctx: &SpecializeContext,
    ) -> FunDef {
        let specialized_ty = apply_type_subst(original.ref_type(), subst);
        let specialized_scheme = Scheme::Mono(specialized_ty);

        let specialized_params: Vec<VarDecl> = original
            .parameters()
            .iter()
            .map(|VarDecl(id, scm)| VarDecl(*id, apply_scheme_subst(scm, subst)))
            .collect();

        let specialized_fv: Vec<VarDecl> = original
            .fv()
            .iter()
            .map(|VarDecl(id, scm)| VarDecl(*id, apply_scheme_subst(scm, subst)))
            .collect();

        let mut specialized_body = specialize_term(original.body(), ctx);
        sanitize_phantom_vars_in_term(&mut specialized_body);
        FunDef::new(
            new_id,
            specialized_scheme,
            specialized_params,
            specialized_fv,
            specialized_body,
        )
    }

    // ========================================================================
    // AST -> Core K-Conversion
    // ========================================================================

    fn unique(&mut self) -> usize {
        self.count += 1;
        self.count
    }

    fn fresh(&mut self) -> String {
        format!(".tmp{}", self.unique())
    }

    fn make_cls_name(&mut self, bound: &str) -> Id {
        let base = self.interner.trace(self.current).to_owned();
        let name = format!("{}.inner.closure.{}{}", base, bound, self.unique());
        let id = self.interner.intern(&name);
        self.lifted_closures
            .entry(self.current)
            .or_default()
            .push(id);
        id
    }

    fn convert_def(&mut self, def: Def) {
        let Def { ident, node, .. } = def;
        let name = self.interner.trace(ident).to_owned();
        match node {
            Item::Form(form) => {
                let Form { node, tag } = *form;
                let ty = tag.ty;
                self.current = ident;

                match node {
                    Expr::Abs(lambda) => {
                        let (ps, _, bd) = self.trans_lambda(lambda, false);
                        self.define_fn(ident, ty, ps, vec![], bd);
                    }
                    other_expr => {
                        // Top-level value definition (`def pi = 3.14`)
                        let val_ty = ty.body().clone();
                        let val_form = Form::typed(tag.pos, ty, other_expr);
                        let init_term = self.transform(val_form);
                        self.global_vals.push(GlobalValDef {
                            name: ident,
                            ty: val_ty,
                            init: init_term,
                        });
                    }
                }
            }
            Item::Alg(ps, vs) => {
                let params = ps
                    .into_iter()
                    .map(|id| self.interner.trace(id).to_owned())
                    .collect();
                let d = Box::new(TypeDef::new(name, params, TypeKind::Algebra(vs)));
                self.typedefs.insert(ident, d);
            }
            Item::Alias(ps, t) => {
                let params = ps
                    .into_iter()
                    .map(|id| self.interner.trace(id).to_owned())
                    .collect();
                let d = Box::new(TypeDef::new(name, params, TypeKind::Alias(t)));
                self.typedefs.insert(ident, d);
            }
            Item::Concept { .. } | Item::Instance { .. } | Item::Extern { .. } => {
                // Instance methods were already synthesized; Extern declarations are tracked in `self.externs`.
            }
        }
    }

    fn define_fn(
        &mut self,
        name: Id,
        ty: Scheme,
        params: Vec<VarDecl>,
        free: Vec<VarDecl>,
        body: TaggedTerm,
    ) -> (Id, Vec<Id>) {
        let fun = FunDef::new(name, ty, params, free, body);
        let ent = self.global.entry(name).or_insert(P(fun));
        (
            (*ent).name(),
            (*ent).fv().iter().map(|v| v.name()).collect(),
        )
    }

    fn fv(&self, source: &TaggedTerm) -> HashSet<Id> {
        use self::Term::*;
        match *source.body() {
            Lit(_) | GlobalVar(_) => HashSet::new(),
            Var(v) => {
                let mut hs = HashSet::new();
                // Only capture local variables present in `self.env`, never global functions
                if self.env.contains_key(&v) {
                    hs.insert(v);
                }
                hs
            }
            Tuple(ref lst) | Block(ref lst) | ApplyDir(_, ref lst) => {
                lst.iter().fold(HashSet::new(), |mut res, v| {
                    res.extend(self.fv(v.deref()));
                    res
                })
            }
            Let(ref var, ref val, ref exp) => {
                let mut res = self.fv(exp.deref());
                res.remove(&var.name());
                res.extend(self.fv(val.deref()));
                res
            }
            MakeCls(ref var, ref cls, ref exp) => {
                let mut r = self.fv(exp.deref());
                for fv_id in cls.deref().fv() {
                    if self.env.contains_key(&fv_id) {
                        r.insert(fv_id);
                    }
                }
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
            Unary(_, ref e) | GetTag(ref e) | GetField(ref e, _) => self.fv(e.deref()),
            If(ref c, ref t, ref f) => {
                let mut r = self.fv(c.deref());
                r.extend(self.fv(t.deref()));
                r.extend(self.fv(f.deref()));
                r
            }
            While(ref c, ref b) => {
                let mut r = self.fv(c.deref());
                r.extend(self.fv(b.deref()));
                r
            }
            Match(ref scrutinee, ref arms) => {
                let mut r = self.fv(scrutinee.deref());
                for (pat, guard, body) in arms {
                    let mut arm_fv = self.fv(body.deref());
                    if let Some(ref g) = guard {
                        arm_fv.extend(self.fv(g.deref()));
                    }
                    Self::remove_pattern_bindings(&mut arm_fv, pat);
                    r.extend(arm_fv);
                }
                r
            }
            MakeData { ref fields, .. } => fields.iter().fold(HashSet::new(), |mut res, v| {
                res.extend(self.fv(v.deref()));
                res
            }),
        }
    }

    fn remove_pattern_bindings(fvs: &mut HashSet<Id>, pat: &Pattern) {
        match pat {
            Pattern::Var(id) => {
                fvs.remove(id);
            }
            Pattern::Wildcard | Pattern::Lit(_) => {}
            Pattern::Tuple(pats) | Pattern::Constructor(_, pats) => {
                for p in pats {
                    Self::remove_pattern_bindings(fvs, p);
                }
            }
            Pattern::StructConstructor(_, fs) => {
                for (_, p) in fs {
                    Self::remove_pattern_bindings(fvs, p);
                }
            }
        }
    }

    fn close_var(&mut self, var: Id, ty: Scheme) -> Option<Scheme> {
        self.env.insert(var, ty)
    }

    fn release_var(&mut self, var: &Id) -> Option<Scheme> {
        self.env.remove(var)
    }

    fn find_var(&self, var: &Id) -> Option<&Scheme> {
        self.env.get(var)
    }

    /// Normalize pattern (converting `StructConstructor` to positional `Constructor`) and collect typed bindings!
    fn normalize_and_collect_pattern(
        &self,
        pattern: &Pattern,
        scrutinee_ty: &Type,
    ) -> (Pattern, Vec<(Id, Scheme)>) {
        match pattern {
            Pattern::Var(id) => (
                Pattern::Var(*id),
                vec![(*id, Scheme::Mono(scrutinee_ty.clone()))],
            ),
            Pattern::Wildcard => (Pattern::Wildcard, vec![]),
            Pattern::Lit(l) => (Pattern::Lit(l.clone()), vec![]),
            Pattern::Tuple(pats) => {
                let elem_tys: Vec<Type> = if let Type::Tuple(es) = scrutinee_ty {
                    es.clone()
                } else {
                    vec![Type::Void; pats.len()]
                };
                let mut norm_pats = Vec::with_capacity(pats.len());
                let mut bindings = Vec::new();
                for (p, et) in pats.iter().zip(elem_tys.iter()) {
                    let (np, b) = self.normalize_and_collect_pattern(p, et);
                    norm_pats.push(np);
                    bindings.extend(b);
                }
                (Pattern::Tuple(norm_pats), bindings)
            }
            Pattern::Constructor(cname, sub_pats) => {
                let field_tys = self.instantiated_variant_fields(cname, scrutinee_ty);
                let mut norm_pats = Vec::with_capacity(sub_pats.len());
                let mut bindings = Vec::new();
                for (i, sp) in sub_pats.iter().enumerate() {
                    let ft = field_tys.get(i).cloned().unwrap_or(Type::Void);
                    let (np, b) = self.normalize_and_collect_pattern(sp, &ft);
                    norm_pats.push(np);
                    bindings.extend(b);
                }
                (Pattern::Constructor(cname.clone(), norm_pats), bindings)
            }
            Pattern::StructConstructor(cname, named_pats) => {
                let field_tys = self.instantiated_variant_fields(cname, scrutinee_ty);
                let decl_names = self
                    .find_variant_field_names(cname)
                    .unwrap_or_default();
                let mut ordered_pats = vec![Pattern::Wildcard; decl_names.len()];
                let mut bindings = Vec::new();
                for (fname, fpat) in named_pats {
                    if let Some(idx) = decl_names.iter().position(|n| n == fname) {
                        let ft = field_tys.get(idx).cloned().unwrap_or(Type::Void);
                        let (np, b) = self.normalize_and_collect_pattern(fpat, &ft);
                        ordered_pats[idx] = np;
                        bindings.extend(b);
                    }
                }
                (Pattern::Constructor(cname.clone(), ordered_pats), bindings)
            }
        }
    }

    fn find_variant_field_names(&self, ctor_name: &str) -> Option<Vec<Name>> {
        for adt_info in self.adt_registry.values() {
            for v in &adt_info.variants {
                if v.name == ctor_name {
                    return v.field_names.clone();
                }
            }
        }
        None
    }

    fn instantiated_variant_fields(&self, ctor_name: &str, scrutinee_ty: &Type) -> Vec<Type> {
        let type_args = extract_type_args(scrutinee_ty);
        for adt_info in self.adt_registry.values() {
            if let Some(v) = adt_info.variants.iter().find(|v| v.name == ctor_name) {
                let sub: HashMap<Name, Type> = adt_info
                    .type_params
                    .iter()
                    .cloned()
                    .zip(type_args.into_iter())
                    .collect();
                return v
                    .field_types
                    .iter()
                    .map(|ft| apply_type_subst(ft, &sub))
                    .collect();
            }
        }
        vec![]
    }

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

    fn trans_lambda(
        &mut self,
        lambda: Lambda,
        cap_fv: bool,
    ) -> (Vec<VarDecl>, Vec<VarDecl>, TaggedTerm) {
        let params = lambda.param;
        let bd = *lambda.body;

        let mut present = Vec::new();
        let mut backup = Vec::new();

        for VarDecl(n, t) in &params {
            if let Some(origin) = self.close_var(*n, t.clone()) {
                backup.push((*n, origin));
            }
            present.push(*n);
        }

        let body_term = self.transform(bd);
        let fvs = if cap_fv {
            let mut raw_fvs = self.fv(&body_term);
            for VarDecl(n, _) in &params {
                raw_fvs.remove(n);
            }
            // Deterministic ordering by Id
            let mut fv_ids: Vec<Id> = raw_fvs.into_iter().collect();
            fv_ids.sort_by_key(|id| self.interner.trace(*id).to_string());
            fv_ids
                .into_iter()
                .filter_map(|vn| self.env.get(&vn).map(|scm| VarDecl(vn, scm.clone())))
                .collect()
        } else {
            vec![]
        };

        for bname in present {
            self.release_var(&bname);
        }
        for (bname, bty) in backup {
            self.close_var(bname, bty);
        }

        (params, fvs, body_term)
    }

    fn transform_list(&mut self, lst: Vec<P<Form>>) -> Vec<P<TaggedTerm>> {
        lst.into_iter().map(|f| Box::new(self.transform(*f))).collect()
    }

    /// Wrap a direct global function `fn_id` in an anonymous closure when used as a first-class value!
    fn eta_wrap_global_fn(&mut self, fn_id: Id, fn_scm: Scheme) -> Term {
        if let Type::Arr(param_tys, ret_ty) = fn_scm.body().clone() {
            let cls_id = self.make_cls_name("eta");
            let mut params = Vec::with_capacity(param_tys.len());
            let mut args = Vec::with_capacity(param_tys.len());
            for (i, pt) in param_tys.iter().enumerate() {
                let pid = self.interner.intern(&format!(".eta_arg{}", i));
                params.push(VarDecl(pid, Scheme::Mono(pt.clone())));
                args.push(Box::new(TaggedTerm::new(
                    Scheme::Mono(pt.clone()),
                    Term::Var(pid),
                )));
            }
            let call_body = TaggedTerm::new(
                Scheme::Mono((*ret_ty).clone()),
                Term::ApplyDir(VarDecl(fn_id, fn_scm.clone()), args),
            );
            self.define_fn(cls_id, fn_scm.clone(), params, vec![], call_body);
            let cls = Closure::new(cls_id, vec![]);
            let fr = self.fresh();
            let tmp_var_id = self.interner.intern(&fr);
            Term::MakeCls(
                VarDecl(tmp_var_id, fn_scm.clone()),
                Box::new(cls),
                Box::new(TaggedTerm::new(fn_scm, Term::Var(tmp_var_id))),
            )
        } else {
            Term::Var(fn_id)
        }
    }

    /// Transform a reference in *callee* position (doesn't eta-wrap direct functions).
    fn resolve_var_id(&mut self, form_node: &Expr) -> Option<Id> {
        match form_node {
            Expr::Var(n) => {
                let resolved = self.import_map.get(n).copied().unwrap_or(*n);
                if self.find_var(&resolved).is_none() {
                    Some(resolved)
                } else {
                    self.direct.get(&resolved).copied()
                }
            }
            Expr::QualifiedVar(path) => {
                let q = path.to_string_with(self.interner);
                Some(self.interner.intern(&q))
            }
            _ => None,
        }
    }

    fn transform(&mut self, form: Form) -> TaggedTerm {
        use self::Expr::*;
        let Form {
            node,
            tag: FormTag { ty: tform, .. },
        } = form;

        let t = match node {
            Lit(l) => Term::Lit(l),

            Tuple(es) => Term::Tuple(self.transform_list(es)),

            StructInit(ctor_name, named_fields) => {
                let (adt_name, tag) = self
                    .find_constructor_info(&ctor_name)
                    .expect("Constructor must exist after typecheck");
                let decl_names = self
                    .find_variant_field_names(&ctor_name)
                    .expect("Struct variant must have field names");

                let mut by_name: HashMap<Name, Form> = named_fields
                    .into_iter()
                    .map(|(k, v)| (k, *v))
                    .collect();
                let mut ordered_terms = Vec::with_capacity(decl_names.len());
                let mut field_types = Vec::with_capacity(decl_names.len());
                for dn in &decl_names {
                    let f = by_name.remove(dn).expect("Field checked by typeinfer");
                    let tt = self.transform(f);
                    field_types.push(tt.ref_scheme().body().clone());
                    ordered_terms.push(Box::new(tt));
                }
                let type_args = extract_type_args(tform.body());
                Term::MakeData {
                    type_name: adt_name,
                    tag,
                    fields: ordered_terms,
                    field_types,
                    type_args,
                }
            }

            List(elems) => {
                // Desugar `[e1, e2, ...]` into `Cons(e1, Cons(e2, Nil))`
                let list_ty = tform.body().clone();
                let type_args = extract_type_args(&list_ty);
                let elem_ty = type_args.first().cloned().unwrap_or(Type::Void);

                let (_, nil_tag) = self
                    .find_constructor_info("Nil")
                    .unwrap_or(("List".to_string(), 0));
                let (_, cons_tag) = self
                    .find_constructor_info("Cons")
                    .unwrap_or(("List".to_string(), 1));

                let mut acc = TaggedTerm::new(
                    Scheme::Mono(list_ty.clone()),
                    Term::MakeData {
                        type_name: "List".to_string(),
                        tag: nil_tag,
                        fields: vec![],
                        field_types: vec![],
                        type_args: type_args.clone(),
                    },
                );

                for el_form in elems.into_iter().rev() {
                    let head_term = self.transform(*el_form);
                    acc = TaggedTerm::new(
                        Scheme::Mono(list_ty.clone()),
                        Term::MakeData {
                            type_name: "List".to_string(),
                            tag: cons_tag,
                            fields: vec![Box::new(head_term), Box::new(acc)],
                            field_types: vec![elem_ty.clone(), list_ty.clone()],
                            type_args: type_args.clone(),
                        },
                    );
                }
                return acc;
            }

            Var(n) => {
                let resolved_n = self.import_map.get(&n).copied().unwrap_or(n);
                let var_name = self.interner.trace(resolved_n).to_string();

                // Unit ADT constructor?
                if let Some((adt_name, tag)) = self.find_constructor_info(&var_name) {
                    if let Some(adt_info) = self.adt_registry.get(&adt_name) {
                        if let Some(v) = adt_info.variants.iter().find(|v| v.name == var_name) {
                            if v.field_types.is_empty() {
                                let type_args = extract_type_args(tform.body());
                                return TaggedTerm::new(
                                    tform,
                                    Term::MakeData {
                                        type_name: adt_name,
                                        tag,
                                        fields: vec![],
                                        field_types: vec![],
                                        type_args,
                                    },
                                );
                            } else {
                                // Non-unit constructor used as a first-class function! Eta-wrap it!
                                if let Type::Arr(param_tys, ret_ty) = tform.body().clone() {
                                    let cls_id = self.make_cls_name("ctor");
                                    let mut params = Vec::new();
                                    let mut fields = Vec::new();
                                    for (i, pt) in param_tys.iter().enumerate() {
                                        let pid = self.interner.intern(&format!(".ctor_arg{}", i));
                                        params.push(VarDecl(pid, Scheme::Mono(pt.clone())));
                                        fields.push(Box::new(TaggedTerm::new(
                                            Scheme::Mono(pt.clone()),
                                            Term::Var(pid),
                                        )));
                                    }
                                    let type_args = extract_type_args(&ret_ty);
                                    let body = TaggedTerm::new(
                                        Scheme::Mono((*ret_ty).clone()),
                                        Term::MakeData {
                                            type_name: adt_name,
                                            tag,
                                            fields,
                                            field_types: param_tys,
                                            type_args,
                                        },
                                    );
                                    self.define_fn(cls_id, tform.clone(), params, vec![], body);
                                    let cls = Closure::new(cls_id, vec![]);
                                    let fr = self.fresh();
                                    let tmp = self.interner.intern(&fr);
                                    return TaggedTerm::new(
                                        tform.clone(),
                                        Term::MakeCls(
                                            VarDecl(tmp, tform.clone()),
                                            Box::new(cls),
                                            Box::new(TaggedTerm::new(tform, Term::Var(tmp))),
                                        ),
                                    );
                                }
                            }
                        }
                    }
                }

                // Lambda-lifted polymorphic local `let` function referenced as a first-class value?
                if let Some((lifted_fn_id, fv_decls, lifted_scm)) =
                    self.poly_lifted.get(&resolved_n).cloned()
                {
                    if let Type::Arr(param_tys, ret_ty) = tform.body().clone() {
                        let eta_id = self.make_cls_name("poly_eta");
                        let mut params = Vec::with_capacity(param_tys.len());
                        let mut all_args: Vec<P<TaggedTerm>> = fv_decls
                            .iter()
                            .map(|VarDecl(fid, fscm)| {
                                Box::new(TaggedTerm::new(fscm.clone(), Term::Var(*fid)))
                            })
                            .collect();
                        for (i, pt) in param_tys.iter().enumerate() {
                            let pid = self.interner.intern(&format!(".poly_eta_arg{}", i));
                            params.push(VarDecl(pid, Scheme::Mono(pt.clone())));
                            all_args.push(Box::new(TaggedTerm::new(
                                Scheme::Mono(pt.clone()),
                                Term::Var(pid),
                            )));
                        }
                        let call_body = TaggedTerm::new(
                            Scheme::Mono((*ret_ty).clone()),
                            Term::ApplyDir(VarDecl(lifted_fn_id, lifted_scm), all_args),
                        );
                        let fv_ids: Vec<Id> = fv_decls.iter().map(|v| v.name()).collect();
                        self.define_fn(eta_id, tform.clone(), params, fv_decls, call_body);
                        let cls = Closure::new(eta_id, fv_ids);
                        let fr = self.fresh();
                        let tmp = self.interner.intern(&fr);
                        return TaggedTerm::new(
                            tform.clone(),
                            Term::MakeCls(
                                VarDecl(tmp, tform.clone()),
                                Box::new(cls),
                                Box::new(TaggedTerm::new(tform, Term::Var(tmp))),
                            ),
                        );
                    }
                }

                // Local variable in lexical scope?
                if self.find_var(&resolved_n).is_some() {
                    Term::Var(resolved_n)
                } else if self.global_val_ids.contains(&resolved_n) {
                    Term::GlobalVar(resolved_n)
                } else if tform.is_fn() {
                    // Top-level function referenced as a first-class value -> eta-wrap into closure!
                    let target_id = self.direct.get(&resolved_n).copied().unwrap_or(resolved_n);
                    self.eta_wrap_global_fn(target_id, tform.clone())
                } else {
                    Term::Var(resolved_n)
                }
            }

            QualifiedVar(path) => {
                let qualified_str = path.to_string_with(self.interner);
                let qualified_id = self.interner.intern(&qualified_str);

                if let Some(name) = path.name() {
                    let var_name = self.interner.trace(name).to_string();
                    if let Some((adt_name, tag)) = self.find_constructor_info(&var_name) {
                        if let Some(adt_info) = self.adt_registry.get(&adt_name) {
                            if let Some(v) = adt_info.variants.iter().find(|v| v.name == var_name) {
                                if v.field_types.is_empty() {
                                    let type_args = extract_type_args(tform.body());
                                    return TaggedTerm::new(
                                        tform,
                                        Term::MakeData {
                                            type_name: adt_name,
                                            tag,
                                            fields: vec![],
                                            field_types: vec![],
                                            type_args,
                                        },
                                    );
                                }
                            }
                        }
                    }
                }

                if self.global_val_ids.contains(&qualified_id) {
                    Term::GlobalVar(qualified_id)
                } else if tform.is_fn() {
                    self.eta_wrap_global_fn(qualified_id, tform.clone())
                } else {
                    Term::Var(qualified_id)
                }
            }

            Block(e) => Term::Block(self.transform_list(e)),
            Unary(op, e) => Term::Unary(op, Box::new(self.transform(*e))),
            If(cond, tr, fl) => Term::If(
                Box::new(self.transform(*cond)),
                Box::new(self.transform(*tr)),
                Box::new(self.transform(*fl)),
            ),
            While(cond, body) => Term::While(
                Box::new(self.transform(*cond)),
                Box::new(self.transform(*body)),
            ),
            Binary(op, left, right) => Term::Binary(
                op,
                Box::new(self.transform(*left)),
                Box::new(self.transform(*right)),
            ),

            Let(v, val, exp) => {
                let VarDecl(id, scm) = v.clone();
                // Polymorphic local lambda (`let id = (x) -> x in ...`): lambda-lift as a polymorphic top-level
                // template so each call/reference specializes independently without a dead untyped `MakeCls`.
                if !scm.type_params().is_empty() {
                    if let Abs(lambda) = val.node {
                        let bound_str = self.interner.trace(id).to_owned();
                        let cls_fn_id = self.make_cls_name(bound_str.as_str());
                        // Any inner closures inside `cls_fn_id` belong to `cls_fn_id` for specialization!
                        if let Some(parent_list) = self.lifted_closures.get_mut(&self.current) {
                            parent_list.retain(|&x| x != cls_fn_id);
                        }
                        let saved_current = self.current;
                        self.current = cls_fn_id;
                        let (ps, fv, bd) = self.trans_lambda(lambda, true);
                        self.current = saved_current;

                        let mut all_params = fv.clone();
                        all_params.extend(ps);

                        let lifted_scm = if fv.is_empty() {
                            scm.clone()
                        } else {
                            let fv_tys: Vec<Type> =
                                fv.iter().map(|VarDecl(_, s)| s.body().clone()).collect();
                            let new_body = match scm.body().clone() {
                                Type::Arr(mut ptys, rty) => {
                                    let mut full = fv_tys;
                                    full.append(&mut ptys);
                                    Type::Arr(full, rty)
                                }
                                other => other,
                            };
                            match scm {
                                Scheme::Poly(vs, _) => Scheme::Poly(vs, new_body),
                                Scheme::PolyConstrained(vs, cs, _) => {
                                    Scheme::PolyConstrained(vs, cs, new_body)
                                }
                                other => other,
                            }
                        };

                        self.define_fn(cls_fn_id, lifted_scm.clone(), all_params, vec![], bd);

                        let prev_lifted =
                            self.poly_lifted.insert(id, (cls_fn_id, fv, lifted_scm));
                        let exp_term = self.transform(*exp);
                        self.poly_lifted.remove(&id);
                        if let Some(prev) = prev_lifted {
                            self.poly_lifted.insert(id, prev);
                        }
                        return exp_term;
                    }
                }

                let origin = self.close_var(id, scm.clone());
                let ret = match val.node {
                    Abs(lambda) => {
                        let bound_str = self.interner.trace(id).to_owned();
                        let cls_fn_id = self.make_cls_name(bound_str.as_str());

                        let (ps, fv, bd) = with_var!(self.direct, id, cls_fn_id => {
                            self.trans_lambda(lambda.clone(), true)
                        });

                        let (cls_name, cls_fv) = if fv.is_empty() {
                            self.define_fn(cls_fn_id, scm, ps, fv, bd)
                        } else {
                            let (ps2, fv2, bd2) = self.trans_lambda(lambda, true);
                            self.define_fn(cls_fn_id, scm, ps2, fv2, bd2)
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
                    _ => {
                        let val_term = self.transform(*val);
                        let exp_term = self.transform(*exp);
                        Term::Let(v, Box::new(val_term), Box::new(exp_term))
                    }
                };
                self.release_var(&id);
                if let Some(prev) = origin {
                    self.close_var(id, prev);
                }
                ret
            }

            Apply(callee, params) => {
                // 1. Constructor application?
                let maybe_ctor_name = match &callee.node {
                    Expr::Var(cid) => {
                        let resolved = self.import_map.get(cid).copied().unwrap_or(*cid);
                        Some(self.interner.trace(resolved).to_string())
                    }
                    Expr::QualifiedVar(path) => {
                        path.name().map(|id| self.interner.trace(id).to_string())
                    }
                    _ => None,
                };
                if let Some(ref cname) = maybe_ctor_name {
                    if let Some((adt_name, tag)) = self.find_constructor_info(cname) {
                        let params_term = self.transform_list(params);
                        let field_types: Vec<Type> = params_term
                            .iter()
                            .map(|t| t.ref_scheme().body().clone())
                            .collect();
                        let type_args = extract_type_args(tform.body());
                        return TaggedTerm::new(
                            tform,
                            Term::MakeData {
                                type_name: adt_name,
                                tag,
                                fields: params_term,
                                field_types,
                                type_args,
                            },
                        );
                    }
                }

                // 1.5. Lambda-lifted polymorphic local `let` function call?
                if let Expr::Var(cid) = &callee.node {
                    let resolved = self.import_map.get(cid).copied().unwrap_or(*cid);
                    if let Some((lifted_fn_id, fv_decls, lifted_scm)) =
                        self.poly_lifted.get(&resolved).cloned()
                    {
                        let mut all_args: Vec<P<TaggedTerm>> = fv_decls
                            .into_iter()
                            .map(|VarDecl(fid, fscm)| {
                                Box::new(TaggedTerm::new(fscm, Term::Var(fid)))
                            })
                            .collect();
                        all_args.extend(self.transform_list(params));
                        return TaggedTerm::new(
                            tform,
                            Term::ApplyDir(VarDecl(lifted_fn_id, lifted_scm), all_args),
                        );
                    }
                }

                // 2. Direct function / concept method call?
                if let Some(direct_id) = self.resolve_var_id(&callee.node) {
                    if !self.global_val_ids.contains(&direct_id) {
                        let callee_scm = callee.tag.ty.clone();
                        let params_term = self.transform_list(params);
                        let target = self.direct.get(&direct_id).copied().unwrap_or(direct_id);
                        return TaggedTerm::new(
                            tform,
                            Term::ApplyDir(VarDecl(target, callee_scm), params_term),
                        );
                    }
                }

                // 3. First-class closure call
                let callee_term = self.transform(*callee);
                let params_term = self.transform_list(params);
                Term::ApplyCls(Box::new(callee_term), params_term)
            }

            Match(scrutinee, arms) => {
                let scrutinee_ty = scrutinee.tag.ref_type().clone();
                let scrutinee_term = self.transform(*scrutinee);

                let arms_term: Vec<_> = arms
                    .into_iter()
                    .map(|arm| {
                        let (norm_pat, bindings) =
                            self.normalize_and_collect_pattern(&arm.pattern, &scrutinee_ty);

                        let mut backup = Vec::new();
                        for (id, scheme) in &bindings {
                            if let Some(old) = self.close_var(*id, scheme.clone()) {
                                backup.push((*id, old));
                            }
                        }

                        let guard_term = arm.guard.map(|g| Box::new(self.transform(*g)));
                        let body_term = self.transform(*arm.body);

                        for (id, _) in &bindings {
                            self.release_var(id);
                        }
                        for (id, scheme) in backup {
                            self.close_var(id, scheme);
                        }

                        (norm_pat, guard_term, Box::new(body_term))
                    })
                    .collect();

                Term::Match(Box::new(scrutinee_term), arms_term)
            }

            Abs(lambda) => {
                let ty = tform.clone();
                let fr_name = self.fresh();
                let tmp_id = self.make_cls_name(fr_name.as_str());
                let (ps, fv, bd) = self.trans_lambda(lambda, true);

                let (cls_name, cls_fv) = self.define_fn(tmp_id, ty.clone(), ps, fv, bd);
                let cls = Closure::new(cls_name, cls_fv);
                let fr2 = self.fresh();
                let cls_var_id = self.interner.intern(&fr2);

                Term::MakeCls(
                    VarDecl(cls_var_id, ty.clone()),
                    Box::new(cls),
                    Box::new(TaggedTerm::new(ty, Term::Var(cls_var_id))),
                )
            }
        };

        TaggedTerm::new(tform, t)
    }
}

pub fn apply_type_subst(ty: &Type, subst: &HashMap<Name, Type>) -> Type {
    match ty {
        Type::Var(name) => subst.get(name).cloned().unwrap_or_else(|| ty.clone()),
        Type::Con(_) | Type::Void => ty.clone(),
        Type::Arr(ps, r) => Type::Arr(
            ps.iter().map(|p| apply_type_subst(p, subst)).collect(),
            Box::new(apply_type_subst(r, subst)),
        ),
        Type::Tuple(es) => Type::Tuple(es.iter().map(|e| apply_type_subst(e, subst)).collect()),
        Type::Comp(base, arg) => Type::Comp(
            Box::new(apply_type_subst(base, subst)),
            Box::new(apply_type_subst(arg, subst)),
        ),
    }
}

fn apply_scheme_subst(scm: &Scheme, subst: &HashMap<Name, Type>) -> Scheme {
    match scm {
        Scheme::Mono(ty) => Scheme::Mono(apply_type_subst(ty, subst)),
        Scheme::Poly(vars, ty) => {
            let filtered: HashMap<Name, Type> = subst
                .iter()
                .filter(|(k, _)| !vars.contains(k))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            Scheme::Poly(vars.clone(), apply_type_subst(ty, &filtered))
        }
        Scheme::PolyConstrained(vars, constraints, ty) => {
            let filtered: HashMap<Name, Type> = subst
                .iter()
                .filter(|(k, _)| !vars.contains(k))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            Scheme::PolyConstrained(
                vars.clone(),
                constraints.clone(),
                apply_type_subst(ty, &filtered),
            )
        }
        Scheme::Slot => Scheme::Slot,
    }
}

struct SpecializeContext {
    type_subst: HashMap<Name, Type>,
    fn_rename: HashMap<Id, Id>,
}

fn specialize_term(term: &TaggedTerm, ctx: &SpecializeContext) -> TaggedTerm {
    let specialized_scheme = apply_scheme_subst(term.ref_scheme(), &ctx.type_subst);
    let specialized_body = specialize_term_body(term.body(), ctx);
    TaggedTerm::new(specialized_scheme, specialized_body)
}

fn specialize_term_body(term: &Term, ctx: &SpecializeContext) -> Term {
    match term {
        Term::Lit(lit) => Term::Lit(lit.clone()),
        Term::GlobalVar(id) => Term::GlobalVar(*id),
        Term::Var(id) => {
            let new_id = ctx.fn_rename.get(id).copied().unwrap_or(*id);
            Term::Var(new_id)
        }
        Term::Tuple(items) => Term::Tuple(
            items
                .iter()
                .map(|i| Box::new(specialize_term(i, ctx)))
                .collect(),
        ),
        Term::Block(items) => Term::Block(
            items
                .iter()
                .map(|i| Box::new(specialize_term(i, ctx)))
                .collect(),
        ),
        Term::Binary(op, l, r) => Term::Binary(
            *op,
            Box::new(specialize_term(l, ctx)),
            Box::new(specialize_term(r, ctx)),
        ),
        Term::Unary(op, e) => Term::Unary(*op, Box::new(specialize_term(e, ctx))),
        Term::Let(VarDecl(id, scm), val, body) => Term::Let(
            VarDecl(*id, apply_scheme_subst(scm, &ctx.type_subst)),
            Box::new(specialize_term(val, ctx)),
            Box::new(specialize_term(body, ctx)),
        ),
        Term::If(c, t, f) => Term::If(
            Box::new(specialize_term(c, ctx)),
            Box::new(specialize_term(t, ctx)),
            Box::new(specialize_term(f, ctx)),
        ),
        Term::While(c, b) => Term::While(
            Box::new(specialize_term(c, ctx)),
            Box::new(specialize_term(b, ctx)),
        ),
        Term::ApplyDir(VarDecl(id, scm), args) => {
            let new_id = ctx.fn_rename.get(id).copied().unwrap_or(*id);
            Term::ApplyDir(
                VarDecl(new_id, apply_scheme_subst(scm, &ctx.type_subst)),
                args.iter()
                    .map(|a| Box::new(specialize_term(a, ctx)))
                    .collect(),
            )
        }
        Term::ApplyCls(callee, args) => Term::ApplyCls(
            Box::new(specialize_term(callee, ctx)),
            args.iter()
                .map(|a| Box::new(specialize_term(a, ctx)))
                .collect(),
        ),
        Term::MakeCls(VarDecl(id, scm), cls, body) => {
            let new_entry = ctx
                .fn_rename
                .get(&cls.entry())
                .copied()
                .unwrap_or_else(|| cls.entry());
            let new_cls = Closure::new(new_entry, cls.fv());
            Term::MakeCls(
                VarDecl(*id, apply_scheme_subst(scm, &ctx.type_subst)),
                Box::new(new_cls),
                Box::new(specialize_term(body, ctx)),
            )
        }
        Term::Match(scrutinee, arms) => Term::Match(
            Box::new(specialize_term(scrutinee, ctx)),
            arms.iter()
                .map(|(pat, guard, body)| {
                    (
                        pat.clone(),
                        guard.as_ref().map(|g| Box::new(specialize_term(g, ctx))),
                        Box::new(specialize_term(body, ctx)),
                    )
                })
                .collect(),
        ),
        Term::MakeData {
            type_name,
            tag,
            fields,
            field_types,
            type_args,
        } => Term::MakeData {
            type_name: type_name.clone(),
            tag: *tag,
            fields: fields
                .iter()
                .map(|f| Box::new(specialize_term(f, ctx)))
                .collect(),
            field_types: field_types
                .iter()
                .map(|t| apply_type_subst(t, &ctx.type_subst))
                .collect(),
            type_args: type_args
                .iter()
                .map(|t| apply_type_subst(t, &ctx.type_subst))
                .collect(),
        },
        Term::GetTag(node) => Term::GetTag(Box::new(specialize_term(node, ctx))),
        Term::GetField(node, idx) => Term::GetField(Box::new(specialize_term(node, ctx)), *idx),
    }
}
