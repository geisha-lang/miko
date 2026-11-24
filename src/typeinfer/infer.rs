use std::collections::{HashMap, HashSet, LinkedList};
use std::mem;
use std::ops::DerefMut;

use crate::internal::*;
use crate::syntax::form::*;
use crate::types::{self, *};
use crate::utils::*;

use super::constraint::Constraint;
pub use super::error::TypeError;
use super::mono::{mangle_type, InstantiationRegistry};
use super::subst::*;

#[derive(Clone, Debug)]
struct WantedConstraint {
    concept: Name,
    ty: Type,
    span: Span,
}

#[derive(Debug)]
pub struct Infer<'interner> {
    unique: usize,
    interner: &'interner mut Interner,
    constraints: LinkedList<Constraint>,
    subst: Subst,
    wanted_constraints: Vec<WantedConstraint>,
    /// Type aliases: name -> (type_params, target_type)
    pub alias_registry: HashMap<Name, (Vec<Name>, Type)>,
    /// ADT definitions
    pub adt_registry: AdtRegistry,
    /// Concept definitions
    pub concept_registry: ConceptRegistry,
    /// Instance definitions
    pub instance_registry: InstanceRegistry,
    /// Method name -> (Concept name, index of constrained type parameter in Concept)
    pub concept_methods: HashMap<Name, (Name, usize)>,
    /// Polymorphic function registry for monomorphization
    pub instantiation_registry: InstantiationRegistry,
    /// Names of top-level value (non-function) definitions
    pub global_values: HashSet<Id>,
}

fn to_mono(ty: Type) -> Scheme {
    Scheme::Mono(ty)
}

impl<'i> Infer<'i> {
    pub fn new(interner: &'i mut Interner) -> Infer<'i> {
        let mut inf = Infer {
            unique: 0,
            interner,
            constraints: LinkedList::new(),
            subst: HashMap::new(),
            wanted_constraints: Vec::new(),
            alias_registry: HashMap::new(),
            adt_registry: HashMap::new(),
            concept_registry: HashMap::new(),
            instance_registry: Vec::new(),
            concept_methods: HashMap::new(),
            instantiation_registry: InstantiationRegistry::new(),
            global_values: HashSet::new(),
        };
        inf.register_builtin_concepts();
        inf.register_default_list_adt();
        inf
    }

    pub fn new_env<'a>() -> TypeEnv<'a> {
        TypeEnv::new()
    }

    fn register_builtin_concepts(&mut self) {
        for (concept, prims) in [
            ("$Num", &["Int", "Float"][..]),
            ("$EqOp", &["Int", "Float", "Bool", "Char", "String"][..]),
            ("$OrdOp", &["Int", "Float", "Char", "String"][..]),
        ] {
            self.concept_registry.insert(
                concept.to_string(),
                ConceptInfo {
                    name: concept.to_string(),
                    type_params: vec!["a".to_string()],
                    superclasses: vec![],
                    methods: vec![],
                },
            );
            for p in prims {
                self.instance_registry.push(InstanceInfo {
                    concept_name: concept.to_string(),
                    type_params: vec![],
                    constraints: vec![],
                    target_types: vec![Type::Con((*p).to_string())],
                    method_impls: HashMap::new(),
                });
            }
        }
    }

    fn register_default_list_adt(&mut self) {
        let list_ty = Type::compose(Type::Con("List".to_string()), Type::Var("a".to_string()));
        let nil_scm = Scheme::Poly(vec!["a".to_string()], list_ty.clone());
        let cons_ty = Type::Arr(
            vec![Type::Var("a".to_string()), list_ty.clone()],
            P(list_ty),
        );
        let cons_scm = Scheme::Poly(vec!["a".to_string()], cons_ty);
        self.adt_registry.insert(
            "List".to_string(),
            AdtInfo {
                name: "List".to_string(),
                type_params: vec!["a".to_string()],
                variants: vec![
                    VariantInfo {
                        name: "Nil".to_string(),
                        tag: 0,
                        field_names: None,
                        field_types: vec![],
                        scheme: nil_scm,
                    },
                    VariantInfo {
                        name: "Cons".to_string(),
                        tag: 1,
                        field_names: None,
                        field_types: vec![
                            Type::Var("a".to_string()),
                            Type::compose(
                                Type::Con("List".to_string()),
                                Type::Var("a".to_string()),
                            ),
                        ],
                        scheme: cons_scm,
                    },
                ],
            },
        );
    }

    fn unique(&mut self) -> usize {
        self.unique += 1;
        self.unique
    }

    fn fresh(&mut self) -> Type {
        Type::Var(format!(".{}", self.unique()))
    }

    fn uni(&mut self, left: &Type, right: &Type, span: Span) {
        self.constraints
            .push_back(Constraint(left.clone(), right.clone(), span));
    }

    /// Expand type aliases recursively, detecting cyclic definitions.
    pub fn expand_type(&self, ty: &Type, span: Span) -> Result<Type, TypeError> {
        let mut visiting = HashSet::new();
        self.expand_type_inner(ty, &mut visiting, span)
    }

    fn expand_type_inner(
        &self,
        ty: &Type,
        visiting: &mut HashSet<Name>,
        span: Span,
    ) -> Result<Type, TypeError> {
        match ty {
            Type::Void | Type::Var(_) => Ok(ty.clone()),
            Type::Con(name) => {
                if let Some((params, target)) = self.alias_registry.get(name) {
                    if !params.is_empty() {
                        return Err(TypeError::TypeAliasError(
                            format!(
                                "Type alias '{}' expects {} type argument(s), got 0",
                                name,
                                params.len()
                            ),
                            span,
                        ));
                    }
                    if !visiting.insert(name.clone()) {
                        return Err(TypeError::TypeAliasError(
                            format!("Cyclic type alias involving '{}'", name),
                            span,
                        ));
                    }
                    let res = self.expand_type_inner(target, visiting, span)?;
                    visiting.remove(name);
                    Ok(res)
                } else {
                    Ok(ty.clone())
                }
            }
            Type::Arr(params, ret) => {
                let mut new_ps = Vec::with_capacity(params.len());
                for p in params {
                    new_ps.push(self.expand_type_inner(p, visiting, span)?);
                }
                let new_r = self.expand_type_inner(ret, visiting, span)?;
                Ok(Type::Arr(new_ps, P(new_r)))
            }
            Type::Tuple(elems) => {
                let mut new_es = Vec::with_capacity(elems.len());
                for e in elems {
                    new_es.push(self.expand_type_inner(e, visiting, span)?);
                }
                Ok(Type::Tuple(new_es))
            }
            Type::Comp(_, _) => {
                // Flatten application spine: head arg1 arg2 ...
                let mut args = Vec::new();
                let mut curr = ty;
                while let Type::Comp(base, arg) = curr {
                    args.push(self.expand_type_inner(arg, visiting, span)?);
                    curr = base.as_ref();
                }
                args.reverse();

                if let Type::Con(name) = curr {
                    if let Some((params, target)) = self.alias_registry.get(name) {
                        if params.len() != args.len() {
                            return Err(TypeError::TypeAliasError(
                                format!(
                                    "Type alias '{}' expects {} type argument(s), got {}",
                                    name,
                                    params.len(),
                                    args.len()
                                ),
                                span,
                            ));
                        }
                        if !visiting.insert(name.clone()) {
                            return Err(TypeError::TypeAliasError(
                                format!("Cyclic type alias involving '{}'", name),
                                span,
                            ));
                        }
                        let sub: Subst = params.iter().cloned().zip(args.into_iter()).collect();
                        let substituted = target.clone().apply(&sub);
                        let res = self.expand_type_inner(&substituted, visiting, span)?;
                        visiting.remove(name);
                        return Ok(res);
                    }
                }

                let head = self.expand_type_inner(curr, visiting, span)?;
                Ok(Type::compose_n(std::iter::once(head).chain(args)))
            }
        }
    }

    pub fn expand_scheme(&self, scm: &Scheme, span: Span) -> Result<Scheme, TypeError> {
        match scm {
            Scheme::Mono(t) => Ok(Scheme::Mono(self.expand_type(t, span)?)),
            Scheme::Poly(vs, t) => Ok(Scheme::Poly(vs.clone(), self.expand_type(t, span)?)),
            Scheme::PolyConstrained(vs, cs, t) => Ok(Scheme::PolyConstrained(
                vs.clone(),
                cs.clone(),
                self.expand_type(t, span)?,
            )),
            Scheme::Slot => Ok(Scheme::Slot),
        }
    }

    /// Instantiate a scheme with fresh type variables and emit wanted constraints.
    fn instantiate(&mut self, scm: &Scheme, span: Span) -> Type {
        match scm {
            Scheme::Mono(ty) => ty.clone(),
            Scheme::Poly(tvs, ty) => {
                let fresh_vars: Vec<Type> = tvs.iter().map(|_| self.fresh()).collect();
                let sub: Subst = tvs.iter().cloned().zip(fresh_vars).collect();
                ty.clone().apply(&sub)
            }
            Scheme::PolyConstrained(tvs, constraints, ty) => {
                let fresh_vars: Vec<Type> = tvs.iter().map(|_| self.fresh()).collect();
                let sub: Subst = tvs.iter().cloned().zip(fresh_vars).collect();
                for c in constraints {
                    let arg_ty = sub
                        .get(&c.type_arg)
                        .cloned()
                        .unwrap_or_else(|| Type::Var(c.type_arg.clone()));
                    self.wanted_constraints.push(WantedConstraint {
                        concept: c.concept.clone(),
                        ty: arg_ty,
                        span,
                    });
                }
                ty.clone().apply(&sub)
            }
            Scheme::Slot => unreachable!("Cannot instantiate Scheme::Slot"),
        }
    }

    /// Match an instance pattern type against a concrete/partially-concrete type.
    fn match_instance_pattern(pattern: &Type, target: &Type, sub: &mut Subst) -> bool {
        match (pattern, target) {
            (Type::Var(v), t) => {
                if let Some(existing) = sub.get(v) {
                    existing == t
                } else {
                    sub.insert(v.clone(), t.clone());
                    true
                }
            }
            (Type::Void, Type::Void) => true,
            (Type::Con(a), Type::Con(b)) => a == b,
            (Type::Comp(b1, a1), Type::Comp(b2, a2)) => {
                Self::match_instance_pattern(b1, b2, sub)
                    && Self::match_instance_pattern(a1, a2, sub)
            }
            (Type::Tuple(es1), Type::Tuple(es2)) if es1.len() == es2.len() => es1
                .iter()
                .zip(es2.iter())
                .all(|(x, y)| Self::match_instance_pattern(x, y, sub)),
            (Type::Arr(ps1, r1), Type::Arr(ps2, r2)) if ps1.len() == ps2.len() => {
                ps1.iter()
                    .zip(ps2.iter())
                    .all(|(x, y)| Self::match_instance_pattern(x, y, sub))
                    && Self::match_instance_pattern(r1, r2, sub)
            }
            _ => false,
        }
    }

    fn given_entails(&self, given: &[(Name, Name)], concept: &str, var: &str) -> bool {
        let mut stack: Vec<Name> = given
            .iter()
            .filter(|(_, v)| v == var)
            .map(|(c, _)| c.clone())
            .collect();
        let mut visited = HashSet::new();
        while let Some(curr) = stack.pop() {
            if curr == concept {
                return true;
            }
            if visited.insert(curr.clone()) {
                if let Some(cinfo) = self.concept_registry.get(&curr) {
                    for sup in &cinfo.superclasses {
                        stack.push(sup.concept.clone());
                    }
                }
            }
        }
        false
    }

    /// Check/reduce a wanted constraint. Residual `Var` constraints are pushed into `residual`.
    fn discharge_constraint(
        &self,
        wc: &WantedConstraint,
        given: &[(Name, Name)],
        residual: &mut Vec<WantedConstraint>,
        depth: usize,
    ) -> Result<(), TypeError> {
        if depth > 32 {
            return Err(TypeError::NoInstance {
                concept: wc.concept.clone(),
                ty: wc.ty.clone(),
                span: wc.span,
            });
        }
        if let Type::Var(ref v) = wc.ty {
            if self.given_entails(given, &wc.concept, v) {
                return Ok(());
            }
            if !residual
                .iter()
                .any(|r| r.concept == wc.concept && r.ty == wc.ty)
            {
                residual.push(wc.clone());
            }
            return Ok(());
        }

        // Non-variable head: look up matching instance in `instance_registry`
        for inst in &self.instance_registry {
            if inst.concept_name != wc.concept || inst.target_types.is_empty() {
                continue;
            }
            let mut sub = HashMap::new();
            if Self::match_instance_pattern(&inst.target_types[0], &wc.ty, &mut sub) {
                // Recursively verify prerequisite constraints
                for prereq in &inst.constraints {
                    let prereq_ty = sub
                        .get(&prereq.type_arg)
                        .cloned()
                        .unwrap_or_else(|| Type::Var(prereq.type_arg.clone()));
                    let sub_wc = WantedConstraint {
                        concept: prereq.concept.clone(),
                        ty: prereq_ty,
                        span: wc.span,
                    };
                    self.discharge_constraint(&sub_wc, given, residual, depth + 1)?;
                }
                return Ok(());
            }
        }

        Err(TypeError::NoInstance {
            concept: wc.concept.clone(),
            ty: wc.ty.clone(),
            span: wc.span,
        })
    }

    fn process_adt(
        &mut self,
        adt_name: &str,
        type_params: &[Id],
        variants: &[Variant],
        span: Span,
    ) -> Result<Vec<(Id, Scheme)>, TypeError> {
        let type_param_names: Vec<Name> = type_params
            .iter()
            .map(|id| self.interner.trace(*id).to_string())
            .collect();

        let result_type = if type_param_names.is_empty() {
            Type::Con(adt_name.to_string())
        } else {
            let base = Type::Con(adt_name.to_string());
            let params: Vec<Type> = type_param_names
                .iter()
                .map(|p| Type::Var(p.clone()))
                .collect();
            Type::compose_n(std::iter::once(base).chain(params))
        };

        let mut constructors = Vec::new();
        let mut variant_infos = Vec::new();

        for (tag, variant) in variants.iter().enumerate() {
            let (field_names, raw_fields): (Option<Vec<Name>>, Vec<Type>) = match &variant.body {
                VariantBody::Unit => (None, vec![]),
                VariantBody::Tuple(fields) => (
                    None,
                    fields.iter().map(|f| (*f.ty).clone()).collect(),
                ),
                VariantBody::Struct(fields) => {
                    let names: Vec<Name> = fields
                        .iter()
                        .map(|f| f.name.clone().unwrap_or_default())
                        .collect();
                    let tys: Vec<Type> = fields.iter().map(|f| (*f.ty).clone()).collect();
                    (Some(names), tys)
                }
            };

            let mut field_types = Vec::with_capacity(raw_fields.len());
            for ft in &raw_fields {
                field_types.push(self.expand_type(ft, span)?);
            }

            let constructor_type = if field_types.is_empty() {
                result_type.clone()
            } else {
                Type::Arr(field_types.clone(), P(result_type.clone()))
            };

            let scheme = if type_param_names.is_empty() {
                Scheme::Mono(constructor_type)
            } else {
                Scheme::Poly(type_param_names.clone(), constructor_type)
            };

            let constructor_id = self.interner.intern(&variant.name);
            constructors.push((constructor_id, scheme.clone()));

            variant_infos.push(VariantInfo {
                name: variant.name.clone(),
                tag,
                field_names,
                field_types,
                scheme,
            });
        }

        self.adt_registry.insert(
            adt_name.to_string(),
            AdtInfo {
                name: adt_name.to_string(),
                type_params: type_param_names,
                variants: variant_infos,
            },
        );

        Ok(constructors)
    }

    fn process_concept(
        &mut self,
        concept_name: &str,
        type_params: &[Id],
        superclasses: &[TypeConstraint],
        methods: &[MethodDecl],
        span: Span,
    ) -> Result<Vec<(Id, Scheme)>, TypeError> {
        let param_names: Vec<Name> = type_params
            .iter()
            .map(|id| self.interner.trace(*id).to_string())
            .collect();
        let primary_var = param_names
            .first()
            .cloned()
            .ok_or_else(|| TypeError::TypeAliasError("Concept must have a type parameter".to_string(), span))?;

        let sups: Vec<SchemeConstraint> = superclasses
            .iter()
            .map(|c| SchemeConstraint {
                concept: c.concept.clone(),
                type_arg: c.type_var.clone(),
            })
            .collect();

        let mut env_entries = Vec::new();
        let mut method_infos = Vec::new();

        for m in methods {
            let m_name = self.interner.trace(m.name).to_string();
            let expanded_scm = self.expand_scheme(&m.ty, span)?;
            let body_ty = expanded_scm.body().clone();

            // Build method scheme: `forall <concept_params> <extra_method_params>. (Concept a, ...) => body_ty`
            let mut binders = param_names.clone();
            for fv in body_ty.ftv_ordered() {
                if !binders.contains(&fv) {
                    binders.push(fv);
                }
            }
            let mut constraints = vec![SchemeConstraint {
                concept: concept_name.to_string(),
                type_arg: primary_var.clone(),
            }];
            for extra_c in expanded_scm.constraints() {
                if !constraints.contains(extra_c) {
                    constraints.push(extra_c.clone());
                }
            }

            let full_scheme = Scheme::PolyConstrained(binders, constraints, body_ty);
            env_entries.push((m.name, full_scheme.clone()));
            method_infos.push((m_name.clone(), full_scheme));
            self.concept_methods
                .insert(m_name, (concept_name.to_string(), 0));
        }

        self.concept_registry.insert(
            concept_name.to_string(),
            ConceptInfo {
                name: concept_name.to_string(),
                type_params: param_names,
                superclasses: sups,
                methods: method_infos,
            },
        );

        Ok(env_entries)
    }

    /// Register an `instance` and synthesize concrete/generic method `Def`s for typechecking and codegen.
    fn process_instance(
        &mut self,
        concept_name: &str,
        type_args: &[Type],
        constraints: &[TypeConstraint],
        methods: &[MethodImpl],
        span: Span,
    ) -> Result<Vec<Def>, TypeError> {
        let cinfo = self
            .concept_registry
            .get(concept_name)
            .cloned()
            .ok_or_else(|| TypeError::NotInScope(concept_name.to_string(), span))?;

        if type_args.len() != cinfo.type_params.len() {
            return Err(TypeError::TypeAliasError(
                format!(
                    "Concept '{}' expects {} type argument(s), got {}",
                    concept_name,
                    cinfo.type_params.len(),
                    type_args.len()
                ),
                span,
            ));
        }

        let mut expanded_args = Vec::with_capacity(type_args.len());
        for ta in type_args {
            expanded_args.push(self.expand_type(ta, span)?);
        }

        // Collect free type variables of the instance head
        let mut inst_tvs = Vec::new();
        for ta in &expanded_args {
            for fv in ta.ftv_ordered() {
                if !inst_tvs.contains(&fv) {
                    inst_tvs.push(fv);
                }
            }
        }
        for c in constraints {
            if !inst_tvs.contains(&c.type_var) {
                inst_tvs.push(c.type_var.clone());
            }
        }

        let inst_constraints: Vec<SchemeConstraint> = constraints
            .iter()
            .map(|c| SchemeConstraint {
                concept: c.concept.clone(),
                type_arg: c.type_var.clone(),
            })
            .collect();

        let type_args_mangled = expanded_args
            .iter()
            .map(mangle_type)
            .collect::<Vec<_>>()
            .join("_");

        let param_sub: Subst = cinfo
            .type_params
            .iter()
            .cloned()
            .zip(expanded_args.iter().cloned())
            .collect();

        let mut method_impls_map = HashMap::new();
        let mut synth_defs = Vec::new();

        for (expected_name, expected_scm) in &cinfo.methods {
            let m_impl = methods
                .iter()
                .find(|mi| self.interner.trace(mi.name) == expected_name)
                .ok_or_else(|| {
                    TypeError::NoInstance {
                        concept: format!(
                            "{} (missing method '{}')",
                            concept_name, expected_name
                        ),
                        ty: expanded_args[0].clone(),
                        span,
                    }
                })?;

            let mangled_fn_name = format!(
                "__inst_{}_{}_{}",
                concept_name.replace('.', "$"),
                type_args_mangled,
                expected_name
            );
            let mangled_id = self.interner.intern(&mangled_fn_name);
            method_impls_map.insert(expected_name.clone(), mangled_fn_name);

            // Substitute concept parameter in method body type
            let inst_method_ty = expected_scm.body().clone().apply(&param_sub);
            let mut method_tvs = inst_tvs.clone();
            for fv in inst_method_ty.ftv_ordered() {
                if !method_tvs.contains(&fv) {
                    method_tvs.push(fv);
                }
            }

            let method_scheme = if method_tvs.is_empty() {
                Scheme::Mono(inst_method_ty)
            } else if inst_constraints.is_empty() {
                Scheme::Poly(method_tvs, inst_method_ty)
            } else {
                Scheme::PolyConstrained(method_tvs, inst_constraints.clone(), inst_method_ty)
            };

            let mut body_form = (*m_impl.body).clone();
            body_form.tag.annotate = Some(method_scheme);
            synth_defs.push(Def::value_with_visibility(
                span,
                mangled_id,
                Box::new(body_form),
                Visibility::Public,
            ));
        }

        self.instance_registry.push(InstanceInfo {
            concept_name: concept_name.to_string(),
            type_params: inst_tvs,
            constraints: inst_constraints,
            target_types: expanded_args,
            method_impls: method_impls_map,
        });

        Ok(synth_defs)
    }

    fn find_constructor_adt(&self, ctor_name: &str) -> Option<AdtInfo> {
        for adt_info in self.adt_registry.values() {
            if adt_info.variants.iter().any(|v| v.name == ctor_name) {
                return Some(adt_info.clone());
            }
        }
        None
    }

    fn infer_pattern(
        &mut self,
        pattern: &Pattern,
        span: Span,
    ) -> Result<(Type, Vec<(Id, Scheme)>), TypeError> {
        let mut seen_vars = HashSet::new();
        self.infer_pattern_inner(pattern, &mut seen_vars, span)
    }

    fn infer_pattern_inner(
        &mut self,
        pattern: &Pattern,
        seen_vars: &mut HashSet<Id>,
        span: Span,
    ) -> Result<(Type, Vec<(Id, Scheme)>), TypeError> {
        match pattern {
            Pattern::Var(id) => {
                if !seen_vars.insert(*id) {
                    return Err(TypeError::PatternError(
                        format!(
                            "Duplicate variable '{}' in pattern",
                            self.interner.trace(*id)
                        ),
                        span,
                    ));
                }
                let fresh_ty = self.fresh();
                Ok((fresh_ty.clone(), vec![(*id, Scheme::Mono(fresh_ty))]))
            }
            Pattern::Wildcard => Ok((self.fresh(), vec![])),
            Pattern::Lit(lit) => Ok((lit.lit_type(), vec![])),
            Pattern::Tuple(pats) => {
                let mut elem_tys = Vec::with_capacity(pats.len());
                let mut bindings = Vec::new();
                for p in pats {
                    let (t, b) = self.infer_pattern_inner(p, seen_vars, span)?;
                    elem_tys.push(t);
                    bindings.extend(b);
                }
                Ok((Type::Tuple(elem_tys), bindings))
            }
            Pattern::Constructor(name, sub_patterns) => {
                let adt_info = self
                    .find_constructor_adt(name)
                    .ok_or_else(|| TypeError::NotInScope(name.clone(), span))?;
                let variant = adt_info
                    .variants
                    .iter()
                    .find(|v| &v.name == name)
                    .unwrap();

                let ctor_type = self.instantiate(&variant.scheme, span);
                let mut all_bindings = Vec::new();

                if sub_patterns.is_empty() {
                    if !variant.field_types.is_empty() {
                        return Err(TypeError::PatternError(
                            format!(
                                "Constructor '{}' expects {} argument(s), got 0",
                                name,
                                variant.field_types.len()
                            ),
                            span,
                        ));
                    }
                    Ok((ctor_type, all_bindings))
                } else if let Type::Arr(field_types, result_ty) = ctor_type {
                    if field_types.len() != sub_patterns.len() {
                        return Err(TypeError::PatternError(
                            format!(
                                "Constructor '{}' expects {} argument(s), got {}",
                                name,
                                field_types.len(),
                                sub_patterns.len()
                            ),
                            span,
                        ));
                    }
                    for (sub_pat, expected_ty) in sub_patterns.iter().zip(field_types.iter()) {
                        let (pat_ty, bindings) =
                            self.infer_pattern_inner(sub_pat, seen_vars, span)?;
                        self.uni(&pat_ty, expected_ty, span);
                        all_bindings.extend(bindings);
                    }
                    Ok((*result_ty, all_bindings))
                } else {
                    Err(TypeError::PatternError(
                        format!("Unit constructor '{}' takes no arguments", name),
                        span,
                    ))
                }
            }
            Pattern::StructConstructor(name, named_pats) => {
                let adt_info = self
                    .find_constructor_adt(name)
                    .ok_or_else(|| TypeError::NotInScope(name.clone(), span))?;
                let variant = adt_info
                    .variants
                    .iter()
                    .find(|v| &v.name == name)
                    .unwrap();
                let decl_names = variant.field_names.as_ref().ok_or_else(|| {
                    TypeError::PatternError(
                        format!("Variant '{}' is not a struct variant", name),
                        span,
                    )
                })?;

                let ctor_type = self.instantiate(&variant.scheme, span);
                if let Type::Arr(field_types, result_ty) = ctor_type {
                    let mut all_bindings = Vec::new();
                    for (fname, fpat) in named_pats {
                        let idx = decl_names.iter().position(|n| n == fname).ok_or_else(|| {
                            TypeError::PatternError(
                                format!("Variant '{}' has no field '{}'", name, fname),
                                span,
                            )
                        })?;
                        let (pat_ty, bindings) =
                            self.infer_pattern_inner(fpat, seen_vars, span)?;
                        self.uni(&pat_ty, &field_types[idx], span);
                        all_bindings.extend(bindings);
                    }
                    Ok((*result_ty, all_bindings))
                } else {
                    Err(TypeError::PatternError(
                        format!("Variant '{}' has no fields", name),
                        span,
                    ))
                }
            }
        }
    }

    // ========================================================================
    // Pattern Exhaustiveness Checking (Maranget usefulness check)
    // ========================================================================

    fn normalize_struct_pat(&self, pat: &Pattern) -> Pattern {
        match pat {
            Pattern::StructConstructor(name, fields) => {
                if let Some(adt_info) = self.find_constructor_adt(name) {
                    if let Some(v) = adt_info.variants.iter().find(|v| &v.name == name) {
                        if let Some(ref fnames) = v.field_names {
                            let ordered: Vec<Pattern> = fnames
                                .iter()
                                .map(|fnm| {
                                    fields
                                        .iter()
                                        .find(|(k, _)| k == fnm)
                                        .map(|(_, p)| self.normalize_struct_pat(p))
                                        .unwrap_or(Pattern::Wildcard)
                                })
                                .collect();
                            return Pattern::Constructor(name.clone(), ordered);
                        }
                    }
                }
                Pattern::Constructor(name.clone(), vec![])
            }
            Pattern::Constructor(n, ps) => Pattern::Constructor(
                n.clone(),
                ps.iter().map(|p| self.normalize_struct_pat(p)).collect(),
            ),
            Pattern::Tuple(ps) => {
                Pattern::Tuple(ps.iter().map(|p| self.normalize_struct_pat(p)).collect())
            }
            other => other.clone(),
        }
    }

    /// Find a missing witness pattern if `matrix` does not exhaustively cover `tys`, else `None`.
    fn find_missing_pattern(&self, matrix: &[Vec<Pattern>], tys: &[Type]) -> Option<String> {
        if tys.is_empty() {
            return if matrix.is_empty() {
                Some("_".to_string())
            } else {
                None
            };
        }
        if matrix.is_empty() {
            return Some("_".to_string());
        }

        let first_ty = &tys[0];
        let rest_tys = &tys[1..];

        let is_wild = |p: &Pattern| matches!(p, Pattern::Wildcard | Pattern::Var(_));

        // Maranget base case (empty constructor signature Σ = ∅ in column 0):
        // When every row in column 0 is a wildcard/variable, peel column 0 directly without splitting constructors
        // so recursive ADTs (`List a`, `Even`/`Odd`, `Tree a`) terminate immediately!
        if matrix.iter().all(|row| is_wild(&row[0])) {
            let sub_matrix: Vec<Vec<Pattern>> = matrix.iter().map(|r| r[1..].to_vec()).collect();
            return self.find_missing_pattern(&sub_matrix, rest_tys);
        }

        match first_ty {
            Type::Void => {
                let mut sub_matrix = Vec::new();
                for row in matrix {
                    match &row[0] {
                        Pattern::Lit(Lit::Unit) | Pattern::Wildcard | Pattern::Var(_) => {
                            sub_matrix.push(row[1..].to_vec());
                        }
                        _ => {}
                    }
                }
                self.find_missing_pattern(&sub_matrix, rest_tys)
                    .map(|_| "()".to_string())
            }
            Type::Tuple(elem_tys) => {
                let k = elem_tys.len();
                let mut new_tys = elem_tys.clone();
                new_tys.extend_from_slice(rest_tys);
                let mut sub_matrix = Vec::new();
                for row in matrix {
                    match &row[0] {
                        Pattern::Tuple(ps) if ps.len() == k => {
                            let mut new_row = ps.clone();
                            new_row.extend_from_slice(&row[1..]);
                            sub_matrix.push(new_row);
                        }
                        p if is_wild(p) => {
                            let mut new_row = vec![Pattern::Wildcard; k];
                            new_row.extend_from_slice(&row[1..]);
                            sub_matrix.push(new_row);
                        }
                        _ => {}
                    }
                }
                self.find_missing_pattern(&sub_matrix, &new_tys)
            }
            Type::Con(name) if name == "Bool" => {
                for b in [true, false] {
                    let mut sub_matrix = Vec::new();
                    for row in matrix {
                        match &row[0] {
                            Pattern::Lit(Lit::Bool(v)) if *v == b => {
                                sub_matrix.push(row[1..].to_vec());
                            }
                            p if is_wild(p) => {
                                sub_matrix.push(row[1..].to_vec());
                            }
                            _ => {}
                        }
                    }
                    if self.find_missing_pattern(&sub_matrix, rest_tys).is_some() {
                        return Some(b.to_string());
                    }
                }
                None
            }
            _ => {
                // Check if `first_ty` is an ADT
                if let Some((adt_name, type_args)) = self.decompose_adt_type(first_ty) {
                    if let Some(adt_info) = self.adt_registry.get(&adt_name) {
                        let sub: Subst = adt_info
                            .type_params
                            .iter()
                            .cloned()
                            .zip(type_args.into_iter())
                            .collect();
                        for variant in &adt_info.variants {
                            let arity = variant.field_types.len();
                            let field_tys: Vec<Type> = variant
                                .field_types
                                .iter()
                                .map(|ft| ft.clone().apply(&sub))
                                .collect();
                            let mut new_tys = field_tys;
                            new_tys.extend_from_slice(rest_tys);

                            let mut sub_matrix = Vec::new();
                            for row in matrix {
                                match &row[0] {
                                    Pattern::Constructor(cname, sub_pats)
                                        if cname == &variant.name && sub_pats.len() == arity =>
                                    {
                                        let mut new_row = sub_pats.clone();
                                        new_row.extend_from_slice(&row[1..]);
                                        sub_matrix.push(new_row);
                                    }
                                    p if is_wild(p) => {
                                        let mut new_row = vec![Pattern::Wildcard; arity];
                                        new_row.extend_from_slice(&row[1..]);
                                        sub_matrix.push(new_row);
                                    }
                                    _ => {}
                                }
                            }
                            if let Some(inner_missing) =
                                self.find_missing_pattern(&sub_matrix, &new_tys)
                            {
                                return if arity == 0 {
                                    Some(variant.name.clone())
                                } else {
                                    Some(format!("{}({})", variant.name, inner_missing))
                                };
                            }
                        }
                        return None;
                    }
                }

                // Infinite domain (`Int`, `Float`, `Char`, `String`, or type variable):
                // requires at least one wildcard/var row covering `rest_tys`.
                let mut wild_matrix = Vec::new();
                for row in matrix {
                    if is_wild(&row[0]) {
                        wild_matrix.push(row[1..].to_vec());
                    }
                }
                self.find_missing_pattern(&wild_matrix, rest_tys)
            }
        }
    }

    fn decompose_adt_type(&self, ty: &Type) -> Option<(Name, Vec<Type>)> {
        let mut args = Vec::new();
        let mut curr = ty;
        while let Type::Comp(base, arg) = curr {
            args.push((**arg).clone());
            curr = base.as_ref();
        }
        args.reverse();
        if let Type::Con(name) = curr {
            if self.adt_registry.contains_key(name) {
                return Some((name.clone(), args));
            }
        }
        None
    }

    fn check_exhaustiveness_in_form(&self, form: &Form) -> Result<(), TypeError> {
        match &form.node {
            Expr::Match(scrutinee, arms) => {
                self.check_exhaustiveness_in_form(scrutinee)?;
                let mut matrix = Vec::new();
                for arm in arms {
                    if let Some(ref g) = arm.guard {
                        self.check_exhaustiveness_in_form(g)?;
                    }
                    self.check_exhaustiveness_in_form(&arm.body)?;
                    // Guarded arms cannot guarantee coverage
                    if arm.guard.is_none() {
                        matrix.push(vec![self.normalize_struct_pat(&arm.pattern)]);
                    }
                }
                let scrut_ty = scrutinee.tag.ref_type().clone();
                if let Some(missing) = self.find_missing_pattern(&matrix, &[scrut_ty]) {
                    return Err(TypeError::NonExhaustiveMatch(missing, form.tag.pos));
                }
                Ok(())
            }
            Expr::Tuple(es) | Expr::List(es) | Expr::Block(es) => {
                for e in es {
                    self.check_exhaustiveness_in_form(e)?;
                }
                Ok(())
            }
            Expr::StructInit(_, fs) => {
                for (_, v) in fs {
                    self.check_exhaustiveness_in_form(v)?;
                }
                Ok(())
            }
            Expr::Apply(c, args) => {
                self.check_exhaustiveness_in_form(c)?;
                for a in args {
                    self.check_exhaustiveness_in_form(a)?;
                }
                Ok(())
            }
            Expr::Abs(fun) => self.check_exhaustiveness_in_form(&fun.body),
            Expr::Binary(_, l, r) => {
                self.check_exhaustiveness_in_form(l)?;
                self.check_exhaustiveness_in_form(r)
            }
            Expr::Unary(_, e) => self.check_exhaustiveness_in_form(e),
            Expr::Let(_, v, b) => {
                self.check_exhaustiveness_in_form(v)?;
                self.check_exhaustiveness_in_form(b)
            }
            Expr::If(c, t, f) => {
                self.check_exhaustiveness_in_form(c)?;
                self.check_exhaustiveness_in_form(t)?;
                self.check_exhaustiveness_in_form(f)
            }
            Expr::Lit(_) | Expr::Var(_) | Expr::QualifiedVar(_) => Ok(()),
        }
    }

    // ========================================================================
    // Main HM Expression Inference
    // ========================================================================

    fn env_ftv_with_subst(env: &TypeEnv, sub: &Subst) -> HashSet<Name> {
        let mut out = HashSet::new();
        Self::collect_env_ftv_with_subst(env, sub, &mut out);
        out
    }

    fn collect_env_ftv_with_subst(env: &TypeEnv, sub: &Subst, out: &mut HashSet<Name>) {
        for scm in env.local_values() {
            out.extend(scm.clone().apply(sub).ftv());
        }
        if let Some(p) = env.parent_env() {
            Self::collect_env_ftv_with_subst(p, sub, out);
        }
    }

    fn apply_subst_to_env(env: &mut TypeEnv, sub: &Subst) {
        env.map_local_mut(|scm| scm.apply_mut(sub));
    }

    fn generalize_type(
        &self,
        env_ftvs: &HashSet<Name>,
        ty: Type,
        wanted: &mut Vec<WantedConstraint>,
        given: &[(Name, Name)],
    ) -> Result<Scheme, TypeError> {
        let mut residual = Vec::new();
        for wc in wanted.drain(..) {
            self.discharge_constraint(&wc, given, &mut residual, 0)?;
        }

        let mut gen_vars = Vec::new();
        for fv in ty.ftv_ordered() {
            if !env_ftvs.contains(&fv) && !gen_vars.contains(&fv) {
                gen_vars.push(fv);
            }
        }

        let mut local_constraints = Vec::new();
        let mut outer_wanted = Vec::new();

        for r in residual {
            if let Type::Var(ref v) = r.ty {
                if gen_vars.contains(v) {
                    let sc = SchemeConstraint {
                        concept: r.concept,
                        type_arg: v.clone(),
                    };
                    if !local_constraints.contains(&sc) {
                        local_constraints.push(sc);
                    }
                } else if env_ftvs.contains(v) {
                    outer_wanted.push(r);
                } else {
                    // Ambiguous unquantified type variable (e.g. `eq(Nothing, Nothing)`)
                    return Err(TypeError::NoInstance {
                        concept: format!("{} (ambiguous type variable)", r.concept),
                        ty: r.ty,
                        span: r.span,
                    });
                }
            }
        }

        *wanted = outer_wanted;

        if gen_vars.is_empty() {
            Ok(Scheme::Mono(ty))
        } else if local_constraints.is_empty() {
            Ok(Scheme::Poly(gen_vars, ty))
        } else {
            Ok(Scheme::PolyConstrained(gen_vars, local_constraints, ty))
        }
    }

    fn infer<'a>(
        &mut self,
        e: &mut TypeEnv,
        form: &'a mut Form,
    ) -> Result<&'a Scheme, TypeError> {
        use self::Expr::*;

        let span = form.tag.pos;

        match form.node {
            Lit(ref lit) => {
                form.tag.ty = to_mono(lit.lit_type());
            }

            Var(ref n) => {
                if let Some(scm) = e.lookup(n).cloned() {
                    let inst_ty = self.instantiate(&scm, span);
                    form.tag.ty = to_mono(inst_ty);
                } else {
                    return Err(TypeError::NotInScope(
                        self.interner.trace(*n).to_owned(),
                        span,
                    ));
                }
            }

            QualifiedVar(ref path) => {
                let path_str = path.to_string_with(self.interner);
                let qualified_id = self.interner.intern(&path_str);

                if let Some(scm) = e.lookup(&qualified_id).cloned() {
                    let inst_ty = self.instantiate(&scm, span);
                    form.tag.ty = to_mono(inst_ty);
                } else if let Some(name) = path.name() {
                    if let Some(scm) = e.lookup(&name).cloned() {
                        let inst_ty = self.instantiate(&scm, span);
                        form.tag.ty = to_mono(inst_ty);
                    } else {
                        return Err(TypeError::NotInScope(path_str, span));
                    }
                } else {
                    return Err(TypeError::NotInScope("(empty path)".to_string(), span));
                }
            }

            Tuple(ref mut elems) => {
                let mut elem_tys = Vec::with_capacity(elems.len());
                for el in elems.iter_mut() {
                    elem_tys.push(self.infer(e, el.deref_mut())?.body().clone());
                }
                form.tag.set_type(Type::Tuple(elem_tys));
            }

            StructInit(ref ctor_name, ref mut field_inits) => {
                let adt_info = self
                    .find_constructor_adt(ctor_name)
                    .ok_or_else(|| TypeError::NotInScope(ctor_name.clone(), span))?;
                let variant = adt_info
                    .variants
                    .iter()
                    .find(|v| &v.name == ctor_name)
                    .unwrap();
                let decl_names = variant.field_names.as_ref().ok_or_else(|| {
                    TypeError::PatternError(
                        format!("Variant '{}' is not a struct variant", ctor_name),
                        span,
                    )
                })?;

                if field_inits.len() != decl_names.len() {
                    return Err(TypeError::PatternError(
                        format!(
                            "Struct variant '{}' expects {} field(s), got {}",
                            ctor_name,
                            decl_names.len(),
                            field_inits.len()
                        ),
                        span,
                    ));
                }

                let ctor_ty = self.instantiate(&variant.scheme, span);
                if let Type::Arr(expected_tys, ret_ty) = ctor_ty {
                    let mut seen = HashSet::new();
                    for (fname, fexpr) in field_inits.iter_mut() {
                        if !seen.insert(fname.clone()) {
                            return Err(TypeError::PatternError(
                                format!("Duplicate field '{}' in '{}'", fname, ctor_name),
                                span,
                            ));
                        }
                        let idx = decl_names.iter().position(|n| n == fname).ok_or_else(|| {
                            TypeError::PatternError(
                                format!("Variant '{}' has no field '{}'", ctor_name, fname),
                                span,
                            )
                        })?;
                        let val_ty = self.infer(e, fexpr.deref_mut())?.body().clone();
                        self.uni(&val_ty, &expected_tys[idx], fexpr.tag.pos);
                    }
                    form.tag.set_type(*ret_ty);
                } else {
                    return Err(TypeError::PatternError(
                        format!("Variant '{}' has no fields", ctor_name),
                        span,
                    ));
                }
            }

            Abs(ref mut fun) => {
                let mut extends = Vec::with_capacity(fun.param.len());
                let mut param_types = Vec::with_capacity(fun.param.len());

                for p in fun.param.iter_mut() {
                    match p.1 {
                        Scheme::Slot => p.1 = Scheme::Mono(self.fresh()),
                        ref existing => p.1 = self.expand_scheme(existing, span)?,
                    }
                    extends.push((p.0, p.1.clone()));
                    param_types.push(p.1.body().clone());
                }

                let mut new_env = e.extend_n(extends);
                let tbody = self.infer(&mut new_env, fun.body.deref_mut())?.body().clone();

                if let Some(ref mut ret_ann) = fun.ret_ty {
                    let expanded_ret = self.expand_type(ret_ann, span)?;
                    *ret_ann = expanded_ret.clone();
                    self.uni(&tbody, &expanded_ret, fun.body.tag.pos);
                }

                form.tag.ty = Scheme::arrow(param_types, tbody);
            }

            Apply(ref mut callee, ref mut args) => {
                let callee_ty = self.infer(e, callee.as_mut())?.body().clone();
                let mut ty_args = Vec::with_capacity(args.len());
                for arg in args.iter_mut() {
                    ty_args.push(self.infer(e, arg.as_mut())?.body().clone());
                }

                let ret_var = self.fresh();
                form.tag.set_type(ret_var.clone());

                let expected_fn_ty = Type::Arr(ty_args, P(ret_var));
                self.uni(&callee_ty, &expected_fn_ty, span);
            }

            Binary(op, ref mut left, ref mut right) => {
                let ty_left = self.infer(e, left.as_mut())?.body().clone();
                let ty_right = self.infer(e, right.as_mut())?.body().clone();

                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        let a = self.fresh();
                        self.wanted_constraints.push(WantedConstraint {
                            concept: "$Num".to_string(),
                            ty: a.clone(),
                            span,
                        });
                        self.uni(&ty_left, &a, left.tag.pos);
                        self.uni(&ty_right, &a, right.tag.pos);
                        form.tag.set_type(a);
                    }
                    BinOp::Rem => {
                        let int_ty = Type::Con("Int".to_string());
                        self.uni(&ty_left, &int_ty, left.tag.pos);
                        self.uni(&ty_right, &int_ty, right.tag.pos);
                        form.tag.set_type(int_ty);
                    }
                    BinOp::Eq | BinOp::Ne => {
                        let a = self.fresh();
                        self.wanted_constraints.push(WantedConstraint {
                            concept: "$EqOp".to_string(),
                            ty: a.clone(),
                            span,
                        });
                        self.uni(&ty_left, &a, left.tag.pos);
                        self.uni(&ty_right, &a, right.tag.pos);
                        form.tag.set_type(Type::Con("Bool".to_string()));
                    }
                    BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                        let a = self.fresh();
                        self.wanted_constraints.push(WantedConstraint {
                            concept: "$OrdOp".to_string(),
                            ty: a.clone(),
                            span,
                        });
                        self.uni(&ty_left, &a, left.tag.pos);
                        self.uni(&ty_right, &a, right.tag.pos);
                        form.tag.set_type(Type::Con("Bool".to_string()));
                    }
                    BinOp::And | BinOp::Or => {
                        let bool_ty = Type::Con("Bool".to_string());
                        self.uni(&ty_left, &bool_ty, left.tag.pos);
                        self.uni(&ty_right, &bool_ty, right.tag.pos);
                        form.tag.set_type(bool_ty);
                    }
                    _ => return Err(TypeError::UnknownOperator(op, span)),
                }
            }

            Unary(op, ref mut operand) => {
                let ty_op = self.infer(e, operand.as_mut())?.body().clone();
                match op {
                    UnOp::Not => {
                        let bool_ty = Type::Con("Bool".to_string());
                        self.uni(&ty_op, &bool_ty, operand.tag.pos);
                        form.tag.set_type(bool_ty);
                    }
                    UnOp::Neg => {
                        let a = self.fresh();
                        self.wanted_constraints.push(WantedConstraint {
                            concept: "$Num".to_string(),
                            ty: a.clone(),
                            span,
                        });
                        self.uni(&ty_op, &a, operand.tag.pos);
                        form.tag.set_type(a);
                    }
                }
            }

            Let(VarDecl(ref name, ref mut var_scm), ref mut val, ref mut body) => {
                let val_pos = val.tag.pos;
                let has_explicit_annot = *var_scm != Scheme::Slot;
                let placeholder_ty = if has_explicit_annot {
                    let expanded = self.expand_scheme(var_scm, span)?;
                    *var_scm = expanded.clone();
                    self.instantiate(&expanded, span)
                } else {
                    let fv = self.fresh();
                    *var_scm = to_mono(fv.clone());
                    fv
                };

                if matches!(val.node, Expr::Abs(..)) {
                    // Save outer wanted constraints so we only generalize constraints originating in `val`
                    let saved_wanted = mem::take(&mut self.wanted_constraints);

                    let mut rec_env = e.extend(*name, to_mono(placeholder_ty.clone()));
                    let inferred_val_ty = self.infer(&mut rec_env, val.as_mut())?.body().clone();
                    self.uni(&placeholder_ty, &inferred_val_ty, val_pos);

                    // Solve current constraints to enable HM let-generalization for local lambdas
                    let sub = self.solve()?;
                    Self::apply_subst_to_env(e, &sub);
                    val.apply_mut(&sub);
                    var_scm.apply_mut(&sub);
                    for wc in &mut self.wanted_constraints {
                        wc.ty = wc.ty.clone().apply(&sub);
                    }
                    let mut outer_wanted: Vec<WantedConstraint> = saved_wanted
                        .into_iter()
                        .map(|mut w| {
                            w.ty = w.ty.apply(&sub);
                            w
                        })
                        .collect();

                    let resolved_val_ty = placeholder_ty.apply(&sub);
                    let env_ftvs = Self::env_ftv_with_subst(e, &sub);
                    let mut val_wanted = mem::take(&mut self.wanted_constraints);
                    let generalized_scm = self.generalize_type(
                        &env_ftvs,
                        resolved_val_ty,
                        &mut val_wanted,
                        &[],
                    )?;
                    outer_wanted.append(&mut val_wanted);
                    self.wanted_constraints = outer_wanted;

                    *var_scm = generalized_scm.clone();
                    val.tag.set_scheme(generalized_scm.clone());

                    let mut body_env = e.extend(*name, generalized_scm);
                    let ty_body = self.infer(&mut body_env, body.as_mut())?.clone();
                    form.tag.ty = ty_body;
                } else {
                    // Value restriction: non-function local bindings stay monomorphic so later uses refine `val`'s type
                    let inferred_val_ty = self.infer(e, val.as_mut())?.body().clone();
                    self.uni(&placeholder_ty, &inferred_val_ty, val_pos);
                    let sub = self.solve()?;
                    Self::apply_subst_to_env(e, &sub);
                    val.apply_mut(&sub);
                    let mono_ty = placeholder_ty.apply(&sub);
                    *var_scm = to_mono(mono_ty.clone());

                    let mut body_env = e.extend(*name, to_mono(mono_ty));
                    let ty_body = self.infer(&mut body_env, body.as_mut())?.clone();
                    form.tag.ty = ty_body;
                }
            }

            If(ref mut cond, ref mut tr, ref mut fl) => {
                let cond_pos = cond.tag.pos;
                let tr_pos = tr.tag.pos;
                let tycond = self.infer(e, cond.as_mut())?.body().clone();
                let tytr = self.infer(e, tr.as_mut())?.body().clone();
                let tyfl = self.infer(e, fl.as_mut())?.body().clone();

                self.uni(&tycond, &Type::Con("Bool".to_string()), cond_pos);
                self.uni(&tytr, &tyfl, tr_pos);
                form.tag.set_type(tytr);
            }

            Block(ref mut exps) => {
                let mut last_ty = Type::Void;
                for f in exps.iter_mut() {
                    last_ty = self.infer(e, f.deref_mut())?.body().clone();
                }
                form.tag.set_type(last_ty);
            }

            List(ref mut exps) => {
                let tyitem = self.fresh();
                for f in exps.iter_mut() {
                    let fpos = f.tag.pos;
                    let ty = self.infer(e, f.as_mut())?.body().clone();
                    self.uni(&ty, &tyitem, fpos);
                }
                form.tag.set_type(Type::compose(
                    Type::Con("List".to_string()),
                    tyitem,
                ));
            }

            Match(ref mut scrutinee, ref mut arms) => {
                let scrutinee_pos = scrutinee.tag.pos;
                let scrutinee_type = self.infer(e, scrutinee.as_mut())?.body().clone();
                let result_type = self.fresh();

                for arm in arms.iter_mut() {
                    let arm_pos = arm.body.tag.pos;
                    let (pattern_type, bindings) = self.infer_pattern(&arm.pattern, arm_pos)?;
                    self.uni(&pattern_type, &scrutinee_type, scrutinee_pos);

                    let mut arm_env = e.extend_n(bindings);
                    if let Some(ref mut guard) = arm.guard {
                        let gpos = guard.tag.pos;
                        let guard_ty = self.infer(&mut arm_env, guard.as_mut())?.body().clone();
                        self.uni(&guard_ty, &Type::Con("Bool".to_string()), gpos);
                    }

                    let body_ty = self.infer(&mut arm_env, arm.body.as_mut())?.body().clone();
                    self.uni(&body_ty, &result_type, arm_pos);
                }

                form.tag.set_type(result_type);
            }
        }

        if let Some(ref annot) = form.tag.annotate.clone() {
            let expanded = self.expand_scheme(annot, span)?;
            let anno_inst = self.instantiate(&expanded, span);
            self.uni(&anno_inst, form.tag.ref_type(), span);
        }

        Ok(form.tag.ref_scheme())
    }

    pub fn infer_defs<'a>(
        &mut self,
        base_env: &'a TypeEnv<'a>,
        program: &'a mut Vec<Def>,
    ) -> Result<(), TypeError> {
        self.infer_defs_with_imports(base_env, program, &[])
    }

    pub fn infer_defs_with_imports<'a>(
        &mut self,
        base_env: &'a TypeEnv<'a>,
        program: &'a mut Vec<Def>,
        import_aliases: &[(Id, Id)],
    ) -> Result<(), TypeError> {
        // Pass 1: Register all type aliases
        for d in program.iter() {
            if let Item::Alias(ref params, ref target_scm) = d.node {
                let alias_name = self.interner.trace(d.ident).to_string();
                let param_names: Vec<Name> = params
                    .iter()
                    .map(|id| self.interner.trace(*id).to_string())
                    .collect();
                self.alias_registry
                    .insert(alias_name, (param_names, target_scm.body().clone()));
            }
        }
        // Validate all aliases are non-cyclic
        for (alias_name, (params, _)) in self.alias_registry.clone() {
            let check_ty = if params.is_empty() {
                Type::Con(alias_name)
            } else {
                Type::compose_n(
                    std::iter::once(Type::Con(alias_name))
                        .chain(params.into_iter().map(Type::Var)),
                )
            };
            self.expand_type(&check_ty, Span::default())?;
        }

        // Pass 2: Process ADT definitions (plus default `List` constructors)
        let mut constructor_types: Vec<(Id, Scheme)> = Vec::new();
        // Add default List constructors first (user `data List` will override them)
        if let Some(default_list) = self.adt_registry.get("List").cloned() {
            for v in default_list.variants {
                let id = self.interner.intern(&v.name);
                constructor_types.push((id, v.scheme));
            }
        }
        for d in program.iter() {
            if let Item::Alg(ref type_params, ref variants) = d.node {
                let adt_name = self.interner.trace(d.ident).to_string();
                let ctors = self.process_adt(&adt_name, type_params, variants, d.pos)?;
                constructor_types.extend(ctors);
            }
        }

        // Pass 3: Process Concept definitions
        let mut concept_method_entries: Vec<(Id, Scheme)> = Vec::new();
        for d in program.iter() {
            if let Item::Concept {
                ref type_params,
                ref superclasses,
                ref methods,
            } = d.node
            {
                let cname = self.interner.trace(d.ident).to_string();
                let entries =
                    self.process_concept(&cname, type_params, superclasses, methods, d.pos)?;
                concept_method_entries.extend(entries);
            }
        }

        // Pass 4: Process Instance definitions and append synthesized method `Def`s to `program`
        let mut synth_instance_defs = Vec::new();
        for d in program.iter() {
            if let Item::Instance {
                ref concept_name,
                ref type_args,
                ref constraints,
                ref methods,
            } = d.node
            {
                let mut defs = self.process_instance(
                    concept_name,
                    type_args,
                    constraints,
                    methods,
                    d.pos,
                )?;
                synth_instance_defs.append(&mut defs);
            }
        }
        program.extend(synth_instance_defs);

        // Pass 5: Seed initial type environment for all top-level value/function definitions
        let mut def_placeholders: HashMap<Id, Type> = HashMap::new();
        let mut initial_bindings: Vec<(Id, Scheme)> = Vec::new();

        for (ctor_id, ctor_scm) in constructor_types {
            initial_bindings.push((ctor_id, ctor_scm));
        }
        for (m_id, m_scm) in concept_method_entries {
            initial_bindings.push((m_id, m_scm));
        }

        for d in program.iter() {
            if let Item::Form(ref f) = d.node {
                if !matches!(f.node, Expr::Abs(..)) {
                    self.global_values.insert(d.ident);
                }
                if let Some(annot) = d.form_annot() {
                    let expanded = self.expand_scheme(annot, d.pos)?;
                    initial_bindings.push((d.name(), expanded));
                } else {
                    let fv = self.fresh();
                    def_placeholders.insert(d.name(), fv.clone());
                    initial_bindings.push((d.name(), to_mono(fv)));
                }
            }
        }

        // Also seed import aliases pointing to placeholders/annotations
        let initial_map: HashMap<Id, Scheme> = initial_bindings.iter().cloned().collect();
        for (local_id, qualified_id) in import_aliases {
            if let Some(scm) = initial_map.get(qualified_id) {
                initial_bindings.push((*local_id, scm.clone()));
            }
        }

        let mut env = base_env.extend_n(initial_bindings);
        let mut inferred_indices = Vec::new();

        // Pass 6: Infer each top-level `Form` definition
        for (idx, d) in program.iter_mut().enumerate() {
            if !d.is_form() {
                continue;
            }
            inferred_indices.push(idx);
            let def_pos = d.pos;
            let explicit_annot = match d.form_annot().cloned() {
                Some(a) => Some(self.expand_scheme(&a, def_pos)?),
                None => None,
            };

            // If explicitly annotated with `Poly` or `PolyConstrained`, skolemize/keep given constraints
            let given_constraints: Vec<(Name, Name)> = explicit_annot
                .as_ref()
                .map(|scm| {
                    scm.constraints()
                        .iter()
                        .map(|c| (c.concept.clone(), c.type_arg.clone()))
                        .collect()
                })
                .unwrap_or_default();

            // Temporarily clear `form.tag.annotate` if polymorphic so `infer()` doesn't instantiate it with fresh vars
            let saved_annot = d.form_body_mut().tag.annotate.take();
            self.subst.clear();
            self.wanted_constraints.clear();

            self.infer(&mut env, d.form_body_mut())?;

            let body_ty = d.form_body_mut().tag.ref_type().clone();
            if let Some(ref ph) = def_placeholders.get(&d.name()) {
                let current_ph = env
                    .lookup(&d.name())
                    .map(|s| s.body().clone())
                    .unwrap_or_else(|| (*ph).clone());
                self.uni(&current_ph, &body_ty, def_pos);
            }

            if let Some(ref exp_scm) = explicit_annot {
                self.uni(exp_scm.body(), &body_ty, def_pos);
            }

            let sub = self.solve()?;
            Self::apply_subst_to_env(&mut env, &sub);
            d.form_body_mut().apply_mut(&sub);
            for wc in &mut self.wanted_constraints {
                wc.ty = wc.ty.clone().apply(&sub);
            }

            // Remove self from `env_ftvs` before generalizing so top-level recursion generalizes properly!
            let mut env_ftvs = HashSet::new();
            for (other_id, other_scm) in env.local_entries() {
                if *other_id != d.name()
                    && !import_aliases.iter().any(|(l, q)| *l == *other_id && *q == d.name())
                {
                    env_ftvs.extend(other_scm.ftv());
                }
            }
            if let Some(p) = env.parent_env() {
                Self::collect_env_ftv_with_subst(p, &sub, &mut env_ftvs);
            }

            let mut def_wanted = mem::take(&mut self.wanted_constraints);
            let final_ty = d.form_type().body().clone();
            let general = if let Some(exp_scm) = explicit_annot {
                // Verify all wanted constraints are satisfied under `given_constraints`
                let mut residual = Vec::new();
                for wc in def_wanted.drain(..) {
                    self.discharge_constraint(&wc, &given_constraints, &mut residual, 0)?;
                }
                if let Some(unmet) = residual.first() {
                    return Err(TypeError::NoInstance {
                        concept: unmet.concept.clone(),
                        ty: unmet.ty.clone(),
                        span: unmet.span,
                    });
                }
                // Apply substitution to non-quantified vars if any, preserving explicit scheme
                exp_scm.apply(&sub)
            } else {
                // Value restriction: only generalize functions (`Expr::Abs`), keep top-level value defs monomorphic unless annotated
                if matches!(d.form_body_mut().node, Expr::Abs(..)) {
                    self.generalize_type(
                        &env_ftvs,
                        final_ty,
                        &mut def_wanted,
                        &given_constraints,
                    )?
                } else {
                    let mut residual = Vec::new();
                    for wc in def_wanted.drain(..) {
                        self.discharge_constraint(&wc, &given_constraints, &mut residual, 0)?;
                    }
                    if let Some(unmet) = residual.first() {
                        return Err(TypeError::NoInstance {
                            concept: unmet.concept.clone(),
                            ty: unmet.ty.clone(),
                            span: unmet.span,
                        });
                    }
                    Scheme::Mono(final_ty)
                }
            };

            d.form_body_mut().tag.annotate = saved_annot;
            d.form_body_mut().tag.set_scheme(general.clone());

            let fn_name = self.interner.trace(d.name()).to_string();
            self.instantiation_registry
                .register_polymorphic(fn_name, general.clone());

            env.insert(d.name(), general.clone());
            for (local_id, qualified_id) in import_aliases {
                if *qualified_id == d.name() {
                    env.insert(*local_id, general.clone());
                    let alias_name = self.interner.trace(*local_id).to_string();
                    self.instantiation_registry
                        .register_polymorphic(alias_name, general.clone());
                }
            }
        }

        // Final pass: apply final environment substitutions across all earlier definitions
        // (for mutual recursion where a later def refined an earlier def's placeholder)
        // and check pattern exhaustiveness on every inferred definition!
        for idx in inferred_indices {
            let d = &mut program[idx];
            self.check_exhaustiveness_in_form(d.form_body_mut())?;
        }

        Ok(())
    }

    fn solve(&mut self) -> Result<Subst, TypeError> {
        let mut constraints = mem::take(&mut self.constraints);
        // Apply any previously accumulated substitutions first
        if !self.subst.is_empty() {
            for c in constraints.iter_mut() {
                c.apply_mut(&self.subst);
            }
        }
        while let Some(cons) = constraints.pop_front() {
            let new_sub = cons.unify()?;
            if !new_sub.is_empty() {
                for c in constraints.iter_mut() {
                    c.apply_mut(&new_sub);
                }
                self.subst = self
                    .subst
                    .drain()
                    .map(|(k, v)| (k, v.apply(&new_sub)))
                    .collect();
                self.subst.extend(new_sub);
            }
        }
        Ok(self.subst.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::parser;
    use crate::typeinfer::*;
    use crate::types::*;
    use crate::utils::*;

    #[test]
    fn test_occurs_check_rejects_self_application() {
        let mut interner = Interner::new();
        let mut program = parser::parse("def omega(x) = x(x)", &mut interner).unwrap();
        let env = Infer::new_env();
        let mut inf = Infer::new(&mut interner);
        let res = inf.infer_defs(&env, &mut program);
        assert!(matches!(res, Err(TypeError::InfiniteType(..))));
    }

    #[test]
    fn test_polymorphic_binary_ops_in_same_function() {
        let mut interner = Interner::new();
        let src = "
def both() = {
    let a = 1 + 2;
    let b = 1.5 + 2.5;
    if (a == 3 && b == 4.0 && \"hi\" == \"hi\") 1 else 0
}
";
        let mut program = parser::parse(src, &mut interner).unwrap();
        let env = Infer::new_env();
        let mut inf = Infer::new(&mut interner);
        assert!(inf.infer_defs(&env, &mut program).is_ok());
    }

    #[test]
    fn test_string_addition_rejected_by_num_concept() {
        let mut interner = Interner::new();
        let mut program = parser::parse("def bad() = \"a\" + \"b\"", &mut interner).unwrap();
        let env = Infer::new_env();
        let mut inf = Infer::new(&mut interner);
        let res = inf.infer_defs(&env, &mut program);
        assert!(matches!(res, Err(TypeError::NoInstance { .. })));
    }

    #[test]
    fn test_local_let_polymorphism() {
        let mut interner = Interner::new();
        let src = "
def test() =
    let id = (x) -> x in
    if (id(true)) id(42) else 0
";
        let mut program = parser::parse(src, &mut interner).unwrap();
        let env = Infer::new_env();
        let mut inf = Infer::new(&mut interner);
        assert!(inf.infer_defs(&env, &mut program).is_ok());
    }

    #[test]
    fn test_non_exhaustive_match_rejected() {
        let mut interner = Interner::new();
        let src = "
data Maybe a { Nothing, Just(a) }
def bad(m) = match m { Just(x) -> x }
";
        let mut program = parser::parse(src, &mut interner).unwrap();
        let env = Infer::new_env();
        let mut inf = Infer::new(&mut interner);
        let res = inf.infer_defs(&env, &mut program);
        assert!(matches!(res, Err(TypeError::NonExhaustiveMatch(..))));
    }
}
