use crate::syntax::form::*;
use crate::internal::*;
use crate::types;
use crate::types::*;

use crate::utils::*;

use std::collections::LinkedList;
use std::collections::HashMap;
use std::collections::HashSet;

use std::iter::IntoIterator;
use std::iter::DoubleEndedIterator;

use std::ops::DerefMut;

use std::mem;

use super::subst::*;
use super::constraint::{ Constraint };
pub use super::error::{ TypeError };
use super::mono::{InstantiationRegistry, Instantiation, CallSite};

#[derive(Debug)]
pub struct Infer<'interner> {
    unique: usize,
    interner: &'interner mut Interner,
    constraints: LinkedList<Constraint>,
    /// Registry of ADT definitions for pattern matching
    pub adt_registry: types::AdtRegistry,
    /// Registry for tracking polymorphic function instantiations
    pub instantiation_registry: InstantiationRegistry,
    /// Active call sites - maps fresh type variables to their call site info
    call_sites: Vec<CallSite>,
}


//fn generalize(e: &TypeEnv, ty: Type) -> Scheme {
//}

fn to_mono(ty: Type) -> Scheme {
    Scheme::Mono(ty)
}

/// Check if a type is fully concrete (contains no type variables)
fn is_concrete_type(ty: &Type) -> bool {
    match ty {
        Type::Var(_) => false,
        Type::Con(_) | Type::Void => true,
        Type::Arr(p, r) => is_concrete_type(p) && is_concrete_type(r),
        Type::Prod(l, r) => is_concrete_type(l) && is_concrete_type(r),
        Type::Comp(base, arg) => is_concrete_type(base) && is_concrete_type(arg),
    }
}


impl<'i> Infer<'i> {
    pub fn new(interner: &'i mut Interner) -> Infer<'i> {
        Infer {
            unique: 0,
            constraints: LinkedList::new(),
            interner,
            adt_registry: HashMap::new(),
            instantiation_registry: InstantiationRegistry::new(),
            call_sites: Vec::new(),
        }
    }

    pub fn new_env<'a>() -> TypeEnv<'a> {
        TypeEnv::new()
    }

    /// Get a unique id, then increase the counter
    fn unique(&mut self) -> usize {
        self.unique = self.unique + 1;
        self.unique
    }

    /// Generate a new temp type variable name
    /// All generated names start with a dot
    fn fresh(&mut self) -> Type {
        let mut f = String::from(".");
        f.push_str(self.unique().to_string().as_str());
        Type::Var(f)
    }

    /// Add a constraint
    fn uni(&mut self, left: (&Type, Span), right: (&Type, Span)) {
        self.constraints.push_back(Constraint(left.0.clone(), right.0.clone()));
    }

    /// Temporary instantiate a polymorphism type
    fn instantiate(&mut self, scm: &Scheme) -> Type {
        self.instantiate_with_tracking(scm, None)
    }

    /// Instantiate a polymorphism type with optional tracking for monomorphization
    fn instantiate_with_tracking(&mut self, scm: &Scheme, fn_name: Option<Name>) -> Type {
        use self::Scheme::*;
        match *scm {
            Mono(ref ty) => ty.clone(),
            Poly(ref tvs, ref ty) => {
                let tvs_: Vec<_> = tvs.iter().map(|_| self.fresh()).collect();
                let sub: Subst = tvs.clone().into_iter().zip(tvs_.clone()).collect();

                // Track this instantiation for monomorphization
                if let Some(fn_name) = fn_name {
                    let fresh_var_names: Vec<Name> = tvs_.iter()
                        .filter_map(|t| if let Type::Var(n) = t { Some(n.clone()) } else { None })
                        .collect();

                    // Record context for each fresh variable
                    for (i, var_name) in fresh_var_names.iter().enumerate() {
                        self.instantiation_registry.record_context(var_name.clone(), fn_name.clone(), i);
                    }

                    // Record the call site
                    self.call_sites.push(CallSite::new(fn_name, fresh_var_names));
                }

                ty.clone().apply(&sub)
            }
            PolyConstrained(ref tvs, ref _constraints, ref ty) => {
                // For now, treat constrained poly the same as unconstrained
                // In the future, we'd also generate "wanted" constraints here
                let tvs_: Vec<_> = tvs.iter().map(|_| self.fresh()).collect();
                let sub: Subst = tvs.clone().into_iter().zip(tvs_.clone()).collect();

                // Track this instantiation for monomorphization
                if let Some(fn_name) = fn_name {
                    let fresh_var_names: Vec<Name> = tvs_.iter()
                        .filter_map(|t| if let Type::Var(n) = t { Some(n.clone()) } else { None })
                        .collect();

                    // Record context for each fresh variable
                    for (i, var_name) in fresh_var_names.iter().enumerate() {
                        self.instantiation_registry.record_context(var_name.clone(), fn_name.clone(), i);
                    }

                    // Record the call site
                    self.call_sites.push(CallSite::new(fn_name, fresh_var_names));
                }

                ty.clone().apply(&sub)
            }
            Slot => unreachable!(),
        }
    }

    /// Collect concrete instantiations from the substitution after solve()
    fn collect_instantiations(&mut self, sub: &Subst) {
        // Process each call site to determine concrete type arguments
        for call_site in &self.call_sites {
            let mut type_args = Vec::new();
            let mut all_concrete = true;

            for fresh_var in &call_site.fresh_vars {
                if let Some(concrete_type) = sub.get(fresh_var) {
                    // Check if the type is fully concrete (no type variables)
                    if is_concrete_type(concrete_type) {
                        type_args.push(concrete_type.clone());
                    } else {
                        all_concrete = false;
                        break;
                    }
                } else {
                    // Type variable wasn't resolved - use a default or skip
                    all_concrete = false;
                    break;
                }
            }

            // Only add instantiation if all type arguments are concrete
            if all_concrete && !type_args.is_empty() {
                let inst = Instantiation::new(call_site.function.clone(), type_args.clone());
                self.instantiation_registry.add_instantiation(inst);
            } else if !type_args.is_empty() {
            }
        }
    }

    /// Process an ADT definition and register its constructors
    /// Returns a list of (constructor_name, constructor_type) pairs to add to the environment
    fn process_adt(&mut self, adt_name: &str, type_params: &[Id], variants: &[Variant]) -> Vec<(Id, Scheme)> {
        let type_param_names: Vec<Name> = type_params.iter()
            .map(|id| self.interner.trace(*id).to_string())
            .collect();

        // Build the result type: e.g., List a for data List a { ... }
        let result_type = if type_param_names.is_empty() {
            Type::Con(adt_name.to_string())
        } else {
            let base = Type::Con(adt_name.to_string());
            let params: Vec<Type> = type_param_names.iter()
                .map(|p| Type::Var(p.clone()))
                .collect();
            Type::compose_n(std::iter::once(base).chain(params))
        };

        let mut constructors = Vec::new();
        let mut variant_infos = Vec::new();

        for (tag, variant) in variants.iter().enumerate() {
            let field_types: Vec<Type> = match &variant.body {
                VariantBody::Unit => vec![],
                VariantBody::Tuple(fields) => {
                    fields.iter().map(|f| (*f.ty).clone()).collect()
                }
                VariantBody::Struct(fields) => {
                    fields.iter().map(|f| (*f.ty).clone()).collect()
                }
            };

            // Build constructor type
            // Unit variant: forall a. List a
            // Tuple variant Cons(a, List a): forall a. a * List a -> List a
            let constructor_type = if field_types.is_empty() {
                result_type.clone()
            } else {
                let param_type = if field_types.len() == 1 {
                    field_types[0].clone()
                } else {
                    Type::product_n(field_types.clone())
                };
                Type::Arr(P(param_type), P(result_type.clone()))
            };

            // Wrap in Poly if there are type parameters
            let scheme = if type_param_names.is_empty() {
                Scheme::Mono(constructor_type.clone())
            } else {
                Scheme::Poly(type_param_names.clone(), constructor_type.clone())
            };

            // Intern the constructor name and add to results
            let constructor_id = self.interner.intern(&variant.name);
            constructors.push((constructor_id, scheme.clone()));

            variant_infos.push(types::VariantInfo {
                name: variant.name.clone(),
                tag,
                field_types,
                scheme,
            });
        }

        // Register the ADT in our registry
        self.adt_registry.insert(adt_name.to_string(), types::AdtInfo {
            name: adt_name.to_string(),
            type_params: type_param_names,
            variants: variant_infos,
        });

        constructors
    }

    /// Infer the type of a pattern and return bindings introduced
    /// Returns (pattern_type, Vec<(binding_name, binding_type)>)
    fn infer_pattern(&mut self, pattern: &Pattern, env: &TypeEnv) -> Result<(Type, Vec<(Id, Scheme)>), TypeError> {
        match pattern {
            Pattern::Var(id) => {
                // Variable pattern: can match anything, introduces a binding
                let fresh_ty = self.fresh();
                Ok((fresh_ty.clone(), vec![(*id, Scheme::Mono(fresh_ty))]))
            }

            Pattern::Wildcard => {
                // Wildcard: matches anything, no bindings
                let fresh_ty = self.fresh();
                Ok((fresh_ty, vec![]))
            }

            Pattern::Lit(lit) => {
                // Literal pattern: type is the literal's type, no bindings
                let ty = lit.lit_type();
                Ok((ty, vec![]))
            }

            Pattern::Constructor(name, sub_patterns) => {
                // Look up constructor in ADT registry
                if let Some(adt_info) = self.find_constructor_adt(name) {
                    // Find the variant
                    let variant = adt_info.variants.iter()
                        .find(|v| &v.name == name)
                        .expect("Constructor not found in ADT");

                    // Instantiate the constructor type
                    let ctor_type = self.instantiate(&variant.scheme);

                    // Collect bindings from sub-patterns
                    let mut all_bindings = vec![];

                    if sub_patterns.is_empty() {
                        // Unit constructor: result type is the constructor type itself
                        Ok((ctor_type, all_bindings))
                    } else {
                        // Extract parameter types and result type from arrow type
                        if let Type::Arr(param_ty, result_ty) = &ctor_type {
                            // Get individual field types
                            let field_types: Vec<&Type> = param_ty.prod_to_vec();

                            if field_types.len() != sub_patterns.len() {
                                return Err(TypeError::PatternError(
                                    format!("Constructor {} expects {} arguments, got {}",
                                            name, field_types.len(), sub_patterns.len()),
                                    Span::new(0, 0)
                                ));
                            }

                            // Check each sub-pattern
                            for (sub_pat, expected_ty) in sub_patterns.iter().zip(field_types.iter()) {
                                let (pat_ty, bindings) = self.infer_pattern(sub_pat, env)?;
                                // Unify sub-pattern type with expected field type
                                self.uni((&pat_ty, Span::new(0, 0)), (expected_ty, Span::new(0, 0)));
                                all_bindings.extend(bindings);
                            }

                            Ok(((**result_ty).clone(), all_bindings))
                        } else {
                            // Non-arrow type means unit constructor was given patterns
                            Err(TypeError::PatternError(
                                format!("Constructor {} takes no arguments", name),
                                Span::new(0, 0)
                            ))
                        }
                    }
                } else {
                    // Constructor not found - report error
                    Err(TypeError::NotInScope(name.clone(), Span::new(0, 0)))
                }
            }
        }
    }

    /// Find which ADT a constructor belongs to
    fn find_constructor_adt(&self, ctor_name: &str) -> Option<types::AdtInfo> {
        for (_, adt_info) in &self.adt_registry {
            for variant in &adt_info.variants {
                if variant.name == ctor_name {
                    return Some(adt_info.clone());
                }
            }
        }
        None
    }

    /// The main inference algorithm
    /// Infering type and tagging form
    fn infer<'a>(&mut self,
                     e: &mut TypeEnv,
                     form: &'a mut Form)
                     -> Result<&'a Scheme, TypeError> {
        use self::Expr::*;
        use self::Scheme::*;
        use self::TypeError::*;

        match form.node {
            // A Literal type is certain
            Lit(ref mut lit) => {
                let ty = lit.lit_type();
                form.tag.ty = to_mono(ty);
            }

            // Find the type of a variable from environment
            Var(ref n) => {
                if let Some(ty) = e.lookup(n) {
                    // Rust 的 lifetime 设计有点厉害
                    // 一开始这里忘了把 type 赋值给 form，直接返回出去
                    // 然后因为这个 ty 的生命周期和 env 相同
                    // 然而 env 的生命周期比 'a 短
                    // 所以报错
                    form.tag.ty = (*ty).clone();
                    // form.set_scheme();
                } else {
                    return Err(NotInScope(self.interner.trace(*n).to_owned(), form.tag.pos));
                }
            }

            // Abstraction (function) should have a arrow type.
            // Generate temporary type var
            //   if parameter has no type annotation.
            // Extend the environment with parameters type,
            //   then infer function body.
            Abs(ref mut fun) => {
                let mut extends = vec![];
                let mut types: Vec<Type> = vec![];

                for p in fun.param.iter_mut() {
                    match p.1 {
                        Slot => p.1 = Scheme::Mono(self.fresh()),
                        _ => {}
                    }
                    extends.push((p.0.to_owned(), p.1.clone()));
                    types.push(p.1.body().clone());
                }
                let typaram = if types.is_empty() {
                    Type::Void
                } else {
                    Type::product_n(types)
                };
                let mut new_env = e.extend_n(extends);
                let tbody = self.infer(&mut new_env, fun.body.deref_mut())?;
                form.tag.ty = Scheme::arrow(typaram, tbody.body().clone());
            }

            // A function apply.
            // Types of arguments should be able to unify with callee.
            // Infer the returned type.
            Apply(ref mut callee, ref mut args) => {
                let callee = callee.as_mut();
                let callee_pos = callee.tag.pos;

                // Get the function name if this is a direct function call
                let fn_name = match &callee.node {
                    Expr::Var(id) => Some(self.interner.trace(*id).to_string()),
                    _ => None,
                };

                let ty_callee = self.infer(e, callee)?;

                // Use tracking instantiation if we have a function name
                let callee_inst = self.instantiate_with_tracking(ty_callee, fn_name);

                let mut ty_args: Vec<Type> = vec![];

                for arg in args.iter_mut() {
                    ty_args.push(self.infer(e, arg)?
                                     .body()
                                     .clone());
                }

                form.tag.set_type(self.fresh());

                let tyfun = Scheme::arrow(Type::product_n(ty_args), form.tag.clone_type());

                self.uni((&callee_inst, callee_pos), (tyfun.body(), form.tag.pos));
            }

            // Type of binary ops should exist in environment.
            Binary(ref op, ref mut left, ref mut right) => {
                let ty_left = self.infer(e, left)?.body();
                let ty_right = self.infer(e, right)?.body();
                if let Some(ty_op) = e.lookup(&self.interner.intern(&op.as_str())) {
                    let ty_lr = Type::product(ty_left.clone(), ty_right.clone());

                    form.tag.ty = to_mono(self.fresh());

                    let ty_fun = Scheme::arrow(ty_lr, form.tag.clone_type());
                    self.uni((ty_op.body(), form.tag.pos), (ty_fun.body(), form.tag.pos));
                } else {
                    return Err(TypeError::UnknownOperator(op.clone(), form.tag.pos.clone()));
                }
            }

            // Let bound
            Let(VarDecl(ref name, ref mut ty), ref mut val, ref mut body) => {
                let val = val.as_mut();
                let body = body.as_mut();
                let val_pos = val.tag.pos;
                if *ty == Scheme::Slot {
                    *ty = to_mono(self.fresh());
                }
                let tyval =
                    if let Abs(..) = val.node {
                        let old = e.insert(name.to_owned(), ty.to_owned());
                        let ret = self.infer(e, val)?;
                        if let Some(t) = old {
                            e.insert(name.to_owned(), t);
                        }
                        ret
                    } else {
                        self.infer(e, val)?
                    };
                self.uni((ty.body(), body.tag.pos), (tyval.body(), val_pos));

                let old = e.insert(name.to_owned(), tyval.to_owned());
                let tyexp = self.infer(e, body)?;
                if let Some(t) = old {
                    e.insert(name.to_owned(), t);
                }
                form.tag.ty = tyexp.clone();
            }

            // If expression
            // Condition should be `Bool`,
            //   all branches should be unified.
            If(ref mut cond, ref mut tr, ref mut fl) => {
                let cond = cond.as_mut();
                let tr = tr.as_mut();
                let fl = fl.as_mut();
                let cond_pos = cond.tag.pos;
                let tr_pos = tr.tag.pos;
                let fl_pos = fl.tag.pos;
                let tycond = self.infer(e, cond)?;
                let tytr = self.infer(e, tr)?;
                let tyfl = self.infer(e, fl)?;

                self.uni((tycond.body(), cond_pos), (&Type::Con("Bool".to_string()), cond_pos));
                self.uni((tytr.body(), tr_pos), (tyfl.body(), fl_pos));
                form.tag.set_scheme(tytr.clone());
            }

            // Type of a block is the type of last expr
            Block(ref mut exps) => {
                let mut ty = &Scheme::Mono(Type::Void);
                for f in exps.iter_mut() {
                    ty = self.infer(e, f.deref_mut())?;
                }

                form.tag.set_scheme(ty.clone());
            }

            // List should be mono
            List(ref mut exps) => {
                let tyitem: Type = self.fresh();
                for f in exps.iter_mut() {
                    let pos = f.tag.pos;
                    let ty = self.infer(e, f)?;
                    self.uni((ty.body(), form.tag.pos), (&tyitem, pos));
                }

                form.tag.set_scheme(to_mono(
                    Type::compose(
                        Type::Con("List".to_string()),
                        tyitem)));
            },

            // Pattern matching expression
            Match(ref mut scrutinee, ref mut arms) => {
                let scrutinee = scrutinee.as_mut();
                let scrutinee_pos = scrutinee.tag.pos;
                let ty_scrutinee = self.infer(e, scrutinee)?;
                let scrutinee_type = ty_scrutinee.body().clone();

                // Result type will be unified across all arms
                let result_type = self.fresh();

                for arm in arms.iter_mut() {
                    // Type check the pattern and get bindings
                    let (pattern_type, bindings) = self.infer_pattern(&arm.pattern, e)?;

                    // Unify pattern type with scrutinee type
                    self.uni((&pattern_type, form.tag.pos), (&scrutinee_type, scrutinee_pos));

                    // Extend environment with pattern bindings
                    let mut arm_env = e.extend_n(bindings);

                    // Type check optional guard
                    if let Some(ref mut guard) = arm.guard {
                        let guard_ty = self.infer(&mut arm_env, guard.as_mut())?;
                        self.uni((guard_ty.body(), form.tag.pos), (&Type::Con("Bool".to_string()), form.tag.pos));
                    }

                    // Type check arm body
                    let body_pos = arm.body.tag.pos;
                    let body_ty = self.infer(&mut arm_env, arm.body.as_mut())?;

                    // Unify body type with result type
                    self.uni((body_ty.body(), body_pos), (&result_type, form.tag.pos));
                }

                form.tag.set_type(result_type);
            },

            _ => unimplemented!(),
        }

        // If there is a type annotation, unify it with inferred type
        if let Some(ref annot) = form.tag.annotate {
            let anno_inst = self.instantiate(annot);
            self.uni((&anno_inst, form.tag.pos), (form.tag.ref_type(), form.tag.pos));
        }

        Ok(form.tag.ref_scheme())

    }

    /// Do type inference over top level definitions
    pub fn infer_defs<'a>(&mut self,
                      _env: &'a TypeEnv<'a>,
                      program: &'a mut Vec<Def>)
                      -> Result<(), TypeError> {

        // First pass: process ADT definitions and collect constructor types
        let mut constructor_types: Vec<(Id, Scheme)> = Vec::new();
        for d in program.iter() {
            if let Item::Alg(ref type_params, ref variants) = d.node {
                let adt_name = self.interner.trace(d.ident).to_string();
                let ctors = self.process_adt(&adt_name, type_params, variants);
                constructor_types.extend(ctors);
            }
        }

        // Give each definitions a temporary type if no annotation
        let mut env = {
            let name_scms = program
                .iter()
                .filter(|ref v| v.is_form())
                .map(|ref d| (d.name(), match d.form_annot() {
                    Some(s) => s.clone(),
                    _ => to_mono(self.fresh())
                }));

            // Add them into environment
            _env.extend_n(name_scms)
        };

        // Add constructor types to environment
        for (ctor_id, ctor_scheme) in constructor_types {
            env.insert(ctor_id, ctor_scheme);
        }

        for d in program.iter_mut() {
            if d.is_form() {
                let sub = {
                    self.infer(&mut env, d.form_body_mut())?;
                    self.solve()?
                };

                // Collect instantiations from this function body
                self.collect_instantiations(&sub);

                d.form_body_mut().apply_mut(&sub);

                let general = {
                    let ty = d.form_type().body().to_owned();
                    let fvs: Vec<_> = ty.ftv().into_iter().collect();
                    if fvs.is_empty() {
                        Scheme::Mono(ty)
                    } else {
                        Scheme::Poly(fvs, ty)
                    }
                };

                d.form_body_mut().tag.set_scheme(general.clone());

                // Register polymorphic functions for potential specialization
                let fn_name = self.interner.trace(d.name()).to_string();
                self.instantiation_registry.register_polymorphic(fn_name, general.clone());

                env.insert(d.name().to_owned(), d.form_type().clone());
            }
        }

        Ok(())
    }


    /// Solve constraints. This will move out the `Infer` struct.
    fn solve<'a>(&mut self) -> Result<Subst, TypeError> {
        let mut sub = HashMap::<Name, Type>::new();
        // let mut constraints = self.constraints;
        let mut constraints = mem::replace(&mut self.constraints, LinkedList::new());
        // println!("Type constraints: ");
        // println!("{:#?}", constraints);
        let mut step: usize = 1;
        while let Some(cons) = constraints.pop_front() {
            // println!("Type solve step {:?}: ", step);
            let new_sub = cons.unify()?;
            // println!("current unify: ");
            // println!("{:#?}", new_sub);
            for ref mut c in constraints.iter_mut() {
                c.apply_mut(&new_sub);
            }
            sub = sub.into_iter().map(|(k, v)| (k, v.apply(&new_sub))).collect();
            sub.extend(new_sub);
            // println!("Type substituation: ");
            // println!("{:#?}", sub);
            step = step + 1;
        }
        Ok(sub)
    }
    // pub fn 
}


#[cfg(test)]
mod tests {
    use crate::typeinfer::*;
    use crate::typeinfer::subst::SubstMut;
    use crate::syntax::form::*;
    use crate::types::*;
    use crate::syntax::parser;
    use crate::utils::*;
    use crate::internal::*;
    use crate::internal;

    fn parse_expr(inter: &mut Interner, src: &str) -> Form {
        parser::parse_expr(src, inter).expect("Failed to parse expression")
    }

    fn s(src: &'static str) -> String {
        String::from(src)
    }

    #[test]
    fn infer_lit() {
        let mut interner = Interner::new();
        let mut inf = Infer::new(&mut interner);
        let mut env = TypeEnv::new();

        assert_eq!(inf.infer(&mut env,
                             &mut Form::new(Span::new(0, 0), Expr::Lit(Lit::Int(123)))),
                   Ok(&Scheme::Mono(Type::Con("Int".to_string()))));
    }

    #[test]
    fn infer_fun() {
        use self::Expr::*;
        use self::Scheme::*;
        use self::Type::*;
        let mut interner = Interner::new();
        let ty_op = Scheme::Poly(vec!["a".to_string()],
                                 Type::Arr(Box::new(Type::Prod(Box::new(Type::Var("a".to_string())),
                                                          Box::new(Type::Var("a".to_string())))),
                                             Box::new(Type::Var("a".to_string()))));
        let primitives: Vec<(&str, &Scheme)> = vec![("+", &ty_op)];
        let mut syn: Form = parse_expr(&mut interner, "(a, b) -> let c = a in { c + b + 1 }");

        let mut env = TypeEnv::from_iter(primitives.iter().map(|&(n, s)| (interner.intern(n), s.clone())));
        let sub = {
            let mut inf = Infer::new(&mut interner);
            inf.infer(&mut env, &mut syn).expect("Type inference failed");
            inf.solve().unwrap()
        };
        syn.apply_mut(&sub);

        // Check that the inferred type is Int * Int -> Int
        let expected_type = parser::parse_type("Int * Int -> Int", &mut Interner::new());
        assert_eq!(syn.tag.ty, expected_type);
    }

    #[test]
    fn infer_adt_constructors() {
        // Test that ADT constructor types are correctly generated
        let mut interner = Interner::new();
        let src = "
data Option a {
    None,
    Some(a)
}

def test(x) = Some(x)
";
        let mut program = parser::parse(src, &mut interner).expect("Parse failed");
        let env = TypeEnv::new();
        let mut inf = Infer::new(&mut interner);

        // Check that ADT info is registered
        inf.infer_defs(&env, &mut program).expect("Type inference failed");

        // Verify Option ADT was registered
        assert!(inf.adt_registry.contains_key("Option"));
        let option_info = &inf.adt_registry["Option"];
        assert_eq!(option_info.name, "Option");
        assert_eq!(option_info.type_params, vec!["a".to_string()]);
        assert_eq!(option_info.variants.len(), 2);

        // Check None variant
        assert_eq!(option_info.variants[0].name, "None");
        assert_eq!(option_info.variants[0].tag, 0);
        assert!(option_info.variants[0].field_types.is_empty());

        // Check Some variant
        assert_eq!(option_info.variants[1].name, "Some");
        assert_eq!(option_info.variants[1].tag, 1);
        assert_eq!(option_info.variants[1].field_types.len(), 1);
    }

    #[test]
    fn infer_pattern_matching() {
        // Test type inference for pattern matching
        let mut interner = Interner::new();
        let src = "
data Option a {
    None,
    Some(a)
}

def unwrap(opt) = match opt {
    None -> 0,
    Some(x) -> x
}
";
        let mut program = parser::parse(src, &mut interner).expect("Parse failed");

        // Need to add prelude operators
        let ty_op = Scheme::Poly(vec!["a".to_string()],
                                 Type::Arr(Box::new(Type::Prod(Box::new(Type::Var("a".to_string())),
                                                          Box::new(Type::Var("a".to_string())))),
                                             Box::new(Type::Var("a".to_string()))));
        let env = TypeEnv::from_iter([
            (interner.intern("+"), ty_op.clone()),
            (interner.intern("-"), ty_op.clone()),
        ].into_iter());

        let mut inf = Infer::new(&mut interner);
        let result = inf.infer_defs(&env, &mut program);

        // Type inference should succeed
        assert!(result.is_ok(), "Type inference failed: {:?}", result);

        // Find the 'unwrap' function and verify its type
        for d in &program {
            if interner.trace(d.ident) == "unwrap" {
                // unwrap should have type: Option Int -> Int
                // (since None -> 0 constrains the result to Int, and Some(x) -> x constrains the input)
                println!("unwrap type: {:?}", d.form_type());
                assert!(d.form_type().is_fn());
            }
        }
    }

}