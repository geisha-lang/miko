use syntax::form::*;
use internal::*;
use types::*;

use utils::*;

use std::collections::LinkedList;
use std::collections::HashMap;
use std::collections::HashSet;

use std::iter::IntoIterator;
use std::iter::DoubleEndedIterator;

use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::Placer;

use std::mem;

use super::subst::*;
use super::constraint::{ Constraint };
pub use super::error::{ TypeError };

#[derive(Debug)]
pub struct Infer<'interner> {
    unique: usize,
    interner: &'interner mut Interner,
    constraints: LinkedList<Constraint>,
}


//fn generalize(e: &TypeEnv, ty: Type) -> Scheme {
//}

fn to_mono(ty: Type) -> Scheme {
    Scheme::Mono(ty)
}


impl<'i> Infer<'i> {
    pub fn new(interner: &'i mut Interner) -> Infer<'i> {
        Infer {
            unique: 0,
            constraints: LinkedList::new(),
            interner,
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
    fn uni(&mut self, left: &Type, right: &Type) {
        self.constraints.push_back(Constraint(left.clone(), right.clone()));
    }

    /// Temporary instantiate a polymorphism type
    fn instantiate(&mut self, scm: &Scheme) -> Type {
        use self::Scheme::*;
        match *scm {
            Mono(ref ty) => ty.deref().clone(),
            Poly(ref tvs, ref ty) => {
                let tvs_: Vec<_> = tvs.iter().map(|_| self.fresh()).collect();
                let sub: Subst = tvs.clone().into_iter().zip(tvs_).collect();

                ty.clone().apply(&sub)
            }
            Slot => unreachable!(),
        }
    }

    /// The main inference algorithm
    /// Infering type and tagging form
    fn infer<'a>(&mut self,
                     e: &TypeEnv,
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
                    return Err(NotInScope(form.to_owned()));
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
                let new_env = e.extend_n(extends);
                let tbody = self.infer(&new_env, fun.body.deref_mut())?;
                form.tag.ty = Scheme::arrow(typaram, tbody.body().clone());
            }

            // A function apply.
            // Types of arguments should be able to unify with callee.
            // Infer the returned type.
            Apply(ref mut callee, ref mut args) => {
                let ty_callee = self.infer(e, (*callee).deref_mut())?;
                let callee_inst = self.instantiate(ty_callee);
                let mut ty_args: Vec<Type> = vec![];

                for arg in args.iter_mut() {
                    ty_args.push(self.infer(e, (*arg).deref_mut())?
                                     .body()
                                     .clone());
                }

                form.tag.set_type(self.fresh());

                let tyfun = Scheme::arrow(Type::product_n(ty_args), form.tag.clone_type());

                self.uni(&callee_inst, tyfun.body());
            }

            // Type of binary ops should exist in environment.
            Binary(ref op, ref mut left, ref mut right) => {
                let ty_left = self.infer(e, left)?.body();
                let ty_right = self.infer(e, right)?.body();
                if let Some(ty_op) = e.lookup(&self.interner.intern(&op.as_str())) {
                    let ty_lr = Type::product(ty_left.clone(), ty_right.clone());

                    form.tag.ty = to_mono(self.fresh());

                    let ty_fun = Scheme::arrow(ty_lr, form.tag.clone_type());
                    self.uni(ty_op.body(), ty_fun.body());
                } else {
                    return Err(TypeError::UnknownOperator(op.clone(), form.tag.pos.clone()));
                }
            }

            // Let bound
            Let(VarDecl(ref name, ref mut ty), ref mut val, ref mut body) => {
                let tyval = self.infer(e, val)?;
                if *ty == Scheme::Slot {
                    *ty = to_mono(self.fresh());
                }
                self.uni(ty.body(), tyval.body());

                let tyexp = self.infer(&e.extend(name.to_owned(), tyval.to_owned()), body)?;
                form.tag.ty = tyexp.clone();
            }

            // If expression
            // Condition should be `Bool`,
            //   all branches should be unified.
            If(ref mut cond, ref mut tr, ref mut fl) => {
                let tycond = self.infer(e, cond)?;
                let tytr = self.infer(e, tr)?;
                let tyfl = self.infer(e, fl)?;

                self.uni(tycond.body(), &Type::Con("Bool".to_string()));
                self.uni(tytr.body(), tyfl.body());
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
                    let ty = self.infer(e, f)?;
                    self.uni(ty.body(), &tyitem);
                }

                form.tag.set_scheme(to_mono(
                    Type::compose(
                        Type::Con("List".to_string()),
                        tyitem)));
            },
            _ => unimplemented!(),
        }

        // If there is a type annotation, unify it with inferred type
        if let Some(ref annot) = form.tag.annotate {
            let anno_inst = self.instantiate(annot);
            self.uni(&anno_inst, form.tag.ref_type());
        }

        Ok(form.tag.ref_scheme())

    }

    /// Do type inference over top level definitions
    pub fn infer_defs<'a>(&mut self,
                      _env: &'a TypeEnv<'a>,
                      program: &'a mut Vec<Def>)
                      -> Result<(), TypeError> {
                
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

        for d in program.iter_mut() {
            if d.is_form() {
                let sub = {
                    self.infer(&env, d.form_body_mut())?;
                    self.solve()?
                };
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

                d.form_body_mut().tag.set_scheme(general);

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
    use typeinfer::*;
    use syntax::form::*;
    use types::*;
    use syntax::parser;
    use utils::*;
    use internal::*;
    use internal;
    // use pest::*;


    fn parse_expr(inter: &mut Interner, src: &str) -> Form {
        parser::expression(src, inter).unwrap()
    }


    fn form(e: Expr) -> P<Form> {
        P(Form::new(Span::new(0, 0), e))
    }

    fn fmty(t: Scheme, e: Expr) -> P<Form> {
        P(Form::typed(Span::new(0, 0), t, e))
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
                                 Type::Arr(P(Type::Prod(P(Type::Var("a".to_string())),
                                                          P(Type::Var("a".to_string())))),
                                             P(Type::Var("a".to_string()))));
        let PRIMITIVES: Vec<(&str, &Scheme)> = vec![("+", &ty_op)];
        let mut syn: Form = parse_expr(&mut interner, "(a, b) -> let c = a in { c + b + 1 }");

        let mut env = TypeEnv::from_iter(PRIMITIVES.iter().map(|&(n, s)| (interner.intern(n), s.clone())));
        let sub = {
            let mut inf = Infer::new(&mut interner);
            inf.infer(&mut env, &mut syn);
            inf.solve().unwrap()
        };
        use typeinfer::subst::SubstMut;
        syn.apply_mut(&sub);

        assert_eq!(syn, Form::typed(
            Span::new(0, 36),
            parser::type_scheme("Int * Int -> Int", &mut Interner::new()).unwrap(),
            Abs(Lambda {
                param: vec![
                    VarDecl(interner.intern("a"), Scheme::con("Int")),
                    VarDecl(interner.intern("b"), Scheme::con("Int"))
                ],
                body: box Form::typed(
                    Span::new(9, 36),
                    Scheme::con("Int"),
                    Let(
                        VarDecl(interner.intern("c"), Mono(Type::Con(s("Int")))),
                        box Form::typed(
                            Span::new(17, 19),
                            Scheme::con("Int"),
                            Expr::Var(interner.intern("a"))
                        ),
                        box Form::typed(
                            Span::new(22, 36),
                            Scheme::con("Int"),
                            Block(vec![
                                box Form::typed(
                                    Span::new(24, 34),
                                    Scheme::con("Int"),
                                    Binary(BinOp::Add,
                                        box Form::typed(
                                            Span::new(24, 30),
                                            Scheme::con("Int"),
                                            Binary(BinOp::Add,
                                                box Form::typed(
                                                    Span::new(24, 26),
                                                    Scheme::con("Int"),
                                                    Expr::Var(interner.intern("c"))),
                                                box Form::typed(
                                                    Span::new(28, 30),
                                                    Scheme::con("Int"),
                                                        Expr::Var(interner.intern("b"))))),
                                        box Form::typed(
                                            Span::new(32, 34),
                                            Scheme::con("Int"),
                                                Expr::Lit(internal::Lit::Int(1)))))
                            ])
                        )
                    )
                )
            })
        ))
    }

}