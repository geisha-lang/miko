use typeinfer::typeenv::*;
use typeinfer::subst::*;
use syntax::*;

use utils::*;

use std::collections::LinkedList;
use std::collections::HashMap;
use std::collections::HashSet;

use std::iter::IntoIterator;
use std::iter::DoubleEndedIterator;

use std::ops::Deref;
use std::ops::DerefMut;
use std::ops::Placer;

#[derive(Clone, PartialEq, Debug)]
pub enum TypeError<'a> {
    NotInScope(&'a Form),
    MisMatch(Type, Type),
    HighRank(&'a Form),
    UnknownOperator(&'a BinOp, &'a Pos),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Infer {
    unique: usize,
    constraints: LinkedList<Constraint>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Constraint(Type, Type);

impl Constraint {
    pub fn apply_mut(&mut self, sub: &Subst) {
        self.0 = self.0.clone().apply(sub);
        self.1 = self.1.clone().apply(sub);
    }
    pub fn unify<'a>(self) -> Result<Subst, TypeError<'a>> {
        use self::Type::*;
        match self {
            Constraint(Var(n), t) |
            Constraint(t, Var(n)) => {
                let mut s = HashMap::new();
                s.insert(n, t);
                Ok(s)
            },
            Constraint(Arr(l1, l2), Arr(r1, r2)) |
            Constraint(Prod(l1, l2), Prod(r1, r2)) |
            Constraint(Comp(l1, l2), Comp(r1, r2)) => {
                let mut u1 = Constraint(*l1, *r1).unify()?;
                let u2 = Constraint(*l2, *r2).apply(&u1).unify()?;
                u1.extend(u2);
                Ok(u1)
            },
            Constraint(el, er) => {
                if el == er {
                    Ok(HashMap::new())
                } else {
                    Err(TypeError::MisMatch(el, er))
                }
            }
        }
    }
}


impl Substituable for Constraint {
    fn apply(self, sub: &Subst) -> Self {
        Constraint(self.0.apply(sub), self.1.apply(sub))
    }
    // fn apply_mut(&mut self, sub: &Subst) {}
    fn ftv(&self) -> HashSet<Name> {
        let mut r = self.0.ftv();
        r.extend(self.1.ftv());
        r
    }
}



fn generalize(e: &TypeEnv, ty: Type) -> Scheme {
    let fvs: Vec<_> = ty.ftv()
        .into_iter()
        .filter(|fv| !e.exist(fv.as_str()))
        .collect();
    if fvs.is_empty() {
        Scheme::Mono(P(ty))
    } else {
        Scheme::Poly(fvs, P(ty))
    }
}

fn to_mono(ty: Type) -> Scheme {
    Scheme::Mono(P(ty))
}


impl Infer {
    pub fn new() -> Infer {
        Infer {
            unique: 0,
            constraints: LinkedList::new(),
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
                let mut sub: Subst = HashMap::new();
                sub.extend(tvs.clone().into_iter().zip(tvs_));

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
                     -> Result<&'a Scheme, TypeError<'a>> {
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
                    return Err(NotInScope(form));
                }
            }

            // Abstraction (function) should have a arrow type.
            // Generate temporary type var
            //   if parameter has no type annotation.
            // Exntend the environment with parameters type,
            //   then infer function body.
            Abs(ref mut fun) => {
                let mut extends: Vec<(&str, &Scheme)> = vec![];
                let mut types: Vec<Type> = vec![];

                for p in fun.param.iter_mut() {
                    match p.1 {
                        Slot => p.1 = Scheme::Mono(P(self.fresh())),
                        _ => {}
                    }
                    extends.push((p.0.as_str(), &p.1));
                    types.push(p.1.body().clone());
                }
                let typaram = Type::product_n(types);
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
                if let Some(ty_op) = e.lookup(op.as_str()) {
                    let ty_lr = Type::product(ty_left.clone(), ty_right.clone());

                    form.tag.ty = to_mono(self.fresh());

                    let ty_fun = Scheme::arrow(ty_lr, form.tag.clone_type());
                    self.uni(ty_op.body(), ty_fun.body());
                } else {
                    return Err(TypeError::UnknownOperator(op, &form.tag.pos));
                }
            }

            // Let bound
            Let(VarDecl(ref name, ref mut ty), ref mut val, ref mut body) => {
                let tyval = self.infer(e, val)?;
                if *ty == Scheme::Slot {
                    *ty = to_mono(self.fresh());
                }
                self.uni(ty.body(), tyval.body());

                let tyexp = self.infer(&e.extend(name.as_str(), &tyval), body)?;
                form.tag.ty = tyexp.clone();
            }

            // If conditional expression
            // Condition expression should be `Bool`,
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
                let mut ty = &Scheme::Mono(P(Type::Void));
                for mut f in exps.iter_mut() {
                    ty = self.infer(e, f.deref_mut())?;
                }

                form.tag.set_scheme(ty.clone());
            }

            // List should be mono
            List(ref mut exps) => {
                let tyitem: Type = self.fresh();
                for mut f in exps.iter_mut() {
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
                      -> Result<(), TypeError<'a>> {
        
        // Only need form definitions
        let defs: Vec<&mut Def> = program
            .iter_mut()
            .filter(|ref v| v.is_form())
            .collect();
        
        // Give each definitions a temporary type
        let name_scms: Vec<(String, Scheme)> = defs
            .iter()
            .map(|ref d| (d.ident.clone(), to_mono(self.fresh())))
            .collect();
        // Add them into environment
        let extends: Vec<_> = name_scms
            .iter()
            .map(|&(ref n, ref s)| (n.as_str(), s))
            .collect();
        let env = _env.extend_n(extends);

        for d in defs {
            self.infer(&env, d.form_body_mut())?;
        }
        Ok(())
    }


    /// Solve constraints. This will move out the `Infer` struct.
    pub fn solve<'a>(self) -> Result<Subst, TypeError<'a>> {
        let mut sub = HashMap::<Name, Type>::new();
        let mut constraints = self.constraints;
        while let Some(cons) = constraints.pop_front() {
            let new_sub = cons.unify()?;
            for ref mut c in constraints.iter_mut() {
                c.apply_mut(&sub);
            }
            sub = sub.into_iter().map(|(k, v)| (k, v.apply(&new_sub))).collect();
            sub.extend(new_sub);
        }
        Ok(sub)
    }
    // pub fn 
}


#[cfg(test)]
mod tests {
    use typeinfer::infer::*;
    use typeinfer::subst::*;
    use typeinfer::typeenv::*;
    use syntax::*;
    use parser::*;
    use utils::*;
    use pest::*;


    fn parse_expr(src: &str) -> Form {
        let mut parser = Rdp::new(StringInput::new(src));
        parser.expr();
        *parser.__expr()
    }


    fn form(e: Expr) -> P<Form> {
        P(Form::new(Pos::new(0, 0), e))
    }

    fn fmty(t: Scheme, e: Expr) -> P<Form> {
        P(Form::typed(Pos::new(0, 0), t, e))
    }

    fn s(src: &'static str) -> String {
        String::from(src)
    }

    #[test]
    fn infer_lit() {
        let mut inf = Infer::new();
        let mut env = TypeEnv::new();

        assert_eq!(inf.infer(&mut env,
                             &mut Form::new(Pos::new(0, 0), Expr::Lit(Lit::Int(123)))),
                   Ok(&Scheme::Mono(P(Type::Con("Int".to_string())))));
    }

    #[test]
    fn infer_fun() {
        use self::Expr::*;
        use self::Scheme::*;
        use self::Type::*;
        let ty_op = Scheme::Poly(vec!["a".to_string()],
                                 P(Type::Arr(P(Type::Prod(P(Type::Var("a".to_string())),
                                                          P(Type::Var("a".to_string())))),
                                             P(Type::Var("a".to_string())))));
        let PRIMITIVES: Vec<(&str, &Scheme)> = vec![("+", &ty_op)];
        let mut syn: Form = parse_expr("(a: Fuck, b) -> let c = a in { c + b }");

        let mut env = TypeEnv::from_iter(PRIMITIVES.clone());
        let sub = {
            let mut inf = Infer::new();
            inf.infer(&mut env, &mut syn);
            inf.solve().unwrap()
        };

        syn.apply_mut(&sub);

        assert_eq!(syn,
                   *fmty(Mono(P(
                       Arr(
                           P(Prod(
                               P(Con("Fuck".to_string())),
                               P(Con("Fuck".to_string()))
                           )),
                           P(Con("Fuck".to_string()))
                       ))),
                         Abs(Lambda {
                                 param: vec![VarDecl(s("a"), Scheme::con("Fuck")),
                                             VarDecl(s("b"), Scheme::con("Fuck"))],
                                 body: fmty(Scheme::con("Fuck"),
                                            Let(VarDecl(s("c"), Mono(P(Type::Con(s("Fuck"))))),
                                                fmty(Scheme::con("Fuck"), Expr::Var(s("a"))),
                                                fmty(Scheme::con("Fuck"),
                                                     Block(vec![fmty(Scheme::con("Fuck"),
                                                                     Binary(BinOp::Add,
                                                                            fmty(Scheme::con("Fuck"),
                                                                                 Expr::Var(s("c"))),
                                                                            fmty(Scheme::con("Fuck"),
                                                                                 Expr::Var(s("b")))))])))),
                             })));

    }

}