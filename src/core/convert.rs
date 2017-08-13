/// Give names to intermediate values

use std::collections::HashSet;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::ops::Deref;
use std::ops::DerefMut;

use utils::*;
use types::*;

use syntax::*;
use core::term::*;


#[derive(Default, Debug)]
pub struct K {
    count: usize,
    env: HashMap<String, Scheme>,
    global: HashMap<String, P<Func>>,
    cur_name: String,
}

impl K {
    /// Do transformation on a syntax module,
    /// generate core term representation
    pub fn go<I>(module: I) -> HashMap<String, P<Func>>
        where I: IntoIterator<Item=P<Def>>
    {
        let mut runner = K {
            count: 0,
            env: HashMap::new(),
            global: HashMap::new(),
            cur_name: "".to_string()
        };
        {
            let b = &mut runner;

            for def in module.into_iter() {
                b.convert_fun_def(*def);
            }
        }

        runner.global
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
    fn make_cls_name(&mut self, bound: &str) -> String {
        self.cur_name.clone() + "$closure$" + self.unique().to_string().as_str()
    }

    /// Convert a global definition to term
    fn convert_fun_def(&mut self, def: Def) {
        let Def { ident, node, .. } = def;
        match node {
            Item::Form(form) => {
                let _form = *form;
                let Form { node, tag } = _form;
                let ty = tag.ty;
                self.cur_name = ident;
                let def_name = self.cur_name.clone();
              
                match node {
                    Expr::Abs(lambda) => {
                        // We can ignore fvs (glocal definitions) there
                        // because of type check
                        let (ps, _, bd) = self.trans_lambda(lambda);
                        self.define_fn(def_name, ty, ps, vec![], bd);
                    }
                    _ => unreachable!()
                }
            },
            _ => {}
        }
    }

    /// Add a function in top level definitions
    fn define_fn<'c: 'b, 'b>(
        &'c mut self,
        name: String,
        ty: Scheme,
        params: Vec<VarDecl>,
        freevars: Vec<VarDecl>,
        body: Term)
        -> (&'b str, Vec<&'b str>)
    {
        let fun = Func::new(name.clone(), ty, params, freevars, body);
        let ent = self.global.entry(name).or_insert(P(fun));
        (&(*ent).name(), (*ent).fv().iter()
                            .map(|ref v| { v.name() })
                            .collect())
    }


    /// Get free variables of a term
    fn fv<'a>(&mut self, source: &'a Term) -> HashSet<&'a str> {
        use self::Term::*;
        match *source {
            Term::Lit(_) => HashSet::new(),
            Var(ref v) => {
                let mut hs = HashSet::new();
                hs.insert(v.as_str());
                return hs
            },

            List(ref lst) | Block(ref lst) => {
                lst.iter().fold(HashSet::new(), |mut res, v| {
                    res.extend(self.fv(v.deref()));
                    res
                })
            },

            // The new binding name should not be visible
            // in binding value.
            Let(ref var, ref val, ref exp) => {
                let mut res = self.fv(exp.deref());
                res.remove(var.name());
                res.extend(&self.fv(val.deref()));
                res
            },
            MakeCls(ref var, ref cls, ref exp) => {
                let mut r = self.fv(exp.deref());
                r.extend(cls.deref().fv());
                r.remove(var.name());
                r
            },
            ApplyCls(ref n, ref args) => {
                let mut r = args.iter().fold(HashSet::new(), |mut res, v| {
                    res.extend(self.fv(v.deref()));
                    res
                });
                r.extend(self.fv(n.deref()));
                r
            },
            Binary(_, ref lhs, ref rhs) => {
                let mut r = self.fv(lhs.deref());
                r.extend(self.fv(rhs.deref()));
                r
            },
            Unary(_, ref e) => self.fv(e.deref()),
            If(ref c, ref t, ref f) => {
                let mut r = self.fv(c.deref());
                r.extend(self.fv(t.deref()));
                r.extend(self.fv(f.deref()));
                r
            },
        }
    }

    /// Add a variable in environment
    fn close_var(&mut self, var: &str, ty: Scheme) -> Option<Scheme> {
        self.env.insert(var.to_string(), ty)
    }
    /// Remove a variable from environment
    fn release_var(&mut self, var: &str) -> Option<Scheme> {
        self.env.remove(var)
    }

    /// Find if a variable is in environment
    fn find_var(&mut self, var: &str) -> Option<&Scheme> {
        self.env.get(var)
    }

    /// Get parameters, free variables, function body term from lambda
    fn trans_lambda(&mut self, lambda: Lambda)
        -> (Vec<VarDecl>, Vec<VarDecl>, Term)
    {
        let params = lambda.param;
        let bd = *lambda.body;

        let (body_term, fvs) = {
            let mut present: Vec<&str> = Vec::new();
            let mut backup: Vec<(&str, Scheme)> = Vec::new();

            // Add parameter into env
            {
                let it = params.iter();
                for para in it {
                    let &VarDecl(ref n, ref t) = para;
                    let pname = n.as_str();
                    if let Some(origin) = self.close_var(pname, t.to_owned()) {
                        backup.push((pname, origin));
                    }
                    present.push(pname);
                }
            }

            let body_term = self.transform(bd);
            let fvs = self.fv(&body_term).iter().map(|vn| {
                VarDecl(vn.to_string(), self.env[&vn.to_string()].to_owned())
            }).collect();

            // Reset env
            for bname in present {
                self.release_var(bname);
            }
            for (bname, bty) in backup {
                self.close_var(bname, bty);
            }
            (body_term, fvs)
        };

        (params, fvs, body_term)
    }

    fn transform_list(&mut self, lst: Vec<P<Form>>) -> Vec<P<Term>> {
        lst.into_iter().map(|f| box self.transform(*f)).collect()
    }

    /// Transform syntax form into core term
    fn transform(&mut self, form: Form) -> Term {
        use self::Expr::*;
        let Form { node, tag: FormTag { ty: tform, .. } } = form;
        match node {
            Lit(l) => Term::Lit(l),
            Var(n) => {
                // A global definition should not be in scope env
                if self.find_var(n.as_str()) == None && tform.is_fn() {
                    // For global function name, make a closure
                    Term::MakeCls(
                        VarDecl(n.clone(), tform),
                        box Closure::new(n.as_str(), vec![]),
                        box Term::Var(n))
                } else {
                    Term::Var(n)
                }
            },
            List(e) | Block(e) => Term::List(self.transform_list(e)),
            Unary(op, e) => Term::Unary(op, box self.transform(*e)),
            If(cond, tr, fl) => {
                Term::If(
                    box self.transform(*cond),
                    box self.transform(*tr),
                    box self.transform(*fl))
            }
            Binary(op, left, right) => {
                Term::Binary(
                    op,
                    box self.transform(*left),
                    box self.transform(*right))
            }
            Let(v, val, exp) => {
                let exp_term = self.transform(*exp);
                if let Abs(lambda) = val.node {
                    // Handle closure

                    let (cls_name, cls_fv) = {
                        let VarDecl(ref var, ref var_ty) = v;
                        let bound = var.as_str();
                        let origin = self.close_var(bound, var_ty.to_owned());

                        let (ps, fv, bd) = self.trans_lambda(lambda);
                        self.release_var(bound);
                        if let Some(v) = origin {
                            self.close_var(bound, v);
                        }

                        let _cls_name = self.make_cls_name(bound);
                        self.define_fn(
                            _cls_name,
                            var_ty.clone(),
                            ps, fv, bd)
                    };
                    
                    let cls = Closure::new(
                        cls_name,
                        cls_fv);
                    
                    Term::MakeCls(v, box cls, box exp_term)
                } else {
                    // Normal variable bingding
                    let val_term = self.transform(*val);
                    Term::Let(v, box val_term, box exp_term)
                }
            },
            
            Apply(callee, params) => {
                let callee_term = self.transform(*callee);
                let params_term = self.transform_list(params);
                Term::ApplyCls(box callee_term, params_term)
            }

            // Give anonymous lambda a name binding
            Abs(lambda) => {
                let ty = tform;
                let tmp_name = {
                    let fr_name = self.fresh();
                    self.make_cls_name(fr_name.as_str())
                };
                let (ps, fv, bd) = self.trans_lambda(lambda);

                let (cls_name, cls_fv) = self.define_fn(tmp_name.clone(), ty.clone(), ps, fv, bd);
                let cls = Closure::new(cls_name, cls_fv);
                Term::MakeCls(VarDecl(tmp_name.clone(), ty), box cls, box Term::Var(tmp_name))
            },
        }
    }

}

