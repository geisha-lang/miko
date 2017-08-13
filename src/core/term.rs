use std::string::*;
use std::collections::HashMap;

use std::ops::Deref;
use std::ops::DerefMut;

use std::iter::IntoIterator;
use std::iter::DoubleEndedIterator;

use types::*;
use syntax::{
    Lit,
    VarDecl,
    BinOp,
    UnOp
};
use utils::*;

#[derive(Clone, PartialEq, Debug)]
pub struct Func {
    name: Name,
    params: Vec<VarDecl>,
    freevars: Vec<VarDecl>,
    body: P<Term>,
    ty: Scheme,
    inst: Option<HashMap<String, Term>>,
}

impl Func {
    pub fn new(
        name: String,
        ty: Scheme,
        params: Vec<VarDecl>,
        freevars: Vec<VarDecl>,
        body: Term)
        -> Func
    {
        Func {
            name: name,
            params: params,
            freevars: freevars,
            body: box body,
            ty: ty,
            inst: None
        }
    }

    pub fn name<'a>(&'a self) -> &'a str {
        self.name.as_str()
    }

    pub fn fv<'a>(&'a self) -> &'a Vec<VarDecl> {
        &self.freevars
    }

}

/// Represents a expression term
#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    /// Literal value
    Lit(Lit),
    /// Identifier (binding/definition)
    Var(Name),
    /// List (array)
    /// e.g. `[fuck, shit]`
    List(Vec<P<Term>>),
    /// Block (statement sequence)
    /// e.g. `{ print(fuck); print(shit); 1 }`
    Block(Vec<P<Term>>),

    /// Make closure with free variables
    MakeCls(VarDecl, P<Closure>, P<Term>),

    /// Apply a closure
    ApplyCls(P<Term>, Vec<P<Term>>),

    //** For now, all functions will represent as closure
    /// Apply a global function directly
    // ApplyDir(Name, Vec<P<Term>>),

    /// Binary operator expression
    /// e.g. `fuck + shit`
    Binary(BinOp, P<Term>, P<Term>),
    /// Unary operator expression
    /// e.g. `!fuck`
    /// e.g. `-shit`
    Unary(UnOp, P<Term>),

    /// Let-in expression
    /// e.g. `let fuck = shit in fuck + 1`
    Let(VarDecl, P<Term>, P<Term>),
    /// Conditional expression
    /// e.g. `if (fuck == shit) 1 else 0`
    If(P<Term>, P<Term>, P<Term>),
}

/// Represents a closure, including a entry as
///   global definition and a actual free variable list.
#[derive(Clone, PartialEq, Debug)]
pub struct Closure {
    entry: String,
    actualFv: Vec<String>
}

impl Closure {
    pub fn new(ent: &str, fv: Vec<&str>) -> Closure {
        Closure {
            entry: ent.to_string(),
            actualFv: fv.iter().map(|s| { s.to_string() }).collect()
        }
    }

    pub fn fv<'a>(&self) -> Vec<&str> {
        (&self.actualFv).iter().map(|s| { s.as_str() }).collect()
    }
}


