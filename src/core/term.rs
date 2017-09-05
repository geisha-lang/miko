use std::string::*;
use std::collections::HashMap;

use std::ops::Deref;
use std::ops::DerefMut;

use std::iter::IntoIterator;
use std::iter::DoubleEndedIterator;

use internal::*;
use types::*;
use utils::*;


type Node = Box<TaggedTerm>;


#[derive(Clone, PartialEq, Debug)]
pub struct TypeDef {
    name: Name,
    params: Vec<Name>,
    body: TypeKind
}

#[derive(Clone, PartialEq, Debug)]
pub enum TypeKind {
    Alias(P<Scheme>),
    Algebra(Vec<Variant>)
}

impl TypeDef {
    pub fn new(name: String, params: Vec<Name>, body: TypeKind) -> Self {
        TypeDef { name, params, body }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunDef {
    name: Name,
    params: Vec<VarDecl>,
    freevars: Vec<VarDecl>,
    body: Node,
    ty: Scheme, 
}

impl FunDef {
    pub fn new(name: Name,
               ty: Scheme,
               params: Vec<VarDecl>,
               freevars: Vec<VarDecl>,
               body: TaggedTerm)
               -> FunDef {
        FunDef {
            name: name,
            params: params,
            freevars: freevars,
            body: box body,
            ty: ty, 
//            inst: None
        }
    }

    pub fn name<'a>(&'a self) -> &str {
        self.name.as_str()
    }

    pub fn fv<'a>(&'a self) -> &'a Vec<VarDecl> {
        &self.freevars
    }

    pub fn body(&self) -> &TaggedTerm {
        self.body.deref()
    }

    pub fn ref_type(&self) -> &Type {
        self.ty.body()
    }

    pub fn parameters(&self) -> &Vec<VarDecl> {
        &self.params
    }
}

/// Represents a expression term
#[derive(Clone, PartialEq, Debug)]
pub struct TaggedTerm {
    ty: Scheme,
    node: Term,
}

impl TaggedTerm {
    pub fn new(ty: Scheme, node: Term) -> TaggedTerm {
        TaggedTerm { ty, node }
    }

    pub fn ref_scheme(&self) -> &Scheme {
        &self.ty
    }
    pub fn body(&self) -> &Term {
        &self.node
    }
}


/// Represents a expression term
#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    /// Literal value
    Lit(Lit),
    /// Identifier (binding/definition)
    Var(Id),
    /// List (array)
    /// e.g. `[fuck, shit]`
    List(Vec<Node>),
    /// Block (statement sequence)
    /// e.g. `{ print(fuck); print(shit); 1 }`
    Block(Vec<Node>),

    /// Make closure with free variables
    MakeCls(VarDecl, P<Closure>, Node),

    /// Apply a closure
    ApplyCls(Node, Vec<Node>),

    //** For now, all functions will represent as closure
    /// Apply a global function directly
    // ApplyDir(Name, Vec<Node>),
    /// Binary operator expression
    /// e.g. `fuck + shit`
    Binary(BinOp, Node, Node),
    /// Unary operator expression
    /// e.g. `!fuck`
    /// e.g. `-shit`
    Unary(UnOp, Node),

    /// Let-in expression
    /// e.g. `let fuck = shit in fuck + 1`
    Let(VarDecl, Node, Node),
    /// Conditional expression
    /// e.g. `if (fuck == shit) 1 else 0`
    If(Node, Node, Node),
}

/// Represents a closure, including a entry as
///   global definition and a actual free variable list.
#[derive(Clone, PartialEq, Debug)]
pub struct Closure {
    entry: String,
    actualFv: Vec<Id>,
}

impl Closure {
    pub fn new(ent: &str, fv: Vec<Id>) -> Closure {
        Closure {
            entry: ent.to_string(),
            actualFv: fv,
        }
    }

    pub fn fv(&self) -> Vec<Id> {
        self.actualFv.clone()
    }

    pub fn entry(&self) -> &str {
        self.entry.as_str()
    }
}
