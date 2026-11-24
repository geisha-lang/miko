use std::ops::Deref;

use crate::internal::*;
use crate::syntax::form::Pattern;
use crate::types::*;
use crate::utils::*;

pub type Node = Box<TaggedTerm>;

#[derive(Clone, PartialEq, Debug)]
pub struct TypeDef {
    pub name: Name,
    pub params: Vec<Name>,
    pub body: TypeKind,
}

#[derive(Clone, PartialEq, Debug)]
pub enum TypeKind {
    Alias(P<Scheme>),
    Algebra(Vec<Variant>),
}

impl TypeDef {
    pub fn new(name: String, params: Vec<Name>, body: TypeKind) -> Self {
        TypeDef { name, params, body }
    }
}

/// Top-level value definition (`def pi = 3.14`), evaluated once at startup
#[derive(Clone, PartialEq, Debug)]
pub struct GlobalValDef {
    pub name: Id,
    pub ty: Type,
    pub init: TaggedTerm,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunDef {
    name: Id,
    params: Vec<VarDecl>,
    freevars: Vec<VarDecl>,
    body: Node,
    ty: Scheme,
}

impl FunDef {
    pub fn new(
        name: Id,
        ty: Scheme,
        params: Vec<VarDecl>,
        freevars: Vec<VarDecl>,
        body: TaggedTerm,
    ) -> FunDef {
        FunDef {
            ty,
            name,
            params,
            freevars,
            body: Box::new(body),
        }
    }

    pub fn name(&self) -> Id {
        self.name
    }

    pub fn fv(&self) -> &Vec<VarDecl> {
        &self.freevars
    }

    pub fn body(&self) -> &TaggedTerm {
        self.body.deref()
    }

    pub fn body_mut(&mut self) -> &mut TaggedTerm {
        &mut self.body
    }

    pub fn ref_type(&self) -> &Type {
        self.ty.body()
    }

    pub fn scheme(&self) -> &Scheme {
        &self.ty
    }

    pub fn parameters(&self) -> &Vec<VarDecl> {
        &self.params
    }
}

/// Expression term annotated with its `Scheme`
#[derive(Clone, PartialEq, Debug)]
pub struct TaggedTerm {
    pub ty: Scheme,
    pub node: Term,
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

/// Core intermediate representation after K-conversion
#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    /// Primitive or unit literal
    Lit(Lit),
    /// Local variable or function reference
    Var(Id),
    /// Reference to a top-level value global (`def x = ...`)
    GlobalVar(Id),
    /// First-class tuple construction (`(e1, e2)`)
    Tuple(Vec<Node>),
    /// Sequential block (`{ e1; e2 }`)
    Block(Vec<Node>),
    /// Create closure value with captured free variables
    MakeCls(VarDecl, P<Closure>, Node),
    /// Call first-class closure value
    ApplyCls(Node, Vec<Node>),
    /// Direct call to a known top-level/non-capturing function
    ApplyDir(VarDecl, Vec<Node>),
    /// Binary operator
    Binary(BinOp, Node, Node),
    /// Unary operator (`!e`, `-e`)
    Unary(UnOp, Node),
    /// Local let binding
    Let(VarDecl, Node, Node),
    /// Conditional branch
    If(Node, Node, Node),
    /// Pattern match with optional transformed guard term
    Match(Node, Vec<(Pattern, Option<Node>, Node)>),
    /// Construct ADT variant value
    MakeData {
        type_name: Name,
        tag: usize,
        fields: Vec<Node>,
        field_types: Vec<Type>,
        type_args: Vec<Type>,
    },
    /// Extract ADT tag
    GetTag(Node),
    /// Extract ADT field
    GetField(Node, usize),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Closure {
    entry: Id,
    actual_fv: Vec<Id>,
}

impl Closure {
    pub fn new(entry: Id, actual_fv: Vec<Id>) -> Closure {
        Closure { entry, actual_fv }
    }

    pub fn fv(&self) -> Vec<Id> {
        self.actual_fv.clone()
    }

    pub fn entry(&self) -> Id {
        self.entry
    }
}
