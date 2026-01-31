use std::string::*;

use std::ops::Deref;
use std::ops::DerefMut;

use std::iter::IntoIterator;
use std::iter::DoubleEndedIterator;

use crate::utils::*;
use crate::types::*;

use crate::internal::*;

pub type E = P<Form>;

/// Represents a top level definition,
/// `def` or `data` or `type`
#[derive(Clone, PartialEq, Debug)]
pub struct Def {
    pub ident: Id,
    pub node: Item,
    pub pos: Span,
}

impl Def {
    /// Get ident as `&str`
    pub fn name(&self) -> Id {
        self.ident.clone()
    }

    /// Create a definition node define a form (value).
    pub fn value(pos: Span, name: Id, body: E) -> Def {
        Def {
              ident: name,
              node: Item::Form(body),
              pos,
        }
    }

    /// If this is a form define
    pub fn is_form(&self) -> bool {
        match self.node {
            Item::Form(_) => true,
            _ => false,
        }
    }

    pub fn form_annot(&self) -> Option<&Scheme> {
        match self.node {
            Item::Form(ref f) => {
                match f.deref().tag.annotate {
                    Some(ref scm) => Some(scm),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Set the type of form
    ///   only called if this is a value definition
    pub fn set_form_scheme(&mut self, scm: Scheme) {
        match self.node {
            Item::Form(ref mut f) => {
                (*f).deref_mut().tag.set_scheme(scm);
            }
            _ => panic!("Set scheme to a non-form definition"),
        }
    }

    /// Get the type of form
    ///   only called if this is a value definition
    pub fn form_type<'a>(&'a self) -> &'a Scheme {
        match self.node {
            Item::Form(ref f) => f.tag.ref_scheme(),
            _ => panic!("Get scheme from a non-form definition"),
        }
    }

    /// Get the form as a body of definition
    pub fn form_body_mut<'a>(&'a mut self) -> &'a mut Form {
        match self.node {
            Item::Form(ref mut f) => (*f).deref_mut(),
            _ => panic!("Get form from a non-form definition"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Item {
    Form(E),
    Alias(Vec<Id>, P<Scheme>),
    Alg(Vec<Id>, Vec<Variant>),
}



/// Represents a form
#[derive(Clone, PartialEq,  Debug)]
pub struct Form {
    /// Expression content
    pub node: Expr,

    /// Form tag
    pub tag: FormTag,
}

impl Form {
    pub fn new(pos: Span, exp: Expr) -> Form {
        Form {
            node: exp,
            tag: FormTag {
                pos: pos,
                ty: Scheme::slot(),
                annotate: None,
            },
        }
    }
    pub fn typed(pos: Span, ty: Scheme, exp: Expr) -> Form {
        Form {
            node: exp,
            tag: FormTag {
                pos,
                ty,
                annotate: None,
            },
        }
    }
    pub fn annotated(pos: Span, anno: Scheme, exp: Expr) -> Form {
        Form {
            node: exp,
            tag: FormTag {
                pos,
                ty: Scheme::slot(),
                annotate: Some(anno),
            },
        }
    }
    pub fn abs(pos: Span, params: Vec<VarDecl>, to: P<Form>) -> Form {
        Form {
            node: Expr::Abs(Lambda {
                                param: params,
                                body: to,
                            }),
            tag: FormTag {
                pos: pos,
                ty: Scheme::slot(),
                annotate: None,
            },
        }
    }
    pub fn span(&self) -> Span {
        self.tag.pos.clone()
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FormTag {
    /// Position in source
    pub pos: Span,
    /// Type of node
    pub ty: Scheme,
    /// Annotate type
    pub annotate: Option<Scheme>,
}

impl FormTag {
    pub fn set_type(&mut self, ty: Type) {
        self.ty = Scheme::Mono(ty);
    }

    pub fn set_scheme(&mut self, scm: Scheme) {
        self.ty = scm;
    }

    pub fn ref_type(&self) -> &Type {
        self.ty.body()
    }

    pub fn clone_type(&self) -> Type {
        self.ty.body().clone()
    }

    pub fn ref_scheme(&self) -> &Scheme {
        &self.ty
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(l: usize, c: usize) -> Span {
        Span { start: l, end: c }
    }
    pub fn union(&self, s: &Span) -> Span {
        Span {
            start: self.start,
            end: s.end
        }
    }
}



/// Represents a expression evalutated to a value
#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    /// Literial value
    Lit(Lit),
    /// Identifier (binding/definition)
    Var(Id),
    /// List (array)
    /// e.g. `[fuck, shit]`
    List(Vec<E>),
    /// Block (statement sequence)
    /// e.g. `{ print(fuck); print(shit); 1 }`
    Block(Vec<E>),
    /// Function apply
    /// `fuck(shit, 1)`
    Apply(E, Vec<E>),

    /// Abstruction (function)
    /// e.g. `(fuck, shit) -> fuck + shit`
    Abs(Lambda),

    /// Binary operator expression
    /// e.g. `fuck + shit`
    Binary(BinOp, E, E),
    /// Unary operator expression
    /// e.g. `!fuck`
    /// e.g. `-shit`
    Unary(UnOp, E),

    /// Let-in expression
    /// e.g. `let fuck = shit in fuck + 1`
    Let(VarDecl, E, E),
    /// Conditional expression
    /// e.g. `if (fuck == shit) 1 else 0`
    If(E, E, E),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Lambda {
    pub param: Vec<VarDecl>,
    pub body: E,
}
