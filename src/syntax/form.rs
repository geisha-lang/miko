use std::ops::{Deref, DerefMut};
use crate::utils::*;
use crate::types::*;
use crate::internal::*;

pub type E = P<Form>;

// ============================================================================
// Module System Types
// ============================================================================

/// A module path representing a sequence of identifiers
/// e.g., `collections.list.length` -> segments = ["collections", "list", "length"]
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ModulePath {
    pub segments: Vec<Id>,
}

impl ModulePath {
    pub fn new(segments: Vec<Id>) -> Self {
        ModulePath { segments }
    }

    pub fn single(id: Id) -> Self {
        ModulePath { segments: vec![id] }
    }

    pub fn is_qualified(&self) -> bool {
        self.segments.len() > 1
    }

    /// Get the last segment (the actual name being referenced)
    pub fn name(&self) -> Option<Id> {
        self.segments.last().copied()
    }

    /// Get the module part (all segments except the last)
    pub fn module_path(&self) -> Vec<Id> {
        if self.segments.len() > 1 {
            self.segments[..self.segments.len() - 1].to_vec()
        } else {
            vec![]
        }
    }

    /// Convert to a string representation using dots
    pub fn to_string_with(&self, interner: &Interner) -> String {
        self.segments
            .iter()
            .map(|id| interner.trace(*id))
            .collect::<Vec<_>>()
            .join(".")
    }
}

/// Visibility modifier for definitions
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum Visibility {
    #[default]
    Private,
    Public,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        matches!(self, Visibility::Public)
    }
}

/// Specification for a single import item
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct UseSpec {
    pub name: Id,
    pub alias: Option<Id>,
}

impl UseSpec {
    pub fn new(name: Id) -> Self {
        UseSpec { name, alias: None }
    }

    pub fn with_alias(name: Id, alias: Id) -> Self {
        UseSpec { name, alias: Some(alias) }
    }

    pub fn local_name(&self) -> Id {
        self.alias.unwrap_or(self.name)
    }
}

/// An import/use statement
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum UseItem {
    /// `use foo.bar`
    Single(ModulePath),
    /// `use foo.{bar, baz}`
    Multiple(ModulePath, Vec<UseSpec>),
    /// `open foo`
    Glob(ModulePath),
    /// `use foo.bar as baz`
    Alias(ModulePath, Id),
}

impl UseItem {
    pub fn module_path(&self) -> &ModulePath {
        match self {
            UseItem::Single(p)
            | UseItem::Multiple(p, _)
            | UseItem::Glob(p)
            | UseItem::Alias(p, _) => p,
        }
    }
}

/// An inline submodule definition (with braces)
#[derive(Clone, PartialEq, Debug)]
pub struct ModuleDef {
    pub name: Id,
    pub items: Vec<ModuleItem>,
    pub pos: Span,
}

/// A single item within a module
#[derive(Clone, PartialEq, Debug)]
pub enum ModuleItem {
    Def(Visibility, Def),
    Use(Visibility, UseItem),
    SubModule(Visibility, Box<ModuleDef>),
    ModDecl(Visibility, Id),
}

impl ModuleItem {
    pub fn visibility(&self) -> Visibility {
        match self {
            ModuleItem::Def(v, _)
            | ModuleItem::Use(v, _)
            | ModuleItem::SubModule(v, _)
            | ModuleItem::ModDecl(v, _) => *v,
        }
    }

    pub fn is_public(&self) -> bool {
        self.visibility().is_public()
    }
}

/// A parsed file representing a file-level module
#[derive(Clone, PartialEq, Debug)]
pub struct FileModule {
    pub path: ModulePath,
    pub items: Vec<ModuleItem>,
}

/// A pattern for pattern matching
#[derive(Clone, PartialEq, Debug)]
pub enum Pattern {
    /// Variable binding: `x`
    Var(Id),
    /// Ignore pattern: `_`
    Wildcard,
    /// Literal pattern: `42`, `"hello"`, `'a'`, `true`, `()`
    Lit(Lit),
    /// First-class tuple pattern: `(x, y)`
    Tuple(Vec<Pattern>),
    /// Positional constructor pattern: `Cons(x, xs)` or `Nil`
    Constructor(Name, Vec<Pattern>),
    /// Named struct constructor pattern: `Circle { radius: r }` or `Circle { radius }`
    StructConstructor(Name, Vec<(Name, Pattern)>),
}

/// A single arm in a match expression
#[derive(Clone, PartialEq, Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<E>,
    pub body: E,
}

/// Represents a top level definition (`def`, `data`, `type`, `concept`, `instance`)
#[derive(Clone, PartialEq, Debug)]
pub struct Def {
    pub ident: Id,
    pub node: Item,
    pub pos: Span,
    pub visibility: Visibility,
}

impl Def {
    pub fn name(&self) -> Id {
        self.ident
    }

    pub fn value(pos: Span, name: Id, body: E) -> Def {
        Def {
            ident: name,
            node: Item::Form(body),
            pos,
            visibility: Visibility::Private,
        }
    }

    pub fn value_with_visibility(pos: Span, name: Id, body: E, visibility: Visibility) -> Def {
        Def {
            ident: name,
            node: Item::Form(body),
            pos,
            visibility,
        }
    }

    pub fn is_public(&self) -> bool {
        self.visibility.is_public()
    }

    pub fn is_form(&self) -> bool {
        matches!(self.node, Item::Form(_))
    }

    pub fn form_annot(&self) -> Option<&Scheme> {
        match self.node {
            Item::Form(ref f) => f.deref().tag.annotate.as_ref(),
            _ => None,
        }
    }

    pub fn set_form_scheme(&mut self, scm: Scheme) {
        match self.node {
            Item::Form(ref mut f) => {
                (*f).deref_mut().tag.set_scheme(scm);
            }
            _ => panic!("Set scheme to a non-form definition"),
        }
    }

    pub fn form_type(&self) -> &Scheme {
        match self.node {
            Item::Form(ref f) => f.tag.ref_scheme(),
            _ => panic!("Get scheme from a non-form definition"),
        }
    }

    pub fn form_body_mut(&mut self) -> &mut Form {
        match self.node {
            Item::Form(ref mut f) => (*f).deref_mut(),
            _ => panic!("Get form from a non-form definition"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Item {
    Form(E),
    Extern {
        link_name: Name,
        ty: Scheme,
    },
    Alias(Vec<Id>, P<Scheme>),
    Alg(Vec<Id>, Vec<Variant>),
    Concept {
        type_params: Vec<Id>,
        superclasses: Vec<TypeConstraint>,
        methods: Vec<MethodDecl>,
    },
    Instance {
        concept_name: Name,
        type_args: Vec<Type>,
        constraints: Vec<TypeConstraint>,
        methods: Vec<MethodImpl>,
    },
}

/// A type constraint (e.g., `Eq a`)
#[derive(Clone, PartialEq, Debug)]
pub struct TypeConstraint {
    pub concept: Name,
    pub type_var: Name,
}

/// A method declaration in a concept
#[derive(Clone, PartialEq, Debug)]
pub struct MethodDecl {
    pub name: Id,
    pub ty: Scheme,
}

/// A method implementation in an instance
#[derive(Clone, PartialEq, Debug)]
pub struct MethodImpl {
    pub name: Id,
    pub body: E,
}

/// Represents an expression with source span and type metadata
#[derive(Clone, PartialEq, Debug)]
pub struct Form {
    pub node: Expr,
    pub tag: FormTag,
}

impl Form {
    pub fn new(pos: Span, exp: Expr) -> Form {
        Form {
            node: exp,
            tag: FormTag {
                pos,
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
                ret_ty: None,
                body: to,
            }),
            tag: FormTag {
                pos,
                ty: Scheme::slot(),
                annotate: None,
            },
        }
    }

    pub fn span(&self) -> Span {
        self.tag.pos
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FormTag {
    pub pos: Span,
    pub ty: Scheme,
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

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }

    pub fn union(&self, s: &Span) -> Span {
        Span {
            start: self.start.min(s.start),
            end: self.end.max(s.end),
        }
    }
}

/// Expression AST
#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    /// Literal value (`42`, `3.14`, `'a'`, `"hi"`, `true`, `()`)
    Lit(Lit),
    /// Identifier (`x`, `add`)
    Var(Id),
    /// Qualified identifier (`math.add`)
    QualifiedVar(ModulePath),
    /// First-class tuple expression (`(1, "a")`)
    Tuple(Vec<E>),
    /// Struct variant construction (`Circle { radius: 2.0 }`)
    StructInit(Name, Vec<(Name, E)>),
    /// List literal (`[1, 2, 3]`)
    List(Vec<E>),
    /// Block sequence (`{ e1; e2; e3 }`)
    Block(Vec<E>),
    /// Function/constructor application (`f(x, y)`)
    Apply(E, Vec<E>),
    /// Lambda abstraction (`(x, y) -> x + y`)
    Abs(Lambda),
    /// Binary operator (`a + b`)
    Binary(BinOp, E, E),
    /// Unary operator (`!a`, `-b`)
    Unary(UnOp, E),
    /// Local let binding (`let x = v in body`)
    Let(VarDecl, E, E),
    /// Conditional (`if (cond) then_e else else_e`)
    If(E, E, E),
    /// Imperative loop (`while (cond) body_e`)
    While(E, E),
    /// Pattern match (`match e { pat -> body, ... }`)
    Match(E, Vec<MatchArm>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Lambda {
    pub param: Vec<VarDecl>,
    pub ret_ty: Option<Type>,
    pub body: E,
}
