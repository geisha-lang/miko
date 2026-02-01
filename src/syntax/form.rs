use std::string::*;

use std::ops::Deref;
use std::ops::DerefMut;

use std::iter::IntoIterator;
use std::iter::DoubleEndedIterator;

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
    /// Private (default) - only accessible within the same module
    #[default]
    Private,
    /// Public - accessible from other modules
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
    /// The name being imported
    pub name: Id,
    /// Optional alias (e.g., `use foo.bar as baz`)
    pub alias: Option<Id>,
}

impl UseSpec {
    pub fn new(name: Id) -> Self {
        UseSpec { name, alias: None }
    }

    pub fn with_alias(name: Id, alias: Id) -> Self {
        UseSpec { name, alias: Some(alias) }
    }

    /// Get the local name (alias if present, otherwise the original name)
    pub fn local_name(&self) -> Id {
        self.alias.unwrap_or(self.name)
    }
}

/// An import/use statement
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum UseItem {
    /// Import a single item: `use foo.bar`
    Single(ModulePath),
    /// Import specific items: `use foo.{bar, baz}`
    Multiple(ModulePath, Vec<UseSpec>),
    /// Import all public items: `use foo.*` (via `open`)
    Glob(ModulePath),
    /// Import with alias: `use foo.bar as baz`
    Alias(ModulePath, Id),
}

impl UseItem {
    /// Get the module path being imported from
    pub fn module_path(&self) -> &ModulePath {
        match self {
            UseItem::Single(p) | UseItem::Multiple(p, _) | UseItem::Glob(p) | UseItem::Alias(p, _) => p,
        }
    }
}

/// An inline submodule definition (with braces)
#[derive(Clone, PartialEq, Debug)]
pub struct ModuleDef {
    /// The module name
    pub name: Id,
    /// Items within this module
    pub items: Vec<ModuleItem>,
    /// Position in source
    pub pos: Span,
}

/// A single item within a module
#[derive(Clone, PartialEq, Debug)]
pub enum ModuleItem {
    /// A function/value definition: `pub def foo(x) = x`
    Def(Visibility, Def),
    /// An import statement: `pub use foo.bar`
    Use(Visibility, UseItem),
    /// An inline submodule: `pub mod utils { ... }`
    SubModule(Visibility, Box<ModuleDef>),
    /// A submodule declaration (file-based): `pub mod list`
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
    /// Module path derived from file path (e.g., collections/list.gs -> collections.list)
    pub path: ModulePath,
    /// Items within the module
    pub items: Vec<ModuleItem>,
}

/// A pattern for pattern matching
#[derive(Clone, PartialEq, Debug)]
pub enum Pattern {
    /// Variable binding: x
    Var(Id),
    /// Ignore pattern: _
    Wildcard,
    /// Literal pattern: 42, "hello", true
    Lit(Lit),
    /// Constructor pattern: Cons(x, xs) or Nil (unit constructor)
    Constructor(Name, Vec<Pattern>),
}

/// A single arm in a match expression
#[derive(Clone, PartialEq, Debug)]
pub struct MatchArm {
    /// The pattern to match against
    pub pattern: Pattern,
    /// Optional guard condition
    pub guard: Option<E>,
    /// The body expression if the pattern matches
    pub body: E,
}

/// Represents a top level definition,
/// `def` or `data` or `type`
#[derive(Clone, PartialEq, Debug)]
pub struct Def {
    pub ident: Id,
    pub node: Item,
    pub pos: Span,
    /// Visibility modifier (default: Private)
    pub visibility: Visibility,
}

impl Def {
    /// Get ident as `&str`
    pub fn name(&self) -> Id {
        self.ident.clone()
    }

    /// Create a definition node define a form (value) with default (private) visibility.
    pub fn value(pos: Span, name: Id, body: E) -> Def {
        Def {
              ident: name,
              node: Item::Form(body),
              pos,
              visibility: Visibility::Private,
        }
    }

    /// Create a definition node with explicit visibility.
    pub fn value_with_visibility(pos: Span, name: Id, body: E, visibility: Visibility) -> Def {
        Def {
              ident: name,
              node: Item::Form(body),
              pos,
              visibility,
        }
    }

    /// Check if this definition is public
    pub fn is_public(&self) -> bool {
        self.visibility.is_public()
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
    /// Concept (typeclass) definition
    /// e.g., `concept Eq a { eq: a * a -> Bool }`
    Concept {
        /// Type parameters for the concept (e.g., "a" in "Eq a")
        type_params: Vec<Id>,
        /// Superclass constraints (e.g., "Ord a" requires "Eq a")
        superclasses: Vec<TypeConstraint>,
        /// Method signatures
        methods: Vec<MethodDecl>,
    },
    /// Instance of a concept for a type
    /// e.g., `instance Eq Int { def eq(x, y) = x == y }`
    Instance {
        /// Name of the concept being implemented
        concept_name: Name,
        /// Type arguments (e.g., "Int" in "Eq Int")
        type_args: Vec<Type>,
        /// Context constraints (e.g., "Eq a" in "instance (Eq a) => Eq (List a)")
        constraints: Vec<TypeConstraint>,
        /// Method implementations
        methods: Vec<MethodImpl>,
    },
}

/// A type constraint (e.g., "Eq a")
#[derive(Clone, PartialEq, Debug)]
pub struct TypeConstraint {
    /// Name of the concept (e.g., "Eq")
    pub concept: Name,
    /// Type variable (e.g., "a")
    pub type_var: Name,
}

/// A method declaration in a concept
#[derive(Clone, PartialEq, Debug)]
pub struct MethodDecl {
    /// Method name
    pub name: Id,
    /// Method type signature
    pub ty: Scheme,
}

/// A method implementation in an instance
#[derive(Clone, PartialEq, Debug)]
pub struct MethodImpl {
    /// Method name
    pub name: Id,
    /// Implementation (as a Form expression, usually a lambda)
    pub body: E,
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
    /// Qualified name access (e.g., `collections.list.length`)
    QualifiedVar(ModulePath),
    /// List (array)
    /// e.g. `[foo, bar]`
    List(Vec<E>),
    /// Block (statement sequence)
    /// e.g. `{ print(foo); print(bar); 1 }`
    Block(Vec<E>),
    /// Function apply
    /// `foo(bar, 1)`
    Apply(E, Vec<E>),

    /// Abstruction (function)
    /// e.g. `(a, b) -> a + b`
    Abs(Lambda),

    /// Binary operator expression
    /// e.g. `a + b`
    Binary(BinOp, E, E),
    /// Unary operator expression
    /// e.g. `!a`
    /// e.g. `-b`
    Unary(UnOp, E),

    /// Let-in expression
    /// e.g. `let x = y in x + 1`
    Let(VarDecl, E, E),
    /// Conditional expression
    /// e.g. `if (x == y) 1 else 0`
    If(E, E, E),

    /// Pattern matching expression
    /// e.g. `match x { Nil -> 0, Cons(h, t) -> 1 }`
    Match(E, Vec<MatchArm>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Lambda {
    pub param: Vec<VarDecl>,
    pub body: E,
}
