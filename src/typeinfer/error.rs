use crate::types::*;
use crate::syntax::form::*;
use crate::internal::*;

#[derive(Clone, PartialEq, Debug)]
pub enum TypeError {
    NotInScope(String, Span),
    MisMatch(Type, Type, Span),
    InfiniteType(String, Type, Span),
    ArityMismatch {
        expected: usize,
        actual: usize,
        span: Span,
    },
    HighRank(Type, Span),
    UnknownOperator(BinOp, Span),
    PatternError(String, Span),
    NonExhaustiveMatch(String, Span),
    NoInstance {
        concept: String,
        ty: Type,
        span: Span,
    },
    TypeAliasError(String, Span),
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::NotInScope(name, _) => write!(f, "Identifier '{}' is not in scope", name),
            TypeError::MisMatch(expected, actual, _) => {
                write!(f, "Type mismatch: cannot unify '{}' with '{}'", expected, actual)
            }
            TypeError::InfiniteType(var, ty, _) => {
                write!(f, "Occurs check failed: infinite type '{} ~ {}'", var, ty)
            }
            TypeError::ArityMismatch { expected, actual, .. } => {
                write!(f, "Function arity mismatch: expected {} argument(s), got {}", expected, actual)
            }
            TypeError::HighRank(ty, _) => write!(f, "Higher-rank type not supported: {}", ty),
            TypeError::UnknownOperator(op, _) => write!(f, "Unknown operator '{}'", op.as_str()),
            TypeError::PatternError(msg, _) => write!(f, "Pattern error: {}", msg),
            TypeError::NonExhaustiveMatch(missing, _) => {
                write!(f, "Non-exhaustive match: missing case '{}'", missing)
            }
            TypeError::NoInstance { concept, ty, .. } => {
                let display_concept = match concept.as_str() {
                    "$Num" => "Num (Int or Float)",
                    "$EqOp" => "Eq (Int, Float, Bool, Char, or String)",
                    "$OrdOp" => "Ord (Int, Float, Char, or String)",
                    other => other,
                };
                write!(f, "Type '{}' does not satisfy concept '{}'", ty, display_concept)
            }
            TypeError::TypeAliasError(msg, _) => write!(f, "Type alias error: {}", msg),
        }
    }
}

impl TypeError {
    pub fn span(&self) -> Span {
        match self {
            TypeError::NotInScope(_, s)
            | TypeError::MisMatch(_, _, s)
            | TypeError::InfiniteType(_, _, s)
            | TypeError::ArityMismatch { span: s, .. }
            | TypeError::HighRank(_, s)
            | TypeError::UnknownOperator(_, s)
            | TypeError::PatternError(_, s)
            | TypeError::NonExhaustiveMatch(_, s)
            | TypeError::NoInstance { span: s, .. }
            | TypeError::TypeAliasError(_, s) => *s,
        }
    }
}
