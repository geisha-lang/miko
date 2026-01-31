use crate::types::*;
use crate::syntax::form::*;
use crate::internal::*;

#[derive(Clone, PartialEq, Debug)]
pub enum TypeError {
    NotInScope(String, Span),
    MisMatch(Type, Type),
    HighRank(Type, Span),
    UnknownOperator(BinOp, Span),
}
