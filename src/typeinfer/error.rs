use types::*;
use syntax::form::*;
use internal::*;

#[derive(Clone, PartialEq, Debug)]
pub enum TypeError {
    NotInScope(Form),
    MisMatch(Type, Type),
    HighRank(Form),
    UnknownOperator(BinOp, Span),
}
