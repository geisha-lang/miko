use std::string::*;

use std::collections::HashMap;
use std::ops::Deref;
use std::ops::DerefMut;

use std::iter::IntoIterator;
use std::iter::DoubleEndedIterator;

use utils::*;
use types::*;


#[derive(Debug, PartialEq, Clone)]
pub struct VarDecl(pub Id, pub Scheme);

impl VarDecl {
    pub fn name(&self) -> Id {
        self.0.clone()
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Lit {
    Int(i32),
    Float(f64),
    Str(String),
    Bool(bool),
}

impl Lit {
    pub fn lit_type(&self) -> Type {
        use self::Lit::*;
        let ty_str = match *self {
            Lit::Int(_) => "Int",
            Lit::Float(_) => "Float",
            Lit::Str(_) => "String",
            Lit::Bool(_) => "Bool",
        };

        Type::Con(ty_str.to_string())
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum BinOp {
    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (modulus)
    Rem,
    /// The `&&` operator (logical and)
    And,
    /// The `||` operator (logical or)
    Or,
    /// The `^` operator (bitwise xor)
    BitXor,
    /// The `&` operator (bitwise and)
    BitAnd,
    /// The `|` operator (bitwise or)
    BitOr,
    /// The `<<` operator (shift left)
    Shl,
    /// The `>>` operator (shift right)
    Shr,
    /// The `==` operator (equality)
    Eq,
    /// The `<` operator (less than)
    Lt,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `!=` operator (not equal to)
    Ne,
    /// The `>=` operator (greater than or equal to)
    Ge,
    /// The `>` operator (greater than)
    Gt,
    /// `.`
    Dot,
}
impl BinOp {
    pub fn as_str(&self) -> &'static str {
        use self::BinOp::*;
        match *self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Rem => "%",
            And => "&&",
            Or => "||",
            BitXor => "^",
            BitAnd => "&",
            BitOr => "|",
            Shl => "<<",
            Shr => ">>",
            Eq => "==",
            Lt => "<",
            Le => "<=",
            Ne => "!=",
            Ge => ">=",
            Gt => ">",
            Dot => ".",
        }
    }
    pub fn take(op_str: &str) -> BinOp {
        use self::BinOp::*;
        match op_str {
            "+" => Add,
            "-" => Sub,
            "*" => Mul,
            "/" => Div,
            "%" => Rem,
            "<" => Lt,
            "<=" => Le,
            "!=" => Ne,
            ">=" => Ge,
            ">" => Gt,
            "==" => Eq,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum UnOp {
    /// The `!` operator for logical inversion
    Not,
    /// The `-` operator for negation
    Neg,
}

impl UnOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            UnOp::Not => "!",
            UnOp::Neg => "-",
        }
    }
}
