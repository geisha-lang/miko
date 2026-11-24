use crate::utils::*;
use crate::types::*;

#[derive(Debug, PartialEq, Clone)]
pub struct VarDecl(pub Id, pub Scheme);

impl VarDecl {
    pub fn name(&self) -> Id {
        self.0
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Lit {
    Int(i32),
    Float(f64),
    Char(u8),
    Str(String),
    Bool(bool),
    Unit,
}

impl Lit {
    pub fn lit_type(&self) -> Type {
        match *self {
            Lit::Int(_) => Type::Con("Int".to_string()),
            Lit::Float(_) => Type::Con("Float".to_string()),
            Lit::Char(_) => Type::Con("Char".to_string()),
            Lit::Str(_) => Type::Con("String".to_string()),
            Lit::Bool(_) => Type::Con("Bool".to_string()),
            Lit::Unit => Type::Void,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum BinOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Rem,
    /// `&&`
    And,
    /// `||`
    Or,
    /// `^`
    BitXor,
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
    /// `==`
    Eq,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `!=`
    Ne,
    /// `>=`
    Ge,
    /// `>`
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
            "&&" => And,
            "||" => Or,
            _ => unreachable!("Unknown binary operator: {}", op_str),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub enum UnOp {
    /// `!` logical NOT
    Not,
    /// `-` arithmetic negation
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
