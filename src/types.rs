#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
}

impl Op {
    pub fn bp(&self) -> u8 {
        match self {
            Op::Add | Op::Sub => 1,
            Op::Mul | Op::Div | Op::Mod => 2,
            Op::Pow => 3,
        }
    }
}

pub const OP_MUL_IMPLIED_BP: u8 = 2; // Implied multiplication has the same precedence as normal multiplication
pub const OP_UNARY_MINUS_BP: u8 = u8::MAX; // Unary minus always binds to the next atom
pub const BIND_RIGHT_BP: u8 = 0; // Used for always bind right constructs like functions and parantheses

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum UnaryOp {
    Neg,
    Factorial,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Token {
    Val(f32),
    Op(Op),
    ParOpen,
    ParClose,
    Var(String),
    Fun(Fun),
    Const(Const),
    Factorial,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Fun {
    Sin,
    Cos,
    Tan,
    Log,
    Sqrt,
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Const {
    Pi,
    E,
}
