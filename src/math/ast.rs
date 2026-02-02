#![allow(dead_code, unused_variables, unused_assignments)]

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{
    math::lexer::Lexer,
    types::{BIND_RIGHT_BP, Const, Fun, OP_MUL_IMPLIED_BP, OP_UNARY_MINUS_BP, Op, Token, UnaryOp},
};

#[derive(Debug, Diagnostic, Error, PartialEq, Eq)]
#[error("Unexpected token '{found}'")]
pub struct UnexpectedTokenError {
    found: String,

    #[source_code]
    src: NamedSource<String>,
    #[label("{message}")]
    token: SourceSpan,

    message: String,
}

#[allow(unused)]
#[derive(Debug, Diagnostic, Error, PartialEq, Eq)]
pub enum Error {
    #[error("{0}")]
    #[diagnostic(transparent)]
    UnexpectedToken(Box<UnexpectedTokenError>),
}

/// Convert a token to its display string representation
fn token_to_string(token: &Token) -> String {
    match token {
        Token::Val(v) => format!("{}", v),
        Token::Var(v) => v.clone(),
        Token::Op(Op::Add) => "+".to_string(),
        Token::Op(Op::Sub) => "-".to_string(),
        Token::Op(Op::Mul) => "*".to_string(),
        Token::Op(Op::Div) => "/".to_string(),
        Token::Op(Op::Pow) => "^".to_string(),
        Token::Op(Op::Mod) => "%".to_string(),
        Token::ParOpen => "(".to_string(),
        Token::ParClose => ")".to_string(),
        Token::Factorial => "!".to_string(),
        Token::Fun(Fun::Sin) => "sin".to_string(),
        Token::Fun(Fun::Cos) => "cos".to_string(),
        Token::Fun(Fun::Tan) => "tan".to_string(),
        Token::Fun(Fun::Log) => "log".to_string(),
        Token::Fun(Fun::Sqrt) => "sqrt".to_string(),
        Token::Const(Const::Pi) => "pi".to_string(),
        Token::Const(Const::E) => "e".to_string(),
    }
}

#[derive(PartialEq, Debug)]
enum Exp {
    Var(String),
    Val(f32),
    Fun(Fun, Box<Exp>),
    Const(Const),
    Op(Op, Box<Exp>, Box<Exp>),
    Unary(UnaryOp, Box<Exp>),
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn next(&mut self) -> Option<Token> {
        if self.index < self.tokens.len() {
            let token = self.tokens[self.index].clone();
            self.index += 1;
            Some(token)
        } else {
            None
        }
    }

    fn peek(&self) -> Option<Token> {
        if self.index < self.tokens.len() {
            Some(self.tokens[self.index].clone())
        } else {
            None
        }
    }

    fn unexpected_token_error(&self, message: &str) -> Error {
        // Use index - 1 because next() has already consumed the token
        let error_index = self.index.saturating_sub(1);

        // Build display string and find error position
        let mut display = String::new();
        let mut error_start = 0;
        let mut error_len = 1;
        let mut found_token = String::new();

        for (i, token) in self.tokens.iter().enumerate() {
            let token_str = token_to_string(token);
            if i == error_index {
                error_start = display.len();
                error_len = token_str.len();
                found_token = token_str.clone();
            }
            display.push_str(&token_str);
        }

        Error::UnexpectedToken(Box::new(UnexpectedTokenError {
            found: found_token,
            src: NamedSource::new("input", display),
            token: (error_start, error_len).into(),
            message: message.to_string(),
        }))
    }
}

#[derive(PartialEq, Debug)]
struct Ast {
    root: Exp,
}

impl Ast {
    fn parse(input: &str) -> Result<Ast, Error> {
        let tokens = Lexer::tokenize(input).unwrap();
        let mut parser = Parser { tokens, index: 0 };
        Ok(Ast {
            root: parse_recurse(&mut parser, BIND_RIGHT_BP)?,
        })
    }

    fn ast(&self) -> &Exp {
        &self.root
    }
}

fn parse_atom(parser: &mut Parser) -> Result<Exp, Error> {
    let val = match parser.next().expect("Expected token") {
        Token::Val(v) => Exp::Val(v),
        Token::Var(v) => Exp::Var(v),
        Token::Const(c) => Exp::Const(c),
        Token::Op(Op::Sub) => {
            let rhs = parse_recurse(parser, OP_UNARY_MINUS_BP)?;
            Exp::Unary(UnaryOp::Neg, Box::new(rhs))
        }
        Token::Op(op) => {
            return Err(parser.unexpected_token_error(&format!("Unexpected operator {:?}", op)));
        }
        Token::ParOpen => {
            let exp = parse_recurse(parser, BIND_RIGHT_BP)?;
            assert!(
                parser.next() == Some(Token::ParClose),
                "Expected closing paren for opening paren"
            );
            exp
        }
        Token::ParClose => {
            return Err(parser.unexpected_token_error("Unexpected closing paren"));
        }
        Token::Fun(fun) => {
            if parser.next() != Some(Token::ParOpen) {
                return Err(parser
                    .unexpected_token_error("Function arguments must be enclosed in parentheses"));
            }
            let exp = parse_recurse(parser, BIND_RIGHT_BP)?;
            if parser.next() != Some(Token::ParClose) {
                return Err(parser.unexpected_token_error("Expected closing paren for function"));
            }
            Exp::Fun(fun, Box::new(exp))
        }
        Token::Factorial => {
            return Err(parser.unexpected_token_error("Unexpected factorial operator"));
        }
    };
    Ok(val)
}

fn parse_recurse(parser: &mut Parser, min_bp: u8) -> Result<Exp, Error> {
    let mut lhs = parse_atom(parser)?;

    loop {
        let Some(t) = parser.peek() else {
            break;
        };

        if t == Token::Factorial {
            parser.next();
            lhs = Exp::Unary(UnaryOp::Factorial, Box::new(lhs));
            continue;
        }

        // End of parentheses - opener will assert and consume closer
        if t == Token::ParClose {
            break;
        }

        // Implied multiplication
        match t {
            Token::Val(_) | Token::Var(_) | Token::Const(_) | Token::Fun(_) | Token::ParOpen => {
                if OP_MUL_IMPLIED_BP <= min_bp {
                    break; // Respect precedence for left-associativity
                }
                let rhs = parse_recurse(parser, OP_MUL_IMPLIED_BP)?;
                lhs = Exp::Op(Op::Mul, Box::new(lhs), Box::new(rhs));
                continue;
            }
            _ => (),
        }

        let Token::Op(op) = t else {
            break;
        };

        let bp = op.bp();
        if bp <= min_bp {
            break;
        }

        parser.next();
        // Power is special with right associativity => x^y^z = x^(y^z)
        let bp = if op == Op::Pow { bp - 1 } else { bp };
        let rhs = parse_recurse(parser, bp)?;
        lhs = Exp::Op(op, Box::new(lhs), Box::new(rhs));
    }
    Ok(lhs)
}

#[cfg(test)]
mod test {
    use super::*;

    fn parse_ok(input: &str) -> Exp {
        Ast::parse(input).expect("parse failed").root
    }

    fn parse_err(input: &str) -> Error {
        Ast::parse(input).expect_err("Expected parse to fail")
    }

    #[test]
    fn basic1() {
        let input = "2";
        let expected = Exp::Val(2.0);
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn basic2() {
        let input = "a";
        let expected = Exp::Var("a".to_string());
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn basic3() {
        let input = "1+2";
        let expected = Exp::Op(Op::Add, Box::new(Exp::Val(1.0)), Box::new(Exp::Val(2.0)));
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn basic4() {
        let input = "a+b";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Var("a".to_string())),
            Box::new(Exp::Var("b".to_string())),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn basic5() {
        let input = "1+2*3";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Val(1.0)),
            Box::new(Exp::Op(
                Op::Mul,
                Box::new(Exp::Val(2.0)),
                Box::new(Exp::Val(3.0)),
            )),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn basic6() {
        let input = "1*2+3";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Op(
                Op::Mul,
                Box::new(Exp::Val(1.0)),
                Box::new(Exp::Val(2.0)),
            )),
            Box::new(Exp::Val(3.0)),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn basic7() {
        let input = "pi";
        let expected = Exp::Const(Const::Pi);
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn parens1() {
        let input = "((2))";
        let expected = Exp::Val(2.0);
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn parens2() {
        let input = "(1)(2)";
        let expected = Exp::Op(Op::Mul, Box::new(Exp::Val(1.0)), Box::new(Exp::Val(2.0)));
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn parens3() {
        let input = "x(2)";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Var("x".to_string())),
            Box::new(Exp::Val(2.0)),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn parens4() {
        let input = "x(1+2)";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Var("x".to_string())),
            Box::new(Exp::Op(
                Op::Add,
                Box::new(Exp::Val(1.0)),
                Box::new(Exp::Val(2.0)),
            )),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn parens5() {
        let input = "(1+2)x";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Op(
                Op::Add,
                Box::new(Exp::Val(1.0)),
                Box::new(Exp::Val(2.0)),
            )),
            Box::new(Exp::Var("x".to_string())),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn parens6() {
        let input = "(3*2)+x";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Op(
                Op::Mul,
                Box::new(Exp::Val(3.0)),
                Box::new(Exp::Val(2.0)),
            )),
            Box::new(Exp::Var("x".to_string())),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn parens7() {
        let input = "(2(3y)+x)a";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Op(
                Op::Add,
                Box::new(Exp::Op(
                    Op::Mul,
                    Box::new(Exp::Val(2.0)),
                    Box::new(Exp::Op(
                        Op::Mul,
                        Box::new(Exp::Val(3.0)),
                        Box::new(Exp::Var("y".to_string())),
                    )),
                )),
                Box::new(Exp::Var("x".to_string())),
            )),
            Box::new(Exp::Var("a".to_string())),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn adv1() {
        let input = "log(10)^x+e^y";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Op(
                Op::Pow,
                Box::new(Exp::Fun(Fun::Log, Box::new(Exp::Val(10.0)))),
                Box::new(Exp::Var("x".to_string())),
            )),
            Box::new(Exp::Op(
                Op::Pow,
                Box::new(Exp::Const(Const::E)),
                Box::new(Exp::Var("y".to_string())),
            )),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn associativity1() {
        let input = "1+2+3";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Op(
                Op::Add,
                Box::new(Exp::Val(1.0)),
                Box::new(Exp::Val(2.0)),
            )),
            Box::new(Exp::Val(3.0)),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn associativity2() {
        let input = "1-2-3";
        let expected = Exp::Op(
            Op::Sub,
            Box::new(Exp::Op(
                Op::Sub,
                Box::new(Exp::Val(1.0)),
                Box::new(Exp::Val(2.0)),
            )),
            Box::new(Exp::Val(3.0)),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn associativity3() {
        let input = "12/3/2";
        let expected = Exp::Op(
            Op::Div,
            Box::new(Exp::Op(
                Op::Div,
                Box::new(Exp::Val(12.0)),
                Box::new(Exp::Val(3.0)),
            )),
            Box::new(Exp::Val(2.0)),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn associativity4() {
        let input = "2^3^4";
        let expected = Exp::Op(
            Op::Pow,
            Box::new(Exp::Val(2.0)),
            Box::new(Exp::Op(
                Op::Pow,
                Box::new(Exp::Val(3.0)),
                Box::new(Exp::Val(4.0)),
            )),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn implied_mul1() {
        let input = "2x";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Val(2.0)),
            Box::new(Exp::Var("x".to_string())),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn implied_mul2() {
        let input = "x2";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Var("x".to_string())),
            Box::new(Exp::Val(2.0)),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn implied_mul3() {
        let input = "xyz";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Op(
                Op::Mul,
                Box::new(Exp::Var("x".to_string())),
                Box::new(Exp::Var("y".to_string())),
            )),
            Box::new(Exp::Var("z".to_string())),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn implied_mul4() {
        let input = "2xy";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Op(
                Op::Mul,
                Box::new(Exp::Val(2.0)),
                Box::new(Exp::Var("x".to_string())),
            )),
            Box::new(Exp::Var("y".to_string())),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn implied_mul5() {
        let input = "2x+3y";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Op(
                Op::Mul,
                Box::new(Exp::Val(2.0)),
                Box::new(Exp::Var("x".to_string())),
            )),
            Box::new(Exp::Op(
                Op::Mul,
                Box::new(Exp::Val(3.0)),
                Box::new(Exp::Var("y".to_string())),
            )),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn implied_mul6() {
        let input = "2pi";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Val(2.0)),
            Box::new(Exp::Const(Const::Pi)),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn implied_mul7() {
        let input = "2x^3";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Val(2.0)),
            Box::new(Exp::Op(
                Op::Pow,
                Box::new(Exp::Var("x".to_string())),
                Box::new(Exp::Val(3.0)),
            )),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn function1() {
        let input = "sin(cos(x))";
        let expected = Exp::Fun(
            Fun::Sin,
            Box::new(Exp::Fun(Fun::Cos, Box::new(Exp::Var("x".to_string())))),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn function2() {
        let input = "sin(x)cos(y)";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Fun(Fun::Sin, Box::new(Exp::Var("x".to_string())))),
            Box::new(Exp::Fun(Fun::Cos, Box::new(Exp::Var("y".to_string())))),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn function3() {
        let input = "2sin(x)";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Val(2.0)),
            Box::new(Exp::Fun(Fun::Sin, Box::new(Exp::Var("x".to_string())))),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn function4() {
        let input = "sin(x)+cos(y)";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Fun(Fun::Sin, Box::new(Exp::Var("x".to_string())))),
            Box::new(Exp::Fun(Fun::Cos, Box::new(Exp::Var("y".to_string())))),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn unary1() {
        let input = "-5";
        let expected = Exp::Unary(UnaryOp::Neg, Box::new(Exp::Val(5.0)));
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn unary2() {
        let input = "-x";
        let expected = Exp::Unary(UnaryOp::Neg, Box::new(Exp::Var("x".to_string())));
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn unary3() {
        let input = "-(1+2)";
        let expected = Exp::Unary(
            UnaryOp::Neg,
            Box::new(Exp::Op(
                Op::Add,
                Box::new(Exp::Val(1.0)),
                Box::new(Exp::Val(2.0)),
            )),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn unary4() {
        let input = "1+-2";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Val(1.0)),
            Box::new(Exp::Unary(UnaryOp::Neg, Box::new(Exp::Val(2.0)))),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn unary5() {
        let input = "x*-y";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Var("x".to_string())),
            Box::new(Exp::Unary(
                UnaryOp::Neg,
                Box::new(Exp::Var("y".to_string())),
            )),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn unary6() {
        let input = "5!";
        let expected = Exp::Unary(UnaryOp::Factorial, Box::new(Exp::Val(5.0)));
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn unary7() {
        let input = "(2+3)!";
        let expected = Exp::Unary(
            UnaryOp::Factorial,
            Box::new(Exp::Op(
                Op::Add,
                Box::new(Exp::Val(2.0)),
                Box::new(Exp::Val(3.0)),
            )),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn unary8() {
        let input = "2*3!+1";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Op(
                Op::Mul,
                Box::new(Exp::Val(2.0)),
                Box::new(Exp::Unary(UnaryOp::Factorial, Box::new(Exp::Val(3.0)))),
            )),
            Box::new(Exp::Val(1.0)),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn unary9() {
        let input = "-sin(x)";
        let expected = Exp::Unary(
            UnaryOp::Neg,
            Box::new(Exp::Fun(Fun::Sin, Box::new(Exp::Var("x".to_string())))),
        );
        assert_eq!(parse_ok(input), expected);
    }

    #[test]
    fn error1() {
        let input = "*2*3!)+1";
        let err = parse_err(input);
        let report = miette::Report::new(err);
        assert!(format!("{report}").contains("Unexpected token '*'"));
        assert!(format!("{report:?}").contains("Unexpected operator Mul"));
    }
}
