#![allow(dead_code, unused_variables)]

use crate::{
    math::lexer::Lexer,
    types::{BIND_RIGHT_BP, Const, Fun, OP_MUL_IMPLIED_BP, OP_UNARY_MINUS_BP, Op, Token, UnaryOp},
};

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
}

#[derive(PartialEq, Debug)]
struct Ast {
    root: Exp,
}

impl Ast {
    fn parse(input: &str) -> Ast {
        let tokens = Lexer::tokenize(input).unwrap();
        let mut parser = Parser { tokens, index: 0 };
        Ast {
            root: parse_recurse(&mut parser, BIND_RIGHT_BP),
        }
    }

    fn ast(&self) -> &Exp {
        &self.root
    }
}

fn parse_atom(parser: &mut Parser) -> Exp {
    match parser.next().expect("Expected token") {
        Token::Val(v) => Exp::Val(v),
        Token::Var(v) => Exp::Var(v),
        Token::Const(c) => Exp::Const(c),
        Token::Op(Op::Sub) => {
            let rhs = parse_recurse(parser, OP_UNARY_MINUS_BP);
            Exp::Unary(UnaryOp::Neg, Box::new(rhs))
        }
        Token::Op(op) => panic!("Unexpected operator {:?}", op),
        Token::ParOpen => {
            let exp = parse_recurse(parser, BIND_RIGHT_BP);
            assert!(
                parser.next() == Some(Token::ParClose),
                "Expected closing paren for opening paren"
            );
            exp
        }
        Token::ParClose => panic!("Unexpected closing paren"),
        Token::Fun(fun) => {
            assert!(
                parser.next() == Some(Token::ParOpen),
                "Function arguments must be enclosed in parentheses"
            );
            let exp = parse_recurse(parser, BIND_RIGHT_BP);
            assert!(
                parser.next() == Some(Token::ParClose),
                "Expected closing paren for function"
            );
            Exp::Fun(fun, Box::new(exp))
        }
        Token::Factorial => panic!("Unexpected factorial operator"),
    }
}

fn parse_recurse(parser: &mut Parser, min_bp: u8) -> Exp {
    let mut lhs = parse_atom(parser);

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
                let rhs = parse_recurse(parser, OP_MUL_IMPLIED_BP);
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
        let rhs = parse_recurse(parser, bp);
        lhs = Exp::Op(op, Box::new(lhs), Box::new(rhs));
    }
    lhs
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic1() {
        let input = "2";
        let expected = Exp::Val(2.0);
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn basic2() {
        let input = "a";
        let expected = Exp::Var("a".to_string());
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn basic3() {
        let input = "1+2";
        let expected = Exp::Op(Op::Add, Box::new(Exp::Val(1.0)), Box::new(Exp::Val(2.0)));
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn basic4() {
        let input = "a+b";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Var("a".to_string())),
            Box::new(Exp::Var("b".to_string())),
        );
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn basic7() {
        let input = "pi";
        let expected = Exp::Const(Const::Pi);
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn parens1() {
        let input = "((2))";
        let expected = Exp::Val(2.0);
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn parens2() {
        let input = "(1)(2)";
        let expected = Exp::Op(Op::Mul, Box::new(Exp::Val(1.0)), Box::new(Exp::Val(2.0)));
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn parens3() {
        let input = "x(2)";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Var("x".to_string())),
            Box::new(Exp::Val(2.0)),
        );
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn implied_mul1() {
        let input = "2x";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Val(2.0)),
            Box::new(Exp::Var("x".to_string())),
        );
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn implied_mul2() {
        let input = "x2";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Var("x".to_string())),
            Box::new(Exp::Val(2.0)),
        );
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn implied_mul6() {
        let input = "2pi";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Val(2.0)),
            Box::new(Exp::Const(Const::Pi)),
        );
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn function1() {
        let input = "sin(cos(x))";
        let expected = Exp::Fun(
            Fun::Sin,
            Box::new(Exp::Fun(Fun::Cos, Box::new(Exp::Var("x".to_string())))),
        );
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn function2() {
        let input = "sin(x)cos(y)";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Fun(Fun::Sin, Box::new(Exp::Var("x".to_string())))),
            Box::new(Exp::Fun(Fun::Cos, Box::new(Exp::Var("y".to_string())))),
        );
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn function3() {
        let input = "2sin(x)";
        let expected = Exp::Op(
            Op::Mul,
            Box::new(Exp::Val(2.0)),
            Box::new(Exp::Fun(Fun::Sin, Box::new(Exp::Var("x".to_string())))),
        );
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn function4() {
        let input = "sin(x)+cos(y)";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Fun(Fun::Sin, Box::new(Exp::Var("x".to_string())))),
            Box::new(Exp::Fun(Fun::Cos, Box::new(Exp::Var("y".to_string())))),
        );
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn unary1() {
        let input = "-5";
        let expected = Exp::Unary(UnaryOp::Neg, Box::new(Exp::Val(5.0)));
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn unary2() {
        let input = "-x";
        let expected = Exp::Unary(UnaryOp::Neg, Box::new(Exp::Var("x".to_string())));
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn unary4() {
        let input = "1+-2";
        let expected = Exp::Op(
            Op::Add,
            Box::new(Exp::Val(1.0)),
            Box::new(Exp::Unary(UnaryOp::Neg, Box::new(Exp::Val(2.0)))),
        );
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn unary6() {
        let input = "5!";
        let expected = Exp::Unary(UnaryOp::Factorial, Box::new(Exp::Val(5.0)));
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
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
        assert_eq!(Ast::parse(input).ast(), &expected);
    }

    #[test]
    fn unary9() {
        let input = "-sin(x)";
        let expected = Exp::Unary(
            UnaryOp::Neg,
            Box::new(Exp::Fun(Fun::Sin, Box::new(Exp::Var("x".to_string())))),
        );
        assert_eq!(Ast::parse(input).ast(), &expected);
    }
}
