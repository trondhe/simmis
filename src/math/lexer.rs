use crate::types::{Op, Token};

pub struct Lexer;

#[derive(thiserror::Error, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub enum LexerError {
    #[error("Invalid input: {0}")]
    InvalidInput(String),
}

impl Lexer {
    pub fn tokenize(input: &str) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();
        let mut count = 0;

        loop {
            count += 1;
            if count > 10_000 {
                return Err(LexerError::InvalidInput(
                    "Exceeded maximum tokenization steps, possible infinite loop".to_string(),
                ));
            }

            let Some(c) = chars.peek() else {
                break;
            };

            if c.is_whitespace() {
                chars.next();
                continue;
            }

            if c.is_ascii_digit() {
                tokens.push(parse_value(&mut chars)?);
                continue;
            }

            if *c == '(' {
                tokens.push(Token::ParOpen);
                chars.next();
                continue;
            }

            if *c == ')' {
                tokens.push(Token::ParClose);
                chars.next();
                continue;
            }

            if *c == '!' {
                tokens.push(Token::Factorial);
                chars.next();
                continue;
            }

            if *c == '+' || *c == '-' || *c == '*' || *c == '/' || *c == '^' || *c == '%' {
                match c {
                    '+' => tokens.push(Token::Op(Op::Add)),
                    '-' => tokens.push(Token::Op(Op::Sub)),
                    '*' => tokens.push(Token::Op(Op::Mul)),
                    '/' => tokens.push(Token::Op(Op::Div)),
                    '^' => tokens.push(Token::Op(Op::Pow)),
                    '%' => tokens.push(Token::Op(Op::Mod)),
                    _ => (),
                }
                chars.next();
                continue;
            }

            if c.is_ascii_alphabetic() {
                tokens.extend(parse_alphabetic(&mut chars)?);
                continue;
            }

            return Err(LexerError::InvalidInput(format!(
                "Unexpected character: {}",
                c
            )));
        }
        Ok(tokens)
    }
}

fn parse_value(chars: &mut std::iter::Peekable<std::str::Chars>) -> Result<Token, LexerError> {
    let mut value_str = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_ascii_digit() || c == '.' || c == ',' {
            value_str.push(c);
            chars.next();
        } else {
            break;
        }
    }

    match value_str.parse::<f32>() {
        Ok(value) => Ok(Token::Val(value)),
        Err(e) => Err(LexerError::InvalidInput(format!(
            "Failed to parse value: '{e}', with input '{value_str}'"
        ))),
    }
}

fn parse_alphabetic(
    chars: &mut std::iter::Peekable<std::str::Chars>,
) -> Result<Vec<Token>, LexerError> {
    let mut s = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_ascii_alphabetic() {
            s.push(c);
            chars.next();
        } else {
            break;
        }
    }

    use crate::types::{Const, Fun};
    Ok(match s.to_lowercase().as_str() {
        "sin" => vec![Token::Fun(Fun::Sin)],
        "cos" => vec![Token::Fun(Fun::Cos)],
        "tan" => vec![Token::Fun(Fun::Tan)],
        "log" => vec![Token::Fun(Fun::Log)],
        "sqrt" => vec![Token::Fun(Fun::Sqrt)],
        "pi" => vec![Token::Const(Const::Pi)],
        "e" => vec![Token::Const(Const::E)],
        _ => s.chars().map(|c| Token::Var(c.to_string())).collect(),
    })
}

#[cfg(test)]
mod test {
    use crate::types::{Const, Fun};

    use super::*;

    #[test]
    fn can_parse_value() {
        let input = "2";
        let value = parse_value(&mut input.chars().peekable());
        assert_eq!(value, Ok(Token::Val(2.0)));

        let input = "22";
        let value = parse_value(&mut input.chars().peekable());
        assert_eq!(value, Ok(Token::Val(22.0)));
        let input = "22.56";
        let value = parse_value(&mut input.chars().peekable());
        assert_eq!(value, Ok(Token::Val(22.56)));
    }

    #[test]
    fn parse_value_only_consumes_the_value() {
        let input = "2*a";
        let iter = &mut input.chars().peekable();
        let value = parse_value(iter);
        assert_eq!(value, Ok(Token::Val(2.0)));
        assert_eq!(iter.next(), Some('*'));
        assert_eq!(iter.next(), Some('a'));
    }

    #[test]
    fn lexing_basic() {
        let value = Lexer::tokenize("1+1");
        assert_eq!(
            value,
            Ok(vec![Token::Val(1.0), Token::Op(Op::Add), Token::Val(1.0),])
        );
    }

    #[test]
    fn lexing_all_operators() {
        let value = Lexer::tokenize("1+2-3*4/5^6%7");
        assert_eq!(
            value,
            Ok(vec![
                Token::Val(1.0),
                Token::Op(Op::Add),
                Token::Val(2.0),
                Token::Op(Op::Sub),
                Token::Val(3.0),
                Token::Op(Op::Mul),
                Token::Val(4.0),
                Token::Op(Op::Div),
                Token::Val(5.0),
                Token::Op(Op::Pow),
                Token::Val(6.0),
                Token::Op(Op::Mod),
                Token::Val(7.0),
            ])
        );
    }

    #[test]
    fn lexing_variables() {
        let value = Lexer::tokenize("x + y - z");
        assert_eq!(
            value,
            Ok(vec![
                Token::Var("x".to_string()),
                Token::Op(Op::Add),
                Token::Var("y".to_string()),
                Token::Op(Op::Sub),
                Token::Var("z".to_string()),
            ])
        );
    }

    #[test]
    fn lexing_parens() {
        let value = Lexer::tokenize("x(y+2)");
        assert_eq!(
            value,
            Ok(vec![
                Token::Var("x".to_string()),
                Token::ParOpen,
                Token::Var("y".to_string()),
                Token::Op(Op::Add),
                Token::Val(2.0),
                Token::ParClose,
            ])
        );
    }

    #[test]
    fn lexing_all_functions() {
        let value = Lexer::tokenize("sin(x) + cos(y) * tan(z) - log(10) / sqrt(4)");
        assert_eq!(
            value,
            Ok(vec![
                Token::Fun(Fun::Sin),
                Token::ParOpen,
                Token::Var("x".to_string()),
                Token::ParClose,
                Token::Op(Op::Add),
                Token::Fun(Fun::Cos),
                Token::ParOpen,
                Token::Var("y".to_string()),
                Token::ParClose,
                Token::Op(Op::Mul),
                Token::Fun(Fun::Tan),
                Token::ParOpen,
                Token::Var("z".to_string()),
                Token::ParClose,
                Token::Op(Op::Sub),
                Token::Fun(Fun::Log),
                Token::ParOpen,
                Token::Val(10.0),
                Token::ParClose,
                Token::Op(Op::Div),
                Token::Fun(Fun::Sqrt),
                Token::ParOpen,
                Token::Val(4.0),
                Token::ParClose,
            ])
        );
    }

    #[test]
    fn lexing_all_constants() {
        let value = Lexer::tokenize("pi * e");
        assert_eq!(
            value,
            Ok(vec![
                Token::Const(Const::Pi),
                Token::Op(Op::Mul),
                Token::Const(Const::E),
            ])
        );
    }

    #[test]
    fn lexing_complex() {
        let value = Lexer::tokenize("e^(2+sqrt(4x))-log(10^y)/z");
        assert_eq!(
            value,
            Ok(vec![
                Token::Const(Const::E),
                Token::Op(Op::Pow),
                Token::ParOpen,
                Token::Val(2.0),
                Token::Op(Op::Add),
                Token::Fun(Fun::Sqrt),
                Token::ParOpen,
                Token::Val(4.0),
                Token::Var("x".to_string()),
                Token::ParClose,
                Token::ParClose,
                Token::Op(Op::Sub),
                Token::Fun(Fun::Log),
                Token::ParOpen,
                Token::Val(10.0),
                Token::Op(Op::Pow),
                Token::Var("y".to_string()),
                Token::ParClose,
                Token::Op(Op::Div),
                Token::Var("z".to_string()),
            ])
        );
    }
}
