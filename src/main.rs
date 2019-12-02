fn main() {
    println!("Hello, world!");
}

pub struct RecordIdent(String);

// purescript expr with more members/info than needed
#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Int(String),            // 1
    Number(String),         // 1.0
    NegativeInt(String),    // (-1)
    NegativeNumber(String), // (-1.0)
    Quoted(String),         // "Hi"
    Space,                  // " "
    Newline,                // "\n"
    Symbol,                 // asdf1324
    LeftBrace,              // {
    RightBrace,             // }
    LeftParens,             // (
    RightParens,            // )
    LeftBracket,            // [
    RightBracket,           // ]
}

// fuck
#[allow(unused_imports)]
pub mod parser {
    use super::Token;
    use nom;
    use nom::branch::*;
    use nom::bytes::complete::*;
    use nom::character::complete::*;
    use nom::combinator::*;
    use nom::multi::*;
    use nom::number::complete::*;
    use nom::sequence::*;
    use std::fmt::Display;

    pub type Result<'a, A> = nom::IResult<&'a str, A>;

    pub type TokenResult<'a> = Result<'a, Token>;

    pub fn parse_expr(s: &str) -> TokenResult<'_> {
        alt((
            parse_negative_number,
            parse_negative_int,
            parse_number,
            parse_int,
            parse_string,
            parse_newline,
            parse_space,
            parse_l_brace,
            parse_r_brace,
            parse_l_parens,
            parse_r_parens,
            parse_l_bracket,
            parse_r_bracket,
        ))(s)
    }

    fn join_tuple2<A: Display, B: Display>(tuple: (A, B)) -> String {
        let (a, b) = tuple;
        format!("{}{}", a, b)
    }

    fn join_tuple3<A: Display, B: Display, C: Display>(tuple: (A, B, C)) -> String {
        let (a, b, c) = tuple;
        format!("{}{}{}", a, b, c)
    }

    macro_rules! parse_literal {
        ($name: ident, $char: expr, $constructor: expr) => {
            pub fn $name(s: &str) -> TokenResult {
                map(tag($char), |_| $constructor)(s)
            }
        };
    }

    parse_literal!(parse_space, " ", Token::Space);
    parse_literal!(parse_newline, "\n", Token::Newline);
    parse_literal!(parse_l_brace, "{", Token::LeftBrace);
    parse_literal!(parse_r_brace, "}", Token::RightBrace);
    parse_literal!(parse_l_parens, "(", Token::LeftParens);
    parse_literal!(parse_r_parens, ")", Token::RightParens);
    parse_literal!(parse_l_bracket, "[", Token::LeftBracket);
    parse_literal!(parse_r_bracket, "]", Token::RightBracket);

    pub fn parse_string(s: &str) -> TokenResult {
        map(
            map(
                tuple((char('"'), take_till(|c| c == '"'), char('"'))),
                join_tuple3,
            ),
            |x| Token::Quoted(x.to_owned()),
        )(s)
    }

    pub fn parse_int(s: &str) -> TokenResult {
        map(digit1, |x: &str| Token::Int(x.to_owned()))(s)
    }

    pub fn decimal_number(s: &str) -> Result<String> {
        let seq = (digit1, char('.'), digit1);
        map(tuple(seq), join_tuple3)(s)
    }

    pub fn parse_number(s: &str) -> TokenResult {
        map(decimal_number, |x| Token::Number(x))(s)
    }

    macro_rules! parens {
        ($parser: expr) => {
            map(tuple((char('('), $parser, char(')'))), join_tuple3)
        };
    }

    macro_rules! negative {
        ($digits_parser: expr, $constructor: expr, $s: expr) => {
            map(
                parens!(map(tuple((char('-'), $digits_parser)), join_tuple2)),
                |x| $constructor(x),
            )($s)
        };
    }

    pub fn parse_negative_number(s: &str) -> TokenResult {
        negative!(decimal_number, Token::NegativeNumber, s)
    }

    pub fn parse_negative_int(s: &str) -> TokenResult {
        negative!(digit1, Token::NegativeInt, s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expr() {
        let token_cases: Vec<(&str, parser::TokenResult)> = vec![
            ("0", Ok(("", Token::Int("0".to_owned())))),
            ("1.0", Ok(("", Token::Number("1.0".to_owned())))),
            ("(-1)", Ok(("", Token::NegativeInt("(-1)".to_owned())))),
            (
                "(-1.0)",
                Ok(("", Token::NegativeNumber("(-1.0)".to_owned()))),
            ),
            (" ", Ok(("", Token::Space))),
            ("\n", Ok(("", Token::Newline))),
            ("\"hello\"", Ok(("", Token::Quoted("\"hello\"".to_owned())))),
            ("{", Ok(("", Token::LeftBrace))),
            ("}", Ok(("", Token::RightBrace))),
            ("(", Ok(("", Token::LeftParens))),
            (")", Ok(("", Token::RightParens))),
            ("[", Ok(("", Token::LeftBracket))),
            ("]", Ok(("", Token::RightBracket))),
        ];

        for (input, expected) in token_cases.iter() {
            assert_eq!(parser::parse_expr(input), *expected);
        }
    }
}
