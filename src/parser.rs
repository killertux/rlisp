use crate::enviroment::JoinString;
use crate::executor::RuntimeResult;
use crate::lexer::{Lexer, LexerError, NumberType, Token, TokenType};

use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::iter::Peekable;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum ParserError {
    LexerError(LexerError),
    WrongToken { expected: TokenType, found: Token },
    WrongExpression { expected: Ast, found: Ast },
    MissingToken(TokenType),
    MissingExpression,
    UnexpectedToken(TokenType),
}

impl From<LexerError> for ParserError {
    fn from(error: LexerError) -> Self {
        Self::LexerError(error)
    }
}

#[derive(Clone)]
pub struct Function(pub Rc<Box<dyn Fn(Vec<Ast>) -> RuntimeResult<Ast>>>);

impl Function {
    pub fn empty() -> Self {
        Self(Rc::new(Box::new(|_| unreachable!())))
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "<Function>")
    }
}

impl PartialEq for Function {
    fn eq(&self, _: &Function) -> bool {
        true
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    Number(NumberType),
    Text(String),
    List(Vec<Ast>),
    Ident(String),
    Nil,
    Bool(bool),
    Vector(Vec<Ast>),
    Quote(Box<Ast>),
    QuasiQuote(Box<Ast>),
    Unquote(Box<Ast>),
    SpliceUnquote(Box<Ast>),
    Deref(Box<Ast>),
    Key(String),
    Map(HashMap<MapKey, Ast>),
    Metadata(HashMap<MapKey, Ast>, Box<Ast>),
    Function(Function),
    Variadic(String),
}

impl Ast {
    pub fn type_as_text(&self) -> String {
        match self {
            Ast::Number(_) => "Number",
            Ast::Text(_) => "Text",
            Ast::List(_) => "List",
            Ast::Ident(_) => "Identifier",
            Ast::Nil => "Nil",
            Ast::Bool(_) => "Bool",
            Ast::Vector(_) => "Vector",
            Ast::Quote(_) => "Quote",
            Ast::QuasiQuote(_) => "QuasiQuote",
            Ast::Unquote(_) => "Unquote",
            Ast::SpliceUnquote(_) => "SpliceUnquote",
            Ast::Deref(_) => "Deref",
            Ast::Key(_) => "Key",
            Ast::Map(_) => "Map",
            Ast::Metadata(_, _) => "Metadata",
            Ast::Function(_) => "Function",
            Ast::Variadic(_) => "&",
        }
        .to_string()
    }

    pub fn pr_str(&self, readably: bool) -> String {
        match self {
            Ast::Number(NumberType::Float(number)) => format!("{number}"),
            Ast::Number(NumberType::Integer(number)) => format!("{number}"),
            Ast::Text(string) => {
                if readably {
                    format!("\"{}\"", escape_string(string.clone()))
                } else {
                    string.clone()
                }
            }
            Ast::Ident(string) => format!("{string}"),
            Ast::Nil => format!("nil"),
            Ast::Variadic(value) => format!("& {value}"),
            Ast::Bool(value) => format!("{}", if *value { "true" } else { "false" }),
            Ast::Quote(value) => format!("(quote {value})",),
            Ast::QuasiQuote(value) => format!("(quasiquote {value})",),
            Ast::Unquote(value) => format!("(unquote {value})",),
            Ast::SpliceUnquote(value) => format!("(splice-unquote {value})",),
            Ast::Deref(value) => format!("(deref {value})",),
            Ast::Key(value) => format!(":{value}",),
            Ast::Function(_) => format!("#<function>",),
            Ast::Metadata(metadata, value) => {
                format!(
                    "(with-meta {value} {{{}}})",
                    metadata
                        .iter()
                        .map(|(key, element)| format!(
                            "{} {}",
                            key.pr_str(readably),
                            element.pr_str(readably)
                        ))
                        .join(" ")
                )
            }
            Ast::List(elements) => {
                format!(
                    "({})",
                    elements
                        .iter()
                        .map(|element| element.pr_str(readably))
                        .join(" ")
                )
            }
            Ast::Vector(elements) => {
                format!(
                    "[{}]",
                    elements
                        .iter()
                        .map(|element| element.pr_str(readably))
                        .join(" ")
                )
            }
            Ast::Map(elements) => {
                format!(
                    "{{{}}}",
                    elements
                        .iter()
                        .map(|(key, element)| format!(
                            "{} {}",
                            key.pr_str(readably),
                            element.pr_str(readably)
                        ))
                        .join(" ")
                )
            }
        }
    }

    pub fn is_equal(&self, other: &Self) -> bool {
        use Ast::*;
        match (self, other) {
            (List(a), List(b))
            | (List(a), Vector(b))
            | (Vector(a), Vector(b))
            | (Vector(a), List(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .map(|(a, b)| a.is_equal(b))
                        .all(|result| result)
            }
            (a, b) => a == b,
        }
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.pr_str(true))
    }
}

fn escape_string(string: String) -> String {
    let mut result = String::new();
    for c in string.chars() {
        match c {
            '\\' => result.push_str("\\\\"),
            '\"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            other => result.push(other),
        }
    }
    result
}

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub enum MapKey {
    Key(String),
    Text(String),
}

impl MapKey {
    pub fn pr_str(&self, readably: bool) -> String {
        match &self {
            MapKey::Text(string) => {
                if readably {
                    format!("\"{}\"", escape_string(string.clone()))
                } else {
                    string.clone()
                }
            }
            MapKey::Key(value) => format!(":{value}"),
        }
    }
}

impl TryFrom<Ast> for MapKey {
    type Error = ParserError;

    fn try_from(ast: Ast) -> Result<Self, Self::Error> {
        match ast {
            Ast::Text(string) => Ok(MapKey::Text(string)),
            Ast::Key(string) => Ok(MapKey::Key(string)),
            other => Err(ParserError::WrongExpression {
                expected: Ast::Key(String::new()),
                found: other,
            }),
        }
    }
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }

    fn parse_expression(&mut self) -> Result<Option<Ast>, ParserError> {
        match self.lexer.next().transpose()? {
            None => Ok(None),
            Some(token) => match token.ttype {
                TokenType::Number(number_type) => Ok(Some(Ast::Number(number_type))),
                TokenType::Text(string) => Ok(Some(Ast::Text(string))),
                TokenType::SingleQuote => {
                    let expression = self.parse_expression()?;
                    match expression {
                        Some(value) => Ok(Some(Ast::Quote(Box::new(value)))),
                        None => Err(ParserError::MissingExpression),
                    }
                }
                TokenType::Backtick => {
                    let expression = self.parse_expression()?;
                    match expression {
                        Some(value) => Ok(Some(Ast::QuasiQuote(Box::new(value)))),
                        None => Err(ParserError::MissingExpression),
                    }
                }
                TokenType::Tilde => {
                    let expression = self.parse_expression()?;
                    match expression {
                        Some(value) => Ok(Some(Ast::Unquote(Box::new(value)))),
                        None => Err(ParserError::MissingExpression),
                    }
                }
                TokenType::UnquoteSplice => {
                    let expression = self.parse_expression()?;
                    match expression {
                        Some(value) => Ok(Some(Ast::SpliceUnquote(Box::new(value)))),
                        None => Err(ParserError::MissingExpression),
                    }
                }
                TokenType::At => {
                    let expression = self.parse_expression()?;
                    match expression {
                        Some(value) => Ok(Some(Ast::Deref(Box::new(value)))),
                        None => Err(ParserError::MissingExpression),
                    }
                }
                TokenType::Colon => {
                    let expression = self.parse_expression()?;
                    match expression {
                        Some(Ast::Ident(ident)) => Ok(Some(Ast::Key(ident))),
                        Some(expression) => Err(ParserError::WrongExpression {
                            expected: Ast::Ident(String::new()),
                            found: expression,
                        }),
                        None => Err(ParserError::MissingExpression),
                    }
                }
                TokenType::Circumflex => {
                    let metadata = match self.parse_expression()? {
                        Some(Ast::Map(map)) => map,
                        Some(expression) => {
                            return Err(ParserError::WrongExpression {
                                expected: Ast::Map(HashMap::new()),
                                found: expression,
                            })
                        }
                        None => return Err(ParserError::MissingExpression),
                    };
                    match self.parse_expression()? {
                        Some(expression) => Ok(Some(Ast::Metadata(metadata, Box::new(expression)))),
                        None => Err(ParserError::MissingExpression),
                    }
                }
                TokenType::Ident(ident) => match ident.as_str() {
                    "nil" => Ok(Some(Ast::Nil)),
                    "true" => Ok(Some(Ast::Bool(true))),
                    "false" => Ok(Some(Ast::Bool(false))),
                    _ => Ok(Some(Ast::Ident(ident))),
                },
                TokenType::LeftParen => {
                    let mut vec = Vec::new();
                    while let Some(value) = self.lexer.peek() {
                        if Self::is_next_token_eq(value, TokenType::RightParen) {
                            break;
                        }
                        match self.next().transpose()? {
                            None => return Err(ParserError::MissingToken(TokenType::RightParen)),
                            Some(element) => vec.push(element),
                        }
                    }
                    self.consume_and_expect(TokenType::RightParen)?;
                    Ok(Some(Ast::List(vec)))
                }
                TokenType::LeftBracket => {
                    let mut vec = Vec::new();
                    while let Some(value) = self.lexer.peek() {
                        if Self::is_next_token_eq(value, TokenType::RightBracket) {
                            break;
                        }
                        match self.next().transpose()? {
                            None => return Err(ParserError::MissingToken(TokenType::RightBracket)),
                            Some(element) => vec.push(element),
                        }
                    }
                    self.consume_and_expect(TokenType::RightBracket)?;
                    Ok(Some(Ast::Vector(vec)))
                }
                TokenType::LeftBrace => {
                    let mut map = HashMap::new();
                    while let Some(value) = self.lexer.peek() {
                        if Self::is_next_token_eq(value, TokenType::RightBrace) {
                            break;
                        }
                        let key = match self.next().transpose()? {
                            None => return Err(ParserError::MissingToken(TokenType::RightBrace)),
                            Some(element) => element,
                        };

                        match self.next().transpose()? {
                            None => return Err(ParserError::MissingToken(TokenType::RightBrace)),
                            Some(value) => {
                                map.insert(key.try_into()?, value);
                            }
                        }
                    }
                    self.consume_and_expect(TokenType::RightBrace)?;
                    Ok(Some(Ast::Map(map)))
                }
                TokenType::Comment(_) => self.parse_expression(),
                TokenType::Amper => match self.parse_expression()? {
                    Some(Ast::Ident(symbol)) => Ok(Some(Ast::Variadic(symbol))),
                    Some(other) => Err(ParserError::WrongExpression {
                        expected: Ast::Ident(String::new()),
                        found: other,
                    }),

                    None => Err(ParserError::MissingExpression),
                },
                unexpected => Err(ParserError::UnexpectedToken(unexpected)),
            },
        }
    }

    fn consume_and_expect(&mut self, expected: TokenType) -> Result<(), ParserError> {
        let token = self.lexer.next();
        match token {
            None => Err(ParserError::MissingToken(expected)),
            Some(Err(e)) => Err(e.into()),
            Some(Ok(token)) => {
                if token.ttype == expected {
                    Ok(())
                } else {
                    Err(ParserError::WrongToken {
                        expected,
                        found: token,
                    })
                }
            }
        }
    }

    fn is_next_token_eq(value: &Result<Token, LexerError>, token_type: TokenType) -> bool {
        match value {
            Err(_) => false,
            Ok(token) if token.ttype == token_type => true,
            _ => false,
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Ast, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_expression().transpose()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Ast::*;

    #[test]
    fn numbers() {
        let data = "1 -32.21";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![
                Number(NumberType::Integer(1)),
                Number(NumberType::Float(-32.21))
            ],
            result.unwrap()
        );
    }

    #[test]
    fn text() {
        let data = "\"some test\"";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(vec![Text("some test".to_string())], result.unwrap());
    }

    #[test]
    fn ident() {
        let data = "ident idenWith1Numbers";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![
                Ident("ident".to_string()),
                Ident("idenWith1Numbers".to_string())
            ],
            result.unwrap()
        );
    }

    #[test]
    fn empty_list() {
        let data = "()";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(vec![List(vec![])], result.unwrap());
    }

    #[test]
    fn list() {
        let data = "(1 \"text\" ident (foo bar) foobar)";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![List(vec![
                Number(NumberType::Integer(1)),
                Text("text".to_string()),
                Ident("ident".to_string()),
                List(vec![Ident("foo".to_string()), Ident("bar".to_string())],),
                Ident("foobar".to_string())
            ])],
            result.unwrap()
        );
    }

    #[test]
    fn nil() {
        let data = "nil";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(vec![Nil], result.unwrap());
    }

    #[test]
    fn bool() {
        let data = "true false";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(vec![Bool(true), Bool(false)], result.unwrap());
    }

    #[test]
    fn vector() {
        let data = "[] [true ident [false]]";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![
                Vector(vec![]),
                Vector(vec!(
                    Bool(true),
                    Ident("ident".to_string()),
                    Vector(vec!(Bool(false)))
                ))
            ],
            result.unwrap()
        );
    }

    #[test]
    fn quote() {
        let data = "'a 'b";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![
                Quote(Box::new(Ident("a".to_string()))),
                Quote(Box::new(Ident("b".to_string()))),
            ],
            result.unwrap()
        );
    }

    #[test]
    fn quote_error() {
        let data = "'";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(Err(ParserError::MissingExpression), result);
    }

    #[test]
    fn quasiquote() {
        let data = "`a `b";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![
                QuasiQuote(Box::new(Ident("a".to_string()))),
                QuasiQuote(Box::new(Ident("b".to_string()))),
            ],
            result.unwrap()
        );
    }

    #[test]
    fn quasiquote_error() {
        let data = "`";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(Err(ParserError::MissingExpression), result);
    }

    #[test]
    fn unquote() {
        let data = "~a ~b";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![
                Unquote(Box::new(Ident("a".to_string()))),
                Unquote(Box::new(Ident("b".to_string()))),
            ],
            result.unwrap()
        );
    }

    #[test]
    fn unquote_error() {
        let data = "~";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(Err(ParserError::MissingExpression), result);
    }

    #[test]
    fn splice_unquote() {
        let data = "~@a ~@b";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![
                SpliceUnquote(Box::new(Ident("a".to_string()))),
                SpliceUnquote(Box::new(Ident("b".to_string()))),
            ],
            result.unwrap()
        );
    }

    #[test]
    fn splice_unquote_error() {
        let data = "~@";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(Err(ParserError::MissingExpression), result);
    }

    #[test]
    fn key() {
        let data = ":a :b";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![Key("a".to_string()), Key("b".to_string()),],
            result.unwrap()
        );
    }

    #[test]
    fn key_error() {
        let data = ":123";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            Err(ParserError::WrongExpression {
                expected: Ident(String::new()),
                found: Number(NumberType::Integer(123))
            }),
            result
        );
    }

    #[test]
    fn map() {
        let data = "{} {:a foo \"b\" bar}";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![
                Map(HashMap::new()),
                Map(HashMap::from([
                    (MapKey::Key("a".to_string()), Ast::Ident("foo".to_string())),
                    (MapKey::Text("b".to_string()), Ast::Ident("bar".to_string()))
                ])),
            ],
            result.unwrap()
        );
    }

    #[test]
    fn invalid_key_in_map() {
        let data = "{1 a}";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            Err(ParserError::WrongExpression {
                expected: Key(String::new()),
                found: Number(NumberType::Integer(1))
            }),
            result
        );
    }

    #[test]
    fn comment() {
        let data = ";Some comment";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(Vec::<Ast>::new(), result.unwrap());
    }

    #[test]
    fn deref() {
        let data = "@a @b";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![
                Deref(Box::new(Ident("a".to_string()))),
                Deref(Box::new(Ident("b".to_string()))),
            ],
            result.unwrap()
        );
    }

    #[test]
    fn derf_error() {
        let data = "@";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(Err(ParserError::MissingExpression), result);
    }

    #[test]
    fn metadata() {
        let data = "^{:foo \"bar\"} [1 2 3]";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            vec![Metadata(
                HashMap::from([(MapKey::Key("foo".to_string()), Text("bar".to_string()))]),
                Box::new(Vector(vec!(
                    Number(NumberType::Integer(1)),
                    Number(NumberType::Integer(2)),
                    Number(NumberType::Integer(3))
                )))
            )],
            result.unwrap()
        );
    }

    #[test]
    fn metadata_not_using_map() {
        let data = "^ident [1 2 3]";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(
            Err(ParserError::WrongExpression {
                expected: Ast::Map(HashMap::new()),
                found: Ident("ident".to_string())
            }),
            result
        );
    }

    #[test]
    fn metadata_without_any_meta() {
        let data = "^";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(Err(ParserError::MissingExpression), result);
    }

    #[test]
    fn metadata_without_value() {
        let data = "^{:a b}";
        let result: Result<Vec<Ast>, ParserError> = Parser::new(Lexer::new(data)).collect();
        assert_eq!(Err(ParserError::MissingExpression), result);
    }
}
