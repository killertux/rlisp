use std::error::Error;
use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq, Debug)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    UnquoteSplice,
    SingleQuote,
    Backtick,
    Tilde,
    Circumflex,
    At,
    Colon,
    Amper,
    Text(String),
    Comment(String),
    Number(NumberType),
    Ident(String),
}

#[derive(PartialEq, Debug, Clone)]
pub enum NumberType {
    Float(f64),
    Integer(i64),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Location {
    line: usize,
    column: usize,
}

impl Default for Location {
    fn default() -> Self {
        Self { line: 1, column: 0 }
    }
}

impl Location {
    #[allow(dead_code)]
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }

    pub fn advance(&mut self) {
        self.column += 1;
    }

    pub fn advance_line(&mut self) {
        self.column = 0;
        self.line += 1;
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub ttype: TokenType,
    pub location: Location,
}

impl Token {
    pub fn new(ttype: TokenType, location: Location) -> Self {
        Self { ttype, location }
    }
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    location: Location,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            chars: code.chars().peekable(),
            location: Location::default(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    UnmatchedString(Location),
    NumberParseError(String, Location),
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        use TokenType::*;
        self.location.advance();
        match self.chars.next() {
            None => None,
            Some('\n') => {
                self.location.advance_line();
                self.next()
            }
            Some(c) if c.is_whitespace() => self.next(),
            Some(',') => self.next(),
            Some('(') => Some(Ok(Token::new(LeftParen, self.location.clone()))),
            Some(')') => Some(Ok(Token::new(RightParen, self.location.clone()))),
            Some('[') => Some(Ok(Token::new(LeftBracket, self.location.clone()))),
            Some(']') => Some(Ok(Token::new(RightBracket, self.location.clone()))),
            Some('{') => Some(Ok(Token::new(LeftBrace, self.location.clone()))),
            Some('}') => Some(Ok(Token::new(RightBrace, self.location.clone()))),
            Some('\'') => Some(Ok(Token::new(SingleQuote, self.location.clone()))),
            Some('`') => Some(Ok(Token::new(Backtick, self.location.clone()))),
            Some('^') => Some(Ok(Token::new(Circumflex, self.location.clone()))),
            Some('@') => Some(Ok(Token::new(At, self.location.clone()))),
            Some(':') => Some(Ok(Token::new(Colon, self.location.clone()))),
            Some('&') => Some(Ok(Token::new(Amper, self.location.clone()))),
            Some('~') if self.chars.next_if_eq(&'@') == Some('@') => {
                Some(Ok(Token::new(UnquoteSplice, self.location.clone())))
            }
            Some('~') => Some(Ok(Token::new(Tilde, self.location.clone()))),
            Some('"') => {
                let location = self.location.clone();
                let mut text = String::new();
                while let Some(c) = self.chars.next() {
                    self.location.advance();
                    match c {
                        '\n' => {
                            self.location.advance_line();
                            text.push('\n')
                        }
                        '\\' if self.chars.next_if_eq(&'"').is_some() => {
                            self.location.advance();
                            text.push('\"')
                        }
                        '\\' if self.chars.next_if_eq(&'\\').is_some() => {
                            self.location.advance();
                            text.push('\\')
                        }
                        '\\' if self.chars.next_if_eq(&'n').is_some() => {
                            self.location.advance();
                            text.push('\n')
                        }
                        '"' => return Some(Ok(Token::new(Text(text), location))),
                        _ => text.push(c),
                    }
                }
                Some(Err(LexerError::UnmatchedString(location)))
            }
            Some(';') => {
                let location = self.location.clone();
                let mut text = String::new();
                for c in self.chars.by_ref() {
                    self.location.advance();
                    if c == '\n' {
                        self.location.advance_line();
                        break;
                    }
                    text.push(c);
                }
                Some(Ok(Token::new(Comment(text), location)))
            }
            Some(n)
                if n.is_numeric()
                    || (n == '-' && self.chars.peek().map(|c| c.is_numeric()).unwrap_or(false)) =>
            {
                let location = self.location.clone();
                let mut number = String::new();
                number.push(n);
                while let Some(n) = self
                    .chars
                    .next_if(|c| c.is_numeric() || *c == '.' || *c == '_')
                {
                    self.location.advance();
                    if n == '_' {
                        continue;
                    }
                    number.push(n);
                }
                return match parse_number(&number) {
                    Ok(number) => Some(Ok(Token::new(Number(number), location))),
                    Err(e) => Some(Err(LexerError::NumberParseError(e.to_string(), location))),
                };
            }
            Some(c) => {
                let location = self.location.clone();
                let mut text = String::new();
                text.push(c);
                while let Some(c) = self
                    .chars
                    .next_if(|c| !(c.is_whitespace() || is_special_character(c)))
                {
                    self.location.advance();
                    text.push(c);
                }
                Some(Ok(Token::new(Ident(text), location)))
            }
        }
    }
}

fn parse_number(text: &str) -> Result<NumberType, Box<dyn Error>> {
    if text.contains('.') {
        Ok(NumberType::Float(text.parse()?))
    } else {
        Ok(NumberType::Integer(text.parse()?))
    }
}

fn is_special_character(c: &char) -> bool {
    matches!(
        c,
        '(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | ':' | '~' | '@' | '`' | '\'' | '"' | '&'
    )
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    use super::*;

    macro_rules! t {
        ($type:expr, $line:literal, $column:literal) => {
            Token::new($type, Location::new($line, $column))
        };
    }

    #[test]
    fn lex_parens() {
        let data = " ()(())~@[]{}'`~^@\n";
        let expected = vec![
            LeftParen,
            RightParen,
            LeftParen,
            LeftParen,
            RightParen,
            RightParen,
            UnquoteSplice,
            LeftBracket,
            RightBracket,
            LeftBrace,
            RightBrace,
            SingleQuote,
            Backtick,
            Tilde,
            Circumflex,
            At,
        ];
        let result: Vec<TokenType> = Lexer::new(data).map(|token| token.unwrap().ttype).collect();
        assert_eq!(expected, result);
    }

    #[test]
    fn ignore_white_spaces() {
        let data = "( \r  )  ";
        let result: Vec<Token> = Lexer::new(data).map(|d| d.unwrap()).collect();
        assert_eq!(vec![t!(LeftParen, 1, 1), t!(RightParen, 1, 6),], result)
    }

    #[test]
    fn advance_line() {
        let data = "(\n)";
        let result: Vec<Token> = Lexer::new(data).map(|d| d.unwrap()).collect();
        assert_eq!(vec![t!(LeftParen, 1, 1), t!(RightParen, 2, 1),], result)
    }

    #[test]
    fn texts() {
        let data = r#""single line"
        "multiple
        lines" "single line\" with slashed quotes"
        "#;
        let result: Vec<Token> = Lexer::new(data).map(|d| d.unwrap()).collect();
        assert_eq!(
            vec![
                t!(Text("single line".to_string()), 1, 1),
                t!(Text("multiple\n        lines".to_string()), 2, 9),
                t!(
                    Text("single line\\\" with slashed quotes".to_string()),
                    3,
                    16
                ),
            ],
            result
        )
    }

    #[test]
    fn texts_without_closing_quotes() {
        let data = "\"unclosed quote";
        let result: Result<Vec<Token>, LexerError> = Lexer::new(data).collect();
        assert_eq!(
            Err(LexerError::UnmatchedString(Location::new(1, 1))),
            result
        );
    }

    #[test]
    fn comments() {
        let data = r#";One comment "comentary"; bla"#;
        let result: Vec<Token> = Lexer::new(data).map(|d| d.unwrap()).collect();
        assert_eq!(
            vec![t!(
                Comment("One comment \"comentary\"; bla".to_string()),
                1,
                1
            ),],
            result
        )
    }

    #[test]
    fn numbers() {
        let data = "123 00213 -1_23 32.31 -54.32";
        let result: Vec<Token> = Lexer::new(data).map(|d| d.unwrap()).collect();
        assert_eq!(
            vec![
                t!(Number(NumberType::Integer(123)), 1, 1),
                t!(Number(NumberType::Integer(213)), 1, 5),
                t!(Number(NumberType::Integer(-123)), 1, 11),
                t!(Number(NumberType::Float(32.31)), 1, 17),
                t!(Number(NumberType::Float(-54.32)), 1, 23),
            ],
            result
        )
    }

    #[test]
    fn numbers_with_problems() {
        let data = "123.32.12";
        let result: Result<Vec<Token>, LexerError> = Lexer::new(data).collect();
        assert_eq!(
            Err(LexerError::NumberParseError(
                "invalid float literal".to_string(),
                Location::new(1, 1)
            )),
            result
        );
    }

    #[test]
    fn ident() {
        let data = "abc ads123 bru{}";
        let result: Result<Vec<Token>, LexerError> = Lexer::new(data).collect();
        assert_eq!(
            vec![
                t!(Ident("abc".to_string()), 1, 1),
                t!(Ident("ads123".to_string()), 1, 5),
                t!(Ident("bru".to_string()), 1, 12),
                t!(LeftBrace, 1, 15),
                t!(RightBrace, 1, 16),
            ],
            result.unwrap()
        )
    }

    #[test]
    fn comma_is_whitespace() {
        let data = "(,,,, ,, ,,)";
        let result: Result<Vec<Token>, LexerError> = Lexer::new(data).collect();
        assert_eq!(
            vec![t!(LeftParen, 1, 1), t!(RightParen, 1, 12)],
            result.unwrap()
        );
    }
}
