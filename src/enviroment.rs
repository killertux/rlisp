use crate::executor::{RuntimeError, RuntimeResult};
use crate::lexer::NumberType;
use crate::parser::{Ast, Function};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Enviroment {
    inner: Rc<RefCell<Inner>>,
}

struct Inner {
    outer: Option<Rc<RefCell<Inner>>>,
    symbols: HashMap<String, Ast>,
}

impl Inner {
    pub fn get(&self, symbol: &str) -> Option<Ast> {
        match (self.symbols.get(symbol), &self.outer) {
            (Some(ast), _) => Some(ast.clone()),
            (None, Some(outer)) => outer.borrow().get(symbol),
            (None, None) => None,
        }
    }

    pub fn set(&mut self, symbol: String, ast: Ast) {
        self.symbols.insert(symbol, ast);
    }
}

impl Enviroment {
    pub fn get_sub_enviroment(&self) -> Self {
        Self {
            inner: Rc::new(RefCell::new(Inner {
                outer: Some(self.inner.clone()),
                symbols: HashMap::new(),
            })),
        }
    }

    pub fn get(&self, symbol: &str) -> Option<Ast> {
        self.inner.borrow().get(symbol)
    }

    pub fn set(&mut self, symbol: String, ast: Ast) -> RuntimeResult<()> {
        let mut value = self.inner.borrow_mut();
        value.set(symbol, ast);
        Ok(())
    }

    pub fn empty() -> Self {
        Self {
            inner: Rc::new(RefCell::new(Inner {
                outer: None,
                symbols: HashMap::new(),
            })),
        }
    }

    pub fn default() -> Self {
        Self {
            inner: Rc::new(RefCell::new(Inner {
                outer: None,
                symbols: HashMap::from([
                    (
                        "+".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.len() < 2 {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 2,
                                    found: args.len(),
                                });
                            }
                            let mut result = NumberType::Integer(0);
                            for arg in args {
                                let number_type = match arg {
                                    Ast::Number(number_type) => number_type,
                                    _ => {
                                        return Err(RuntimeError::InvalidArgumentType {
                                            expected: Ast::Number(NumberType::Integer(0)),
                                            found: arg,
                                        })
                                    }
                                };
                                result = match (result, number_type) {
                                    (NumberType::Integer(a), NumberType::Integer(b)) => {
                                        NumberType::Integer(a + b)
                                    }
                                    (NumberType::Integer(a), NumberType::Float(b)) => {
                                        NumberType::Float(a as f64 + b)
                                    }
                                    (NumberType::Float(a), NumberType::Integer(b)) => {
                                        NumberType::Float(a + b as f64)
                                    }
                                    (NumberType::Float(a), NumberType::Float(b)) => {
                                        NumberType::Float(a + b)
                                    }
                                };
                            }
                            Ok(Ast::Number(result))
                        })))),
                    ),
                    (
                        "-".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.len() < 2 {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 2,
                                    found: args.len(),
                                });
                            }
                            let mut result = NumberType::Integer(0);
                            for (index, arg) in args.into_iter().enumerate() {
                                let number_type = match arg {
                                    Ast::Number(number_type) => number_type,
                                    _ => {
                                        return Err(RuntimeError::InvalidArgumentType {
                                            expected: Ast::Number(NumberType::Integer(0)),
                                            found: arg,
                                        })
                                    }
                                };
                                result = match (index, result, number_type) {
                                    (0, _, number) => number,
                                    (_, NumberType::Integer(a), NumberType::Integer(b)) => {
                                        NumberType::Integer(a - b)
                                    }
                                    (_, NumberType::Integer(a), NumberType::Float(b)) => {
                                        NumberType::Float(a as f64 - b)
                                    }
                                    (_, NumberType::Float(a), NumberType::Integer(b)) => {
                                        NumberType::Float(a - b as f64)
                                    }
                                    (_, NumberType::Float(a), NumberType::Float(b)) => {
                                        NumberType::Float(a - b)
                                    }
                                };
                            }
                            Ok(Ast::Number(result))
                        })))),
                    ),
                    (
                        "*".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.len() < 2 {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 2,
                                    found: args.len(),
                                });
                            }
                            let mut result = NumberType::Integer(1);
                            for arg in args {
                                let number_type = match arg {
                                    Ast::Number(number_type) => number_type,
                                    _ => {
                                        return Err(RuntimeError::InvalidArgumentType {
                                            expected: Ast::Number(NumberType::Integer(0)),
                                            found: arg,
                                        })
                                    }
                                };
                                result = match (result, number_type) {
                                    (NumberType::Integer(a), NumberType::Integer(b)) => {
                                        NumberType::Integer(a * b)
                                    }
                                    (NumberType::Integer(a), NumberType::Float(b)) => {
                                        NumberType::Float(a as f64 * b)
                                    }
                                    (NumberType::Float(a), NumberType::Integer(b)) => {
                                        NumberType::Float(a * b as f64)
                                    }
                                    (NumberType::Float(a), NumberType::Float(b)) => {
                                        NumberType::Float(a * b)
                                    }
                                };
                            }
                            Ok(Ast::Number(result))
                        })))),
                    ),
                    (
                        "/".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.len() < 2 {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 2,
                                    found: args.len(),
                                });
                            }
                            let mut result = NumberType::Float(0.0);
                            for (index, arg) in args.into_iter().enumerate() {
                                let number_type = match arg {
                                    Ast::Number(number_type) => number_type,
                                    _ => {
                                        return Err(RuntimeError::InvalidArgumentType {
                                            expected: Ast::Number(NumberType::Integer(0)),
                                            found: arg,
                                        })
                                    }
                                };
                                result = match (index, result, number_type) {
                                    (0, _, number) => number,
                                    (_, NumberType::Integer(a), NumberType::Integer(b)) => {
                                        NumberType::Float(a as f64 / b as f64)
                                    }
                                    (_, NumberType::Integer(a), NumberType::Float(b)) => {
                                        NumberType::Float(a as f64 / b)
                                    }
                                    (_, NumberType::Float(a), NumberType::Integer(b)) => {
                                        NumberType::Float(a / b as f64)
                                    }
                                    (_, NumberType::Float(a), NumberType::Float(b)) => {
                                        NumberType::Float(a / b)
                                    }
                                };
                            }
                            Ok(Ast::Number(result))
                        })))),
                    ),
                    (
                        "list".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            Ok(Ast::List(args))
                        })))),
                    ),
                    (
                        "list?".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.is_empty() {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 1,
                                    found: 0,
                                });
                            }
                            Ok(match args[0] {
                                Ast::List(_) => Ast::Bool(true),
                                _ => Ast::Bool(false),
                            })
                        })))),
                    ),
                    (
                        "prn".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            println!(
                                "{}",
                                args.iter().map(|element| element.pr_str(true)).join(" ")
                            );
                            Ok(Ast::Nil)
                        })))),
                    ),
                    (
                        "println".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            println!(
                                "{}",
                                args.iter().map(|element| element.pr_str(false)).join(" ")
                            );
                            Ok(Ast::Nil)
                        })))),
                    ),
                    (
                        "empty?".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.is_empty() {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 1,
                                    found: 0,
                                });
                            }
                            Ok(Ast::Bool(match &args[0] {
                                Ast::List(list) => list.is_empty(),
                                Ast::Vector(vec) => vec.is_empty(),
                                Ast::Map(map) => map.is_empty(),
                                _ => false,
                            }))
                        })))),
                    ),
                    (
                        "count".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.is_empty() {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 1,
                                    found: 0,
                                });
                            }
                            Ok(Ast::Number(NumberType::Integer(match &args[0] {
                                Ast::List(list) => list.len() as i64,
                                Ast::Vector(vec) => vec.len() as i64,
                                Ast::Map(map) => map.len() as i64,
                                _ => 0,
                            })))
                        })))),
                    ),
                    (
                        "=".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.len() < 2 {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 2,
                                    found: args.len(),
                                });
                            }
                            Ok(Ast::Bool(args[0].is_equal(&args[1])))
                        })))),
                    ),
                    (
                        ">".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.len() < 2 {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 2,
                                    found: args.len(),
                                });
                            }
                            Ok(Ast::Bool(match (&args[0], &args[1]) {
                                (
                                    Ast::Number(NumberType::Integer(a)),
                                    Ast::Number(NumberType::Integer(b)),
                                ) => *a > *b,
                                (
                                    Ast::Number(NumberType::Integer(a)),
                                    Ast::Number(NumberType::Float(b)),
                                ) => *a as f64 > *b,
                                (
                                    Ast::Number(NumberType::Float(a)),
                                    Ast::Number(NumberType::Integer(b)),
                                ) => *a > *b as f64,
                                (
                                    Ast::Number(NumberType::Float(a)),
                                    Ast::Number(NumberType::Float(b)),
                                ) => *a > *b,
                                _ => false,
                            }))
                        })))),
                    ),
                    (
                        ">=".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.len() < 2 {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 2,
                                    found: args.len(),
                                });
                            }
                            Ok(Ast::Bool(match (&args[0], &args[1]) {
                                (
                                    Ast::Number(NumberType::Integer(a)),
                                    Ast::Number(NumberType::Integer(b)),
                                ) => *a >= *b,
                                (
                                    Ast::Number(NumberType::Integer(a)),
                                    Ast::Number(NumberType::Float(b)),
                                ) => (*a as f64) >= *b,
                                (
                                    Ast::Number(NumberType::Float(a)),
                                    Ast::Number(NumberType::Integer(b)),
                                ) => *a >= *b as f64,
                                (
                                    Ast::Number(NumberType::Float(a)),
                                    Ast::Number(NumberType::Float(b)),
                                ) => *a >= *b,
                                _ => false,
                            }))
                        })))),
                    ),
                    (
                        "<".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.len() < 2 {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 2,
                                    found: args.len(),
                                });
                            }
                            Ok(Ast::Bool(match (&args[0], &args[1]) {
                                (
                                    Ast::Number(NumberType::Integer(a)),
                                    Ast::Number(NumberType::Integer(b)),
                                ) => *a < *b,
                                (
                                    Ast::Number(NumberType::Integer(a)),
                                    Ast::Number(NumberType::Float(b)),
                                ) => (*a as f64) < *b,
                                (
                                    Ast::Number(NumberType::Float(a)),
                                    Ast::Number(NumberType::Integer(b)),
                                ) => *a < *b as f64,
                                (
                                    Ast::Number(NumberType::Float(a)),
                                    Ast::Number(NumberType::Float(b)),
                                ) => *a < *b,
                                _ => false,
                            }))
                        })))),
                    ),
                    (
                        "<=".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            if args.len() < 2 {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: 2,
                                    found: args.len(),
                                });
                            }
                            Ok(Ast::Bool(match (&args[0], &args[1]) {
                                (
                                    Ast::Number(NumberType::Integer(a)),
                                    Ast::Number(NumberType::Integer(b)),
                                ) => *a <= *b,
                                (
                                    Ast::Number(NumberType::Integer(a)),
                                    Ast::Number(NumberType::Float(b)),
                                ) => *a as f64 <= *b,
                                (
                                    Ast::Number(NumberType::Float(a)),
                                    Ast::Number(NumberType::Integer(b)),
                                ) => *a <= *b as f64,
                                (
                                    Ast::Number(NumberType::Float(a)),
                                    Ast::Number(NumberType::Float(b)),
                                ) => *a <= *b,
                                _ => false,
                            }))
                        })))),
                    ),
                    ("not".to_string(), {
                        use crate::executor::execute;
                        use crate::lexer::Lexer;
                        use crate::parser::{Parser, ParserError};
                        execute(
                            &mut Enviroment::empty(),
                            Parser::new(Lexer::new("(def! not (fn* (a) (if a false true)))"))
                                .collect::<Result<Vec<Ast>, ParserError>>()
                                .unwrap()
                                .remove(0),
                        )
                        .unwrap()
                    }),
                    (
                        "pr-str".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            Ok(Ast::Text(
                                args.into_iter().map(|arg| arg.pr_str(true)).join(" "),
                            ))
                        })))),
                    ),
                    (
                        "str".to_string(),
                        Ast::Function(Function::Closure(Rc::new(Box::new(|args: Vec<Ast>| {
                            Ok(Ast::Text(
                                args.into_iter().map(|arg| arg.pr_str(false)).join(""),
                            ))
                        })))),
                    ),
                ]),
            })),
        }
    }
}

pub trait JoinString {
    fn join(self, separator: &str) -> String;
}

impl<T, R> JoinString for T
where
    T: Iterator<Item = R>,
    R: ToString,
{
    fn join(self, separator: &str) -> String {
        let mut result = String::new();
        for (index, item) in self.map(|item| item.to_string()).enumerate() {
            let element = match index {
                0 => item,
                _ => format!("{}{}", separator, item),
            };
            result.push_str(&element)
        }
        result
    }
}
