use crate::enviroment::Enviroment;
use crate::parser::{Ast, Function, MapKey, ParserError};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::rc::Rc;

pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Debug)]
pub enum RuntimeError {
    ParserError(ParserError),
    InvalidNumberOfArguments { expected: usize, found: usize },
    InvalidArgumentType { expected: Ast, found: Ast },
    InvalidExpression { expected: Ast, found: Ast },
    SymbolNotFound(String),
    ExpectedExpression,
    VariadicIsNotTheLastBind(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            RuntimeError::ParserError(parser_error) => {
                write!(f, "Parser error: {:?}", parser_error)
            } // TODO: Improve this error
            RuntimeError::InvalidNumberOfArguments { expected, found } => {
                write!(
                    f,
                    "Invalid number of arguments. Expected {expected} and got {found}",
                )
            }
            RuntimeError::InvalidArgumentType { expected, found } => {
                write!(
                    f,
                    "Invalid argument type. Expected {} and got {found}",
                    expected.type_as_text(),
                )
            }
            RuntimeError::InvalidExpression { expected, found } => {
                write!(
                    f,
                    "Invalid argument type. Expected {} and got {found}",
                    expected.type_as_text(),
                )
            }
            RuntimeError::SymbolNotFound(symbol) => {
                write!(f, "Symbol {symbol} not found")
            }
            RuntimeError::ExpectedExpression => write!(f, "Expected a expression"),
            RuntimeError::VariadicIsNotTheLastBind(variadic) => {
                write!(
                    f,
                    "Variadic {variadic} is not in the last position of the bind"
                )
            }
        }
    }
}

impl From<ParserError> for RuntimeError {
    fn from(error: ParserError) -> Self {
        Self::ParserError(error)
    }
}

pub struct Executor {
    env: Enviroment,
}

impl Executor {
    pub fn new(env: Enviroment) -> Self {
        Self { env }
    }

    pub fn execute_all(
        &mut self,
        ast_vec: Result<Vec<Ast>, ParserError>,
    ) -> Result<Vec<Ast>, RuntimeError> {
        let mut results = Vec::new();
        for ast in ast_vec? {
            results.push(self.execute(ast)?);
        }
        Ok(results)
    }

    pub fn execute(&mut self, ast: Ast) -> Result<Ast, RuntimeError> {
        match ast {
            Ast::List(list) if list.is_empty() => Ok(Ast::List(vec![])),
            Ast::List(mut elements) => {
                let first = self.execute_if_list(elements.remove(0))?;
                if let Some(function) = function_or_null(&first) {
                    return function.0(
                        elements
                            .into_iter()
                            .map(|element| self.execute(element))
                            .collect::<Result<Vec<Ast>, RuntimeError>>()?,
                    );
                }
                let head = ident(&first)?;
                match head.as_str() {
                    "def!" => {
                        let symbol_name = pop_ident(&mut elements)?;
                        let value = self.pop_and_evaluate(&mut elements)?;
                        self.env.set(symbol_name, value.clone())?;
                        Ok(value)
                    }
                    "let*" => {
                        let bindings = pop_list_or_vector(&mut elements)?;
                        let mut new_env = self.env.get_sub_enviroment();
                        // We are doing some unecessary clones here
                        for bind in bindings.chunks(2) {
                            let symbol = match &bind[0] {
                                Ast::Ident(symbol) => symbol.clone(),
                                other => {
                                    return Err(RuntimeError::InvalidExpression {
                                        expected: Ast::Ident(String::new()),
                                        found: other.clone(),
                                    })
                                }
                            };
                            let mut executor = Executor::new(new_env);
                            let evaluated_expression = executor.execute(bind[1].clone())?;
                            new_env = executor.enviroment();
                            new_env.set(symbol, evaluated_expression)?;
                        }
                        Executor::new(new_env).pop_and_evaluate(&mut elements)
                    }
                    "do" => match elements
                        .into_iter()
                        .map(|element| self.execute(element))
                        .collect::<Result<Vec<Ast>, RuntimeError>>()?
                        .pop()
                    {
                        None => Err(RuntimeError::ExpectedExpression),
                        Some(element) => Ok(element),
                    },
                    "if" => {
                        let boolean_condition = !matches!(
                            self.pop_and_evaluate(&mut elements)?,
                            Ast::Bool(false) | Ast::Nil
                        );
                        if boolean_condition {
                            self.pop_and_evaluate(&mut elements)
                        } else if elements.len() > 1 {
                            self.execute(elements.remove(1))
                        } else {
                            Ok(Ast::Nil)
                        }
                    }
                    "fn*" => {
                        if elements.len() < 2 {
                            return Err(RuntimeError::InvalidNumberOfArguments {
                                expected: 2,
                                found: elements.len(),
                            });
                        }
                        let list_of_bindings = pop_list_or_vector(&mut elements)?;
                        let n_bindings = list_of_bindings.len();
                        let bindings = list_of_bindings
                            .into_iter()
                            .enumerate()
                            .map(|(index, element)| match element {
                                Ast::Ident(ident) => Ok(Bind::Ident(ident)),
                                Ast::Variadic(variadic) => {
                                    if index == n_bindings - 1 {
                                        Ok(Bind::Variadic(variadic))
                                    } else {
                                        Err(RuntimeError::VariadicIsNotTheLastBind(variadic))
                                    }
                                }
                                other => Err(RuntimeError::InvalidArgumentType {
                                    expected: Ast::Ident(String::new()),
                                    found: other,
                                }),
                            })
                            .collect::<Result<Vec<Bind>, RuntimeError>>()?;
                        let body = elements.remove(0);
                        let new_env = self.env.get_sub_enviroment();
                        Ok(Ast::Function(Function(Rc::new(Box::new(move |args| {
                            let mut new_env = new_env.get_sub_enviroment();
                            let mut bindings = bindings.clone();
                            let variadic_element = variadic_element(&mut bindings);
                            if (bindings.len() != args.len() && variadic_element.is_none())
                                || (args.len() < bindings.len() && variadic_element.is_some())
                            {
                                return Err(RuntimeError::InvalidNumberOfArguments {
                                    expected: bindings.len(),
                                    found: args.len(),
                                });
                            }
                            if let Some(element) = variadic_element {
                                if args.len() < bindings.len() {
                                    new_env.set(element, Ast::List(vec![]))?;
                                } else {
                                    let mut variadic_values = Vec::new();
                                    for arg in args.iter().skip(bindings.len()) {
                                        variadic_values.push(arg.clone());
                                    }
                                    new_env.set(element, Ast::List(variadic_values))?;
                                }
                            }
                            for (bind, value) in bindings.iter().zip(args.into_iter()) {
                                match bind {
                                    Bind::Ident(ident) => new_env.set(ident.clone(), value)?,
                                    Bind::Variadic(_) => {}
                                }
                            }
                            Executor::new(new_env).execute(body.clone())
                        })))))
                    }
                    _ => {
                        let function = match self.env.get(&head) {
                            Some(Ast::Function(function)) => function,
                            Some(other) => {
                                return Err(RuntimeError::InvalidExpression {
                                    expected: Ast::Function(Function::empty()),
                                    found: other,
                                })
                            }
                            None => return Err(RuntimeError::SymbolNotFound(head)),
                        };
                        Ok(function.0(
                            elements
                                .into_iter()
                                .map(|element| self.execute(element))
                                .collect::<Result<Vec<Ast>, RuntimeError>>()?,
                        )?)
                    }
                }
            }
            any => self.eval_ast(any),
        }
    }

    fn enviroment(self) -> Enviroment {
        self.env
    }

    fn eval_ast(&mut self, ast: Ast) -> Result<Ast, RuntimeError> {
        match ast {
            Ast::Ident(ident) => Ok(self.env.get(&ident).unwrap_or(Ast::Nil)),
            Ast::List(list) => Ok(Ast::List(
                list.into_iter()
                    .map(|element| self.execute(element))
                    .collect::<Result<Vec<Ast>, RuntimeError>>()?,
            )),
            Ast::Vector(vector) => Ok(Ast::Vector(
                vector
                    .into_iter()
                    .map(|element| self.execute(element))
                    .collect::<Result<Vec<Ast>, RuntimeError>>()?,
            )),
            Ast::Map(map) => Ok(Ast::Map(
                map.into_iter()
                    .map(|(key, element)| match self.execute(element) {
                        Ok(element) => Ok((key, element)),
                        Err(error) => Err(error),
                    })
                    .collect::<Result<HashMap<MapKey, Ast>, RuntimeError>>()?,
            )),
            any => Ok(any),
        }
    }

    fn pop_and_evaluate(&mut self, elements: &mut Vec<Ast>) -> Result<Ast, RuntimeError> {
        if elements.is_empty() {
            return Err(RuntimeError::ExpectedExpression);
        }
        self.execute(elements.remove(0))
    }

    fn execute_if_list(&mut self, ast: Ast) -> Result<Ast, RuntimeError> {
        match ast {
            Ast::List(list) => self.execute(Ast::List(list)),
            not_list => Ok(not_list),
        }
    }
}

fn pop_ident(elements: &mut Vec<Ast>) -> Result<String, RuntimeError> {
    if elements.is_empty() {
        return Err(RuntimeError::ExpectedExpression);
    }
    match elements.remove(0) {
        Ast::Ident(symbol) => Ok(symbol),
        other => Err(RuntimeError::InvalidExpression {
            expected: Ast::Ident(String::new()),
            found: other,
        }),
    }
}

fn function_or_null(ast: &Ast) -> Option<Function> {
    match ast {
        Ast::Function(function) => Some(function.clone()),
        _ => None,
    }
}

fn ident(ast: &Ast) -> Result<String, RuntimeError> {
    match ast {
        Ast::Ident(ident) => Ok(ident.clone()),
        other => Err(RuntimeError::InvalidExpression {
            expected: Ast::Ident(String::new()),
            found: other.clone(),
        }),
    }
}

fn pop_list_or_vector(elements: &mut Vec<Ast>) -> Result<Vec<Ast>, RuntimeError> {
    if elements.is_empty() {
        return Err(RuntimeError::ExpectedExpression);
    }
    match elements.remove(0) {
        Ast::List(list) => Ok(list),
        Ast::Vector(vector) => Ok(vector),
        other => Err(RuntimeError::InvalidExpression {
            expected: Ast::List(Vec::new()),
            found: other,
        }),
    }
}

#[derive(Clone)]
enum Bind {
    Ident(String),
    Variadic(String),
}

fn variadic_element(bindings: &mut Vec<Bind>) -> Option<String> {
    match bindings.pop() {
        Some(Bind::Variadic(variadic)) => Some(variadic),
        Some(other) => {
            bindings.push(other);
            None
        }
        None => None,
    }
}
