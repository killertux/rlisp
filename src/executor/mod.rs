use crate::enviroment::Enviroment;
use crate::parser::{Ast, Function, MapKey, ParserError};
use mut_ref::MutRef;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

mod mut_ref;

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

pub fn execute_all(
    env: &mut Enviroment,
    ast_vec: Result<Vec<Ast>, ParserError>,
) -> Result<Vec<Ast>, RuntimeError> {
    let mut results = Vec::new();
    for ast in ast_vec? {
        results.push(execute(env, ast)?);
    }
    Ok(results)
}

pub fn execute(env: &mut Enviroment, mut ast: Ast) -> RuntimeResult<Ast> {
    let mut env = MutRef::from_ref(env);
    loop {
        match ast {
            Ast::List(list) if list.is_empty() => return Ok(Ast::List(vec![])),
            Ast::List(mut elements) => {
                return {
                    let first = execute_if_list(&mut env, elements.remove(0))?;
                    if let Some(function) = function_or_null(&first) {
                        let args = elements
                            .into_iter()
                            .map(|element| execute(&mut env, element))
                            .collect::<Result<Vec<Ast>, RuntimeError>>()?;
                        match function {
                            Function::Closure(closure) => return closure(args),
                            Function::UserDefined {
                                bindings,
                                body,
                                env: function_env,
                            } => {
                                env = MutRef::from_value(prepare_enviroment_for_closure(
                                    &args,
                                    &bindings,
                                    &function_env,
                                )?);
                                ast = *body;
                                continue;
                            }
                            Function::Empty => unreachable!(),
                        }
                    }
                    let head = ident(&first)?;
                    match head.as_str() {
                        "def!" => {
                            let symbol_name = pop_ident(&mut elements)?;
                            let value = execute(&mut env, pop(&mut elements)?)?;
                            env.set(symbol_name, value.clone())?;
                            Ok(value)
                        }
                        "let*" => {
                            let bindings = pop_list_or_vector(&mut elements)?;
                            let mut new_env = env.get_sub_enviroment();
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
                                let evaluated_expression = execute(&mut new_env, bind[1].clone())?;
                                new_env.set(symbol, evaluated_expression)?;
                            }
                            ast = pop(&mut elements)?;
                            env = MutRef::from_value(new_env);
                            continue;
                        }
                        "do" => match elements.pop() {
                            None => Err(RuntimeError::ExpectedExpression),
                            Some(last) => {
                                elements
                                    .into_iter()
                                    .map(|element| execute(&mut env, element))
                                    .collect::<RuntimeResult<Vec<Ast>>>()?;
                                ast = last;
                                continue;
                            }
                        },
                        "if" => {
                            let boolean_condition = !matches!(
                                execute(&mut env, pop(&mut elements)?)?,
                                Ast::Bool(false) | Ast::Nil
                            );
                            if boolean_condition {
                                ast = pop(&mut elements)?;
                                continue;
                            } else if elements.len() > 1 {
                                ast = elements.remove(1);
                                continue;
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

                            Ok(Ast::Function(Function::UserDefined {
                                bindings,
                                body: Box::new(elements.remove(0)),
                                env: env.get_sub_enviroment(),
                            }))
                        }
                        _ => {
                            let function = match env.get(&head) {
                                Some(Ast::Function(function)) => function,
                                Some(other) => {
                                    return Err(RuntimeError::InvalidExpression {
                                        expected: Ast::Function(Function::empty()),
                                        found: other,
                                    })
                                }
                                None => return Err(RuntimeError::SymbolNotFound(head)),
                            };
                            let args = elements
                                .into_iter()
                                .map(|element| execute(&mut env, element))
                                .collect::<Result<Vec<Ast>, RuntimeError>>()?;
                            match function {
                                Function::Closure(closure) => return closure(args),
                                Function::UserDefined {
                                    bindings,
                                    body,
                                    env: function_env,
                                } => {
                                    env = MutRef::from_value(prepare_enviroment_for_closure(
                                        &args,
                                        &bindings,
                                        &function_env,
                                    )?);
                                    ast = *body;
                                    continue;
                                }
                                Function::Empty => unreachable!(),
                            }
                        }
                    }
                };
            }
            any => return eval_ast(&mut env, any),
        }
    }
}

fn execute_if_list(env: &mut Enviroment, ast: Ast) -> RuntimeResult<Ast> {
    match ast {
        Ast::List(list) => execute(env, Ast::List(list)),
        not_list => Ok(not_list),
    }
}

fn pop(elements: &mut Vec<Ast>) -> RuntimeResult<Ast> {
    if elements.is_empty() {
        return Err(RuntimeError::ExpectedExpression);
    }
    Ok(elements.remove(0))
}

fn eval_ast(env: &mut Enviroment, ast: Ast) -> Result<Ast, RuntimeError> {
    match ast {
        Ast::Ident(ident) => Ok(env.get(&ident).unwrap_or(Ast::Nil)),
        Ast::List(list) => Ok(Ast::List(
            list.into_iter()
                .map(|element| execute(env, element))
                .collect::<Result<Vec<Ast>, RuntimeError>>()?,
        )),
        Ast::Vector(vector) => Ok(Ast::Vector(
            vector
                .into_iter()
                .map(|element| execute(env, element))
                .collect::<Result<Vec<Ast>, RuntimeError>>()?,
        )),
        Ast::Map(map) => Ok(Ast::Map(
            map.into_iter()
                .map(|(key, element)| match execute(env, element) {
                    Ok(element) => Ok((key, element)),
                    Err(error) => Err(error),
                })
                .collect::<Result<HashMap<MapKey, Ast>, RuntimeError>>()?,
        )),
        any => Ok(any),
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

fn prepare_enviroment_for_closure(
    args: &[Ast],
    bindings: &[Bind],
    env: &Enviroment,
) -> RuntimeResult<Enviroment> {
    let mut new_env = env.get_sub_enviroment();
    let mut bindings = bindings.to_owned();
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
    for (bind, value) in bindings.iter().zip(args.iter()) {
        match bind {
            Bind::Ident(ident) => new_env.set(ident.clone(), value.clone())?,
            Bind::Variadic(_) => {}
        }
    }
    Ok(new_env)
}

#[derive(Clone)]
pub enum Bind {
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
