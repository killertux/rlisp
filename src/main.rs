use std::io::{stdin, stdout, BufRead, BufReader, Result as IoResult, Write};

use enviroment::Enviroment;
use executor::{Executor, RuntimeError};
use lexer::Lexer;
use parser::{Ast, Parser};

mod enviroment;
mod executor;
mod lexer;
mod parser;

fn main() -> IoResult<()> {
    let reader = BufReader::new(stdin());
    let mut executor = Executor::new(Enviroment::default());
    print_cursor()?;
    for line in reader.lines() {
        let line = line?;
        print!("{}", rep(&line, &mut executor));
        print_cursor()?;
    }
    Ok(())
}

fn print_cursor() -> IoResult<()> {
    print!("user> ");
    stdout().flush()
}

fn print(ast: Result<Vec<Ast>, RuntimeError>) -> String {
    match ast {
        Err(error) => format!("Error: {}\n", error),
        Ok(vec_ast) => {
            let mut result = String::new();
            for ast in vec_ast {
                result += &ast.to_string();
                result.push('\n');
            }
            result
        }
    }
}

fn rep(input: &str, executor: &mut Executor) -> String {
    print(executor.execute_all(Parser::new(Lexer::new(input)).collect()))
}
