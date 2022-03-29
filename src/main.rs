use std::env;
use std::error::Error;
use std::fs;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: monkey <filename>");
        std::process::exit(1);
    }
    let input = fs::read_to_string(args.get(1).unwrap_or(&"main.mk".to_string()))?;
    let l = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(l);

    let parsed = parser.parse_program();

    match parsed {
        None => {
            if !parser.errors().is_empty() {
                for error in &*parser.errors() {
                    eprintln!("{error}");
                }
            }
        }
        Some(x) => {
            let mut evaluator = evaluator::Evaluator::new();
            let evaluated = evaluator.eval(x)?;

            for output in evaluated {
                println!("{output}");
            }
        }
    }

    Ok(())
}
