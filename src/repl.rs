use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history!");
    }

    let mut e = evaluator::Evaluator::new();
    loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                // Pass it to the lexer here
                let l = lexer::Lexer::new(line);
                // Pass the lexer to the Parser
                let mut p = parser::Parser::new(l);
                let parsed = p.parse_program();

                let parsed = match parsed {
                    Some(p) => p,
                    None => {
                        if !p.errors().is_empty() {
                            for error in &*p.errors() {
                                eprintln!("{error}");
                            }
                        } else {
                            eprintln!("Err: Unexpected Parsing Error!");
                        }
                        continue;
                    }
                };
                // Pass it to the evaluator
                match e.eval(parsed) {
                    Ok(outputs) => {
                        for output in outputs {
                            println!("{output}");
                        }
                    }
                    Err(e) => {
                        eprintln!("{e}");
                        continue;
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt")
        .expect("Error while saving history!");
}
