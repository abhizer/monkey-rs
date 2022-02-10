use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history!");
    }

    loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                // Pass it to the lexer here
                let l = lexer::Lexer::new(line);
                // Pass the lexer to the Parser
                let p = parser::Parser::new(l);

                match p.parse_program() {
                    Some(x) => println!("{:?}", x),
                    None => {
                        if p.errors().borrow().len() > 0 {
                            for error in &*p.errors().borrow() {
                                eprintln!("{error}");
                            }
                        }
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
