#![feature(let_chains)]
pub mod ast;
use lexer::token::Token;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<lexer::token::Token>,
    index: usize,
    errors: Vec<String>,
}
impl Parser {
    pub fn new(lexer: lexer::Lexer) -> Self {
        Self {
            tokens: lexer.tokenize(),
            index: 0,
            errors: vec![],
        }
    }

    fn parse_let(&mut self) -> Option<ast::Statement> {
        self.index += 1;

        let next_token = self.tokens.get(self.index)?.clone();

        let ident = match next_token {
            Token::Ident(ref x) => ast::Identifier::new(x),
            unexpected => {
                self.err_unexpected_token(
                    Token::Ident("<identifier>".to_string()),
                    unexpected,
                    Token::Let,
                );
                return None;
            }
        };

        self.index += 1;

        match self.tokens.get(self.index).cloned() {
            None => {
                self.err_unexpected_token(Token::Assignment, Token::Eof, next_token);
                return None;
            }
            Some(Token::Assignment) => {}
            Some(unexpected) => {
                self.err_unexpected_token(
                    Token::Assignment,
                    unexpected,
                    next_token,
                );
            }
        }

        self.index += 1;

        let expr = self.parse_expression(ast::Precendence::Lowest)?;

        Some(ast::Statement::Let(ident, expr))
    }

    fn parse_return(&mut self) -> Option<ast::Statement> {
        self.index += 1;

        let expr = self.parse_expression(ast::Precendence::Lowest)?;

        if Some(&Token::Semicolon) == self.tokens.get(self.index + 1) {
            self.index += 1;
        } else {
            self.err_unexpected_token(
                Token::Semicolon,
                self.tokens
                    .get(self.index + 1)
                    .unwrap_or(&Token::Eof)
                    .to_owned(),
                self.tokens
                    .get(self.index)
                    .unwrap_or(&Token::Return)
                    .to_owned(),
            );
            return None;
        }

        Some(ast::Statement::Return(expr))
    }

    fn parse_identifier(&self, t: &str) -> ast::Expression {
        ast::Expression::Ident(ast::Identifier::new(t))
    }

    fn parse_int_literal(&mut self, n: &str) -> Option<ast::Expression> {
        let num: i64 = match n.parse() {
            Ok(n) => n,
            Err(_) => {
                self.err_unexpected(
                    "Parsing Err: Illegal value supplied for Integer Literal!".to_string(),
                );
                return None;
            }
        };
        Some(ast::Expression::Literal(ast::Literal::Int(num)))
    }

    fn parse_bool_literal(&mut self) -> Option<ast::Expression> {
        Some(ast::Expression::Literal(ast::Literal::Bool(
            match self.tokens.get(self.index) {
                None => return None,
                Some(t) => match t {
                    Token::True => true,
                    Token::False => false,
                    _ => {
                        self.err_unexpected("Parse Errr: Unexpected boolean!".to_string());
                        return None;
                    }
                },
            },
        )))
    }

    fn parse_prefix(&mut self) -> Option<ast::Expression> {
        let prefix_operator = match self.tokens.get(self.index) {
            Some(x) => match x {
                Token::Bang => ast::Prefix::Not,
                Token::Minus => ast::Prefix::Minus,
                // Token::Plus => ast::Prefix::Plus,
                _ => {
                    self.err_unexpected(String::from("Parse Err: Unexpected prefix operator!"));
                    return None;
                }
            },
            None => return None,
        };

        self.index += 1;
        let right = self.parse_expression(ast::Precendence::Prefix)?;

        Some(ast::Expression::Prefix(prefix_operator, Box::new(right)))
    }

    fn parse_inflix(&mut self, left: ast::Expression) -> Option<ast::Expression> {

        let current_token = self.tokens.get(self.index)?.clone();
        let precendence = ast::Precendence::get_precendence(&current_token);
        self.index += 1;

        let right = self.parse_expression(precendence)?;
        Some(ast::Expression::Inflix {
            operator: ast::Inflix::from_token(&current_token)?,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn parse_grouped_expression(&mut self) -> Option<ast::Expression> {
        self.index += 1;

        let expr = self.parse_expression(ast::Precendence::Lowest)?;

        if Some(&Token::Rparen) != self.tokens.get(self.index + 1) {
            self.err_unexpected_token(
                Token::Rparen,
                self.tokens
                    .get(self.index + 1)
                    .unwrap_or(&Token::Eof)
                    .to_owned(),
                self.tokens
                    .get(self.index)
                    .unwrap_or(&Token::Lparen)
                    .to_owned(),
            );
            return None;
        }
        self.index += 1;

        Some(expr)
    }

    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        if Some(&Token::Lparen) != self.tokens.get(self.index + 1) {
            self.err_unexpected_token(
                Token::Lparen,
                self.tokens
                    .get(self.index + 1)
                    .unwrap_or(&Token::Eof)
                    .to_owned(),
                Token::If,
            );
            return None;
        }
        self.index += 2; // Increase self.index by 2 as, tokens should be: If, Lparen <condition>

        let condition = self.parse_expression(ast::Precendence::Lowest)?;

        if Some(&Token::Rparen) != self.tokens.get(self.index + 1) {
            self.err_unexpected_token(
                Token::Rparen,
                self.tokens
                    .get(self.index + 1)
                    .unwrap_or(&Token::Eof)
                    .to_owned(),
                self.tokens
                    .get(self.index)
                    .unwrap_or(&Token::Lparen)
                    .to_owned(),
            );
            return None;
        }

        self.index += 1; // Increase self.index by 1 as, tokens should be: Rparen Lbrace

        if Some(&Token::Lbrace) != self.tokens.get(self.index + 1) {
            self.err_unexpected_token(
                Token::Lbrace,
                self.tokens
                    .get(self.index + 1)
                    .unwrap_or(&Token::Eof)
                    .to_owned(),
                self.tokens
                    .get(self.index)
                    .unwrap_or(&Token::Rparen)
                    .to_owned(),
            );
            return None;
        }

        self.index += 2; // Increase self.index by 2, So that it now points to the Expression after Lbrace

        let consequence = self.parse_blocks()?;

        let mut alternative = None;

        if Some(&Token::Else) == self.tokens.get(self.index + 1) {
            self.index += 1;
            if Some(&Token::Lbrace) != self.tokens.get(self.index + 1) {
                self.err_unexpected_token(
                    Token::Lbrace,
                    self.tokens
                        .get(self.index + 1)
                        .unwrap_or(&Token::Eof)
                        .to_owned(),
                    self.tokens
                        .get(self.index)
                        .unwrap_or(&Token::Rparen)
                        .to_owned(),
                );
                return None;
            }

            self.index += 2;

            alternative = self.parse_blocks();
        }

        let ret = ast::Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        };

        Some(ret)
    }

    fn parse_blocks(&mut self) -> Option<ast::Program> {
        let mut block: ast::Program = Vec::new();

        while let x = self.tokens.get(self.index).cloned() && x.is_some() && x != Some(Token::Rbrace) {
            let stmt = self.parse_statement(&x.unwrap())?;
            self.index += 1; 
            block.push(stmt);
        }

        Some(block)
    }

    fn parse_functions(&mut self) -> Option<ast::Expression> {
        if Some(&Token::Lparen) != self.tokens.get(self.index + 1) {
            self.err_unexpected_token(
                Token::Lparen,
                self.tokens.get(self.index + 1).unwrap_or(&Token::Eof).to_owned(),
                Token::Function,
            );
            return None;
        }

        self.index += 1;

        let parameters = self.parse_function_parameters()?;

        self.index += 1; 

        if Some(&Token::Lbrace) != self.tokens.get(self.index) {
            self.err_unexpected_token(
                Token::Lbrace,
                self.tokens.get(self.index).unwrap_or(&Token::Eof).to_owned(),
                Token::Rparen,
            );
            return None;
        }

        self.index += 1; 

        let body = self.parse_blocks()?;

        Some(ast::Expression::Function { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<ast::Identifier>> {
        let mut identifiers = Vec::new();

        if Some(&Token::Rparen) == self.tokens.get(self.index + 1) {
            self.index += 1; 
            return Some(identifiers);
        }

        self.index += 1; 

        match self.tokens.get(self.index)?.clone() {
            Token::Ident(ref x) => {
                identifiers.push(ast::Identifier::new(x));
                self.index += 1;
            }
            unexpected => {
                self.err_unexpected_token(
                    Token::Ident("<identifier>".to_string()),
                    unexpected,
                    self.tokens.get(self.index).unwrap_or(&Token::Eof).to_owned(),
                );
                return None;
            }
        };

        while Some(&Token::Comma) == self.tokens.get(self.index) {
            self.index += 1;

            match self.tokens.get(self.index)?.clone() {
                Token::Ident(ref x) => {
                    identifiers.push(ast::Identifier::new(x));
                    self.index += 1;
                }
                unexpected => {
                    self.err_unexpected_token(
                        Token::Ident("<identifier>".to_string()),
                        unexpected,
                        Token::Lparen,
                    );
                    return None;
                }
            };
        }

        if Some(&Token::Rparen) != self.tokens.get(self.index) {
            self.err_unexpected_token(
                Token::Rparen,
                self.tokens.get(self.index).unwrap_or(&Token::Eof).to_owned(),
                self.tokens
                    .get(self.index - 1)
                    .unwrap_or(&Token::Illegal) // This should always unwrap just fine
                    .to_owned(),
            );
            return None;
        }

        Some(identifiers)
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<ast::Expression>> {
        let mut args = Vec::new();

        if Some(&Token::Rparen) == self.tokens.get(self.index + 1) {
            return Some(args);
        }

        self.index += 1;

        args.push(self.parse_expression(ast::Precendence::Lowest)?);


        while Some(&Token::Comma) == self.tokens.get(self.index + 1) {
            self.index += 2;
            args.push(self.parse_expression(ast::Precendence::Lowest)?);
        }

        if Some(&Token::Rparen) != self.tokens.get(self.index + 1) {
            self.err_unexpected_token(
                Token::Rparen,
                self.tokens.get(self.index + 1).unwrap_or(&Token::Eof).to_owned(),
                self.tokens
                    .get(self.index)
                    .unwrap_or(&Token::Illegal) // This should always unwrap just fine
                    .to_owned(),
            );
            return None;
        }

        Some(args)
    }

    fn parse_call_expression(&mut self, function: ast::Expression) -> Option<ast::Expression> {
        let arguments = self.parse_call_arguments()?;
        Some(ast::Expression::Call {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_expression(&mut self, precendence: ast::Precendence) -> Option<ast::Expression> {
        let current_token = self
            .tokens
            .get(self.index)
            .unwrap_or(&lexer::token::Token::Eof).clone();

        let mut left = match current_token {
            Token::Ident(x) => Some(self.parse_identifier(&x)),
            Token::Int(x) => self.parse_int_literal(&x),
            Token::Bang | Token::Minus => self.parse_prefix(),
            Token::True | Token::False => self.parse_bool_literal(),
            Token::Lparen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_functions(),
            _ => return None,
        };
        let mut index = self.index;

        while Some(&Token::Semicolon) != self.tokens.get(index + 1)
            && precendence
                < ast::Precendence::get_precendence(
                    self.tokens.get(index + 1).unwrap_or(&Token::Eof),
                )
        {
            left = match self.tokens.get(index + 1)? {
                Token::Equal
                | Token::NotEqual
                | Token::LT
                | Token::GT
                | Token::LE
                | Token::GE
                | Token::Minus
                | Token::Plus
                | Token::Slash
                | Token::Asterisk => {
                    self.index = index + 1;
                    let val = self.parse_inflix(left.to_owned()?);
                    index = self.index;
                    val
                }
                Token::Lparen => {
                    self.index = index + 1;
                    let val = self.parse_call_expression(left.to_owned()?);
                    index = self.index;
                    val
                }
                _ => return left,
            };
        }

        left
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let stmt = self.parse_expression(ast::Precendence::Lowest)?;
        let index = self.index;
        if Some(&Token::Semicolon) == self.tokens.get(index + 1) {
            self.index = index + 1;
        }
        Some(ast::Statement::Expr(stmt))
    }

    fn parse_statement(&mut self, tok: &lexer::token::Token) -> Option<ast::Statement> {
        match tok {
            Token::Let => self.parse_let(),
            Token::Return => self.parse_return(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_program(&mut self) -> Option<ast::Program> {
        let mut program: ast::Program = Vec::new();

        while let Some(token) = self.tokens.get(self.index).cloned() {
            if let Some(x) = self.parse_statement(&token) {
                program.push(x);
            }
            self.index += 1;
        }

        if program.is_empty() {
            return None;
        }

        if !self.errors().is_empty() {
            for error in &*self.errors() {
                println!("{}", error);
            }
        }

        Some(program)
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn err_unexpected_token(&mut self, expected: Token, got: Token, current: Token) {
        self.errors.push(format!(
            "Parse Err: Unexpected token after {:?}! Expected: {:?} but Got {:?}",
            current, expected, got
        ));
    }

    fn err_unexpected(&mut self, err: String) {
        self.errors.push(err);
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        ast::{self, Expression, Identifier, Inflix, Literal, Prefix, Statement},
        Parser,
    };

    #[test]
    fn test_let() {
        let input = "
            let x = 5;
        let y = 10;
        let foobar = 838383;";

        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let expected: crate::ast::Program = Vec::from([
            crate::ast::Statement::Let(
                crate::ast::Identifier::new("x"),
                crate::ast::Expression::Literal(crate::ast::Literal::Int(5)),
            ),
            crate::ast::Statement::Let(
                crate::ast::Identifier::new("y"),
                crate::ast::Expression::Literal(crate::ast::Literal::Int(10)),
            ),
            crate::ast::Statement::Let(
                crate::ast::Identifier::new("foobar"),
                crate::ast::Expression::Literal(crate::ast::Literal::Int(838383)),
            ),
        ]);

        let output = p.parse_program().unwrap();

        assert_eq!(expected, output);
    }

    #[test]
    fn test_return() {
        let input = "return 4;
                    return -5;
                    return 44;";

        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let expected: crate::ast::Program = Vec::from([
            crate::ast::Statement::Return(crate::ast::Expression::Literal(
                crate::ast::Literal::Int(4),
            )),
            crate::ast::Statement::Return(crate::ast::Expression::Prefix(
                crate::ast::Prefix::Minus,
                Box::new(crate::ast::Expression::Literal(crate::ast::Literal::Int(5))),
            )),
            crate::ast::Statement::Return(crate::ast::Expression::Literal(
                crate::ast::Literal::Int(44),
            )),
        ]);

        let output = p.parse_program().unwrap();

        assert_eq!(expected, output);
    }

    #[test]
    fn test_identifier() {
        let input = "foobar;";

        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let expected: crate::ast::Program = Vec::from([crate::ast::Statement::Expr(
            ast::Expression::Ident(crate::ast::Identifier::new("foobar")),
        )]);

        let output = p.parse_program().unwrap();

        assert_eq!(expected, output);
    }

    #[test]
    fn test_integer_literals() {
        let input = "5;";

        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let expected: crate::ast::Program = Vec::from([crate::ast::Statement::Expr(
            ast::Expression::Literal(ast::Literal::Int(5)),
        )]);

        let output = p.parse_program().unwrap();

        assert_eq!(expected, output);
    }

    #[test]
    fn test_prefix() {
        let input = "!5;
        -15;
        !false;";

        let expected: crate::ast::Program = Vec::from([
            crate::ast::Statement::Expr(crate::ast::Expression::Prefix(
                ast::Prefix::Not,
                Box::new(ast::Expression::Literal(ast::Literal::Int(5))),
            )),
            crate::ast::Statement::Expr(crate::ast::Expression::Prefix(
                ast::Prefix::Minus,
                Box::new(ast::Expression::Literal(ast::Literal::Int(15))),
            )),
            crate::ast::Statement::Expr(crate::ast::Expression::Prefix(
                ast::Prefix::Not,
                Box::new(ast::Expression::Literal(ast::Literal::Bool(false))),
            )),
        ]);

        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let output = p.parse_program().unwrap();

        assert_eq!(expected, output);
    }

    #[test]
    fn test_inflix() {
        let input = "5 + 5;
        5 - 5; 
        5 * 5;
        5 / 5;
        5 > 5;
        5 < 5;
        5 == 5; 
        5 != 5;";

        let l = lexer::Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let five = Box::new(crate::ast::Expression::Literal(crate::ast::Literal::Int(5)));

        let expected: crate::ast::Program = Vec::from([
            crate::ast::Statement::Expr(crate::ast::Expression::Inflix {
                operator: crate::ast::Inflix::Plus,
                left: five.clone(),
                right: five.clone(),
            }),
            crate::ast::Statement::Expr(crate::ast::Expression::Inflix {
                operator: crate::ast::Inflix::Minus,
                left: five.clone(),
                right: five.clone(),
            }),
            crate::ast::Statement::Expr(crate::ast::Expression::Inflix {
                operator: crate::ast::Inflix::Multiply,
                left: five.clone(),
                right: five.clone(),
            }),
            crate::ast::Statement::Expr(crate::ast::Expression::Inflix {
                operator: crate::ast::Inflix::Divide,
                left: five.clone(),
                right: five.clone(),
            }),
            crate::ast::Statement::Expr(crate::ast::Expression::Inflix {
                operator: crate::ast::Inflix::Greater,
                left: five.clone(),
                right: five.clone(),
            }),
            crate::ast::Statement::Expr(crate::ast::Expression::Inflix {
                operator: crate::ast::Inflix::Lesser,
                left: five.clone(),
                right: five.clone(),
            }),
            crate::ast::Statement::Expr(crate::ast::Expression::Inflix {
                operator: crate::ast::Inflix::Equal,
                left: five.clone(),
                right: five.clone(),
            }),
            crate::ast::Statement::Expr(crate::ast::Expression::Inflix {
                operator: crate::ast::Inflix::NotEqual,
                left: five.clone(),
                right: five,
            }),
        ]);

        let output = p.parse_program().unwrap();

        assert_eq!(output, expected);
    }

    #[test]
    fn test_arithmetic_precedence() {
        let lexer = lexer::Lexer::new("3 * 2 - 1;".to_string());
        let mut p = Parser::new(lexer);
        let output = p.parse_program().unwrap();
        let expected: crate::ast::Program = Vec::from([crate::ast::Statement::Expr(
            crate::ast::Expression::Inflix {
                operator: crate::ast::Inflix::Minus,
                left: Box::new(crate::ast::Expression::Inflix {
                    operator: crate::ast::Inflix::Multiply,
                    left: Box::new(crate::ast::Expression::Literal(crate::ast::Literal::Int(3))),
                    right: Box::new(crate::ast::Expression::Literal(crate::ast::Literal::Int(2))),
                }),
                right: Box::new(crate::ast::Expression::Literal(crate::ast::Literal::Int(1))),
            },
        )]);

        assert_eq!(expected, output);
    }

    #[test]
    fn test_prefix_inflix() {
        let lexer = lexer::Lexer::new("-1 + 2;".to_string());
        let mut p = Parser::new(lexer);
        let output = p.parse_program().unwrap();

        let expected = Vec::from([Statement::Expr(Expression::Inflix {
            operator: Inflix::Plus,
            left: Box::new(Expression::Prefix(
                Prefix::Minus,
                Box::new(Expression::Literal(Literal::Int(1))),
            )),
            right: Box::new(Expression::Literal(Literal::Int(2))),
        })]);
        assert_eq!(expected, output);
    }

    #[test]
    fn test_bool_literal() {
        let lexer = lexer::Lexer::new(
            "true;
            false;"
                .to_string(),
        );
        let mut p = Parser::new(lexer);

        let output = p.parse_program().unwrap();

        let expected = Vec::from([
            Statement::Expr(Expression::Literal(Literal::Bool(true))),
            Statement::Expr(Expression::Literal(Literal::Bool(false))),
        ]);

        assert_eq!(expected, output);
    }

    #[test]
    fn test_parenthesis() {
        let lexer = lexer::Lexer::new("(1 + 2) * 3;".to_string());
        let mut p = Parser::new(lexer);

        let output = p.parse_program().unwrap();

        let expected = Vec::from([Statement::Expr(Expression::Inflix {
            operator: Inflix::Multiply,
            left: Box::new(Expression::Inflix {
                operator: Inflix::Plus,
                left: Box::new(Expression::Literal(Literal::Int(1))),
                right: Box::new(Expression::Literal(Literal::Int(2))),
            }),
            right: Box::new(Expression::Literal(Literal::Int(3))),
        })]);

        assert_eq!(expected, output);
    }

    #[test]
    fn test_if_expression() {
        let lexer = lexer::Lexer::new("if ( x < y ) { x }".to_string());
        let mut p = Parser::new(lexer);
        let output = p.parse_program().unwrap();

        assert_eq!(p.errors().len(), 0);

        let expected = Vec::from([Statement::Expr(Expression::If {
            condition: Box::new(Expression::Inflix {
                operator: Inflix::Lesser,
                left: Box::new(Expression::Ident(Identifier::new("x"))),
                right: Box::new(Expression::Ident(Identifier::new("y"))),
            }),
            consequence: Vec::from([Statement::Expr(Expression::Ident(Identifier::new("x")))]),
            alternative: None,
        })]);
        assert_eq!(expected, output);
    }

    #[test]
    fn test_if_else_expression() {
        let lexer = lexer::Lexer::new("if ( x < y ) { x } else { y }".to_string());
        let mut p = Parser::new(lexer);
        let output = p.parse_program().unwrap();

        assert_eq!(p.errors().len(), 0);

        let expected = Vec::from([Statement::Expr(Expression::If {
            condition: Box::new(Expression::Inflix {
                operator: Inflix::Lesser,
                left: Box::new(Expression::Ident(Identifier::new("x"))),
                right: Box::new(Expression::Ident(Identifier::new("y"))),
            }),
            consequence: Vec::from([Statement::Expr(Expression::Ident(Identifier::new("x")))]),
            alternative: Some(Vec::from([Statement::Expr(Expression::Ident(
                Identifier::new("y"),
            ))])),
        })]);
        assert_eq!(expected, output);
    }

    #[test]
    fn test_functions() {
        let lexer = lexer::Lexer::new("fn(x, y) { x + y; }".to_string());
        let mut p = Parser::new(lexer);
        let output = p.parse_program().unwrap();

        let expected = Vec::from([Statement::Expr(Expression::Function {
            parameters: Vec::from([Identifier::new("x"), Identifier::new("y")]),
            body: Vec::from([Statement::Expr(Expression::Inflix {
                operator: Inflix::Plus,
                left: Box::new(Expression::Ident(Identifier::new("x"))),
                right: Box::new(Expression::Ident(Identifier::new("y"))),
            })]),
        })]);

        assert_eq!(expected, output);
    }

    #[test]
    fn test_call() {
        let lexer = lexer::Lexer::new("add(x, y)".to_string());
        let mut p = Parser::new(lexer);

        let output = p.parse_program().unwrap();

        let expected = Vec::from([Statement::Expr(Expression::Call {
            function: Box::new(Expression::Ident(Identifier::new("add"))),
            arguments: Vec::from([
                Expression::Ident(Identifier::new("x")),
                Expression::Ident(Identifier::new("y")),
            ]),
        })]);

        assert_eq!(expected, output);
    }
}
