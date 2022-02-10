#![feature(let_chains)]
pub mod ast;
use lexer::token::Token;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<lexer::token::Token>,
    index: RefCell<usize>, // Index of current token
    errors: Rc<RefCell<Vec<String>>>,
}
impl Parser {
    pub fn new(lexer: lexer::Lexer) -> Self {
        Self {
            tokens: lexer.tokenize(),
            index: RefCell::new(0),
            errors: Rc::new(RefCell::new(Vec::new())),
        }
    }

    fn parse_let(&self) -> Option<ast::Statement> {
        let mut position = self.index.clone().into_inner() + 1;
        self.index.replace(position);

        let next_token = self.tokens.get(position)?;

        let ident = match next_token {
            Token::Ident(x) => ast::Identifier::new(x),
            unexpected => {
                self.err_unexpected_token(
                    Token::Ident("<identifier>".to_string()),
                    unexpected.to_owned(),
                    Token::Let,
                );
                return None;
            }
        };

        position += 1;
        self.index.replace(position);

        match self.tokens.get(position) {
            None => {
                self.err_unexpected_token(Token::Assignment, Token::Eof, next_token.to_owned());
                return None;
            }
            Some(Token::Assignment) => {}
            Some(unexpected) => {
                self.err_unexpected_token(
                    Token::Assignment,
                    unexpected.to_owned(),
                    next_token.to_owned(),
                );
            }
        }

        position += 1;
        self.index.replace(position);

        let expr = self.parse_expression(ast::Precendence::Lowest)?;

        if Some(&Token::Semicolon) == self.tokens.get(position) {
            self.index.replace(position);
        }

        Some(ast::Statement::Let(ident, expr))
    }

    fn parse_return(&self) -> Option<ast::Statement> {
        let mut index = self.index.clone().into_inner();
        self.index.replace(index + 1);

        let expr = self.parse_expression(ast::Precendence::Lowest)?;

        index = self.index.clone().into_inner();

        if Some(&Token::Semicolon) == self.tokens.get(index + 1) {
            self.index.replace(index + 1);
        } else {
            self.err_unexpected_token(
                Token::Semicolon,
                self.tokens.get(index + 1).unwrap_or(&Token::Eof).to_owned(),
                self.tokens.get(index).unwrap_or(&Token::Return).to_owned(),
            );
            return None;
        }

        Some(ast::Statement::Return(expr))
    }

    fn parse_identifier(&self, t: &str) -> ast::Expression {
        ast::Expression::Ident(ast::Identifier::new(t))
    }

    fn parse_int_literal(&self, n: &str) -> Option<ast::Expression> {
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

    fn parse_bool_literal(&self) -> Option<ast::Expression> {
        Some(ast::Expression::Literal(ast::Literal::Bool(
            match self.tokens.get(*self.index.borrow()) {
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

    fn parse_prefix(&self) -> Option<ast::Expression> {
        let position = *self.index.borrow();
        let prefix_operator = match self.tokens.get(position) {
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

        self.index.replace(position + 1);
        let right = self.parse_expression(ast::Precendence::Prefix)?;

        Some(ast::Expression::Prefix(prefix_operator, Box::new(right)))
    }

    fn parse_inflix(&self, left: ast::Expression) -> Option<ast::Expression> {
        let index = *self.index.borrow();
        let current_token = self.tokens.get(index)?;
        let precendence = ast::Precendence::get_precendence(current_token);
        self.index.replace(index + 1);
        let right = self.parse_expression(precendence)?;
        Some(ast::Expression::Inflix {
            operator: ast::Inflix::from_token(current_token)?,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn parse_grouped_expression(&self) -> Option<ast::Expression> {
        let mut index = self.index.clone().into_inner() + 1;
        self.index.replace(index);

        let expr = self.parse_expression(ast::Precendence::Lowest)?;

        index = self.index.clone().into_inner();

        if Some(&Token::Rparen) != self.tokens.get(index + 1) {
            self.err_unexpected_token(
                Token::Rparen,
                self.tokens.get(index + 1).unwrap_or(&Token::Eof).to_owned(),
                self.tokens.get(index).unwrap_or(&Token::Lparen).to_owned(),
            );
            return None;
        }
        self.index.replace(index + 1);

        Some(expr)
    }

    fn parse_if_expression(&self) -> Option<ast::Expression> {
        let mut index = self.index.clone().into_inner();
        if Some(&Token::Lparen) != self.tokens.get(index + 1) {
            self.err_unexpected_token(
                Token::Lparen,
                self.tokens.get(index + 1).unwrap_or(&Token::Eof).to_owned(),
                Token::If,
            );
            return None;
        }
        self.index.replace(index + 2); // Increase self.index by 2 as, tokens should be: If, Lparen <condition>

        let condition = self.parse_expression(ast::Precendence::Lowest)?;

        index = self.index.clone().into_inner();

        if Some(&Token::Rparen) != self.tokens.get(index + 1) {
            self.err_unexpected_token(
                Token::Rparen,
                self.tokens.get(index + 1).unwrap_or(&Token::Eof).to_owned(),
                self.tokens.get(index).unwrap_or(&Token::Lparen).to_owned(),
            );
            return None;
        }

        self.index.replace(index + 1); // Increase self.index by 1 as, tokens should be: Rparen Lbrace
        index = self.index.clone().into_inner();

        if Some(&Token::Lbrace) != self.tokens.get(index + 1) {
            self.err_unexpected_token(
                Token::Lbrace,
                self.tokens.get(index + 1).unwrap_or(&Token::Eof).to_owned(),
                self.tokens.get(index).unwrap_or(&Token::Rparen).to_owned(),
            );
            return None;
        }

        self.index.replace(index + 2); // Increase self.index by 2, So that it now points to the Expression after Lbrace

        let consequence = self.parse_blocks()?;

        index = self.index.clone().into_inner();
        let mut alternative = None;

        if Some(&Token::Else) == self.tokens.get(index + 1) {
            index += 1;
            self.index.replace(index);
            if Some(&Token::Lbrace) != self.tokens.get(index + 1) {
                self.err_unexpected_token(
                    Token::Lbrace,
                    self.tokens.get(index + 1).unwrap_or(&Token::Eof).to_owned(),
                    self.tokens.get(index).unwrap_or(&Token::Rparen).to_owned(),
                );
                return None;
            }

            index += 2;
            self.index.replace(index);

            alternative = self.parse_blocks();
        }

        let ret = ast::Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        };

        Some(ret)
    }

    fn parse_blocks(&self) -> Option<ast::Program> {
        let mut block: ast::Program = Vec::new();

        while let x = self.tokens.get(self.index.clone().into_inner()) && x.is_some() && x != Some(&Token::Rbrace) {
            let stmt = self.parse_statement(x.unwrap())?;
            self.index.replace(self.index.clone().into_inner() + 1);
            block.push(stmt);
        }

        Some(block)
    }

    fn parse_functions(&self) -> Option<ast::Expression> {
        let mut index = self.index.clone().into_inner();

        if Some(&Token::Lparen) != self.tokens.get(index + 1) {
            self.err_unexpected_token(
                Token::Lparen,
                self.tokens.get(index + 1).unwrap_or(&Token::Eof).to_owned(),
                Token::Function,
            );
            return None;
        }

        index += 1;
        self.index.replace(index);

        let parameters = self.parse_function_parameters()?;

        index = self.index.clone().into_inner() + 1;

        if Some(&Token::Lbrace) != self.tokens.get(index) {
            self.err_unexpected_token(
                Token::Lbrace,
                self.tokens.get(index).unwrap_or(&Token::Eof).to_owned(),
                Token::Rparen,
            );
            return None;
        }

        index += 1;
        self.index.replace(index);

        let body = self.parse_blocks()?;

        Some(ast::Expression::Function { parameters, body })
    }

    fn parse_function_parameters(&self) -> Option<Vec<ast::Identifier>> {
        let mut identifiers = Vec::new();
        let mut index = self.index.clone().into_inner();

        if Some(&Token::Rparen) == self.tokens.get(index + 1) {
            self.index.replace(index + 1);
            return Some(identifiers);
        }

        index += 1;
        self.index.replace(index);

        match self.tokens.get(index)? {
            Token::Ident(x) => {
                identifiers.push(ast::Identifier::new(x));
                index += 1;
                self.index.replace(index);
            }
            unexpected => {
                self.err_unexpected_token(
                    Token::Ident("<identifier>".to_string()),
                    unexpected.to_owned(),
                    self.tokens.get(index).unwrap_or(&Token::Eof).to_owned(),
                );
                return None;
            }
        };

        while Some(&Token::Comma) == self.tokens.get(index) {
            index += 1;
            self.index.replace(index);

            match self.tokens.get(index)? {
                Token::Ident(x) => {
                    identifiers.push(ast::Identifier::new(x));
                    index += 1;
                    self.index.replace(index);
                }
                unexpected => {
                    self.err_unexpected_token(
                        Token::Ident("<identifier>".to_string()),
                        unexpected.to_owned(),
                        Token::Lparen,
                    );
                    return None;
                }
            };
        }

        if Some(&Token::Rparen) != self.tokens.get(index) {
            self.err_unexpected_token(
                Token::Rparen,
                self.tokens.get(index).unwrap_or(&Token::Eof).to_owned(),
                self.tokens
                    .get(index - 1)
                    .unwrap_or(&Token::Illegal) // This should always unwrap just fine
                    .to_owned(),
            );
            return None;
        }

        Some(identifiers)
    }

    fn parse_call_arguments(&self) -> Option<Vec<ast::Expression>> {
        let mut args = Vec::new();
        let mut index = self.index.clone().into_inner();

        if Some(&Token::Rparen) == self.tokens.get(index + 1) {
            return Some(args);
        }

        index += 1;
        self.index.replace(index);

        args.push(self.parse_expression(ast::Precendence::Lowest)?);

        index = self.index.clone().into_inner();

        while Some(&Token::Comma) == self.tokens.get(index + 1) {
            index += 2;
            self.index.replace(index);
            args.push(self.parse_expression(ast::Precendence::Lowest)?);
            index = self.index.clone().into_inner();
        }

        index = self.index.clone().into_inner();

        if Some(&Token::Rparen) != self.tokens.get(index + 1) {
            self.err_unexpected_token(
                Token::Rparen,
                self.tokens.get(index + 1).unwrap_or(&Token::Eof).to_owned(),
                self.tokens
                    .get(index)
                    .unwrap_or(&Token::Illegal) // This should always unwrap just fine
                    .to_owned(),
            );
            return None;
        }

        Some(args)
    }

    fn parse_call_expression(&self, function: ast::Expression) -> Option<ast::Expression> {
        let arguments = self.parse_call_arguments()?;
        Some(ast::Expression::Call {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_expression(&self, precendence: ast::Precendence) -> Option<ast::Expression> {
        let current_token = self
            .tokens
            .get(*self.index.borrow())
            .unwrap_or(&lexer::token::Token::Eof);

        let mut left = match current_token {
            Token::Ident(x) => Some(self.parse_identifier(x)),
            Token::Int(x) => self.parse_int_literal(x),
            Token::Bang | Token::Minus => self.parse_prefix(),
            Token::True | Token::False => self.parse_bool_literal(),
            Token::Lparen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_functions(),
            _ => return None,
        };
        let mut index = self.index.clone().into_inner();

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
                    self.index.replace(index + 1);
                    let val = self.parse_inflix(left.to_owned()?);
                    index = self.index.clone().into_inner();
                    val
                }
                Token::Lparen => {
                    self.index.replace(index + 1);
                    let val = self.parse_call_expression(left.to_owned()?);
                    index = self.index.clone().into_inner();
                    val
                }
                _ => return left,
            };
        }

        left
    }

    fn parse_expression_statement(&self) -> Option<ast::Statement> {
        let stmt = self.parse_expression(ast::Precendence::Lowest)?;
        let index = *self.index.borrow();
        if Some(&Token::Semicolon) == self.tokens.get(index + 1) {
            self.index.replace(index + 1);
        }
        Some(ast::Statement::Expr(stmt))
    }

    fn parse_statement(&self, tok: &lexer::token::Token) -> Option<ast::Statement> {
        match tok {
            Token::Let => self.parse_let(),
            Token::Return => self.parse_return(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_program(&self) -> Option<ast::Program> {
        let mut program: ast::Program = Vec::new();

        while let Some(token) = self.tokens.get(self.index.clone().into_inner()) {
            if let Some(x) = self.parse_statement(token) {
                program.push(x);
            }
            self.index.replace(self.index.clone().into_inner() + 1);
        }

        if program.is_empty() {
            return None;
        }

        if self.errors().borrow().len() != 0 {
            for error in &*self.errors().borrow() {
                println!("{}", error);
            }
        }

        Some(program)
    }

    pub fn errors(&self) -> Rc<RefCell<Vec<String>>> {
        self.errors.clone()
    }

    fn err_unexpected_token(&self, expected: Token, got: Token, current: Token) {
        self.errors.borrow_mut().push(format!(
            "Parse Err: Unexpected token after {:?}! Expected: {:?} but Got {:?}",
            current, expected, got
        ));
    }

    fn err_unexpected(&self, err: String) {
        self.errors.borrow_mut().push(err);
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
        let p = Parser::new(l);

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
        let p = Parser::new(l);

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
        let p = Parser::new(l);

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
        let p = Parser::new(l);

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
        let p = Parser::new(l);
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
        let p = Parser::new(l);

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
        let p = Parser::new(lexer);
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
        let p = Parser::new(lexer);
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
        let p = Parser::new(lexer);

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
        let p = Parser::new(lexer);

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
        let p = Parser::new(lexer);
        let output = p.parse_program().unwrap();

        assert_eq!(p.errors().borrow().len(), 0);

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
        let p = Parser::new(lexer);
        let output = p.parse_program().unwrap();

        assert_eq!(p.errors().borrow().len(), 0);

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
        let p = Parser::new(lexer);
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
        let p = Parser::new(lexer);

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
