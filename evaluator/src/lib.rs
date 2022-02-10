mod environment;
pub mod object;

use environment::Environment;
use parser::ast::{self, Expression, Inflix, Literal, Statement};
use std::{cell::RefCell, rc::Rc, result::Result};

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn eval(&self, program: ast::Program) -> Result<Vec<object::Object>, String> {
        let mut objects = Vec::new();

        for stmt in program {
            match stmt {
                Statement::Expr(Expression::If {
                    condition,
                    consequence,
                    alternative,
                }) => {
                    objects.append(&mut self.eval_if_expression(Expression::If {
                        condition: condition.clone(),
                        consequence: consequence.clone(),
                        alternative: alternative.clone(),
                    })?);
                }
                Statement::Return(expr) => {
                    objects.push(object::Object::Return(Box::new(
                        self.eval_expression(expr)?,
                    )));
                    break;
                }
                Statement::Expr(expr) => objects.push(self.eval_expression(expr)?),
                Statement::Let(ident, expr) => self
                    .env
                    .borrow_mut()
                    .set(ident.get(), self.eval_expression(expr)?),
            }
        }

        Ok(objects)
    }

    fn eval_call(
        &self,
        function: Expression,
        arguments: Vec<Expression>,
    ) -> Result<Vec<object::Object>, String> {
        match function {
            Expression::Function { parameters, body } => {
                let identifiers = parameters;
                let env = Rc::new(RefCell::new(Environment::new()));
                if arguments.len() != identifiers.len() {
                    return Err(format!("Err: The number of arguments provided for the function isn't the same as the number of arguments the function takes!\n Provided: {} - Takes: {}", arguments.len(), identifiers.len()));
                }

                let args = self.eval_expressions(arguments)?;

                for (k, v) in identifiers.into_iter().zip(args) {
                    env.borrow_mut().set(k.get(), v)
                }

                let mut evaluator = Self::new();
                evaluator.env = env;

                evaluator.eval(body)
            }
            Expression::Ident(ref ident) => match self.eval_identifier(ident.to_owned())? {
                object::Object::Function {
                    identifiers,
                    body,
                    env,
                } => {
                    if arguments.len() != identifiers.len() {
                        return Err(format!("Err: The number of arguments provided for the function isn't the same as the number of arguments the function takes!\n Provided: {} - Takes: {}", arguments.len(), identifiers.len()));
                    }

                    let args = self.eval_expressions(arguments)?;

                    for (k, v) in identifiers.into_iter().zip(args) {
                        env.borrow_mut().set(k.get(), v)
                    }

                    let mut evaluator = Self::new();
                    evaluator.env = env;
                    evaluator.eval(body)
                }
                _ => {
                    return Err(format!(
                "Err: Unexpected object received while trying to evaluate a function call! Expected function, found: {function:?}"
            ));
                }
            },
            _ => {
                return Err(format!(
                "Err: Unexpected error while evaluating function call! {function:?} {arguments:?}"
            ))
            }
        }
    }

    #[inline]
    fn eval_expressions(&self, exprs: Vec<Expression>) -> Result<Vec<object::Object>, String> {
        let mut objects = Vec::new();
        for expr in exprs {
            objects.push(self.eval_expression(expr)?);
        }
        Ok(objects)
    }

    #[inline]
    fn eval_literals(&self, lit: Literal) -> object::Object {
        match lit {
            Literal::Int(x) => object::Object::Integer(x),
            Literal::Bool(x) => object::Object::Bool(x),
        }
    }

    fn eval_prefix(&self, operator: ast::Prefix, right: object::Object) -> object::Object {
        match operator {
            ast::Prefix::Not => match right {
                object::Object::Bool(x) => match x {
                    true => object::Object::Bool(false),
                    false => object::Object::Bool(true),
                },
                object::Object::Null => object::Object::Bool(true),
                _ => object::Object::Bool(false),
            },
            ast::Prefix::Minus => match right {
                object::Object::Integer(x) => object::Object::Integer(-x),
                _ => object::Object::Null,
            },
        }
    }

    fn eval_expression(&self, expression: Expression) -> Result<object::Object, String> {
        Ok(match expression {
            Expression::Literal(x) => self.eval_literals(x),
            Expression::Prefix(operator, expr) => {
                let right = self.eval_expression(*expr)?;
                self.eval_prefix(operator, right)
            }
            Expression::Inflix {
                operator,
                left,
                right,
            } => {
                let left = self.eval_expression(*left)?;
                let right = self.eval_expression(*right)?;
                self.eval_inflix_expression(&left, &right, operator)?
            }
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                let output = self.eval_if_expression(Expression::If {
                    condition,
                    consequence,
                    alternative,
                })?;
                output.last().unwrap_or(&object::Object::Null).to_owned()
            }
            Expression::Function { parameters, body } => object::Object::Function {
                identifiers: parameters,
                body,
                env: Rc::new(RefCell::new(Environment::new())),
            },
            Expression::Ident(ident) => self.eval_identifier(ident)?,
            Expression::Call {
                function,
                arguments,
            } => {
                let evaluated = self.eval_call(*function, arguments)?;
                evaluated.last().unwrap_or(&object::Object::Null).to_owned()
            }
            _ => todo!(),
        })
    }

    #[inline]
    fn eval_identifier(&self, ident: ast::Identifier) -> Result<object::Object, String> {
        match self.env.borrow().get(ident.get()) {
            Some(x) => Ok(x),
            None => Err(format!(
                "Err: Undeclared identifier {:?} used!",
                ident.get()
            )),
        }
    }

    fn eval_inflix_expression(
        &self,
        left: &object::Object,
        right: &object::Object,
        operator: ast::Inflix,
    ) -> Result<object::Object, String> {
        let left = match left {
            object::Object::Bool(_) => {
                match operator {
                    Inflix::Equal => return Ok(object::Object::Bool(left == right)),
                    Inflix::NotEqual => return Ok(object::Object::Bool(left != right)),
                    _ => {
                        return Err(format!(
                            "Err: Cannot perform {right:?} {operator:?} {left:?} operation!"
                        ))
                    }
                };
            }
            object::Object::Null => {
                match operator {
                    Inflix::Equal => return Ok(object::Object::Bool(left == right)),
                    Inflix::NotEqual => return Ok(object::Object::Bool(left != right)),
                    _ => {
                        return Err(format!(
                            "Err: Cannot perform {right:?} {operator:?} {left:?} operation!"
                        ))
                    }
                };
            }
            object::Object::Integer(x) => x,
            _ => {
                return Err(format!(
                    "Err: Cannot perform {right:?} {operator:?} {left:?} operation!"
                ))
            }
        };
        let right = match right {
            object::Object::Integer(x) => x,
            _ => {
                return Err(format!(
                    "Err: Cannot perform {right:?} {operator:?} {left:?} operation!"
                ))
            }
        };
        Ok(match operator {
            Inflix::Plus => object::Object::Integer(left + right),
            Inflix::Minus => object::Object::Integer(left - right),
            Inflix::Multiply => object::Object::Integer(left * right),
            Inflix::Divide => object::Object::Integer(left / right),
            Inflix::Lesser => object::Object::Bool(left < right),
            Inflix::Greater => object::Object::Bool(left > right),
            Inflix::LessEqual => object::Object::Bool(left <= right),
            Inflix::GreaterEqual => object::Object::Bool(left >= right),
            Inflix::Equal => object::Object::Bool(left == right),
            Inflix::NotEqual => object::Object::Bool(left != right),
        })
    }

    fn is_truthy(&self, condition: object::Object) -> bool {
        !matches!(
            condition,
            object::Object::Bool(false) | object::Object::Null
        )
    }

    fn eval_if_expression(&self, expression: Expression) -> Result<Vec<object::Object>, String> {
        Ok(match expression {
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                if self.is_truthy(self.eval_expression(*condition)?) {
                    self.eval(consequence)?
                } else {
                    match alternative {
                        Some(x) => self.eval(x)?,
                        None => vec![],
                    }
                }
            }
            _ => {
                return Err(
                    "Err: Internal Interpreter error! The type of expression is not If!"
                        .to_string(),
                )
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{object::Object, Evaluator};

    #[test]
    fn test_eval_literals_expression() {
        let p = parser::Parser::new(lexer::Lexer::new("5; false 10; true;".to_string()));

        assert!(p.errors().borrow().len() == 0);

        let evaluator = Evaluator::new();
        let output = evaluator.eval(p.parse_program().unwrap()).unwrap();

        let expected = Vec::from([
            Object::Integer(5),
            Object::Bool(false),
            Object::Integer(10),
            Object::Bool(true),
        ]);

        assert_eq!(expected, output);
    }

    #[test]
    fn test_prefix() {
        let p = parser::Parser::new(lexer::Lexer::new(
            "!true;
                !false;
                !5;
                !!true
                !!false
                !!5;
                -5;
                10;
                -15;"
                .to_string(),
        ));

        assert!(p.errors().borrow().len() == 0);

        let evaluator = Evaluator::new();
        let output = evaluator.eval(p.parse_program().unwrap()).unwrap();

        let expected = Vec::from([
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
            Object::Integer(-5),
            Object::Integer(10),
            Object::Integer(-15),
        ]);

        assert_eq!(expected, output);
    }

    #[test]
    fn test_inflix() {
        let p = parser::Parser::new(lexer::Lexer::new(
            "
                5 + 5 + 5 + 5;
                5 + 5 - 10;
                3 * 6 - 21;
                5 * 2 - 3;
                3 + 8 / 2 * 2;
                (2 + 4) / (3 - 2);
                true != 3; 
                3 < 2; 
                true == true; 
                false != true; 
                4 <= 1;
                1 <= 3; 
                3 >= 2; 
                3 >= 3;
                4 <= 4; 
                "
            .to_string(),
        ));

        assert!(p.errors().borrow().len() == 0);

        let evaluator = Evaluator::new();
        let output = evaluator.eval(p.parse_program().unwrap()).unwrap();

        let expected = Vec::from([
            Object::Integer(20),
            Object::Integer(0),
            Object::Integer(-3),
            Object::Integer(7),
            Object::Integer(11),
            Object::Integer(6),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(true),
            Object::Bool(true),
            Object::Bool(true),
        ]);

        assert_eq!(expected, output);
    }

    #[test]
    fn test_if_else() {
        let p = parser::Parser::new(lexer::Lexer::new(
            "if (2 < 3) { 3 }; 
                if (3 < 2) { 2 } else { 3 };
                if (false) { return 4; };"
                .to_string(),
        ));

        assert!(p.errors().borrow().len() == 0);

        let evaluator = Evaluator::new();
        let output = evaluator.eval(p.parse_program().unwrap()).unwrap();

        let expected = Vec::from([Object::Integer(3), Object::Integer(3)]);

        assert_eq!(expected, output);
    }

    #[test]
    fn test_return() {
        let p = parser::Parser::new(lexer::Lexer::new("return 3; ".to_string()));

        assert!(p.errors().borrow().len() == 0);

        let evaluator = Evaluator::new();
        let output = evaluator.eval(p.parse_program().unwrap()).unwrap();

        let expected = Vec::from([Object::Return(Box::new(Object::Integer(3)))]);

        assert_eq!(expected, output);

        let p = parser::Parser::new(lexer::Lexer::new("return !false; 2 + false;".to_string()));

        assert!(p.errors().borrow().len() == 0);

        let evaluator = Evaluator::new();
        let output = evaluator.eval(p.parse_program().unwrap()).unwrap();

        let expected = Vec::from([Object::Return(Box::new(Object::Bool(true)))]);

        assert_eq!(expected, output);
    }

    #[test]
    fn test_let() {
        let p = parser::Parser::new(lexer::Lexer::new(
            " let x = 434; let b = 433; x - b;".to_string(),
        ));

        assert!(p.errors().borrow().len() == 0);

        let evaluator = Evaluator::new();
        let output = evaluator.eval(p.parse_program().unwrap()).unwrap();

        let expected = Vec::from([Object::Integer(1)]);

        assert_eq!(expected, output);
    }

    #[test]
    fn test_function_calls() {
        let p = parser::Parser::new(lexer::Lexer::new(
            "let add = fn (x, y) { return x + y; }; let x = 2; add(x + 2, 3 + 3);".to_string(),
        ));

        assert!(p.errors().borrow().len() == 0);

        let evaluator = Evaluator::new();
        let output = evaluator.eval(p.parse_program().unwrap()).unwrap();

        let expected = Vec::from([Object::Return(Box::new(Object::Integer(10)))]);

        eprintln!("{output:?}");

        assert_eq!(expected, output);
    }
}
