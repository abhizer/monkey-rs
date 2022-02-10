use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::environment::Environment;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Bool(bool),
    Return(Box<Object>),
    Function {
        identifiers: Vec<parser::ast::Identifier>,
        body: parser::ast::Program,
        env: Rc<RefCell<Environment>>,
    },
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(x) => write!(f, "{x}"),
            Object::Bool(x) => write!(f, "{x}"),
            Object::Return(x) => write!(f, "{x}"),
            Object::Function { .. } => unreachable!(),
            Object::Null => write!(f, ""),
        }
    }
}
