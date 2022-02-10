use lexer::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(String);

impl Identifier {
    pub fn new(s: &str) -> Self {
        Self(s.to_string())
    }

    pub fn get(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Prefix {
    Not, // Bang -> !
    Minus,
    // Plus,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Ident(Identifier),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Inflix {
        operator: Inflix,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: Program,
        alternative: Option<Program>,
    },
    Function {
        parameters: Vec<Identifier>,
        body: Program,
    },
    Call {
        function: Box<Expression>, // The identifier or Function Literal
        arguments: Vec<Expression>,
    },
    None, // temporary
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expr(Expression),
}

pub type Program = Vec<Statement>;

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precendence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl Precendence {
    pub fn get_precendence(t: &Token) -> Self {
        match t {
            Token::Equal => Precendence::Equals,
            Token::NotEqual => Precendence::Equals,
            Token::LT => Precendence::LessGreater,
            Token::GT => Precendence::LessGreater,
            Token::LE => Precendence::LessGreater,
            Token::GE => Precendence::LessGreater,
            Token::Plus => Precendence::Sum,
            Token::Minus => Precendence::Sum,
            Token::Slash => Precendence::Product,
            Token::Asterisk => Precendence::Product,
            Token::Lparen => Precendence::Call,
            _ => Precendence::Lowest,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Inflix {
    Plus,
    Minus,
    Multiply,
    Divide,
    Lesser,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
}

impl Inflix {
    pub fn from_token(t: &Token) -> Option<Self> {
        match t {
            Token::Plus => Some(Inflix::Plus),
            Token::Minus => Some(Inflix::Minus),
            Token::Asterisk => Some(Inflix::Multiply),
            Token::Slash => Some(Inflix::Divide),
            Token::LT => Some(Inflix::Lesser),
            Token::GT => Some(Inflix::Greater),
            Token::LE => Some(Inflix::LessEqual),
            Token::GE => Some(Inflix::GreaterEqual),
            Token::Equal => Some(Inflix::Equal),
            Token::NotEqual => Some(Inflix::NotEqual),
            _ => None,
        }
    }
}
