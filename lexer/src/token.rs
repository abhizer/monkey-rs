use phf::phf_map;
use regex::Regex;

/// Tokens in Monkey-rs are defined in this Enum
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    // Identifiers & Literals
    Ident(String),
    Int(String),

    // Operators
    Assignment,
    LT,
    GT,
    Asterisk,
    Slash,
    Plus,
    Minus,
    Bang,
    Equal,
    NotEqual,
    GE,
    LE,

    // Delimiters
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

/// A constant map that stores the string representation of the tokens with their respective values
/// in the Token enum
pub const TOKENS: phf::Map<&'static str, Token> = phf_map! {
    "=" => Token::Assignment,
    "<" => Token::LT,
    ">" => Token::GT,
    "*" => Token::Asterisk,
    "/" => Token::Slash,
    "+" => Token::Plus,
    "-" => Token::Minus,
    "!" => Token::Bang,
    "," => Token::Comma,
    "==" => Token::Equal,
    "!=" => Token::NotEqual,
    ">=" => Token::GE,
    "<=" => Token::LE,
    ";" => Token::Semicolon,
    "(" => Token::Lparen,
    ")" => Token::Rparen,
    "{" => Token::Lbrace,
    "}" => Token::Rbrace,
    "fn" => Token::Function,
    "let" => Token::Let,
    "if" => Token::If,
    "else" => Token::Else,
    "return" => Token::Return,
    "true" => Token::True,
    "false" => Token::False,
};

/// Takes a &str and returns a Token
/// First checks if the TOKENS map contains the passed &str's token, if it does, returns it
/// else, it checks if it matches the regex for an identifier, if it does, it returns
/// Token::Ident(s) else, it checks if s is a valid Integer and if it is returns Token::Int(s)
/// else, It checks if the str is empty, if it is, returns Token::Eof, else returns Token::Illegal
pub fn get_token(s: &str) -> Token {
    match TOKENS.contains_key(s) {
        true => TOKENS.get(s).unwrap().to_owned(),
        false => match Regex::new(r"^[a-zA-Z_][0-9a-zA-Z_]*$").unwrap().is_match(s) {
            true => Token::Ident(s.to_string()),
            false => match s.parse::<i64>() {
                Ok(_) => Token::Int(s.to_string()),
                _ => match s {
                    "" => Token::Eof,
                    _ => Token::Illegal,
                },
            },
        },
    }
}
