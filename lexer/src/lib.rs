pub mod token;

/// Defines the Lexer, it only has one field, input, that takes a String
#[derive(Debug, PartialEq)]
pub struct Lexer {
    input: String,
}

impl Lexer {
    /// Create a new Lexer from a string input
    pub fn new(input: String) -> Self {
        Self { input }
    }

    /// Splits the identifiers (also works for Int as it calls the token::get_token() method) and returns a vector of tokens
    fn split_identifier(mut s: &str) -> Vec<token::Token> {
        let mut index = 0;
        let mut tokens = Vec::new();
        let mut chars = s.chars().peekable();

        // Is the current character a part of an identifier?
        while let Some(c) = chars.next() {
            if ('a'..='z').contains(&c)
                || ('A'..='Z').contains(&c)
                || ('0'..='9').contains(&c)
                || c == '_'
            {
                // If it is, increase the index by the character's length
                index += c.len_utf8();
            } else {
                if index != 0 && index <= s.len() {
                    // If a character that isn't a valid identifier or Int is encountered, it
                    // pushes the previous characters as a token
                    tokens.push(token::get_token(&s[..index]));
                    s = s.get(index..).unwrap();
                    index = 0;
                }
                // Is the character a part of two chars special operator like == | != | >= | <=?
                if (c == '!' || c == '=' || c == '>' || c == '<') && (Some(&'=') == chars.peek()) {
                    let token_recv = token::get_token(&format!("{}{}", c, &chars.peek().unwrap()));
                    tokens.push(token_recv);
                    s = s
                        .get((index + c.len_utf8() + chars.peek().unwrap().len_utf8())..)
                        .unwrap_or_else(|| unreachable!());
                    chars = s.chars().peekable();
                } else {
                    // If it reaches here, just call token::get_token() for the char and push it
                    tokens.push(token::get_token(&c.to_string()));
                    s = s.get(c.len_utf8()..).unwrap();
                }
            }
        }

        // If the &str to parse only had identifiers or ended with identifiers, this is called
        if index != 0 {
            if let Some(x) = s.get(..index) {
                tokens.push(token::get_token(x))
            }
        }

        tokens
    }

    /// Create a vector of tokens from the input that was given to the lexer
    pub fn tokenize(&self) -> Vec<token::Token> {
        let mut tokens = Vec::new();
        let input = self.input.split_whitespace();
        for word in input {
            if let Some(tok) = token::TOKENS.get(word).cloned() {
                tokens.push(tok);
            } else {
                tokens.append(&mut Self::split_identifier(word));
            }
        }

        tokens
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;
    use crate::Lexer;
    #[test]
    fn new_test() {
        assert_eq!(
            Lexer {
                input: "a".to_string(),
            },
            Lexer::new("a".to_string())
        );
    }

    #[test]
    fn mod_read_identifier_test() {
        let input = "let;x=3;";
        let expected = vec![
            Token::Let,
            Token::Semicolon,
            Token::Ident("x".to_string()),
            Token::Assignment,
            Token::Int("3".to_string()),
            Token::Semicolon,
        ];
        let reality = Lexer::split_identifier(input);
        assert_eq!(expected, reality);
    }

    #[test]
    fn tokenize_test() {
        let input = "let x = 5;
        return alpha;
        !2<>4*
            if a 
            else b
                true
                    and 
                    false
                    fn();
        {hel-lo/ wor,ld}
        ";
        let output = vec![
            Token::Let,
            Token::Ident("x".to_string()),
            Token::Assignment,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Return,
            Token::Ident("alpha".to_string()),
            Token::Semicolon,
            Token::Bang,
            Token::Int("2".to_string()),
            Token::LT,
            Token::GT,
            Token::Int("4".to_string()),
            Token::Asterisk,
            Token::If,
            Token::Ident("a".to_string()),
            Token::Else,
            Token::Ident("b".to_string()),
            Token::True,
            Token::Ident("and".to_string()),
            Token::False,
            Token::Function,
            Token::Lparen,
            Token::Rparen,
            Token::Semicolon,
            Token::Lbrace,
            Token::Ident("hel".to_string()),
            Token::Minus,
            Token::Ident("lo".to_string()),
            Token::Slash,
            Token::Ident("wor".to_string()),
            Token::Comma,
            Token::Ident("ld".to_string()),
            Token::Rbrace,
        ]
        .into_iter();

        let tokenizer_vec = Lexer::new(input.to_string()).tokenize().into_iter();
        assert!(output.eq(tokenizer_vec));
    }

    #[test]
    fn test_doublesymbols() {
        let input = "!=x==y>=z<=r";
        let expected = vec![
            Token::NotEqual,
            Token::Ident("x".to_string()),
            Token::Equal,
            Token::Ident("y".to_string()),
            Token::GE,
            Token::Ident("z".to_string()),
            Token::LE,
            Token::Ident("r".to_string()),
        ];

        let tokenizer_vec = Lexer::new(input.to_string()).tokenize();
        assert!(expected.eq(&tokenizer_vec));
    }
}
