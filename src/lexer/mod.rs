pub mod token;

use crate::span::Span;
use thiserror::Error;
pub use token::{Token, TokenType};

/// Errors that can occur during lexical analysis
#[derive(Debug, Error, Clone, PartialEq)]
pub enum LexError {
    #[error("Unexpected character '{char}' at {span:?}")]
    UnexpectedChar { char: char, span: Span },
    
    #[error("Unterminated string literal at {span:?}")]
    UnterminatedString { span: Span },
    
    #[error("Invalid number format at {span:?}")]
    InvalidNumber { span: Span },
    
    #[error("Unexpected end of file")]
    UnexpectedEof,
}
    
/// Lexer for tokenizing pixdsl source code
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    current_char: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            current_char: None,
        };
        lexer.current_char = lexer.input.chars().next();
        lexer
    }
    
    fn advance(&mut self) {
        self.position += self.current_char.map(|c| c.len_utf8()).unwrap_or(0);
        self.current_char = self.input.chars().nth(self.position);
    }
    
    fn peek(&self) -> Option<char> {
        self.input.chars().nth(self.position + 1)
    }
    
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }
    
    fn read_number(&mut self) -> Result<f64, LexError> {
        let start = self.position;
        
        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() || ch == '.' {
                self.advance();
            } else {
                break;
            }
        }
        
        let number_str = &self.input[start..self.position];
        number_str.parse().map_err(|_| LexError::InvalidNumber {
            span: Span::new(start, self.position),
        })
    }
    
    fn read_identifier(&mut self) -> String {
        let start = self.position;
        
        while let Some(ch) = self.current_char {
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }
        
        self.input[start..self.position].to_string()
    }
    
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();
        
        let start_pos = self.position;
        
        match self.current_char {
            None => Ok(Token::new(TokenType::Eof, Span::new(start_pos, start_pos))),
            
            Some(ch) => {
                match ch {
                    '+' => {
                        self.advance();
                        Ok(Token::new(TokenType::Plus, Span::new(start_pos, self.position)))
                    }
                    '-' => {
                        self.advance();
                        Ok(Token::new(TokenType::Minus, Span::new(start_pos, self.position)))
                    }
                    '*' => {
                        self.advance();
                        Ok(Token::new(TokenType::Multiply, Span::new(start_pos, self.position)))
                    }
                    '/' => {
                        self.advance();
                        Ok(Token::new(TokenType::Divide, Span::new(start_pos, self.position)))
                    }
                    '=' => {
                        self.advance();
                        Ok(Token::new(TokenType::Assign, Span::new(start_pos, self.position)))
                    }
                    '(' => {
                        self.advance();
                        Ok(Token::new(TokenType::LeftParen, Span::new(start_pos, self.position)))
                    }
                    ')' => {
                        self.advance();
                        Ok(Token::new(TokenType::RightParen, Span::new(start_pos, self.position)))
                    }
                    '{' => {
                        self.advance();
                        Ok(Token::new(TokenType::LeftBrace, Span::new(start_pos, self.position)))
                    }
                    '}' => {
                        self.advance();
                        Ok(Token::new(TokenType::RightBrace, Span::new(start_pos, self.position)))
                    }
                    ',' => {
                        self.advance();
                        Ok(Token::new(TokenType::Comma, Span::new(start_pos, self.position)))
                    }
                    ';' => {
                        self.advance();
                        Ok(Token::new(TokenType::Semicolon, Span::new(start_pos, self.position)))
                    }
                    '.' => {
                        self.advance();
                        Ok(Token::new(TokenType::Dot, Span::new(start_pos, self.position)))
                    }
                    
                    '0'..='9' => {
                        let number = self.read_number()?;
                        Ok(Token::new(TokenType::Float(number), Span::new(start_pos, self.position)))
                    }
                    
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let identifier = self.read_identifier();
                        let token_type = match identifier.as_str() {
                            "let" => TokenType::Let,
                            "float" => TokenType::Float32,
                            "vec2" => TokenType::Vec2,
                            "vec3" => TokenType::Vec3,
                            "vec4" => TokenType::Vec4,
                            _ => TokenType::Identifier(identifier),
                        };
                        Ok(Token::new(token_type, Span::new(start_pos, self.position)))
                    }
                    
                    _ => {
                        let error_char = ch;
                        let error_span = Span::new(start_pos, self.position + ch.len_utf8());
                        self.advance(); // Important: advance past the error character
                        Err(LexError::UnexpectedChar {
                            char: error_char,
                            span: error_span,
                        })
                    }
                }
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexError>;
    
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token { token_type: TokenType::Eof, .. }) => None,
            result => Some(result),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple_expression() {
        let mut lexer = Lexer::new("1.0 + 2.0");
        
        let tokens: Result<Vec<_>, _> = lexer.collect();
        assert!(tokens.is_ok());
        
        let tokens = tokens.unwrap();
        assert_eq!(tokens.len(), 3);
        
        match &tokens[0].token_type {
            TokenType::Float(f) => assert_eq!(*f, 1.0),
            _ => panic!("Expected float"),
        }
        
        assert_eq!(tokens[1].token_type, TokenType::Plus);
        
        match &tokens[2].token_type {
            TokenType::Float(f) => assert_eq!(*f, 2.0),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_tokenize_keywords() {
        let mut lexer = Lexer::new("let vec4");
        
        let tokens: Result<Vec<_>, _> = lexer.collect();
        assert!(tokens.is_ok());
        
        let tokens = tokens.unwrap();
        assert_eq!(tokens[0].token_type, TokenType::Let);
        assert_eq!(tokens[1].token_type, TokenType::Vec4);
    }

    #[test]
    fn test_tokenize_identifiers() {
        let mut lexer = Lexer::new("myVar _test123");
        
        let tokens: Result<Vec<_>, _> = lexer.collect();
        assert!(tokens.is_ok());
        
        let tokens = tokens.unwrap();
        match &tokens[0].token_type {
            TokenType::Identifier(name) => assert_eq!(name, "myVar"),
            _ => panic!("Expected identifier"),
        }
        
        match &tokens[1].token_type {
            TokenType::Identifier(name) => assert_eq!(name, "_test123"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_vector_constructor() {
        let mut lexer = Lexer::new("vec4(1.0, 0.0, 0.0, 1.0)");
        
        let tokens: Result<Vec<_>, _> = lexer.collect();
        assert!(tokens.is_ok());
        
        let tokens = tokens.unwrap();
        assert_eq!(tokens[0].token_type, TokenType::Vec4);
        assert_eq!(tokens[1].token_type, TokenType::LeftParen);
        match &tokens[2].token_type {
            TokenType::Float(f) => assert_eq!(*f, 1.0),
            _ => panic!("Expected float"),
        }
        assert_eq!(tokens[3].token_type, TokenType::Comma);
        // ... more assertions could be added
    }

    #[test]
    fn test_let_binding() {
        let mut lexer = Lexer::new("let x = 42.0;");
        
        let tokens: Result<Vec<_>, _> = lexer.collect();
        assert!(tokens.is_ok());
        
        let tokens = tokens.unwrap();
        assert_eq!(tokens[0].token_type, TokenType::Let);
        match &tokens[1].token_type {
            TokenType::Identifier(name) => assert_eq!(name, "x"),
            _ => panic!("Expected identifier"),
        }
        assert_eq!(tokens[2].token_type, TokenType::Assign);
        match &tokens[3].token_type {
            TokenType::Float(f) => assert_eq!(*f, 42.0),
            _ => panic!("Expected float"),
        }
        assert_eq!(tokens[4].token_type, TokenType::Semicolon);
    }

    #[test]
    fn test_error_unexpected_char() {
        let mut lexer = Lexer::new("1.0 @ 2.0");
        
        let tokens: Vec<_> = lexer.collect();
        assert_eq!(tokens.len(), 3); // 1.0, error, 2.0
        
        assert!(tokens[0].is_ok());
        assert!(tokens[1].is_err());
        assert!(tokens[2].is_ok());
        
        match &tokens[1] {
            Err(LexError::UnexpectedChar { char, .. }) => assert_eq!(*char, '@'),
            _ => panic!("Expected UnexpectedChar error"),
        }
    }
}
