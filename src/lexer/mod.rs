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
/// 
/// Uses zero-copy string slicing and iterator-based design for efficiency
pub struct Lexer<'input> {
    input: &'input str,
    chars: std::str::CharIndices<'input>,
    current: Option<(usize, char)>,
    position: usize,
}

impl<'input> Lexer<'input> {
    /// Create a new lexer for the given input
    /// 
    /// # Examples
    /// ```
    /// use GLSLCompiler::lexer::Lexer;
    /// 
    /// let mut lexer = Lexer::new("let x = 1.0;");
    /// let tokens: Result<Vec<_>, _> = lexer.collect();
    /// ```
    pub fn new(input: &'input str) -> Self {
        let mut chars = input.char_indices();
        let current = chars.next();
        
        Self {
            input,
            chars,
            current,
            position: current.map_or(0, |(pos, _)| pos),
        }
    }
    
    /// Advance to the next character
    fn advance(&mut self) {
        self.current = self.chars.next();
        self.position = self.current.map_or(self.input.len(), |(pos, _)| pos);
    }
    
    /// Peek at the next character without consuming it
    fn peek_char(&self) -> Option<char> {
        self.chars.as_str().chars().next()
    }
    
    /// Get the current character
    fn current_char(&self) -> Option<char> {
        self.current.map(|(_, ch)| ch)
    }
    
    /// Skip whitespace characters
    fn skip_whitespace(&mut self) {
        while let Some((_, ch)) = self.current {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }
    
    /// Read a number literal with proper validation
    /// 
    /// Supports integers and floats, validates format
    fn read_number(&mut self) -> Result<f64, LexError> {
        let start_pos = self.position;
        let mut has_dot = false;
        
        // Consume digits and at most one decimal point
        while let Some((_, ch)) = self.current {
            match ch {
                '0'..='9' => self.advance(),
                '.' if !has_dot => {
                    has_dot = true;
                    self.advance();
                    
                    // Ensure there's at least one digit after the dot
                    if !matches!(self.current_char(), Some('0'..='9')) {
                        return Err(LexError::InvalidNumber {
                            span: Span::new(start_pos, self.position),
                        });
                    }
                }
                _ => break,
            }
        }
        
        let number_str = &self.input[start_pos..self.position];
        number_str.parse().map_err(|_| LexError::InvalidNumber {
            span: Span::new(start_pos, self.position),
        })
    }
    
    /// Read an identifier with proper validation
    /// 
    /// Identifiers must start with letter or underscore,
    /// followed by letters, digits, or underscores
    fn read_identifier(&mut self) -> Result<String, LexError> {
        let start_pos = self.position;
        
        // First character must be letter or underscore
        if !matches!(self.current_char(), Some('a'..='z' | 'A'..='Z' | '_')) {
            return Err(LexError::UnexpectedChar {
                char: self.current_char().unwrap_or('\0'),
                span: Span::new(start_pos, self.position),
            });
        }
        
        // Continue with alphanumeric or underscore
        while let Some((_, ch)) = self.current {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }
        
        Ok(self.input[start_pos..self.position].to_string())
    }
    
    /// Get the next token from the input
    /// 
    /// Returns Ok(Token) or Err(LexError) for invalid input
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();
        
        let start_pos = self.position;
        
        let token_type = match self.current_char() {
            None => TokenType::Eof,
            
            // Single-character tokens
            Some('+') => { self.advance(); TokenType::Plus }
            Some('-') => { self.advance(); TokenType::Minus }
            Some('*') => { self.advance(); TokenType::Multiply }
            Some('/') => { self.advance(); TokenType::Divide }
            Some('=') => { self.advance(); TokenType::Assign }
            Some('(') => { self.advance(); TokenType::LeftParen }
            Some(')') => { self.advance(); TokenType::RightParen }
            Some('{') => { self.advance(); TokenType::LeftBrace }
            Some('}') => { self.advance(); TokenType::RightBrace }
            Some(',') => { self.advance(); TokenType::Comma }
            Some(';') => { self.advance(); TokenType::Semicolon }
            Some('.') => { self.advance(); TokenType::Dot }
            
            // Number literals
            Some('0'..='9') => {
                TokenType::Float(self.read_number()?)
            }
            
            // Identifiers and keywords
            Some('a'..='z' | 'A'..='Z' | '_') => {
                let identifier = self.read_identifier()?;
                self.classify_identifier(identifier)
            }
            
            // Invalid characters
            Some(ch) => {
                let error_char = ch;
                let char_end = self.position + ch.len_utf8();
                self.advance(); // Advance past error to prevent infinite loop
                
                return Err(LexError::UnexpectedChar {
                    char: error_char,
                    span: Span::new(start_pos, char_end),
                });
            }
        };
        
        Ok(Token::new(token_type, Span::new(start_pos, self.position)))
    }
    
    /// Classify an identifier as either a keyword or identifier token
    #[inline]
    fn classify_identifier(&self, identifier: String) -> TokenType {
        match identifier.as_str() {
            "let" => TokenType::Let,
            "float" => TokenType::Float32,
            "vec2" => TokenType::Vec2,
            "vec3" => TokenType::Vec3,
            "vec4" => TokenType::Vec4,
            _ => TokenType::Identifier(identifier),
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
