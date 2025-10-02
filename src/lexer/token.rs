use crate::span::Span;

/// Token types for the pixdsl language
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Literals
    Float(f64),
    Identifier(String),
    
    // Keywords
    Let,
    
    // Types
    Float32,
    Vec2,
    Vec3,
    Vec4,
    
    // Operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Assign,
    
    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Semicolon,
    Dot,
    
    // Special
    Eof,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Float(n) => write!(f, "{}", n),
            TokenType::Identifier(name) => write!(f, "{}", name),
            TokenType::Let => write!(f, "let"),
            TokenType::Float32 => write!(f, "float"),
            TokenType::Vec2 => write!(f, "vec2"),
            TokenType::Vec3 => write!(f, "vec3"),
            TokenType::Vec4 => write!(f, "vec4"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Multiply => write!(f, "*"),
            TokenType::Divide => write!(f, "/"),
            TokenType::Assign => write!(f, "="),
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::Comma => write!(f, ","),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Dot => write!(f, "."),
            TokenType::Eof => write!(f, "EOF"),
        }
    }
}

/// A token with its type and source location
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType, span: Span) -> Self {
        Self { token_type, span }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_creation() {
        let span = Span::new(0, 5);
        let token = Token::new(TokenType::Float(1.5), span);
        
        assert_eq!(token.span, span);
        match token.token_type {
            TokenType::Float(f) => assert_eq!(f, 1.5),
            _ => panic!("Expected float token"),
        }
    }

    #[test]
    fn test_token_display() {
        assert_eq!(TokenType::Plus.to_string(), "+");
        assert_eq!(TokenType::Let.to_string(), "let");
        assert_eq!(TokenType::Float(3.14).to_string(), "3.14");
        assert_eq!(TokenType::Identifier("myVar".to_string()).to_string(), "myVar");
    }
}