pub mod ast;

use crate::span::Span;
use crate::lexer::{Lexer, Token, TokenType, LexError};
use thiserror::Error;
use std::iter::Peekable;

pub use ast::*;

/// Errors that can occur during parsing
#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParseError {
    #[error("Expected {expected}, found {found:?} at {span:?}")]
    UnexpectedToken {
        expected: String,
        found: TokenType,
        span: Span,
    },
    
    #[error("Unexpected end of file, expected {expected}")]
    UnexpectedEof { expected: String },
    
    #[error("Invalid expression at {span:?}")]
    InvalidExpression { span: Span },
    
    #[error("Lexer error: {0}")]
    Lex(#[from] LexError),
}

/// Recursive descent parser for pixdsl
pub struct Parser {
    tokens: Peekable<std::vec::IntoIter<Token>>,
    current_token: Option<Token>,
}

impl Parser {
    pub fn new(input: &str) -> Result<Self, ParseError> {
        let lexer = Lexer::new(input);
        let tokens: Result<Vec<_>, _> = lexer.collect();
        let tokens = tokens?;
        
        let mut parser = Self {
            tokens: tokens.into_iter().peekable(),
            current_token: None,
        };
        
        parser.advance();
        Ok(parser)
    }
    
    fn advance(&mut self) {
        self.current_token = self.tokens.next();
    }
    
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }
    
    fn current_span(&self) -> Span {
        self.current_token
            .as_ref()
            .map(|t| t.span)
            .unwrap_or(Span::new(0, 0))
    }
    
    fn expect_token(&mut self, expected: TokenType) -> Result<Token, ParseError> {
        match &self.current_token {
            Some(token) if std::mem::discriminant(&token.token_type) == std::mem::discriminant(&expected) => {
                let token = token.clone();
                self.advance();
                Ok(token)
            }
            Some(token) => Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", expected),
                found: token.token_type.clone(),
                span: token.span,
            }),
            None => Err(ParseError::UnexpectedEof {
                expected: format!("{:?}", expected),
            }),
        }
    }
    
    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let start_span = self.current_span();
        let mut statements = Vec::new();
        
        while self.current_token.is_some() {
            statements.push(self.parse_statement()?);
        }
        
        let end_span = statements
            .last()
            .map(|s| s.span())
            .unwrap_or(start_span);
        
        Ok(Program::new(statements, Span::new(start_span.start, end_span.end)))
    }
    
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match &self.current_token {
            Some(Token { token_type: TokenType::Let, .. }) => {
                self.parse_let_statement().map(Statement::Let)
            }
            _ => {
                self.parse_expression().map(Statement::Expression)
            }
        }
    }
    
    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        let start_span = self.current_span();
        
        // Consume 'let'
        self.expect_token(TokenType::Let)?;
        
        // Get identifier
        let name = match &self.current_token {
            Some(Token { token_type: TokenType::Identifier(name), .. }) => {
                let name = name.clone();
                self.advance();
                name
            }
            Some(token) => return Err(ParseError::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token.token_type.clone(),
                span: token.span,
            }),
            None => return Err(ParseError::UnexpectedEof {
                expected: "identifier".to_string(),
            }),
        };
        
        // Expect '='
        self.expect_token(TokenType::Assign)?;
        
        // Parse value expression
        let value = self.parse_expression()?;
        
        // Expect ';'
        let end_token = self.expect_token(TokenType::Semicolon)?;
        
        Ok(LetStatement {
            name,
            value,
            span: Span::new(start_span.start, end_token.span.end),
        })
    }
    
    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_additive()
    }
    
    // Precedence climbing for binary operators
    fn parse_additive(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_multiplicative()?;
        
        while let Some(token) = &self.current_token {
            match &token.token_type {
                TokenType::Plus => {
                    self.advance();
                    let right = self.parse_multiplicative()?;
                    let span = Span::new(left.span().start, right.span().end);
                    left = Expression::Binary(BinaryExpression {
                        left: Box::new(left),
                        operator: BinaryOperator::Add,
                        right: Box::new(right),
                        span,
                    });
                }
                TokenType::Minus => {
                    self.advance();
                    let right = self.parse_multiplicative()?;
                    let span = Span::new(left.span().start, right.span().end);
                    left = Expression::Binary(BinaryExpression {
                        left: Box::new(left),
                        operator: BinaryOperator::Subtract,
                        right: Box::new(right),
                        span,
                    });
                }
                _ => break,
            }
        }
        
        Ok(left)
    }
    
    fn parse_multiplicative(&mut self) -> Result<Expression, ParseError> {
        let mut left = self.parse_primary()?;
        
        while let Some(token) = &self.current_token {
            match &token.token_type {
                TokenType::Multiply => {
                    self.advance();
                    let right = self.parse_primary()?;
                    let span = Span::new(left.span().start, right.span().end);
                    left = Expression::Binary(BinaryExpression {
                        left: Box::new(left),
                        operator: BinaryOperator::Multiply,
                        right: Box::new(right),
                        span,
                    });
                }
                TokenType::Divide => {
                    self.advance();
                    let right = self.parse_primary()?;
                    let span = Span::new(left.span().start, right.span().end);
                    left = Expression::Binary(BinaryExpression {
                        left: Box::new(left),
                        operator: BinaryOperator::Divide,
                        right: Box::new(right),
                        span,
                    });
                }
                _ => break,
            }
        }
        
        Ok(left)
    }
    
    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match &self.current_token {
            Some(Token { token_type: TokenType::Float(value), span }) => {
                let value = *value;
                let span = *span;
                self.advance();
                Ok(Expression::Literal(LiteralExpression {
                    value: LiteralValue::Float(value),
                    span,
                }))
            }
            
            Some(Token { token_type: TokenType::Identifier(name), span }) => {
                let name = name.clone();
                let span = *span;
                self.advance();
                
                // Check if this is a function call
                if let Some(Token { token_type: TokenType::LeftParen, .. }) = &self.current_token {
                    self.parse_function_call(name, span)
                } else {
                    Ok(Expression::Variable(VariableExpression { name, span }))
                }
            }
            
            Some(Token { token_type: TokenType::Vec2, span }) => {
                let span = *span;
                self.advance();
                self.parse_function_call("vec2".to_string(), span)
            }
            
            Some(Token { token_type: TokenType::Vec3, span }) => {
                let span = *span;
                self.advance();
                self.parse_function_call("vec3".to_string(), span)
            }
            
            Some(Token { token_type: TokenType::Vec4, span }) => {
                let span = *span;
                self.advance();
                self.parse_function_call("vec4".to_string(), span)
            }
            
            Some(Token { token_type: TokenType::LeftParen, .. }) => {
                self.advance(); // consume '('
                let expr = self.parse_expression()?;
                self.expect_token(TokenType::RightParen)?;
                Ok(expr)
            }
            
            Some(token) => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: token.token_type.clone(),
                span: token.span,
            }),
            
            None => Err(ParseError::UnexpectedEof {
                expected: "expression".to_string(),
            }),
        }
    }
    
    fn parse_function_call(&mut self, function_name: String, start_span: Span) -> Result<Expression, ParseError> {
        // Consume '('
        self.expect_token(TokenType::LeftParen)?;
        
        let mut arguments = Vec::new();
        
        // Parse arguments
        if let Some(Token { token_type: TokenType::RightParen, .. }) = &self.current_token {
            // Empty argument list
        } else {
            loop {
                arguments.push(self.parse_expression()?);
                
                match &self.current_token {
                    Some(Token { token_type: TokenType::Comma, .. }) => {
                        self.advance(); // consume ','
                    }
                    Some(Token { token_type: TokenType::RightParen, .. }) => {
                        break;
                    }
                    Some(token) => return Err(ParseError::UnexpectedToken {
                        expected: "',' or ')'".to_string(),
                        found: token.token_type.clone(),
                        span: token.span,
                    }),
                    None => return Err(ParseError::UnexpectedEof {
                        expected: "',' or ')'".to_string(),
                    }),
                }
            }
        }
        
        // Consume ')'
        let end_token = self.expect_token(TokenType::RightParen)?;
        
        Ok(Expression::Call(CallExpression {
            function: function_name,
            arguments,
            span: Span::new(start_span.start, end_token.span.end),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_literal() {
        let mut parser = Parser::new("42.0").unwrap();
        let program = parser.parse().unwrap();
        
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expression::Literal(lit)) => {
                match &lit.value {
                    LiteralValue::Float(f) => assert_eq!(*f, 42.0),
                }
            }
            _ => panic!("Expected literal expression"),
        }
    }

    #[test]
    fn test_parse_variable() {
        let mut parser = Parser::new("myVar").unwrap();
        let program = parser.parse().unwrap();
        
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expression::Variable(var)) => {
                assert_eq!(var.name, "myVar");
            }
            _ => panic!("Expected variable expression"),
        }
    }

    #[test]
    fn test_parse_binary_expression() {
        let mut parser = Parser::new("1.0 + 2.0").unwrap();
        let program = parser.parse().unwrap();
        
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expression::Binary(bin)) => {
                assert_eq!(bin.operator, BinaryOperator::Add);
                match (&*bin.left, &*bin.right) {
                    (Expression::Literal(left), Expression::Literal(right)) => {
                        match (&left.value, &right.value) {
                            (LiteralValue::Float(l), LiteralValue::Float(r)) => {
                                assert_eq!(*l, 1.0);
                                assert_eq!(*r, 2.0);
                            }
                        }
                    }
                    _ => panic!("Expected literal operands"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_let_statement() {
        let mut parser = Parser::new("let x = 42.0;").unwrap();
        let program = parser.parse().unwrap();
        
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Let(let_stmt) => {
                assert_eq!(let_stmt.name, "x");
                match &let_stmt.value {
                    Expression::Literal(lit) => {
                        match &lit.value {
                            LiteralValue::Float(f) => assert_eq!(*f, 42.0),
                        }
                    }
                    _ => panic!("Expected literal value"),
                }
            }
            _ => panic!("Expected let statement"),
        }
    }

    #[test]
    fn test_parse_function_call() {
        let mut parser = Parser::new("vec4(1.0, 0.0, 0.0, 1.0)").unwrap();
        let program = parser.parse().unwrap();
        
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expression::Call(call)) => {
                assert_eq!(call.function, "vec4");
                assert_eq!(call.arguments.len(), 4);
                
                for (i, expected) in [1.0, 0.0, 0.0, 1.0].iter().enumerate() {
                    match &call.arguments[i] {
                        Expression::Literal(lit) => {
                            match &lit.value {
                                LiteralValue::Float(f) => assert_eq!(*f, *expected),
                            }
                        }
                        _ => panic!("Expected literal argument"),
                    }
                }
            }
            _ => panic!("Expected function call"),
        }
    }

    #[test]
    fn test_parse_complex_expression() {
        let mut parser = Parser::new("let color = vec3(1.0 + 2.0, 0.5 * 2.0, 1.0);").unwrap();
        let program = parser.parse().unwrap();
        
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Let(let_stmt) => {
                assert_eq!(let_stmt.name, "color");
                match &let_stmt.value {
                    Expression::Call(call) => {
                        assert_eq!(call.function, "vec3");
                        assert_eq!(call.arguments.len(), 3);
                        
                        // First argument should be 1.0 + 2.0
                        match &call.arguments[0] {
                            Expression::Binary(bin) => {
                                assert_eq!(bin.operator, BinaryOperator::Add);
                            }
                            _ => panic!("Expected binary expression for first argument"),
                        }
                        
                        // Second argument should be 0.5 * 2.0
                        match &call.arguments[1] {
                            Expression::Binary(bin) => {
                                assert_eq!(bin.operator, BinaryOperator::Multiply);
                            }
                            _ => panic!("Expected binary expression for second argument"),
                        }
                    }
                    _ => panic!("Expected function call"),
                }
            }
            _ => panic!("Expected let statement"),
        }
    }

    #[test]
    fn test_parse_multiple_statements() {
        let mut parser = Parser::new("let x = 1.0; let y = 2.0; vec4(x, y, 0.0, 1.0)").unwrap();
        let program = parser.parse().unwrap();
        
        assert_eq!(program.statements.len(), 3);
        
        // First statement: let x = 1.0;
        match &program.statements[0] {
            Statement::Let(let_stmt) => {
                assert_eq!(let_stmt.name, "x");
            }
            _ => panic!("Expected let statement"),
        }
        
        // Second statement: let y = 2.0;
        match &program.statements[1] {
            Statement::Let(let_stmt) => {
                assert_eq!(let_stmt.name, "y");
            }
            _ => panic!("Expected let statement"),
        }
        
        // Third statement: vec4(x, y, 0.0, 1.0)
        match &program.statements[2] {
            Statement::Expression(Expression::Call(call)) => {
                assert_eq!(call.function, "vec4");
                assert_eq!(call.arguments.len(), 4);
            }
            _ => panic!("Expected function call"),
        }
    }

    #[test]
    fn test_parse_operator_precedence() {
        let mut parser = Parser::new("1.0 + 2.0 * 3.0").unwrap();
        let program = parser.parse().unwrap();
        
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expression::Binary(bin)) => {
                // Should be parsed as 1.0 + (2.0 * 3.0), not (1.0 + 2.0) * 3.0
                assert_eq!(bin.operator, BinaryOperator::Add);
                
                match (&*bin.left, &*bin.right) {
                    (Expression::Literal(_), Expression::Binary(right_bin)) => {
                        assert_eq!(right_bin.operator, BinaryOperator::Multiply);
                    }
                    _ => panic!("Incorrect precedence parsing"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_parentheses() {
        let mut parser = Parser::new("(1.0 + 2.0) * 3.0").unwrap();
        let program = parser.parse().unwrap();
        
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Expression(Expression::Binary(bin)) => {
                // Should be parsed as (1.0 + 2.0) * 3.0
                assert_eq!(bin.operator, BinaryOperator::Multiply);
                
                match (&*bin.left, &*bin.right) {
                    (Expression::Binary(left_bin), Expression::Literal(_)) => {
                        assert_eq!(left_bin.operator, BinaryOperator::Add);
                    }
                    _ => panic!("Incorrect parentheses parsing"),
                }
            }
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_error_unexpected_token() {
        let result = Parser::new("let x = +;");
        match result {
            Ok(mut parser) => {
                let result = parser.parse();
                assert!(result.is_err());
            }
            Err(_) => {} // Lexer error is also acceptable
        }
    }

    #[test]
    fn test_parse_error_missing_semicolon() {
        let mut parser = Parser::new("let x = 1.0").unwrap();
        let result = parser.parse();
        assert!(result.is_err());
    }
}
