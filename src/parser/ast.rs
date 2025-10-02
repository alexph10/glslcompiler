use crate::span::Span;

/// Root AST node representing a complete program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub span: Span,
}

/// Statements in the DSL
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// Let binding: `let x = expr;`
    Let(LetStatement),
    /// Expression statement (the final expression that becomes the fragment color)
    Expression(Expression),
}

/// Let binding statement
#[derive(Debug, Clone, PartialEq)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
    pub span: Span,
}

/// Expressions in the DSL
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Literal values: `1.0`, `42.5`
    Literal(LiteralExpression),
    /// Variable references: `x`, `myVar`
    Variable(VariableExpression),
    /// Binary operations: `a + b`, `x * y`
    Binary(BinaryExpression),
    /// Function calls: `vec4(1.0, 0.0, 0.0, 1.0)`, `sin(x)`
    Call(CallExpression),
}

/// Literal expression
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpression {
    pub value: LiteralValue,
    pub span: Span,
}

/// Literal values
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Float(f64),
}

/// Variable expression
#[derive(Debug, Clone, PartialEq)]
pub struct VariableExpression {
    pub name: String,
    pub span: Span,
}

/// Binary expression
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
    pub span: Span,
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    Add,       // +
    Subtract,  // -
    Multiply,  // *
    Divide,    // /
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Subtract => write!(f, "-"),
            BinaryOperator::Multiply => write!(f, "*"),
            BinaryOperator::Divide => write!(f, "/"),
        }
    }
}

/// Function call expression
#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub function: String,
    pub arguments: Vec<Expression>,
    pub span: Span,
}

impl Program {
    pub fn new(statements: Vec<Statement>, span: Span) -> Self {
        Self { statements, span }
    }
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::Let(let_stmt) => let_stmt.span,
            Statement::Expression(expr) => expr.span(),
        }
    }
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Literal(lit) => lit.span,
            Expression::Variable(var) => var.span,
            Expression::Binary(bin) => bin.span,
            Expression::Call(call) => call.span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_operator_display() {
        assert_eq!(BinaryOperator::Add.to_string(), "+");
        assert_eq!(BinaryOperator::Subtract.to_string(), "-");
        assert_eq!(BinaryOperator::Multiply.to_string(), "*");
        assert_eq!(BinaryOperator::Divide.to_string(), "/");
    }

    #[test]
    fn test_expression_span() {
        let span = Span::new(0, 5);
        let expr = Expression::Literal(LiteralExpression {
            value: LiteralValue::Float(1.0),
            span,
        });
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn test_program_creation() {
        let span = Span::new(0, 10);
        let program = Program::new(vec![], span);
        assert_eq!(program.statements.len(), 0);
        assert_eq!(program.span, span);
    }
}
