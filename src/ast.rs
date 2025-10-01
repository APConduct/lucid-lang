#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Number,
    String,
    Boolean,
    Nil,
    Function(Vec<Type>, Vec<Type>),
    Table,
    Any,
    UserDefined(String),
}

impl Type {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "number" => Some(Type::Number),
            "string" => Some(Type::String),
            "boolean" => Some(Type::Boolean),
            "nil" => Some(Type::Nil),
            "function" => Some(Type::Function(vec![], vec![Type::Any])),
            "table" => Some(Type::Table),
            "any" => Some(Type::Any),
            _ => Some(Type::UserDefined(s.to_string())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedIdent {
    pub name: String,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Ident(String),
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    Table(Vec<TableField>),
    Index {
        table: Box<Expr>,
        index: Box<Expr>,
    },
    Function {
        params: Vec<TypedIdent>,
        return_types: Vec<Type>,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone)]
pub struct TableField {
    pub key: Option<Expr>,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Concat,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Local {
        vars: Vec<TypedIdent>,
        init: Option<Vec<Expr>>,
    },
    Assign {
        targets: Vec<Expr>,
        values: Vec<Expr>,
    },
    FunctionDecl {
        name: String,
        params: Vec<TypedIdent>,
        return_types: Vec<Type>,
        body: Vec<Stmt>,
    },
    If {
        condition: Expr,
        then_block: Vec<Stmt>,
        else_block: Option<Vec<Stmt>>,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    Return(Vec<Expr>),
    Expr(Expr),
    // For {
    //     var: TypeIdent,
    //     start: Expr,
    //     end: Expr,
    //     step: Option<Expr>,
    //     body: Vec<Stmt>,
    // },
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_program() {
        let program = Program {
            statements: vec![Stmt::Expr(Expr::Number(42.0))],
        };
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_program_with_statements() {
        let program = Program {
            statements: vec![
                Stmt::Expr(Expr::Number(42.0)),
                Stmt::Expr(Expr::String("Hello".to_string())),
            ],
        };
        assert_eq!(program.statements.len(), 2);
    }

    #[test]
    fn test_program_with_function() {
        let program = Program {
            statements: vec![Stmt::FunctionDecl {
                name: "add".to_string(),
                params: vec![
                    TypedIdent {
                        name: "x".to_string(),
                        ty: Some(Type::Number),
                    },
                    TypedIdent {
                        name: "y".to_string(),
                        ty: Some(Type::Number),
                    },
                ],
                return_types: vec![Type::Number],
                body: vec![Stmt::Return(vec![Expr::BinOp {
                    left: Box::new(Expr::Ident("x".to_string())),
                    op: BinOp::Add,
                    right: Box::new(Expr::Ident("y".to_string())),
                }])],
            }],
        };
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_program_with_if_statement() {
        let program = Program {
            statements: vec![Stmt::If {
                condition: Expr::Boolean(true),
                then_block: vec![Stmt::Expr(Expr::Number(1.0))],
                else_block: None,
            }],
        };
        assert_eq!(program.statements.len(), 1);
    }
}
