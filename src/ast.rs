use std::fmt::Display;

/// Represents a type in the Lucid language.
///
/// This enum defines all the possible types that can be used in the language.
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
    Generic(String),
    /// Represents a union type (e.g., `number | string`)
    Union(Vec<Type>),
    /// Represents a tuple type (e.g., `{number, string}`)
    Tuple(Vec<(String, Type)>),
    /// Represents a map type (e.g., `{string: number}`)
    Map(Box<Type>, Box<Type>),
    /// Represents an interface type (e.g., `{name: string, age: number}`)
    Interface(String),
    /// Represents an enum type (e.g., `enum Color { Red, Green, Blue }`)
    Enum(String),
    /// Represents a record type (e.g., `{ name: string, age: number }`)
    Record(String),
    /// Represents a trait type (e.g., `trait Printable { print(): void }`)
    Trait(String),
    /// Represents an array with a single element (A.K.A. a table with a single element)
    MonoTable(Box<Type>), // Might add a "Wrapped" type and make this a Wrapped<Type>
    /// Represents a map with a single element (A.K.A. a table with a single element)
    MonoMap(Box<Type>, Box<Type>), // Might add a "Wrapped" type and make this a Wrapped<Type, Type>
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number => write!(f, "number"),
            Type::String => write!(f, "string"),
            Type::Boolean => write!(f, "boolean"),
            Type::Nil => write!(f, "nil"),
            Type::Function(params, returns) => {
                write!(
                    f,
                    "function({})",
                    params
                        .iter()
                        .map(|param| param.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
                write!(
                    f,
                    " -> {}",
                    returns
                        .iter()
                        .map(|return_type| return_type.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Type::Table => write!(f, "table"),
            Type::Any => write!(f, "any"),
            Type::UserDefined(name) => write!(f, "{}", name),
            Type::Generic(name) => write!(f, "{}", name),
            Type::Union(types) => write!(
                f,
                "{}",
                types
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
            Type::Tuple(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Map(key, value) => write!(f, "{{{}: {}}}", key, value),
            Type::Interface(name) => write!(f, "interface {}", name),
            Type::Enum(name) => write!(f, "enum {}", name),
            Type::Record(name) => write!(f, "record {}", name),
            Type::Trait(name) => write!(f, "trait {}", name),
            Type::MonoTable(element) => write!(f, "mono_table({})", element),
            Type::MonoMap(key, value) => write!(f, "mono_map({}, {})", key, value),
        }
    }
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
            _ => {
                if s.len() == 1 && s.chars().next().unwrap().is_uppercase() {
                    Some(Type::Generic(s.to_string()))
                } else {
                    Some(Type::UserDefined(s.to_string()))
                }
            }
        }
    }

    pub fn flatten_union(types: Vec<Type>) -> Vec<Type> {
        let mut flattened = Vec::new();
        for ty in types {
            match ty {
                Type::Union(inner) => {
                    flattened.extend(Self::flatten_union(inner));
                }
                _ => flattened.push(ty),
            }
        }
        // Remove duplicates
        flattened.sort_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)));
        flattened.dedup();
        flattened
    }
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct TypedIdent {
    pub name: String,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct InterfaceField {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct InterfaceDecl {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub fields: Vec<InterfaceField>,
}

#[derive(Debug, Clone)]
pub struct TraitField {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct TraitDecl {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub fields: Vec<TraitField>,
}

#[derive(Debug, Clone)]
pub struct TraitImpl {
    pub trait_name: String,
    pub generic_params: Vec<GenericParam>,
    pub fields: Vec<TraitField>,
}

#[derive(Debug, Clone)]
pub struct TraitFieldImpl {
    pub trait_name: String,
    pub generic_params: Vec<GenericParam>,
    pub fields: Vec<TraitField>,
}

#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct RecordDecl {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub interfaces: Vec<String>,
    pub fields: Vec<RecordField>,
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
        generic_params: Vec<GenericParam>,
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
        generic_params: Vec<GenericParam>,
        params: Vec<TypedIdent>,
        return_types: Vec<Type>,
        body: Vec<Stmt>,
    },
    InterfaceDecl(InterfaceDecl),
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
                generic_params: Vec::new(),
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
