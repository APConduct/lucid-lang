use crate::{
    ast::{BinOp, Expr, GenericParam, InterfaceField, Program, Stmt, TableField, Type, TypedIdent},
    lexer::{Lexer, Token},
};

use super::ast::InterfaceDecl;

use tracing::Level;
use tracing::event;
use tracing::span;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next_token();
            tracing::trace!("Wrapping token {:?}", tok);
            let is_eof = tok == Token::Eof;
            tokens.push(tok);
            if is_eof {
                break;
            }
        }
        Self { tokens, pos: 0 }
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::Eof)
    }

    fn tok_ref_wrap<'a>(&self, tok: &'a Token) -> &'a Token {
        tracing::trace!("Wrapping token {:?}", tok);
        tok
    }

    pub fn peek(&self, offset: usize) -> &Token {
        self.tokens.get(self.pos + offset).unwrap_or(&Token::Eof)
    }

    fn advance(&mut self) -> Token {
        let tok = self.current().clone();
        self.pos += 1;
        tok
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        let _span = tracing::span!(tracing::Level::INFO, "expect");
        let _enter = _span.enter();
        let current = self.current().clone();
        if std::mem::discriminant(&current) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(format!(
                "Expected {:?}, got {:?}; next token: {:?}",
                expected,
                current,
                self.peek(1)
            ))
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let _span = tracing::span!(tracing::Level::DEBUG, "parse_program");
        tracing::event!(tracing::Level::INFO, "Parsing program");
        let _enter = _span.enter();
        let mut statements = Vec::new();
        while self.current() != &Token::Eof {
            statements.push(self.parse_stmt()?);
        }
        Ok(Program { statements })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        let _span = tracing::span!(tracing::Level::INFO, "parse_stmt");
        tracing::event!(tracing::Level::DEBUG, "Parsing statement");
        let _enter = _span.enter();
        match self.current() {
            Token::Local => {
                tracing::event!(tracing::Level::DEBUG, "Parsing local statement");
                let stmt = self.parse_local()?;
                tracing::event!(tracing::Level::DEBUG, "Parsed local statement: {:?}", stmt);
                Ok(stmt)
            }
            Token::Function => {
                tracing::event!(tracing::Level::DEBUG, "Parsing function statement");
                let stmt = self.parse_function()?;
                tracing::event!(
                    tracing::Level::DEBUG,
                    "Parsed function statement: {:?}",
                    stmt
                );
                Ok(stmt)
            }
            Token::Interface => {
                tracing::event!(tracing::Level::DEBUG, "Parsing interface statement");
                let stmt = self.parse_interface()?;
                tracing::event!(
                    tracing::Level::DEBUG,
                    "Parsed interface statement: {:?}",
                    stmt
                );
                Ok(stmt)
            }
            Token::If => {
                tracing::event!(tracing::Level::DEBUG, "Parsing if statement");
                let stmt = self.parse_if()?;
                tracing::event!(tracing::Level::DEBUG, "Parsed if statement: {:?}", stmt);
                Ok(stmt)
            }
            Token::While => {
                tracing::event!(tracing::Level::DEBUG, "Parsing while statement");
                let stmt = self.parse_while()?;
                tracing::event!(tracing::Level::DEBUG, "Parsed while statement: {:?}", stmt);
                Ok(stmt)
            }
            Token::Return => {
                tracing::event!(tracing::Level::DEBUG, "Parsing return statement");
                let stmt = self.parse_return()?;
                tracing::event!(tracing::Level::DEBUG, "Parsed return statement: {:?}", stmt);
                Ok(stmt)
            }
            _ => {
                // Try to parse as expression statement or assignment
                let expr = self.parse_expr()?;
                tracing::event!(tracing::Level::DEBUG, "Parsed expression: {:?}", expr);

                // Check if this is an assignment
                if self.current() == &Token::Assign {
                    self.advance();
                    let /*mut*/ targets = vec![expr];
                    let mut values = vec![self.parse_expr()?];

                    while self.current() == &Token::Comma {
                        self.advance();
                        values.push(self.parse_expr()?);
                    }
                    tracing::event!(
                        tracing::Level::DEBUG,
                        "Finished parsing assignment statement"
                    );
                    return Ok(Stmt::Assign { targets, values });
                }
                tracing::event!(
                    tracing::Level::DEBUG,
                    "Finished parsing expression statement"
                );
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_local(&mut self) -> Result<Stmt, String> {
        let _span = tracing::span!(target: "parser", tracing::Level::INFO, "parse_local");
        let _guard = _span.enter();
        self.expect(Token::Local)?;

        if self.current() == &Token::Function {
            return self.parse_local_function();
        }

        let mut vars = Vec::new();

        // Parse variable names with optional types
        loop {
            if let Token::Ident(name) = self.current() {
                let name = name.clone();
                tracing::event!(target: "parser", tracing::Level::INFO, "Parsed variable: {}", name);
                self.advance();

                // Check for type annotation
                let ty = if self.current() == &Token::Colon {
                    self.advance();
                    Some(self.parse_type().expect("Expected type after ':'"))
                } else {
                    None
                };

                tracing::event!(target: "parser", tracing::Level::INFO, "Parsed variable: {} as type {}", name, ty.as_ref().map_or("None".to_string(), |ty| ty.to_string()));
                vars.push(TypedIdent { name, ty });

                if self.current() == &Token::Comma {
                    self.advance();
                } else {
                    break;
                }
            } else {
                return Err("Expected identifier after 'local'".to_string());
            }
        }

        // Check for initialization
        let init = if self.current() == &Token::Assign {
            self.advance();
            let mut exprs = vec![self.parse_expr()?];
            while self.current() == &Token::Comma {
                self.advance();
                exprs.push(self.parse_expr()?);
            }
            Some(exprs)
        } else {
            None
        };

        Ok(Stmt::Local { vars, init })
    }

    fn parse_generic_params(&mut self) -> Result<Vec<GenericParam>, String> {
        let _span = tracing::span!(tracing::Level::DEBUG, "parse_generic_params");
        tracing::event!(tracing::Level::DEBUG, "Parsing generic parameters");
        let _enter = _span.enter();
        let mut generic_params = Vec::new();

        if self.current() == &Token::Lt {
            self.advance();
            loop {
                if let Token::Ident(name) = self.current() {
                    let name = name.clone();
                    self.advance();
                    generic_params.push(GenericParam { name });

                    if self.current() == &Token::Comma {
                        self.advance();
                    } else {
                        break;
                    }
                } else {
                    return Err("Expected generic parameter name".to_string());
                }
            }

            if self.current() == &Token::Gt {
                self.advance();
            } else {
                return Err("Expected '>' after generic parameters".to_string());
            }
        }
        Ok(generic_params)
    }

    fn parse_local_function(&mut self) -> Result<Stmt, String> {
        let _span = tracing::span!(tracing::Level::TRACE, "parse_local_function");
        let _enter = _span.enter();
        self.expect(Token::Function)?;

        let name = if let Token::Ident(n) = self.current() {
            let n = n.clone();
            self.advance();
            n
        } else {
            return Err("Expected function name after 'local function'".to_string());
        };

        // Parse generic parameters
        let generic_params = self.parse_generic_params()?;

        self.expect(Token::LParen)?;

        let mut params = Vec::new();
        if self.current() != &Token::RParen {
            loop {
                if let Token::Ident(param_name) = self.current() {
                    let param_name = param_name.clone();
                    self.advance();

                    let ty = if self.current() == &Token::Colon {
                        self.advance();
                        Some(self.parse_type()?)
                    } else {
                        None
                    };

                    params.push(TypedIdent {
                        name: param_name,
                        ty,
                    });

                    if self.current() == &Token::Comma {
                        self.advance();
                    } else {
                        break;
                    }
                } else {
                    return Err("Expected parameter name.".to_string());
                }
            }
        }

        self.expect(Token::RParen)?;

        let return_types = if self.current() == &Token::Colon {
            self.advance();
            let mut types = vec![self.parse_type()?];
            while self.current() == &Token::Comma {
                self.advance();
                types.push(self.parse_type()?);
            }
            types
        } else {
            Vec::new()
        };

        let mut body = Vec::new();
        while self.current() != &Token::End {
            body.push(self.parse_stmt()?);
        }

        self.expect(Token::End)?;

        Ok(Stmt::Local {
            vars: vec![TypedIdent { name, ty: None }],
            init: Some(vec![Expr::Function {
                generic_params,
                params,
                return_types,
                body,
            }]),
        })
    }

    fn parse_function(&mut self) -> Result<Stmt, String> {
        let _span = tracing::span!(tracing::Level::TRACE, "parse_function");
        let _enter = _span.enter();
        self.expect(Token::Function)?;

        let name = if let Token::Ident(n) = self.current() {
            let n = n.clone();
            self.advance();
            n
        } else {
            return Err("Expected function name".to_string());
        };

        let generic_params = self.parse_generic_params()?;

        self.expect(Token::LParen)?;

        let mut params = Vec::new();
        if self.current() != &Token::RParen {
            loop {
                if let Token::Ident(param_name) = self.current() {
                    let param_name = param_name.clone();
                    self.advance();

                    // Expect type annotation for parameters
                    self.expect(Token::Colon)?;
                    let ty = Some(self.parse_type()?);

                    params.push(TypedIdent {
                        name: param_name,
                        ty,
                    });

                    if self.current() == &Token::Comma {
                        self.advance();
                    } else {
                        break;
                    }
                } else {
                    return Err("Expected parameter name".to_string());
                }
            }
        }

        self.expect(Token::RParen)?;

        // Parse return type annotations (can be multiple)
        let return_types = if self.current() == &Token::Colon {
            self.advance();
            let mut types = vec![self.parse_type()?];
            while self.current() == &Token::Comma {
                self.advance();
                types.push(self.parse_type()?);
            }
            types
        } else {
            Vec::new()
        };

        // Parse function body
        let mut body = Vec::new();
        while self.current() != &Token::End {
            body.push(self.parse_stmt()?);
        }

        self.expect(Token::End)?;

        Ok(Stmt::FunctionDecl {
            name,
            generic_params,
            params,
            return_types,
            body,
        })
    }

    fn parse_if(&mut self) -> Result<Stmt, String> {
        let _span = tracing::span!(tracing::Level::TRACE, "parse_if");
        let _enter = _span.enter();
        self.expect(Token::If)?;
        let condition = self.parse_expr()?;
        self.expect(Token::Then)?;

        let mut then_block = Vec::new();
        while self.current() != &Token::Else && self.current() != &Token::End {
            then_block.push(self.parse_stmt()?);
        }

        let else_block = if self.current() == &Token::Else {
            self.advance();
            let mut block = Vec::new();
            while self.current() != &Token::End {
                block.push(self.parse_stmt()?);
            }
            Some(block)
        } else {
            None
        };

        self.expect(Token::End)?;

        Ok(Stmt::If {
            condition,
            then_block,
            else_block,
        })
    }

    fn parse_while(&mut self) -> Result<Stmt, String> {
        let _span = tracing::span!(tracing::Level::TRACE, "parse_while");
        let _enter = _span.enter();
        self.expect(Token::While)?;
        let condition = self.parse_expr()?;
        self.expect(Token::Do)?;

        let mut body = Vec::new();
        while self.current() != &Token::End {
            body.push(self.parse_stmt().expect("Failed to parse statement"));
        }

        self.expect(Token::End)?;

        Ok(Stmt::While { condition, body })
    }

    fn parse_return(&mut self) -> Result<Stmt, String> {
        let _span = tracing::span!(tracing::Level::TRACE, "parse_return");
        let _enter = _span.enter();
        self.expect(Token::Return)?;

        let mut exprs = Vec::new();

        // Check if there are return values
        if !matches!(self.current(), Token::End | Token::Else | Token::Eof) {
            exprs.push(self.parse_expr()?);
            while self.current() == &Token::Comma {
                self.advance();
                exprs.push(self.parse_expr()?);
            }
        }

        Ok(Stmt::Return(exprs))
    }

    fn parse_interface(&mut self) -> Result<Stmt, String> {
        self.expect(Token::Interface)?;

        let name = if let Token::Ident(n) = self.current() {
            let n = n.clone();
            self.advance();
            n
        } else {
            return Err("Expected interface name".to_string());
        };

        // Parse generic parameters
        let generic_params = self.parse_generic_params()?;

        let mut fields = Vec::new();

        // Parse fields until 'end'
        while self.current() != &Token::End {
            if let Token::Ident(field_name) = self.current() {
                let field_name = field_name.clone();
                self.advance();

                self.expect(Token::Colon)?;
                let ty = self.parse_type()?;

                fields.push(InterfaceField {
                    name: field_name,
                    ty,
                });

                // Optional comma or newline separator
                if self.current() == &Token::Comma {
                    self.advance();
                }
            } else if self.current() == &Token::End {
                break;
            } else {
                return Err(format!(
                    "Expected field name or 'end' in interface, got {:?}",
                    self.current()
                ));
            }
        }

        self.expect(Token::End)?;

        Ok(Stmt::InterfaceDecl(InterfaceDecl {
            name,
            generic_params,
            fields,
        }))
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let _span = tracing::span!(Level::TRACE, "parse_type");
        let _enter = _span.enter();
        let current_token = self.current();
        tracing::trace!(
            "Current token: {:?}, next token is {:?}",
            current_token,
            self.peek(1)
        );
        let mut base_type = match current_token {
            Token::Ident(type_name) => {
                tracing::trace!("Parsing type: {}", type_name.clone());
                let type_name = type_name.clone();
                self.advance();
                Type::from_str(&type_name).ok_or_else(|| format!("Unknown type: {}", type_name))?
            }
            Token::Nil => {
                tracing::trace!("Parsing type: nil");
                self.advance();
                Type::Nil
            }
            Token::True | Token::False => {
                tracing::trace!("Parsing type: boolean");
                self.advance();
                Type::Boolean
            }
            Token::Function => {
                tracing::trace!("Parsing type: function");
                tracing::event!(
                    Level::TRACE,
                    "current token: {}, next token: {}",
                    self.current(),
                    self.peek(1)
                );
                self.advance();
                tracing::event!(
                    Level::TRACE,
                    "current token: {}, next token: {}",
                    self.current(),
                    self.peek(1)
                );

                if self.current() == &Token::LParen {
                    let (prm, ret): (Vec<Type>, Vec<Type>) = (vec![], vec![]);
                    loop {
                        if self.current() == &Token::RParen {
                            self.advance();
                            break;
                        }
                        self.advance();
                    }
                    Type::Function(prm, ret)
                } else {
                    tracing::trace!("Parsing function type");
                    Type::Function(vec![Type::Any], vec![Type::Any])
                }
            }
            _ => {
                return Err(format!(
                    "Expected type name, got {}; next token: {}",
                    self.current(),
                    self.peek(1)
                ));
            }
        };
        tracing::event!(Level::DEBUG, "Base type initialized as {}", base_type);

        // Check for union type (|)
        if self.current() == &Token::Pipe {
            let mut union_types = vec![base_type];

            while self.current() == &Token::Pipe {
                tracing::event!(Level::DEBUG, "found pipe, advancing to {}", self.current());
                let _advance = self.advance();

                let next_type = match self.current() {
                    Token::Ident(type_name) => {
                        tracing::event!(Level::DEBUG, "Parsing type: {}", type_name.clone());
                        let type_name = type_name.clone();
                        self.advance();
                        Type::from_str(&type_name)
                            .ok_or_else(|| format!("Unknown type: {}", type_name))?
                    }
                    Token::Nil => {
                        tracing::event!(Level::DEBUG, "Parsing type: nil");
                        self.advance();
                        Type::Nil
                    }
                    Token::True | Token::False => {
                        tracing::event!(Level::DEBUG, "Parsing type: boolean");
                        self.advance();
                        Type::Boolean
                    }
                    _ => {
                        tracing::event!(
                            Level::ERROR,
                            "Expected type after '|', got {}",
                            self.current()
                        );
                        return Err(format!("Expected type after '|', got {}", self.current()));
                    }
                };

                union_types.push(next_type.clone());
                tracing::event!(Level::DEBUG, "Parsed type: {}", next_type);
            }

            // Flatten and create union
            base_type = Type::Union(Type::flatten_union(union_types));
        }
        Ok(base_type)
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        let _span = tracing::span!(Level::TRACE, "parse_expr");
        let _enter = _span.enter();
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, String> {
        let _span = tracing::span!(Level::TRACE, "parse_or");
        let _enter = _span.enter();
        let mut left = self.parse_and()?;

        while self.current() == &Token::Or {
            self.advance();
            let right = self.parse_and()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op: BinOp::Or,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, String> {
        let _span = tracing::span!(Level::TRACE, "parse_and");
        let _enter = _span.enter();
        let mut left = self.parse_comparison().expect("Failed to parse comparison");

        while self.current() == &Token::And {
            self.advance();
            let right = self.parse_comparison().expect("Failed to parse comparison");
            left = Expr::BinOp {
                left: Box::new(left),
                op: BinOp::And,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let _span = tracing::span!(Level::TRACE, "parse_comparison");
        let _enter = _span.enter();
        let mut left = self.parse_concat().expect("Failed to parse concat");

        loop {
            let op = match self.current() {
                Token::EqEq => BinOp::Eq,
                Token::NotEq => BinOp::Ne,
                Token::Lt => BinOp::Lt,
                Token::Le => BinOp::Le,
                Token::Gt => BinOp::Gt,
                Token::Ge => BinOp::Ge,
                _ => break,
            };

            self.advance();
            let right = self.parse_concat().expect("Failed to parse concat");
            left = Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_concat(&mut self) -> Result<Expr, String> {
        let _span = tracing::span!(Level::TRACE, "parse_concat");
        let _enter = _span.enter();
        let mut left = self.parse_additive().expect("Failed to parse additive");

        while self.current() == &Token::Concat {
            self.advance();
            let right = self.parse_additive().expect("Failed to parse additive");
            left = Expr::BinOp {
                left: Box::new(left),
                op: BinOp::Concat,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expr, String> {
        let _span = tracing::span!(Level::TRACE, "parse_additive");
        let _enter = _span.enter();
        let mut left = self
            .parse_multiplicative()
            .expect("Failed to parse multiplicative");

        loop {
            let op = match self.current() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => break,
            };

            self.advance();
            let right = self.parse_multiplicative()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, String> {
        let _span = tracing::span!(Level::TRACE, "parse_multiplicative");
        let _enter = _span.enter();
        let mut left = self
            .parse_unary()
            .expect("Failed to parse unary expression");

        loop {
            let op = match self.current() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => break,
            };

            self.advance();
            let right = self
                .parse_unary()
                .expect("Failed to parse unary expression");
            left = Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        let _span = tracing::span!(Level::TRACE, "parse_unary");
        let _enter = _span.enter();
        match self.current() {
            Token::Not | Token::Minus => {
                tracing::warn!("Unary operators are not implemented yet");
                // TODO: Implement unary operators properly
                self.advance();
                tracing::warn!("About to call parse_unary recursively");
                self.parse_unary()
            }
            _ => {
                tracing::warn!("About to call parse_postfix");
                self.parse_postfix()
            }
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, String> {
        let _span = tracing::span!(Level::TRACE, "parse_postfix");
        let _enter = _span.enter();
        let mut _expr = self.parse_primary();
        if _expr.is_err() {
            tracing::warn!(
                "Failed to parse primary expression; Result was: {:?}",
                _expr
            );
        }

        let mut expr = _expr.expect("Failed to parse primary expression");

        loop {
            match self.current() {
                Token::LParen => {
                    // Function call
                    self.advance();
                    let mut args = Vec::new();

                    if self.current() != &Token::RParen {
                        args.push(self.parse_expr()?);
                        while self.current() == &Token::Comma {
                            self.advance();
                            args.push(self.parse_expr()?);
                        }
                    }

                    self.expect(Token::RParen)?;
                    expr = Expr::Call {
                        func: Box::new(expr),
                        args,
                    };
                }
                Token::LBracket => {
                    // Index access
                    self.advance();
                    let index = self.parse_expr()?;
                    self.expect(Token::RBracket)?;
                    expr = Expr::Index {
                        table: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                Token::Dot => {
                    // Field access (syntactic sugar for string index)
                    self.advance();
                    if let Token::Ident(field) = self.current() {
                        let field = field.clone();
                        self.advance();
                        expr = Expr::Index {
                            table: Box::new(expr),
                            index: Box::new(Expr::String(field)),
                        };
                    } else {
                        return Err("Expected field name after '.'".to_string());
                    }
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        let _span = tracing::span!(Level::TRACE, "parse_primary");
        let _enter = _span.enter();
        match self.current().clone() {
            Token::Number(n) => {
                self.advance();
                Ok(Expr::Number(n))
            }
            Token::String(s) => {
                self.advance();
                Ok(Expr::String(s))
            }
            Token::True => {
                self.advance();
                Ok(Expr::Boolean(true))
            }
            Token::False => {
                self.advance();
                Ok(Expr::Boolean(false))
            }
            Token::Nil => {
                self.advance();
                Ok(Expr::Nil)
            }
            Token::Ident(name) => {
                self.advance();
                Ok(Expr::Ident(name))
            }
            Token::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::LBrace => {
                tracing::span!(tracing::Level::INFO, "Parsing table constructor");
                // Table constructor
                self.advance();
                let mut fields = Vec::new();

                if self.current() != &Token::RBrace {
                    loop {
                        // Check for bracket notation: [expr] = value
                        if self.current() == &Token::LBracket {
                            self.advance();
                            let key_expr = self.parse_expr()?;
                            self.expect(Token::RBracket)?;
                            self.expect(Token::Assign)?;
                            let value = self.parse_expr()?;
                            fields.push(TableField {
                                key: Some(key_expr),
                                value,
                            });
                        } else {
                            // Check if this is a key-value pair (key = value) or just a value
                            let first_expr = self.parse_expr()?;

                            if self.current() == &Token::Assign {
                                // This is a key-value pair
                                self.advance();
                                let value = self.parse_expr()?;
                                fields.push(TableField {
                                    key: Some(first_expr),
                                    value,
                                });
                            } else {
                                // This is just a value
                                fields.push(TableField {
                                    key: None,
                                    value: first_expr,
                                });
                            }
                        }

                        if self.current() == &Token::Comma {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }

                self.expect(Token::RBrace)?;
                Ok(Expr::Table(fields))
            }
            Token::Function => {
                let generic_params = self.parse_generic_params()?;

                self.advance();

                self.expect(Token::LParen)?;

                let mut params = Vec::new();
                if self.current() != &Token::RParen {
                    loop {
                        if let Token::Ident(param_name) = self.current() {
                            let param_name = param_name.clone();
                            self.advance();

                            // Expect type annotation for parameters
                            self.expect(Token::Colon)?;
                            let ty = Some(self.parse_type()?);

                            params.push(TypedIdent {
                                name: param_name,
                                ty,
                            });

                            if self.current() == &Token::Comma {
                                self.advance();
                            } else {
                                break;
                            }
                        } else {
                            return Err("Expected parameter name".to_string());
                        }
                    }
                }

                self.expect(Token::RParen)?;

                // Parse return type annotations (can be multiple)
                let return_types = if self.current() == &Token::Colon {
                    self.advance();
                    let mut types = vec![self.parse_type()?];
                    while self.current() == &Token::Comma {
                        self.advance();
                        types.push(self.parse_type()?);
                    }
                    types
                } else {
                    Vec::new()
                };

                // Parse function body
                let mut body = Vec::new();
                while self.current() != &Token::End {
                    body.push(self.parse_stmt()?);
                }

                self.expect(Token::End)?;

                // // Parse function attributes (can be multiple)
                // let mut attributes = Vec::new();
                // while self.current() == &Token::At {
                //     self.advance();
                //     attributes.push(self.parse_attribute()?);
                // }

                Ok(Expr::Function {
                    generic_params,
                    params,
                    return_types,
                    body,
                })
            }
            tok => Err(format!("Unexpected token in expression: {:?}", tok)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expression() {
        let input = "1 + 2";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expr().unwrap();
        let expected = Expr::BinOp {
            left: Box::new(Expr::Number(1.0)),
            op: BinOp::Add,
            right: Box::new(Expr::Number(2.0)),
        };
        assert!(
            format!("{:?}", expr) == format!("{:?}", expected),
            "Parsed expr: {:?}, expected: {:?}",
            expr,
            expected
        );
    }

    #[test]
    fn test_parse_table() {
        let input = "{1, 2, 3}";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expr().unwrap();
        let expected = Expr::Table(vec![
            TableField {
                key: None,
                value: Expr::Number(1.0),
            },
            TableField {
                key: None,
                value: Expr::Number(2.0),
            },
            TableField {
                key: None,
                value: Expr::Number(3.0),
            },
        ]);
        assert!(
            format!("{:?}", expr) == format!("{:?}", expected),
            "Parsed expr: {:?}, expected: {:?}",
            expr,
            expected
        );
    }

    #[test]
    #[ignore = "key/value pairs are not supported yet"]
    fn test_parse_table_with_keys() {
        let input = "{a = 1, b = 2, c = 3}";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expr().unwrap();
        let expected = Expr::Table(vec![
            TableField {
                key: Some(Expr::Ident("a".to_string())),
                value: Expr::Number(1.0),
            },
            TableField {
                key: Some(Expr::Ident("b".to_string())),
                value: Expr::Number(2.0),
            },
            TableField {
                key: Some(Expr::Ident("c".to_string())),
                value: Expr::Number(3.0),
            },
        ]);
        assert!(
            format!("{:?}", expr) == format!("{:?}", expected),
            "Parsed expr: {:?}, expected: {:?}",
            expr,
            expected
        );
    }
}
