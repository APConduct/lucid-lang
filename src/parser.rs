use crate::{
    ast::{BinOp, Expr, Program, Stmt, TableField, Type, TypedIdent},
    lexer::{Lexer, Token},
};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next_token();
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

    pub fn peek(&self, offset: usize) -> &Token {
        self.tokens.get(self.pos + offset).unwrap_or(&Token::Eof)
    }

    fn advance(&mut self) -> Token {
        let tok = self.current().clone();
        self.pos += 1;
        tok
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        let current = self.current().clone();
        if std::mem::discriminant(&current) == std::mem::discriminant(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(format!("Expected {:?}, got {:?}", expected, current))
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut statements = Vec::new();
        while self.current() != &Token::Eof {
            statements.push(self.parse_stmt()?);
        }
        Ok(Program { statements })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        match self.current() {
            Token::Local => self.parse_local(),
            Token::Function => self.parse_function(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::Return => self.parse_return(),
            _ => {
                // Try to parse as expression statement or assignment
                let expr = self.parse_expr()?;

                // Check if this is an assignment
                if self.current() == &Token::Assign {
                    self.advance();
                    let /*mut*/ targets = vec![expr];
                    let mut values = vec![self.parse_expr()?];

                    while self.current() == &Token::Comma {
                        self.advance();
                        values.push(self.parse_expr()?);
                    }

                    return Ok(Stmt::Assign { targets, values });
                }

                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_local(&mut self) -> Result<Stmt, String> {
        self.expect(Token::Local)?;

        if self.current() == &Token::Function {
            return self.parse_local_function();
        }

        let mut vars = Vec::new();

        // Parse variable names with optional types
        loop {
            if let Token::Ident(name) = self.current() {
                let name = name.clone();
                self.advance();

                // Check for type annotation
                let ty = if self.current() == &Token::Colon {
                    self.advance();
                    Some(self.parse_type()?)
                } else {
                    None
                };

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

    fn parse_local_function(&mut self) -> Result<Stmt, String> {
        self.expect(Token::Function)?;

        let name = if let Token::Ident(n) = self.current() {
            let n = n.clone();
            self.advance();
            n
        } else {
            return Err("Expected function name after 'local function'".to_string());
        };

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
                params,
                return_types,
                body,
            }]),
        })
    }

    fn parse_function(&mut self) -> Result<Stmt, String> {
        self.expect(Token::Function)?;

        let name = if let Token::Ident(n) = self.current() {
            let n = n.clone();
            self.advance();
            n
        } else {
            return Err("Expected function name".to_string());
        };

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
            params,
            return_types,
            body,
        })
    }

    fn parse_if(&mut self) -> Result<Stmt, String> {
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
        self.expect(Token::While)?;
        let condition = self.parse_expr()?;
        self.expect(Token::Do)?;

        let mut body = Vec::new();
        while self.current() != &Token::End {
            body.push(self.parse_stmt()?);
        }

        self.expect(Token::End)?;

        Ok(Stmt::While { condition, body })
    }

    fn parse_return(&mut self) -> Result<Stmt, String> {
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

    fn parse_type(&mut self) -> Result<Type, String> {
        if let Token::Ident(type_name) = self.current() {
            let type_name = type_name.clone();
            self.advance();
            Type::from_str(&type_name).ok_or_else(|| format!("Unknown type: {}", type_name))
        } else {
            Err("Expected type name".to_string())
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, String> {
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
        let mut left = self.parse_comparison()?;

        while self.current() == &Token::And {
            self.advance();
            let right = self.parse_comparison()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op: BinOp::And,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_concat()?;

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
            let right = self.parse_concat()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_concat(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_additive()?;

        while self.current() == &Token::Concat {
            self.advance();
            let right = self.parse_additive()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op: BinOp::Concat,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_multiplicative()?;

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
        let mut left = self.parse_unary()?;

        loop {
            let op = match self.current() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => break,
            };

            self.advance();
            let right = self.parse_unary()?;
            left = Expr::BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        match self.current() {
            Token::Not | Token::Minus => {
                // TODO: Implement unary operators properly
                self.advance();
                self.parse_unary()
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;

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
                // Table constructor
                self.advance();
                let mut fields = Vec::new();

                while self.current() != &Token::RBrace {
                    // TODO: Parse table fields properly (key-value pairs)
                    let value = self.parse_expr()?;
                    fields.push(TableField { key: None, value });

                    if self.current() == &Token::Comma {
                        self.advance();
                    } else {
                        break;
                    }
                }

                self.expect(Token::RBrace)?;
                Ok(Expr::Table(fields))
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
