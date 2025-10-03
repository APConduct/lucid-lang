use crate::ast::{BinOp, Expr, Program, Stmt};

pub struct CodeGen {
    output: String,
    indent: usize,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
        }
    }

    fn emit(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn emit_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
    }

    fn emit_line(&mut self, s: &str) {
        self.emit_indent();
        self.emit(s);
        self.emit("\n");
    }

    pub fn generate(&mut self, program: &Program) -> String {
        for stmt in &program.statements {
            self.gen_stmt(stmt);
        }
        self.output.clone()
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Local { vars, init } => {
                self.emit_indent();
                self.emit("local ");
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&var.name);
                }
                if let Some(exprs) = init {
                    self.emit(" = ");
                    for (i, expr) in exprs.iter().enumerate() {
                        if i > 0 {
                            self.emit(", ");
                        }
                        self.gen_expr(expr);
                    }
                }
                self.emit("\n");
            }
            Stmt::FunctionDecl {
                name, params, body, ..
            } => {
                self.emit_indent();
                self.emit(&format!("function {}(", name));
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&param.name);
                }
                self.emit(")\n");
                self.indent += 1;
                for s in body {
                    self.gen_stmt(s);
                }
                self.indent -= 1;
                self.emit_line("end");
            }
            Stmt::Return(exprs) => {
                self.emit_indent();
                self.emit("return");
                if !exprs.is_empty() {
                    self.emit(" ");
                    for (i, expr) in exprs.iter().enumerate() {
                        if i > 0 {
                            self.emit(", ");
                        }
                        self.gen_expr(expr);
                    }
                }
                self.emit("\n");
            }
            Stmt::Expr(expr) => {
                self.emit_indent();
                self.gen_expr(expr);
                self.emit("\n");
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.emit_indent();
                self.emit("if ");
                self.gen_expr(condition);
                self.emit(" then\n");
                self.indent += 1;
                for s in then_block {
                    self.gen_stmt(s);
                }
                self.indent -= 1;
                if let Some(else_stmts) = else_block {
                    self.emit_line("else");
                    self.indent += 1;
                    for s in else_stmts {
                        self.gen_stmt(s);
                    }
                    self.indent -= 1;
                }
                self.emit_line("end");
            }
            Stmt::While { condition, body } => {
                self.emit_indent();
                self.emit("while ");
                self.gen_expr(condition);
                self.emit(" do\n");
                self.indent += 1;
                for s in body {
                    self.gen_stmt(s);
                }
                self.indent -= 1;
                self.emit_line("end");
            }
            Stmt::Assign { targets, values } => {
                self.emit_indent();
                for (i, target) in targets.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.gen_expr(target);
                }
                self.emit(" = ");
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.gen_expr(value);
                }
                self.emit("\n");
            }
            #[allow(unreachable_patterns)]
            _ => {
                // TODO: Implement other statement types
                self.emit_line("-- TODO: Implement statement");
            }
        }
    }

    fn gen_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Number(n) => self.emit(&n.to_string()),
            Expr::String(s) => self.emit(&format!("\"{}\"", s)),
            Expr::Boolean(b) => self.emit(if *b { "true" } else { "false" }),
            Expr::Nil => self.emit("nil"),
            Expr::Ident(name) => self.emit(name),
            Expr::BinOp { left, op, right } => {
                self.emit("(");
                self.gen_expr(left);
                self.emit(&format!(" {} ", self.op_to_str(op.clone())));
                self.gen_expr(right);
                self.emit(")");
            }
            Expr::Call { func, args } => {
                self.gen_expr(func);
                self.emit("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.gen_expr(arg);
                }
                self.emit(")");
            }
            Expr::Function { params, body, .. } => {
                self.emit("function(");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&param.name);
                }
                self.emit(")\n");
                self.indent += 1;
                for s in body {
                    self.gen_stmt(s);
                }
                self.indent -= 1;
                self.emit_indent();
                self.emit("end");
            }
            Expr::Table(fields) => {
                self.emit("{");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    if let Some(key) = &field.key {
                        self.emit("[");
                        self.gen_expr(key);
                        self.emit("] = ");
                    }
                    self.gen_expr(&field.value);
                }
                self.emit("}");
            }
            Expr::Index { table, index } => {
                self.gen_expr(table);
                self.emit("[");
                self.gen_expr(index);
                self.emit("]");
            }
            #[allow(unreachable_patterns)]
            _ => self.emit("nil"), // TODO: Implement other expressions
        }
    }

    fn op_to_str(&self, op: BinOp) -> &'static str {
        match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Eq => "==",
            BinOp::Ne => "~=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "and",
            BinOp::Or => "or",
            BinOp::Concat => "..",
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Type, TypedIdent};

    use super::*;

    #[test]
    fn test_codegen_print() {
        // Test codegen for: print("Hello, world!")
        use crate::ast::{Expr, Stmt};

        let ast = Stmt::Expr(Expr::Call {
            func: Box::new(Expr::Ident("print".to_string())),
            args: vec![Expr::String("Hello, world!".to_string())],
        });

        let mut codegen = CodeGen::new();
        codegen.gen_stmt(&ast);
        assert_eq!(codegen.output.trim(), "print(\"Hello, world!\")");
    }

    #[test]
    fn test_codegen_function() {
        // function add(x, y)
        //   return x + y
        // end
        use crate::ast::{BinOp, Expr, Stmt, TypedIdent};

        let ast = Stmt::FunctionDecl {
            name: "add".to_string(),
            params: vec![
                TypedIdent {
                    name: "x".to_string(),
                    ty: None,
                },
                TypedIdent {
                    name: "y".to_string(),
                    ty: None,
                },
            ],
            return_types: Vec::new(),
            body: vec![Stmt::Return(vec![Expr::BinOp {
                left: Box::new(Expr::Ident("x".to_string())),
                op: BinOp::Add,
                right: Box::new(Expr::Ident("y".to_string())),
            }])],
            generic_params: Vec::new(),
        };

        let mut codegen = CodeGen::new();
        codegen.gen_stmt(&ast);
        let expected = r#"
function add(x, y)
    return (x + y)
end"#;
        assert_eq!(codegen.output.trim(), expected.trim());
    }

    #[test]
    fn test_codegen_if_else() {
        // if x > 0 then
        //   print("positive")
        // else
        //   print("non-positive")
        // end
        use crate::ast::{BinOp, Expr, Stmt};

        let ast = Stmt::If {
            condition: Expr::BinOp {
                left: Box::new(Expr::Ident("x".to_string())),
                op: BinOp::Gt,
                right: Box::new(Expr::Number(0.0)),
            },
            then_block: vec![Stmt::Expr(Expr::Call {
                func: Box::new(Expr::Ident("print".to_string())),
                args: vec![Expr::String("positive".to_string())],
            })],
            else_block: Some(vec![Stmt::Expr(Expr::Call {
                func: Box::new(Expr::Ident("print".to_string())),
                args: vec![Expr::String("non-positive".to_string())],
            })]),
        };

        let mut codegen = CodeGen::new();
        codegen.gen_stmt(&ast);
        let expected = r#"if (x > 0) then
    print("positive")
else
    print("non-positive")
end"#;
        assert_eq!(codegen.output.trim(), expected);
    }

    #[test]
    fn test_codegen_local_assign() {
        // local a, b = 1, 2
        use crate::ast::{Expr, Stmt, TypedIdent};

        let ast = Stmt::Local {
            vars: vec![
                TypedIdent {
                    name: "a".to_string(),
                    ty: None,
                },
                TypedIdent {
                    name: "b".to_string(),
                    ty: None,
                },
            ],
            init: Some(vec![Expr::Number(1.0), Expr::Number(2.0)]),
        };

        let mut codegen = CodeGen::new();
        codegen.gen_stmt(&ast);
        let expected = r#"local a, b = 1, 2"#;
        assert_eq!(codegen.output.trim(), expected);
    }

    #[test]
    fn test_codegen_table() {
        // {1, 2, 3}
        use crate::ast::{Expr, Stmt, TableField};

        let ast = Stmt::Expr(Expr::Table(vec![
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
        ]));

        let mut codegen = CodeGen::new();
        codegen.gen_stmt(&ast);
        let expected = r#"{1, 2, 3}"#;
        assert_eq!(codegen.output.trim(), expected);
    }

    #[test]
    fn test_codegen_table_with_keys() {
        // {["a"] = 1, ["b"] = 2}
        use crate::ast::{Expr, Stmt, TableField};

        let ast = Stmt::Expr(Expr::Table(vec![
            TableField {
                key: Some(Expr::String("a".to_string())),
                value: Expr::Number(1.0),
            },
            TableField {
                key: Some(Expr::String("b".to_string())),
                value: Expr::Number(2.0),
            },
        ]));

        let mut codegen = CodeGen::new();
        codegen.gen_stmt(&ast);
        let expected = r#"{["a"] = 1, ["b"] = 2}"#;
        assert_eq!(codegen.output.trim(), expected);
    }

    #[test]
    fn test_codegen_simple() {
        let program = Program {
            statements: vec![Stmt::Local {
                vars: vec![TypedIdent {
                    name: "x".to_string(),
                    ty: Some(Type::Number),
                }],
                init: Some(vec![Expr::Number(42.0)]),
            }],
        };

        let mut codegen = CodeGen::new();
        let output = codegen.generate(&program);
        assert_eq!(output.trim(), "local x = 42");
    }
}
