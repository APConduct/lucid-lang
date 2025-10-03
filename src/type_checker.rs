use std::collections::HashMap;

use crate::ast::{BinOp, Expr, Program, Stmt, Type};

#[derive(Debug, Clone)]
pub struct TypeChecker {
    scopes: Vec<HashMap<String, Type>>,
    generic_scopes: Vec<HashMap<String, Type>>,
    errors: Vec<String>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],         // Global scope
            generic_scopes: vec![HashMap::new()], // Global generic scope
            errors: Vec::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn push_generic_scope(&mut self) {
        self.generic_scopes.push(HashMap::new());
    }

    fn pop_generic_scope(&mut self) {
        self.generic_scopes.pop();
    }

    fn declare_generic(&mut self, name: String) {
        if let Some(scope) = self.generic_scopes.last_mut() {
            scope.insert(name.clone(), Type::Generic(name));
        }
    }

    pub fn lookup_generic(&self, name: &str) -> Option<Type> {
        for scope in self.generic_scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn declare(&mut self, name: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    fn lookup(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn error(&mut self, msg: String) {
        self.errors.push(msg);
    }

    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<String>> {
        for stmt in &program.statements {
            self.check_stmt(stmt);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Local { vars, init } => {
                // Check initializers first to get their types
                let init_types = if let Some(exprs) = init {
                    exprs.iter().map(|e| self.check_expr(e)).collect::<Vec<_>>()
                } else {
                    vec![]
                };

                // Declare variables with their annotated or inferred types
                for (i, var) in vars.iter().enumerate() {
                    let declared_type = var.ty.clone().unwrap_or(Type::Any);

                    // If there's an initializer, check type compatibility
                    if let Some(init_type) = init_types.get(i) {
                        if !self.types_compatible(&declared_type, init_type) {
                            self.error(format!(
                                "Type mismatch for variable '{}': expected {:?}, got {:?}",
                                var.name, declared_type, init_type
                            ));
                        }
                    }

                    self.declare(var.name.clone(), declared_type);
                }
            }

            Stmt::Assign { targets, values } => {
                let target_types: Vec<Type> = targets.iter().map(|t| self.check_expr(t)).collect();
                let value_types: Vec<Type> = values.iter().map(|v| self.check_expr(v)).collect();

                for (i, (target_type, value_type)) in
                    target_types.iter().zip(value_types.iter()).enumerate()
                {
                    if !self.types_compatible(target_type, value_type) {
                        self.error(format!(
                            "Type mismatch in assignment {}: expected {:?}, got {:?}",
                            i, target_type, value_type
                        ));
                    }
                }
            }

            Stmt::FunctionDecl {
                name,
                generic_params,
                params,
                return_types,
                body,
            } => {
                // Create function type
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| p.ty.clone().unwrap_or(Type::Any))
                    .collect();
                let func_type = Type::Function(param_types.clone(), return_types.clone());

                // Declare the function in current scope
                self.declare(name.clone(), func_type);

                // Check function body in new scope
                self.push_scope();
                self.pop_generic_scope();

                // Declare generic type parameters
                for generic_param in generic_params {
                    self.declare_generic(generic_param.name.clone());
                }

                // Declare parameters
                for param in params {
                    let param_type = param.ty.clone().unwrap_or(Type::Any);
                    self.declare(param.name.clone(), param_type);
                }

                // Check body statements
                for s in body {
                    self.check_stmt(s);
                }

                self.pop_generic_scope();
                self.pop_scope();
            }

            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                let _cond_type = self.check_expr(condition);
                // In Lua, any value can be used as a condition (nil and false are falsy)
                // But we can warn if it's always true/false

                self.push_scope();
                for s in then_block {
                    self.check_stmt(s);
                }
                self.pop_scope();

                if let Some(else_stmts) = else_block {
                    self.push_scope();
                    for s in else_stmts {
                        self.check_stmt(s);
                    }
                    self.pop_scope();
                }
            }

            Stmt::While { condition, body } => {
                let _cond_type = self.check_expr(condition);

                self.push_scope();
                for s in body {
                    self.check_stmt(s);
                }
                self.pop_scope();
            }

            Stmt::Return(exprs) => {
                tracing::warn!("Proper return type checking is not implemented yet");
                // TODO: Check return types match function signature
                for expr in exprs {
                    self.check_expr(expr);
                }
            }

            Stmt::Expr(expr) => {
                self.check_expr(expr);
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Number(_) => Type::Number,
            Expr::String(_) => Type::String,
            Expr::Boolean(_) => Type::Boolean,
            Expr::Nil => Type::Nil,

            Expr::Ident(name) => self.lookup(name).unwrap_or_else(|| {
                self.error(format!("Undefined variable: {}", name));
                Type::Any
            }),

            Expr::BinOp { left, op, right } => {
                let left_type = self.check_expr(left);
                let right_type = self.check_expr(right);

                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        if !self.is_numeric(&left_type) {
                            self.error(format!(
                                "Expected number in arithmetic operation, got {:?}",
                                left_type
                            ));
                        }
                        if !self.is_numeric(&right_type) {
                            self.error(format!(
                                "Expected number in arithmetic operation, got {:?}",
                                right_type
                            ));
                        }
                        Type::Number
                    }

                    BinOp::Concat => {
                        // Lua's .. operator works with strings and numbers
                        if !matches!(left_type, Type::String | Type::Number | Type::Any) {
                            self.error(format!("Cannot concatenate {:?}", left_type));
                        }
                        if !matches!(right_type, Type::String | Type::Number | Type::Any) {
                            self.error(format!("Cannot concatenate {:?}", right_type));
                        }
                        Type::String
                    }

                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                        Type::Boolean
                    }

                    BinOp::And | BinOp::Or => {
                        // In Lua, these return one of the operands, not necessarily boolean
                        // For simplicity, we'll say they return the same type as the operands
                        // if they match, otherwise Any
                        if self.types_compatible(&left_type, &right_type) {
                            left_type
                        } else {
                            Type::Any
                        }
                    }
                }
            }

            Expr::Call { func, args } => {
                let func_type = self.check_expr(func);

                match func_type {
                    Type::Function(param_types, return_types) => {
                        // Check argument count
                        if args.len() != param_types.len() {
                            self.error(format!(
                                "Function expects {} arguments, got {}",
                                param_types.len(),
                                args.len()
                            ));
                        }

                        // Check argument types
                        for (i, (arg, expected_type)) in
                            args.iter().zip(param_types.iter()).enumerate()
                        {
                            let arg_type = self.check_expr(arg);
                            if !self.types_compatible(expected_type, &arg_type) {
                                self.error(format!(
                                    "Argument {} type mismatch: expected {:?}, got {:?}",
                                    i + 1,
                                    expected_type,
                                    arg_type
                                ));
                            }
                        }

                        // Return first return type, or Nil if no returns
                        return_types.first().cloned().unwrap_or(Type::Nil)
                    }
                    Type::Any => Type::Any,
                    _ => {
                        self.error(format!(
                            "Attempting to call non-function type: {:?}",
                            func_type
                        ));
                        Type::Any
                    }
                }
            }

            Expr::Function {
                generic_params,
                params,
                return_types,
                body,
            } => {
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| p.ty.clone().unwrap_or(Type::Any))
                    .collect();

                // Check function body in new scope
                self.push_scope();
                self.push_generic_scope();

                // Declare generic type parameters
                for generic_param in generic_params {
                    self.declare_generic(generic_param.name.clone());
                }

                for param in params {
                    let param_type = param.ty.clone().unwrap_or(Type::Any);
                    self.declare(param.name.clone(), param_type);
                }

                for s in body {
                    self.check_stmt(s);
                }

                self.pop_generic_scope();
                self.pop_scope();

                Type::Function(param_types, return_types.clone())
            }

            Expr::Table(fields) => {
                // For now, just check field expressions
                for field in fields {
                    if let Some(key) = &field.key {
                        self.check_expr(key);
                    }
                    self.check_expr(&field.value);
                }
                Type::Table
            }

            Expr::Index { table, index } => {
                let table_type = self.check_expr(table);
                let _index_type = self.check_expr(index);

                // For now, table indexing returns Any
                // Later we can add more sophisticated table typing
                if !matches!(table_type, Type::Table | Type::Any) {
                    self.error(format!(
                        "Attempting to index non-table type: {:?}",
                        table_type
                    ));
                }
                Type::Any
            }
        }
    }

    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        // Any is compatible with everything
        if matches!(expected, Type::Any) || matches!(actual, Type::Any) {
            return true;
        }

        // Generic types are compatible with anything (they're placeholders)
        if matches!(expected, Type::Generic(_)) || matches!(actual, Type::Generic(_)) {
            return true;
        }

        // If expected is a union, actual must be compatible with at least one variant
        if let Type::Union(expected_types) = expected {
            return expected_types
                .iter()
                .any(|t| self.types_compatible(t, actual));
        }

        // If actual is a union, all variants must be compatible with expected
        if let Type::Union(actual_types) = actual {
            return actual_types
                .iter()
                .all(|t| self.types_compatible(expected, t));
        }

        // Check if types are equal
        match (expected, actual) {
            (Type::Number, Type::Number) => true,
            (Type::String, Type::String) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Nil, Type::Nil) => true,
            (Type::Table, Type::Table) => true,
            (Type::Function(p1, r1), Type::Function(p2, r2)) => {
                p1.len() == p2.len()
                    && p1
                        .iter()
                        .zip(p2.iter())
                        .all(|(a, b)| self.types_compatible(a, b))
                    && r1.len() == r2.len()
                    && r1
                        .iter()
                        .zip(r2.iter())
                        .all(|(a, b)| self.types_compatible(a, b))
            }
            (Type::UserDefined(n1), Type::UserDefined(n2)) => n1 == n2,
            _ => false,
        }
    }

    fn is_numeric(&self, ty: &Type) -> bool {
        matches!(ty, Type::Number | Type::Any)
    }

    pub fn is_string_or_number(&self, ty: &Type) -> bool {
        match ty {
            Type::String | Type::Number | Type::Any => true,
            Type::Union(types) => types.iter().all(|t| self.is_string_or_number(t)),
            _ => false,
        }
    }
}
