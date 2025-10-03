use crate::{codegen::CodeGen, lexer::Lexer, parser::Parser};

pub struct Compiler {
    // Future: Add type checker, symbol table, etc.
}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&mut self, source: &str) -> Result<String, String> {
        // Lex and parse
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program()?;

        // TODO: Type check AST

        // Generate Lua code
        let mut codegen = CodeGen::new();
        let lua_code = codegen.generate(&program);

        Ok(lua_code)
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;

    #[test]
    fn test_compile_function() {
        let source = "function foo(x: number, y: number): number\n    return x + y\nend";
        let mut compiler = Compiler::new();
        let result = compiler.compile(source).unwrap();

        // Type annotations should be erased
        assert!(result.contains("function foo(x, y)"));
        assert!(result.contains("return (x + y)"));
        assert!(!result.contains("number")); // Types are erased
    }

    #[test]
    fn test_compile_generic_function() {
        let source = r#"
function identity<T>(x: T): T
    return x
end
"#;
        let mut compiler = Compiler::new();
        let result = compiler.compile(source).unwrap();

        // Generics should be erased in output
        assert!(result.contains("function identity(x)"));
        assert!(!result.contains("T"));
        assert!(!result.contains("<"));
        assert!(!result.contains(">"));
    }

    #[test]
    fn test_generic_local_function() {
        let source = r#"
local function swap<T, U>(a: T, b: U): U, T
    return b, a
end
"#;
        let mut compiler = Compiler::new();
        assert!(compiler.compile(source).is_ok());
    }

    #[test]
    fn test_generic_function_map() {
        let source = r#"
function map<T, U>(items: T, f: U): table
    local result = {}
    return result
end
"#;
        let mut compiler = Compiler::new();
        let res = compiler.compile(source);
        assert!(
            res.is_ok(),
            "Failed to compile generic function map. Error: {}",
            res.err().unwrap()
        );
    }
}
