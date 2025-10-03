use crate::{codegen::CodeGen, lexer::Lexer, parser::Parser, type_checker::TypeChecker};

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

        // Type check AST
        let mut type_checker = TypeChecker::new();
        type_checker
            .check_program(&program)
            .map_err(|errors| errors.join("\n"))?;

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

    #[test]
    fn test_union_types() {
        let source = r#"
  local x: number | string = 42
  local y: number | string = "hello"
  "#;
        let mut compiler = Compiler::new();
        assert!(compiler.compile(source).is_ok());
    }
    #[test]
    fn test_union_type_error() {
        let source = r#"
   local x: number | string = true
   "#;
        let mut compiler = Compiler::new();
        let result = compiler.compile(source);
        assert!(
            result.is_err(),
            "No error here. this is the result: {}",
            result.unwrap()
        );
        assert!(result.unwrap_err().contains("Type mismatch"));
    }

    #[test]
    fn test_union_in_function() {
        let source = r#"
   function process(value: number | string): string
       return "processed"
   end

   local a = process(42)
   local b = process("test")
   "#;
        let mut compiler = Compiler::new();
        let result = compiler.compile(source);
        assert!(
            result.is_ok(),
            "Failed to compile union in function. Error: {}",
            result.err().unwrap()
        );
    }

    #[test]
    fn test_union_with_nil() {
        tracing_subscriber::fmt::init();
        let source = r#"
   local x: number | nil = 42
   local y: number | nil = nil
   "#;
        let mut compiler = Compiler::new();
        let result = compiler.compile(source);
        assert!(
            result.is_ok(),
            "Failed to compile union with nil. Error: {}",
            result.err().unwrap()
        );
    }

    #[test]
    fn test_complex_union() {
        let source = r#"
   function getId(obj: table): number | string | nil
       return 42
   end
   "#;
        let mut compiler = Compiler::new();
        assert!(
            compiler.compile(source).is_ok(),
            "Failed to compile complex union. Error: {}",
            compiler.compile(source).err().unwrap()
        );
    }

    #[test]
    fn test_generic_with_union() {
        let source = r#"
   function getOrDefault<T>(value: T | nil, default: T): T
       return value
   end

   local x: number = getOrDefault(42, 0)
   local y: string = getOrDefault(nil, "default")
   "#;
        let mut compiler = Compiler::new();
        assert!(
            compiler.compile(source).is_ok(),
            "Failed to compile generic with union. Error: {}",
            compiler.compile(source).err().unwrap()
        );
    }

    #[test]
    fn test_comments_are_stripped() {
        let source = r#"
-- This is a single-line comment
local x: number = 42

--[[
This is a multi-line block comment
It spans multiple lines
]]
local y: string = "hello"

function add(a: number, b: number): number
    -- Comment inside function
    return a + b -- inline comment
end

--[[ Block comment ]] local z = add(x, 10)
"#;
        let mut compiler = Compiler::new();
        let result = compiler.compile(source);
        assert!(
            result.is_ok(),
            "Failed to compile code with comments. Error: {}",
            result.err().unwrap()
        );

        let output = result.unwrap();
        // Comments should not appear in output
        assert!(!output.contains("single-line comment"));
        assert!(!output.contains("multi-line block comment"));
        assert!(!output.contains("Comment inside function"));
        assert!(!output.contains("inline comment"));

        // But the actual code should be present
        assert!(output.contains("local x = 42"));
        assert!(output.contains("local y = \"hello\""));
        assert!(output.contains("function add(a, b)"));
        assert!(output.contains("local z = add(x, 10)"));
    }

    #[test]
    fn test_interface_declaration() {
        let source = r#"
interface Person
    name: string
    age: number
end
"#;
        let mut compiler = Compiler::new();
        assert!(compiler.compile(source).is_ok());
    }
}
