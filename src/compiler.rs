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
