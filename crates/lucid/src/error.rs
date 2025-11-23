pub mod source {
    #[derive(Debug, Clone)]
    pub struct Location {
        pub line: usize,
        pub column: usize,
        pub offset: usize,
    }

    impl Location {
        pub fn new(line: usize, column: usize, offset: usize) -> Self {
            Self {
                line,
                column,
                offset,
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompileError {
    pub message: String,
    pub location: source::Location,
    pub source_line: String,
}

impl CompileError {
    pub fn format(&self) -> String {
        format!(
            "Error at line {}, column {}:\n{}\n{}\n{}^\n{}",
            self.location.line,
            self.location.column,
            self.source_line,
            " ".repeat(self.location.column.saturating_sub(1)),
            "",
            self.message
        )
    }
}
