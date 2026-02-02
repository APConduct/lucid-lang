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

pub mod compile {
    #[derive(Debug, Clone)]
    pub struct Error {
        pub message: String,
        pub location: super::source::Location,
        pub source_line: String,
    }
    impl Error {
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
}
