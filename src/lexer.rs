use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Local,
    Function,
    If,
    Then,
    Else,
    End,
    While,
    Do,
    Return,
    And,
    Or,
    Not,
    True,
    False,
    Nil,
    Interface,

    // Literals
    Number(f64),
    String(String),
    Ident(String),

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    EqEq,
    NotEq,
    Lt,
    Le,
    Gt,
    Ge,
    Assign,
    Concat,

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Dot,
    Colon,
    Semicolon,

    LAngle,
    RAngle,

    Pipe, // | for union types
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Ident(i) => write!(f, "{}", i),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::EqEq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::Lt => write!(f, "<"),
            Token::Le => write!(f, "<="),
            Token::Gt => write!(f, ">"),
            Token::Ge => write!(f, ">="),
            Token::Assign => write!(f, "="),
            Token::Concat => write!(f, ".."),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::LAngle => write!(f, "<"),
            Token::RAngle => write!(f, ">"),
            Token::Pipe => write!(f, "|"),
            Token::Eof => write!(f, "EOF"),
            Token::Nil => write!(f, "nil"),
            _ => todo!("Implement token formatting for {:?}", self),
        }
    }
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn current(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    pub fn peek(&self, offset: usize) -> Option<char> {
        self.input.get(self.pos + offset).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.current();
        self.pos += 1;
        c
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current() {
            if c.is_whitespace() {
                self.advance();
            } else if c == '-' && self.peek(1) == Some('-') {
                // Found a comment, skip it
                self.skip_comment();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        // Skip --
        self.advance();
        self.advance();

        // Check for block comment --[[
        if self.current() == Some('[') && self.peek(1) == Some('[') {
            self.advance(); // [
            self.advance(); // [

            // Skip until ]]
            while let Some(c) = self.current() {
                if c == ']' && self.peek(1) == Some(']') {
                    self.advance(); // ]
                    self.advance(); // ]
                    break;
                }
                self.advance();
            }
        } else {
            // Single-line comment: skip until newline
            while let Some(c) = self.current() {
                if c == '\n' {
                    break;
                }
                self.advance();
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.current() {
            None => Token::Eof,
            Some(c) if c.is_ascii_digit() => self.number(),
            Some(c) if c.is_alphabetic() || c == '_' => self.ident_or_keyword(),
            Some('"') | Some('\'') => self.string(),
            Some('+') => {
                self.advance();
                Token::Plus
            }
            Some('-') => {
                // Check if this is a comment
                if self.peek(1) == Some('-') {
                    self.skip_comment();
                    // After skipping comment, get next token
                    return self.next_token();
                }
                self.advance();
                Token::Minus
            }
            Some('*') => {
                self.advance();
                Token::Star
            }
            Some('/') => {
                self.advance();
                Token::Slash
            }
            Some('%') => {
                self.advance();
                Token::Percent
            }
            Some('|') => {
                self.advance();
                Token::Pipe
            }
            Some('(') => {
                self.advance();
                Token::LParen
            }
            Some(')') => {
                self.advance();
                Token::RParen
            }
            Some('{') => {
                self.advance();
                Token::LBrace
            }
            Some('}') => {
                self.advance();
                Token::RBrace
            }
            Some('[') => {
                self.advance();
                Token::LBracket
            }
            Some(']') => {
                self.advance();
                Token::RBracket
            }
            Some(',') => {
                self.advance();
                Token::Comma
            }
            Some('.') => {
                self.advance();
                if self.current() == Some('.') {
                    self.advance();
                    Token::Concat
                } else {
                    Token::Dot
                }
            }
            Some(':') => {
                self.advance();
                Token::Colon
            }
            Some(';') => {
                self.advance();
                Token::Semicolon
            }
            Some('=') => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    Token::EqEq
                } else {
                    Token::Assign
                }
            }
            Some('~') => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    Token::NotEq
                } else {
                    panic!("Unexpected character: ~");
                }
            }
            Some('<') => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    Token::Le
                } else {
                    Token::Lt
                }
            }
            Some('>') => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    Token::Ge
                } else {
                    Token::Gt
                }
            }
            Some(c) => panic!("Unexpected character: {}", c),
        }
    }

    fn number(&mut self) -> Token {
        let mut num = String::new();
        while let Some(c) = self.current() {
            if c.is_ascii_digit() || c == '.' {
                num.push(c);
                self.advance();
            } else {
                break;
            }
        }
        Token::Number(num.parse().unwrap())
    }

    fn ident_or_keyword(&mut self) -> Token {
        let mut ident = String::new();
        while let Some(c) = self.current() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }

        match ident.as_str() {
            "local" => Token::Local,
            "function" => Token::Function,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "end" => Token::End,
            "while" => Token::While,
            "do" => Token::Do,
            "return" => Token::Return,
            "and" => Token::And,
            "or" => Token::Or,
            "not" => Token::Not,
            "true" => Token::True,
            "false" => Token::False,
            "nil" => Token::Nil,
            "interface" => Token::Interface,
            _ => Token::Ident(ident),
        }
    }

    fn string(&mut self) -> Token {
        let quote = self.advance().unwrap();
        let mut s = String::new();
        while let Some(c) = self.current() {
            if c == quote {
                self.advance();
                break;
            }
            s.push(c);
            self.advance();
        }
        Token::String(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let input = r#"
                local x = 10
                function add(a, b)
                    return a + b
                end
                if x > 5 then
                    print("x is greater than 5")
                else
                    print("x is less than or equal to 5")
                end
                while x > 0 do
                    print(x)
                    x = x - 1
                end
                return x
            "#;

        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next_token();
            if tok == Token::Eof {
                break;
            }
            tokens.push(tok);
        }

        assert_eq!(tokens.len(), 48);
    }

    #[test]
    fn test_lexer_numbers() {
        let mut lexer = Lexer::new("123 45.67");
        assert_eq!(lexer.next_token(), Token::Number(123.0));
        assert_eq!(lexer.next_token(), Token::Number(45.67));
    }

    #[test]
    fn test_lexer_keywords() {
        let mut lexer = Lexer::new("local function if then end");
        assert_eq!(lexer.next_token(), Token::Local);
        assert_eq!(lexer.next_token(), Token::Function);
        assert_eq!(lexer.next_token(), Token::If);
        assert_eq!(lexer.next_token(), Token::Then);
        assert_eq!(lexer.next_token(), Token::End);
    }

    #[test]
    fn test_single_line_comment() {
        let input = "local x = 42 -- this is a comment\nlocal y = 10";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::Local);
        assert_eq!(lexer.next_token(), Token::Ident("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Number(42.0));
        // Comment should be skipped
        assert_eq!(lexer.next_token(), Token::Local);
        assert_eq!(lexer.next_token(), Token::Ident("y".to_string()));
    }

    #[test]
    fn test_block_comment() {
        let input = "local x = 1 --[[ this is a block comment ]] local y = 2";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::Local);
        assert_eq!(lexer.next_token(), Token::Ident("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Number(1.0));
        // Block comment should be skipped
        assert_eq!(lexer.next_token(), Token::Local);
        assert_eq!(lexer.next_token(), Token::Ident("y".to_string()));
    }

    #[test]
    fn test_multiline_block_comment() {
        let input = r#"local x = 1
--[[
This is a
multi-line
comment
]]
local y = 2"#;
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::Local);
        assert_eq!(lexer.next_token(), Token::Ident("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Number(1.0));
        // Multi-line block comment should be skipped
        assert_eq!(lexer.next_token(), Token::Local);
        assert_eq!(lexer.next_token(), Token::Ident("y".to_string()));
    }

    #[test]
    fn test_comment_at_start() {
        let input = "-- comment at start\nlocal x = 5";
        let mut lexer = Lexer::new(input);

        // Comment should be skipped automatically
        assert_eq!(lexer.next_token(), Token::Local);
        assert_eq!(lexer.next_token(), Token::Ident("x".to_string()));
    }

    #[test]
    fn test_minus_vs_comment() {
        let input = "5 - 3"; // Subtraction, not a comment
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::Number(5.0));
        assert_eq!(lexer.next_token(), Token::Minus);
        assert_eq!(lexer.next_token(), Token::Number(3.0));
    }

    #[test]
    fn test_multiple_comments() {
        let input = r#"
-- First comment
local x = 1
-- Second comment
local y = 2
--[[ Block comment ]]
local z = 3
"#;
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::Local);
        assert_eq!(lexer.next_token(), Token::Ident("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Number(1.0));

        assert_eq!(lexer.next_token(), Token::Local);
        assert_eq!(lexer.next_token(), Token::Ident("y".to_string()));
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Number(2.0));

        assert_eq!(lexer.next_token(), Token::Local);
        assert_eq!(lexer.next_token(), Token::Ident("z".to_string()));
    }
}

/*
function foo(x: number, y: number): number
    return x + y
end
*/
