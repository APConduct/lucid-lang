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

    Eof,
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
            } else {
                break;
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
