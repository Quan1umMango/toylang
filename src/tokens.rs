#[derive(Debug, PartialEq, Copy, Clone)]
pub enum DataType {
    Int32,
    Bool,
    Infer // Type has not been assigned yet, the parser will infer it later on
}
 


#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    LET,
    ASSIGN,
    ADD_ASSIGN,
    SUB_ASSIGN,
    MULT_ASSIGN,
    DIV_ASSIGN,
    EQ,
    NEQ,

    ADD,
    SUB,
    MULT,
    DIV,
    MOD,

    NUM,

    EXIT,
    SEMICOLON,

    IDENT,
    OPEN_PAREN,
    CLOSE_PAREN,

    OPEN_CURLY,
    CLOSE_CURLY,

    OPEN_SQUARE,
    CLOSE_SQUARE,

    IF,
    ELSE,

    TRUE,
    FALSE,

    AND,
    OR,
    NOT,

    WHILE

}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub value: Option<String>,
    pub datatype: Option<DataType>
}

impl Token {
    pub fn process_word(word: String) -> Token {
        match word.as_str() {
            "exit" => {
                return Token {
                    token_type: TokenType::EXIT,
                    value: None,
                    datatype:None,
                }
            }
            "let" => {
                return Token {
                    token_type: TokenType::LET,
                    value: None,
                    datatype:None,
                }
            }
            "if" => {
                return Token {
                    token_type:TokenType::IF,
                    value:None,
                    datatype:None,
                }
            },
            "else" => {
                return Token {
                    token_type: TokenType::ELSE,
                    value:None,
                    datatype:None,
                }
            }
            "true" => {
                return Token {
                    token_type: TokenType::TRUE,
                    value:None,
                    datatype:Some(DataType::Bool),
                }
            }
            "false" => {
                return Token {
                    token_type: TokenType::FALSE,
                    value:None,
                    datatype:Some(DataType::Bool),
                }
            }
            "and" => {
                return Token {
                    token_type: TokenType::AND,
                    value:None,
                    datatype:None,
                }
            }
            "or" => {
                return Token {
                    token_type: TokenType::OR,
                    value:None,
                    datatype:None
                }
            },
            "while" => {
                return Token {
                    token_type: TokenType::WHILE,
                    value:None,
                    datatype:None,
                }
            }
            _ => {
                return Token {
                    token_type: TokenType::IDENT,
                    value: Some(word),
                    datatype:Some(DataType::Infer),
                }
            }
        }
    }
}

pub struct Tokenizer {
    input: String,
    char_index: usize,
}

impl Tokenizer {
    pub fn new(input: String) -> Tokenizer {
        Tokenizer {
            input,
            char_index: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut buf = String::new();
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(ch) = self.peek_char() {
            if ch.is_alphabetic() || ch =='_' {
                buf.push(ch);
                self.consume_char();

                while let Some(next_ch) = self.peek_char() {
                    if next_ch.is_alphanumeric() {
                        buf.push(self.consume_char().unwrap());
                    } else {
                        break;
                    }
                }

                tokens.push(Token::process_word(buf.clone()));
                buf.clear();
            } else if ch.is_numeric() {
                buf.push(ch);
                self.consume_char();

                while let Some(next_ch) = self.peek_char() {
                    if next_ch.is_numeric() {
                        buf.push(self.consume_char().unwrap());
                    } else {
                        break;
                    }
                }

                tokens.push(Token {
                    token_type: TokenType::NUM,
                    value: Some(buf.clone()), 
                    datatype: Some(DataType::Int32),
                });
                buf.clear();
            } else if ch.is_whitespace() {
                self.consume_char(); // Skip whitespace
                if !buf.is_empty() {
                    tokens.push(Token::process_word(buf.clone()));
                    buf.clear();
                }
            } else {
                match ch {
                    '=' => {
                        if self.peek_char_offset(1) == Some('=') {
                            self.consume_char();
                            tokens.push(Token {
                                token_type: TokenType::EQ,
                                value: None,
                                datatype:None,

                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::ASSIGN,
                                value: None,
                                datatype:None,
                            });
                        }
                    }
                    '!' => {
                        if self.peek_char_offset(1) == Some('=') {
                            self.consume_char();
                            tokens.push(Token {
                                token_type: TokenType::NEQ,
                                value:None,
                                datatype:None,
                            });
                        }else {
                            tokens.push(Token {
                                token_type:TokenType::NOT,
                                value:None,
                                datatype:None,
                            });
                        }
                    }
                    '+' => {
                        if self.peek_char_offset(1) == Some('=') {
                            self.consume_char();
                            tokens.push(Token {
                                token_type: TokenType::ADD_ASSIGN,
                                value: None,
                    datatype:None,
                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::ADD,
                                value: None,
                    datatype:None,
                            });
                        }
                    },
                    '-' => {
                        if self.peek_char_offset(1) == Some('=') {
                            self.consume_char();
                            tokens.push(Token {
                                token_type: TokenType::SUB_ASSIGN,
                                value: None,
                    datatype:None,
                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::SUB,
                                value: None,
                    datatype:None,
                            });
                        }
                    },
                    '*' => {
                        if self.peek_char_offset(1) == Some('=') {

                            self.consume_char();
                            tokens.push(Token {
                                token_type: TokenType::MULT_ASSIGN,
                                value: None,
                    datatype:None,
                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::MULT,
                                value: None,
                    datatype:None,
                            });
                        }
                    },
                    '/' => {
                        if self.peek_char_offset(1) == Some('=') {

                            self.consume_char();
                            tokens.push(Token {
                                token_type: TokenType::DIV_ASSIGN,
                                value: None,
                    datatype:None,
                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::DIV,
                                value: None,
                    datatype:None,
                            });
                        }
                    },                
                    '%' => tokens.push(Token {
                        token_type: TokenType::MOD,
                        value: None,
                    datatype:None,
                    }),
                    ';' => tokens.push(Token {
                        token_type: TokenType::SEMICOLON,
                        value: None,
                    datatype:None,
                    }),
                    '(' => tokens.push(Token {
                        token_type: TokenType::OPEN_PAREN,
                        value: None,
                    datatype:None,
                    }),

                    ')' => tokens.push(Token {
                        token_type: TokenType::CLOSE_PAREN,
                        value: None,
                    datatype:None,
                    }),

                    '{' => tokens.push(Token {
                        token_type: TokenType::OPEN_CURLY,
                        value: None,
                    datatype:None,
                    }),

                    '}' => tokens.push(Token {
                        token_type: TokenType::CLOSE_CURLY,
                        value: None,
                    datatype:None,
                    }),
                    '[' => tokens.push(Token {
                        token_type: TokenType::OPEN_SQUARE,
                        value: None,
                    datatype:None,
                    }),

                    ']' => tokens.push(Token {
                        token_type: TokenType::CLOSE_SQUARE,
                        value: None,
                    datatype:None,
                    }),
                   

                    _ => {} // Handle other characters if needed
                }
                self.consume_char();
            }
        }

        // Check if there's any remaining content in the buffer
        if !buf.is_empty() {
            tokens.push(Token::process_word(buf));
        }
        tokens
    }

    fn peek_char(&self) -> Option<char> {
        self.peek_char_offset(0)
    }

    fn peek_char_offset(&self, offset: usize) -> Option<char> {
        self.input.chars().nth(self.char_index + offset)
    }

    fn consume_char(&mut self) -> Option<char> {
        let ch = self.peek_char();
        if ch.is_some() {
            self.char_index += 1;
        }
        ch
    }
}


pub fn is_bin_op(token_type:TokenType) -> bool {
    match token_type {
        TokenType::ADD | TokenType::MULT | TokenType::SUB | TokenType::DIV | TokenType::MOD => return true,
        _ => false
    }
} 

pub fn is_bool_op(token_type:TokenType) -> bool {
    match token_type {
        TokenType::AND | TokenType::OR | TokenType::NOT | TokenType::EQ | TokenType::NEQ  => return true,
        _ => false
    }
}  


pub fn get_bin_precedence_level(token_type:TokenType) -> Option<usize>{
    match token_type {
        TokenType::ADD | TokenType::SUB => return Some(0), // Lowest
        TokenType::MULT | TokenType::DIV | TokenType::MOD => return Some(1),
        _ => return None
    }
}
