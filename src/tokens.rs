pub type TokenLocation = (usize,usize);

#[derive(Debug, PartialEq, Copy, Clone,Hash,Eq)]
pub enum DataType {
    Int32,
    Bool,
    Infer, // Type has not been assigned yet, the parser will infer it later on
    Void,
}
 
#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Copy, Clone,Hash,Eq)]
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
    COMMA,
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

    LESS_THAN,
    GREATER_THAN,
    LESS_THAN_EQUAL_TO,
    GREATER_THAN_EQUAL_TO,

    AND,
    OR,
    NOT,

    WHILE,
    TYPE_INT32,
    TYPE_BOOL,

    FN,
    RETURN,
    COLON,
}

#[derive(Debug, Clone,Hash,Eq,PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub value: Option<String>,
    pub datatype: Option<DataType>,
    pub token_location: TokenLocation,  // (line number, col number)
}

impl Token {
    pub fn process_word(word: String,loc:TokenLocation) -> Token {
        match word.as_str() {
            "exit" => {
                return Token {
                    token_type: TokenType::EXIT,
                    value: None,
                    datatype:None,
                    token_location:loc,
                }
            }
            "let" => {
                return Token {
                    token_type: TokenType::LET,
                    value: None,
                    datatype:None,
                    token_location:loc,
                }
            }
            "if" => {
                return Token {
                    token_type:TokenType::IF,
                    value:None,
                    datatype:None,
                    token_location:loc,
                }
            },
            "else" => {
                return Token {
                    token_type: TokenType::ELSE,
                    value:None,
                    datatype:None,
                    token_location:loc,
                }
            }
            "true" => {
                return Token {
                    token_type: TokenType::TRUE,
                    value:None,
                    datatype:Some(DataType::Bool),
                    token_location:loc,
                }
            }
            "false" => {
                return Token {
                    token_type: TokenType::FALSE,
                    value:None,
                    datatype:Some(DataType::Bool),
                    token_location:loc,
                }
            }
            "and" => {
                return Token {
                    token_type: TokenType::AND,
                    value:None,
                    datatype:None,
                    token_location:loc,
                }
            }
            "or" => {
                return Token {
                    token_type: TokenType::OR,
                    value:None,
                    datatype:None,
                        token_location:loc,
                }
            },
            "while" => {
                return Token {
                    token_type: TokenType::WHILE,
                    value:None,
                    datatype:None,
                    token_location:loc,
                }
            }

            "int32" => {
                return Token {
                    token_type: TokenType::TYPE_INT32,
                    value:None,
                    token_location:loc,
                    datatype: None,
                }
            }

            "bool" => {
                return Token {
                    token_type: TokenType::TYPE_BOOL,
                    token_location:loc,
                    value:None,
                    datatype: None,
                }
            }
            "fn" => {
                return Token {
                    token_type: TokenType::FN,
                    token_location:loc,
                    value:None,
                    datatype:None,
                }
            }

            "return" => {
                return Token {
                    token_type: TokenType::RETURN,
                    value:None,
                    datatype:None,
                    token_location:loc,
                }
            }


            _ => {
                return Token {
                    token_type: TokenType::IDENT,
                    value: Some(word),
                    datatype:Some(DataType::Infer),
                    token_location:loc,
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
        let mut line_count = 1; 
        let mut char_count = 1;
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(ch) = self.peek_char() {

            if ch.is_alphabetic() || ch =='_' {
                buf.push(ch);
                self.consume_char(&mut char_count);

                while let Some(next_ch) = self.peek_char() {
                    if next_ch.is_alphanumeric() {
                        buf.push(self.consume_char(&mut char_count).unwrap());
                    } else {
                        break;
                    }
                }

                tokens.push(Token::process_word(buf.clone(),(line_count,char_count-1-buf.len())));
                buf.clear();
            } else if ch.is_numeric() {
                buf.push(ch);
                self.consume_char(&mut char_count);
                while let Some(next_ch) = self.peek_char() {
                    if next_ch.is_numeric() {
                        buf.push(self.consume_char(&mut char_count).unwrap());
                    } else {
                        break;
                    }
                }

                tokens.push(Token {
                    token_type: TokenType::NUM,
                    value: Some(buf.clone()), 
                    datatype: Some(DataType::Int32),
                   token_location:(line_count,char_count-1-buf.len()), 
                });
                buf.clear();
            } else if ch.is_whitespace() {
                if ch == '\n' {

                
                    char_count= 1;
                line_count += 1;
                }

                self.consume_char(&mut char_count); // Skip whitespace
                if !buf.is_empty() {
                    tokens.push(Token::process_word(buf.clone(),(line_count,char_count-1-buf.len())));
                    buf.clear();
                }
            } else {
                match ch {
                    '=' => {
                        if self.peek_char_offset(1) == Some('=') {
                            self.consume_char(&mut char_count);
                            tokens.push(Token {
                                token_type: TokenType::EQ,
                                value: None,
                                datatype:None,
                                token_location:(line_count,char_count-1-buf.len()),
                        
                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::ASSIGN,
                                value: None,
                                datatype:None,
                                token_location:(line_count,char_count-1-buf.len()),
                            });
                        }
                    }
                    '!' => {
                        if self.peek_char_offset(1) == Some('=') {
                            self.consume_char(&mut char_count);
                            tokens.push(Token {
                                token_type: TokenType::NEQ,
                                value:None,
                                datatype:None,
                                token_location:(line_count,char_count-1-buf.len()),
                            });
                        }else {
                            tokens.push(Token {
                                token_type:TokenType::NOT,
                                value:None,
                                datatype:None,
                                token_location:(line_count,char_count-1-buf.len()),
                            });
                        }
                    }
                    '+' => {
                        if self.peek_char_offset(1) == Some('=') {
                            self.consume_char(&mut char_count);
                            tokens.push(Token {
                                token_type: TokenType::ADD_ASSIGN,
                                value: None,
                    datatype:None,
                    token_location:(line_count,char_count-1-buf.len()),
                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::ADD,
                                value: None,
                    datatype:None,
                    token_location:(line_count,char_count-1-buf.len()),
                            });
                        }
                    },
                    '-' => {
                        if self.peek_char_offset(1) == Some('=') {
                            self.consume_char(&mut char_count);
                            tokens.push(Token {
                                token_type: TokenType::SUB_ASSIGN,
                                value: None,
                    datatype:None,
                    token_location:(line_count,char_count-1-buf.len()),
                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::SUB,
                                value: None,
                    datatype:None,
                    token_location:(line_count,char_count-1-buf.len()),
                            });
                        }
                    },
                    '*' => {
                        if self.peek_char_offset(1) == Some('=') {

                            self.consume_char(&mut char_count);
                            tokens.push(Token {
                                token_type: TokenType::MULT_ASSIGN,
                                value: None,
                    datatype:None,
                    token_location:(line_count,char_count-1-buf.len()),
                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::MULT,
                                value: None,
                    datatype:None,
                    token_location:(line_count,char_count-1-buf.len()),
                            });
                        }
                    },
                    '/' => {
                        if self.peek_char_offset(1) == Some('=') {

                            self.consume_char(&mut char_count);
                            tokens.push(Token {
                                token_type: TokenType::DIV_ASSIGN,
                                value: None,
                    datatype:None,
                    token_location:(line_count,char_count-1-buf.len()),
                            });
                        } else {
                            tokens.push(Token {
                                token_type: TokenType::DIV,
                                value: None,
                    datatype:None,
                    token_location:(line_count,char_count-1-buf.len()),
                            });
                        }
                    },                
                    '%' => tokens.push(Token {
                        token_type: TokenType::MOD,
                        value: None,
                    datatype:None,
                    token_location:(line_count,char_count-1-buf.len()),
                    }),
                    ';' => tokens.push(Token {
                        token_type: TokenType::SEMICOLON,
                        value: None,
                    datatype:None,
                    token_location:(line_count,char_count-1-buf.len()),
                    }),
                    '(' => tokens.push(Token {
                        token_type: TokenType::OPEN_PAREN,
                        value: None,
                        datatype:None,
                        token_location:(line_count,char_count-1-buf.len()),
                    }),

                    ')' => tokens.push(Token {
                        token_type: TokenType::CLOSE_PAREN,
                        value: None,
                        datatype:None,
                        token_location:(line_count,char_count-1-buf.len()),
                    }),

                    '{' => tokens.push(Token {
                        token_type: TokenType::OPEN_CURLY,
                        value: None,
                        datatype:None,
                        token_location:(line_count,char_count-1-buf.len()),
                    }),

                    '}' => tokens.push(Token {
                        token_type: TokenType::CLOSE_CURLY,
                        value: None,
                        datatype:None,
                        token_location:(line_count,char_count-1-buf.len()),
                    }),
                    '[' => tokens.push(Token {
                        token_type: TokenType::OPEN_SQUARE,
                        value: None,
                        datatype:None,
                        token_location:(line_count,char_count-1-buf.len()),
                    }),

                    ']' => tokens.push(Token {
                        token_type: TokenType::CLOSE_SQUARE,
                        value: None,
                        datatype:None,
                        token_location:(line_count,char_count-1-buf.len()),
                    }),
                    '>' => {
                        if self.peek_char_offset(1) == Some('=') {
                            self.consume_char(&mut char_count);
                            tokens.push(Token {
                                token_type:TokenType::GREATER_THAN_EQUAL_TO,
                                value:None,
                                datatype:None,
                                token_location:(line_count,char_count-1-buf.len()),
                            });
                        }else { 
                            tokens.push(Token {
                                token_type:TokenType::GREATER_THAN,
                                value:None,
                                datatype:None,
                                token_location:(line_count,char_count-1-buf.len()),
                            });
                        }
                    } 
                    '<' => {
                        if self.peek_char_offset(1) == Some('=') {
                            self.consume_char(&mut char_count);
                            tokens.push(Token {
                                token_type:TokenType::LESS_THAN_EQUAL_TO,
                                value:None,
                                datatype:None,
                                token_location:(line_count,char_count-1-buf.len()),
                            });
                        }else { 
                            tokens.push(Token {
                                token_type:TokenType::LESS_THAN,
                                value:None,
                                datatype:None,
                                token_location:(line_count,char_count-1-buf.len()),
                            });
                        }
                    }
                    ',' => {
                        tokens.push(Token {
                            token_type:TokenType::COMMA,
                            value:None,
                            datatype:None,
                            token_location:(line_count,char_count-1-buf.len()),
                        });
                    }

                    ':' => {
                        tokens.push(Token {
                            token_type:TokenType::COLON,
                            value:None,
                            datatype:None,
                            token_location:(line_count,char_count-1-buf.len()),
                        });
                    }

                    _ => {} // Handle other characters if needed
                }
                self.consume_char(&mut char_count);
            }
        }

        // Check if there's any remaining content in the buffer
        if !buf.is_empty() {
            tokens.push(Token::process_word(buf.clone(),(line_count,char_count-1-buf.len())));
        }
        tokens
    }

    fn peek_char(&self) -> Option<char> {
        self.peek_char_offset(0)
    }

    fn peek_char_offset(&self, offset: usize) -> Option<char> {
        self.input.chars().nth(self.char_index + offset)
    }

    fn consume_char(&mut self,char_count:&mut usize) -> Option<char> {
        let ch = self.peek_char();
        if ch.is_some() {
        *char_count += 1;
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
        TokenType::AND | TokenType::OR | TokenType::NOT | TokenType::EQ | TokenType::NEQ | TokenType::LESS_THAN | TokenType::LESS_THAN_EQUAL_TO | TokenType::GREATER_THAN | TokenType::GREATER_THAN_EQUAL_TO => return true,
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
