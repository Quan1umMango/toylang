use crate::erroring::TokenizationError;

use std::fmt;
pub type TokenLocation = (usize,usize);

#[derive(Debug, PartialEq, Clone,Hash,Eq)]
pub enum DataType {
    Int32,
    Bool,
    Infer, // Type has not been assigned yet, the parser will infer it later on
    Void,
    Array(Box<DataType>,usize),
    Char,
    Pointer { 
        ty: Box<DataType>,
    },
    Slice {
        ty:Box<DataType>
    }
}

impl DataType {
    pub fn size(&self) -> usize {
        use DataType::*;
        match self {
            Int32 | Bool | Char | Pointer{..} => 1,
            Void => 0,
            Slice{ty:_} => 1,
            Array(dt,len) => dt.size() * len,
            Infer => panic!("Couldn't get the type of Infer.")
        }
    }
}
impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use DataType::*;
        match self {
            Int32 => write!(f,"int32"),
            Void => write!(f,"void"),
            Slice{ty} => write!(f,"&[{}]",ty),
            Pointer{ty} => write!(f,"&{}",ty),
            Char => write!(f,"char"),
            Bool => write!(f,"bool"),
            Array(dt,len) => write!(f,"[{},{}]",dt,len),
            Infer => write!(f,"infer (you should not be seeing this.)")

        } 
    }
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
    CHAR_LIT,
    STR_LIT,

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
    TYPE_CHAR,
    FN,
    RETURN,
    COLON,
    AMPERSAND,

    BUILTIN, // @

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
            "char" => {
                return Token {
                    token_type: TokenType::TYPE_CHAR,
                    token_location: loc,
                    value:None,
                    datatype:None,
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
            let is_builtin =  ch == '@';

            let ch = if is_builtin {self.consume_char(&mut char_count); self.peek_char().unwrap()} else {ch};
            if ch.is_alphabetic() || ch =='_' {
                buf.push(ch);
                self.consume_char(&mut char_count);

                while let Some(next_ch) = self.peek_char() {
                    if next_ch.is_alphanumeric() || next_ch == '_' {
                        buf.push(self.consume_char(&mut char_count).unwrap());
                    } else {
                        break;
                    }
                }
                if is_builtin {
                    tokens.push(Token {
                        token_type:TokenType::BUILTIN,
                        value: Some(buf.clone()),
                        datatype:None,
                        token_location: (line_count,char_count-1-buf.len())
                    });
                } else {
                    tokens.push(Token::process_word(buf.clone(),(line_count,char_count-1-buf.len())));
                }
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
            }  else {
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
                        } else if self.peek_char_offset(1) == Some('/') {
                            self.consume_char(&mut char_count);
                            while let Some(s) = self.peek_char() {
                                if s == '\n' { break; }
                                self.consume_char(&mut char_count);
                            }
                            line_count +=1;

                        }

                        else {
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
                    '\'' => {
                        self.consume_char(&mut char_count);
                        let c = {
                            if self.peek_char().is_some() {
                                if self.peek_char().unwrap() == '\'' {
                                    TokenizationError::Custom(format!("Empty character found at: {}:{}",line_count,char_count-1-buf.len()).as_str()).error_and_exit();
                                }
                                self.consume_char(&mut char_count)
                            }else {

                                TokenizationError::Custom(format!("Expected character, found nothing at: {}:{}",line_count,char_count-1-buf.len()).as_str()).error_and_exit();
                                None // Rust compiler 
                            }
                        };
                        if self.try_consume(&mut char_count,'\'').is_none() {
                            TokenizationError::Custom(format!("Expected closing charater, found {:?} at: {}:{}",self.peek_char().unwrap_or(' '),line_count,char_count-1-buf.len()).as_str()).error_and_exit();
                        }
                        tokens.push(Token {
                            token_type: TokenType::STR_LIT,
                            value: Some(c.unwrap().to_string()),
                            datatype:Some(DataType::Char),
                            token_location:(line_count,char_count-1-buf.len()),
                        });
                        self.char_index -=1; // no clue why i did this,
                    }
                    '&' => {
                        tokens.push(Token {
                            token_type: TokenType::AMPERSAND,
                            value: None,
                            datatype:None,
                            token_location:(line_count,char_count-1-buf.len()),
                        });
                    } 
                    '\"' => {
                        self.consume_char(&mut char_count);
                        let string ={ 
                            let mut s = "".to_string();
                            loop {
                                if self.peek_char().is_some() {
                                    if self.peek_char().unwrap() == '\"' {
                                        break Some(s);
                                    }
                                    s.push(self.consume_char(&mut char_count).unwrap());
                                }else {
                                    TokenizationError::Custom(format!("Expected character, found nothing at: {}:{}",line_count,char_count-1-buf.len()).as_str()).error_and_exit();
                                    break None // Rust compiler tings
                                }

                            }};
                        if self.try_consume(&mut char_count,'\"').is_none() {
                            TokenizationError::Custom(format!("Expected closing charater, found {:?} at: {}:{}",self.peek_char().unwrap_or(' '),line_count,char_count-1-buf.len()).as_str()).error_and_exit();
                        }
                        let len = string.clone().unwrap().len();
                        tokens.push(Token {
                            token_type: TokenType::STR_LIT,
                            value: Some(string.unwrap().to_string()),
                            datatype:Some(DataType::Array(Box::new(DataType::Char),len)),
                            token_location:(line_count,char_count-1-buf.len()),
                        });
                        self.char_index -=1;
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

    fn try_consume(&mut self,char_count:&mut usize,c: char) -> Option<char> {
        let ch = self.peek_char();
        if ch.is_some() && ch.unwrap() == c{
            *char_count += 1;
            self.char_index += 1;
            return ch 
        }else {
            None
        }
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
