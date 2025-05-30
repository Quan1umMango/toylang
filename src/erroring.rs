use crate::*;

use std::fmt;


pub trait CompilingError {
     fn error_and_exit(&self) {}
}

#[derive(Debug)]
pub enum GenerationError {
    UndeclaredIdentifier {ident:Token},
    SameIdentifiers {ident:Token},
    Custom(String),
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum ParsingError {
    ExpectedTokenNotFound{expected_token:String,loc:TokenLocation},
    UnexpectedTokenFound {unexpected_token:Token},
    FoundInsteadOf{expected_token:Token,unexpected_token:Token,loc:TokenLocation},
    InvalidExpr{loc: TokenLocation},
    ExpectedExpr{loc :TokenLocation},
    InvalidOperator{op:String,loc:TokenLocation},
    InvalidIdentifier {ident:Token},
    UndeclaredIdentifier {ident:Token},
    IncorrectType{ident:Token,expected_type:DataType,got_type:DataType},
    IncorrectTypeExpr{expected_type:DataType,got_type:DataType,loc:TokenLocation},
    ExpectedStatementNotFound{expected_statement:String,loc:TokenLocation},
    Custom(String)
}

impl ParsingError {
    pub fn error_and_exit(&self) {
        println!("Unable to compile program: Parsing Error\n\t{}",self);
        std::process::exit(1);
    }
}

impl GenerationError {
    pub fn error_and_exit(&self) {
      println!("Unable to compile program:Generation Error\n\t{}",self);
        std::process::exit(1);

    }
}

impl fmt::Display for ParsingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsingError::ExpectedTokenNotFound { expected_token, loc } => {
                write!(f, "Expected token not found: {:?} at {}:{}", expected_token,loc.0,loc.1)
            }
            ParsingError::UnexpectedTokenFound { unexpected_token } => {
                let loc = unexpected_token.token_location;
                write!(f, "Unexpected token found: {:?} at {}:{}", unexpected_token.token_type,loc.0,loc.1)
            }
            ParsingError::FoundInsteadOf {
                expected_token,
                unexpected_token,
                loc
            } => write!(
                f,
                "Found {:?} instead of {:?} at {}:{}",
                unexpected_token, expected_token,loc.0,loc.1
            ),
            ParsingError::InvalidExpr {loc } => write!(f, "Invalid expression at {}:{}",loc.0,loc.1),
            ParsingError::ExpectedExpr { loc } => write!(f, "Expected expression at {}:{}",loc.0,loc.1),
            ParsingError::InvalidOperator { op, loc } => {
                write!(f, "Invalid operator: {:?} at {}:{}", op,loc.0,loc.1)
            }
            ParsingError::InvalidIdentifier { ident } => {
                let loc = ident.token_location;
                write!(f, "Invalid identifier: {:?} at {}:{}", ident.value.clone().unwrap(),loc.0,loc.1)
            }
            ParsingError::UndeclaredIdentifier { ident } => {
                let loc = ident.token_location;
                write!(f, "Undeclared identifier: {:?} at {}:{}", ident.value.clone().unwrap(),loc.0,loc.1)
            }
            ParsingError::IncorrectType{ident,expected_type,got_type} => {
                let loc = ident.token_location;
                write!(f,"Cannot assign {} which is of type {} to {}\n\t at {}:{}",ident.value.clone().unwrap(),expected_type,got_type,loc.0,loc.1)
            }
            ParsingError::IncorrectTypeExpr{expected_type,got_type, loc} => {
                write!(f,"Expected type:{}, but got type {}\n\tat {}:{}",expected_type,got_type,loc.0,loc.1)
            },
            ParsingError::ExpectedStatementNotFound{expected_statement, loc} => {
                write!(f,"Expected {} statement  at {}:{}",expected_statement,loc.0,loc.1)
            }
            ParsingError::Custom(err_msg) => {
                write!(f,"{}",err_msg)
            }


        }
    }
}


impl fmt::Display for GenerationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenerationError::UndeclaredIdentifier { ident } => {
                write!(f, "Undeclared identifier: {:?} at line {}:{}", ident.value.clone().unwrap(),ident.token_location.0,ident.token_location.1)
            }
            GenerationError::SameIdentifiers { ident } => {
                write!(f, "Same identifiers found: {:?} at line {}:{}", ident.value.clone().unwrap(),ident.token_location.0,ident.token_location.1)
            }
            GenerationError::Custom(err_msg) => {
                write!(f,"{}",err_msg)
            }
        }
    }
}

pub enum TokenizationError<'a> {
    Custom(&'a str),
}

impl fmt::Display for TokenizationError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Custom(a) => {
               write!(f,"{}",a) 
            }
        }
    }
}
impl TokenizationError<'_>{
    pub fn error_and_exit(&self) {
      println!("Unable to compile program:Tokenization Error\n\t{}",self);
        std::process::exit(1);

    }
}
