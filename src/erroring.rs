use crate::*;

use std::fmt;


pub trait CompilingError {
     fn error_and_exit(&self) {}
}



#[derive(Debug)]
pub enum GenerationError {
    UndeclaredIdentifier {ident:Token},
    SameIdentifiers {ident:Token},
}

#[derive(Debug)]
pub enum ParsingError {
    ExpectedTokenNotFound{expected_token:String},
    UnexpectedTokenFound {unexpected_token:String},
    FoundInsteadOf{expected_token:String,unexpected_token:String},
    InvalidExpr,
    ExpectedExpr,
    InvalidOperator{op:String},
    InvalidIdentifier {ident:String},
    UndeclaredIdentifier {ident:String},
    IncorrectType{ident:String,expected_type:DataType,got_type:DataType},
    IncorrectTypeExpr{expected_type:DataType,got_type:DataType},
    ExpectedStatementNotFound{expected_statement:String},
    Custom(String)
}

impl ParsingError {
    pub fn error_and_exit(&self) {
        println!("Unable to compile program:Parsing Error\n\t{}",self);
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
            ParsingError::ExpectedTokenNotFound { expected_token } => {
                write!(f, "Expected token not found: {:?}", expected_token)
            }
            ParsingError::UnexpectedTokenFound { unexpected_token } => {
                write!(f, "Unexpected token found: {:?}", unexpected_token)
            }
            ParsingError::FoundInsteadOf {
                expected_token,
                unexpected_token,
            } => write!(
                f,
                "Found {:?} instead of {:?}",
                unexpected_token, expected_token
            ),
            ParsingError::InvalidExpr => write!(f, "Invalid expression"),
            ParsingError::ExpectedExpr => write!(f, "Expected expression"),
            ParsingError::InvalidOperator { op } => {
                write!(f, "Invalid operator: {:?}", op)
            }
            ParsingError::InvalidIdentifier { ident } => {
                write!(f, "Invalid identifier: {:?}", ident)
            }
            ParsingError::UndeclaredIdentifier { ident } => {
                write!(f, "Undeclared identifier: {:?}", ident)
            }
            ParsingError::IncorrectType{ident,expected_type,got_type} => {
                write!(f,"Cannot assign {} which is of type {:?} to {:?}",ident,expected_type,got_type)
            }
            ParsingError::IncorrectTypeExpr{expected_type,got_type} => {
                write!(f,"Expression is of type:{:?}, but the value of the expressions is of type {:?}",expected_type,got_type)
            },
            ParsingError::ExpectedStatementNotFound{expected_statement} => {
                write!(f,"Expected {} statement.",expected_statement)
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
                write!(f, "Undeclared identifier: {:?}", ident.value.clone().unwrap())
            }
            GenerationError::SameIdentifiers { ident } => {
                write!(f, "Same identifiers found: {:?}", ident.value.clone().unwrap())
            }
        }
    }
}
