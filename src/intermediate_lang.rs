use std::fmt;
use crate::GenerationError;

pub enum IMStatementType {

   Exit 
}

pub enum IMExpr {

}

impl fmt::Display for IMStatementType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>,expr:Option<IMExpr>) -> fmt::Result {
        match self {
            IMStatementType::Exit => {
                if expr.is_none() {
                    return GenerationError::Custom("Could not find expression for exit statement".to_string());
                }
                let out = expr.unwrap().
                write!(,f);
            }
        }
    }
}

pub struct IMStatement {
    expr:Option<IMExpr>,
    ty:IMStatementType,
}

impl fmt::Display for IMStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      return self.ty.to_string(self.expr); 
    }
}


pub struct IMProgram {
    
}

impl IMProgram {
    pub fn new() -> Self {
        Self
    }
}
