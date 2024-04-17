use crate::*;
use std::collections::HashMap;

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    vars: HashMap<String,DataType>
}


#[derive(Debug,Clone)]
pub enum NodeTerm {
    NodeTermIntLit{value:Token},
    NodeTermIdent{value:Token},
    NodeTermParen{value:Box<NodeExpr>},
    NodeTermBool{value:Token},
}

#[derive(Debug,Clone)]
pub enum BoolExpr {
    BoolExprAnd {
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>,
    },
    BoolExprOr {
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>,
    },
    BoolExprEqualTo {
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>,
    },
    BoolExprNotEqualTo {
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>
    },
    BoolExprLessThan {
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>
    },
    BoolExprGreaterThan {
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>
    },
    BoolExprLessThanOrEqualTo {
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>
    },
    BoolExprGreaterThanOrEqualTo {
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>
    },
}

#[derive(Debug,Clone)]
pub enum NodeExpr {   
    NodeExprTerm{value:NodeTerm},
    NodeExprBinExpr{value:BinExpr},
    NodeExprBoolExpr{value:BoolExpr}
}


#[derive(Debug,Clone)]
pub enum BinExpr {
    BinExprAdd{ 
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>
    },
    BinExprMult{
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>
    },
    BinExprSub{ 
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>
    },
    BinExprDiv{
        lhs:Box<NodeExpr>,
        rhs:Box<NodeExpr>
    },

}


#[derive(Debug,Clone)]
pub struct NodeWhileLoop {
    pub condition:NodeExpr,
    pub scope:NodeScope
}

#[derive(Debug,Clone)]
pub struct NodeElse {
    pub if_stmt:Option<Box<NodeStatement>>,
    pub scope:Option<NodeScope>
}

#[derive(Debug,Clone)]
pub struct NodeIf {
    pub expr:NodeExpr,
    pub scope:NodeScope,
    pub else_stmt: Option<Box<NodeElse>>
}

#[derive(Debug,Clone)]
pub struct NodeExit {
    pub expr: NodeExpr,
}

#[derive(Debug,Clone)]
pub struct NodeLet {
    pub ident: Token,
    pub expr: NodeExpr,
}


#[derive(Debug,Clone)]
pub struct NodeLoop {
    
}

#[derive(Debug,Clone)]
pub enum NodeReassign {
    Assign{ident:Token,expr:NodeExpr},
    Add{ident:Token,expr:NodeExpr},
    Sub{ident:Token,expr:NodeExpr},
    Mul{ident:Token,expr:NodeExpr},
    Div{ident:Token,expr:NodeExpr},
}

#[derive(Debug,Clone)]
pub struct NodeScope {
    pub stmts:Vec<NodeStatement>
}


#[derive(Debug,Clone)]
pub enum NodeStatement{
    NodeStatementExit{ value:NodeExit},
    NodeStatementLet{ value:NodeLet},
    NodeStatementScope{value:NodeScope},
    NodeStatementIf{value:NodeIf},
    NodeStatementReassign{value:NodeReassign},
    NodeStatementWhileLoop{value:NodeWhileLoop}
}

pub struct NodeProgram {
    pub stmts: Vec<NodeStatement>
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0, vars:HashMap::new() }
    }


    pub fn parse_term(&mut self) -> Option<NodeTerm> {
        if let Some(t) = self.peek_token() {
            if t.token_type == TokenType::NUM {
                let term = NodeTerm::NodeTermIntLit { value: self.consume_token().unwrap() };
                return Some(term);
            } else if t.token_type == TokenType::IDENT {
                if let Some(dt) = self.vars.get_mut(&t.value.clone().unwrap()) {
                    let new_tok = Token {
                        datatype: Some(*dt),
                        token_type: TokenType::IDENT,
                        value: t.value,
                    }; 
                    self.consume_token();
                    return Some(NodeTerm::NodeTermIdent{value:new_tok.clone()});
                } 
                self.vars.insert(t.value.unwrap().clone(),t.datatype.unwrap());
                let term = NodeTerm::NodeTermIdent { value: self.consume_token().unwrap() };
                return Some(term);
            }else if let Some(_open_paren) = self.try_consume(TokenType::OPEN_PAREN){
                if let Some(expr) =self. parse_expr(0,DataType::Infer) {
                    if let Some(_) = self.try_consume(TokenType::CLOSE_PAREN) {
                        let term = NodeTerm::NodeTermParen{value:Box::new(expr)};
                        return Some(term);
                    }else { 
                        ParsingError::ExpectedTokenNotFound{expected_token:")".to_string()}.error_and_exit(); 
                    }
                }else {
                    ParsingError::ExpectedExpr.error_and_exit();

                }

            }else if let Some(boolean) = self.try_consume(TokenType::TRUE) {
                let term = NodeTerm::NodeTermBool{value:boolean};
                return Some(term);
            }else if let Some(boolean) = self.try_consume(TokenType::FALSE) {
                let term = NodeTerm::NodeTermBool{value:boolean};
                return Some(term);
            }
        }
        None
    }


    pub fn check_type_expr(&mut self,ident:&mut Token,expr:NodeExpr) {
        let datatype = match expr.clone() {
            NodeExpr::NodeExprTerm{value:term} =>  {
                match term {
                    NodeTerm::NodeTermBool{..} => DataType::Bool,
                    NodeTerm::NodeTermIntLit{..} => DataType::Int32,
                    NodeTerm::NodeTermIdent{value:tok} => {
                        let dt =if tok.datatype.is_some() {
                            tok.datatype.unwrap()
                        }else {
                            DataType::Infer
                        };
                        dt
                    } 
                    _=> DataType::Infer,

                }
            }
            NodeExpr::NodeExprBoolExpr{value:_nodeterm} => {
                DataType::Bool 
            }  
            NodeExpr::NodeExprBinExpr{value:_tok} =>DataType::Int32,
        };

        if let Some(expected_dt) = self.vars.get(&ident.value.clone().unwrap()) {
            if *expected_dt != datatype && *expected_dt != DataType::Infer{
                ParsingError::IncorrectType{ident:ident.value.clone().unwrap(),expected_type:*expected_dt,got_type:datatype}.error_and_exit();
            }
        }else {
            ParsingError::UndeclaredIdentifier{ident:ident.value.clone().unwrap()}.error_and_exit();

        }
        if ident.datatype.is_some() && ident.datatype.unwrap() == DataType::Infer && datatype != DataType::Infer{
            ident.datatype = Some(datatype);
        }

    } 


    pub fn get_datatype_expr(&mut self, expr:&NodeExpr) -> Option<DataType> {
        let datatype = match expr.clone() {
            NodeExpr::NodeExprTerm{value:term} =>  {
                match term {
                    NodeTerm::NodeTermBool{..} => DataType::Bool,
                    NodeTerm::NodeTermIntLit{..} => DataType::Int32,
                    NodeTerm::NodeTermIdent{value:tok} => {
                        *if let Some(dt) = self.vars.get(&tok.value.clone().unwrap())  {
                            dt
                        }else {

                            ParsingError::InvalidIdentifier{ident:tok.value.clone().unwrap()}.error_and_exit();
                            return None;
                        }
                    }
                    NodeTerm::NodeTermParen { value } => {
                        return self.get_datatype_expr(&value);
                    }
                    _=> DataType::Infer,

                }
            }
            NodeExpr::NodeExprBoolExpr{value:_nodeterm} => {
                DataType::Bool 
            }  
            NodeExpr::NodeExprBinExpr{value:_tok} =>DataType::Int32,
        };
        return Some(datatype);

    }

    // https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
    pub fn parse_expr(&mut self, min_prec: usize,expr_datatype:DataType) -> Option<NodeExpr> {
        if let Some(lhs_term) = self.parse_term() {

            let mut lhs_expr = NodeExpr::NodeExprTerm { value: lhs_term };
            let expr_dt = self.get_datatype_expr(&lhs_expr);
            if expr_datatype != DataType::Infer && expr_dt.is_some() && expr_dt.unwrap() != expr_datatype {
       
                    ParsingError::IncorrectTypeExpr{expected_type:expr_datatype,got_type:expr_dt.unwrap()}.error_and_exit();
            }
            while let Some(cur) = self.peek_token() {

                if is_bool_op(cur.token_type) {
                    if let Some(bool_op) = self.consume_token() {
                        if let Some(rhs) = self.parse_expr(0,expr_dt.unwrap()) {
                            match bool_op.token_type {
                                TokenType::AND => {
                                    let bool_expr_and =
                                    BoolExpr::BoolExprAnd { lhs: Box::new(lhs_expr), rhs: Box::new(rhs) };
                                    lhs_expr = NodeExpr::NodeExprBoolExpr { value: bool_expr_and };
                                }
                                TokenType::OR => {
                                    let bool_expr_or =
                                    BoolExpr::BoolExprOr { lhs: Box::new(lhs_expr), rhs: Box::new(rhs) };
                                    lhs_expr = NodeExpr::NodeExprBoolExpr { value: bool_expr_or };
                                }
                                TokenType::EQ => {
                                    let bool_expr_eq = BoolExpr::BoolExprEqualTo {lhs:Box::new(lhs_expr),rhs:Box::new(rhs)};
                                    lhs_expr=   NodeExpr::NodeExprBoolExpr{value:bool_expr_eq};
                                }
                                TokenType::NEQ => {
                                    let bool_expr_neq = BoolExpr::BoolExprNotEqualTo {lhs:Box::new(lhs_expr),rhs:Box::new(rhs)};
                                    lhs_expr=   NodeExpr::NodeExprBoolExpr{value:bool_expr_neq};
                                }
                                TokenType::GREATER_THAN => {
                                    let bool_expr = BoolExpr::BoolExprGreaterThan{lhs:Box::new(lhs_expr),rhs:Box::new(rhs)};
                                    lhs_expr = NodeExpr::NodeExprBoolExpr{value:bool_expr};
                                }
                                TokenType::GREATER_THAN_EQUAL_TO => {

                                    let bool_expr = BoolExpr::BoolExprGreaterThanOrEqualTo{lhs:Box::new(lhs_expr),rhs:Box::new(rhs)};
                                    lhs_expr = NodeExpr::NodeExprBoolExpr{value:bool_expr};
                                }
                                TokenType::LESS_THAN => {
 
                                    let bool_expr = BoolExpr::BoolExprLessThan{lhs:Box::new(lhs_expr),rhs:Box::new(rhs)};
                                    lhs_expr = NodeExpr::NodeExprBoolExpr{value:bool_expr};                                   
                                }
                                TokenType::LESS_THAN_EQUAL_TO => {

                                    let bool_expr = BoolExpr::BoolExprLessThanOrEqualTo{lhs:Box::new(lhs_expr),rhs:Box::new(rhs)};
                                    lhs_expr = NodeExpr::NodeExprBoolExpr{value:bool_expr};
                                }
                                _ => {
                                    println!("Invalid Boolean Operator: {:?}. Possibly unimplemented.", bool_op);
                                    std::process::exit(1);
                                }
                            }
                        } else {
                            println!("Unable to parse rhs");
                            std::process::exit(1);
                        }
                    }
                } else if !is_bin_op(cur.token_type)
                || get_bin_precedence_level(cur.token_type).map_or(true, |cur_prec| cur_prec < min_prec)
                {
                    break;
                } else {
                    let next_min_prec = get_bin_precedence_level(cur.token_type).unwrap() + 1;
                    if let Some(op) = self.consume_token() {
                        if let Some(rhs) = self.parse_expr(next_min_prec,expr_dt.unwrap()) {
                            match op.token_type {
                                TokenType::ADD => {
                                    let bin_expr_add =
                                    BinExpr::BinExprAdd { lhs: Box::new(lhs_expr), rhs: Box::new(rhs) };
                                    lhs_expr = NodeExpr::NodeExprBinExpr { value: bin_expr_add };
                                }
                                TokenType::MULT => {
                                    let bin_expr_mult =
                                    BinExpr::BinExprMult { lhs: Box::new(lhs_expr), rhs: Box::new(rhs) };
                                    lhs_expr = NodeExpr::NodeExprBinExpr { value: bin_expr_mult };
                                }
                                TokenType::SUB => {
                                    let bin_expr_sub =
                                    BinExpr::BinExprSub { lhs: Box::new(lhs_expr), rhs: Box::new(rhs) };
                                    lhs_expr = NodeExpr::NodeExprBinExpr { value: bin_expr_sub };
                                }
                                TokenType::DIV => {
                                    let bin_expr_div =
                                    BinExpr::BinExprDiv { lhs: Box::new(lhs_expr), rhs: Box::new(rhs) };
                                    lhs_expr = NodeExpr::NodeExprBinExpr { value: bin_expr_div };
                                }
                                _ => {}
                            }
                        } else {
                            println!("Unable to parse rhs");
                            std::process::exit(1);
                        }
                    } else {
                        println!("Invalid Operator");
                        std::process::exit(1);
                    }
                }
            }

            Some(lhs_expr)
        } else {
            None
        }
    }


    pub fn parse_program(&mut self) -> Option<NodeProgram> {
        let mut node_prog = NodeProgram { stmts: Vec::new() };

        while self.peek_token().is_some() {
            if let Some(stmt) = self.parse_stmt() {
                node_prog.stmts.push(stmt);
            } else {
                println!("Invalid Statement");
                std::process::exit(1);
            }
        }
        Some(node_prog)
    }

    pub fn parse_scope(&mut self) -> Option<NodeScope> {
        if let Some(_) =  self.try_consume(TokenType::OPEN_CURLY) {
            let stmts = {
                let mut v = Vec::new();
                while let Some(stmt) = self.parse_stmt() {
                    v.push(stmt);
                }

                // If scope is empty
                if v.len() == 0 {
                    if !self.try_consume(TokenType::CLOSE_CURLY).is_some(){
                        ParsingError::ExpectedTokenNotFound{expected_token:"}".to_string()}.error_and_exit(); 
                    }
                    return None;
                }
                v
            };
            if !self.try_consume(TokenType::CLOSE_CURLY).is_some(){
                ParsingError::ExpectedTokenNotFound{expected_token:"}".to_string()}.error_and_exit(); 

            }
            let scope = NodeScope {
                stmts,
            };
            return Some(scope);
        }
        return None;
    }

    pub fn parse_exit_stmt(&mut self) -> Option<NodeStatement> {
        if self.try_consume(TokenType::EXIT).is_some() && self.try_consume(TokenType::OPEN_PAREN).is_some() {
            let mut stmt_exit = None;

            if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                stmt_exit = Some(NodeStatement::NodeStatementExit { value: NodeExit { expr } });
            } else {
                ParsingError::InvalidExpr.error_and_exit();
            }

            // Check if it is boolean or int.
            /*
            match expr {

            }
            */

            if self.try_consume(TokenType::CLOSE_PAREN).is_none() {
                ParsingError::ExpectedTokenNotFound { expected_token: ")".to_string() }.error_and_exit();
            }

            if self.try_consume(TokenType::SEMICOLON).is_none() {
                ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string() }.error_and_exit();
            }

            return stmt_exit;
        } 
        return None;
    }

    pub fn parse_let_stmt(&mut self) -> Option<NodeStatement> {
        if self.peek_token().is_some()
        && self.peek_token().unwrap().token_type == TokenType::LET
        && self.peek_token_offset(1).is_some()
        && self.peek_token_offset(1).unwrap().token_type == TokenType::IDENT
        && self.peek_token_offset(2).is_some()
        && self.peek_token_offset(2).unwrap().token_type == TokenType::ASSIGN
        {
            self.consume_token();
            let mut ident = self.consume_token().unwrap();
            if self.vars.get(&ident.value.clone().unwrap()).is_some() {
                println!("Cannot have two identifiers with the same name {:?}", ident.value.unwrap());
                std::process::exit(0);
            }
            self.vars.insert(ident.value.clone().unwrap(), DataType::Infer);
            let mut stmt_let = None;
            self.consume_token(); // Consume =

            if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                // Set Type for identifier
                match expr.clone() {
                    NodeExpr::NodeExprTerm { value: term } => match term {
                        NodeTerm::NodeTermIdent { value: token } => ident.datatype = token.datatype,
                        NodeTerm::NodeTermBool { .. } => ident.datatype = Some(DataType::Bool),
                        NodeTerm::NodeTermIntLit { .. } => ident.datatype = Some(DataType::Int32),
                        _ => {}
                    },
                    NodeExpr::NodeExprBinExpr { .. } => ident.datatype = Some(DataType::Int32),
                    NodeExpr::NodeExprBoolExpr { .. } =>{println!("sa"); ident.datatype = Some(DataType::Bool);},
                    _ => unimplemented!("Unimplemented Type"),
                }
                if let Some(v) = self.vars.get_mut(&ident.value.clone().unwrap()) {
                    *v = ident.datatype.unwrap();
                }
                stmt_let = Some(NodeStatement::NodeStatementLet {
                    value: NodeLet { expr, ident },
                });
            } else {
                ParsingError::InvalidExpr.error_and_exit();
            }

            if self.try_consume(TokenType::SEMICOLON).is_none() {
                ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string() }.error_and_exit();
            }

            return stmt_let;
        }
        return None;
    }

  
pub fn parse_else(&mut self) -> Option<NodeElse> {
    if let Some(_else_keyword) = self.try_consume(TokenType::ELSE) {
        let if_stmt = if let Some(i) = self.parse_if_stmt() {
            match i {
                NodeStatement::NodeStatementIf { .. } => Some(Box::new(i)),
                _ =>{ ParsingError::ExpectedStatementNotFound {
                    expected_statement: "if statement".to_string(),
                }
                .error_and_exit();
                None // I have to do this to satisfy rust compiler
                }
            }
        } else {
            None
        };
        if if_stmt.is_some() {
                return Some(NodeElse{if_stmt,scope:None});
        }
        
        if let Some(scope) = self.parse_scope() {
            return Some(NodeElse { if_stmt, scope:Some(scope) });
        } else {
            ParsingError::Custom("Expected scope after else keyword, found none".to_string())
                .error_and_exit();
        }
    }
    None
}

    pub fn parse_if_stmt(&mut self) -> Option<NodeStatement> {
        if let Some(_if_tok) = self.try_consume(TokenType::IF) {
            if let Some(_open_paren) = self.try_consume(TokenType::OPEN_PAREN) {
                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                    if let Some(_close_paren) = self.try_consume(TokenType::CLOSE_PAREN) {
                        if let Some(scope) = self.parse_stmt() {
                            let scope = match scope {
                                NodeStatement::NodeStatementScope { value } => value,
                                _ => {
                                    println!("Failed parsing scope.");
                                    std::process::exit(1);
                                }
                            };
                            let else_stmt = {
                                if let Some(el) = self.parse_else() {
                                    Some(Box::new(el))
                                }else {
                                None
                                }
                            };
                            
                            let node_if = NodeIf { expr, scope, else_stmt:else_stmt };
                            let if_stmt = NodeStatement::NodeStatementIf { value: node_if };
                            return Some(if_stmt);
                        } else {
                            println!("Expected scope after if condition. It is either empty or does not exist");
                            std::process::exit(1);
                        }
                    } else {

                        ParsingError::ExpectedTokenNotFound{expected_token:")".to_string()}.error_and_exit(); 

                    }
                } else {
                    println!("Failed to parse expression");
                    std::process::exit(1);
                }
            } else {
                ParsingError::ExpectedTokenNotFound { expected_token: "(".to_string() }.error_and_exit();
            }

        }
        return None;
    }

    pub fn parse_reassign_stmt(&mut self) -> Option<NodeStatement> {
        if let Some(mut ident) = self.try_consume(TokenType::IDENT) {
            if let Some(_assign) = self.try_consume(TokenType::ASSIGN) {
                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                    self.check_type_expr(&mut ident, expr.clone());
                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string() }.error_and_exit();
                    }
                    let node_reassign = NodeReassign::Assign { expr, ident };
                    let reassign_stmt = NodeStatement::NodeStatementReassign { value: node_reassign };
                    return Some(reassign_stmt)
                } else {
                    ParsingError::InvalidExpr.error_and_exit();
                }
            } else if let Some(_add_assign) = self.try_consume(TokenType::ADD_ASSIGN) {
                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                    self.check_type_expr(&mut ident, expr.clone());

                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string() }.error_and_exit();
                    }
                    let node_reassign = NodeReassign::Add { expr, ident };
                    let reassign_stmt = NodeStatement::NodeStatementReassign { value: node_reassign };

                    return Some(reassign_stmt)

                } else {
                    ParsingError::InvalidExpr.error_and_exit();
                }
            } else if let Some(_sub_assign) = self.try_consume(TokenType::SUB_ASSIGN) {
                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                    self.check_type_expr(&mut ident, expr.clone());

                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string() }.error_and_exit();
                    }
                    let node_reassign = NodeReassign::Sub { expr, ident };
                    let reassign_stmt = NodeStatement::NodeStatementReassign { value: node_reassign };

                    return Some(reassign_stmt)

                } else {
                    ParsingError::InvalidExpr.error_and_exit();
                }
            } else if let Some(_mult_assign) = self.try_consume(TokenType::MULT_ASSIGN) {
                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                    self.check_type_expr(&mut ident, expr.clone());

                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string() }.error_and_exit();
                    }
                    let node_reassign = NodeReassign::Mul { expr, ident };
                    let reassign_stmt = NodeStatement::NodeStatementReassign { value: node_reassign };

                    return Some(reassign_stmt)
                } else {
                    ParsingError::InvalidExpr.error_and_exit();
                }
            } else if let Some(_div_assign) = self.try_consume(TokenType::DIV_ASSIGN) {
                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                    self.check_type_expr(&mut ident, expr.clone());

                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string() }.error_and_exit();
                    }
                    let node_reassign = NodeReassign::Div { expr, ident };
                    let reassign_stmt = NodeStatement::NodeStatementReassign { value: node_reassign };

                    return Some(reassign_stmt)
                } else {
                    ParsingError::InvalidExpr.error_and_exit();
                }
            } else {
                ParsingError::UnexpectedTokenFound { unexpected_token: ident.value.unwrap() }.error_and_exit();
            }
        }
        return None;
    }

    pub fn parse_while_loop(&mut self) -> Option<NodeStatement> {
        if self.try_consume(TokenType::WHILE).is_some(){
            if self.try_consume(TokenType::OPEN_PAREN).is_none() {
                ParsingError::ExpectedTokenNotFound{expected_token:"(".to_string()}.error_and_exit();
            } 
            if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                if self.try_consume(TokenType::CLOSE_PAREN).is_none() {
                    ParsingError::ExpectedTokenNotFound{expected_token:")".to_string()}.error_and_exit();
                    return None; // Rust Compiler things
                }
                if let Some(scope) = self.parse_scope() {
                    let node_while = NodeWhileLoop {
                        condition:expr,
                        scope
                    }; 
                    return Some(NodeStatement::NodeStatementWhileLoop{value:node_while});
                }else {
                    ParsingError::Custom("Expected scope after while(condition) statement.".to_string()).error_and_exit();
                }
            }else {
                ParsingError::ExpectedExpr.error_and_exit();
            }
        }
        
        return None;

        
    }

    pub fn parse_stmt(&mut self) -> Option<NodeStatement> {
        while let Some(_curp) = self.peek_token() {
            if let Some(stmt_exit) = self.parse_exit_stmt() {
                return Some(stmt_exit);
            } else if let Some(let_stmt) = self.parse_let_stmt() {
                return Some(let_stmt);
            } else if let Some(node_scope) = self.parse_scope() {
                let scope_stmt = NodeStatement::NodeStatementScope { value: node_scope };
                return Some(scope_stmt);
            } else if let Some(if_stmt) =  self.parse_if_stmt() {
                return Some(if_stmt);
            } else if let Some(reassign_stmt) = self.parse_reassign_stmt() {
                return  Some(reassign_stmt)   
            }else if let Some(while_loop) = self.parse_while_loop() {
                return Some(while_loop);
            } 
            else {
                break;
            }
        }

        None
    }

    fn try_consume(&mut self, token_type:TokenType) -> Option<Token> {
        if self.peek_token().is_some() && self.peek_token().unwrap().token_type == token_type {
            return self.consume_token();

        }
        return None
    }

    fn peek_token(&self) -> Option<Token> {
        self.peek_token_offset(0)
    }

    fn peek_token_offset(&self, offset: usize) -> Option<Token> {
        if self.index + offset >= self.tokens.len() {
            return None;
        }
        return Some(self.tokens[self.index + offset].clone());
    }

    fn consume_token(&mut self) -> Option<Token> {
        let token = self.peek_token();
        if token.is_some() {
            self.index += 1;
        }
        token
    }
}
