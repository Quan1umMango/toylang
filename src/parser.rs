use crate::*;

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}


#[derive(Debug,Clone)]
pub enum NodeTerm {
    NodeTermIntLit{value:Token},
    NodeTermIdent{value:Token},
    NodeTermParen{value:Box<NodeExpr>}
}

#[derive(Debug,Clone)]
pub enum NodeExpr {   
    NodeExprTerm{value:NodeTerm},
    NodeExprBinExpr{value:BinExpr}
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
pub struct NodeIf {
    pub expr:NodeExpr,
    pub scope:NodeScope
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
    NodeStatementReassign{value:NodeReassign}
}

pub struct NodeProgram {
    pub stmts: Vec<NodeStatement>
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0 }
    }

    pub fn parse_term(&mut self) -> Option<NodeTerm> {
        if let Some(t) = self.peek_token() {
            if t.token_type == TokenType::NUM {
                let term = NodeTerm::NodeTermIntLit { value: self.consume_token().unwrap() };
                return Some(term);
            } else if t.token_type == TokenType::IDENT {
                let term = NodeTerm::NodeTermIdent { value: self.consume_token().unwrap() };
                return Some(term);
            }else if let Some(open_paren) = self.try_consume(TokenType::OPEN_PAREN){
                if let Some(expr) =self. parse_expr(0) {
                    if let Some(_) = self.try_consume(TokenType::CLOSE_PAREN) {
                        let term = NodeTerm::NodeTermParen{value:Box::new(expr)};
                        return Some(term);
                    }else {
                        println!("Expected )");
                        std::process::exit(1);
                    }
                }else {
                    println!("Expected Expression.");
                    std::process::exit(1);
                }

            }
        }
        None
    }

    // https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
    pub fn parse_expr(&mut self,min_prec:usize) -> Option<NodeExpr> {
        if let Some(lhs_term) = self.parse_term() {

            let mut lhs_expr= NodeExpr::NodeExprTerm{value:lhs_term};

            loop {
                if let Some(cur) = self.peek_token() {
                    if !is_bin_op(cur.token_type) || get_bin_precedence_level(cur.token_type).map_or(true, |cur_prec| cur_prec <min_prec) {
                        break;
                    }
                    let next_min_prec = get_bin_precedence_level(cur.token_type).unwrap() + 1;
                    if let Some(op) =self.consume_token() {
                        if let Some(rhs) = self.parse_expr(next_min_prec) {

                            match op.token_type {
                                TokenType::ADD => {
                                    let bin_expr_add = BinExpr::BinExprAdd{lhs:Box::new(lhs_expr),rhs:Box::new(rhs)};
                                    let node_expr = NodeExpr::NodeExprBinExpr{value:bin_expr_add};
                                    lhs_expr = node_expr;
                                },
                                TokenType::MULT => {
                                    let bin_expr_mult = BinExpr::BinExprMult{lhs:Box::new(lhs_expr),rhs:Box::new(rhs)};
                                    let node_expr = NodeExpr::NodeExprBinExpr{value:bin_expr_mult};
                                    lhs_expr = node_expr;
                                }
                                TokenType::SUB => {
                                    let bin_expr_sub = BinExpr::BinExprSub{lhs:Box::new(lhs_expr),rhs:Box::new(rhs)};
                                    let node_expr = NodeExpr::NodeExprBinExpr{value:bin_expr_sub};
                                    lhs_expr = node_expr;
                                },
                                TokenType::DIV => {
                                    let bin_expr_div = BinExpr::BinExprDiv{lhs:Box::new(lhs_expr),rhs:Box::new(rhs)};
                                    let node_expr = NodeExpr::NodeExprBinExpr{value:bin_expr_div};
                                    lhs_expr = node_expr;
                                }
                                _ => {}
                            }
                        }else {
                            println!("Invalid Operator");
                            std::process::exit(1);
                        }
                    }else {
                        println!("Unable to parse rhs");
                        std::process::exit(1);
                    }
                } else {
                    break;
                }           
            }
            return Some(lhs_expr);
        }  
        else{
            return None;
        } 
        /*
        if let Some(lhs) = self.parse_term() {
        if self.try_consume(TokenType::ADD).is_some(){
        if let Some(rhs) = self.parse_expr() {
        let bin_expr_add = BinExpr::BinExprAdd{lhs:Box::new(NodeExpr::NodeExprTerm{value:lhs}),rhs:Box::new(rhs)};
        let node_expr = NodeExpr::NodeExprBinExpr{value:bin_expr_add};
        return Some(node_expr);
        }
        }else if self.try_consume(TokenType::MULT).is_some() {
        if let Some(rhs) = self.parse_expr() {
        let bin_expr_mult = BinExpr::BinExprMult{lhs:Box::new(NodeExpr::NodeExprTerm{value:lhs}),rhs:Box::new(rhs)};
        let node_expr = NodeExpr::NodeExprBinExpr{value:bin_expr_mult};
        return Some(node_expr);
        }            }
        else {
        return Some(NodeExpr::NodeExprTerm{value:lhs})
        }
        }else {
        return None;
        }        None
        */
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
                        println!("Expected }}");
                        std::process::exit(1);
                    }
                    return None;
                }
                v
            };
            if !self.try_consume(TokenType::CLOSE_CURLY).is_some(){
                println!("Expected }}");
                std::process::exit(1);
            }
            let scope = NodeScope {
                stmts,
            };
            return Some(scope);
        }
        return None;
    }

    pub fn parse_stmt(&mut self) -> Option<NodeStatement> {
        while let Some(curp) = self.peek_token() {
            if self.try_consume(TokenType::EXIT).is_some() && self.try_consume(TokenType::OPEN_PAREN).is_some()
            {
                let mut stmt_exit = None;

                if let Some(expr) = self.parse_expr(0) {
                    stmt_exit = Some(NodeStatement::NodeStatementExit { value: NodeExit { expr } });
                } else {
                    println!("Invalid Expression.");
                    std::process::exit(1);
                }

                if self.try_consume(TokenType::CLOSE_PAREN).is_none() {
                    println!("Expected )");
                    std::process::exit(1);
                };

                if self.try_consume(TokenType::SEMICOLON).is_none() {
                    println!("Expected ;");
                    std::process::exit(1);
                };
                return stmt_exit;
            } else if curp.token_type == TokenType::LET
            && self.peek_token_offset(1).is_some()
            && self.peek_token_offset(1).unwrap().token_type == TokenType::IDENT
            && self.peek_token_offset(2).is_some()
            && self.peek_token_offset(2).unwrap().token_type == TokenType::ASSIGN
            {
                self.consume_token(); // CONSUME let
                let ident = self.consume_token();
                let mut stmt_let = None;
                self.consume_token(); // Consume =

                if let Some(expr) = self.parse_expr(0) {
                    stmt_let = Some(NodeStatement::NodeStatementLet {
                        value: NodeLet {
                            expr,
                            ident: ident.unwrap(),
                        },
                    });
                } else {
                    println!("Incorrect Expression");
                    std::process::exit(1);
                }
                if self.try_consume(TokenType::SEMICOLON).is_none() {
                    println!("Expected ;");
                    std::process::exit(1);
                };


                return stmt_let;
            }else if let Some(node_scope) = self.parse_scope() {
                let scope_stmt = NodeStatement::NodeStatementScope{value:node_scope};
                return Some(scope_stmt);
            } else if let Some(if_tok) = self.try_consume(TokenType::IF) {
                if let Some(open_paren) = self.try_consume(TokenType::OPEN_PAREN) {
                    if let Some(expr) = self.parse_expr(0) {
                        if let Some(close_paren) = self.try_consume(TokenType::CLOSE_PAREN) {
                            if let Some(scope) = self.parse_stmt() {
                                let scope = match scope {
                                    NodeStatement::NodeStatementScope{value} => value, 
                                    _ => {
                                        println!("Failed parsing scope.");
                                        std::process::exit(1);
                                    }
                                };
                                let node_if = NodeIf {
                                    expr,
                                    scope,
                                };
                                let if_stmt = NodeStatement::NodeStatementIf{value:node_if};
                                return Some(if_stmt);
                            }else {
                                println!("Expected scope after if condition. It is either empty or does not exist");
                                std::process::exit(1);
                            }
                        }else {
                            println!("Expected )");
                            std::process::exit(1);
                        }
                    }else {
                        println!("Failed to parse expression");
                        std::process::exit(1);
                    }
                }else {
                    println!("Expected (");
                    std::process::exit(1);
                }
            }           
            else if let Some(ident) = self.try_consume(TokenType::IDENT) {
                if let Some(assign) = self.try_consume(TokenType::ASSIGN) {
                    if let Some(expr) = self.parse_expr(0) {
                        println!("{:?}",self.peek_token());
                        if self.try_consume(TokenType::SEMICOLON).is_none() {
                            println!("Expected ;");
                            std::process::exit(1);
                        }
                        let node_reassign = NodeReassign::Assign {
                            expr, 
                            ident,
                        };
                        let reassign_stmt = NodeStatement::NodeStatementReassign{value:node_reassign};
                        return Some(reassign_stmt)
                    }else {
                        println!("Invalid Expression");
                    }
                }else if let Some(add_assign) = self.try_consume(TokenType::ADD_ASSIGN) {
                    if let Some(expr) = self.parse_expr(0) {
                        if self.try_consume(TokenType::SEMICOLON).is_none() {
                            println!("Expected ;");
                            std::process::exit(1);
                        }
                        let node_reassign = NodeReassign::Add {
                            expr,
                            ident,
                        };
                        let reassign_stmt = NodeStatement::NodeStatementReassign{value:node_reassign};
                        return Some(reassign_stmt)

                    }else {
                        println!("Invalid Expression");
                    }
                }else if let Some(sub_assign) = self.try_consume(TokenType::SUB_ASSIGN) {
                    if let Some(expr) = self.parse_expr(0) {
                        if self.try_consume(TokenType::SEMICOLON).is_none() {
                            println!("Expected ;");
                            std::process::exit(1);
                        }
                        let node_reassign = NodeReassign::Sub {
                            expr,
                            ident,
                        };
                        let reassign_stmt = NodeStatement::NodeStatementReassign{value:node_reassign};
                        return Some(reassign_stmt)

                    }else {
                        println!("Invalid Expression");
                    }
                }else if let Some(mult_assign) = self.try_consume(TokenType::MULT_ASSIGN) {
                    if let Some(expr) = self.parse_expr(0) {
                        if self.try_consume(TokenType::SEMICOLON).is_none() {
                            println!("Expected ;");
                            std::process::exit(1);
                        }
                        let node_reassign = NodeReassign::Mul {
                            expr,
                            ident,
                        };
                        let reassign_stmt = NodeStatement::NodeStatementReassign{value:node_reassign};
                        return Some(reassign_stmt)
                    }else {
                        println!("Invalid Expression");
                    }
                }else if let Some(div_assign) = self.try_consume(TokenType::DIV_ASSIGN) {
                    if let Some(expr) = self.parse_expr(0) {
                        if self.try_consume(TokenType::SEMICOLON).is_none() {
                            println!("Expected ;");
                            std::process::exit(1);
                        }
                        let node_reassign = NodeReassign::Div {
                            expr,
                            ident,
                        };
                        let reassign_stmt = NodeStatement::NodeStatementReassign{value:node_reassign};
                        return Some(reassign_stmt)
                    }else {
                        println!("Invalid Expression");
                    }
                }else {
                    println!("Unexpect symbol when trying to assign to ident: {}",ident.value.unwrap());
                }
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
