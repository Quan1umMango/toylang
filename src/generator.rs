use crate::*;
use std::collections::HashMap;
use indexmap::IndexMap;


pub struct Generator {
    node_program: NodeProgram,
    output: String,
    stack_size:usize,
    variables:IndexMap<String,Var>,
    scopes:Vec<usize>,
    label_count:usize,
}

#[derive(Clone)]
pub struct Var {
    stack_loc:usize,
}


impl Generator {
    pub fn new(node_program: NodeProgram) -> Generator {

        return Generator { node_program,output:"".to_string(),stack_size: 0,variables:IndexMap::new(),scopes:Vec::new(),label_count:0};
    }


    pub fn begin_scope(&mut self) {
        self.scopes.push(self.variables.len());
    }

    pub fn end_scope(&mut self) {
        /* if self.scopes[self.scopes.len()-1] == 0 {
        return;
        self.scopes
        }*/
        let pop_count = self.variables.len()- self.scopes[self.scopes.len()-1];
        self.output = format!("{}\n\tadd rsp, {}\n",self.output,pop_count*8);
        self.stack_size -= pop_count;
        for i in self.variables.len()..pop_count {
            self.variables.pop();
        }   
    }

    pub fn generate_term(&mut self, term:&NodeTerm) {
        match &term {

            NodeTerm::NodeTermIntLit{value:t} => {
                self.output = format!("{}\n\tmov rax, {}",self.output,t.value.clone().unwrap());
                self.push("rax".to_string());
            },
            NodeTerm::NodeTermIdent{value:t} => {  
                if let Some(var) = self.variables.get(&t.value.clone().unwrap()) {
                    self.push(format!("qword [rsp + {}]\n",(self.stack_size-var.stack_loc-1)*8)); 
                }else {
                    println!("Undeclared identifier: '{}'",t.value.clone().unwrap());
                    std::process::exit(0);
                }
            },
            NodeTerm::NodeTermParen{value:expr} => {
                self.generate_expr(&expr);
            }
        }
    }



    pub fn generate_bin_expr(&mut self,bin_expr:&BinExpr) {
        match &bin_expr {
            
            BinExpr::BinExprDiv{lhs,rhs} => {
                let lhs =*lhs.clone();
                let rhs =*rhs.clone();
                self.generate_expr(&rhs);
                self.generate_expr(&lhs);
                self.pop("rax".to_string());
                self.pop("rbx".to_string());
                
                self.output = format!("{}\n\txor rdx, rdx\n\tdiv rbx\n\t",self.output);
                self.push("rax".to_string());

            }
            
            BinExpr::BinExprMult{lhs,rhs} => {
               let lhs =*lhs.clone();
                let rhs =*rhs.clone();
                self.generate_expr(&rhs);
                self.generate_expr(&lhs);
                self.pop("rax".to_string());
                self.pop("rdx".to_string());
                self.output = format!("{}\n\tmul rdx\n\t",self.output);
                self.push("rax".to_string());
            }

            BinExpr::BinExprAdd{lhs,rhs} => {
                let lhs =*lhs.clone();
                let rhs =*rhs.clone();
                self.generate_expr(&rhs);
                self.generate_expr(&lhs);
                self.pop("rax".to_string());
                self.pop("rdx".to_string());
                self.output = format!("{}\n\tadd rax,rdx\n\t",self.output);
                self.push("rax".to_string());
            },
            BinExpr::BinExprSub{lhs,rhs} => {
               let lhs =*lhs.clone();
                let rhs =*rhs.clone();
                self.generate_expr(&rhs);
                self.generate_expr(&lhs);
                self.pop("rax".to_string());
                self.pop("rdx".to_string());
                self.output = format!("{}\n\tsub rax,rdx\n\t",self.output);
                self.push("rax".to_string());
            }
        }
    }

    pub fn generate_expr(&mut self,expr:&NodeExpr) {
        match &expr {
            NodeExpr::NodeExprTerm{value} => {self.generate_term(&value);}
            NodeExpr::NodeExprBinExpr{value:bin_expr} => {self.generate_bin_expr(&bin_expr);}
        }
    }

    pub fn generate_scope(&mut self,node_scope:&NodeScope) {
                       self.begin_scope();
                for stmt in node_scope.stmts.clone().iter() {
                    self.generate_statement(stmt);
                }
                self.end_scope();
    }
    

    pub fn generate_statement(&mut self,stmt:&NodeStatement) {
        match &stmt {

            NodeStatement::NodeStatementLet{value:let_stmt} => {
                if self.variables.get(&let_stmt.ident.value.clone().unwrap()).is_some() {
                    println!("Cannot have variables with same name: '{}'",let_stmt.ident.value.clone().unwrap());
                    std::process::exit(0);
                }
                self.variables.insert(let_stmt.ident.value.clone().unwrap(),Var {stack_loc:self.stack_size});

                self.generate_expr(&let_stmt.expr);     

            }
            NodeStatement::NodeStatementExit{value:exit_stmt} => {
                self.generate_expr(&exit_stmt.expr);
                self.pop("rdi".to_string());
                self.output = format!("{}\n\tmov rcx, rdi\n\tsub rsp, 32\n\tcall ExitProcess\n",self.output);    

            }
            NodeStatement::NodeStatementScope{value:node_scope} =>  {
                self.generate_scope(&node_scope);
            }
            NodeStatement::NodeStatementIf{value:if_stmt} => {
                self.generate_expr(&if_stmt.expr);
                self.pop("rax".to_string());
                let label = self.get_label();
                self.output = format!("{}\n\ttest rax,rax\n\tjz {}",self.output,label.clone());
                self.generate_scope(&if_stmt.scope);
                self.output = format!("{}\n\t{}:",self.output,label.clone());
            }
            NodeStatement::NodeStatementReassign{value:reassign_stmt} => {
                match &reassign_stmt {
                   NodeReassign::Assign{ident,expr} => {
                       
                        let ident_term = NodeTerm::NodeTermIdent{value:ident.clone()};
                        self.generate_term(&ident_term);
                        self.generate_expr(&expr);
                        self.pop("rax".to_string());
                        if let Some(var) = self.variables.get(&ident.value.clone().unwrap()) {
                            self.output = format!("{}\n\tmov qword [rsp + {}],rax\n",self.output,(self.stack_size-var.stack_loc-1)*8); 
                        }else {
                            println!("Identifier '{}' is not defined.",ident.value.clone().unwrap());
                            std::process::exit(1);
                        }                        
                    },
                    NodeReassign::Add{ident,expr} => {
                        let ident_term = NodeTerm::NodeTermIdent{value:ident.clone()};
                        self.generate_term(&ident_term);
                        self.generate_expr(&expr);
                        self.pop("rdx".to_string());
                        if let Some(var) = self.variables.get(&ident.value.clone().unwrap()) {
                            self.output = format!("{}\n\tmov rax ,qword [rsp+{}]",self.output,(self.stack_size-var.stack_loc-1)*8);
                            self.output = format!("{}\n\tadd rax,rdx",self.output);
                            self.output = format!("{}\n\tmov qword [rsp + {}],rax\n",self.output,(self.stack_size-var.stack_loc-1)*8); 
                        }else {
                            println!("Identifier '{}' is not defined.",ident.value.clone().unwrap());
                            std::process::exit(1);
                        }      
                    }
                    NodeReassign::Sub{ident,expr} => {
                        let ident_term = NodeTerm::NodeTermIdent{value:ident.clone()};
                        self.generate_term(&ident_term);
                        self.generate_expr(&expr);
                        self.pop("rdx".to_string());
                        if let Some(var) = self.variables.get(&ident.value.clone().unwrap()) {
                            self.output = format!("{}\n\tmov rax ,qword [rsp+{}]",self.output,(self.stack_size-var.stack_loc-1)*8);
                            self.output = format!("{}\n\tsub rax,rdx",self.output);
                            self.output = format!("{}\n\tmov qword [rsp + {}],rax\n",self.output,(self.stack_size-var.stack_loc-1)*8); 
                        }else {
                            println!("Identifier '{}' is not defined.",ident.value.clone().unwrap());
                            std::process::exit(1);
                        }      
                    }
                    NodeReassign::Mul{ident,expr} => {
                        let ident_term = NodeTerm::NodeTermIdent{value:ident.clone()};
                        self.generate_term(&ident_term);
                        self.generate_expr(&expr);

                        self.pop("rdx".to_string());
                        if let Some(var) = self.variables.get(&ident.value.clone().unwrap()) {
 
                        self.output = format!("{}\n\tmov rax ,qword [rsp+{}]",self.output,(self.stack_size-var.stack_loc-1)*8);

 
                           self.output = format!("{}\n\tmul rdx",self.output);
                            self.output = format!("{}\n\tmov qword [rsp + {}],rax\n",self.output,(self.stack_size-var.stack_loc-1)*8); 
                        }else {
                            println!("Identifier '{}' is not defined.",ident.value.clone().unwrap());
                            std::process::exit(1);
                        }      
                    }
                    NodeReassign::Div{ident,expr} => {
                    let ident_term = NodeTerm::NodeTermIdent{value:ident.clone()};
                     self.generate_expr(&expr);
                    self.generate_term(&ident_term);
  let variables = self.variables.clone();

                    if let Some(var) = variables.get(&ident.value.clone().unwrap()) {
                    
//                    self.output = format!("{}\n\tmov rax ,qword [rsp+{}]",self.output,(self.stack_size-var.stack_loc-1)*8);
  self.pop("rax".to_string());
                            self.pop("rbx".to_string());
                        self.output = format!("{}\n\txor rdx, rdx\n\tdiv rbx",self.output);
                        self.output = format!("{}\n\tmov qword [rsp + {}],rax\n",self.output,(self.stack_size-var.stack_loc-1)*8); 
                    }else {
                        println!("Identifier '{}' is not defined.",ident.value.clone().unwrap());
                        std::process::exit(1);
                    }      

                }
            }
            }
        };
    }

    pub fn generate_program(&mut self) -> String {
        self.output = String::from(
            "section .text\n\tglobal _start\n\t
                extern ExitProcess\n_start:\n\t
                ",
        );

        for node_stmt in self.node_program.stmts.clone().iter() {
            self.generate_statement(node_stmt);
        }
/*
        let exit_code = match &self.node_program.stmts[self.node_program.stmts.len()-1].value {
            NodeExprVarient::NodeExprIntLit(tok) => tok.value.clone(),
            _ => Some("0".to_string()),
        };
        self.output = format!(
            "{}mov rcx, {}\n\tsub rsp, 32\n\tcall ExitProcess",
            self.output,
            exit_code.unwrap()
        );
        */

        // Add Exit code 
        self.output = format!("{}\n\tmov rcx, 0\n\tsub rsp, 32\n\tcall ExitProcess",self.output.clone());
        return self.output.clone();
    }

    pub fn push(&mut self, reg:String) {
        self.output = format!("{}\n\tpush {}\n\t",self.output,reg);
        self.stack_size +=1;
    }

    pub fn pop(&mut self, reg:String) {
  self.output = format!("{}\n\tpop {}\n\t",self.output,reg);
        self.stack_size -=1;

    }

    // Gets a new label name
    pub fn get_label(&mut self) -> String {
        self.label_count +=1;
        return format!("label{}",self.label_count-1);
    }
}
