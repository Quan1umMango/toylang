
use crate::*;
use std::collections::HashMap;
use indexmap::IndexMap;

#[derive(Debug)]
pub enum VarType {
    // If its normal, then we get the position on the stack by using getstack 
    Normal,
    // if its func arg, then we get using getfromsp.
    FuncArg,
}

#[derive(Debug)]
pub struct Var {
    stack_loc:usize,
    var_type:VarType,
}


pub struct GeneratorVM {
    input: NodeProgram,
    output:String,
    variables:IndexMap<String,Var>,
    stack_size:usize,
    parser_fundefs:HashMap<String,NodeFunctionDefination>,
    parser_vars:HashMap<String,DataType>,
    scopes:Vec<usize>, // where the scope starts 
    num_scopes:usize, // number of all the scopes in the program ever created
}



impl GeneratorVM {
    pub fn new(input:NodeProgram,parser_fundefs:HashMap<String,NodeFunctionDefination>,parser_vars:HashMap<String,DataType>) -> Self {
        Self {
            input,
            output:String::from("label main:"),
            parser_vars,
            parser_fundefs,
            variables:IndexMap::new(),
            stack_size:0,
            scopes:Vec::new(),
            num_scopes:0
        
        }
    }


    pub fn generate(&mut self) {
        for stmt in self.input.stmts.clone().iter() {
            self.generate_statement(stmt);
        }
    }

    pub fn generate_statement(&mut self, stmt:&NodeStatement) {
        use NodeStatement::*;
        match stmt {
            NodeStatementExit { value } => {
                self.generate_expr_into(&value.expr,"rax");
                self.output = format!("{}\n\tdisplay rax\n\thalt",self.output);
            }
            NodeStatementLet { value } => {
                let ident = value.ident.clone();
                if self.variables.contains_key(ident.value.as_ref().unwrap()) {
                    GenerationError::SameIdentifiers{ident:ident.clone()}.error_and_exit();
                }    
                
                self.generate_expr_into(&value.expr,"rax");
                self.push("rax");

                let var = Var {
                    stack_loc:self.stack_size,
                    var_type:VarType::Normal,
                };
                
                self.variables.insert(ident.value.unwrap(),var);
                // self.variables.insert(ident.value.unwrap(),value.expr.get_datatype(
                //      &self.parser_fundefs,&self.parser_vars));
            }

            NodeStatementReassign { value } => {
                match value {
                    NodeReassign::Assign { ident, expr } => {
                        match ident {
                            NodeTerm::NodeTermIdent { value } => {
                                self.generate_expr_into(expr,"rax");
                                self.try_set_ident_reg(value.clone(),"rax");
                            }
                            _ => unreachable!()
                       } 
                    }
                    NodeReassign::Add { ident, expr } => {
                        match ident {
                            NodeTerm::NodeTermIdent { value } => {
                                self.try_load_ident_to(&value,"rbx");
                                self.generate_expr_into(expr,"rax");
                                self.output = format!("{}\n\tadd rax,rbx",self.output);
                                self.try_set_ident_reg(value.clone(),"rax");
                            }
                            _ => unreachable!()
                       } 
                    }
                    NodeReassign::Sub { ident, expr } => {
                        match ident {
                            NodeTerm::NodeTermIdent { value } => {
                                self.try_load_ident_to(&value,"rbx");
                                self.generate_expr_into(expr,"rax");
                                self.output = format!("{}\n\tSub rax,rbx",self.output);
                                self.try_set_ident_reg(value.clone(),"rax");
                            }
                            _ => unreachable!()
                       } 
                    }
                    NodeReassign::Div { ident, expr } => {
                        match ident {
                            NodeTerm::NodeTermIdent { value } => {
                                self.try_load_ident_to(&value,"rbx");
                                self.generate_expr_into(expr,"rax");
                                self.output = format!("{}\n\tdiv rax,rbx",self.output);
                                self.try_set_ident_reg(value.clone(),"rax");
                            }
                            _ => unreachable!()
                       } 
                    }
                    NodeReassign::Mul { ident, expr } => {
                        match ident {
                            NodeTerm::NodeTermIdent { value } => {
                                self.try_load_ident_to(&value,"rbx");
                                self.generate_expr_into(expr,"rax");
                                self.output = format!("{}\n\tmul rax,rbx",self.output);
                                self.try_set_ident_reg(value.clone(),"rax");
                            }
                            _ => unreachable!()
                       } 
                    }
                    _ => todo!()
                }
            }
            NodeStatementScope { value } => {
                self.scopes.push(self.stack_size);
                self.output = format!("{}\nlabel scope{}:",self.output,self.num_scopes);
                self.generate_scope_without_label(value); 
                self.num_scopes +=1;
            }
            NodeStatementIf { value } => {
                self.generate_expr_into(&value.expr,"rax");
                let main_scope_name = format!("scope_end{}",self.num_scopes);
                self.output = format!("{}\n\tcmp rax, 1\n\tjne {}",self.output,main_scope_name);
                self.scopes.push(self.stack_size);
                self.generate_scope_without_label(&value.scope);
                self.num_scopes+=1;
                if value.else_stmt.is_some() {
                    todo!();
                }
                self.output = format!("{}\nlabel {}:",self.output,main_scope_name);
            }
            NodeStatementWhileLoop { value } => {
                self.scopes.push(self.stack_size);
                let loop_scope = format!("scope{}",self.num_scopes);
                let main_scope = format!("scope_end{}",self.num_scopes);
                self.output = format!("{}\nlabel {loop_scope}:",self.output);
                self.generate_expr_into(&value.condition,"rax");
                self.output = format!("{}\n\tcmp rax, 1\n\tjne {main_scope}",self.output);
                self.generate_scope_without_label(&value.scope);
                self.output = format!("{}\n\tjmp {loop_scope}",self.output);
                self.num_scopes +=1;
                self.output = format!("{}\nlabel {main_scope}:",self.output);
            }
            NodeStatementFunctionCall { value } => {
                if self.parser_fundefs.get(value.ident.value.as_ref().unwrap()).is_none() {

                    GenerationError::UndeclaredIdentifier{ident:value.ident.clone()}.error_and_exit();
                } 
                let pfd = self.parser_fundefs.clone();
                let f= pfd.get(value.ident.value.as_ref().unwrap()).unwrap();
                if value.args.len() != f.args.len() {
                    GenerationError::Custom(format!("Function {} require {} arguments. Found {}. ",f.ident.clone().value.unwrap(),f.args.len(),value.args.len())).error_and_exit();
                }
                for i in 0..value.args.len() {
                    let expected_arg_type = f.args[i].get_datatype(&self.parser_fundefs,&self.parser_vars);
                    let got_arg_type = value.args[i].get_datatype(&self.parser_fundefs,&self.parser_vars);
                    if got_arg_type != expected_arg_type {
                        GenerationError::Custom(
                            format!("Function {} required argument of type {expected_arg_type} for argument number {}. Found argument of type {got_arg_type}. "
                                    ,f.ident.clone().value.unwrap(),i)).error_and_exit();
                    }
                    self.generate_expr_into(&value.args[i],"rax");
                    self.push("rax");
                }
                self.append_output(format!("\n\tcall {}",f.ident.value.clone().unwrap()));
            } 

            NodeStatementFunctionDefination { value } => {
                // we dont need to check it because its already there in self.parser_funcdefs
                // because of the parser
                /*if self.parser_fundefs.get(value.ident.value.as_ref().unwrap()).is_some() {

                    GenerationError::SameIdentifiers{ident:value.ident.clone()}.error_and_exit();
                } */
                let prev_var_nums = self.variables.len();

                // Now we be a lil bit sneaky. So essentially we are generating the function (label) separately, then prepending to the main program
                let op = self.output.clone();
                self.output = "".to_string();


                for (i,(tok,_expr)) in value.args.iter().enumerate() {
                    let var = Var {
                        stack_loc:value.args.len()-1-i,
                        var_type:VarType::FuncArg,
                    };
                    self.variables.insert(tok.clone().value.unwrap(),var);
                }  
                self.scopes.push(self.stack_size);
                self.output = format!("{}\nlabel {}:",self.output,value.ident.clone().value.unwrap());
                // We store where the sp currently is. We'll use this to get argument variables
                self.append_output("\n\tgetsp rcx".to_owned());
               
                self.generate_scope_without_label(&value.scope); 
                
                // Cleanup funtion argument variables.
                // if not done, we will error if we do something  like:
                // fn myfunc(int32 x) {}
                // let x = 23; // error: cannot use same identifier
                for _ in 0..self.variables.len() - prev_var_nums {
                    let _ = self.variables.pop();
                }

                self.num_scopes +=1;
                self.append_output("\n\tret \n".to_string());
                self.append_output(op);
            }
            _ => todo!()
        } 
    }

    pub fn generate_scope_without_label(&mut self, value:&NodeScope) {
        let num_vars = self.variables.len();
        for stmt in value.stmts.iter() {
            self.generate_statement(stmt);
        }
        let _ = self.scopes.pop();
        for _ in 0..self.variables.len() - num_vars {
            self.variables.pop();
        }
    }

    pub fn generate_expr_into(&mut self,expr:&NodeExpr,reg:&str) {
        match expr {
            NodeExpr::NodeExprTerm { value } => {
                self.generate_term_into(value,reg);
           }
            NodeExpr::NodeExprBinExpr { value } => {
                match value {
                    BinExpr::BinExprAdd { lhs, rhs } => {
                        self.generate_expr_into(&**lhs,"rdx");
                        self.push("rbx");
                        self.generate_expr_into(&**rhs,"rbx");
                        self.output = format!("{}\n\tadd rdx, rbx",self.output);
                        if reg != "rdx" {
                            self.output = format!("{}\n\tmov {reg},rdx",self.output);
                        }
                        self.pop("rbx");
                    }
                    BinExpr::BinExprSub { lhs, rhs } => {
                        self.generate_expr_into(&**lhs,"rdx");
                        self.push("rbx");
                        self.generate_expr_into(&**rhs,"rbx");
                        self.append_output("{}\n\tsub rdx, rbx".to_owned());
                        self.append_output(format!("\n\tmov {reg},rdx"));
                        self.pop("rbx");
                    }
                    BinExpr::BinExprMult { lhs, rhs } => {
                        self.generate_expr_into(&**lhs,"rdx");
                        self.push("rbx");
                        self.generate_expr_into(&**rhs,"rbx");
                        self.append_output("\n\tmul rdx, rbx".to_owned());
                        self.append_output(format!("\n\tmov {reg},rdx"));
                        self.pop("rbx");
                    }


                    _ => todo!()
                }
            }
            NodeExpr::NodeExprBoolExpr { value } => {
                match value {
                    BoolExpr::BoolExprEqualTo { lhs, rhs } => {
                        self.generate_expr_into(&**lhs,"rax");
                        self.generate_expr_into(&**rhs,"rbx");
                        self.append_output(format!("\n\tcmp rax, rbx\n\tgetflag {reg},eqf"));
                    }
                    BoolExpr::BoolExprNotEqualTo { lhs, rhs } => {
                        self.generate_expr_into(&**lhs,"rax");
                        self.generate_expr_into(&**rhs,"rbx");
                        self.append_output(format!("\n\tcmp rax, rbx\n\tgetflag {reg},eqf\n\tnot {reg}"));
                        
                    }
                    BoolExpr::BoolExprLessThanOrEqualTo { lhs, rhs } => {
                        self.generate_expr_into(&**lhs,"rax");
                        self.generate_expr_into(&**rhs,"rbx");
                        self.append_output(format!("\n\tcmp rax, rbx\n\tgetflag rax,gf\n\tcmp rax, 0\n\tgetflag {reg},eqf"));
                    }
                    BoolExpr::BoolExprGreaterThanOrEqualTo { lhs, rhs } => {
                        self.generate_expr_into(&**lhs,"rax");
                        self.generate_expr_into(&**rhs,"rbx");
                        self.append_output(format!("\n\tcmp rax, rbx\n\tgetflag rax,lf\n\tcmp rax, 0\n\tgetflag {reg},eqf"));
                    } 
                    BoolExpr::BoolExprLessThan { lhs, rhs } => {
                        self.generate_expr_into(&**lhs,"rax");
                        self.generate_expr_into(&**rhs,"rbx");
                        self.append_output(format!("\n\tcmp rax, rbx\n\tgetflag rax,lf\n\tcmp rax, 1\n\tgetflag {reg},eqf"));
                    }
                    BoolExpr::BoolExprGreaterThan { lhs, rhs } => {
                        self.generate_expr_into(&**lhs,"rax");
                        self.generate_expr_into(&**rhs,"rbx");
                        self.append_output(format!("\n\tcmp rax, rbx\n\tgetflag rax,gf\n\tcmp rax, 1\n\tgetflag {reg},eqf"));
                    }
                    _ => todo!()
                }
            }
            NodeExpr::NodeExprFunctionCall { value } => {
                 if self.parser_fundefs.get(value.ident.value.as_ref().unwrap()).is_none() {

                    GenerationError::UndeclaredIdentifier{ident:value.ident.clone()}.error_and_exit();
                } 
                let pfd = self.parser_fundefs.clone();
                let f= pfd.get(value.ident.value.as_ref().unwrap()).unwrap();
                if value.args.len() != f.args.len() {
                    GenerationError::Custom(format!("Function {} require {} arguments. Found {}. ",f.ident.clone().value.unwrap(),f.args.len(),value.args.len())).error_and_exit();
                }
                for i in 0..value.args.len() {
                    let expected_arg_type = f.args[i].get_datatype(&self.parser_fundefs,&self.parser_vars);
                    let got_arg_type = value.args[i].get_datatype(&self.parser_fundefs,&self.parser_vars);
                    if got_arg_type != expected_arg_type && expected_arg_type != DataType::Infer {
                        GenerationError::Custom(
                            format!("Function {} required argument of type {expected_arg_type} for argument number {}. Found argument of type {got_arg_type}. "
                                    ,f.ident.clone().value.unwrap(),i)).error_and_exit();
                    }
                    self.generate_expr_into(&value.args[i],"rax");
                    self.push("rax");
                }
                self.append_output(format!("\n\tcall {}",f.ident.value.clone().unwrap()));

            }
            _ => todo!()
        }
    } 

    pub fn generate_term_into(&mut self, term:&NodeTerm,reg:&str) {
        match term {
            NodeTerm::NodeTermIntLit { value } => {
                self.output = format!("{}\n\tmov {reg},{}",self.output,value.value.clone().unwrap());
            }
            NodeTerm::NodeTermIdent { value } => {
               self.try_load_ident_to(value,reg); 
            }
            NodeTerm::NodeTermBool { value } => {
                self.append_output(format!("\n\tmov {reg}, {}",if value.token_type == TokenType::TRUE { "1" } else {"0"}))
            }
            _ => todo!()
        }
    }

    // sets the identifier vvalue to the specified register
    pub fn try_set_ident_reg(&mut self, ident:Token, reg:&str) {
        if let Some(v) = self.variables.get(ident.value.as_ref().unwrap()) {
            match v.var_type {
                VarType::Normal => self.output = format!("{}\n\tsetstack {},{reg}",self.output,v.stack_loc-1),
                VarType::FuncArg => {
                    self.append_output(format!("\n\tmov rax, rcx\n\tsub rax, {}\n\tsetstack rax,{reg}",v.stack_loc+1))
                    //self.append_output(format!("\n\tsetfromsp {},{reg}",v.stack_loc));
                }
            }
        }else {
            GenerationError::UndeclaredIdentifier{ident:ident.clone()}.error_and_exit();
        }
    }

    pub fn try_load_ident_to(&mut self, ident:&Token,reg:&str) {
        if let Some(v) = self.variables.get(ident.value.as_ref().unwrap()) {
            match v.var_type {
                VarType::Normal => self.output = format!("{}\n\tgetfromstack {},{reg}",self.output,v.stack_loc-1),
                VarType::FuncArg => {
                    self.append_output(format!("\n\tmov rax, rcx\n\tsub rax, {}\n\tgetfromstack rax, {reg}",v.stack_loc+1))
                    //self.append_output(format!("\n\tgetfromsp {},{reg}",v.stack_loc));
                }
            }
            //self.output = format!("{}\n\t getfromstack {},{reg}",self.output,v.stack_loc-1);
        }else {
            GenerationError::UndeclaredIdentifier{ident:ident.clone()}.error_and_exit();
        }

    }


    pub fn append_output(&mut self, s:String) {
        self.output.push_str(s.as_str());
    }

    pub fn output(&self) -> &String {
        &self.output
    }

    pub fn push(&mut self,v:&str) {
        self.output = format!("{}\n\tpush {v}",self.output);
        self.stack_size+=1;
    }

    pub fn pop(&mut self,r:&str) {
        self.output = format!("{}\n\tpop {r}",self.output);
        self.stack_size-=1;
    }


}
