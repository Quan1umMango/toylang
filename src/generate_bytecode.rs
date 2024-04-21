use crate::*;
use std::collections::HashMap;
use indexmap::IndexMap;


pub struct GeneratorVM {
    node_program: NodeProgram,
    output: String,
    stack_size:usize,
    variables:IndexMap<String,Var>,
    functions:HashMap<String,NodeFunctionDefination>, 
    scopes:Vec<usize>,
    label_count:usize,
}

#[derive(Debug,Clone,PartialEq)]
enum VarType {
    FuncVariable, // for function params
    NormalVariable
}

#[derive(Debug,Clone)]
pub struct Var {
    stack_loc:usize,
    t: VarType,
}


impl GeneratorVM {
    pub fn new(node_program: NodeProgram) -> GeneratorVM {
        return GeneratorVM { node_program,output:"".to_string(),stack_size: 0,variables:IndexMap::new(),scopes:Vec::new(),label_count:0,functions:HashMap::new()};
    }

    pub fn generate(&mut self) {
        // create main label_count  
        self.output = "label main:\n".to_string();

        for node_stmt in self.node_program.stmts.clone().iter() {
            self.generate_statement(&node_stmt);
        }
        self.output = format!("{}\n\t\n\tmov rax,0\n\t display rax\n\thalt",self.output);
    }

    pub fn generate_statement(&mut self, node_stmt: &NodeStatement) {
        use NodeStatement::*;
        match node_stmt {
            NodeStatementExit { value } => {
                self.generate_expr(&value.expr);
                self.pop("rax"); 
                self.output = format!("{}\n\tdisplay rax\n\thalt",self.output);
            }
            NodeStatementLet { value } => {
                if self.variables.get(&value.ident.value.clone().unwrap()).is_some() {

                    GenerationError::SameIdentifiers{ident:value.ident.clone()}.error_and_exit();
                }
                self.generate_expr(&value.expr);
                let v = Var {stack_loc:self.stack_size-1,t:VarType::NormalVariable};
                self.variables.insert(value.ident.clone().value.unwrap(),v);
            }
            
            NodeStatementScope { value } => {
                self.generate_scope(&value);
            }

            NodeStatementIf { value:if_stmt } => {
                self.generate_expr(&if_stmt.expr);
                let label = self.get_label();
                if let Some(else_stmt) = &if_stmt.else_stmt {

                    self.output = format!("{}\n\tcmp rax,0\n\tjne {}else",self.output,&label);
                    self.generate_scope(&if_stmt.clone().scope);
                    if else_stmt.if_stmt.is_some() {
                        let else_if = *else_stmt.if_stmt.clone().unwrap(); 
                        self.output = format!("{}\n\tlabel {}else:\n",self.output,label.clone());
                        self.generate_statement(&else_if);
                    }else {
                        self.output = format!("{}\n\tlabel {}else:\n",self.output,label.clone());
                        self.generate_scope(&else_stmt.clone().scope.unwrap());
                    }
                    self.output = format!("{}\n\tjmp {}\n\tlabel {}:",self.output,label.clone(),label.clone())

                }else {
                    self.output = format!("{}\n\tcmp rax,rax\n\tjz {}",self.output,label.clone());
                    self.generate_scope(&if_stmt.scope);
                self.output = format!("{}\n\tlabel {}: ",self.output,label.clone());
                }
            }
            NodeStatementWhileLoop { value:while_loop } => {
                let loop_label = self.get_label();
                self.output = format!("{}\nlabel {}_entry:",self.output,loop_label);
                // Evaluate the loop condition
                self.generate_expr(&while_loop.condition);
                self.pop("rbx");
                // Jump to the exit label if the condition is false

                self.output = format!("{}\n\tcmp rax, rax\n\tjz {}_exit",self.output,loop_label);

                // Generate the loop body
                self.generate_scope(&while_loop.scope);

                // Jump back to the entry label
                self.output = format!("{}\n\tjmp {}_entry",self.output,loop_label);
                // Exit Label

                self.output = format!("{}\nlabel {}_exit:",self.output,loop_label);
            }

            NodeStatementReassign { value } => {
                self.generate_reassign(&value);
            }
            NodeStatementFunctionDefination { value } => {
                let fn_name = value.ident.value.clone().unwrap(); 
                if self.functions.get(&fn_name).is_some() {
                    GenerationError::SameIdentifiers{ident:value.ident.clone()}.error_and_exit();
                }
                self.functions.insert(fn_name.clone(),value.clone());
                let old_out = self.output().clone();
                self.output = format!("label {}:\n\t",fn_name);
                let mut arg_locs:Vec<usize> = Vec::new();

                for i in (0..value.args.len()).rev() {
                    if let Some((ident,_nodeexpr)) = value.args.get_index(i) {

                        let v = Var {stack_loc:value.args.len()-i-1,t:VarType::FuncVariable};
                        self.variables.insert(ident.value.clone().unwrap(),v);

                        arg_locs.push(i);
                        self.stack_size +=1;
                    } else {
                        GenerationError::Custom("Unknown error lol".to_string()).error_and_exit();
                    }
                }
                
                self.generate_scope(&value.scope);
                

                self.variables.retain(|_,x| !arg_locs.contains(&x.stack_loc) && x.t != VarType::FuncVariable); 
                self.output = format!("{}\n\tmov rax, 0\n\tret\n\t{}",self.output,old_out);
            }
            NodeStatementReturn { value } => {

                if value.expr.is_some() {
                    let e = value.expr.as_ref().unwrap();
               
                    self.generate_expr(&e);
                }
                self.output = format!("{}\n\t\n\tgetsp rax\n\tsub rax,1\n\ttruncstackr rcx,rax",self.output);
                self.output = format!("{}\n\tret",self.output);
            }

            NodeStatementFunctionCall { value } => {
                if self.functions.get(&value.ident.value.clone().unwrap()).is_none() {
                    GenerationError::UndeclaredIdentifier{ident:value.ident.clone()}.error_and_exit();
                }
                // store rcx as base pointer 
                // we get the arguments  of function by doing getfromstack (rcx-(argnumber*1)), rax
                self.output = format!("{}\n\tgetsp rcx\n\tsub rcx, 1",self.output);
                self.generate_expr(&NodeExpr::NodeExprFunctionCall{value:value.clone()});
            }
            _ => {}
        }
    }

    pub fn generate_expr(&mut self, expr:&NodeExpr) {
        use NodeExpr::*;
        match expr {
            NodeExprTerm { value } => {
                self.generate_term(value);
            }
            NodeExprBinExpr{ value } => {
                use BinExpr::*;
                match value {
                    BinExprAdd { lhs, rhs } => {
                        self.generate_expr(&lhs);
                        self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.pop("rax");
                        self.output = format!("{}\n\tadd rax, rbx",self.output);
                        self.push("rax");
                    }
                    BinExprSub { lhs, rhs } => {
                        self.generate_expr(&lhs);
                        self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.pop("rax");
                        self.output = format!("{}\n\tsub rax, rbx",self.output);
                        self.push("rax");
                    }
                    BinExprMult { lhs, rhs } => {
                        self.generate_expr(&lhs);
                        self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.pop("rax");                
                        self.output = format!("{}\n\tmul rax, rbx",self.output);
                        self.push("rax");
                    }
                    BinExprDiv { lhs, rhs } => {
                        self.generate_expr(&lhs);
                        self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.pop("rax"); 
                        self.output = format!("{}\n\tdiv rax, rbx",self.output);
                        self.push("rax");
                    }
                }
            }
            NodeExprBoolExpr { value } => {
                use BoolExpr::*; 
                match value {
                    BoolExprAnd { lhs, rhs } => {
                        self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.generate_expr(&lhs);
                        self.pop("rax");
                        self.output = format!("{}\n\tcmp rax, rbx\n\tgetflag rax, eqf",self.output);
                    }
                    BoolExprOr { lhs, rhs } => {
                        self.generate_expr(&lhs);
                        self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.pop("rax");
                        self.output = format!("{}\n\tcmp rax 1\n\tgetflag rax, eqf\n\t",self.output);
                        todo!();
                    }
                    BoolExprEqualTo { lhs, rhs} => {
                        self.generate_expr(&lhs);
                        self.pop("rax");
                        self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.output = format!("{}\n\tcmp rax, rbx\n\tgetflag rax, eqf\n\tcmp rax,1\n\tgetflag rax, eqf\n\t",self.output);
                        self.push("rax");
                        
                    },
                    BoolExprNotEqualTo { lhs, rhs } => {
                        self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.generate_expr(&lhs);
                        self.pop("rax");
                        self.output = format!("{}\n\tcmp rax, rbx\n\tgetflag rax, eqf\n\tcmp rax,0\n\tgetflag rax, eqf\n\t",self.output);
                        self.push("rax");
                    }
                    BoolExprLessThan { lhs, rhs } => {
 self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.generate_expr(&lhs);
                        self.pop("rax");

                        self.output = format!("{}\n\tcmp rax, rbx\n\tgetflag rax, lf\n\tcmp rax, 1\n\tgetflag rax, eqf\n\t",self.output);
                        self.push("rax");
                    }
                    BoolExprGreaterThan { lhs, rhs } => {
                       self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.generate_expr(&lhs);
                        self.pop("rax");                     

                        self.output = format!("{}\n\tcmp rax, rbx\n\tgetflag rax, gf\n\tcmp rax, 1\n\tgetflag rax, eqf\n\t",self.output);
                        self.push("rax");
                    }
                    BoolExprLessThanOrEqualTo { lhs, rhs } => {
                         self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.generate_expr(&lhs);
                        self.pop("rax");                     
                        self.output = format!("{}\n\tcmp rax, rbx\n\tgetflag rax, gf\n\tcmp rax,1\n\tgetflag rax,eqf\n\t",self.output);
                    }
                    BoolExprGreaterThanOrEqualTo { lhs, rhs } => {
                       self.generate_expr(&rhs);
                        self.pop("rbx");
                        self.generate_expr(&lhs);
                        self.pop("rax");

                        self.output = format!("{}\n\tcmp rax, rbx\n\tgetflag rax, lf\n\tcmp rax, 0\n\tgetflag rax,eqf",self.output);
                    }

                }
            }
            NodeExprFunctionCall { value } => {
                if let Some(nodefuncdef) =  self.functions.get(&value.ident.value.clone().unwrap()) {

                    let ident = value.ident.clone();
                    if value.args.len() != nodefuncdef.args.len() {
                        GenerationError::Custom(format!("Expected {:?} arguments for function {:?}, found {:?}",nodefuncdef.args.len(),ident,value.args.len())).error_and_exit();
                    }
                    for args in value.args.iter() {
                        self.generate_expr(args);
                    }
                    // store rcx as base pointer 
                    // we get the arguments  of function by doing getfromstack (rcx-(argnumber*1)), rax
                    self.output = format!("{}\n\tgetsp rcx\n\tsub rcx, 1",self.output);


                    self.output =format!("{}\n\tcall {}",self.output,ident.value.unwrap());
                }else {
                    GenerationError::UndeclaredIdentifier{ident:value.ident.clone()}.error_and_exit();
                }
            }
            _ => todo!()
        }
    }

    pub fn generate_term(&mut self, term:&NodeTerm) {
        use NodeTerm::*;
        match term {
            NodeTermIntLit { value } => {
                self.push(&value.value.clone().unwrap());
            }
            NodeTermIdent { value } => {
                if self.variables.get(&value.value.clone().unwrap()).is_none() {
                    GenerationError::UndeclaredIdentifier{ident:value.clone()}.error_and_exit();
                }
                let v = self.variables.get(&value.value.clone().unwrap()).unwrap();
                self.variable_get_data(&v.clone(),"rax");
                self.push("rax");
            }
    
            NodeTermBool{ value } => {
                self.push(
                    if value.token_type == TokenType::TRUE {"1"} else {"0"} 
                );
            }
            NodeTermParen { value } => {
                self.generate_expr(&value);
            }
        }
    }


    pub fn generate_scope(&mut self, scope:&NodeScope) {
        self.begin_scope();
        
            for stmt in scope.stmts.iter() {
            self.generate_statement(&stmt);
        }
        
        self.end_scope();
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(self.variables.len());
    }

    pub fn end_scope(&mut self) {
        // self.output = format!("{}\n\tendlabel",self.output);
        if let Some(pop) = self.scopes.pop() {
            let pop_count = self.variables.len()-pop;
            self.stack_size -= pop_count;
            dbg!(&self.stack_size);
            for _ in 0..pop_count{
                self.variables.pop();
            }
            self.output = format!("{}\n\ttruncstack {}",self.output,pop_count);

        }
    }

    pub fn generate_reassign(&mut self, reassign:&NodeReassign) {
        use NodeReassign::*;
        match reassign {
            Assign {ident,expr} => {
                self.generate_expr(&expr);
                self.pop("rax");
               if let Some(var) = self.variables.get(&ident.value.clone().unwrap()) {
                    let var = var.clone();
                    self.variable_set_data(&var);
                }else {

                    GenerationError::UndeclaredIdentifier{ident:ident.clone()}.error_and_exit();
                }
                 
            }
            Add { ident, expr } => {
                self.generate_expr(&expr);
                self.pop("rdx");
                if let Some(var) = self.variables.get(&ident.value.clone().unwrap()) {
                    let var = var.clone();
                    self.variable_get_data(&var,"rax");
                    self.output = format!("{}\n\tadd rax,rdx\n\t",self.output);
                    self.variable_set_data(&var);
                }else {

                    GenerationError::UndeclaredIdentifier{ident:ident.clone()}.error_and_exit();
                }
            }

            Sub { ident, expr } => {
               self.generate_expr(&expr);
                self.pop("rdx");
                if let Some(var) = self.variables.get(&ident.value.clone().unwrap()) {
                    let var = var.clone();
                    self.variable_get_data(&var,"rax");
                    self.output = format!("{}\n\tsub rax,rdx\n\t",self.output);
                    self.variable_set_data(&var);
                }else {

                    GenerationError::UndeclaredIdentifier{ident:ident.clone()}.error_and_exit();
                }
            }

            Mul { ident, expr } => {
                 self.generate_expr(&expr);
                self.pop("rdx");
                if let Some(var) = self.variables.get(&ident.value.clone().unwrap()) {
                    let var = var.clone();
                    self.variable_get_data(&var,"rax");
                    self.output = format!("{}\n\tmul rax,rdx\n\t",self.output);
                    self.variable_set_data(&var);
                }else {

                    GenerationError::UndeclaredIdentifier{ident:ident.clone()}.error_and_exit();
                }
            }

            Div { ident, expr } => {
                 self.generate_expr(&expr);
                self.pop("rdx");
                if let Some(var) = self.variables.get(&ident.value.clone().unwrap()) {
                    let var = var.clone();
                    self.variable_get_data(&var,"rax");
                    self.output = format!("{}\n\tdiv rax,rdx\n\t",self.output);
                    self.variable_set_data(&var);
                }else {

                    GenerationError::UndeclaredIdentifier{ident:ident.clone()}.error_and_exit();
                }
            }
        }
    }

    pub fn push(&mut self,v:&str) {
        self.output = format!("{}\n\tpush {v}",self.output);
        self.stack_size += 1;
    }

    pub fn pop(&mut self,r:&str) {
        self.output = format!("{}\n\tpop {r}",self.output);
        self.stack_size -= 1;
    }

    pub fn output(&mut self) -> &String {
        return &self.output;
    }

    pub fn get_label(&mut self) -> String {
        self.label_count += 1;
        return format!("label{}",self.label_count-1);
    }

    fn variable_get_data(&mut self, var:&Var,reg:&str) {
        match var.t {
            VarType::FuncVariable => {
                //self.output = format!("{}\n\t mov rax, rcx\n\tsub rax, {}\n\tgetfromstack rax,{reg}",self.output,var.stack_loc);

                self.output = format!("{}\n\t mov rax, rcx\n\tsub rax, {}\n\tgetfromstack rax,{reg}",self.output,var.stack_loc);
            }
            VarType::NormalVariable => {
                self.output = format!("{}\n\tgetfromsp {},{reg}",self.output,self.stack_size-1-var.stack_loc);
            }
        };
    }
    fn variable_set_data(&mut self,var:&Var)  {
        match var.t {
            VarType::FuncVariable => {
                self.output = format!("{}\n\t mov rdx, rcx\n\tsub rdx, {}\n\tsetstack rdx,rax",self.output,var.stack_loc);
            }
            VarType::NormalVariable => {
                self.output = format!("{}\n\tsetstack {}, rax",self.output,var.stack_loc);
            }
        }

    }
}

