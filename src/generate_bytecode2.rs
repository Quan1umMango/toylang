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
    current_func_ident:Option<String>, // used to check and apply return types
    current_func_bp:usize,
    function_base_pointers:Vec<usize> // stores the (relative) address of where the base point (stored normally in rcx) is stored in stack 
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
            num_scopes:0,
            current_func_ident:None,
            current_func_bp:0,
            function_base_pointers:Vec::new(),
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
                if value.expr.get_datatype(&self.parser_fundefs,&self.parser_vars) == DataType::Void {
                    GenerationError::Custom("Cannot have void as exit value".to_owned()).error_and_exit();
                }
                self.generate_expr(&value.expr);
                self.pop("rax");
                self.output = format!("{}\n\tdisplay rax\n\thalt",self.output);
            }
            NodeStatementLet { value } => {
                let ident = value.ident.clone();
                if self.variables.contains_key(ident.value.as_ref().unwrap()) {
                    GenerationError::SameIdentifiers{ident:ident.clone()}.error_and_exit();
                }    
                
                self.generate_expr(&value.expr);

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
                                self.generate_expr(expr);
                                self.pop("rbx");
                                self.try_set_ident(value.clone(),"rbx");
                            }
                            _ => unreachable!()
                       } 
                    }
                    NodeReassign::Add { ident, expr } => {
                        match ident {
                            NodeTerm::NodeTermIdent { value } => {
                                self.try_load_ident_to(&value,"rbx");
                                self.generate_expr(expr);
                                self.pop("rax");
                                self.output = format!("{}\n\tadd rbx,rax",self.output);
                                self.try_set_ident(value.clone(),"rbx");
                            }
                            _ => unreachable!()
                       } 
                    }
                    NodeReassign::Sub { ident, expr } => {
                        match ident {
                            NodeTerm::NodeTermIdent { value } => {
                                self.try_load_ident_to(&value,"rbx");
                                self.generate_expr(expr);
                                self.pop("rax");
                                self.output = format!("{}\n\tsub rbx,rax",self.output);
                                self.try_set_ident(value.clone(),"rbx");
                            }
                            _ => unreachable!()
                       } 
                    }
                    NodeReassign::Div { ident, expr } => {
                        match ident {
                            NodeTerm::NodeTermIdent { value } => {
                                self.try_load_ident_to(&value,"rbx");
                                self.generate_expr(expr);
                                self.pop("rax");
                                self.output = format!("{}\n\tdiv rbx,rax",self.output);
                                self.try_set_ident(value.clone(),"rbx");
                            }
                            _ => unreachable!()
                       } 
                    }
                    NodeReassign::Mul { ident, expr } => {
                        match ident {
                            NodeTerm::NodeTermIdent { value } => {
                                self.try_load_ident_to(&value,"rbx");
                                self.generate_expr(expr);
                                self.pop("rax");
                                self.output = format!("{}\n\tmul rbx,rax",self.output);
                                self.try_set_ident(value.clone(),"rbx");
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
                self.generate_expr(&value.expr);
                self.pop("rax");
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
                self.generate_expr(&value.condition);
                self.pop("rax");
                self.output = format!("{}\n\tcmp rax, 1\n\tjne {main_scope}",self.output);
                self.generate_scope_without_label(&value.scope);
                self.output = format!("{}\n\tjmp {loop_scope}",self.output);
                self.num_scopes +=1;
                self.output = format!("{}\nlabel {main_scope}:",self.output);
            }
            NodeStatementFunctionCall { value } => {
                self.function_base_pointers.push(self.stack_size);
                self.push("rcx");
            
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
                    self.generate_expr(&value.args[i]);
                }
                self.append_output(format!("\n\tcall {}",f.ident.value.clone().unwrap()));
		_ = self.function_base_pointers.push(self.stack_size);
            } 

            NodeStatementFunctionDefination { value } => {
                // we dont need to check it because its already there in self.parser_funcdefs
                // because of the parser
                /*if self.parser_fundefs.get(value.ident.value.as_ref().unwrap()).is_some() {

                    GenerationError::SameIdentifiers{ident:value.ident.clone()}.error_and_exit();
                } */
                self.current_func_ident = value.ident.value.clone();
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
		self.function_base_pointers.push(0);

                self.stack_size += value.args.len();
               self.output = format!("{}\nlabel {}:",self.output,value.ident.clone().value.unwrap());
               // We store where the sp currently is. We'll use this to get argument variables
               self.append_output("\n\tgetsp rcx".to_owned());
	
               self.generate_scope_without_label(&value.scope);
               // Cleanup funtion argument variables.
               // if not done, we will error if we do something  like:
               // fn myfunc(int32 x) {}
                // let x = 23; // error: cannot use same identifier
                let n =self.variables.len() - prev_var_nums;
                //self.stack_size -= n;
		self.stack_size = 0;
                for _ in 0..n{
                    let _ = self.variables.pop();
                }

                self.num_scopes +=1;
                self.append_output("\n\tret \n".to_string());
                self.append_output(op);
                self.current_func_ident = None;
		_ = self.function_base_pointers.pop();
            }
		
	    	
	
            NodeStatementReturn { value } => {
                if self.current_func_ident.is_none() {
                    GenerationError::Custom("Cannot return from non-function body.".to_owned()).error_and_exit();
                }
                let cfd = self.current_func_ident.clone().unwrap();
                if self.parser_fundefs.get(&cfd).is_none() {
                    unreachable!();
                }
                let f = self.parser_fundefs.get(&cfd);
                if f.is_none() {
               		todo!();     
                }
                let  f = f.unwrap();
                let expected_return_type =  match &f.return_type {
                    &None | &Some(DataType::Void) => DataType::Void,
                    x => x.clone().unwrap()
                };  
 		// Reset the base pointer
                //let fn_start_sp = self.function_base_pointers.pop().unwrap_or(0);
		let fn_start_sp = self.function_base_pointers[self.function_base_pointers.len()-1]; // this is always going to be zero  
		let prev_sp = self.stack_size; // points to the start of where the return value is going to be

                let return_size = expected_return_type.size();
                if let Some(v) = value.expr.as_ref() {

                    let v_datatype =v.get_datatype(&self.parser_fundefs,&self.parser_vars);
                    if v_datatype != expected_return_type {
                        GenerationError::Custom(format!("Incorrect return type for function `{cfd}`. 
                                                        \n\tExpected return type: {expected_return_type}
                                                        \n\tGot:{v_datatype}\n
                                                        ")).error_and_exit();
                    }
                    self.generate_expr(v);
                }

		let new_sp = self.stack_size; 
		self.append_output(format!("\n\tgetfromsp {}, rax", fn_start_sp+new_sp+1)); // we will store the getsp value which was push before the function wass called
               	if expected_return_type != DataType::Void {
			//let new_loc = fn_start_sp + (new_sp-cur_sp); // this location is one ahead of the getsp value which was pushed before calling the function
			let new_loc = prev_sp+return_size+1; // this location is of the getsp value which was pushed before calling the function relative to the current sp
	

			// copies the return value at the position of the getsp value which as pushed before calling the function
			// we will also reset the value of stack pointer to the callee's value
			//
			// stkcpybacksp stack_start_relative_to_sp, stack_end_relative_to_sp, move_back_relavtive_to_sp 
			self.append_output(format!("\n\tstkcpybacksp {return_size}, 0, {new_loc}"));
			//self.append_output(format!("\n\tsub rcx, {bp}\n\tsub rcx,{return_size}\n\tdisplay rcx\n\tgetfromstack rcx,rcx\n\tdisplay rcx"));
		}
		//self.append_output(format!("\n\tgetfromsp {},rcx",prev_sp-return_size-1));
		self.append_output(format!("\n\tmov rcx,rax\n\ttruncstack {}",dbg!(new_sp)-(return_size+1)+2));
                self.append_output("\n\tret".to_owned());

		// clean up stack 
		self.stack_size -= return_size;	
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

    pub fn generate_expr(&mut self,expr:&NodeExpr) {
        match expr {
            NodeExpr::NodeExprTerm { value } => {
                self.generate_term(value,);
           }
            NodeExpr::NodeExprBinExpr { value } => {
                match value {
                    BinExpr::BinExprAdd { lhs, rhs } => {
                        self.generate_expr(&**lhs);
                        self.generate_expr(&**rhs);
                        self.pop("rbx");
                        self.pop("rdx");
                        self.output = format!("{}\n\tadd rdx, rbx",self.output);
                        self.push("rdx");
                    }
                    BinExpr::BinExprSub { lhs, rhs } => {
                        self.generate_expr(&**lhs);
                        self.generate_expr(&**rhs);
                        self.pop("rbx");
                        self.pop("rdx");
                        self.output = format!("{}\n\tsub rdx, rbx",self.output);
                        self.push("rdx")
                    }
                    BinExpr::BinExprMult { lhs, rhs } => {
                        self.generate_expr(&**lhs);
                        self.generate_expr(&**rhs);
                        self.pop("rbx");
                        self.pop("rdx");
                        self.output = format!("{}\n\tmul rdx, rbx",self.output);
                        self.push("rdx");
                    }
                    BinExpr::BinExprDiv { lhs, rhs } => {
                        self.generate_expr(&**lhs);
                        self.generate_expr(&**rhs);
                        self.pop("rbx");
                        self.pop("rdx");
                        self.output = format!("{}\n\tdiv rdx, rbx",self.output);
                        self.push("rdx");

                    }

                }
            }
            NodeExpr::NodeExprBoolExpr { value } => {
                match value {
                    BoolExpr::BoolExprEqualTo { lhs, rhs } => {
                        self.generate_expr(&**lhs);
                        self.pop("rdx");
                        self.generate_expr(&**rhs);
                        self.pop("rbx");
                        self.append_output(format!("\n\tcmp rdx, rbx\n\tgetflag rax,eqf\n\t"));
                        self.push("rax");
                    }
                    BoolExpr::BoolExprNotEqualTo { lhs, rhs } => {
                        self.generate_expr(&**lhs);
                        self.pop("rdx");
                        self.generate_expr(&**rhs);
                        self.pop("rbx");
                        self.append_output(format!("\n\tcmp rdx, rbx\n\tgetflag rax,eqf\n\tnot rax\n\t"));
                        self.push("rax");
                    }
                    BoolExpr::BoolExprLessThanOrEqualTo { lhs, rhs } => {
                        self.generate_expr(&**lhs);
                        self.pop("rdx");
                        self.generate_expr(&**rhs);
                        self.pop("rbx");
                        self.append_output(format!("\n\tcmp rdx, rbx\n\tgetflag rax,gf\n\tcmp rax, 0\n\tgetflag rax,eqf\n\t"));
                        self.push("rax");
                    }
                    BoolExpr::BoolExprGreaterThanOrEqualTo { lhs, rhs } => {
                        self.generate_expr(&**lhs);
                        self.pop("rdx");
                        self.generate_expr(&**rhs);
                        self.pop("rbx");
                        self.append_output(format!("\n\tcmp rdx, rbx\n\tgetflag rax,lf\n\tcmp rax, 0\n\tgetflag rax,eqf\n\t"));
                        self.push("rax");
                    } 
                    BoolExpr::BoolExprLessThan { lhs, rhs } => {
                        self.generate_expr(&**lhs);
                        self.pop("rdx");
                        self.generate_expr(&**rhs);
                        self.pop("rbx");
                        self.append_output(format!("\n\tcmp rdx, rbx\n\tgetflag rax,lf\n\tcmp rax, 1\n\tgetflag rax,eqf\n\t"));
                        self.push("rax");
                    }
                    BoolExpr::BoolExprGreaterThan { lhs, rhs } => {
                        self.generate_expr(&**lhs);
                        self.pop("rdx");
                        self.generate_expr(&**rhs);
                        self.pop("rbx");
                        self.append_output(format!("\n\tcmp rdx, rbx\n\tgetflag rax,gf\n\tcmp rax, 1\n\tgetflag rax,eqf\n\t"));
                        self.push("rax");
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
                
               self.push("rcx");
               self.function_base_pointers.push(self.stack_size);


               for i in 0..value.args.len() {
                    let expected_arg_type = f.args[i].get_datatype(&self.parser_fundefs,&self.parser_vars);
                    let got_arg_type = value.args[i].get_datatype(&self.parser_fundefs,&self.parser_vars);
                    if got_arg_type != expected_arg_type && expected_arg_type != DataType::Infer {
                        GenerationError::Custom(
                            format!("Function {} required argument of type {expected_arg_type} for argument number {}. Found argument of type {got_arg_type}. "
                                    ,f.ident.clone().value.unwrap(),i)).error_and_exit();
                    }
                    self.generate_expr(&value.args[i]);
               }
               self.append_output(format!("\n\tcall {}",f.ident.value.clone().unwrap()));
		_ = self.function_base_pointers.pop();
		self.stack_size -= 1; // remove the previously pushe rcx

            }
            _ => todo!()
        }
    } 

    pub fn generate_term(&mut self, term:&NodeTerm) {
        match term {
            NodeTerm::NodeTermIntLit { value } => {
                self.output = format!("{}\n\tmov rax,{}",self.output,value.value.clone().unwrap());
               self.push("rax");
            }
            NodeTerm::NodeTermIdent { value } => {
               self.try_load_ident_to(value,"rax");
               self.push("rax");
            }
            NodeTerm::NodeTermBool { value } => {
                self.append_output(format!("\n\tmov rax, {}",if value.token_type == TokenType::TRUE { "1" } else {"0"}));
               self.push("rax");
            }
            NodeTerm::NodeTermArray { value } => {
                match value {
                    ArrayTerm::DefaultValues { ty:_, values } => {
                        self.push(values.len().to_string().as_str());
                        for val in values.iter() {
                            self.generate_expr(&**val);
                        }
                    }
                    _ => todo!()
                }
            }
            NodeTerm::NodeTermParen { value } => {
                self.generate_expr(&**value);
            }
            _ => {
                dbg!(term);
                todo!();
            } 
        }
    }

    // sets the identifier vvalue to the specified register
    pub fn try_set_ident(&mut self, ident:Token, reg:&str) {
        if let Some(v) = self.variables.get(ident.value.as_ref().unwrap()) {
            match v.var_type {
                VarType::Normal => self.output = format!("{}\n\tmov rax, rcx\n\tadd rax, {}\n\tsetstack rax,{reg}",self.output,v.stack_loc-1),
                VarType::FuncArg => {
                    self.append_output(format!("\n\tmov rax, rcx\n\tsub rax, {}\n\tsetstack rax,{reg}",v.stack_loc+1))
                }
            }
        }else {
            GenerationError::UndeclaredIdentifier{ident:ident.clone()}.error_and_exit();
        }
    }

    pub fn try_load_ident_to(&mut self, ident:&Token, reg:&str) {
        if let Some(v) = self.variables.get(ident.value.as_ref().unwrap()) {
            match v.var_type {

                VarType::Normal => self.output = format!("{}\n\tmov rax, rcx\n\tadd rax,{}\n\tgetfromstack rax,{reg}",self.output,v.stack_loc-1),
                VarType::FuncArg => {
                    self.append_output(format!("\n\tmov rax, rcx\n\tsub rax, {}\n\tgetfromstack rax, {reg}",v.stack_loc+1))
                }
            }
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

    pub fn error_if_ident_exists(&self, ident:&Token) {

        if self.parser_fundefs.get(ident.value.as_ref().unwrap()).is_none() {
            GenerationError::UndeclaredIdentifier{ident:ident.clone()}.error_and_exit();
        } 
        if self.variables.get(ident.value.as_ref().unwrap()).is_none() {
            GenerationError::UndeclaredIdentifier{ident:ident.clone()}.error_and_exit();
        } 
    }

}
