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
    local_variables: Vec<(Token,VarType)>,
    local_variables_base_pointer:Vec<usize>,
    parser_fundefs: HashMap<String,NodeFunctionDefination>,
    parser_vars:HashMap<String,DataType>,
}

#[derive(Debug,Clone,PartialEq,Default)]
enum VarType {
    Array {
        len:usize,
    }, 
    #[default]
    Normal,
    Slice,
}

#[derive(Debug,Clone,PartialEq)]
enum ScopeType {
    FuncArgument, // for function params
    NormalVariable,
}

#[derive(Debug,Clone)]
pub struct Var {
    stack_loc:usize,
    t: ScopeType,
    dt:DataType,
    var_type: VarType,
}


impl GeneratorVM {
    pub fn new(node_program: NodeProgram,parser_fundefs:HashMap<String,NodeFunctionDefination>,parser_vars:HashMap<String,DataType>) -> GeneratorVM {
        return GeneratorVM {
            node_program,output:"".to_string(),
            stack_size: 0,
            variables:IndexMap::new(),
            scopes:Vec::new(),
            label_count:0,
            functions:HashMap::new(),
            local_variables:Vec::new(),
            local_variables_base_pointer:Vec::new(),
            parser_fundefs,parser_vars
        };
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
                let var_type = match &value.expr {
                    NodeExpr::NodeExprTerm { value } => {
                        match value {
                            NodeTerm::NodeTermArray { value:av } => {
           //                     self.push(av.len().unwrap_or(0).to_string().as_str());
                                VarType::Array {len:0}
                            }
                        _ => VarType::default()
                        }
                    }
                    _ => VarType::default()
                };
                let mut v = Var {stack_loc:self.stack_size,t:ScopeType::NormalVariable,dt:value.ident.datatype.clone().unwrap_or(DataType::Infer),var_type};
                self.generate_expr(&value.expr);
                match v.var_type {
                    VarType::Array { ref mut len } => {
                        *len = self.stack_size-v.stack_loc;
                    }
                    _ => ()
                }
                self.local_variables.push((value.ident.clone(),v.var_type.clone()));
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
                let old_stack_size = self.stack_size;
                for i in (0..value.args.len()).rev() {
                    if let Some((ident,nodeexpr)) = value.args.get_index(i) {
                       let var_type = match nodeexpr.get_datatype(&self.parser_fundefs,&self.parser_vars) {
                           DataType::Array(_,len) => {
                               VarType::Array {
                                   len
                               }
                           }
                           DataType::Slice { ty:_ } => {
                               VarType::Slice
                           }
                           _ => VarType::Normal,
                       };
                       let v = Var {stack_loc:value.args.len()-i-1,t:ScopeType::FuncArgument,dt:ident.datatype.clone().unwrap(),var_type};
                        self.variables.insert(ident.value.clone().unwrap(),v);

                        arg_locs.push(i);
                        self.stack_size +=1;
                    } else {
                        GenerationError::Custom("Unknown error lol".to_string()).error_and_exit();
                    }
                }
                self.generate_scope(&value.scope);
                self.stack_size = old_stack_size;

                self.variables.retain(|_,x| !arg_locs.contains(&x.stack_loc) && x.t != ScopeType::FuncArgument); 
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
                self.generate_expr(&NodeExpr::NodeExprFunctionCall{value:value.clone()});
            }
            NodeStatementBuiltin { value } => self.generate_builtin(value)
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
            NodeExprBuiltin { value } => {
                self.generate_builtin(value);
            }
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
                //todo!("FIX THIS, MAKE IT SO THAT IT CHECKS IF ITS A ARRAY AND IF IT IS THEN IT PUSHS THE LOCATION OF ARRAY");
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

            NodeTermArray { value } => {
                self.generate_array(&value);
            }
            
            NodeTermChar { value } => {
                self.push((value.clone().value.unwrap().parse::<char>().unwrap() as u8).to_string().as_str());
            }
            NodePointer { value } => {
                match *value.clone() {
                    NodeTermIdent { value } => {
                        if let Some(s) = self.variables.get(&value.value.clone().unwrap()) {
                            self.push(&format!("{}",s.stack_loc)); 
                        }else{
                            todo!();
                        }
                    }
                    _ => unimplemented!("Ony useable in identifiers")
                }
            }
            NodeSlice { value } => {
                self.generate_array(&value);
                let ident = value.ident();
                if ident.is_some() {
                    if let Some(s) = self.variables.get(&ident.unwrap().value.unwrap()){
                        self.push(&format!("{}",s.stack_loc)); 
                    }else{
                        todo!();
                    }
                }else {
                    let loc = self.variables.last().unwrap().1.stack_loc;
                    self.push(&format!("{}",loc));
                }
            }
            NodeTermBuiltinFunc { value } => self.generate_builtin(value),
            NodeTermDeref { value } => {
               self.generate_term(&**value);
               self.pop("rax");
               self.output = format!("{}\n\tgetfromstack rax, rax",self.output);
               self.push("rax");
            } 
        }
    }


    pub fn generate_array(&mut self, array:&ArrayTerm) {
        use ArrayTerm::*;
        match array {
            DefaultValues { ty:_, values } => {
                self.push(values.len().to_string().as_str());
                for v in values.iter() {
                    self.generate_expr(v);
                }
            }
            ArrayBuilder { init_value, len } => {
                self.generate_expr(init_value);
                self.pop("rbx");
                self.generate_expr(len);
                self.pop("rax");
                self.stack_size += {
                    match *len.clone() {
                        NodeExpr::NodeExprTerm{value} => {
                            match value {
                                NodeTerm::NodeTermIntLit {value:token} => {
                                  token.value.unwrap().parse::<usize>().unwrap()
                                }
                                _ => unimplemented!("Use integer literal instead. {:?}",value),
                            }
                        }
                        _ => unimplemented!("Use an integer literal instead"),
                    }
                };
                self.output = format!("{}\n\textendstack rax, rbx",self.output);
                //self.output = format!("{}\n\tlabel {label_name}:\n\tpush rbx\n\tsub rax, 1\n\t cmp rax,0 jne {label_name} ",self.output);
            }
            ArrayIndexer {value, ty, index} => {
                //self.generate_term(value);
                
                if let Some( ref array_var) = self.variables.get(&value.value.clone().unwrap()).cloned() {
                    dbg!(array_var);
                    let len = {
                        match array_var.var_type {
                            VarType::Array { len } => {
                                Some(len)
                            }
                            _ => {
                                let range = *self.local_variables_base_pointer.last().unwrap()..;
                                if let Some(s) = self.local_variables[range]
                                    .to_vec().iter()
                                    .filter(|x| x.0.value == value.value).collect::<Vec<_>>().get(0) {
                                        match s.1 {
                                            VarType::Array { len } => {
                                                Some(len) 
                                            }
                                            _ => unreachable!()
                                        }
                                    }else {
                                        None  
                                            // todo
                                    }
                            } 
                        }
                    };
                    self.generate_expr(&index);
                    self.pop("rbx");
                    match *index.clone(){
                        NodeExpr::NodeExprTerm { value } => {
                            match value {
                                NodeTerm::NodeTermIntLit { value } => {
                                    if len.is_none() {
                                        // get the pointed location -1 to get size of array
                                        //self.load_array_loc(&array_var);
                                        let label = self.get_label();
                                        self.output = format!("{}\n\tmov rax, 0\n\tadd rax, {}\n\tgetfromsp rax,rax\n\tsub rax,2\n\tgetfromstack rax,rax
                                        \n\tcmp rax,{}\n\t
                                        jg {label}
                                        \n\t @loadstringn(\"Attempted to index out of array slice.\")\n\t pop rax \n\t write rax \n\thalt\n\t   label {label}:\n\tmov rbx, rax 
                                                             ",self.output,array_var.stack_loc,value.value.unwrap().to_string()); 
                                    }else {
                                        if let Some(v) = value.value.unwrap().parse::<usize>().ok() {
                                            let len = len.unwrap();
                                            if v >= len {
                                                GenerationError::Custom(format!("Cannot index {:?} in a array of length {:?}",v,len)).error_and_exit();
                                            }

                                        }else {
                                            GenerationError::Custom("Index supplied was not a whole number.".to_string()).error_and_exit()
                                        }
                                    }
                                }
                                NodeTerm::NodeTermIdent { value:_ } => {
                                    self.generate_term(&value);
                                    self.pop("rax");
                                    let label = self.get_label();
                                    self.output = format!("{} cmp rax, {}\n\t jl {label}\n\t @loadstringn(\"Attempted to index out of array with lenght {}\")\n\t pop rax \n\t write rax \n\thalt\n\t   label {label}:",self.output,len.unwrap_or(0),len.unwrap_or(0));
                                }
                            _ => unreachable!()
                            }
                        }
                        _ => todo!()
                    }
                    self.array_get_index_stack_location(&array_var,"rbx","rbx");
                    match ty {
                        ArrayIndexerType::Lhs => {
                            self.push("rbx");
                        }
                        ArrayIndexerType::Rhs => {
                            // do nothing
                        }
                    }
                    self.output = format!("{}\n\tgetfromstack rbx, rax",self.output);
                    self.push("rax");

                }else {
                    GenerationError::Custom(format!("Array with name {:?} not found.",
                                                    value.value.clone().unwrap())).error_and_exit();
                }
            }
            _ => todo!()
        }

    
    }

pub fn generate_scope(&mut self, scope:&NodeScope) {
    self.local_variables_base_pointer.push(self.local_variables.len());
    self.begin_scope();

    for stmt in scope.stmts.iter() {
        self.generate_statement(&stmt);
    }
    self.local_variables.drain(self.local_variables_base_pointer.pop().unwrap()..);
    self.end_scope();
}

    pub fn begin_scope(&mut self) {
        self.scopes.push(self.variables.len());
    }

    pub fn end_scope(&mut self) {
        // self.output = format!("{}\n\tendlabel",self.output);
        if let Some(pop) = self.scopes.pop() {
            let pop_count = self.variables.len()-pop;
        //    self.stack_size -= pop_count;
            for _ in 0..pop_count{
                self.variables.pop();
            }
            //self.output = format!("{}\n\ttruncstack {}",self.output,pop_count);

        }
    }

    pub fn generate_reassign(&mut self, reassign:&NodeReassign) {
        use NodeReassign::*;
        match reassign {
            Assign {ident,expr} => {
                self.generate_expr(&expr);
                self.pop("rdx");
                if let Some(var) = self.variables.clone().get(&ident.ident().unwrap().value.clone().unwrap()) {
                    if ident.is_deref(){
                        self.variable_get_data(&var,"rax");
                        self.output = format!("{}\n\tsetstack rax, rdx",self.output);
                    }else {
                        let var = var.clone();
                        self.variable_set_data(&var);
                    }
                }else {
                    GenerationError::UndeclaredIdentifier{ident:ident.ident().unwrap().clone()}.error_and_exit();
                }

            }
            Add { ident, expr} => {
                self.generate_expr(&expr);
                self.pop("rdx");
                
                if let Some(var) = self.variables.clone().get(&ident.ident().unwrap().value.clone().unwrap()) {

                    let var = var.clone();
                    self.variable_get_data(&var,"rax");
                    self.output = format!("{}\n\tadd rax,rdx\n\t",self.output);
                    self.variable_set_data(&var);
                }else {

                    GenerationError::UndeclaredIdentifier{ident:ident.ident().unwrap().clone()}.error_and_exit();

                }
            }

            Sub { ident, expr} => {
               self.generate_expr(&expr);
                self.pop("rdx");
                if let Some(var) = self.variables.clone().get(&ident.ident().unwrap().value.clone().unwrap()){
                    let var = var.clone();
                    panic!("SOMETHINGS WRONG HERE LPOl");
                    self.variable_get_data(&var,"rax");
                    self.output = format!("{}\n\tsub rax,rdx\n\t",self.output);
                    self.variable_set_data(&var);
                }else {

                    GenerationError::UndeclaredIdentifier{ident:ident.ident().unwrap().clone()}.error_and_exit();
                }
            }

            Mul { ident, expr} => {
                 self.generate_expr(&expr);
                self.pop("rdx");
                if let Some(var) = self.variables.clone().get(&ident.ident().unwrap().value.clone().unwrap()){
                    let var = var.clone();
                    self.variable_get_data(&var,"rax");
                    self.output = format!("{}\n\tmul rax,rdx\n\t",self.output);
                    self.variable_set_data(&var);
                }else {

                    GenerationError::UndeclaredIdentifier{ident:ident.ident().unwrap().clone()}.error_and_exit();
                }
            }

            Div { ident, expr} => {
                 self.generate_expr(&expr);
                self.pop("rdx");
                if let Some(var) = self.variables.clone().get(&ident.ident().unwrap().value.clone().unwrap()){
                    let var = var.clone();
                    self.variable_get_data(&var,"rax");
                    self.output = format!("{}\n\tdiv rax,rdx\n\t",self.output);
                    self.variable_set_data(&var);
                }else {

                    GenerationError::UndeclaredIdentifier{ident:ident.ident().unwrap().clone()}.error_and_exit();
                }
            }
            ArrayReassign { value, array} => {
                self.generate_expr(&array);
                self.pop("rbx"); // gets the value
                self.pop("rdx"); //gets the index 
                match *value.clone() {
                    Add { ident:_, expr} => {
                        self.generate_expr(&expr);
                        self.pop("rax");
                        self.output = format!("{}\n\tadd rax, rbx",self.output);

                        self.output = format!("{}\n\tsetstack rdx, rax",self.output);
                    }

                    Assign { ident:_, expr} => {
                        self.generate_expr(&expr);
                        self.pop("rax");
                        self.output = format!("{}\n\tsetstack rdx, rax",self.output);
                    }
                     _ => todo!()
                }
            }
        }
    }

pub fn generate_builtin(&mut self,builtin:&NodeBuiltin) {
    match builtin.value {
        BuiltinType::SizeOf => {
           
            let expr_type = builtin.expr.get_datatype(&self.parser_fundefs,&self.parser_vars);
            match expr_type {
                DataType::Slice { .. } => {
                   self.generate_expr(&*builtin.expr);
                   self.pop("rax");
                   self.output = format!("{}\n\tsub rax,1\n\tgetfromstack rax,rax\n\t",self.output);
                   self.push("rax");
                   
                } 
                _ => self.push(expr_type.size().to_string().as_str())
            }
        }
        BuiltinType::Print => {
            let expr_type =builtin.expr.get_datatype(&self.parser_fundefs,&self.parser_vars);
            match expr_type {
                DataType::Array(ty,len) => {
                    let len = len -1;
                   if *ty != DataType::Char {
                    GenerationError::Custom(format!("Expected a string in builtin function @print")).error_and_exit();
                   }
                   self.generate_expr(dbg!(&*builtin.expr));
                   self.pop("rax");
                   self.output = format!("{} write {},{}",self.output,len+1,self.stack_size);
                }
                DataType::Slice{..} => {
                    self.generate_expr(&*builtin.expr);
                    self.pop("rax");
                    self.output = format!("{}\n\tmov rbx, rax\n\tsub rbx,1 \n\tgetfromstack rbx,rbx\n\tadd rbx, rbx\n\twrite rax,rbx",self.output);
                }
                DataType::Pointer { ty } => {

                }
                DataType::Bool | DataType::Int32 => {
                    self.generate_expr(&*builtin.expr);
                    self.pop("rax");
                    self.output = format!("{}\n\tdisplay rax",self.output);
                }
                _ => {

                }
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
        match &var.t {
            ScopeType::FuncArgument => {
                //self.output = format!("{}\n\t mov rax, rcx\n\tsub rax, {}\n\tgetfromstack rax,{reg}",self.output,var.stack_loc);

                self.output = format!("{}\n\t mov rax, rcx\n\tsub rax, {}\n\tgetfromstack rax,{reg}",self.output,var.stack_loc);
            }
            ScopeType::NormalVariable => {
                match &var.dt {
                    DataType::Array(..) => {
                        self.output = format!("{}\n\tmov rax,{}",self.output,self.stack_size-1-var.stack_loc);
                    }
                    _ => self.output = format!("{}\n\tgetfromsp {},{reg}",self.output,self.stack_size-1-var.stack_loc)
                }
            }
        };
    }
    fn variable_set_data(&mut self,var:&Var)  {
        match var.t {
            ScopeType::FuncArgument => {
            
                self.output = format!("{}\n\t mov rdx, rcx\n\tsub rdx, {}\n\tsetstack rdx,rax",self.output,var.stack_loc);
            }
            ScopeType::NormalVariable => {
                self.output = format!("{}\n\tsetfromsp {}, rax",self.output,self.stack_size-var.stack_loc);
            }
        }
    }

    // Gets the abosolute stack location of theindex
    fn array_get_index_stack_location(&mut self,var:&Var, index_reg:&str,dest:&str) {
       self.output = format!("{}\n\tadd {index_reg},{}\n\tmov {dest},{index_reg}",self.output,var.stack_loc+1);
    }

    fn load_array_loc(&mut self, var:&Var) {
        self.output = format!("{}\n\t",self.output,);
    }
}

