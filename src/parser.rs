use crate::*;
use indexmap::IndexMap;
use std::collections::HashMap;

#[derive(Debug,Copy,Clone,PartialEq)]
enum ParsingState {
    Idle,
    ParsingLet,
    ParsingFuncDef,
    ParsingFuncCall,
    ParsingExit,
    ParsingWhile,
    ParsingIf,
}

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    pub vars: HashMap<String,DataType>,
    pub  functions: HashMap<String,NodeFunctionDefination>,
    state:ParsingState,
}


#[derive(Debug,Clone)]
pub enum NodeTerm {
    NodeTermIntLit{value:Token},
    NodeTermChar { value: Token},
    NodeTermIdent{value:Token},
    NodeTermParen{value:Box<NodeExpr>},
    NodeTermBool{value:Token},
    NodeTermArray { value: ArrayTerm },
    NodePointer {value:Box<NodeTerm>},
    NodeSlice{value:ArrayTerm},
    NodeTermBuiltinFunc{value:NodeBuiltin},
    NodeTermDeref { value: Box<NodeTerm> },
}

impl NodeTerm {
    pub fn get_datatype(&self,vars:&HashMap<String,DataType>,functions:&HashMap<String,NodeFunctionDefination>) -> DataType {
        match self {
            NodeTerm::NodeTermIdent { value } => {
                let ident =value.value.clone().unwrap();
                if let Some(dt) = vars.get(&ident) {
                    return dt.clone() 
                }else {
                    ParsingError::UndeclaredIdentifier{ident:value.clone()}.error_and_exit();
                }
                DataType::Void
            }
            NodeTerm::NodeTermBool { value:_ }  => DataType::Bool,
            NodeTerm::NodeTermIntLit { value:_ } => DataType::Int32,
            NodeTerm::NodeTermParen { value } => value.get_datatype(functions,vars),
            NodeTerm::NodeTermArray{value} => {
                match value {
                    ArrayTerm::ArrayIndexer { value, .. } => { 
                        if let Some(v) = vars.get(&value.value.clone().unwrap()) {
                          match v {
                              DataType::Array(dt,_) => dbg!(*dt.clone()),
                              _ => unreachable!()
                          } 
                        }else {
                           unimplemented!() 
                        }
                    },
                    _ => value.get_datatype(vars,functions)
                }
            } 
            NodeTerm::NodeTermChar {..} => DataType::Char,
            NodeTerm::NodePointer { value } => DataType::Pointer{ty:Box::new(value.get_datatype(vars,functions))},
            NodeTerm::NodeSlice { value } => DataType::Slice {
                ty: Box::new(value.get_datatype(vars,functions))    
            },
            NodeTerm::NodeTermBuiltinFunc { value } => value.value.get_return_type().unwrap(),
           NodeTerm::NodeTermDeref { value } => {
                let dt = value.get_datatype(vars,functions);

                match dt {
                    DataType::Pointer { ty } => {
                       *ty 
                    }
                    _ => {

                        ParsingError::Custom(format!("Cannot derefernce type {}. \nThe object must be a pointer to derefernce.",dt)).error_and_exit();
                        DataType::Void
                    }
                }

            }
        }
    }

    pub fn ident(&self) -> Option<Token> {
        match self {
            NodeTerm::NodeTermIdent { value } => Some(value.clone()),
            NodeTerm::NodeTermDeref { value } => value.ident(),
            _ => None
        }
    }
    // TODO: Make a result type for this.
    pub fn try_set_dt(&mut self, dt:DataType) -> Option<()> {
        match self {
            NodeTerm::NodeTermIdent { value } => value.datatype = Some(dt),
            _ => return None
        }
        Some(())
    }

    pub fn is_deref(&self) -> bool {
        match self {
            NodeTerm::NodeTermDeref { .. } => true,
            _ => false,
        }
    }
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


#[derive(Debug,Clone,Default)]
pub enum ArrayIndexerType {
    Lhs,
    #[default]
    Rhs
}
#[derive(Debug,Clone)]
pub enum ArrayTerm {
    DefaultValues{ty:DataType,values:Vec<Box<NodeExpr>>}, // [1,2,3,4];
    ArrayBuilder{init_value:Box<NodeExpr>,len:Box<NodeExpr>}, // [0;1000];
    ArrayIndexer{value:Token, index:Box<NodeExpr>,ty:ArrayIndexerType}, // arr[index];
    ArraySlice{ty:DataType} // &arr which is of type &[dt],
}

impl ArrayTerm {
    pub fn get_datatype(&self,vars:&HashMap<String,DataType>,functions:&HashMap<String,NodeFunctionDefination>) -> DataType {
        use ArrayTerm::*;
        match self {
            DefaultValues { ty, .. } =>ty.clone() ,
            ArrayBuilder { init_value, .. } => {
                DataType::Array(Box::new(init_value.get_datatype(functions,vars)),0)
            }
            ArrayIndexer { value, ..} => {
                vars.get(&value.value.clone().unwrap()).unwrap_or(&DataType::Infer).clone()
            }
            ArraySlice { ty } => ty.clone()
        }
    }

    pub fn ident(&self) -> Option<Token> {
        use ArrayTerm::*;
        match self {
            DefaultValues {.. } | ArrayBuilder {..} => None,
            ArrayIndexer { value, .. } => {
                Some(value.clone())     
            }
            ArraySlice{ty} => None
        }
    }

    pub fn len(&self) -> Option<usize> {
        match self {
            Self::DefaultValues { ty:_, values } => Some(values.len()),
            Self::ArrayBuilder { init_value:_, len} => {
                match *len.clone() {
                    NodeExpr::NodeExprTerm { value } =>{
                        match value {
                            NodeTerm::NodeTermIntLit { value } => {
                                return value.value.unwrap().parse::<usize>().ok();
                            }
                            _ => unimplemented!()
                        }
                    }
                    _ => unimplemented!()
                }
            }
            _ => None
        }
    }
}

#[derive(Debug,Clone)]
pub enum NodeExpr {   
    NodeExprTerm{value:NodeTerm},
    NodeExprBinExpr{value:BinExpr},
    NodeExprBoolExpr{value:BoolExpr},
    NodeExprFunctionCall{value:NodeFunctionCall},
    NodeExprBuiltin{value:NodeBuiltin},
}

impl NodeExpr {
    pub fn get_datatype(&self,functions:&HashMap<String,NodeFunctionDefination>,vars:&HashMap<String,DataType>) -> DataType {
        match self {
            NodeExpr::NodeExprTerm { value } => value.get_datatype(vars,functions),
            NodeExpr::NodeExprBinExpr { value:_ } => DataType::Int32,
            NodeExpr::NodeExprBoolExpr { value:_ } => DataType::Bool,
            NodeExpr::NodeExprFunctionCall { value } => {
                let ident = value.ident.value.clone().unwrap();
                if let Some(nodefuncdef) = functions.get(&ident) {
                    return nodefuncdef.clone().return_type.unwrap_or(DataType::Void);
                }else {
                    let loc = value.ident.token_location;
                    ParsingError::Custom(format!("Undefined function {:?} at {}:{}",ident,loc.0,loc.1)).error_and_exit();
                    DataType::Void
                }
            }
            NodeExpr::NodeExprBuiltin { value } => value.value.get_return_type().unwrap_or(DataType::Void),
        }
    }
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
pub struct NodeExit {
    pub expr: NodeExpr,
}

#[derive(Debug,Clone)]
pub struct NodeLet {
    pub ident: Token,
    pub expr: NodeExpr,
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
pub struct NodeFunctionCall {
    pub ident: Token,
    pub args: Vec<NodeExpr>,
}

#[derive(Debug,Clone)] 
pub struct NodeFunctionDefination {
    pub ident: Token,
    pub args: IndexMap<Token,NodeExpr>,
    pub return_type: Option<DataType>,
    pub scope: NodeScope,
}

#[derive(Debug,Clone)]
pub struct NodeReturn {
    pub expr: Option<NodeExpr>,
}

#[derive(Debug,Clone)]
pub enum NodeReassign {
    Assign{ident:NodeTerm,expr:NodeExpr},
    Add{ident:NodeTerm,expr:NodeExpr},
    Sub{ident:NodeTerm,expr:NodeExpr},
    Mul{ident:NodeTerm,expr:NodeExpr},
    Div{ident:NodeTerm,expr:NodeExpr},
    ArrayReassign{value:Box<NodeReassign>,array:NodeExpr},
}

#[derive(Debug,Clone)]
pub struct NodeScope {
    pub stmts:Vec<NodeStatement>
}

#[derive(Debug,Clone)]
pub enum BuiltinType {
    /// Gets the size it occupies on the stack. size is calulated in how many stack locations it occupies. 
    /// int32, bool, char,ptr and slice takes 1 stack location, array of n elements takes n + 1 stack
    /// locations (because the -1 index stores length of array) (may change in the future)
    /// structs take the sum of all the size of the fields.
    SizeOf, 
    /// Takes in a string, prints to the screen
    Print,
}

impl BuiltinType {
    pub fn get_return_type(&self) -> Option<DataType> {
        match self {
            BuiltinType::SizeOf => Some(DataType::Int32),
            BuiltinType::Print => Some(DataType::Void),
        }
    }

    pub fn from_str(s:&str) -> Option<BuiltinType> {
        match s {
            "sizeof" => Some(BuiltinType::SizeOf),
            "print" => Some(BuiltinType::Print),
            _ => unimplemented!()
        }
    }
}

#[derive(Debug,Clone)]
pub struct NodeBuiltin {
    pub value:BuiltinType,
    pub expr: Box<NodeExpr>
}

#[derive(Debug,Clone)]
pub enum NodeStatement{
    NodeStatementExit{ value:NodeExit},
    NodeStatementLet{ value:NodeLet},
    NodeStatementScope{value:NodeScope},
    NodeStatementIf{value:NodeIf},
    NodeStatementReassign{value:NodeReassign},
    NodeStatementWhileLoop{value:NodeWhileLoop},
    NodeStatementFunctionDefination{value: NodeFunctionDefination},
    NodeStatementFunctionCall{value: NodeFunctionCall},
    NodeStatementReturn{value:NodeReturn},
    NodeStatementBuiltin {value:NodeBuiltin}
}

pub struct NodeProgram {
    pub stmts: Vec<NodeStatement>
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, index: 0, vars:HashMap::new(), functions: HashMap::new(), state:ParsingState::Idle }
    }


    pub fn parse_type(&mut self) -> Option<DataType> {
        if self.try_consume(TokenType::TYPE_CHAR).is_some() {
            Some(DataType::Char)
        }else if self.try_consume(TokenType::TYPE_INT32).is_some() {
            Some(DataType::Int32)
        }else  if self.try_consume(TokenType::FALSE).is_some() || self.try_consume(TokenType::TRUE).is_some() {
            Some(DataType::Bool)
        } 
        // Array 
        else if self.try_consume(TokenType::OPEN_SQUARE).is_some() {
            if let Some(t) = self.parse_type(){
                if self.try_consume(TokenType::SEMICOLON).is_none() {
                    let loc = self.peek_token().unwrap().token_location;
                    ParsingError::ExpectedTokenNotFound{expected_token:"';'".to_string(),loc}.error_and_exit();
                }
                if let Some(len) = self.try_consume(TokenType::NUM) {
                    if self.try_consume(TokenType::CLOSE_SQUARE).is_none() {
                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::ExpectedTokenNotFound{expected_token:"Close Paren ']'".to_string(),loc}.error_and_exit();
                    }
                    return Some(DataType::Array(Box::new(t),len.value.unwrap().parse::<usize>().unwrap()))
                }else {
                    let loc = self.peek_token().unwrap().token_location;
                    ParsingError::Custom(format!("Expected a lenght of array at {}:{}",loc.0,loc.1)).error_and_exit();
                    None
                }
            }else {
                let loc = self.peek_token().unwrap().token_location;
                ParsingError::Custom(format!("Expected a data type at {}:{}",loc.0,loc.1)).error_and_exit();
                None
            }
        }
        else if self.try_consume(TokenType::AMPERSAND).is_some() {
            // Check if its a slice or array pointer
            if self.try_consume(TokenType::OPEN_SQUARE).is_some() {
                if let Some(dt) = self.parse_type() {

                    if self.try_consume(TokenType::SEMICOLON).is_some(){
                        // it is an array 
                        if let Some(len) = self.try_consume(TokenType::NUM) {
                            if self.try_consume(TokenType::CLOSE_SQUARE).is_none() {
                                let loc = self.peek_token().unwrap().token_location;
                                ParsingError::ExpectedTokenNotFound{expected_token:"Close Paren ']'".to_string(),loc}.error_and_exit();
                            }
                            return Some(DataType::Pointer {
                                ty: Box::new(DataType::Array(Box::new(dt),len.value.unwrap().parse::<usize>().unwrap()))
                            })

                        }else {
                            let loc = self.peek_token().unwrap().token_location;
                            ParsingError::Custom(format!("Expected a lenght of array at {}:{}",loc.0,loc.1)).error_and_exit();
                            None
                        }
                    }else {
                        // Slice!
                        if self.try_consume(TokenType::CLOSE_SQUARE).is_none() {
                            let loc = self.peek_token().unwrap().token_location;
                            ParsingError::ExpectedTokenNotFound{expected_token:"Close Paren ']'".to_string(),loc}.error_and_exit();    
                        }
                        return Some(DataType::Slice{
                            ty:Box::new(dt)
                        })
                    }
                } else  {
                    let loc = self.peek_token().unwrap().token_location;
                    ParsingError::Custom(format!("Expected a data type at {}:{}",loc.0,loc.1)).error_and_exit();
                    None
                }

            }else if let Some(dt) = self.parse_type() {
                Some(DataType::Pointer {
                    ty:Box::new(dt),
                })
            }
            else {
                let loc = self.peek_token().unwrap().token_location;
                ParsingError::Custom(format!("Expected a data type at {}:{}",loc.0,loc.1)).error_and_exit();
                None
            }
        }
        else {
            None
        }

    }

    pub fn parse_ident_term(&mut self) ->Option<NodeTerm> {
        if let Some(value) =self.try_consume(TokenType::IDENT) {
            return Some(NodeTerm::NodeTermIdent {
                value
            })
        }
        None
    }
    pub fn parse_term(&mut self) -> Option<NodeTerm> {
        if let Some(bt) = self.parse_builtin() {
          
           return Some(NodeTerm::NodeTermBuiltinFunc{value:bt}); 
        }
        if self.try_consume(TokenType::AMPERSAND).is_some() {
            return Some(NodeTerm::NodePointer {
                value: Box::new(self.parse_term().unwrap()),
            })
        }
        if self.try_consume(TokenType::MULT).is_some() {
            return Some(NodeTerm::NodeTermDeref {
                value: Box::new(self.parse_term().unwrap()),
            })
        }
        if let Some(t) = self.peek_token() {
                if t.token_type == TokenType::OPEN_SQUARE {
                self.consume_token();
                let init_value = self.parse_expr(0, DataType::Infer).unwrap();

                if let Some(_s) = self.try_consume(TokenType::SEMICOLON) {
                    let len = self.parse_expr(0, DataType::Infer);
                    if len.is_none() {
                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::InvalidExpr { loc }.error_and_exit();
                        return None;
                    } else if self.try_consume(TokenType::CLOSE_SQUARE).is_none() {
                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::InvalidExpr { loc }.error_and_exit();
                        return None;
                    }
                    Some(NodeTerm::NodeTermArray {
                        value: ArrayTerm::ArrayBuilder {
                            init_value: Box::new(init_value),
                            len: Box::new(len.unwrap()),
                        }
                    })
                } else if let Some(_c) = self.try_consume(TokenType::COMMA) {
                    let mut v: Vec<Box<NodeExpr>> = Vec::new();
                    v.push(Box::new(init_value.clone()));
                    let mut c = 0;
                    loop {
                        if let Some(t) = self.peek_token() {
                            if t.token_type == TokenType::CLOSE_SQUARE {
                                break;
                            }
                            if c % 2 == 0 {
                                if let Some(e) = self.parse_expr(0, DataType::Infer) {
                                    v.push(Box::new(e));
                                } else {
                                    ParsingError::Custom(format!("Expected value, found {:?} at {}:{}", t, t.token_location.0, t.token_location.1)).error_and_exit();
                                }
                            } else {
                                if t.token_type != TokenType::COMMA {
                                    ParsingError::Custom(format!("Expected comma (,) found {:?} at {}:{}", t, t.token_location.0, t.token_location.1)).error_and_exit();
                                }
                                self.consume_token();
                            }
                            c += 1;
                        } else {
                            ParsingError::Custom("Unexpected end of input".to_string()).error_and_exit();
                        }
                    }
                    if self.try_consume(TokenType::CLOSE_SQUARE).is_none() {
                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::InvalidExpr { loc }.error_and_exit();
                    }
                    Some(NodeTerm::NodeTermArray {
                        value: ArrayTerm::DefaultValues {
                            ty: {
                                let dt = init_value.get_datatype(&self.functions, &self.vars);
                                match dt {
                                    DataType::Array(_,_) => dt,
                                    _ => DataType::Array(Box::new(dt.clone()),v.len())
                                }
                            },
                            values: v,
                        }
                    })
                } else {
                    let loc = self.peek_token().unwrap().token_location;
                    ParsingError::InvalidExpr { loc }.error_and_exit();
                    None
                }
            } else if t.token_type == TokenType::NUM {
                let term = NodeTerm::NodeTermIntLit { value: self.consume_token().unwrap() };
                Some(term)
            } else if t.token_type == TokenType::STR_LIT {
                if t.value.clone().unwrap().len() == 1 {
                    let term = NodeTerm::NodeTermChar { value: self.consume_token().unwrap() };
                    return Some(term);
                }
                let ty = t.datatype.unwrap();
                let values = t.value.clone().unwrap().chars().enumerate().map(|x| Box::new(NodeExpr::NodeExprTerm {
                    value: NodeTerm::NodeTermChar {
                        value: Token {
                            token_type: TokenType::STR_LIT,
                            datatype: Some(DataType::Char),
                            token_location: (t.token_location.0, t.token_location.1 + x.0),
                            value: Some(x.1.to_string())
                        }
                    }
                })).collect();
                let term = NodeTerm::NodeTermArray {
                    value: ArrayTerm::DefaultValues {
                        values,
                        ty
                    }
                };
                self.consume_token();
                Some(term)
            } else if t.token_type == TokenType::IDENT {
                if let Some(os) = self.peek_token_offset(1) {
                    if os.token_type == TokenType::OPEN_SQUARE {
                        if let Some(dt) = self.vars.get_mut(&t.value.clone().unwrap()) {
                            let new_tok = Token {
                                datatype: Some(dt.clone()),
                                token_type: TokenType::IDENT,
                                value: t.value,
                                token_location: t.token_location
                            };
                            self.consume_token(); // Consume ident
                            self.consume_token(); // Consume [

                            let index_expr = self.parse_expr(0, DataType::Int32);
                            if index_expr.is_none() {
                                ParsingError::ExpectedExpr { loc: t.token_location }.error_and_exit();
                            }

                            if self.try_consume(TokenType::CLOSE_SQUARE).is_none() {
                                let loc = self.peek_token().unwrap().token_location;
                                ParsingError::ExpectedTokenNotFound { expected_token: "]".to_string(), loc }.error_and_exit();
                            }
                            return Some(NodeTerm::NodeTermArray {
                                value: ArrayTerm::ArrayIndexer {
                                    value: new_tok,
                                    index: Box::new(index_expr.unwrap()),
                                    ty: ArrayIndexerType::default()
                                }
                            });
                        }
                    }
                }
                if let Some(dt) = self.vars.get_mut(&t.value.clone().unwrap()) {
                    let new_tok = Token {
                        datatype: Some(dt.clone()),
                        token_type: TokenType::IDENT,
                        value: t.value,
                        token_location: t.token_location
                    };
                    self.consume_token();
                    Some(NodeTerm::NodeTermIdent { value: new_tok.clone() })
                }else {
                    self.vars.insert(t.value.unwrap().clone(), t.datatype.unwrap());
                    let term = NodeTerm::NodeTermIdent { value: self.consume_token().unwrap() };
                    Some(term)
                }
            } else if let Some(_open_paren) = self.try_consume(TokenType::OPEN_PAREN) {
                if let Some(expr) = self.parse_expr(0, DataType::Infer) {
                    if let Some(_) = self.try_consume(TokenType::CLOSE_PAREN) {
                        let term = NodeTerm::NodeTermParen { value: Box::new(expr) };
                       Some(term)
                    } else {
                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::ExpectedTokenNotFound { expected_token: ")".to_string(), loc }.error_and_exit();
                        None
                    }
                } else {
                    let loc = self.peek_token().unwrap().token_location;
                    ParsingError::ExpectedExpr { loc }.error_and_exit();
                    None
                }
            } else if let Some(boolean) = self.try_consume(TokenType::TRUE) {
                let term = NodeTerm::NodeTermBool { value: boolean };
                Some(term)
            } else if let Some(boolean) = self.try_consume(TokenType::FALSE) {
                let term = NodeTerm::NodeTermBool { value: boolean };
                Some(term)
            } else {
                None
            }
        } else {
            None
        }
    }
    
    
    pub fn check_type_term_and_expr(&mut self,ident:&mut NodeTerm,expr:NodeExpr) {
        let ident_clone = ident.clone();
        let expected_dt = ident.get_datatype(&self.vars,&self.functions);
        let expected_dt_clone = expected_dt.clone();
        let got_dt = expr.get_datatype(&self.functions,&self.vars).clone();
        let got_dt_clone = got_dt.clone();
            let inc_type_err = move || {

                ParsingError::IncorrectType{ident:ident_clone.ident().unwrap(),expected_type:expected_dt_clone,got_type:got_dt_clone}.error_and_exit()

            };
        if expected_dt != got_dt && expected_dt != DataType::Infer {
            match expected_dt {

                DataType::Array(ref d,_) => {
                    if **d == got_dt {
                        return;
                    }
                    inc_type_err();
                }
                DataType::Slice{ref ty} => {
                    match got_dt{
                        DataType::Pointer {ty: ref dty } => {
                            if ty == dty {
                                return 
                            }else {

                                inc_type_err();
                            }
                        }
                        _ =>  inc_type_err()

                    }

                }
                _ =>inc_type_err()
            }

        }
        if expected_dt ==DataType::Infer && got_dt != DataType::Infer {
            ident.try_set_dt(got_dt).unwrap();
        }
    }

    pub fn check_type_expr(&mut self,ident:&mut Token,expr:NodeExpr) {
        let datatype = self.get_datatype_expr(&expr).unwrap();
        if let Some(expected_dt) = self.vars.get(&ident.value.clone().unwrap()) {
            let expected_dt = expected_dt.clone();
            if expected_dt != datatype && expected_dt != DataType::Infer  {
                match expected_dt {
                    DataType::Array(ref d,_)=> {
                        if **d == datatype{
                            return;
                        }
                        dbg!(ParsingError::IncorrectType{ident:ident.clone(),expected_type:expected_dt,got_type:datatype.clone()}).error_and_exit()

                    }
                    DataType::Slice { ref ty } => {
                        match datatype {
                            DataType::Pointer {ty: ref dty } => {
                                if ty == dty {
                                    return 
                                }else {
                                    ParsingError::IncorrectType{ident:ident.clone(),expected_type:expected_dt,got_type:datatype.clone()}.error_and_exit();
                                }
                            }
                            _ =>ParsingError::IncorrectType{ident:ident.clone(),expected_type:expected_dt,got_type:datatype.clone()}.error_and_exit()
                        }
                    }
                    _ => ParsingError::IncorrectType{ident:ident.clone(),expected_type:expected_dt,got_type:datatype.clone()}.error_and_exit()

                }
            }
        }else {
            ParsingError::UndeclaredIdentifier{ident:ident.clone()}.error_and_exit();

        }
        if ident.datatype.is_some() && ident.datatype.clone().unwrap() == DataType::Infer && datatype != DataType::Infer{
            ident.datatype = Some(datatype);
        }

    } 


    pub fn get_datatype_expr(&mut self, expr:&NodeExpr) -> Option<DataType> {
        let datatype = match expr.clone() {
            NodeExpr::NodeExprTerm{value:term} =>  {
                term.get_datatype(&self.vars,&self.functions) 
            }
            NodeExpr::NodeExprBoolExpr{value:_nodeterm} => {
                DataType::Bool 
            }  
            NodeExpr::NodeExprBinExpr{value:_tok} =>DataType::Int32,
            NodeExpr::NodeExprFunctionCall { value} => { 
                let d1 = self.vars.get(value.ident.value.as_ref().unwrap());
                let d2 = self.functions.get(value.ident.value.as_ref().unwrap());
                dbg!((d1,d2));
                if d1.is_none() && d2.is_none() {
                    DataType::Void 
                }else if d1.is_some() && d2.is_some() {
                    unreachable!();
                }else if d1.is_some() {
                    d1.unwrap().clone()
                }else  {
                   d2.unwrap().clone().return_type.unwrap_or(DataType::Void)
                }
            },
NodeExpr::NodeExprBuiltin { value } => value.value.get_return_type().unwrap_or(DataType::Void),
        };
        return Some(datatype);

    }

    // https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
    pub fn parse_expr(&mut self, min_prec: usize,expr_datatype:DataType) -> Option<NodeExpr> {
        if let Some(lhs_term) = self.parse_term() {
            match lhs_term.clone() {
                NodeTerm::NodeTermIdent { value } => {


                    // Funcction
                    // TODO: Change this to be nodeterm function term rather than a nodeexpr
                    if self.try_consume(TokenType::OPEN_PAREN).is_some() {
                        let mut args:Vec<NodeExpr> = Vec::new();

                        loop {
                            match self.consume_token() {
                                None => {
                                    let loc = self.peek_token().unwrap().token_location;
                                    ParsingError::ExpectedTokenNotFound{expected_token:")".to_string(),loc}.error_and_exit()
                                }
                                Some(t) => {
                                    let loc = self.peek_token().unwrap().token_location;
                                    match t.token_type {
                                        TokenType::CLOSE_PAREN => {

                                            let value =NodeFunctionCall {
                                                ident:value.clone(),
                                                args:args.clone()
                                            };
                                            return Some(NodeExpr::NodeExprFunctionCall{value});
                                        }
                                        TokenType::IDENT | TokenType::NUM | TokenType::TRUE | TokenType::FALSE => {
                                            self.index -=1;
                                            if let Some(argexpr) = self.parse_expr(0,DataType::Infer) {
                                                args.push(argexpr);
                                            }else {
                                                ParsingError::InvalidExpr{loc}.error_and_exit();
                                            }
                                            if let Some(_c) = self.try_consume(TokenType::COMMA) {

                                            }else {
                                                if self.peek_token().is_some() && self.peek_token().unwrap().token_type == TokenType::CLOSE_PAREN {
                                                    continue;
                                                }else {
                                                    let tok = Token {token_type:TokenType::COMMA,value:None,datatype:None,token_location:loc};
                                                    ParsingError::UnexpectedTokenFound{unexpected_token:tok}.error_and_exit();
                                                }
                                            }
                                        }

                                        e => {
                                            ParsingError::Custom(format!("Found token of type {:?} instead of indentifer or literal\n\t at {}:{}",e,loc.0,loc.1)).error_and_exit();
                                            //ParsingError::FoundInsteadOf{unexpected_token:e,expected_token:"identifier or literal".to_string()}.error_and_exit();
                                        }
                                    }
                                }
                            } 
                        }

                    }
                }
                _ => {}
            }
            let mut lhs_expr = NodeExpr::NodeExprTerm { value: lhs_term };

            let expr_dt = self.get_datatype_expr(&lhs_expr);
            if expr_datatype != DataType::Infer && expr_dt.is_some() && expr_dt.clone().unwrap() != expr_datatype {
                // check for slice 
                match expr_datatype {
                    DataType::Slice { ty: ref slice_type } => {
                        match expr_dt.clone().unwrap() {
                            DataType::Pointer { ty:arr } => {
                                match *arr {
                                    DataType::Array(ty,_len) => {
                                        if *slice_type != ty {
                                            let loc = self.peek_token().unwrap().token_location;
                                            ParsingError::IncorrectTypeExpr{expected_type:expr_datatype,got_type:expr_dt.clone().unwrap(),loc}.error_and_exit();

                                        }
                                    }
                                    _ => {
                                        let loc = self.peek_token().unwrap().token_location;
                                        ParsingError::IncorrectTypeExpr{expected_type:expr_datatype,got_type:expr_dt.clone().unwrap(),loc}.error_and_exit();
                                    }
                                }
                            }
                            _ => {
                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::IncorrectTypeExpr{expected_type:expr_datatype,got_type:expr_dt.clone().unwrap(),loc}.error_and_exit();
                            }
                        }
                    }
                    _ =>{
                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::IncorrectTypeExpr{expected_type:expr_datatype,got_type:expr_dt.clone().unwrap(),loc}.error_and_exit();

                    }
                }
            } 

            while let Some(cur) = self.peek_token() {
                if is_bool_op(cur.token_type) {
                    if let Some(bool_op) = self.consume_token() {
                        if let Some(rhs) = self.parse_expr(0,expr_dt.clone().unwrap()) {
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
                } 

                else if !is_bin_op(cur.token_type)
                    || get_bin_precedence_level(cur.token_type).map_or(true, |cur_prec| cur_prec < min_prec)
                    {
                        break;
                    } else {
                        let next_min_prec = get_bin_precedence_level(cur.token_type).unwrap() + 1;
                        if let Some(op) = self.consume_token() {
                            if let Some(rhs) = self.parse_expr(next_min_prec,expr_dt.clone().unwrap()) {
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
                v
            };
            if self.try_consume(TokenType::CLOSE_CURLY).is_none(){
                let loc = self.peek_token().unwrap_or(self.tokens[self.index-1].clone()).token_location;
                ParsingError::ExpectedTokenNotFound{expected_token:"}".to_string(),loc}.error_and_exit(); 


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
            let prev_state = self.state;
            self.state = ParsingState::ParsingExit;
            let mut stmt_exit = None;

            if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                stmt_exit = Some(NodeStatement::NodeStatementExit { value: NodeExit { expr } });
            } else {
                let loc = self.peek_token().unwrap().token_location;
                ParsingError::InvalidExpr{loc}.error_and_exit();
            }
            if self.try_consume(TokenType::CLOSE_PAREN).is_none() {
                let loc = self.peek_token().unwrap().token_location;
                ParsingError::ExpectedTokenNotFound { expected_token: ")".to_string(),loc }.error_and_exit();
            }

            if self.try_consume(TokenType::SEMICOLON).is_none() {
                let loc = self.peek_token().unwrap().token_location;
                ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string(), loc }.error_and_exit();
            }

            self.state = prev_state; 
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
                    let prev_state = self.state;
                    self.state = ParsingState::ParsingLet;
                    self.consume_token();
                    let mut ident = self.consume_token().unwrap();
                    if self.vars.get(&ident.value.clone().unwrap()).is_some() {
//                        println!("Cannot have two identifiers with the same name {:?}", ident.value.unwrap());
                      //  std::process::exit(0);
                    }
                    self.vars.insert(ident.value.clone().unwrap(), DataType::Infer);
                    let mut stmt_let = None;
                    self.consume_token(); // Consume =

                    if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                        // Set Type for identifier
                        match expr.clone() {
                            NodeExpr::NodeExprTerm { value: term } => {
                                ident.datatype = Some(term.get_datatype(&self.vars,&self.functions));
                            },
                            NodeExpr::NodeExprBinExpr { .. } => ident.datatype = Some(DataType::Int32),
                            NodeExpr::NodeExprBoolExpr { .. } =>{ident.datatype = Some(DataType::Bool);},
                            NodeExpr::NodeExprFunctionCall { value } => {
                                //ident.datatype = Some(DataType::Infer);
                                let fn_def = self.functions.get(&value.ident.value.unwrap()).unwrap();
                                ident.datatype = fn_def.return_type.clone();
                            }
                            _ => unimplemented!("Unimplemented Type, {:?}",expr),
                        }
                        if let Some(v) = self.vars.get_mut(&ident.value.clone().unwrap()) {
                            *v = ident.datatype.clone().unwrap();
                        }
                        stmt_let = Some(NodeStatement::NodeStatementLet {
                            value: NodeLet { expr, ident },
                        });
                    } else {
                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::InvalidExpr{loc}.error_and_exit();
                    }

                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        let loc = self.peek_token().unwrap().token_location;
                        println!("{:?}",self.peek_token());
                        ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string(),loc }.error_and_exit();
                    }

                    self.state = prev_state;
                    return stmt_let;
                }
        return None;
    }


    pub fn parse_else(&mut self) -> Option<NodeElse> {
        if let Some(_else_keyword) = self.try_consume(TokenType::ELSE) {
            let prev_state = self.state;
            self.state = ParsingState::ParsingLet;

            let if_stmt = if let Some(i) = self.parse_if_stmt() {
                match i {
                    NodeStatement::NodeStatementIf { .. } => Some(Box::new(i)),
                    _ =>{ ParsingError::ExpectedStatementNotFound {
                        expected_statement: "if statement".to_string(),
                        loc: self.peek_token().unwrap().token_location
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
                ParsingError::Custom("Expected scope after else keyword, found none.".to_string())
                    .error_and_exit();
            }
            self.state =prev_state;
        }
        None
    }

    pub fn parse_if_stmt(&mut self) -> Option<NodeStatement> {
        if let Some(_if_tok) = self.try_consume(TokenType::IF) {
            if let Some(_open_paren) = self.try_consume(TokenType::OPEN_PAREN) {
                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                    if let Some(_close_paren) = self.try_consume(TokenType::CLOSE_PAREN) {
                        if let Some(scope) = self.parse_stmt() {
                            let prev_state = self.state;
                            self.state = ParsingState::ParsingIf;

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

                            let node_if = NodeIf { expr, scope, else_stmt };
                            let if_stmt = NodeStatement::NodeStatementIf { value: node_if };
                            self.state =prev_state;
                            return Some(if_stmt);
                        } else {
                            println!("Expected scope after if condition. It is either empty or does not exist");
                            std::process::exit(1);
                        }
                    } else {

                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::ExpectedTokenNotFound{expected_token:")".to_string(),loc}.error_and_exit(); 

                    }
                } else {
                    println!("Failed to parse expression");
                    std::process::exit(1);
                }
            } else {
                let loc = self.peek_token().unwrap().token_location;
                ParsingError::ExpectedTokenNotFound { expected_token: "(".to_string(), loc}.error_and_exit();
            }

        }
        return None;
    }

    pub fn parse_reassign_stmt(&mut self) -> Option<NodeStatement> {
        let ident = self.parse_ident_term();
        if ident.is_none() {
            return None;
        } 
        let mut ident = ident.unwrap();


        let (is_array,array_expr)  =  {
            if self.peek_token().is_none() || self.peek_token().unwrap().token_type != TokenType::OPEN_SQUARE {
                (false,None)
            }else {
                self.index -=1;
                (true,self.parse_expr(0,DataType::Infer))
            }
        };
        let node_reassign = {
            if let Some(_assign) = self.try_consume(TokenType::ASSIGN) {
                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                    self.check_type_term_and_expr(&mut ident, expr.clone());
                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string() ,loc}.error_and_exit();
                    }
                    let node_reassign = NodeReassign::Assign { expr, ident};
                    node_reassign
                } else {
                    let loc = self.peek_token().unwrap().token_location;
                    ParsingError::InvalidExpr{loc}.error_and_exit();
                    unreachable!();
                }
            } else if let Some(_add_assign) = self.try_consume(TokenType::ADD_ASSIGN) {

                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                  
                    self.check_type_term_and_expr(&mut ident, expr.clone());

                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string(),loc }.error_and_exit();
                    }
                    let node_reassign = NodeReassign::Add { expr, ident  };

                    node_reassign

                } else {
                    
                    ParsingError::InvalidExpr{loc:self.peek_token().unwrap().token_location}.error_and_exit();
                    unreachable!();
                }
            } else if let Some(_sub_assign) = self.try_consume(TokenType::SUB_ASSIGN) {
                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                    self.check_type_term_and_expr(&mut ident,expr.clone());

                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
                    }
                    let node_reassign = NodeReassign::Sub { expr, ident };

                    node_reassign

                } else {
                    ParsingError::InvalidExpr{loc:self.peek_token().unwrap().token_location}.error_and_exit();
                    unreachable!();

                }
            } else if let Some(_mult_assign) = self.try_consume(TokenType::MULT_ASSIGN) {
                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                    self.check_type_term_and_expr(&mut ident,expr.clone());

                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
                    }
                    let node_reassign = NodeReassign::Mul { expr, ident };
                    node_reassign
                } else {
                    ParsingError::InvalidExpr{loc:self.peek_token().unwrap().token_location}.error_and_exit();
                    unreachable!();

                } 
            } else if let Some(_div_assign) = self.try_consume(TokenType::DIV_ASSIGN) {
                if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                    self.check_type_term_and_expr(&mut ident,expr.clone());

                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        ParsingError::ExpectedTokenNotFound { expected_token: ";".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
                    }
                    let node_reassign = NodeReassign::Div { expr, ident };
                    node_reassign

                } else {
                    ParsingError::InvalidExpr{loc:self.peek_token().unwrap().token_location}.error_and_exit();
                    unreachable!()
                }
            } else {
                ParsingError::UnexpectedTokenFound { unexpected_token: ident.ident().unwrap()}.error_and_exit();
                unreachable!();
            }
        };
        if is_array {
            let mut array_expr =  array_expr.unwrap();
            match array_expr {
                NodeExpr::NodeExprTerm { ref mut value } => {
                    match value {
                        NodeTerm::NodeTermArray { ref mut value } => {
                            match value{
                                ArrayTerm::ArrayIndexer { value:_, index:_, ref mut ty } => {
                                    *ty = ArrayIndexerType::Lhs;
                                }
                                _ => unimplemented!()

                            }
                        } 
                        _ => unreachable!()
                    }
                }
                _ => unreachable!()
            }

            return Some(NodeStatement::NodeStatementReassign{value:NodeReassign::ArrayReassign {value:Box::new(node_reassign),array:array_expr}});

        }else{
            return Some(NodeStatement::NodeStatementReassign{value:node_reassign})
        }
    }

    pub fn parse_while_loop(&mut self) -> Option<NodeStatement> {
        if self.try_consume(TokenType::WHILE).is_some(){
            if self.try_consume(TokenType::OPEN_PAREN).is_none() {
                ParsingError::ExpectedTokenNotFound{expected_token:"(".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
            } 

            let prev_state = self.state;
            self.state = ParsingState::ParsingWhile;
            if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                if self.try_consume(TokenType::CLOSE_PAREN).is_none() {
                    ParsingError::ExpectedTokenNotFound{expected_token:")".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
                    return None; // Rust Compiler things
                }
                if let Some(scope) = self.parse_scope() {
                    let node_while = NodeWhileLoop {
                        condition:expr,
                        scope
                    }; 

                    self.state = prev_state; 
                    return Some(NodeStatement::NodeStatementWhileLoop{value:node_while});
                }else {
                    ParsingError::Custom("Expected scope after while(condition) statement.".to_string()).error_and_exit();
                }
            }else {
                ParsingError::ExpectedExpr{loc:self.peek_token().unwrap().token_location}.error_and_exit();
            }
        }

        return None;


    }

    pub fn parse_func_def(&mut self) -> Option<NodeStatement> {
        if self.try_consume(TokenType::FN).is_none() {
            return None
        }
        if let Some(ident) = self.try_consume(TokenType::IDENT) {
            if self.try_consume(TokenType::OPEN_PAREN).is_none() {
                ParsingError::ExpectedTokenNotFound{expected_token:"(".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
            }
            if self.state == ParsingState::ParsingFuncDef {
                ParsingError::Custom(format!("Cannot create function `{}` inside function",ident.value.clone().unwrap())).error_and_exit();
            }
            let prev_state = self.state;
            self.state = ParsingState::ParsingFuncDef;
            let mut args:IndexMap<Token,NodeExpr> = IndexMap::new();
            loop {
                if self.peek_token().is_none() {

                    ParsingError::Custom(format!("Unexpected end of input after function declaration :{:?}",ident.value.clone().unwrap())).error_and_exit();
                }
                if self.try_consume(TokenType::CLOSE_PAREN).is_some() {
                    let return_type:Option<DataType> = {

                        let mut res = None;
                        if self.try_consume(TokenType::COLON).is_some() {
                            match self.peek_token() {
                                None => {
                                    ParsingError::Custom(format!("Unexpected end of input after function declaration :{:?}",ident.value.clone().unwrap())).error_and_exit();
                                }
                                Some(_t) => {
                                    if let Some(return_dt) = self.parse_type() {
                                        res = Some(return_dt);
                                    }else {
                                        let loc = self.peek_token().unwrap().token_location;
                                        ParsingError::Custom(format!("Expected return type at {}:{}\n\tHint: if you don't want a return type, remove the ':'",loc.0,loc.1)).error_and_exit();
                                    }
                                }
                            };
                        } 
                        res
                    };

                    
                    let mut nodefuncdef = NodeFunctionDefination {
                        ident:ident.clone(),
                        args: args.clone(),
                        return_type,
                        scope:NodeScope {stmts:Vec::new()},
                    };
                    self.functions.insert(ident.value.clone().unwrap(),nodefuncdef.clone());

                    if let Some(scope) = self.parse_scope(){ 
                        nodefuncdef.scope = scope.clone();
                        self.functions.get_mut(&ident.value.clone().unwrap()).unwrap().scope = scope;
                        self.state = prev_state;
                        return Some(NodeStatement::NodeStatementFunctionDefination {value:nodefuncdef});
                    }else {
                        todo!();
                    }
                }
                match self.parse_type() {
                    None => {
                        let loc = self.peek_token().unwrap().token_location;
                        ParsingError::Custom(format!("Expected type at {}:{} ",loc.0,loc.1)).error_and_exit();
                    }
                    Some(dt) => {
                        if let Some(argident) = self.try_consume(TokenType::IDENT) {
                            self.index -= 1;
                            self.tokens[self.index].datatype = Some(dt.clone());
                            let argexpr = self.parse_expr(0,dt.clone());
                            if argexpr.is_none() { ParsingError::InvalidExpr{loc:self.peek_token().unwrap().token_location}.error_and_exit(); }
                            args.insert(argident,argexpr.unwrap());
                            if self.try_consume(TokenType::COMMA).is_none() 
                                && self.peek_token().is_none() && self.peek_token().unwrap().token_type != TokenType::CLOSE_PAREN{
                                    ParsingError::ExpectedTokenNotFound{expected_token:",".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
                                }
                        }else {
                            ParsingError::ExpectedTokenNotFound{expected_token:"identifier".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
                        }
                    }

                }
            }

        }else {
            ParsingError::ExpectedTokenNotFound{expected_token:"Identifier".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
            None
        }
    }

    pub fn parse_func_call_stmt(&mut self) -> Option<NodeStatement> {
        if let Some(ident) = self.try_consume(TokenType::IDENT) {
            let funcident = ident.value.clone().unwrap();
            if let Some(_) = self.try_consume(TokenType::OPEN_PAREN) {

                if let Some(nodefuncdef) = self.functions.get(&funcident) {

                    let prev_state = self.state;
                    self.state = ParsingState::ParsingFuncCall;
                    let nodefuncdef = nodefuncdef.clone();
                    //let returntype = nodefuncdef.return_type;
                    let mut args:Vec<NodeExpr> = Vec::new();

                    for i in 0..nodefuncdef.args.len() {
                        match self.peek_token() {
                            None => ParsingError::Custom(format!("Unexpected end of input after calling function {:?}. \n\t{} arguments were expected, {i} were given.",ident.value.clone().unwrap(),nodefuncdef.args.len())).error_and_exit(),
                            Some(_t) => {
                                let funcs = &self.functions;
                                let vars = &self.vars;

                                let argexpr = self.parse_expr(0,nodefuncdef.args[i].get_datatype(funcs,vars));
                                if argexpr.is_none() {
                                    ParsingError::InvalidExpr{loc:self.peek_token().unwrap().token_location}.error_and_exit();
                                }
                                args.push(argexpr.unwrap());
                                if nodefuncdef.args.len() !=0 && i != nodefuncdef.args.len()-1&& self.try_consume(TokenType::COMMA).is_none() {
                                    let tok = self.peek_token_force();
                                    match tok.token_type {
                                        TokenType::CLOSE_PAREN => {
                                    ParsingError::Custom(format!("Expected argument number {} in function call {} at {}:{}",i+2,funcident,tok.token_location.0,tok.token_location.1)).error_and_exit();
                                        }        
                                        _ =>   ParsingError::Custom(format!("Expected comma `,` after argument number {} in function call {}",i+2,funcident)).error_and_exit()
                                    }
                                    
                                }  
                            }
                        }
                    }
                    if self.try_consume(TokenType::CLOSE_PAREN).is_none() {
                        ParsingError::Custom(format!("Expected `(` after calling function {}",funcident)).error_and_exit();
                    }
                    if self.try_consume(TokenType::SEMICOLON).is_none() {
                        ParsingError::ExpectedTokenNotFound{expected_token:";".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
                    }
                    let nodefunccall = NodeFunctionCall {
                        ident,args
                    };
                    self.state = prev_state;
                    return Some(NodeStatement::NodeStatementFunctionCall{value:nodefunccall});
                }else {
                    let loc = ident.token_location;
                    ParsingError::Custom(format!("Undeclared function {:?} at line {}:{}",ident.value.clone().unwrap(),loc.0,loc.1)).error_and_exit();

                }

            }else {
                self.index -=1;
                return None;
            }
        } 
        None  
    }

    pub fn parse_return(&mut self) -> Option<NodeStatement> {
        if let Some(_) = self.try_consume(TokenType::RETURN) {
            if let Some(expr) = self.parse_expr(0,DataType::Infer) {
                if self.try_consume(TokenType::SEMICOLON).is_none() {
                    ParsingError::ExpectedTokenNotFound{expected_token:";".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
                }
                return Some(NodeStatement::NodeStatementReturn{value:NodeReturn {
                    expr: Some(expr)
                }})
            }      
            ParsingError::InvalidExpr{loc:self.peek_token().unwrap().token_location}.error_and_exit();
        }
        None
    }

    pub fn parse_builtin(&mut self) -> Option<NodeBuiltin>{
        if let Some(builtin) = self.try_consume(TokenType::BUILTIN) {
            if let Some(builtin) = BuiltinType::from_str(builtin.value.clone().unwrap().as_str()) {
                if self.try_consume(TokenType::OPEN_PAREN).is_none() {
                    ParsingError::ExpectedTokenNotFound{expected_token:"(".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
                }
                let expr = self.parse_expr(0,DataType::Infer);
                if expr.is_none() {
                    ParsingError::ExpectedExpr{loc:self.peek_token().unwrap().token_location}.error_and_exit();
                }
                if self.try_consume(TokenType::CLOSE_PAREN).is_none() {
                    ParsingError::ExpectedTokenNotFound{expected_token:")".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
                }
                let builtin = NodeBuiltin{
                    expr:Box::new(expr.unwrap()),
                    value:builtin

                };
                return Some(builtin)
            }else {
                ParsingError::Custom(format!("Invalid builtin function {:?} at {}:{}",builtin.value.unwrap(),builtin.token_location.0,builtin.token_location.1)).error_and_exit();
            }
        }
        None
    }

    pub fn parse_builtin_statement(&mut self) ->Option<NodeStatement> {
        if let Some(bt) = self.parse_builtin() {
            if self.try_consume(TokenType::SEMICOLON).is_none() {
                ParsingError::ExpectedTokenNotFound{expected_token:";".to_string(), loc:self.peek_token().unwrap().token_location}.error_and_exit();
            }
           return Some(NodeStatement::NodeStatementBuiltin {value:bt})
        }
        None
    }

    pub fn parse_stmt(&mut self) -> Option<NodeStatement> {
        while let Some(_curp) = self.peek_token() {
            if let Some(stmt_exit) = self.parse_exit_stmt() {
                return Some(stmt_exit);
            } else if let Some(let_stmt) = self.parse_let_stmt() {
                return Some(let_stmt);

            } else if let Some(func_call) = self.parse_func_call_stmt() {
                return Some(func_call);
            }
            else if let Some(node_scope) = self.parse_scope() {

                let scope_stmt = NodeStatement::NodeStatementScope { value: node_scope };
                return Some(scope_stmt);
            } else if let Some(if_stmt) =  self.parse_if_stmt() {
                return Some(if_stmt);
            } else if let Some(reassign_stmt) = self.parse_reassign_stmt() {
                return  Some(reassign_stmt)   
            }else if let Some(while_loop) = self.parse_while_loop() {
                return Some(while_loop);
            }else if let Some(func_def) = self.parse_func_def() {
                return Some(func_def);
            }else if let Some(re) = self.parse_return() {
                return Some(re);
            }else if let Some(bt) = self.parse_builtin_statement() {
                return Some(bt);
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

    fn peek_token_force(&self) ->Token {
        if let Some(t) = self.peek_token() {
           return t
        }else {
            let t = &self.tokens[self.index-1];
            let (loc1,loc2) = (t.token_location.0,t.token_location.1);
            ParsingError::Custom(format!("Unexpected end of input at {}:{}",loc1,loc2)).error_and_exit();
        }
        unreachable!()
    }

    fn consume_token(&mut self) -> Option<Token> {
        let token = self.peek_token();
        if token.is_some() {
            self.index += 1;
        }
        token
    }
}
