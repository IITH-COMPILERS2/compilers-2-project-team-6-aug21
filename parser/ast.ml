type var_type = 
    | IntType
    | StringType 
    | Stack 
    | FloatType 
    | VoidType 
    | Eos 

type binary_op = 
    | Add 
    | Sub 
    | Mult 
    | Div 
    | Mod 
    | Equal 
    | Neq 
    | Lt 
    | Leq 
    | Gt 
    | Geq

type unop = 
    | Not 
    | Neg

type re = 
    | And 
    | Or

type ident =
    | Ident of string

type datatype = 
    | Datatype of var_type 
    | Stacktype of datatype
    | Eostype of var_type

type expr = 
    | IntLit of int 
    | StringLit of string 
    | FloatLit of float  
    | EosLit 
    | Variable of ident 
    | Unop of unop * expr 
    | BinaryOp of expr * binary_op * expr 
    | Brela of expr * re * expr 
    | Call of ident * expr list 
    | Push of ident * expr 
    | Pop of ident 
    | Peek of ident 
    | Print of expr
    

type value = 
    ExprVal of expr

and decl = 
    | VarDecl of datatype * ident 
    | VarAssignDecl of datatype * ident * value

type stmt = 
    | Block of stmt list 
    | Expr of expr 
    | Declaration of decl 
    | Assign of ident * expr
    | Transition of ident * expr 
    | Return of expr 
    | Print of expr
    

type formal = 
    | Formal of datatype * ident

type node = 
    | Node of ident * stmt list

type dfa_decl = 
{
    return : datatype;
    dfa_name: ident;
    formals : formal list;
    var_body : decl list;
    node_body : node list;
}


type program = dfa_decl list
