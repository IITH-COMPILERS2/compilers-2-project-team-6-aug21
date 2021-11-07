type var_type = 
    | Int
    | String
    | Stack 
    | Float
    | Void
    | Eos 

type binop = 
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
    | And 
    | Or
type unop = 
    | Not 
    | Neg



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
    | BinaryOp of expr * binop * expr 
    | Call of ident * expr list 
    | Push of ident * expr 
    | Pop of ident 
    | Peek of ident 
    

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


type scope = 
    NodeScope
    | DFAScope
    | StateScope

type sident =
    SIdent of ident * scope

type sval = 
	SExprVal of  sexpr

and sexpr = 
    SIntLit of int * datatype 
    | SFloatLit of float * datatype
    | SStringLit of string * datatype
    | SVariable of sident * datatype
    | SUnop of unop * sexpr * datatype
    | SBinop of sexpr * binop * sexpr * datatype
    | SCall of sident * sexpr list * datatype
    | SPeek of sident * datatype
    | SPop of sident * datatype
    | SPush of sident * sexpr * datatype
    | SEosLit

type sdecl =
	SVarDecl of datatype * sident
	| SVarAssignDecl of datatype * sident * sval

type sstmt = 
	SBlock of sstmt list
	| SSExpr of sexpr
	| SReturn of sexpr
	| SDeclaration of sdecl
	| SAssign of sident * sexpr
  | STransition of sident * sexpr 

type snode = 
  SNode of sident * sstmt 

type sdfastr = {
  sreturn: datatype;
  sdfaname : ident;
  sformals : formal list;
  svar_body : sstmt list;
  snode_body: snode list;
}

type sdfa_decl =
	SDfa_Decl of sdfastr * datatype

type sprogram = 
  Prog of sdfa_decl list
  
  
  
  
