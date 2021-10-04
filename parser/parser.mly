%{ open Ast 
    let parse_error s = print_endline s; (* Called by parser on error *)
		flush stdout
%}

/* Tokens defined according to the preference */
%token SEMICOLON LPAREN RPAREN LBRACE RBRACE COMMA RBRAC LBRAC COLON DOT LBRACKET RBRACKET APOS
%token PLUS MINUS MULTIPLICATION DIV ASSIGN PUSH POP PEEK CONCATENATE
%token EQ NEQ LT LEQ GT GEQ MOD
%token BITAND BITOR AND OR NOT TRUE FALSE
%token RETURN SSTATE DEFSTATE ESTATE STATE PRINT DEFSTATE ARROW 
%token DFA STACK
%token <int> INTEGER_LITERAL
%token <string>  ID
%token <string> STRING_LITERAL
%token <string> STRING_LITERAL TYPE STATE
%token <float> FLOAT_LITERAL
%token EOF EOS
%token MAIN BOOLEAN
%token STRING INT VOID FLOAT DOUBLE

%right ASSIGN
%left OR BITAND BITOR
%left AND
%right NOT
%left EQ NEQ LT GT LEQ GEQ
%left PLUS MINUS
%left MULTIPLICATION DIV MOD
%right UMINUS
%left PUSH POP PEEK
%nonassoc LPAREN RPAREN LBRAC RBRAC LBRACKET RBRACKET


/* start program */
%start program
%type <Ast.program> program

%%

/* Grammer Rules */

program:
  {[]}
  | automaton_declaration program { $1 :: $2 }

Data_type:
    | VOID                  {print_endline "found void"; VoidType}
    | INT                   {print_endline "found int"; IntType}
    | FLOAT                 {print_endline "found float"; FloatType}
    | STRING                {print_endline "found string"; StringType}

return_type:
    | Data_type {Datatype($1)} 
    | STACK LT Data_type GT {Stacktype(Datatype($3))}
    
automaton_declaration:
    return_type DFA ID LPAREN formals_opt RPAREN LBRACE vdecl_list node_list RBRACE
    { 
      { 
        return = $1;
        dfa_name =  Ident($3);
        formals = $5;
        var_body = $8; 
        node_body = $9
      }
    } 

vdecl_list:
    | {[]}
    | variable_declaration vdecl_list { $1 :: $2 } 

variable_declaration:
    | Data_type ID SEMICOLON             { print_endline "int declared";VarDecl(Datatype($1), Ident($2)) }
    | Data_type ID ASSIGN expr SEMICOLON { print_endline "int assigned to identifier";VarAssignDecl(Datatype($1), Ident($2), ExprVal($4))} 
    | STACK LT Data_type GT ID SEMICOLON { print_endline "stack declared";VarDecl(Stacktype(Datatype($3)), Ident($5)) }

node_list:
    | {[]}
    | node node_list { $1 :: $2 }

node:
  | STATE LBRACE statement_list RBRACE {print_endline "found transition"; Node(Ident($1), $3) }

statement_list:
	| {[]}
	| statement statement_list { $1 :: $2 }

/* TODO: add method calls */
statement:
  	| RETURN expr SEMICOLON  {print_endline "found return statement";Return($2)}
  	| expr ARROW STATE SEMICOLON {print_endline "change in transition";Transition(Ident($3),$1)} 
  	| DEFSTATE ARROW STATE SEMICOLON {print_endline "default transition";Transition(Ident($3),IntLit(1))} /*Star evaluates to IntLit 1 because that's True in StateMap*/
  	| variable_declaration {print_endline "variable declaration"; Declaration($1)}
    | ID ASSIGN expr SEMICOLON { print_endline "assignig value";Assign(Ident($1), $3) } /*Assignment post-declaration*/
  	| expr SEMICOLON {Expr($1)}
    | RETURN SEMICOLON {print_endline "found return";Return(IntLit(1))}
    | PRINT LPAREN expr RPAREN SEMICOLON        {print_endline "found print statement"; Print($3)}
    /*| PRINT LPAREN APOS ID APOS RPAREN SEMICOLON        {print_endline "found printf statement"; Print2($2)}     | PRINT LPAREN APOS expr APOS RPAREN SEMICOLON        {Print($4)} */

formals_opt:
    | {[]} /*nothing*/
    | formal_list { List.rev $1}

formal_list:
    | param { [$1] }
    | formal_list COMMA param {print_endline "function parameters"; $3 :: $1}

param:
      | Data_type ID { print_endline "function parameter";Formal(Datatype($1),Ident($2)) }
      | STACK LT Data_type GT ID { print_endline "function parameter(stack)";Formal(Stacktype(Datatype($3)), Ident($5)) }

expr_list:
    | {[]}
    | expr COMMA expr_list { $1 :: $3 }
    | expr { [$1] }

expr:
  | INTEGER_LITERAL                       {  print_endline "found digit";IntLit($1)   }
  | STRING_LITERAL                    {  print_endline "found string";StringLit($1) }
  | FLOAT_LITERAL                     {  print_endline "found float";FloatLit($1) }
  | ID                                {  print_endline "found ID";Variable(Ident($1))  }
  | EOS                               {  EosLit }  
  | expr PLUS   expr                  {  print_endline "found +";BinaryOp($1, Add,   $3) }
  | expr MINUS  expr                  {  print_endline "found -";BinaryOp($1, Sub,   $3) }
  | expr MULTIPLICATION  expr         {  print_endline "found *";BinaryOp($1, Mult,  $3) }
  | expr DIV expr                     {  print_endline "found /";BinaryOp($1, Div,   $3) }
  | expr EQ     expr                  {  print_endline "found =";BinaryOp($1, Equal, $3) }
  | expr NEQ    expr                  {  print_endline "found !=";BinaryOp($1, Neq,   $3) }
  | expr LT     expr                  {  print_endline "found <";BinaryOp($1, Lt,  $3) }
  | expr LEQ    expr                  {  print_endline "found <=";BinaryOp($1, Leq,   $3) }
  | expr GT     expr                  {  print_endline "found >";BinaryOp($1, Gt,$3)}
  | expr GEQ    expr                  {  print_endline "found >=";BinaryOp($1, Geq,   $3) }
  | expr MOD    expr                  {  print_endline "found mod";BinaryOp($1, Mod,   $3) }
  | expr AND expr                     {  print_endline "found AND";Brela($1, And, $3) } 
  | expr OR expr                      {  print_endline "found OR"; Brela($1, Or, $3) } 
  | MINUS expr %prec UMINUS           {  Unop(Neg, $2) }
  | NOT   expr                        {  Unop(Not, $2) }
  | LPAREN expr RPAREN                {  $2 }
  | ID DOT POP LPAREN RPAREN          {  print_endline "found pop";Pop(Ident($1)) } 
  | ID DOT PUSH LPAREN expr RPAREN    {  print_endline "found push";Push(Ident($1), $5) }
  | ID DOT PEEK LPAREN RPAREN         {  print_endline "found peek";Peek(Ident($1)) } 
  | ID LPAREN expr_list RPAREN        {  Call(Ident($1), $3) (*call a sub dfa*)}
