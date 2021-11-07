{ 
	open Printf
	open Parser
}
let digit  = ['0'-'9']
let character = ['A'-'Z' 'a'-'z']

rule tokens = 
	parse 
| [' ' '\t' '\r' '\n'] { tokens lexbuf } 
| "/*"     { print_endline "multiline comments start"; comment lexbuf } 
| "//"     { print_endline "singleline comments start";singleComment lexbuf } 
| ';'      { SEMICOLON}
| ':'      { COLON }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ','      { COMMA }
| '.'      { DOT }
| '"'      { APOS} 

| '+'      { PLUS }
| '-'      { MINUS}
| '*'      { MULTIPLICATION }
| '/'      { DIV }
| '^'      { CONCATENATE }
| '='      { ASSIGN }
| "=="     { EQ }
| '!'      { NOT }
| "!="     { NEQ }
| "&&"     { AND }
| "||"     { OR }
|"true"	   { TRUE }
|"false"   { FALSE }
| '<'      { LT }
| "<-"     { ARROW }
| "<="     { LEQ }
| '>'      { GT }
| ">="     { GEQ }
| '%'      { MOD }
| "return" { RETURN }
| "print"  { PRINT} 
| '$'      { DEFSTATE}

| "int"    { INT }
| "float"  { FLOAT }
| "string" { STRING }
| "void"   { VOID }
| "DFA"    { DFA }
| "stack"  { STACK }
| "pop"    { POP }
| "peek"   { PEEK }
| "push"   { PUSH }
| "EOS"    { EOS }
| (digit)+ as lexemme                {INTEGER_LITERAL(int_of_string lexemme)}
| (digit)+ '.'(digit)+ as lexemme    {FLOAT_LITERAL(float_of_string lexemme)}
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { STATE(lxm) }
| '"' (('\\' _  | [^'"'])* as lxm) '"'{ STRING_LITERAL(lxm) }
| ((['0'-'9']+('.'['0'-'9']*|('.'?['0'-'9']*'e'('+'|'-')?))['0'-'9']*) |
(['0'-'9']*('.'['0'-'9']*|('.'?['0'-'9']*'e'('+'|'-')?))['0'-'9']+)) 
    as lxm { FLOAT_LITERAL(float_of_string lxm) }
| eof      { EOF }
| _        {printf "INVALID TOKEN";tokens lexbuf}
| _ {tokens lexbuf }
and comment = parse
|  "*/" { print_endline "multiline comments end"; tokens lexbuf }
| _    { comment lexbuf }

and singleComment = parse
|  '\n' { print_endline "singleline comment end"; tokens lexbuf }
| _    { singleComment lexbuf }
