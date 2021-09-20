{
    open Printf
    open Parser
}
let digit  = ['0'-'9']
let character = ['A'-'Z' 'a'-'z']
rule tokens = parse
    [' ' '\t' '\r' '\n']    { tokens lexbuf }
    | "/*"                  {print_endline "multiline comments start";comment lexbuf }   
    | "//"                  {print_endline "singleline comments start";singleComment lexbuf }          
    |';'                    {print_endline "SEMICOLON";tokens lexbuf}
    |':'                    {print_endline "COLON";tokens lexbuf} 
    |'"'                    {print_endline "APOS";tokens lexbuf}   
    |'('                    {print_endline "LPAREN";tokens lexbuf}
    |')'                    {print_endline "RPAREN";tokens lexbuf}
    |'{'                    {print_endline "LBRACE";tokens lexbuf}
    |'}'                    {print_endline "RBRACE";tokens lexbuf}
    |'['                    {print_endline "LBRACKET";tokens lexbuf}
    |']'                    {print_endline "RBRACKET";tokens lexbuf}
    |','                    {print_endline "COMMA";tokens lexbuf}
    |'+'                    {print_endline "PLUS";tokens lexbuf}
    |'-'                    {print_endline "MINUS";tokens lexbuf}
    |'*'                    {print_endline "MULTIPLICATION";tokens lexbuf}
    |'/'                    {print_endline "DIV";tokens lexbuf}
    |'%'                    {print_endline "MOD";tokens lexbuf} 
    |'.'                    {print_endline "ACCESS";tokens lexbuf}
    |'^'                    {print_endline "CONCATENATE";tokens lexbuf}
    |'='                    {print_endline "ASSIGN";tokens lexbuf}
    |"=="                   {print_endline "EQUAL";tokens lexbuf}
    |"!="                   {print_endline "NEQ";tokens lexbuf}
    |'<'                    {print_endline "LT";tokens lexbuf}
    |"<="                   {print_endline "LTQ";tokens lexbuf}
    |'>'                    {print_endline "GT";tokens lexbuf}
    |">="                   {print_endline "GTQ";tokens lexbuf}
    | '&'                   {print_endline "BITAND";tokens lexbuf}
    | '|'                   {print_endline "BITOR";tokens lexbuf}
    |"&&"                   {print_endline "AND";tokens lexbuf}
    |"||"                   {print_endline "OR";tokens lexbuf}
    |'!'                    {print_endline "NOT";tokens lexbuf}
    | "->"                  {print_endline "ARROW";tokens lexbuf} 
    |"int"                  {print_endline "INT";tokens lexbuf}
    |"void"                 {print_endline "VOID";tokens lexbuf}
    |"DFA"                  {print_endline "DFA";tokens lexbuf}
    |"$"                    {print_endline "DEFSTATE";tokens lexbuf}
    |"double"               {print_endline "DOUBLE";tokens lexbuf}
    |"string"               {print_endline "STRING";tokens lexbuf}
    |"boolean"              {print_endline "BOOLEAN";tokens lexbuf}
    |"main"                 {print_endline "MAIN FUC";tokens lexbuf}
    |"return"               {print_endline "RETURN";tokens lexbuf}
    |"Start"                {print_endline "SSTATE";tokens lexbuf}
    |"Exit"                 {print_endline "ESTATE";tokens lexbuf}
    |"input"                {print_endline "INPUT";tokens lexbuf}
    |"print"                {print_endline "PRINT";tokens lexbuf}   
    |"true"		     {print_endline "TRUE";tokens lexbuf}
    |"false"	             {print_endline "FALSE";tokens lexbuf}
    |(digit)+ '.'(digit)+   {print_endline "FLOAT";tokens lexbuf}
    |digit+                          {print_endline "DIGIT";tokens lexbuf}
    |'"' [^'"' '\n']*'"'          {print_endline "STRING";tokens lexbuf}
    |['a' - 'z'](character|digit)*   {print_endline "VARIABLE";tokens lexbuf}
    |['A' - 'Z'](character|digit)*   {print_endline "STATE";tokens lexbuf}
    | eof                            {EOF} 
    | _                              {print_endline "INVALID TOKEN";tokens lexbuf}
  
    | _ {tokens lexbuf }

and comment = parse
  "*/"                               {print_endline "multiline comments end"; tokens lexbuf }
| _                                  {comment lexbuf }

and singleComment = parse
  '\n'                               {print_endline "singleline comment end"; tokens lexbuf}
| _                                  {singleComment lexbuf}

