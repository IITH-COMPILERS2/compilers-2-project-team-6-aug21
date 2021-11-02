(* file: main.ml *)
(* Assumes the parser file is "parser.mly" and the lexer file is "lexer.mll". *)
exception InvalidProgram
open Printf

let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let prog = Parser.program Lexer.tokens lexbuf in
	 Semantic.check_program prog 
	 with End_of_file -> printf "Hurray"; exit 0
let _ = Printexc.print main ()
					
