open Printf

let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let prog = Parser.program Lexer.tokens lexbuf in
    let code=Semantic.check_program prog in
         Gen_python.gen_program code 
	 with End_of_file -> printf "Hurray"; exit 0
let _ = Printexc.print main ()
					
