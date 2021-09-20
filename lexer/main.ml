open Printf
let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    (* while true do *)
      Parser.datatype Lexer.tokens lexbuf
    (* done *)
  with End_of_file -> printf "plp"; exit 0
let _ = Printexc.print main ()
