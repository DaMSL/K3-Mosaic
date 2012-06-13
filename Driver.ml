(* Driver for the K3 programming language. Reads K3 syntax, outputs AST *)

open K3
open K3Util

let main () =
    let lexbuf = Lexing.from_channel stdin in
    let p = K3Parser.program K3Lexer.tokenize lexbuf
    in print_endline(string_of_program p)

let _ = Printexc.print main ()
