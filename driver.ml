(* Driver for the K3 programming language. Reads K3 syntax, outputs AST *)

open K3

let main () =
    let lexbuf = Lexing.from_channel stdin in
    let e = K3Parser.line K3Lexer.tokenize lexbuf
    in print_endline(string_of_expr e)

let _ = Printexc.print main ()
