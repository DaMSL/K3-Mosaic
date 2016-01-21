open Util

(* Error handlers *)
let handle_lexer_error () =
  print_endline ("Lexer failure");
  exit 1

let handle_local_parse_error lexbuf =
  let curpos = lexbuf.Lexing.lex_curr_p in
  let curr = curpos.Lexing.pos_cnum in
  let bol = curpos.Lexing.pos_bol in
  let diff = curr-bol in
  let line = curpos.Lexing.pos_lnum in
  let tok = Lexing.lexeme lexbuf in
  Printf.eprintf "\nError on line %d , character %d , token %s\n"
    line diff tok; print_newline (); exit 1

let handle_parse_error ?(msg = "") lexbuf =
  print_endline ("Lexer reached: '"^(Lexing.lexeme lexbuf)^"'; "^msg);
  exit 1

(* Helpers *)
let error s = prerr_endline s; exit 1

let parse parsefn lexfn lexbuf =
  try parsefn lexfn lexbuf
  with
  | Parsing.Parse_error -> handle_local_parse_error lexbuf
  | Failure(msg) -> handle_parse_error ~msg:msg lexbuf
  | Pervasives.Exit -> handle_local_parse_error lexbuf

let parse_program parsefn lexfn file =
  let in_chan = try open_in file
    with Sys_error _ -> error ("failed to open file: "^file) in
  let lexbuf =
    try Lexing.from_channel in_chan
    with Failure _ -> handle_lexer_error () in
  let prog = parse parsefn lexfn lexbuf in
    close_in in_chan;
    prog

let parse_k3_file = parse_program K3Parser.program K3Lexer.tokenize

let parse_m3_file =
    parse_program Calculusparser.mapProgram Calculuslexer.tokenize

let parse_k3_prog s = parse K3Parser.program K3Lexer.tokenize @@ Lexing.from_string s
let parse_k3_expr s = parse K3Parser.expr K3Lexer.tokenize @@ Lexing.from_string s

