(* Driver for the K3 programming language. *)

open Arg
open Util
open K3.AST
open K3.Annotation
open K3Values
open K3Util
open K3Printing
open K3Typechecker
open K3Streams
open K3Consumption
open K3Interpreter
open Testing
open ReifiedK3

(* Note these override module names *)
module Imperative = Imperative.AST(CPP.CPPTarget)
module ImperativeUtil = ImperativeUtil.Util(CPP.CPPTarget)
module RK3ToImperative = RK3ToImperative.Make(CPP.CPPTarget)

module CPPExt = CPP.CPPTarget
module CPPAST = CPP.CPPTarget.ASTImport
module CPPGen = CPP.CPPGenerator
open ImperativeToCPP


(* Helpers *)
let error s = prerr_endline s; exit 1

(* Compilation languages *)
type in_lang_t = K3in | M3in

let in_lang_descs = [
    K3in,     "k3",  "K3";
    M3in,     "m3",  "M3";
  ]

type out_lang_t = K3 | K3Dist | ReifiedK3 | Imperative | CPPInternal | CPP

let out_lang_descs = [
    K3,          "k3",     "K3";
    K3Dist,      "k3dist", "Distributed K3";
    ReifiedK3,   "rk3",    "Reified K3";
    Imperative,  "imp",    "Imperative";
    CPPInternal, "cppi",   "C++-internal";
    CPP,         "cpp",    "C++";
  ]

let format_language_description (lang_t, short_desc, long_desc) =
  let pad_or_truncate_str s len =
    let s_len = String.length s in
    if s_len > len then String.sub s 0 len
    else s^(String.make (len - s_len) ' ')
  in
  "  "^(pad_or_truncate_str short_desc 10)^long_desc

let parse_lang data str s = 
  try
    let (x,_,_) = List.find (fun (_,s2,_) -> s = s2) data
    in x
  with Not_found -> error ("Invalid "^str^": "^s)

let parse_in_lang = parse_lang in_lang_descs "input language"
let parse_out_lang = parse_lang out_lang_descs "output language"

(* Actions *)
type action_t = REPL | Compile | Interpret | Print | ExpressionTest

let action_descriptions = [
    REPL,             "-top",  "Interactive toplevel";
    Compile,          "-c",    "Compile to specified language";
    Interpret,        "-r",    "Interpret with specified language";
    Print,            "-p",    "Print program as specified language";
    ExpressionTest,   "-test", "Unit test for expressions"
  ]

let action_specs action_param = List.map (fun (act, flag, desc) -> 
  let fn () = action_param := act
  in (flag, Arg.Unit fn, "       "^desc)) action_descriptions

(* Driver parameters *)
type parameters = {
    action : action_t ref;
    mutable in_lang : in_lang_t;
    mutable out_lang : out_lang_t;
    mutable search_paths : string list;
    mutable input_files : string list;
    mutable node_address : address;
    mutable peers : address list;
    mutable print_types : bool; (* TODO: change to a debug flag *)
    mutable debug_info : bool;
  }

let cmd_line_params : parameters = {
    action       = ref Print;
    in_lang      = K3in;
    out_lang     = K3;
    search_paths = ["."];
    input_files  = [];
    node_address = ("127.0.0.1", 10000);
    peers        = [];
    print_types  = false;
    debug_info   = false;
  }

(* General parameter setters *)
let set_output_language l =
  cmd_line_params.out_lang <- parse_out_lang l

let set_input_language l =
  cmd_line_params.in_lang <- parse_in_lang l

let set_print_types () =
  cmd_line_params.print_types <- true

let set_debug_info () =
  cmd_line_params.debug_info <- true

let append_search_path p = 
  cmd_line_params.search_paths <- cmd_line_params.search_paths @ [p]

let append_input_file f = 
  cmd_line_params.input_files <- cmd_line_params.input_files @ [f]
  
(* Address parameter setters *)
let parse_ip ip_str = match Str.split (Str.regexp (Str.quote ":")) ip_str with
  | [ip; port] -> ip, (int_of_string port)
  | _ -> invalid_arg "invalid ip string format"

let set_node_address ip_str =
  cmd_line_params.node_address <- parse_ip ip_str
  
let append_peers ip_str_list =
  let ips = Str.split (Str.regexp (Str.quote ",")) ip_str_list in
  cmd_line_params.peers <- cmd_line_params.peers @ (List.map parse_ip ips)

let param_specs = Arg.align (action_specs cmd_line_params.action@[
  "-i", Arg.String set_input_language, 
      "lang   Set the compiler's input language";
  "-l", Arg.String set_output_language, 
      "lang   Set the compiler's output language";
  "-I", Arg.String append_search_path, 
      "dir    Include a directory in the module search path";
  "-h", Arg.String set_node_address, 
      "addr   Set the current node address for evaluation";
  "-n", Arg.String append_peers, 
      "[addr] Append addresses to the peer list";
  "-t", Arg.Unit set_print_types,
      "       Print types as part of output";
  "-d", Arg.Unit set_debug_info,
      "       Print debug info (context specific)";
  ])

let usage_msg =
  ("k3 [opts] sourcefile1 [sourcefile2 [...]]"^
     "\n---- Input languages ----\n"^
     (String.concat "\n"
       (List.map format_language_description in_lang_descs))^
     "\n---- Output languages ----\n"^
     (String.concat "\n"
       (List.map format_language_description out_lang_descs))^
     "\n---- Options ----")

let parse_cmd_line () =
  Arg.parse param_specs append_input_file usage_msg


(* Error handlers *)
let handle_lexer_error () =
  print_endline ("Lexer failure");
  exit 1

let handle_parse_error ?(msg = "") lexbuf =
  print_endline ("Lexer reached: '"^(Lexing.lexeme lexbuf)^"'; "^msg);
  exit 1

let handle_type_error p (uuid,error) =
  print_endline "----Type error----";
  print_endline ("Error("^(string_of_int uuid)^"): "^error);
  print_endline (string_of_program ~print_id:true p);
  exit 1


(* Program parsers *)
let parse_program parsefn lexfn file =
  let in_chan = try open_in file
    with Sys_error _ -> error ("failed to open file: "^file) in
  let lexbuf =
    try Lexing.from_channel in_chan
    with Failure _ -> handle_lexer_error ()
  in
  let prog =
    try parsefn lexfn lexbuf
    with 
    | Parsing.Parse_error -> 
        let curpos = lexbuf.Lexing.lex_curr_p in
        let curr = curpos.Lexing.pos_cnum in 
        let bol = curpos.Lexing.pos_bol in
        let diff = curr-bol in
        let line = curpos.Lexing.pos_lnum in
        let tok = Lexing.lexeme lexbuf in  
        Printf.printf "\nError on line %d , character %d , token %s\n" 
            line diff tok; raise Parsing.Parse_error
    | Failure(msg) -> handle_parse_error ~msg:msg lexbuf
    | Pervasives.Exit -> raise Parsing.Parse_error
  in
    close_in in_chan;
    prog

let parse_program_k3 = parse_program K3Parser.program K3Lexer.tokenize

let parse_program_m3 = 
    parse_program Calculusparser.mapProgram Calculuslexer.tokenize


(* Program transformers *)
let typed_program p =
  try deduce_program_type p
  with TypeError (uuid, error) -> handle_type_error p (uuid, error)

let imperative_program p =
  RK3ToImperative.imperative_of_program (fun () -> []) (typed_program p)
  
let cpp_program p = 
  let mk_meta() = [] in
  CPPTyping.deduce_program_type @:
    cpp_of_imperative mk_meta @:
      RK3ToImperative.imperative_of_program mk_meta @: typed_program p


(* Action handlers *)
(* TODO *) 
let repl params = ()

(* TODO *)
let compile params = ()

(* Interpret actions *)
(* TODO: accept role from command line *)
let interpret params =
  let eval_fn = match params.out_lang with
    | K3 -> (fun f -> 
      let typed_program = deduce_program_type @: parse_program_k3 f
      in eval_program params.node_address None typed_program)
    | _ -> error "Output language not yet implemented"
  in
  List.iter eval_fn params.input_files

(* Print actions *)
let print_event_loop (id, (res_env, ds_env, instrs)) = 
    print_endline ("----Role "^id^" Flow Program----");
    print_string (string_of_resource_env res_env);
    print_string (string_of_dispatcher_env ds_env)

let string_of_typed_meta (t,a) = string_of_annotation a

let print_k3_program p =
  let tp = typed_program p in
  let event_loops, default = roles_of_program tp in
    print_endline (string_of_program tp);
    List.iter print_event_loop event_loops;
    match default with None -> () 
      | Some (_,x) -> print_event_loop ("DEFAULT", x)

let print_k3_dist_prog (p, m) = match m with
  | None -> error "Cannot construct distributed K3 without ProgInfo metadata"
  | Some meta ->
  let tp = typed_program p in
  let dist = try
      GenDist.gen_dist meta tp
    with Invalid_argument(msg) -> 
      print_endline ("ERROR: " ^msg);
      print_endline (ProgInfo.string_of_prog_data meta);
      exit (-1)
  in
  let dist_tp = typed_program dist in
  let event_loops, default = roles_of_program dist_tp in 
    print_endline (string_of_program dist_tp);
    List.iter print_event_loop event_loops;
    match default with None -> () 
        | Some (_,x) -> print_event_loop ("DEFAULT", x)

let print_reified_k3_program p =
  let print_expr_fn ?(print_id=false) e = 
      lazy (print_reified_expr (reify_expr [] e)) in
  let tp = typed_program p in 
  print_endline (string_of_program ~print_expr_fn:print_expr_fn tp)

let print_imp func print_types f =
  let string_of_meta m =
    (if print_types then (ImperativeUtil.string_of_type (fst m))^";" else "")^
    (string_of_annotation (snd m))
  in print_endline @: 
    ImperativeUtil.string_of_program string_of_meta (func f)

let print_cppi_program = print_imp cpp_program
let print_imperative_program = print_imp imperative_program

let print_cpp_program f = 
  let files_and_content = CPPGen.generate_program (cpp_program f) in
  let mk_filename id = (String.make 40 '=')^" "^id in 
  let print_content (f,c) =
    print_endline (mk_filename f); print_endline c; print_endline "\n\n"
  in
  List.iter print_content files_and_content
  
(* Top-level print handler *)
let print params =
  let print_fn = match params.out_lang with
    | K3 -> print_k3_program |- fst
    | K3Dist -> params.in_lang <- M3in; print_k3_dist_prog
    | ReifiedK3 -> print_reified_k3_program |- fst
    | Imperative -> (print_imperative_program params.print_types) |- fst
    | CPPInternal -> (print_cppi_program params.print_types) |- fst
    | CPP -> print_cpp_program |- fst
  in
  let read_fn = match params.in_lang with
    | K3in -> fun f -> (parse_program_k3 f, None)
    | M3in -> fun f -> (
        let m3prog = parse_program_m3 f in
        let proginfo = M3ProgInfo.prog_data_of_m3 m3prog in
        if params.debug_info then 
            print_endline (ProgInfo.string_of_prog_data proginfo);
        (M3ToK3.m3_to_k3 m3prog, Some proginfo)
      )
  in
  List.iter (print_fn |- read_fn) params.input_files

(* Test actions *)
let print_test_case (decls,e,x) =
  print_endline "----Decls----";
  print_endline (string_of_program decls);
  print_endline "----Expression----";
  print_endline (string_of_expr e);
  print_endline "----Expected----";
  print_endline (string_of_expr x)

let test params =
  let test_fn = match params.out_lang with
    | K3 -> (fun f -> 
      let test_triples = parse_expression_test (read_file f) in
      let test_cases = snd (List.fold_left (fun (i, test_acc) (decls, e, x) ->
          (* print_test_case (decls,e,x); *)
          let name = f^" "^(string_of_int i) in
          let test_case = case name @: eval_test_expr (decls, e) @=? eval_test_expr (decls, x) 
          in i+1, test_acc@[test_case]
        ) (0, []) test_triples)
      in List.iter run_tests test_cases)
    | _ -> error "Testing language not yet implemented"
  in List.iter test_fn params.input_files


(* Driver execution *)
let process_parameters params = match !(params.action) with
  | REPL -> repl params
  | Compile -> compile params
  | Interpret -> interpret params
  | Print -> print params
  | ExpressionTest -> test params

let main () =
  parse_cmd_line();
  if (List.length cmd_line_params.input_files) < 1 then
    (Arg.usage param_specs usage_msg;
     error "\nNo input files specified");
  
  process_parameters cmd_line_params

let _ = Printexc.print main ()
