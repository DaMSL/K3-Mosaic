(* Driver for the K3 programming language. *)

open Arg
open Str
open Constants
open Util
open K3.AST
open K3Printing
open K3.Annotation
open K3Values
open K3Util
open K3Typechecker
open K3Streams
open K3Consumption
open K3Interpreter
open K3Runtime
open Testing
open ReifiedK3

(* Note these override module names *)
module Imperative = Imperative.AST(CPP.CPPTarget)
module ImperativeUtil = ImperativeUtil.Util(CPP.CPPTarget)
module RK3ToImperative = RK3ToImperative.Make(CPP.CPPTarget)

module CPPExt = CPP.CPPTarget
module CPT = CPP.CPPTarget.ASTImport
module CPPGen = CPP.CPPGenerator
open ImperativeToCPP

module PS = K3PrintSyntax

(* Helpers *)
let error s = prerr_endline s; exit 1

(* Compilation languages *)
type in_lang_t = K3in | M3in

let in_lang_descs = [
    K3in,     "k3",  "K3";
    M3in,     "m3",  "M3";
  ]

type out_lang_t = K3 | AstK3| K3Dist | AstK3Dist | ReifiedK3 | Imperative | CPPInternal | CPP

let out_lang_descs = [
    K3,          "k3",        "K3";
    AstK3,       "k3ast",    "K3 AST";
    K3Dist,      "k3dist",    "Distributed K3";
    AstK3Dist,   "k3distast", "Distributed K3 AST";
    ReifiedK3,   "rk3",       "Reified K3";
    Imperative,  "imp",       "Imperative";
    CPPInternal, "cppi",      "C++-internal";
    CPP,         "cpp",       "C++";
  ]

(* Evaluation option setters *)
let parse_port p = 
  let error () = invalid_arg ("invalid port: "^p) in
  try let r = int_of_string p in if r > 65535 then error() else r
  with Failure _ -> error()

let parse_ip_role ipr_str =
  match Str.full_split (Str.regexp "[:/]") ipr_str with
  | [Text ip; Delim ":"; Text port; Delim "/"; Text role] -> (ip, (parse_port port)), Some(role)
  | [Text ip; Delim ":"; Text port] -> (ip, (parse_port port)), None
  | [Text ip_or_port; Delim "/"; Text role] -> 
    if String.contains ip_or_port '.' then (ip_or_port, default_port), Some(role)
    else (default_ip, (parse_port ip_or_port)), Some(role)
  | _ -> invalid_arg "invalid ip string format"

let string_of_address_and_role (addr, role_opt) =
  (string_of_address addr)^(match role_opt with None -> "" | Some r -> "/"^r)

let string_of_lang_descs descs i = 
  let l = List.filter (fun (x,_,_) -> x = i) descs in
  if l = [] then "unknown" else let _,_,r = List.hd l in r

let string_of_in_lang = string_of_lang_descs in_lang_descs
let string_of_out_lang = string_of_lang_descs out_lang_descs

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

(* Spec helpers *)
let setter_specs param spec_desc = List.map (fun (param_val, flag, desc) -> 
  let fn () = param := param_val
  in (flag, Arg.Unit fn, "         "^desc)) spec_desc

(* Testing modes *)
type test_mode_t = ExpressionTest | ProgramTest

let test_descriptions = [
    ExpressionTest,   "-expr",  "Use expression test input";
    ProgramTest,      "-prog",  "Use program test input";
  ]

let test_specs test_mode_param = setter_specs test_mode_param test_descriptions

let string_of_test_mode = function ExpressionTest -> "Expression" | ProgramTest -> "Program"

(* Actions *)
type action_t = REPL | Compile | Interpret | Print | Test

let action_descriptions = [
    Print,            "-p",    "Print program as specified language";
    Compile,          "-c",    "Compile to specified language";
    Interpret,        "-eval", "Interpret with specified language";
    Test,             "-test", "K3 testing";
    REPL,             "-top",  "Interactive toplevel";
  ]

let action_specs action_param = setter_specs action_param action_descriptions

(* Driver parameters *)
type parameters = {
    action               : action_t ref;
    test_mode            : test_mode_t ref;
    mutable in_lang      : in_lang_t;
    mutable out_lang     : out_lang_t;
    mutable search_paths : string list;
    mutable input_files  : string list;
    mutable node_address : address;
    mutable role         : id_t option;
    mutable peers        : (address * id_t option) list;
    mutable partition_map : K3Route.part_map_t;
    mutable run_length   : int64;
    mutable print_types  : bool; (* TODO: change to a debug flag *)
    mutable debug_info   : bool;
    mutable verbose      : bool;
  }

let cmd_line_params : parameters = {
    action       = ref Print;
    test_mode    = ref ProgramTest;
    in_lang      = K3in;
    out_lang     = K3;
    search_paths = default_search_paths;
    input_files  = [];
    node_address = default_node_address;
    role         = default_role;
    peers        = default_peers;
    partition_map = [];
    run_length   = default_run_length;
    print_types  = default_print_types;
    debug_info   = default_debug_info;
    verbose      = default_verbose;
  }

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
  Printf.printf "\nError on line %d , character %d , token %s\n" 
      line diff tok; raise Parsing.Parse_error

let handle_parse_error ?(msg = "") lexbuf =
  print_endline ("Lexer reached: '"^(Lexing.lexeme lexbuf)^"'; "^msg);
  exit 1

let handle_type_error p (uuid, name, msg) =
  let s = K3TypeError.string_of_error msg in
  print_endline "----Type error----";
  print_endline @: "Error("^(string_of_int uuid)^"): "^name^": "^s;
  if cmd_line_params.debug_info then
    (print_endline @: 
      string_of_program ~print_id:true ~verbose:cmd_line_params.verbose p;)
  else 
    print_endline @: PS.string_of_program ~uuid_highlight:(Some uuid) p;
  exit 1

let handle_interpret_error p (uuid,error) =
  print_endline "----Interpreter error----";
  print_endline @: "Error("^(string_of_int uuid)^"): "^error;
  print_endline @: string_of_program ~print_id:true p;
  exit 1

(* Program parsers *)
let parse_program_from_string parsefn lexfn str =
  let lexbuf =
    try Lexing.from_string str
    with Failure _ -> handle_lexer_error ()
  in
    try parsefn lexfn lexbuf
    with 
    | Parsing.Parse_error -> handle_local_parse_error lexbuf
    | Failure(msg) -> handle_parse_error ~msg:msg lexbuf
    | Pervasives.Exit -> raise Parsing.Parse_error

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
    | Parsing.Parse_error -> handle_local_parse_error lexbuf
    | Failure(msg) -> handle_parse_error ~msg:msg lexbuf
    | Pervasives.Exit -> raise Parsing.Parse_error
  in
    close_in in_chan;
    prog

let parse_program_k3 = parse_program K3Parser.program K3Lexer.tokenize

let parse_program_m3 = 
    parse_program Calculusparser.mapProgram Calculuslexer.tokenize


(* Program transformers *)
let typed_program_with_globals p =
  try deduce_program_type @: 
      K3Global.add_globals cmd_line_params.node_address cmd_line_params.peers p
  with TypeError (a,b,c) -> handle_type_error p (a,b,c) 

(* for most functions, we don't need the globals included *)
let typed_program p =
  K3Global.remove_globals cmd_line_params.node_address cmd_line_params.peers @: 
      typed_program_with_globals p

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
let compile params inputs = ()

(* Interpret actions *)
let interpret_k3_program params p = 
  let tp = typed_program_with_globals p in 
  configure_scheduler params.run_length;
  try
    begin match params.peers with
    | []    -> ignore(eval_program params.node_address params.role tp)
    | nodes -> 
        let peers = 
        let skip_primary = 
            List.exists (fun (addr,_) -> addr = params.node_address) nodes in
        let ipr = params.node_address, params.role in
        (if skip_primary then [] else [ipr])@nodes
        in 
        List.iter (fun ipr -> 
            print_endline @: "Starting node "^(string_of_address_and_role ipr)
        ) peers;
        ignore(eval_networked_program peers tp)
    end
  with RuntimeError (uuid,str) -> handle_interpret_error p (uuid,str)

let interpret params inputs =
  let eval_fn = match params.out_lang with
    | K3 | AstK3 | K3Dist | AstK3Dist -> interpret_k3_program params
    | _ -> error "Output language not yet implemented"
  in
  List.iter eval_fn inputs

(* Print actions *)
let print_event_loop (id, (res_env, ds_env, instrs)) = 
    print_endline ("----Role "^id^" Flow Program----");
    print_string (string_of_resource_env res_env);
    print_string (string_of_dispatcher_env ds_env)

let string_of_typed_meta (t,a) = string_of_annotation a

let print_k3_program f p =
  let tp = typed_program p in
  let event_loops, default = roles_of_program tp in
    print_endline (f tp);
    List.iter print_event_loop event_loops;
    match default with None -> () 
      | Some (_,x) -> print_event_loop ("DEFAULT", x)

let print_reified_k3_program p =
  let print_expr_fn c e = 
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
let print params inputs =
  let sofp = string_of_program ~verbose:cmd_line_params.verbose in
  let print_fn = match params.out_lang with
    | AstK3 | AstK3Dist -> print_k3_program sofp
    | K3 | K3Dist -> print_k3_program PS.string_of_program
    | ReifiedK3 -> print_reified_k3_program
    | Imperative -> print_imperative_program params.print_types 
    | CPPInternal -> print_cppi_program params.print_types
    | CPP -> print_cpp_program
  in List.iter print_fn inputs

(* Test actions *)
let print_test_case (decls,e,x) =
  print_endline "----Decls----";
  print_endline (string_of_program decls);
  print_endline "----Expression----";
  print_endline (string_of_expr e);
  print_endline "----Expected----";
  print_endline (string_of_expr x)

let test params =
  let test_fn = match !(params.test_mode), params.out_lang with
    | ExpressionTest, K3 ->
      (fun f -> test_expressions f @: parse_expression_test @: read_file f)
    
    | ProgramTest, K3 ->
      fun f -> let (p, cond) = parse_program_test @: read_file f in
               let p_test = (typed_program_with_globals p, cond) in
               begin try test_program f params.node_address params.role p_test
               with RuntimeError (uuid, str) -> 
                    handle_interpret_error (fst p_test) (uuid, str) end
    | x,y -> 
      let mode, lang = string_of_test_mode x, string_of_out_lang y
      in error @: mode^" testing not yet implemented for "^lang
  in List.iter test_fn params.input_files

let transform_to_k3_dist partmap p mproginfo = match mproginfo with
  | None -> error "Cannot construct distributed K3 without ProgInfo metadata"
  | Some meta ->
    (* do not use. Simply for type checking original program *)
    (*tp = typed_program p in *)
    try
      GenDist.gen_dist meta partmap p
    with Invalid_argument(msg) -> 
      print_endline ("ERROR: " ^msg);
      print_endline (ProgInfo.string_of_prog_data meta);
      exit (-1)

let process_inputs params =
  let proc_fn f = match params.in_lang with
    | K3in -> (parse_program_k3 f, None)
    | M3in -> let m3prog = parse_program_m3 f in
        let proginfo = M3ProgInfo.prog_data_of_m3 m3prog in
        if params.debug_info then 
            print_endline (ProgInfo.string_of_prog_data proginfo);
        let prog = M3ToK3.m3_to_k3 m3prog in
        (prog, Some proginfo)
  in List.map proc_fn params.input_files

(* this function can only transform to another k3 format *)
let transform params ds =
  let proc_fn (d, mproginfo) = match params.out_lang with
   | K3Dist
   | AstK3Dist -> transform_to_k3_dist params.partition_map d mproginfo
   | _         -> d
  in List.map proc_fn ds

(* Driver execution *)
let process_parameters params = 
  (* preprocess params *)
  if params.out_lang = K3Dist or params.out_lang = AstK3Dist then params.in_lang <- M3in;
  let a = !(params.action) in
  match a with
  | Compile | Interpret | Print -> 
    let inputs = transform params @: process_inputs params in
    begin match a with
    | Compile -> compile params inputs
    | Interpret -> interpret params inputs
    | Print -> print params inputs
    | _ -> ()
    end
  | REPL -> repl params
  | Test -> test params

(* General parameter setters *)
let set_output_language l =
  cmd_line_params.out_lang <- parse_out_lang l

let set_input_language l =
  cmd_line_params.in_lang <- parse_in_lang l

let set_print_types () =
  cmd_line_params.print_types <- true

let set_debug_info () =
  cmd_line_params.debug_info <- true

let set_verbose () =
  cmd_line_params.verbose <- true

let append_search_path p = 
  cmd_line_params.search_paths <- cmd_line_params.search_paths @ [p]

let append_input_file f = 
  cmd_line_params.input_files <- cmd_line_params.input_files @ [f]
  
let set_node_address ipr_str =
  let addr, role_opt = parse_ip_role ipr_str in
  cmd_line_params.node_address <- addr;
  cmd_line_params.role <- role_opt

let set_role role_id = 
  cmd_line_params.role <- (if role_id = "" then None else Some role_id)

let append_peers ipr_str_list =
  let ip_roles = Str.split (Str.regexp (Str.quote ",")) ipr_str_list in
  cmd_line_params.peers <- cmd_line_params.peers @ (List.map parse_ip_role ip_roles)

let load_partition_map file = 
  let str = read_file file in
  let str_full = "declare pmap : [(string, [(int, int)])] = "^str in
  let prog = parse_program_from_string K3Parser.program K3Lexer.tokenize str_full 
  in cmd_line_params.partition_map <- K3Route.list_of_k3_partition_map prog
  
let set_run_length len =
  cmd_line_params.run_length <- Int64.of_string len


(* Argument descriptions *)
let param_specs = Arg.align

  (* Actions *) 
  (action_specs cmd_line_params.action@
   test_specs cmd_line_params.test_mode@[
  
  (* Compilation parameters *)
  "-i", Arg.String set_input_language, 
      "lang     Set the compiler's input language";
  "-l", Arg.String set_output_language, 
      "lang     Set the compiler's output language";
  "-I", Arg.String append_search_path, 
      "dir      Include a directory in the module search path";
  
  (* Interpreter and evaluation parameters *)
  "-h", Arg.String set_node_address, 
      "addr     Set the current node address for evaluation";
  "-r", Arg.String set_role, 
      "role     Set this node's role during evaluation";
  "-n", Arg.String append_peers, 
      "[addr]   Append addresses to the peer list";
  "-steps", Arg.String set_run_length, 
      "int64    Set program run length in # of messages";
  "-m", Arg.String load_partition_map,
      "file     Load a partition map from a file";
  
  (* Debugging parameters *)
  "-t", Arg.Unit set_print_types,
      "         Print types as part of output";
  "-d", Arg.Unit set_debug_info,
      "         Print debug info (context specific)";
  "-v", Arg.Unit set_verbose,
      "         Print verbose output (context specific)";
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

(* --- Start --- *)
let main () =
  parse_cmd_line();
  if (List.length cmd_line_params.input_files) < 1 then
    (Arg.usage param_specs usage_msg;
     error "\nNo input files specified");
  
  process_parameters cmd_line_params

let _ = if not !Sys.interactive then Printexc.print main ()
