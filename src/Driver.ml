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
open K3Testing
open ReifiedK3
open DriverHelpers

(* Note these override module names *)
module Imperative = Imperative.AST(CPP.CPPTarget)
module ImperativeUtil = ImperativeUtil.Util(CPP.CPPTarget)
module RK3ToImperative = RK3ToImperative.Make(CPP.CPPTarget)

module CPPExt = CPP.CPPTarget
module CPT = CPP.CPPTarget.ASTImport
module CPPGen = CPP.CPPGenerator
module P = Printf
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

type out_lang_t =
  | K3 | AstK3
  | K3Dist
  | K3Test | K3DistTest (* output k3 with some test code *)
  | AstK3Dist
  | ReifiedK3
  | Imperative | CPPInternal | CPP
  | K3New (* new k3 syntax *)

let out_lang_descs = [
    K3,          "k3",        "K3";
    K3Test,      "k3test",    "K3 Test";
    AstK3,       "k3ast",     "K3 AST";
    K3Dist,      "k3dist",    "Distributed K3";
    K3DistTest,  "k3disttest","Distributed K3 with test code";
    AstK3Dist,   "k3distast", "Distributed K3 AST";
    ReifiedK3,   "rk3",       "Reified K3";
    Imperative,  "imp",       "Imperative";
    CPPInternal, "cppi",      "C++-internal";
    CPP,         "cpp",       "C++";
    K3New,       "k3new",     "New K3";
  ]

(* types of data carried around  by the driver *)
type data_t =
  | K3Data of program_t
  (* for distributed programs, we also need the metadata of the maps, and we include
   * the non-distributed program *)
  | K3DistData of program_t * ProgInfo.prog_data_t * program_t
  | K3TestData of program_test_t

let string_of_data = function
  | K3Data _ -> "k3data"
  | K3DistData _ -> "k3distdata"
  | K3TestData _ -> "k3testdata"

(* Evaluation option setters *)
let parse_port p =
  let error () = invalid_arg ("invalid port: "^p) in
  try let r = int_of_string p in if r > 65535 then error() else r
  with Failure _ -> error()

(* ip-role format is 'alias=ip:port/role' *)
let parse_ip_role ipr_str =
  let ident = "[a-zA-Z_][a-zA-Z0-9_]*" in (* legal identifier *)
  let num = "[0-9]+" in
  let ip = num^"\\."^num^"\\."^num^"\\."^num in
  let r = Str.regexp @:
    "\\("^ident^"=\\)?\\("^ip^"\\|localhost\\)\\(:"^num^"\\)?\\(/"^ident^"\\)?"
  in
  let error () = invalid_arg "invalid ip string format" in
  if Str.string_match r ipr_str 0 then
    let ms = List.map (fun i ->
        try some @: Str.matched_group i ipr_str
        with Not_found -> None
      ) [1;2;3;4]
    in
    let alias = match at ms 0 with
    | None   -> None
    | Some x -> some @: str_drop_end 1 x (* get rid of extra char *)
    in
    let role = match at ms 3 with
    | None   -> None
    | Some x -> some @: str_drop 1 x (* get rid of extra char *)
    in
    match at ms 1, at ms 2 with (* ip, port *)
    | None, None         -> error ()
    | Some ip, None      -> (ip, default_port), role, alias
    | None, Some port    -> (default_ip, parse_port @: str_drop 1 port), role, alias
    | Some ip, Some port -> (ip, parse_port @: str_drop 1 port), role, alias
  else error ()

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
    ExpressionTest,   "--expr",  "Use expression test input";
    ProgramTest,      "--prog",  "Use program test input";
  ]

let test_specs test_mode_param = setter_specs test_mode_param test_descriptions

let string_of_test_mode = function ExpressionTest -> "Expression" | ProgramTest -> "Program"

(* Actions *)
type action_t = REPL | Compile | Interpret | Print | Test

let action_descriptions = [
    Print,            "-p",    "Print program as specified language";
    Compile,          "-c",    "Compile to specified language";
    Interpret,        "--eval", "Interpret with specified language";
    Test,             "--test", "K3 testing";
    REPL,             "--top",  "Interactive toplevel";
  ]

let action_specs action_param = setter_specs action_param action_descriptions

(* Driver parameters *)
type parameters = {
    action                : action_t ref;
    test_mode             : test_mode_t ref;
    mutable in_lang       : in_lang_t;
    mutable out_lang      : out_lang_t;
    mutable search_paths  : string list;
    mutable input_files   : string list;
    mutable peers         : K3Global.peer_t list;
    mutable default_peer  : bool; (* whether we're using the default peer *)
    mutable partition_map : K3Route.part_map_t;
    mutable run_length    : int64;
    mutable print_types   : bool; (* TODO                                    : change to a debug flag *)
    mutable debug_info    : bool;
    mutable verbose       : bool;
    mutable trace_files   : string list;
    mutable order_files   : string list;

    mutable queue_type   : K3Runtime.queue_type; (* type of queue for interpreter *)
    mutable shuffle_tasks : bool; (* Shuffle tasks from different node
                                    to simulate network delay *)
    mutable force_correctives : bool; (* Force correctives being generated *)
    mutable lambda_ret : bool;    (* print syntax with * when return isn't a tuple *)
    mutable k3new_data_file : string;
  }

let default_cmd_line_params () = {
    action            = ref Print;
    test_mode         = ref ProgramTest;
    in_lang           = K3in;
    out_lang          = K3;
    search_paths      = default_search_paths;
    input_files       = [];
    peers             = default_peers;
    default_peer      = true;
    partition_map     = [];
    run_length        = default_run_length;
    print_types       = default_print_types;
    debug_info        = default_debug_info;
    verbose           = default_verbose;
    trace_files       = [];
    order_files       = [];

    queue_type        = K3Runtime.GlobalQ;
    shuffle_tasks     = default_shuffle_tasks;
    force_correctives = false;
    lambda_ret        = false;
    k3new_data_file   = "default.k3";
  }

let cmd_line_params = default_cmd_line_params ()


let handle_type_error p (uuid, name, msg) =
  let s = K3TypeError.string_of_error msg in
  print_endline "----Type error----";
  print_endline @: "Error("^(string_of_int uuid)^"): "^name^": "^s;
  (match p with
  | K3Data p | K3DistData(p,_,_)->
    print_endline @: PS.string_of_program ~uuid_highlight:uuid p
  | K3TestData p_test -> print_endline @:
    PS.string_of_program_test ~uuid_highlight:uuid p_test);
  exit 1

let handle_interpret_error p (uuid,error) =
  print_endline "----Interpreter error----";
  print_endline @: "Error("^(string_of_int uuid)^"): "^error;
  (match p with
  | K3Data p | K3DistData(p,_,_)->
      print_endline @: PS.string_of_program ~uuid_highlight:uuid p
  | K3TestData p_test -> print_endline @:
    PS.string_of_program_test ~uuid_highlight:uuid p_test);
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

let parse_test_file params = match !(params.test_mode) with
  | ExpressionTest -> parse_program K3Parser.expression_test K3Lexer.tokenize
  | ProgramTest    -> parse_program K3Parser.program_test K3Lexer.tokenize

(* Program transformers *)
let typed_program_with_globals p =
  let p' =
    K3Global.add_globals cmd_line_params.peers p in
  try 
    let p, env, trig_env, _ = type_bindings_of_program p' in
    p, (env, trig_env)
  with TypeError (a,b,c) -> handle_type_error (K3Data p') (a,b,c)

let typed_program_test_with_globals prog_test =
  let add_g p =
    K3Global.add_globals cmd_line_params.peers p
  in
  match prog_test with
  | ProgTest(p, testl)    ->
      let p' = add_g p in
      let p_t = ProgTest(p', testl) in
      begin
        try deduce_program_test_type p_t
        with TypeError (a,b,c) -> handle_type_error (K3TestData p_t) (a,b,c)
      end
  | NetworkTest(p, testl) ->
      let p' = add_g p in
      let p_t = NetworkTest(p', testl) in
      begin
        try deduce_program_test_type p_t
        with TypeError (a,b,c) -> handle_type_error (K3TestData p_t) (a,b,c)
      end
  | ExprTest(p_ts) -> failwith "expr_test unhandled"


(* for most functions, we don't need the globals included *)
let typed_program p =
  let p, envs = typed_program_with_globals p in
  K3Global.remove_globals cmd_line_params.peers p, envs
      

(* don't include globals *)
let typed_program_test prog_test =
  let prog_test' = typed_program_test_with_globals prog_test in
  let remove_g p =
    K3Global.remove_globals cmd_line_params.peers p
  in
  match prog_test' with
  | ProgTest(p, tl)    -> ProgTest(remove_g p, tl)
  | NetworkTest(p, tl) -> NetworkTest(remove_g p, tl)
  | ExprTest(p_ts)     -> failwith "expr_test unhandled"

let imperative_program p =
  RK3ToImperative.imperative_of_program (fun () -> []) (fst @: typed_program p)

let cpp_program p =
  let mk_meta() = [] in
  CPPTyping.deduce_program_type @:
    cpp_of_imperative mk_meta @:
      RK3ToImperative.imperative_of_program mk_meta @: fst @: typed_program p

(* Action handlers *)
(* TODO *)
let repl params inputs = ()

(* TODO *)
let compile params inputs = ()

(* Interpret actions *)
let interpret_k3 params prog = let p = params in
  (* this not only adds type info, it adds the globals which are crucial
    * for interpretation *)
  let tp, envs = typed_program_with_globals prog in
  let open K3Interpreter in
  try
    let interp = init_k3_interpreter tp ~run_length:p.run_length
                                        ~peers:p.peers
                                        ~shuffle_tasks:p.shuffle_tasks
                                        ~queue_type:p.queue_type in
      snd @: interpret_k3_program interp
  with RuntimeError (uuid,str) -> handle_interpret_error (K3Data tp) (uuid,str)

let interpret params inputs =
  let f = function
    | K3Data p
    | K3DistData(p,_,_)
    | K3TestData(ProgTest(p, _))
    | K3TestData(NetworkTest(p, _)) -> ignore @: interpret_k3 params p
    | _ -> failwith "data type not supported for interpretation"
  in
  List.iter f inputs

(* Print actions *)
let print_event_loop (id, (res_env, ds_env, instrs)) =
    print_endline ("----Role "^id^" Flow Program----");
    print_string (string_of_resource_env res_env);
    print_string (string_of_dispatcher_env ds_env)

let string_of_typed_meta (t,a) = string_of_annotation a

let print_k3_program ?(no_roles=false) f = function
  | K3Data p | K3DistData (p,_,_) ->
    let tp, envs = typed_program p in
    let event_loops, default = roles_of_program tp in
      print_endline @: f (tp, envs);
      if not no_roles then (
        List.iter print_event_loop event_loops;
        (match default with None -> ()
          | Some (_,x) -> print_event_loop ("DEFAULT", x))
      )
  | _ -> error "Cannot print this type of data"

(* create and print a k3 program with an expected section *)
let print_k3_test_program ~lambda_ret = function
  | idx, K3DistData (p, meta, orig_p) ->
      (* get the folded expressions comparing values for latest vid.
       * These go in the expected statements at the end *)
      let tests_by_map = GenTest.expected_code_all_maps meta in
      let drop_roles =
        List.filter (fun d -> not (is_role d || is_def_role d)) in
      let p', test_vals =
        (* get the test values from the dbtoaster trace if available *)
        if not @: null cmd_line_params.trace_files then
          (* we only care about the code part *)
          let tests_by_map = List.map (fun (nm, (code, empty)) ->
            nm, code) tests_by_map 
          in
          let role_s, maplist =
            let trace_file = at cmd_line_params.trace_files idx in
            FromTrace.string_of_file trace_file ~is_dist:true
          in
           (* debug *)
           (* List.iter (fun (nm, code) -> print_endline code) maplist; *)
          let map_final_l =
            list_map (fun (nm, code) -> nm, parse_k3_expr code) maplist in
          (* join according to map name *)
          let map_tests_join = assoc_join map_final_l tests_by_map in
          let tests_vals =
            list_map (fun (_, (final, e)) -> e, InlineExpr final)
              map_tests_join
          in
          (* add the produced test roles and trigger *)
          (* debug *)
          (*print_endline @: code_s;*)
          (* filter out all role stuff in the original generated ast *)
          let new_p = drop_roles p @ parse_k3_prog role_s in
          new_p, tests_vals

        (* use the order files to simulate the system *)
        else if not @: null cmd_line_params.order_files then
          let order_file = at cmd_line_params.order_files idx in
          let events = FromTrace.events_of_order_file order_file in
          let role_s = FromTrace.string_of_test_role ~is_dist:false events in
          (* Note that we take the single-site version here *)
          (* debug *)
          print_endline @: role_s;
          let p' = drop_roles orig_p @ parse_k3_prog role_s in
          (* run the interpreter on our code *)
          let params = default_cmd_line_params () in
          let _, (_, (env, _)) = 
            hd @: interpret_k3 params p' in (* assume one node *)
          (* match the maps with their values *)
          let tests_vals = 
            List.map (fun (name, (exp_code, empty)) ->
              let v = 
                try !(IdMap.find name env)
                with Not_found -> failwith "Missing map in environment"
              in
              (* deal with empty sets. In these cases, we don't have the types *)
              let res = match v with 
              | VSet [] -> empty
              | _       -> expr_of_value 0 v
              in 
              exp_code, InlineExpr res
            ) tests_by_map
          in
          (* now create a distributed version *)
          let role_dist_s =
            FromTrace.string_of_test_role ~is_dist:true events in
          let p' = drop_roles p @ parse_k3_prog role_dist_s in
          p', tests_vals

        else
          (* we don't have a trace file or order file for final value tests *)
          p, list_map (fun (_, (e,_)) -> e, FileExpr "dummy") tests_by_map
      in
      let prog_test = NetworkTest(p', test_vals) in
      let _, prog_test = renumber_test_program_ids prog_test in
      let prog_test = typed_program_test prog_test in
      print_endline @: PS.string_of_program_test ~lambda_ret prog_test

  | idx, K3Data p ->
      (* get the test values from the dbtoaster trace *)
      let p', test_vals =
        if cmd_line_params.trace_files <> [] then
          let code_s, maplist =
            let trace_file = at cmd_line_params.trace_files idx in
            FromTrace.string_of_file trace_file ~is_dist:false
          in
           (* debug *)
            (*List.iter (fun (nm, code) -> print_endline code) maplist; *)
          let map_final_l = list_map (fun (nm, code) ->
            K3Helpers.mk_var nm, parse_k3_expr code
          ) maplist in
          let tests_vals = list_map (fun (nm, e) ->
            nm, InlineExpr e) map_final_l in
          (* filter our all role stuff in the original generated ast *)
          let filter_p = List.filter
            (fun d -> not (is_role d || is_def_role d)) p in
          (* add the produced test roles and trigger *)
          (* debug *)
          (*print_endline code_s;*)
          let s = parse_k3_prog code_s in
          let new_p = filter_p @ s in
          new_p, tests_vals
        else error "Test printout requires trace file"
      in
      let prog_test = NetworkTest(p', test_vals) in
      let _, prog_test = renumber_test_program_ids prog_test in
      let prog_test = typed_program_test prog_test in
      print_endline @: PS.string_of_program_test ~lambda_ret prog_test

  | _ -> error "Cannot print this type of data"

let print_reified_k3_program = function
  | K3Data p | K3DistData(p,_,_) ->
    let print_expr_fn c e =
        lazy (print_reified_expr @: reify_expr [] e) in
    let tp, _ = typed_program p in
    print_endline @: string_of_program ~print_expr_fn:print_expr_fn tp
  | _ -> error "Cannot print this type of data"

let print_imp func print_types = function
  | K3Data p | K3DistData(p, _, _) ->
    let string_of_meta m =
      (if print_types then (ImperativeUtil.string_of_type @: fst m)^";"
       else "")^
      (string_of_annotation @: snd m)
    in print_endline @:
      ImperativeUtil.string_of_program string_of_meta @: func p
  | _ -> error "Cannot print this type of data"

let print_cppi_program = print_imp cpp_program
let print_imperative_program = print_imp imperative_program

let print_cpp_program = function
  | K3Data p | K3DistData(p, _, _) ->
    let files_and_content = CPPGen.generate_program @: cpp_program p in
    let mk_filename id = (String.make 40 '=')^" "^id in
    let print_content (f,c) =
      print_endline @: mk_filename f; print_endline c; print_endline "\n\n"
    in
    List.iter print_content files_and_content
  | _ -> error "Cannot print this type of data"

(* Top-level print handler *)
let print params inputs =
  let lambda_ret = params.lambda_ret in
  let idx_inputs = insert_index_fst 0 inputs in
  let sofp = string_of_program ~verbose:cmd_line_params.verbose ~print_id:true in
  let print_fn = match params.out_lang with
    | AstK3 | AstK3Dist   -> print_k3_program (sofp |- fst) |- snd
    | K3 | K3Dist         -> print_k3_program (PS.string_of_program ~lambda_ret |- fst) |- snd
    | K3New               -> print_k3_program ~no_roles:true (K3NewPrint.string_of_dist_program
                               ~file:params.k3new_data_file) |- snd
    | K3Test | K3DistTest -> print_k3_test_program ~lambda_ret
    | ReifiedK3           -> print_reified_k3_program |- snd
    | Imperative          -> print_imperative_program params.print_types |- snd
    | CPPInternal         -> print_cppi_program params.print_types |- snd
    | CPP                 -> print_cpp_program |- snd
  in List.iter print_fn idx_inputs

(* Test actions *)
let test params inputs =
  let test_fn fname input =
    match input with
    | K3TestData(ExprTest _ as x) -> test_expressions fname x
    | K3TestData((ProgTest _ | NetworkTest _) as x) ->
        let globals_k3 = K3Global.globals params.peers in
        test_program globals_k3 (interpret_k3 params) fname x
    | x -> error @: "testing not yet implemented for "^string_of_data x
  in List.iter2 test_fn params.input_files inputs

let transform_to_k3_dist force_correctives partmap p proginfo =
  (* do not use. Simply for type checking original program *)
  (*tp = typed_program p in *)
  try
    GenDist.gen_dist ~force_correctives proginfo partmap p
  with Invalid_argument(msg) ->
    print_endline ("ERROR: " ^msg);
    print_endline (ProgInfo.string_of_prog_data proginfo);
    exit (-1)

let process_inputs params =
  let proc_fn f = match params.in_lang, !(params.action) with
    | K3in, Test -> K3TestData(parse_test_file params f)
    | K3in, _    -> K3Data(parse_k3_file f)
    | M3in, _    ->
        let m3prog = parse_m3_file f in
        let proginfo = M3ProgInfo.prog_data_of_m3 m3prog in
        if params.debug_info then
          print_endline (ProgInfo.string_of_prog_data proginfo);
        let prog = M3ToK3.m3_to_k3 m3prog in
        if params.out_lang = K3Test then K3Data(prog)
        else K3DistData(prog, proginfo, prog)
  in List.map proc_fn params.input_files

(* this function can only transform to another k3 format *)
let transform params ds =
  let proc_fn input = match params.out_lang, input with
   | (AstK3Dist | K3Dist | K3DistTest), K3DistData(p, proginfo, _) ->
       let p' = transform_to_k3_dist 
                  params.force_correctives params.partition_map p proginfo in
       K3DistData(p', proginfo, p)
   | (AstK3Dist | K3Dist | K3DistTest), _ ->
       failwith "Missing metadata for distributed version"
   | _, data -> data
  in List.map proc_fn ds

(* Driver execution *)
let process_parameters params =
  (* preprocess params *)

  (* distributed programs must have M3 as their input language *)
  (match params.out_lang with
    | K3Dist | AstK3Dist | K3DistTest -> params.in_lang <- M3in
    | _ -> ());

  let inputs = process_inputs params in
  let inputs = transform params inputs in
  match !(params.action) with
  | Compile   -> compile params inputs
  | Interpret -> interpret params inputs
  | Print     -> print params inputs
  | REPL      -> repl params inputs
  | Test      -> test params inputs

(* General parameter setters *)
let append_peers ipr_str_list =
  let ip_roles = Str.split (Str.regexp @: Str.quote ",") ipr_str_list in
  let new_peers = List.map parse_ip_role ip_roles in
  if cmd_line_params.default_peer then
    cmd_line_params.peers <- new_peers
  else
    cmd_line_params.peers <- cmd_line_params.peers @ new_peers

(* Load peer address from file
 * NOTE does not contain localhost if peers are specified*)
let load_peer file =
  let line_lst = read_file_lines file in
  if (List.length line_lst) = 0 then
    error "Empty node file"
  else
    cmd_line_params.peers
      <- (List.map parse_ip_role line_lst)

let load_partition_map file =
  let str = read_file file in
  let str_full = "declare pmap : [(string, [(int, int)])] = "^str in
  let prog = parse_program_from_string K3Parser.program K3Lexer.tokenize str_full
  in cmd_line_params.partition_map <- K3Route.list_of_k3_partition_map prog

(* Argument descriptions *)
let param_specs = Arg.align

  (* Actions *)
  (action_specs cmd_line_params.action@
   test_specs cmd_line_params.test_mode@[

  (* Compilation parameters *)
  "-i", Arg.String (fun l -> cmd_line_params.in_lang <- parse_in_lang l),
      "lang     Set the compiler's input language";
  "-l", Arg.String (fun l -> cmd_line_params.out_lang <- parse_out_lang l),
      "lang     Set the compiler's output language";
  "-I", Arg.String (fun p ->
      cmd_line_params.search_paths <- cmd_line_params.search_paths @ [p]),
      "dir      Include a directory in the module search path";

  (* Interpreter and evaluation parameters *)
  "-n", Arg.String append_peers,
      "[addr]   Append addresses to the peer list";
  "--steps", Arg.String (fun len ->
      cmd_line_params.run_length <- Int64.of_string len),
      "int64    Set program run length in # of messages";
  "-m", Arg.String load_partition_map,
      "file     Load a partition map from a file";
  "--peers", Arg.String load_peer,
      "file     Load peer address from a file";
  "--trace", Arg.String (fun file ->
    cmd_line_params.trace_files <- cmd_line_params.trace_files @ [file]),
      "file     Load a DBToaster trace file";
  "--order", Arg.String (fun file ->
    cmd_line_params.order_files <- cmd_line_params.order_files @ [file]),
      "file     Load a map order file";
  "--datafile", Arg.String (fun file ->
    cmd_line_params.k3new_data_file <- file),
      "file     Specify a k3new data file";

  (* Debugging parameters *)

  "-t", Arg.Unit (fun () -> cmd_line_params.print_types <- true),
      "         Print types as part of output";
  "-d", Arg.Unit (fun () -> cmd_line_params.debug_info  <- true),
      "         Print debug info (context specific)";
  "-v", Arg.Unit (fun () -> cmd_line_params.verbose <- true),
  "             Print verbose output (context specific)";
  (* Interpreter related *)
  "-q", Arg.String (fun q -> cmd_line_params.queue_type <- match q with
    | "global"  -> K3Runtime.GlobalQ
    | "node"    -> K3Runtime.PerNodeQ
    | "trigger" -> K3Runtime.PerTriggerQ
    | x         -> error @: "Unknown parameter "^x),
      "         Queue type: global/node/trigger";
  "--shuffle", Arg.Unit (fun () -> cmd_line_params.shuffle_tasks <- true),
      "         Shuffle tasks to simulate network delays";
  "--force", Arg.Unit (fun () -> cmd_line_params.force_correctives <- true),
      "         Force distributed compilation to produce more correctives";
  (* Print related *)
  "--lambda", Arg.Unit (fun () -> cmd_line_params.lambda_ret  <- true),
      "         Print syntax with lambda return hint (*)";
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
  Arg.parse param_specs
    (fun f -> cmd_line_params.input_files <- cmd_line_params.input_files @ [f])
    usage_msg

(* --- Start --- *)
let main () =
  parse_cmd_line();
  if (List.length cmd_line_params.input_files) < 1 then
    (Arg.usage param_specs usage_msg;
     error "\nNo input files specified");

  process_parameters cmd_line_params

let _ = if not !Sys.interactive then Printexc.print main ()
