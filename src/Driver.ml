(* Driver for the K3 programming language. *)

open Arg
open Str
open Constants
open Util
open K3.AST
open K3Printing
open K3.Annotation
open K3Values
open K3Values.Value
open K3Util
open K3Typechecker
open K3Streams
open K3Consumption
open K3Testing
open DriverHelpers

(* Note these override module names *)
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
  | K3New (* new k3 syntax *)

let out_lang_descs = [
    K3,          "k3",        "K3";
    K3Test,      "k3test",    "K3 Test";
    AstK3,       "k3ast",     "K3 AST";
    K3Dist,      "k3dist",    "Distributed K3";
    K3DistTest,  "k3disttest","Distributed K3 with test code";
    AstK3Dist,   "k3distast", "Distributed K3 AST";
    K3New,       "k3new",     "New K3";
  ]

(* types of data carried around  by the driver *)
type data_t =
  | K3Data of program_t
  (* for distributed programs, we also need the metadata of the maps, the
   * non-distributed program, and the map warmup program *)
  | K3DistData of program_t * ProgInfo.prog_data_t * program_t * program_t * string list
  | K3TestData of program_test_t

let string_of_data = function
  | K3Data _ -> "k3data"
  | K3DistData _ -> "k3distdata"
  | K3TestData _ -> "k3testdata"

(* Evaluation option setters *)
let parse_port p =
  let error () = invalid_arg ("invalid port: "^p) in
  try let r = int_of_string p in if r > 65535 then error () else r
  with Failure _ -> error()

let ident = "[a-zA-Z_][a-zA-Z0-9_]*" (* legal identifier *)
let num = "[0-9]+"
let ip = sp "%s.%s.%s.%s" num num num num
let r = Str.regexp @@
  sp "\\(%s\\|localhost\\):\\(%s\\)/\\(%s\\)" ip num ident

(* ip-role format is 'ip:port/role' *)
let parse_ip_role ipr_str =
  let error () = invalid_arg "invalid ip string format" in
  if Str.string_match r ipr_str 0 then
    match r_groups ipr_str ~r ~n:3 with
    | [Some ip; Some port; Some role] -> (ip, parse_port port), role
    | _                               -> error ()
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
    action: action_t ref;
    test_mode: test_mode_t ref;
    mutable in_lang: in_lang_t;
    mutable out_lang: out_lang_t;
    mutable input_files: string list;
    mutable peers: K3Global.peer_t list;
    mutable default_peer: bool; (* whether we're using the default peer *)
    mutable debug_info: bool;
    mutable verbose: bool;

    (* k3 generation options *)
    mutable partition_map: K3Route.part_map_t;
    mutable trace_files: string list;

    (* k3dist generation options *)
    mutable map_type: K3Dist.map_type; (* UNUSED whether to generate code that uses vmaps (no other option now)*)
    mutable gen_deletes: bool;
    mutable gen_correctives: bool;
    mutable gen_single_vid: bool;
    mutable agenda_map: K3Dist.mapping_t; (* mapping from agenda to stream sources *)
    mutable stream_file: string;
      (* file to stream from (into switches). UNUSED -- we splice a varialbe (stream_path) instead *)

    (* interpreter options *)
    mutable queue_type: K3Runtime.queue_type; (* type of queue for interpreter *)
    mutable load_path: string;  (* path for the interpreter to load csv files from *)
    mutable src_interval: int;     (* interval (ms) between the interpreter feeding sources *)
    mutable interp_arg_file: string;  (* args to override k3 globals in interpreter (json) *)
    mutable logging: bool;    (* do we log to a file *)

    (* k3new options *)
    mutable k3new_data_file: string;
    mutable k3new_folds: bool;   (* output fold instead of map/ext *)
    mutable use_filemux: bool;
    mutable use_intmap: bool;
    mutable safe_writes: bool;   (* output safe writes *)
    mutable use_opt_route: bool; (* use optimized 1:1 routing *)

    mutable dump_info: bool;  (* dump proginfo data *)
    mutable print_warmup: bool;  (* print the warmup program *)
  }

let default_cmd_line_params () = {
    action            = ref Print;
    test_mode         = ref ProgramTest;
    in_lang           = K3in;
    out_lang          = K3;
    input_files       = [];
    peers             = default_peers;
    default_peer      = true;
    partition_map     = [];
    debug_info        = default_debug_info;
    verbose           = default_verbose;
    trace_files       = [];

    queue_type        = K3Runtime.PerNodeQ;
    k3new_data_file   = "default.k3";
    k3new_folds       = false;
    load_path         = "";
    map_type          = K3Dist.MapMultiVMap;
    gen_deletes       = true;
    gen_correctives   = true;
    gen_single_vid    = true;

    src_interval      = 2;
    interp_arg_file   = "";
    logging           = true;

    stream_file       = "input.csv";
    agenda_map        = K3Dist.default_mapping;
    use_filemux       = false;
    use_intmap        = false;

    safe_writes       = false;
    dump_info         = false;  (* dump info about prog_info and quit *)
    print_warmup      = false;
    use_opt_route     = true;
  }

let cmd_line_params = default_cmd_line_params ()

let handle_error heading p uuid ?name msg =
  let n = match name with Some nm -> nm^": " | _ -> "" in
  prerr_endline heading;
  prerr_endline @@ "Error("^(string_of_int uuid)^"): "^n^msg;
  (match p with
  | K3Data p | K3DistData(p,_,_,_,_)->
      if cmd_line_params.debug_info then
        prerr_endline @@ string_of_program ~print_id:true p
      else
        prerr_endline @@ PS.string_of_program ~uuid_highlight:uuid p
  | K3TestData p_test -> prerr_endline @@
    PS.string_of_program_test ~uuid_highlight:uuid p_test);
  exit 1

let handle_type_error p uuid nm msg    = handle_error "----Type Error----" p uuid ~name:nm @@
                                         K3TypeError.string_of_error msg
let handle_interpret_error p uuid msg  = handle_error "----Interpreter Error----" p uuid msg
let handle_distribute_error p uuid msg = handle_error "----Distribute Error----" p uuid msg

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
    K3StdLib.add_globals p in
  try
    let p, env, tenv, trig_env, _ = type_bindings_of_program p' in
    p, (env, tenv, trig_env)
  with TypeError (a,b,c) -> handle_type_error (K3Data p') a b c

let typed_program_test_with_globals prog_test =
  let add_g p =
    K3StdLib.add_globals p
  in
  match prog_test with
  | ProgTest(p, testl)    ->
      let p' = add_g p in
      let p_t = ProgTest(p', testl) in
      begin
        try deduce_program_test_type p_t
        with TypeError (a,b,c) -> handle_type_error (K3TestData p_t) a b c
      end
  | NetworkTest(p, testl) ->
      let p' = add_g p in
      let p_t = NetworkTest(p', testl) in
      begin
        try deduce_program_test_type p_t
        with TypeError (a,b,c) -> handle_type_error (K3TestData p_t)  a b c
      end
  | ExprTest(p_ts) -> failwith "expr_test unhandled"


(* for most functions, we don't need the globals included *)
let typed_program p =
  let p, envs = typed_program_with_globals p in
  K3StdLib.remove_globals p, envs


(* don't include globals *)
let typed_program_test prog_test =
  let prog_test' = typed_program_test_with_globals prog_test in
  let remove_g p =
    K3StdLib.remove_globals p
  in
  match prog_test' with
  | ProgTest(p, tl)    -> ProgTest(remove_g p, tl)
  | NetworkTest(p, tl) -> NetworkTest(remove_g p, tl)
  | ExprTest(p_ts)     -> failwith "expr_test unhandled"

(* Action handlers *)
(* TODO *)
let repl params inputs = ()

(* TODO *)
let compile params inputs = ()

(* Interpret actions *)
let interpret_k3 params prog =
  let p = params in
  if not params.logging then Log.set false else Log.set true;

  (* this not only adds type info, it adds the globals which are crucial
    * for interpretation *)
  let tp, (t_env, type_aliases, _) = typed_program_with_globals prog in
  let open K3Interpreter in
  try
    let interp = init_k3_interpreter tp ~peers:p.peers
                                        ~queue_type:p.queue_type
                                        ~load_path:p.load_path
                                        ~src_interval:(foi p.src_interval /. 1000.)
                                        ~interp_file:p.interp_arg_file
                                        ~type_aliases
    in
    interpret_k3_program interp
  with RuntimeError (uuid,s1, str) -> handle_interpret_error (K3Data tp) uuid (s1^":"^str)

let interpret params inputs =
  let f = function
    | K3Data p
    | K3DistData(p,_,_,_,_)
    | K3TestData(ProgTest(p, _))
    | K3TestData(NetworkTest(p, _)) -> ignore @@ interpret_k3 params p
    | _ -> failwith "data type not supported for interpretation"
  in
  List.iter f inputs

(* Print actions *)
let print_event_loop (id, (res_env, ds_env, instrs)) =
    print_endline ("----Role "^id^" Flow Program----");
    print_string (string_of_resource_env res_env);
    print_string (string_of_dispatcher_env ds_env)

let string_of_typed_meta (t,a) = string_of_annotation a

let print_program_and_event_loop ?(no_roles=false) f p =
  let tp, envs = typed_program p in
  let event_loops, default = roles_of_program tp in
    print_endline @@ f (tp, envs);
    if not no_roles then (
      List.iter print_event_loop event_loops;
      (match default with None -> ()
        | Some (_,x) -> print_event_loop ("DEFAULT", x))
    )

let print_k3_program ?(print_warmup=false) ?(no_roles=false) f = function
  | K3Data p -> print_program_and_event_loop ~no_roles (f None []) p
  | K3DistData (p,_,_,warmup,wrelnames) ->
    let wmap_idt = List.fold_left (fun acc (d,a) ->
      match d with
       | Global(nm,t,_) when str_prefix "bs_" nm -> acc@[nm,t]
       | _ -> acc) [] warmup
    in
    if print_warmup then
      print_program_and_event_loop ~no_roles (f (Some wmap_idt) wrelnames) warmup
    else
      print_program_and_event_loop ~no_roles (f (Some wmap_idt) wrelnames) p
  | _ -> error "Cannot print this type of data"

(* create and print a k3 program with an expected section *)
let print_k3_test_program params = function
  | idx, K3DistData (p, meta, orig_p, _, _) ->
      (* get the folded expressions comparing values for latest vid.
       * These go in the expected statements at the end *)
      let tests_by_map = GenTest.expected_code_all_maps params.map_type meta in
      let test_vals =
        (* get the test values from the dbtoaster trace if available *)
        if not @@ null cmd_line_params.trace_files then
          (* we only care about the code part *)
          let tests_by_map = List.map (fun (nm, (code, empty)) ->
            nm, code) tests_by_map
          in
          let _, maplist =
            let trace_file = at cmd_line_params.trace_files idx in
            FromTrace.string_of_file trace_file ~is_dist:true
          in
           (* debug *)
           (* List.iter (fun (nm, code) -> print_endline code) maplist; *)
          let map_final_l =
            List.map (fun (nm, code) -> nm, parse_k3_expr code) maplist in
          (* join according to map name *)
          let map_tests_join = assoc_join map_final_l tests_by_map in
          let tests_vals =
            List.map (fun (n, (final, e)) -> e, InlineExpr (n, final))
              map_tests_join
          in
          (* add the produced test roles and trigger *)
          (* debug *)
          (* filter out all role stuff in the original generated ast *)
          tests_vals
      else failwith "no trace file"

      in
      let prog_test = NetworkTest(p, test_vals) in
      let _, prog_test = renumber_test_program_ids prog_test in
      let prog_test = typed_program_test prog_test in
      print_endline @@ PS.string_of_program_test prog_test

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
            K3Helpers.mk_var nm, nm, parse_k3_expr code
          ) maplist in
          let tests_vals = list_map (fun (nmv, nm, e) ->
            nmv, InlineExpr(nm, e)) map_final_l in
          (* filter our all role stuff in the original generated ast *)
          let filter_p = List.filter
            (fun d -> not (is_role d || is_def_role d)) p in
          (* add the produced test roles and trigger *)
          (* debug *)
          print_endline code_s;
          let s = parse_k3_prog code_s in
          let new_p = filter_p @ s in
          new_p, tests_vals
        else error "Test printout requires trace file"
      in
      let prog_test = NetworkTest(p', test_vals) in
      let _, prog_test = renumber_test_program_ids prog_test in
      let prog_test = typed_program_test prog_test in
      print_endline @@ PS.string_of_program_test prog_test

  | _ -> error "Cannot print this type of data"

(* Top-level print handler *)
let print params inputs =
  let idx_inputs = insert_index_fst inputs in
  let sofp = string_of_program ~verbose:cmd_line_params.verbose ~print_id:true in
  let print_k3_program = print_k3_program ~print_warmup:params.print_warmup in
  let print_fn = match params.out_lang with
    | AstK3 | AstK3Dist   -> print_k3_program (fun _ _ -> (sofp |- fst)) |- snd
    | K3 | K3Dist         -> print_k3_program (fun _ _ -> (PS.string_of_program |- fst)) |- snd
    | K3New               -> print_k3_program ~no_roles:true
       (if params.print_warmup
          then K3NewPrint.string_of_dist_warmup_program
                 ~map_to_fold:params.k3new_folds
                 ~file:params.k3new_data_file
                 ~use_filemux:params.use_filemux
                 ~use_intmap:params.use_intmap
                 ~safe_writes:params.safe_writes
          else K3NewPrint.string_of_dist_program
                 ~map_to_fold:params.k3new_folds
                 ~file:params.k3new_data_file
                 ~use_filemux:params.use_filemux
                 ~use_intmap:params.use_intmap
                 ~safe_writes:params.safe_writes) |- snd
    | K3Test | K3DistTest -> print_k3_test_program params
  in List.iter print_fn idx_inputs

(* Test actions *)
let test params inputs =
  let test_fn fname input =
    match input with
    | K3TestData(ExprTest _ as x) -> test_expressions params.peers fname x
    | K3TestData((ProgTest _ | NetworkTest _) as x) ->
        let globals_k3 = K3Global.globals in
        test_program params.peers globals_k3 (interpret_k3 params) fname x
    | x -> error @@ "testing not yet implemented for "^string_of_data x
  in List.iter2 test_fn params.input_files inputs

let transform_to_k3_dist params proginfo warmup_p ast =
  let {map_type; stream_file;
       gen_deletes; gen_correctives; gen_single_vid;
       agenda_map; use_opt_route} = params in
  try
    GenDist.gen_dist ~map_type ~stream_file ~gen_deletes ~gen_correctives ~gen_single_vid
                     ~agenda_map ~use_opt_route proginfo warmup_p ast

  with DistributeError(uuid, s) -> handle_distribute_error (K3Data ast) uuid s

let process_inputs params =
  let proc_fn f = match params.in_lang, !(params.action) with
    | K3in, Test -> K3TestData(parse_test_file params f)
    | K3in, _    -> K3Data(parse_k3_file f)
    | M3in, _    ->
        let m3prog = parse_m3_file f in
        let proginfo = M3ProgInfo.prog_data_of_m3 m3prog in
        if params.dump_info then begin print_endline @@ ProgInfo.dump_info proginfo; exit(0) end;
        if params.debug_info then
          print_endline (ProgInfo.string_of_prog_data proginfo);
        let (prog, (warmup_prog, warmup_relnames)) = M3ToK3.m3_to_k3 m3prog in
        if params.out_lang = K3Test then K3Data(prog)
        else K3DistData(prog, proginfo, prog, warmup_prog, warmup_relnames)
  in List.map proc_fn params.input_files

(* this function can only transform to another k3 format *)
let transform params ds =
  let proc_fn input = match params.out_lang, input with
   | (AstK3Dist | K3Dist | K3DistTest | K3New), K3DistData(p, proginfo, _, warmup_prog, wrelnames) ->
       let warmup', p' = transform_to_k3_dist params proginfo warmup_prog p in
       K3DistData(p', proginfo, p, warmup', wrelnames)
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
  let ip_roles = Str.split (Str.regexp @@ Str.quote ",") ipr_str_list in
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
    cmd_line_params.peers <- List.map parse_ip_role line_lst

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

  (* Interpreter and evaluation parameters *)
  "-n", Arg.String append_peers,
      "[addr]   Append addresses to the peer list";
  "-m", Arg.String load_partition_map,
      "file     Load a partition map from a file";
  "--peers", Arg.String load_peer,
      "file     Load peer address from a file";
  "--trace", Arg.String (fun file ->
    cmd_line_params.trace_files <- cmd_line_params.trace_files @ [file]),
      "file     Load a DBToaster trace file";
  "--datafile", Arg.String (fun file ->
    cmd_line_params.k3new_data_file <- file),
      "file     Specify a k3new data file";
  "--k3new-folds", Arg.Unit (fun () -> cmd_line_params.k3new_folds <- true),
      "         For k3new: output folds instead of ext and map";
  "--map-vmap", Arg.Unit (fun () -> cmd_line_params.map_type <- K3Dist.MapVMap),
      "         Use vmaps";
  "--map-multi", Arg.Unit (fun () -> cmd_line_params.map_type <- K3Dist.MapMultiVMap),
      "         Use multi-index vmaps";
  "--no-deletes", Arg.Unit (fun () -> cmd_line_params.gen_deletes <- false),
      "         Generate deletes";
  "--no-correctives", Arg.Unit (fun () -> cmd_line_params.gen_correctives <- false),
      "         Generate correctives";
  "--no-single-vid", Arg.Unit (fun () -> cmd_line_params.gen_single_vid <- false),
      "         Generate non-isobatch code";
  "--interp-args", Arg.String (fun s -> cmd_line_params.interp_arg_file <- s),
      "         Load json arg file for interpretation";
  "--safe-writes", Arg.Unit (fun () -> cmd_line_params.safe_writes <- true),
      "         Generate safe writes to arrays";

  (* Debugging parameters *)

  "-d", Arg.Unit (fun () -> cmd_line_params.debug_info  <- true),
      "         Print debug info (context specific)";
  "-v", Arg.Unit (fun () -> cmd_line_params.verbose <- true),
  "             Print verbose output (context specific)";
  "--dump-info", Arg.Unit (fun () -> cmd_line_params.dump_info <- true),
  "             Dump info about prog_info";
  "--print-warmup", Arg.Unit (fun () -> cmd_line_params.print_warmup <- true),
  "             Print warmup code";

  (* Interpreter related *)
  "-q", Arg.String (fun q -> cmd_line_params.queue_type <- match q with
    | "global" -> K3Runtime.GlobalQ
    | "node"   -> K3Runtime.PerNodeQ
    | x        -> error @@ "Unknown parameter "^x),
      "         Queue type: global/node/trigger";
  "--load_path", Arg.String (fun s -> cmd_line_params.load_path <- s),
      "         Path for interpreter to load files from";
  "--msg_interval", Arg.Int (fun x -> cmd_line_params.src_interval <- x),
      "         Interval between feeding in sources";
  "--sfile", Arg.String (fun s -> cmd_line_params.stream_file <- s),
      "         Path for stream file";
  "--agenda", Arg.String (fun s -> cmd_line_params.agenda_map <- K3Dist.load_mapping_file s),
      "         Agenda to read from";
  "--no-log", Arg.Unit (fun () -> cmd_line_params.logging <- false),
      "         Disable logging";
  "--filemux", Arg.Unit (fun () -> cmd_line_params.use_filemux <- true),
      "         Use filemux mechanism";
  "--intmap", Arg.Unit (fun () -> cmd_line_params.use_intmap <- true),
      "         Use intmap ds";
  "--no-opt-route", Arg.Unit (fun () -> cmd_line_params.use_opt_route <- false),
      "         Don't use optimal routing";
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
