(* Driver for the K3 programming language. *)

open Arg
open Util
open K3
open K3Values
open K3Util
open K3Typechecker
open K3Interpreter
open Testing
open ReifiedK3

(* Helpers *)
let error s = prerr_endline s; exit 1

(* Compilation languages *)
type language_t = K3 | ReifiedK3 | Imperative

let language_descriptions = [
    K3,         "k3",  "K3";
    ReifiedK3,  "rk3", "Reified K3";
    Imperative, "imp", "Imperative"
  ]

let format_language_description (lang_t, short_desc, long_desc) =
  let pad_or_truncate_str s len =
    let s_len = String.length s in
    if s_len > len then String.sub s 0 len
    else s^(String.make (len - s_len) ' ')
  in
  "  "^(pad_or_truncate_str short_desc 10)^long_desc

let parse_language s = match String.lowercase s with
  | "k3" -> K3
  | "rk3" -> ReifiedK3
  | "imp" -> Imperative
  | _ -> error ("Invalid output language: "^s)

(* Actions *)
type action_t = REPL | Compile | Interpret | Print | ExpressionTest

let action_descriptions = [
    REPL,             "-i",    "Interactive toplevel";
    Compile,          "-c",    "Compile to specified language";
    Interpret,        "-r",    "Interpret with specified language";
    Print,            "-p",    "Print program as specified language";
    ExpressionTest,   "-t",    "Unit test for expressions"
  ]

let action_specs action_param = List.map (fun (act, flag, desc) -> 
  let fn () = action_param := act
  in (flag, Arg.Unit fn, "       "^desc)) action_descriptions

(* Driver parameters *)
type parameters = {
    action : action_t ref;
    mutable language : language_t;
    mutable search_paths : string list;
    mutable input_files : string list;
    mutable node_address : address;
    mutable peers : address list;
  }

let cmd_line_params : parameters = {
    action = ref Print;
    language = K3;
    search_paths = ["."];
    input_files = [];
    node_address = ("127.0.0.1", 10000);
    peers = [];
  }

(* General parameter setters *)
let set_output_language l =
  cmd_line_params.language <- parse_language l

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

let param_specs = Arg.align ((action_specs cmd_line_params.action)@[
  ("-l", (Arg.String set_output_language), 
      "lang   Set the compiler's output language");
  ("-I", (Arg.String append_search_path), 
      "dir    Include a directory in the module search path");
  ("-h", (Arg.String set_node_address), 
      "addr   Set the current node address for evaluation");
  ("-n", (Arg.String append_peers), 
      "[addr] Append addresses to the peer list");
  ])

let usage_msg =
  ("k3 [opts] sourcefile1 [sourcefile2 [...]]"^
     "\n---- Output languages ----\n"^
     (String.concat "\n"
       (List.map format_language_description language_descriptions))^
     "\n---- Options ----")

let parse_cmd_line () =
  Arg.parse param_specs append_input_file usage_msg

(* Driver execution *)

(* Program constructors *)
let parse_program f =
  let in_chan = try open_in f
                with Sys_error _ -> error ("failed to open file: "^f) in
  let lexbuf = Lexing.from_channel in_chan in
  let prog = K3Parser.program K3Lexer.tokenize lexbuf in
    close_in in_chan;
    prog

let handle_type_error p (uuid,error) =
  print_endline "----Type error----";
  print_endline ("Error("^(string_of_int uuid)^"): "^error);
  print_endline (string_of_program ~print_id:true p);
  exit 1

let typed_program f =
  let p = parse_program f in
  try deduce_program_type p
  with TypeError (uuid, error) -> handle_type_error p (uuid, error)

let imperative_program f =
  RK3ToImperative.imperative_of_program (fun () -> -1) (typed_program f)

(* TODO *) 
let repl params = ()

(* TODO *)
let compile params = ()

(* Interpret actions *)
let interpret params =
  let eval_fn = match params.language with
    | K3 -> (fun f -> 
      let typed_program = deduce_program_type (parse_program f)
      in eval_program params.node_address typed_program)
    | _ -> error "Output language not yet implemented"
  in
  List.iter eval_fn params.input_files

(* Print actions *)
let print_k3_program f = print_endline (string_of_program (parse_program f))

let print_reified_k3_program f =
  let print_expr_fn ?(print_id=false) e = lazy (print_reified_expr (reify_expr [] e)) in
  let tp = typed_program f
  in print_endline (string_of_program ~print_expr_fn:print_expr_fn tp)

let print_imperative_program f =
  print_endline (ImperativeUtil.string_of_program (imperative_program f))

let print params =
  let print_fn = match params.language with
    | K3 -> print_k3_program
    | ReifiedK3 -> print_reified_k3_program
    | Imperative -> print_imperative_program
  in
  List.iter print_fn params.input_files

(* Test actions *)
let test params =
  let test_fn = match params.language with
    | K3 -> (fun f -> 
      let test_triples = parse_expression_test (read_file f) in
      let test_cases = snd (List.fold_left (fun (i, test_acc) (decls, e, x) ->
          let name = f^" "^(string_of_int i) in
          let test_case = case name @: eval_test_expr (decls, e) @=? eval_test_expr (decls, x) 
          in i+1, test_acc@[test_case]
        ) (0, []) test_triples)
      in List.iter run_tests test_cases)
    | _ -> error "Testing language not yet implemented"
  in List.iter test_fn params.input_files


(* Top level *)
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
