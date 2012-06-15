(* Driver for the K3 programming language. *)

open Arg
open K3
open K3Util

(* Helpers *)
let error s = prerr_endline s; exit 1

(* Compilation languages *)
type language_t = K3 | Imperative

let language_descriptions = [
    K3,         "k3",  "K3";
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
  | "imp" -> Imperative
  | _ -> error ("Invalid output language: "^s)

(* Driver parameters *)
type parameters = {
    mutable language : language_t;
    mutable search_paths : string list;
    mutable input_files : string list;
  }

let cmd_line_params : parameters = {
    language = K3;
    search_paths = ["."];
    input_files = [];
  }  

let set_output_language l =
  cmd_line_params.language <- parse_language l

let append_search_path p = 
  cmd_line_params.search_paths <- cmd_line_params.search_paths @ [p]

let append_input_file f = 
  cmd_line_params.input_files <- cmd_line_params.input_files @ [f]

let param_specs = Arg.align [
  ("-l", (Arg.String set_output_language), 
      "lang   Set the compiler's output language");
  ("-I", (Arg.String append_search_path), 
      "dir    Include a directory in the module search path")
  ]

let usage_msg =
  ("k3 [opts] sourcefile1 [sourcefile2 [...]]"^
     "\n---- Output languages ----\n"^
     (String.concat "\n"
       (List.map format_language_description language_descriptions))^
     "\n---- Options ----")

let parse_cmd_line () =
  Arg.parse param_specs append_input_file usage_msg

(* Driver execution *)
let print_k3_program f =
  let in_chan =
    try open_in f
    with Sys_error _ -> error ("failed to open file: "^f)
  in
  let lexbuf = Lexing.from_channel in_chan in
  let p = K3Parser.program K3Lexer.tokenize lexbuf
  in print_endline (string_of_program p);
     close_in in_chan

let main () =
    parse_cmd_line();
    if (List.length cmd_line_params.input_files) < 1 then
      (Arg.usage param_specs usage_msg;
       error "\nNo input files specified");
    
    let print_fn = match cmd_line_params.language with
      | K3 -> print_k3_program
      | _ -> error "Output language not yet implemented"
    in
    List.iter print_fn cmd_line_params.input_files

let _ = Printexc.print main ()
