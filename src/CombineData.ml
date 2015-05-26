(* Utility to combine data together into one file for the new k3 *)
open Util
open K3.AST

module KH = K3Helpers
module KU = K3Util
module DH = DriverHelpers

let _ = Random.self_init ()

type params = {
  mutable data_files : string list;
  mutable agenda_map : K3Dist.mapping_t;
}

let params = {
  data_files = [];
  agenda_map = K3Dist.default_mapping;
}

let param_specs = Arg.align [
  "--agenda",
    Arg.String (fun s -> params.agenda_map <- K3Dist.load_mapping_file s),
    "      Agenda file";
  ]

let usage_msg = "Enter data files and agenda"

type file_data = {
  name : string;
  handle : in_channel;
  r_sep : Str.regexp;
  sep : char;
  defs : string list;
}

let default_val_of_t t = match t.typ with
  | TBool   -> "false"
  | TDate   -> "1900-01-01"
  | TInt
  | TFloat  -> "0"
  | TString -> ""
  | _       -> failwith "unhandled type"

let parse_cmd_line () =
  Arg.parse param_specs
    (fun f -> params.data_files <- params.data_files @ [f])
    usage_msg

let r_pipe    = Str.regexp "|"
let r_comma   = Str.regexp ","
let d         = "[0-9]"
let r_date    = Str.regexp (Printf.sprintf "%s%s%s%s-%s%s-%s%s" d d d d d d d d)
let r_float   = Str.regexp (Printf.sprintf "%s*\\.%s*" d d)
let r_int     = Str.regexp "[0-9]*"
let r_bool    = Str.regexp "true|false"
let r_file    = Str.regexp {|\(.+\)_.*\(\..*\)|}

let split_line sep r_sep line =
  let last  = if line.[String.length line - 1] = sep then [""] else [] in
  let first = if line.[0] = sep then [""] else [] in
  first @ Str.split r_sep line @ last

let get_defaults sep r_sep line =
  let vals = split_line sep r_sep line in
  List.map (fun s ->
    if r_match r_date s then ""
    else if r_match r_float s then "0"
    else if r_match r_int s then "0"
    else if r_match r_bool s then "false"
    else "") vals

let _ =
  parse_cmd_line();
  if params.data_files = [] then print_endline usage_msg else
    if params.agenda_map = K3Dist.default_mapping then print_endline usage_msg else
  let files = List.sort String.compare params.data_files in
  let data_l = List.map (fun path ->
      let handle = open_in path in
      let s = String.uppercase @@ Filename.chop_extension @@ Filename.basename path in
      (* we don't allow anything under underscore *)
      let name =
        if String.contains s '_' then String.sub s 0 (String.index s '_')
        else s in
      (* note: this isn't necessary now, but we'll leave it in anyway *)
      let sample = input_line handle in
      let sep, r_sep    = if String.contains sample '|' then '|', r_pipe else ',', r_comma in
      let defs   = get_defaults sep r_sep sample in
      seek_in handle 0; (* rewind handle *)
      {name; handle; sep; r_sep; defs}) files in
  let data_arr = Array.of_list data_l in
  (* array representing remaining data *)
  let remain = Array.of_list @@ create_range 0 @@ List.length data_l in
  let defaults = List.map default_val_of_t @@ fst params.agenda_map in
  let rec loop remain =
    if Array.length remain = 0 then () else
      (* choose at random from the remain array *)
      let choice = remain.(Random.int @@ Array.length remain) in
      (* access in data array *)
      let datum  = data_arr.(choice) in
      try
        let in_line = split_line datum.sep datum.r_sep @@ input_line datum.handle in
        let out_a = Array.of_list defaults in
        out_a.(0) <- datum.name;
        out_a.(1) <- "1";
        let mapping = StrMap.find datum.name @@ snd params.agenda_map in
        (* map values to their proper places *)
        List.iter2 (fun s i -> out_a.(i) <- s) in_line mapping;
        let out_l = Array.to_list out_a in
        print_endline @@ String.concat "|" out_l;
        loop remain
      with End_of_file ->
        (* remove element of array *)
        let remain' = Array.of_list @@ List.filter ((<>) choice) @@ Array.to_list remain in
        loop remain'
  in loop remain;
  (* sentry line *)
  print_string @@ String.concat "|" defaults
    
