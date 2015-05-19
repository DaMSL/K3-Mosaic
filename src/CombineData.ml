(* Utility to combine data together into one file for the new k3 *)
open Util
open K3.AST

module KH = K3Helpers
module KU = K3Util
module DH = DriverHelpers

let _ = Random.self_init ()

type params = {
  mutable data_files : string list;
}

let params = {
  data_files = [];
}

let param_specs = Arg.align [
  ]

let usage_msg = "Enter data files"

type file_data = {
  name : string;
  handle : in_channel;
  r_sep : Str.regexp;
  sep : char;
  defs : string list;
}

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
  let files = List.sort String.compare params.data_files in
  let data_l = List.map (fun path ->
      let handle = open_in path in
      let s = String.uppercase @@ Filename.chop_extension @@ Filename.basename path in
      (* we don't allow anything under underscore *)
      let name =
        if String.contains s '_' then String.sub s 0 (String.index s '_')
        else s in
      let sample = input_line handle in
      let sep, r_sep    = if String.contains sample '|' then '|', r_pipe else ',', r_comma in
      let defs   = get_defaults sep r_sep sample in
      seek_in handle 0; (* rewind handle *)
      {name; handle; sep; r_sep; defs}) files in
  let data_arr = Array.of_list data_l in
  (* array representing remaining data *)
  let remain = Array.of_list @@ create_range 0 @@ List.length data_l in
  let rec loop remain =
    if Array.length remain = 0 then () else
      (* choose at random from the remain array *)
      let choice = remain.(Random.int @@ Array.length remain) in
      (* access in data array *)
      let datum  = data_arr.(choice) in
      try
        let in_line = input_line datum.handle in
        let out_l =
          datum.name :: "1" ::
          (List.flatten @@ List.mapi (fun i x ->
            if i = choice then split_line x.sep x.r_sep in_line
            else x.defs) data_l)
        in
        print_endline @@ String.concat "|" out_l;
        loop remain
      with End_of_file ->
        (* remove element of array *)
        let remain' = Array.of_list @@ List.filter ((<>) choice) @@ Array.to_list remain in
        loop remain'
  in loop remain;
  (* sentry line *)
  print_string @@ String.concat "|" @@
    "" :: "1" :: (List.flatten @@ List.map (fun x -> x.defs) data_l)
