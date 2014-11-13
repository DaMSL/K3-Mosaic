(* Utility to combine data together into one file for the new k3 *)
open Util

let _ = Random.self_init ()

type params = {
  mutable data_files : string list;
  mutable map_file : string option;
}

let params = {
  data_files = [];
  map_file = None;
}

let param_specs = Arg.align
  [ "--map", Arg.String (fun s -> params.map_file <- Some s),
    "Map file to map file names to trigger names";
  ]

let usage_msg = "Enter data files"

type file_type = String | Int | Float | Date | Bool

let default_val = function
  | String -> ""
  | Int    -> "0"
  | Float  -> "0.0"
  | Bool   -> "false"
  | Date   -> "1900-01-01"

type sep_t = Comma | Pipe

type file_data = {
  name : string;
  trig_name : string;
  mutable data : string list;
  types : file_type list;
  sep : sep_t;
}

let default_vals file = List.map default_val file.types
let default_sep_vals file = String.concat "|" @@ default_vals file

let parse_cmd_line () =
  Arg.parse param_specs
    (fun f -> params.data_files <- params.data_files @ [f])
    usage_msg

let r_pipe    = Str.regexp "|"
let r_comma   = Str.regexp ","
let d         = "[0-9]"
let r_date    = Str.regexp (Printf.sprintf "%s%s%s%s-%s%s-%s%s" d d d d d d d d)
let r_float   = Str.regexp (Printf.sprintf "%s*\\.%s*" d d)
let r_int     = Str.regexp @@ "[0-9]*"
let r_bool    = Str.regexp @@ "true|false"

let to_string i line files : string =
  let file = files.(i) in
  (* normalize to using pipes *)
  let line =
    if file.sep = Comma then Str.global_replace r_comma "|" line else line in
  String.concat "|" @@
    file.trig_name :: (Array.to_list @@
    Array.mapi (fun i' f ->
      if i = i' then line
      else default_sep_vals f)
    files)

let all_defaults files = String.concat "|" @@ Array.to_list @@ Array.map default_sep_vals files

(* returns a list of strings, each one being a line *)
let combine_data files =
  let rem_cnt = ref (List.length files) in
  let files   = Array.of_list files in

  let rec loop () =
    if !rem_cnt <= 0 then () else
    (* choose a random list to draw from *)
    let i = Random.int !rem_cnt in
    let (_, j, _) = Array.fold_left (fun ((i', j, found) as acc) f ->
      if found then acc else
      if f.data <> [] then
        if i' >= i then i',     j,     true
        else            i' + 1, j + 1, found
      else              i',     j + 1, found)
      (0, 0, false) files
    in
    let line = match files.(j).data with
      | [l]   -> files.(j).data <- [];
                 rem_cnt := pred !rem_cnt;
                 l
      | l::ls -> files.(j).data <- ls; l
      | []    -> failwith @@ Printf.sprintf "unexpected: i[%d], j[%d], rem[%d]" i j !rem_cnt
    in
    print_endline @@ to_string j line files;
    loop ()
  in loop ();
  print_string @@ "end" ^"|"^(all_defaults files)

let get_sep ss =
  let s = hd ss in
  if String.contains s '|' then Pipe else Comma

let regex_of_sep = function
  | Comma -> r_comma
  | Pipe  -> r_pipe

let get_types sep data =
  let s = hd data in
  let ts = Str.split (regex_of_sep sep) s in
  List.map (fun s ->
    if r_match r_date s then Date
    else if r_match r_float s then Float
    else if r_match r_int s then Int
    else if r_match r_bool s then Bool
    else String) ts

let _ =
  parse_cmd_line();
  if params.data_files = [] then print_endline usage_msg else
  let files = List.map (fun name ->
    let data = read_file_lines name in
    let name = Filename.basename name in
    let trig_name = "insert_"^String.uppercase @@ Filename.chop_extension name in
    let sep  = get_sep data in
    let types = get_types sep data
    in {data; name; trig_name; sep; types})
    params.data_files in
  let files = List.sort (fun f f' -> String.compare f.name f'.name) files in
  combine_data files


