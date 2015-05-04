(* Utility to combine data together into one file for the new k3 *)
open Util
open K3.AST

module KH = K3Helpers
module KU = K3Util
module DH = DriverHelpers

let _ = Random.self_init ()

type params = {
  mutable data_files : string list;
  mutable k3dist_file : string;
}

let params = {
  data_files = [];
  k3dist_file = "";
}

let param_specs = Arg.align [
    "--k3", Arg.String (fun s -> params.k3dist_file <- s), "K3 file to receive data";
  ]

let usage_msg = "Enter data files"

let default_val = function
  | TString -> " "
  | TInt    -> "0"
  | TFloat  -> "0.0"
  | TBool   -> "false"
  | TDate   -> "1900-01-01"
  | _       -> failwith "unhandled type"

type sep_t = Comma | Pipe

type file_data = {
  name : string;
  trig_name : string;
  trig_id : int;
  mutable data : string list;
  types : base_type_t list;
  sep : sep_t;
  shuffle_map : int list; (* how to shuffle in final type *)
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
let r_small   = Str.regexp {|\(.+\)_small\(\..+\)|}

let to_string i line files combined_t : string=
  let file = files.(i) in
  (* parse the elements of the line *)
  let elems =
    if file.sep = Comma then Str.split r_comma line
    else Str.split r_pipe line in
  (* create default values *)
  let full_line = Array.of_list @@ List.map default_val combined_t in
  (* replace via shuffling *)
  List.iter2 (fun e place -> full_line.(place) <- e) elems file.shuffle_map;
  full_line.(0) <- soi file.trig_id;
  String.concat "|" @@ Array.to_list full_line

(* returns a list of strings, each one being a line *)
let combine_data files combined_t =
  let rem_file_cnt = ref (List.length files) in
  let files   = Array.of_list files in

  let rec loop () =
    if !rem_file_cnt <= 0 then () else
    (* choose a random list to draw from *)
    let i = Random.int !rem_file_cnt in
    (* we have to iterate over all files, even though we only care about the
     * remaining ones *)
    let (_, j, _) = Array.fold_left (fun ((i', j, found) as acc) file ->
      if found then acc else
      if file.data <> [] then
        if i' >= i then i',     j,     true
        else            i' + 1, j + 1, found
      else              i',     j + 1, found)
      (0, 0, false) files
    in
    let line = match files.(j).data with
      (* last element of a file *)
      | [l]   -> files.(j).data <- [];
                 decr rem_file_cnt; l
      | l::ls -> files.(j).data <- ls; l
      | []    -> failwith @@ Printf.sprintf "unexpected: i[%d], j[%d], rem[%d]" i j !rem_file_cnt
    in
    print_endline @@ to_string j line files combined_t;
    loop ()
  in loop ();
  print_string @@ String.concat "|" @@ "-1" :: (tl @@ List.map default_val combined_t)

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
    if r_match r_date s then TDate
    else if r_match r_float s then TFloat
    else if r_match r_int s then TInt
    else if r_match r_bool s then TBool
    else TString) ts

let _ =
  parse_cmd_line();
  if params.data_files = [] then print_endline usage_msg else
  (* try to parse the k3 file *)
  let k3_prog = DH.parse_k3_file params.k3dist_file in
  (* get the combined type of the demux trigger *)
  let demux_ts = KH.unwrap_ttuple @@ hd @@ snd_many @@ KU.typed_vars_of_arg @@
    KU.args_of_code @@ KU.trigger_of_program GenDist.sw_demux_nm k3_prog in
  (* get the map of trigger id to arg indices *)
  let _, _, trig_map =
    KU.decompose_global_fn @@ KU.global_of_program K3Dist.trig_ids_id k3_prog in
  let trig_map = List.map (fun e ->
      match KU.decompose_tuple e with
      | [x; y; l] ->
        begin match KU.tag_of_expr x, KU.tag_of_expr y with
        | Const(CInt i), Const(CString s) ->
            s, (i, List.map (fun e ->
                    match KU.tag_of_expr e with
                    | Const(CInt i) -> i
                    | _ -> failwith "not int") @@
                  KH.list_of_k3_container l)
        | _ -> failwith "wrong format for trig_ids"
        end
      | _ -> failwith "wrong format for trig_ids") @@
    KH.list_of_k3_container trig_map
  in
  let files = List.map (fun name ->
    let data = read_file_lines name in
    let name = Filename.basename name in
    (* check if we have _small in the name *)
    let name = match r_groups ~r:r_small ~n:2 name with
      | [Some x; Some y] -> x^y
      | _                -> name in
    let trig_name = "sw_insert_"^String.uppercase @@ Filename.chop_extension name in
    (* lookup in trig_map *)
    let trig_id, shuffle_map =
      try List.assoc trig_name trig_map
      with Not_found -> failwith @@ Printf.sprintf "Couldn't find %s in trig map" trig_name
    in
    let sep  = get_sep data in
    let types = get_types sep data
    in {data; name; trig_name; sep; types; trig_id; shuffle_map})
    params.data_files in
  let files = List.sort (fun f f' -> f.trig_id - f'.trig_id) files in
  let combined_t = List.map (fun x -> x.typ) demux_ts in
  combine_data files combined_t

