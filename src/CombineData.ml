(* Utility to combine data together into one file for the new k3 *)

open Util

Random.self_init ()

type params = {
  mutable data_files : string list;
  mutable order_file : string option;
  mutable map_file : string option;
}

let params = {
  data_files = [];
  order_file = None;
  map_file = None
}

let param_specs = Arg.align
  ["--order", Arg.String (fun s -> params.order_file <- Some s),
    "Order file to determine the read order of the logs";
   "--map", Arg.String (fun s -> params.map_file <- Some s),
    "Map file to map file names to trigger names"
  ]

let usage_msg = "Enter data files"

let parse_cmd_line () =
  Arg.parse param_specs
    (fun f -> params.data_files <- params.data_files @ [f])
    usage_msg

let r_pipe = Str.regexp "|"
let r_comma = Str.regexp ","

(* convert a tuple to a record *)
let rec_of_tup s =
  let to_rec r =
    let str =
      String.concat "," @:
        K3NewPrint.add_record_ids_str ~sep:"=" @: Str.split r s in
    Printf.sprintf "{%s}" str
  in
  if String.contains s '|' then to_rec r_pipe
  else if String.contains s ',' then to_rec r_comma
  else to_rec r_comma (* single value *)

(* takes a list of trigger names and a list of lists of data lines *)
(* returns a list of strings, each one being a line *)
let combine_data t_d =
  let rec loop remain rem_num acc =
    if rem_num = 0 then acc else
    (* choose a random list to draw from *)
    let i = Random.int rem_num in
    let _, line, remain, rem_num, trig_id =
      List.fold_right (fun (t,l) (idx, out, remain, rem_num, trig_id) -> 
        let do_just x  = (Printf.sprintf "Just %s" @: rec_of_tup x)::out in
        let do_none () = "Nothing"::out in
        match l with
        | []           -> idx,   do_none (), (t,[])::remain, rem_num,   trig_id
        | [x]   when i=idx -> 
                          idx-1, do_just x,  (t,[])::remain, rem_num-1, Some t
        | [x]          -> idx-1, do_none (), (t,[])::remain, rem_num-1, trig_id
        | x::xs when i=idx -> 
                          idx-1, do_just x,  (t,xs)::remain, rem_num,   Some t
        | xs           -> idx-1, do_none (), (t,xs)::remain, rem_num,   trig_id
          (* don't count empty lists for the randomization *)
      )
      remain
      (rem_num-1, [], [], rem_num, None)
    in
    let line = match trig_id with
      | None   -> failwith "trigger not found" 
      | Some t -> (Printf.sprintf "\"%s\"" t)::line
    in
    let line = K3NewPrint.add_record_ids_str ~prefix:"r" ~sep:"=" line in
    let line = Printf.sprintf "{%s}" @: String.concat "," line in
    loop remain rem_num (line::acc)
  in
  let remain_init = List.fold_left (fun acc -> function (_,[]) -> acc | _ -> acc+1) 0 t_d in
  List.rev @: loop t_d remain_init []

let read_files files = List.map read_file_lines files

let parse_map_file ss = 
  let r_comma = Str.regexp "," in
  let pair = function [x;y] -> (x,y) | _ -> failwith "not a pair" in
  List.map (pair |- Str.split r_comma) ss

let _ = 
  parse_cmd_line();
  if params.data_files = [] then print_endline usage_msg else
  let files = read_files params.data_files in
  let file_names = List.map Filename.basename params.data_files in
  let files_names = list_zip file_names files in
  let files_names = match params.map_file with
    | Some fname -> (* we have a map file so use it to map *)
      let ss = read_file_lines fname in
      let fmap = parse_map_file ss in
      List.map (fun (fname,d) -> List.assoc fname fmap, d) files_names
    | None -> (* just use filenames as trigger names *)
      let mod_fname n = 
        let n = Filename.chop_extension n in
        Printf.sprintf "%s%c%s" "insert_" (Char.uppercase n.[0]) (str_drop 1 n)
      in
      List.map (fun (fname, d) -> mod_fname fname, d) files_names
  in
  let data = combine_data files_names in
  List.iter print_endline data


