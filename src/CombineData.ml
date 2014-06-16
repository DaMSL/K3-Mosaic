(* Utility to combine data together into one file for the new k3 *)

open Util

Random.self_init ()

type params = {
  mutable data_files : string list;
  mutable order_file : string option;
  mutable map_file : string option;
  mutable mut : bool; (* mutability qualifiers *)
}

let params = {
  data_files = [];
  order_file = None;
  map_file = None;
  mut = false;
}

let param_specs = Arg.align
  ["--order", Arg.String (fun s -> params.order_file <- Some s),
    "Order file to determine the read order of the logs";
   "--map", Arg.String (fun s -> params.map_file <- Some s),
    "Map file to map file names to trigger names";
   "--mut", Arg.Unit (fun _ -> params.mut <- true),
    "Use mutability qualifiers"
  ]

let usage_msg = "Enter data files"

let parse_cmd_line () =
  Arg.parse param_specs
    (fun f -> params.data_files <- params.data_files @ [f])
    usage_msg

let r_pipe = Str.regexp "|"
let r_comma = Str.regexp ","
let d = "[0-9]"
let r_date = Str.regexp (Printf.sprintf "%s%s%s%s-%s%s-%s%s" d d d d d d d d)
let r_quoted1 = Str.regexp "^\".*\"$"
let r_quoted2 = Str.regexp "^'.*'$"

let wrap_mut_rec s = if params.mut then Printf.sprintf "(%s,MemImmut)" s else s

(* check if we have a non-string. If it's a string, quote it *)
let quote_string s =
  let test f =
    try ignore @: f s; true
    with Invalid_argument _ | Failure _ -> false in
  if test int_of_string ||
     test float_of_string ||
     (*r_match r_date s ||*) (* for now, dates are just strings in the input *)
     test bool_of_string ||
     r_match r_quoted1 s || r_match r_quoted2 s
  then s
  else Printf.sprintf "\"%s\"" (String.escaped s)

(* convert a tuple string to a record string *)
let rec_of_tup s =
  let to_rec r =
    let l = K3NewPrint.add_record_ids @: Str.split r s in
    let str = String.concat "," @:
      List.map (fun (s, s') -> Printf.sprintf "%s=%s" s (wrap_mut_rec @: quote_string s')) l in
    Printf.sprintf "{%s}" str
  in
  if String.contains s '|' then to_rec r_pipe
  else if String.contains s ',' then to_rec r_comma
  else to_rec r_comma (* single value *)


(* takes a list of trigger names and a list of lists of data lines *)
(* returns a list of strings, each one being a line *)
let combine_data t_d : string list=
  let mut_s = if params.mut then " MemImmut" else "" in
  let rec loop remain rem_num acc =
    if rem_num = 0 then acc else
    (* choose a random list to draw from *)
    let i = Random.int rem_num in
    (* Note that we can't remove any list, because empty lists generate a 'Nothing' *)
    let _, line, remain, rem_num, trig_id =
      List.fold_right (fun ((t,l) as vals) (idx, out, remain, rem_num, trig_id) -> 
        let do_just x  = (Printf.sprintf "Just%s %s" mut_s @: rec_of_tup x)::out in
        let do_none () = ("Nothing"^mut_s)::out in
        match l with
        | []           -> idx,   do_none (), vals::remain,   rem_num,   trig_id
                          (* idx only keeps track of live lists *)
        | [x]   when i=idx -> 
                          idx-1, do_just x,  (t,[])::remain, rem_num-1, Some t
        | [x]          -> idx-1, do_none (), vals::remain,   rem_num,   trig_id
        | x::xs when i=idx -> 
                          idx-1, do_just x,  (t,xs)::remain, rem_num,   Some t
        | xs           -> idx-1, do_none (), vals::remain,   rem_num,   trig_id
          (* don't count empty lists for the randomization *)
      )
      remain
      (rem_num-1, [], [], rem_num, None)
    in
    let line = match trig_id with
      | None   -> failwith "trigger not found" 
      | Some t -> (Printf.sprintf "\"%s\"" t)::line
    in
    loop remain rem_num (line::acc)
  in
  let to_string line =
    (* convert every line to a record *)
    let line = K3NewPrint.add_record_ids ~prefix:"r" line in
    let line = List.map (fun (s,s') -> Printf.sprintf "%s=%s" s (wrap_mut_rec s')) line in
    Printf.sprintf "{%s}" @: String.concat "," line
  in
  (* add an 'end' to the (reversed) list of lines *)
  let add_end lines = match lines with
    | (s::ss)::ls -> ("\"end\""::(List.map (fun _ -> "Nothing") ss))::ls
    | _           -> failwith "bad input in adding end line"
  in 
  (* initialize the counts for each source *)
  let remain_init = List.fold_left (fun acc -> function (_,[]) -> acc | _ -> acc+1) 0 t_d in
  let lines = add_end @: loop t_d remain_init [] in
  List.rev_map to_string lines

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
      let fns =
        List.map (fun (fname,d) -> List.assoc fname fmap, d) files_names in
      (* Lexicographical ordering for consistency *)
      List.sort (fun (n,_) (n',_) -> String.compare n n') fns
    | None -> (* just use filenames as trigger names *)
      let mod_fname n = 
        let n = Filename.chop_extension n in
        "insert_"^(String.uppercase n)
      in
      let fns = List.map (fun (fname, d) -> mod_fname fname, d) files_names in
      (* Lexicographical ordering for consistency *)
      List.sort (fun (n,_) (n',_) -> String.compare n n') fns
  in
  let data = combine_data files_names in
  List.iter print_endline data


