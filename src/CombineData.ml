(* Utility to combine logs together into one unit *)

open Util

Random.self_init ()

type params = {
  mutable data_files : string list;
  mutable order_file : string option;
}

let params = {
  data_files = [];
  order_file = None;
}

let param_specs = Arg.align
  ["--order", Arg.String (fun s -> params.order_file <- Some s),
    "Order file to determine the read order of the logs"
  ]

let usage_msg = ""

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
let combine_data triggers data_files =
  let t_d = list_zip triggers data_files in
  let rec loop remain rem_num acc =
    if rem_num = 0 then acc else
    (* choose a random list to draw from *)
    let i = Random.int rem_num in
    let _, line, remain, rem_num, trig_id =
      List.fold_right (fun (t,l) (idx, out, remain, rem_num, trig_id) -> 
        let do_just x  = (Printf.sprintf "Just(%s)" @: rec_of_tup x)::out in
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
    let line = K3NewPrint.add_record_ids_str ~prefix:"d" line in
    let line = Printf.sprintf "{%s}" @: String.concat "," line in
    loop remain rem_num (line::acc)
  in
  let remain_init = List.fold_left (fun acc x -> match x with (_,[]) -> acc | _ -> acc+1) 0 t_d in
  List.rev @: loop t_d remain_init []

let trig_from_file (file:string) : string = Filename.chop_extension @: Filename.basename file

let read_files files = List.map read_file_lines files

let _ = 
  parse_cmd_line();
  let files = read_files params.data_files in
  let trigs = List.map trig_from_file params.data_files in
  let data = combine_data trigs files in
  List.iter print_endline data


