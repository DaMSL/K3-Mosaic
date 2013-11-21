open Util
open Str

let load_log file = read_file_lines file

let pre_sanitize log =
  let _, lines =
    List.fold_left (fun (remove_rest, acc) line ->
        if r_match ".*TRACE -.*consuming" line then remove_rest, acc
        else if r_match ".*TRACE - >>>>.*" line then true, acc
        else if remove_rest then true, acc
        else false, line::acc)
      (false, [])
      log
  in
  List.rev lines

(* group triggers into batches *)
let group_triggers log =
  let r_trig = regexp ".*DEBUG - Trigger.*" in
  let r_send = regexp ".*DEBUG - send.*" in
  let _, trigs =
    List.fold_left (fun (seen_send, acc_groups) line ->
        let add_to_group () = match acc_groups with 
          | []    -> []             (* no group. don't add *)
          | x::xs -> (line::x)::xs
        in
        if line = "" then seen_send, acc_groups
        else if string_match r_trig line 0 && seen_send then
          false, add_to_group ()
        else if string_match r_trig line 0 then    (* start a new group *)
          false, [line]::acc_groups
        else if string_match r_send line 0 && not seen_send then 
          true, [line]::acc_groups
        else seen_send, add_to_group ())
      (false, [])
      log
  in
  List.rev_map (fun trigl -> List.rev trigl) trigs

let unwanted_lines =
  ["----Globals";
   "----Frames"]

let unwanted_lines_r = List.map (fun str -> regexp @: str^".*") unwanted_lines

let unwanted_multilines =
  [
   "__epoch__";
   "__init_vid__";
   "acks =";
   "data_nodes =";
   "init =";
   "gc_vid =";
   "min_max_acked_vid =";
   "node_ring =";
   "peers =";
   "peers_num =";
   "pmap_data =";
   "replicas =";
   "switches =";
   "switches_num =";
   "me =";
   "vid_buf =";
   "vid_buf_2 =";
   "vid_rcv_cnt =";
   "vid_rcv_cnt_2 =";
   "__vid_counter__";
  ]

let unwanted_multilines_r = List.map (fun str -> regexp @: str^".*") unwanted_multilines

(* check if any of the unwanted regexs match our line *)
let check_unwanted unwanted line =
  List.exists (fun regex -> string_match regex line 0) unwanted

let remove_unwanted trig =
  let r_equals = regexp "[^=]*=.*" in
  let r_trig = regexp ".*Trigger.*" in (* remove all before trig *)
  let _, _, lines = 
    List.fold_left (fun (remove, after_trig, acc) line ->
      if string_match r_trig line 0 then remove, true, line::acc
      else if not after_trig then remove, false, acc (* skip anything before trig *)
      else if check_unwanted unwanted_lines_r line then remove, after_trig, acc
      else if check_unwanted unwanted_multilines_r line then true, after_trig, acc
      (* reset the remove flag if we see another = *)
      else if string_match r_equals line 0 then false, after_trig, line::acc
      (* keep removing until we see an =, or till the end of the trigger *)
      else if remove then remove, after_trig, acc
      else remove, after_trig, line::acc)
    (false, false, [])
    trig
  in List.rev lines

(* stop the fold *)
exception Stop of string list

let clean_up_headings trig =
  (* get vid *)
  let vid_r =  regexp ".*__vid_counter__ =.*" in
  let vid_s = List.find (fun s -> string_match vid_r s 0) trig in
  let send_r = regexp ".*DEBUG - send.*" in
  let trig_r = regexp ".*DEBUG - Trigger.*" in
  (* accumulate sends over multiple lines *)
  let sends_s = List.rev @:
    try snd @:
      List.fold_left (fun (in_send, acc_sends) line -> 
        if string_match send_r line 0 then (true, line::acc_sends)
        else if string_match trig_r line 0 then raise @: Stop acc_sends
        else if in_send then match acc_sends with 
          | []    -> true, [line]
          | x::xs -> true, (x^line)::xs
        else failwith "Error in clean_up_headings: no trigger"
      ) (false, []) trig
    with Stop ss -> ss
  in
  let r_hyphen = regexp " - " in
  let sends_s = List.map (fun line ->
      at (split r_hyphen line) 1) 
    sends_s in
  let send_s = match sends_s with [] -> []
      | _ -> ["send_msgs = "^String.concat ";\n" sends_s]
  in
  let vid_s = at (r_split " = " vid_s) 1 in
  let r_amper = regexp "@" in
  List.map (fun line ->
    if string_match trig_r line 0 then
      let trig_s = at (split r_hyphen line) 1 in
      let trig_split = split r_amper trig_s in
      let trig_s, addr_s = hd trig_split, at trig_split 1 in
      String.concat "\n" @: (Format.sprintf "%s, %s %s:" vid_s addr_s trig_s)::send_s
    else line
  ) trig 

let do_per_trigger log = 
  List.map (remove_unwanted |- clean_up_headings) log

let convert_to_db_format log_name trig =
  (* join lines together *)
  let trig_nm = hd trig in
  let r_eq_line = regexp ".+ = .+" in
  (* handle multi-lines *)
  let trig = 
    List.fold_left (fun trig_acc str ->
      (* check for a new line *)
      if string_match r_eq_line str 0 then str::trig_acc
      else match trig_acc with
      | []    -> [str]
      | x::xs -> (x^str)::xs
    ) [] (tl trig)
  in
  (* decompose trigger name *)
  let r_trig = regexp "{\\([0-9]+\\)}, \\(.+\\) Trigger \\(.+\\):" in
  if not @: string_match r_trig trig_nm 0 then failwith "trigger heading mismatch";
  let m = matched_group in
  let vid, addr, trig_nm = m 1 trig_nm, m 2 trig_nm, m 3 trig_nm in
  let r_eq = regexp " = " in
  List.rev_map (fun str ->
    let k_v = split r_eq str in
    let map, value = at k_v 0, at k_v 1 in
    Printf.sprintf "%s/ %s/ %s/ %s/ %s/ %s\n" log_name vid addr trig_nm map value
  ) trig

(* output the log in readable format *)
let string_of_log log =
  let log = List.map unlines log in
  String.concat "\n\n\n" log

type action = Clean | ToDb of string

let input_file = ref ""
let action = ref Clean

let param_specs =
  ["--db", Arg.String (fun log_name -> action := ToDb log_name), "Convert to CSV for a DB"]

let parse_cmd_line () =
  Arg.parse param_specs
    (fun f -> input_file := f)
    "Requires log file to convert"

let main () =
  parse_cmd_line ();
  let log = load_log !input_file in
  let log = pre_sanitize log in
  let log = group_triggers log in
  let log = do_per_trigger log in
  match !action with
  | Clean -> print_endline @: string_of_log log
  | ToDb name ->
      let log = List.map (convert_to_db_format name) log in
      print_endline @:
        String.concat "" @: List.map (fun t -> String.concat "" t) log


let _ = if not !Sys.interactive then Printexc.print main ()


