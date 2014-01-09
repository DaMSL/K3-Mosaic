open Util
open Str

let load_log file = read_file_lines file

let r_trace_consuming = Str.regexp ".*TRACE -.*consuming" 
let r_trace_arrows = Str.regexp ".*TRACE - >>>>.*" 

let pre_sanitize log =
  let _, lines =
    List.fold_left (fun (remove_rest, acc) line ->
        if r_match r_trace_consuming line then remove_rest, acc
        else if r_match r_trace_arrows line then true, acc
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

let r_equals = Str.regexp " = "

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
      | _ -> ["send_msgs = "^String.concat "; " sends_s]
  in
  let vid_s = at (Str.split r_equals vid_s) 1 in
  let r_amper = regexp "@" in
  List.rev @: List.fold_left (fun acc line ->
    if string_match trig_r line 0 then
      let trig_s = at (split r_hyphen line) 1 in
      let trig_split = split r_amper trig_s in
      let trig_s, addr_s = hd trig_split, at trig_split 1 in
      let trig_line = [Format.sprintf "%s, %s %s:" vid_s addr_s trig_s] in
      send_s@trig_line@acc
    else line::acc
  ) [] trig 

let do_per_trigger log = 
  List.map (remove_unwanted |- clean_up_headings) log

let convert_to_db_format log_name (idx,trig) =
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
    Printf.sprintf "%s/%d/%s/%s/%s/%s\n" log_name idx addr trig_nm map value
  ) trig

(* output the log in readable format *)
let string_of_log log =
  let log = List.map unlines log in
  String.concat "\n\n\n" log

type action = Clean | ToDb of string | Events of event_action

(* what to do in case of event output *)
and event_action = Plain | Strip | StripSort 

let event_action_of_string = function
  | "plain" -> Plain
  | "strip" -> Strip
  | "sort"  -> StripSort
  | _       -> invalid_arg "not an event action"

let input_file = ref ""
let action = ref Clean

let param_specs =
  ["--db", Arg.String (fun log_name -> action := ToDb log_name),
     "Convert to CSV for a DB";
   "--events", Arg.String (fun s -> action := Events(event_action_of_string s)),
     "plain, strip, or sort"
  ]

let parse_cmd_line () =
  Arg.parse param_specs
    (fun f -> input_file := f)
    "Requires log file to convert"

let main () =
  parse_cmd_line ();
  if !input_file = "" then Arg.usage param_specs "Please enter a filename" else
  let log = load_log !input_file in
  let log = pre_sanitize log in
  let log = group_triggers log in
  let log = do_per_trigger log in
  match !action with
  | Clean -> print_endline @: string_of_log log
  | ToDb name ->
      let log = insert_index_fst 1 log in (* add indices *)
      let log = List.map (convert_to_db_format name) log in
      print_string @:
        String.concat "" @: List.map (fun t -> String.concat "" t) log
  | Events event_type ->
      let r_trig_add = regexp ".*Trigger insert_\\(.*\\)_send_fetch.*" in
      let r_trig_del = regexp ".*Trigger delete_\\(.*\\)_send_fetch.*" in
      let r_args = regexp ".*args = (\\(.*\\))"  in
      let filtered = filter_map (fun trig ->
        let acc, select =
          List.fold_left (fun ((trig,args),select) line ->
            if Str.string_match r_trig_add line 0 then 
              let trig' = "+"^Str.matched_group 1 line in
              (trig',args), true
            else if Str.string_match r_trig_del line 0 then 
              let trig' = "-"^Str.matched_group 1 line in
              (trig',args), true
            else if Str.string_match r_args line 0 then
              let args' = Str.matched_group 1 line in
              (trig,args'), select
            else (trig,args), select
          ) (("",""),false) trig
        in
        if select=true then Some acc else None
      ) log
      in
      let r_args = regexp "(\\(.*\\), \\(.*\\), \\(.*\\)), \\(.*\\)" in
      let split_data d = List.map (fun (trig, args) ->
        if not @: Str.string_match r_args args 0 then
          failwith "Bad args format"
        else
          let g i = Str.matched_group i args in
          let v1, v2, v3, args =
            ios @: g 1, ios @: g 2, ios @: g 3, g 4 in
          (v1, v2, v3), (trig, args)
      ) d
      in
      let sort d = List.sort (fun ((x1, y1, z1),_) ((x2, y2, z2),_) ->
        if x1 = x2 then if y1 = y2 then z1 - z2
                        else y1 - y2
        else x1 - x2
      ) d
      in
      let data = 
        match event_type with
        | Plain -> filtered
        | Strip -> snd_many @: split_data filtered (* remove vid *)
        | StripSort -> snd_many @: sort @: split_data filtered
      in
      print_string @:
        String.concat "\n" @: List.map (fun (t, args) -> t^": "^args) data

let _ = if not !Sys.interactive then Printexc.print main ()


