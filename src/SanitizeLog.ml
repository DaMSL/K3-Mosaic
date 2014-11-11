open Util
open Str

let load_log file = read_file_lines file

type action = Clean | ToDb of string | Events of event_action

(* what to do in case of event output *)
and event_action = Plain | Strip | StripSort

let input_file = ref ""
let action = ref Clean
let k3new  = ref false

let event_action_of_string = function
  | "plain" -> Plain
  | "strip" -> Strip
  | "sort"  -> StripSort
  | _       -> invalid_arg "not an event action"

let param_specs =
  ["--db", Arg.String (fun log_name -> action := ToDb log_name),
     "Convert to CSV for a DB";
   "--events", Arg.String (fun s -> action := Events(event_action_of_string s)),
     "plain, strip, or sort";
   "--k3new", Arg.Set k3new, "Handle k3new files"
  ]

let parse_cmd_line () =
  Arg.parse param_specs
    (fun f -> input_file := f)
    "Requires log file to convert"


let pre_sanitize log =
  if !k3new then log else
  let r_old_consuming = Str.regexp ".*consuming" in
  let r_old_arrows = Str.regexp ".*>>>>.*" in
  let _, lines =
    List.fold_left (fun (remove_rest, acc) line ->
      if r_match r_old_consuming line then remove_rest, acc
      else if r_match r_old_arrows line then true, acc
      else if remove_rest then true, acc
      else false, line::acc
    ) (false, []) log
  in
  List.rev lines

(* group triggers into batches *)
let group_triggers log =
  let r_old_trig = (regexp "send(.*\\|Trigger.*", regexp "^$") in
  let r_new_trig = (regexp ".*Message for:.*", regexp ".*=====.*")  in
  let r_trig = if !k3new then r_new_trig else r_old_trig in
  let _, trigs =
    List.fold_left (fun (collect, acc_groups) line ->
        let add_to_group () = match acc_groups with
          | []    -> []             (* no group. don't add *)
          | x::xs -> (line::x)::xs
        in
        if collect then
          if r_match (snd r_trig) line then
            false, acc_groups
          else
            true, add_to_group ()
        else (* not collect *)
          if r_match (fst r_trig) line then
            true, [line]::acc_groups
          else
            false, acc_groups)
      (false, [])
      log
  in
  List.rev_map (fun trigl -> List.rev trigl) trigs

let unwanted_lines =
  [ "----Globals"
  ; "----Frames"
  ; "Contents: "
  ; "Environment:"
  ]

let unwanted_lines_r = List.map (fun str -> regexp @@ str^".*") unwanted_lines

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

let unwanted_multilines_r = List.map (fun str -> regexp @@ str^".*") unwanted_multilines

(* check if any of the unwanted regexs match our line *)
let check_unwanted unwanted line =
  List.exists (fun regex -> string_match regex line 0) unwanted

let remove_unwanted trig =
  let r_equals = regexp "[^=]*=.*" in
  let r_trig = regexp ".*Trigger.*" in (* remove all before trig *)
  List.rev @@ snd @@
    List.fold_left (fun (remove, acc) line ->
      if string_match r_trig line 0 then remove, line::acc
      else if check_unwanted unwanted_lines_r line then remove, acc
      else if check_unwanted unwanted_multilines_r line then true, acc
      (* reset the remove flag if we see another = *)
      else if string_match r_equals line 0 then false, line::acc
      else remove, line::acc)
    (false, [])
    trig

let r_equals = Str.regexp " = "
let r_amper = regexp "@"

(* remove headings and change them into fields *)
let vid_r =  regexp ".*__vid_counter__ =.*"
let send_r = regexp "^send(.*"
let trig_old_r = regexp {|.*Trigger \(.*\)@\(.*\)|}
let trig_new_r = regexp {|.*Message for: \(.*\)@\(.*\)|}

let clean_up_headings trig =
  (* get vid *)
  let trig_r = if !k3new then trig_new_r else trig_old_r in
  let vid_s = List.find (r_match vid_r) trig in
  (* accumulate sends over multiple lines *)
  let x = List.fold_left (fun (status, t, acc, acc_sends) line ->
            if r_match send_r line then      `InSend, t, acc, line::acc_sends
            else if r_match trig_r line then `InTrig, Some line, acc, acc_sends
            else match status, acc_sends with
              | `InSend, x::xs -> `InSend, t, acc, (x^line)::xs
              | `InTrig, _     -> `InTrig, t, line::acc, acc_sends
              | _              ->  failwith "bad state"
            ) (`Out, None, [], []) trig
  in
  match x with
  | _, None, _, _ -> failwith "no trigger name found"
  | _, Some t, acc, acc_sends ->
    let send_s = match acc_sends with [] -> [] | _ -> ["send_msgs = "^String.concat "; " acc_sends] in
    let vid_s = at (Str.split r_equals vid_s) 1 in
    let trig_line = match r_groups t ~r:trig_r ~n:2 with
    | [Some trig_s; Some addr_s] -> Format.sprintf "%s, %s %s:" vid_s addr_s trig_s
    | _ -> failwith "bad trigger pattern"
    in
    trig_line :: List.rev send_s @ List.rev acc


(* for new format: switch records to tuples *)
let r_label = regexp {|i:\|key:\|value:\|_r[0-9]_+:|}
let r_lbrace = regexp "{"
let r_rbrace = regexp "}"
let record_to_tuple line =
     Str.global_replace r_label "" line
  |> Str.global_replace r_lbrace "("
  |> Str.global_replace r_rbrace ")"

(* for new format: remove junk from beginning of lines *)
let r_new_prefix = regexp {|\[.*trace\] *|}
let remove_junk line = Str.global_replace r_new_prefix "" line
let clean_lines t =
  if !k3new then
    List.map (record_to_tuple |- remove_junk) t
  else t

let do_per_trigger log =
  List.map (remove_unwanted |- clean_up_headings |- clean_lines) log

let r_sp_begin = regexp {|^ +\([^ ].*\)$|}  (* spaces followed by anything *)
let r_trig = regexp {|Trigger \(.+\)|}
let r_eq_line = regexp ".+ =.*"

let convert_to_db_format log_name (idx, trig) =
  let drop_spaces s =
    if r_match r_sp_begin s then
      Str.replace_first r_sp_begin {|\1|} s
    else s
  in
  let trig_nm = hd trig in
  (* join map lines together *)
  let trig =
    List.fold_left (fun trig_acc str ->
      (* check for a new line *)
      if r_match r_eq_line str then str::trig_acc
      else match trig_acc with
      | []    -> [str]
      | x::xs -> (x^drop_spaces str)::xs
    ) [] (tl trig)
  in
  (* decompose trigger name *)
  if not @@ r_match r_trig trig_nm then failwith "trigger heading mismatch";
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

let main () =
  parse_cmd_line ();
  if !input_file = "" then Arg.usage param_specs "Please enter a filename" else
  let log = load_log !input_file in
  let log = pre_sanitize log in
  let log = group_triggers log in
  let log = do_per_trigger log in
  List.iter (fun t -> List.iter print_endline t; print_newline ()) log
  (*
  match !action with
  | Clean -> print_endline @@ string_of_log log
  | ToDb name ->
      let log = insert_index_fst 1 log in (* add indices *)
      let log = List.map (convert_to_db_format name) log in
      print_string @@
        String.concat "" @@ List.map (fun t -> String.concat "" t) log
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
        if select then Some acc else None
      ) log
      in
      let r_args = regexp "(\\(.*\\), \\(.*\\), \\(.*\\)), \\(.*\\)" in
      let split_data d = List.map (fun (trig, args) ->
        if not @@ Str.string_match r_args args 0 then
          failwith "Bad args format"
        else
          let g i = Str.matched_group i args in
          let v1, v2, v3, args =
            ios @@ g 1, ios @@ g 2, ios @@ g 3, g 4 in
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
        | Strip -> snd_many @@ split_data filtered (* remove vid *)
        | StripSort -> snd_many @@ sort @@ split_data filtered
      in
      print_string @@
        String.concat "\n" @@ List.map (fun (t, args) -> t^": "^args) data
    *)

let _ = if not !Sys.interactive then Printexc.print main ()


