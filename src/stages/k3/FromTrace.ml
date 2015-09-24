(* Convert a file from a DBToaster trace to a test of values *)
open Util
open K3.AST

(* redefine sof (string_of_float) to write integers if possible *)
let sof f =
  let i = iof f in
  if foi i = f then soi i
  else sof f

(* concat a list of mapvals into a string *)
let concat_f str f_list =
  let s_list = list_map (fun f -> sof f) f_list in
  String.concat str s_list

(* float of string list *)
let foss l = list_map fos l

(* connect with newlines *)
let str_make = String.concat "\n"

module StrMap = Map.Make(struct type t = string let compare = compare end)

type mapval = Float of float | Int of int | String of string | Bool of bool | Date of int
type maptype = TFloat | TInt | TString | TDate | TBool

let r_float_dot = Str.regexp "^-?[0-9]+\\.[0-9]*$"

(* string_of_mapval *)
let somv = function
  | Float f  -> let s = sof f in
    (* because we type by decimal point, add one for float *)
    if r_match r_float_dot s then s else s^".0"
  | Int i    -> soi i
  | String s -> Printf.sprintf "\"%s\"" s
  | Bool b   -> sob b
  | Date i   -> soi i (* handle date like int for output purposes *)

let r_dash = Str.regexp "-"

(* mapval_of_string *)
let mvos typ s = match typ with
  | TFloat  -> Float(fos s)
  | TInt    -> Int(ios s)
  | TDate   -> Int(int_of_sql_date s)
  | TString -> let len = String.length s in
               if s.[len-1]='"' && s.[0]='"' then
                  String(String.sub s 1 (len-2))
               else invalid_arg @@ s^" is not a string"
  | TBool   -> Bool(bos s)

let mvos_many types ss = List.map2 mvos types ss

(* string_of_maptype *)
let somt = function
  | TFloat  -> "float"
  | TInt    -> "int"
  | TDate   -> "date"
  | TString -> "string"
  | TBool   -> "bool"

(* maptype_of_string *)
let mtos = function
  | "float"  -> TFloat
  | "int"    -> TInt
  | "date"   -> TDate
  | "string" -> TString
  | "bool"   -> TBool
  | _        -> invalid_arg "unhandled type"

let default_val = function
  | TFloat  -> Float(0.)
  | TInt
  | TDate   -> Int(0)
  | TBool   -> Bool(false)
  | TString -> String("")

(* concat a list of mapvals into a string *)
let concat_mv str mv_list =
  let s_list = list_map somv mv_list in
  String.concat str s_list

module SingletonMap = struct
  type t = {name:string; typ:maptype; v:mapval}

  let init name typ =
    let typ = mtos typ in
    let v = default_val typ in
    {name; typ; v}

  let set map ivars ovars v = {map with v}

  let set_s map ivars ovars v = set map [] [] (mvos map.typ v)

  let del map ivars ovars = {map with typ=TFloat; v=default_val TFloat}

  let del_s map ivars ovars = del map ivars ovars

  let to_s map = map.name^"-Singleton"

  let val_s map = String.concat "" @@ "("::somv map.v::[")"]
end

module OutputMap = struct
  type t = {
    name:string;
    typ:maptype;
    ovars:string list;
    otypes:maptype list;
    vs:(mapval list, mapval) Hashtbl.t;
  }

  let init name maptype ovars otypes =
    {name;
    typ=mtos maptype;
    ovars;
    otypes=List.map mtos otypes;
    vs=Hashtbl.create 0}

  let set map ivars ovars v =
    Hashtbl.replace map.vs ovars v;
    map

  let set_s map ivars ovars v =
    set map [] (mvos_many map.otypes ovars) (mvos map.typ v)

  let del map ivar ovars =
    Hashtbl.remove map.vs ovars;
    map

  let del_s map ivars ovars =
    del map [] (mvos_many map.otypes ovars)

  (* get the names of the map + output vars *)
  let to_s map =
    map.name^"["^String.concat ", " map.ovars^"]"

  (* get types of the map in string format *)
  let types_s map =
    String.concat ", " (List.map somt map.otypes)^", "^somt map.typ

  (* get values of the map in string format *)
  let val_s map =
    let s_list =
      List.map (concat_mv ", ") @@
      List.sort compare @@
      Hashtbl.fold (fun ovars v acc -> (ovars@[v])::acc) map.vs []
    in
    String.concat "; " s_list

end

module RelEvent = struct
  let next_evt_id = ref 0

  type op_t = Insert | Delete | System_Ready

  let string_of_op = function
    | Insert       -> "insert"
    | Delete       -> "delete"
    | System_Ready -> "system_ready_event"

                (*      ivars        ovars        val *)
  type hash_t = (op_t * string list * string list * string) list
  type t = {
    op:op_t;
    relname:string;
    types: maptype list;
    vals:mapval list;
    (* mapname, effect *)
    effects: (string, hash_t) Hashtbl.t;
    id:int;
  }

  let init oper relname types values =
    (* debug *)
    (*Printf.printf "types[%s] values[%s]" (String.concat ";" types) (String.concat ";" values);*)
    (*print_newline ();*)
    let op = match oper with
      | "+" -> Insert
      | "-" -> Delete
      | _   -> System_Ready in
    let types = List.map mtos types in
    let vals = mvos_many types values in
    let r = {
        op; relname; vals; types;
        effects=Hashtbl.create 0;
        id = !next_evt_id
      } in
    next_evt_id := !next_evt_id + 1;
    r

  let add_effect evt mapn ivars ovars v =
    let eff_list =
      try Hashtbl.find evt.effects mapn
      with Not_found -> [] in
    let eff' = (Insert, ivars, ovars, v)::eff_list in
    Hashtbl.replace evt.effects mapn eff';
    evt

  let del_effect evt mapn ivars ovars =
    let eff_list =
      try Hashtbl.find evt.effects mapn
      with Not_found -> [] in
    let eff' = (Delete, ivars, ovars, "0.")::eff_list in
    Hashtbl.replace evt.effects mapn eff';
    evt

  let to_s evt = match evt.op with
    | Insert | Delete -> string_of_op evt.op ^"_"^ evt.relname
    | System_Ready    -> "system_ready_event"

  (* convert the event to a send message *)
  let dispatch_s ~last evt =
    let send = "send("^to_s evt^", me, "^concat_mv ", " evt.vals^")" in
    if last then send else send^";"

  (* convert the event to a stream message *)
  let stream_s events =
    if events = [] then [""] else
    let groups = match events with
    | [e] -> [[e]]
    | _   -> let fst_evt = hd events in
      (* group events by op and name *)
      let _,_,gs = List.fold_left
        (fun (last_op, last_relname, groups) evt ->
          match groups with
          | []        -> failwith "error"
          | grp::grps ->
            (* continue the same group *)
            if evt.op = last_op && evt.relname = last_relname then
              last_op, last_relname, (evt::grp)::grps
            else (* start a new group *)
              evt.op, evt.relname, [evt]::grp::grps)
        (fst_evt.op, fst_evt.relname, [[fst_evt]])
        (tl events)
      in
      (* reverse all groups *)
      List.rev_map (fun grp -> List.rev grp) gs
    in
    let src num = Printf.sprintf "s%d" num in
    let len = List.length groups in
    let _, src_s = mapfold (fun num group ->
      let src = src num in
      let evt = hd group in
      let types = match evt.types with
        | []  -> failwith "missing types"
        | [t] -> somt t
        | ts  -> "("^String.concat ", " (List.map somt ts)^")"
      in
      let vals_inner = list_map (fun evt -> concat_mv ", " evt.vals) group in
      let vals = "["^String.concat "; " vals_inner^"]" in
      num - 1, Printf.sprintf "ssource %s : %s = stream(%s)\n\
                      bindflow %s -> %s"
                      src types vals
                      src (to_s evt)
    ) len groups
    in
    let _, consume_s = mapfold (fun num group ->
      num - 1, Printf.sprintf "sconsume %s" (src num)
    ) len groups
    in
    src_s@consume_s

end

(* possible map types *)
type map_t = SingletonMap of SingletonMap.t | OutputMap of OutputMap.t

module StringMap = Map.Make(struct type t = string let compare = compare end)

(* update map values to match events *)
let update_maps maps events =
  if StringMap.is_empty maps then failwith "empty maps" else
  List.fold_left (fun acc evt ->
    Hashtbl.fold (fun mapname eff_list acc' ->
      let map = try StringMap.find mapname acc'
        with Not_found -> failwith @@ "Couldn't find "^mapname
      in
      List.fold_left (fun acc'' (op, ivars, ovars, v) ->
        (* debug *)
        (*print_endline @@ mapname^" "^concat_f "," ovars^":= "^sof v;*)
        let m' = match map, op with
        | SingletonMap m, (RelEvent.Insert | RelEvent.System_Ready) ->
            SingletonMap(SingletonMap.set_s m ivars ovars v)
        | SingletonMap m, RelEvent.Delete ->
            SingletonMap(SingletonMap.del_s m ivars ovars)
        | OutputMap m, (RelEvent.Insert | RelEvent.System_Ready) ->
            OutputMap(OutputMap.set_s m ivars ovars v)
        | OutputMap m, RelEvent.Delete ->
            OutputMap(OutputMap.del_s m ivars ovars)
        in
        StringMap.add mapname m' acc''
      ) acc' eff_list
    ) evt.RelEvent.effects acc
  ) maps events

(* forward *)
let wrap =  ref (fun (x:string) -> x)

(* dump a map into a string *)
let dump_map ~is_dist mapname m =
  let w = if is_dist then !wrap else fun s -> "{|"^s^"|}" in
  match m with
  | SingletonMap m -> w @@ SingletonMap.val_s m
  | OutputMap m    ->
      let s = OutputMap.val_s m in
      (* if our map is empty, we need the types *)
      if s = "" then (w "") ^ " : " ^ (w @@ OutputMap.types_s m)
      else w @@ OutputMap.val_s m

(* return the dimensions of the map *)
let map_dims = function
    | SingletonMap _ -> 1
    | OutputMap m    -> 1 + List.length m.OutputMap.ovars

let r_semi = Str.regexp "; "
let r_comma = Str.regexp ", "
let r_declare_map_short = Str.regexp "DECLARE MAP"
let r_declare_map = Str.regexp
    "DECLARE MAP \\([^(]+\\)(\\(int\\|float\\|string\\|date\\|bool\\))\\[\\([^]]*\\)\\]\\[\\([^]]*\\)\\]"
let r_system_ready = Str.regexp "ON SYSTEM READY {"
let r_system_ready_empty = Str.regexp "ON SYSTEM READY <- \\[\\]"
let r_event = Str.regexp
    "ON \\(\\+\\|-\\) \\([^(]+\\)(\\([^)]+\\)) <- \\[\\([^]]*\\)\\]"
let r_update = Str.regexp
    "UPDATE '\\([^']*\\)'\\[\\([^]]*\\)\\]\\[\\([^]]*\\)\\] := \\(.*\\)$"
let r_remove = Str.regexp
    "REMOVE '\\([^']*\\)'\\[\\([^]]*\\)\\]\\[\\([^]]*\\)\\]"
let r_colon = Str.regexp ":"

(* we need to correct for cases when we have a string with a separator inside. The symptom
 * after splitting is that we'll have a member with only a single quote inside. *)
let r_quote = Str.regexp "^[^\"]*\"[^\"]*$"
let correct_for_strings ss =
  List.rev @@ fst @@ List.fold_left (fun (acc, concat) s ->
    let has_quote = r_match r_quote s in
    match concat, has_quote, acc with
    | false, false, _ -> s::acc, false
    | false, true,  _ -> s::acc, true
    | true,  false, x::acc -> (x^s)::acc, true (* we're now concatenating *)
    | true,  true,  x::acc -> (x^s)::acc, false
    | _, _, _ -> invalid_arg "invalid string quotes in list"
  ) ([], false) ss


let parse_trace file =
  let lines = read_file_lines file in
  let maps, _, sys_ready, events, _ =
    List.fold_left (fun (maps, line, sys_ready, events, acc_str) s ->
      let str = acc_str^s in
      (* parse declare map *)
      (* because we split up into lines, we may miss some things. We accumulate once we
       * pick up on small pieces of what we want *)
      if r_match r_declare_map_short str then (* we picked up on a map partially *)
        let m = r_groups str ~n:4 ~r:r_declare_map in
        match m with
        | [] -> (* not enough to read, so add a line *)
            maps, line+1, sys_ready, events, str

        | [_; _; Some ivars; _] when ivars <> "" ->
            failwith @@ "input vars unsupported at line "^soi line

        | [Some mapname; Some maptype; _; None] ->
            let newmap = SingletonMap(SingletonMap.init mapname maptype) in
            let maps' = StringMap.add mapname newmap maps in
            maps', line+1, sys_ready, events, ""

        | [Some mapname; Some maptype; _; Some ovars] ->
            let newmap =
              let ovars_s = Str.split r_comma ovars in
              let ovars', otypes =
                List.split @@ list_map (fun v ->
                  match Str.split r_colon v with
                  | [x;y] -> (x,y)
                  | _     -> failwith @@ "incorrect data at line "^soi line)
                ovars_s in
              OutputMap(OutputMap.init mapname maptype ovars' otypes)
            in
            let maps' = StringMap.add mapname newmap maps in
            maps', line+1, sys_ready, events, ""

        | _ -> failwith @@ "couldn't extract map details at line "^soi line

      else
      (* parse system ready *)
      if r_match r_system_ready str then
        maps, line+1, true, events, acc_str else
      if r_match r_system_ready_empty str then
        (* only if full system ready has been seen *)
        if sys_ready then
          let evt = RelEvent.init "system_ready" "" ["int"] ["0"] in
          maps, line+1, sys_ready, evt::events, acc_str
        else
          maps, line+1, sys_ready, events, acc_str
      else
      (* parse an event *)
      let m = r_groups str ~n:4 ~r:r_event in
      if not @@ null m then
        match m with
        | [Some op; Some relname; Some id_types; Some vals] ->
            let id_types = Str.split r_comma id_types in
            let id_types = List.map (Str.split r_colon) id_types in
            let types = List.map (function
              | _::y::_ -> y
              | _       -> failwith @@ "Bad data at line "^soi line)
            id_types in
            let vals = correct_for_strings @@ Str.split r_semi vals in
            let evt = RelEvent.init op relname types vals in
            maps, line+1, sys_ready, evt::events, acc_str

        | _ -> failwith @@ "invalid input for ON at line "^soi line
      else
      (* parse an update *)
      let m = r_groups str ~n:4 ~r:r_update in
      if not @@ null m then match m with
        | [Some mapname; Some ivars; Some ovars; Some v] ->
            let ivars = if ivars = "-" then []
              else correct_for_strings @@ Str.split r_semi ivars in
            let ovars = if ovars = "-" then []
              else correct_for_strings @@ Str.split r_semi ovars in
            begin match events with
            | []    -> failwith @@ "No event to update at line "^soi line
            | e::es ->
                let e' = RelEvent.add_effect e mapname ivars ovars v in
                maps, line+1, sys_ready, e'::es, acc_str
            end
        | _ -> failwith @@ "error in update at line "^soi line
      else
      (* parse a remove *)
      let m = r_groups str ~n:3 ~r:r_remove in
      if not @@ null m then match m with
          | (Some mapname)::(Some ivars)::(Some ovars)::_ ->
            let ivars = if ivars = "-" then []
                        else correct_for_strings @@ Str.split r_semi ivars in
            let ovars = if ovars = "-" then []
                        else correct_for_strings @@ Str.split r_semi ovars in
            begin match events with
            | []    -> failwith @@ "No event to update at line "^soi line
            | e::es ->
                let e' = RelEvent.del_effect e mapname ivars ovars in
                maps, line+1, sys_ready, e'::es, acc_str
            end
          | _ -> failwith @@ "error in remove at line "^soi line
      else
        maps, line+1, sys_ready, events, acc_str
    ) (StringMap.empty, 1, false, [], "") lines
  in
  let events = List.rev events in
  (* get rid of events with no effects *)
  let events = filter_map (fun e ->
    let len = Hashtbl.length e.RelEvent.effects in
    if len = 0 then None
    else begin
      (* reverse the effect lists *)
      Hashtbl.iter (fun mapname efflist ->
        Hashtbl.replace e.RelEvent.effects mapname @@ List.rev efflist
      ) e.RelEvent.effects;
      Some e
    end
  ) events
  in
  events, maps, sys_ready

let string_of_go_trig ~has_sys_ready events =
  (* the extra input trigger *)
  let s = ["trigger go(id : int) {} = do {\n"] in
  let s = if has_sys_ready then s@["  send(system_ready_event, me, 1);\n"] else s
  in
  let len = List.length events in
  let s2 = list_mapi (fun (i, x) ->
    let last = if i >= len - 1 then true else false in
    Printf.sprintf "  %s\n" @@ RelEvent.dispatch_s x ~last
  ) events
  in
  let s2 = s2@["}\n"] in
  String.concat "" @@ s@s2

(* parse an order file, which is an alternative to a trace file *)
let events_of_order_file file =
  let lines = read_file_lines file in
  (* read a line of the order file: +R: 4, 3 *)
  let r_line = Str.regexp "\\(.\\)\\([^:]+\\): \\(.*\\)" in
  let r_float = Str.regexp "^-?[0-9]+\\.[0-9]*$" in (* check if float *)
  let r_int = Str.regexp "^-?[0-9]+$" in
  let r_date = Str.regexp "^[0-9]+-[0-9]+-[0-9]+$" in
  List.map (fun line ->
    if not @@ Str.string_match r_line line 0 then
      failwith "Bad order file syntax"
    else
      let g i = Str.matched_group i line in
      let op, name, args = g 1, g 2, g 3 in
      let argl = Str.split r_comma args in
      let argl = correct_for_strings argl in
      let types = List.map (fun str ->
        if r_match r_float str then "float" else
        if r_match r_int str then "int" else
        if r_match r_date str then "date" else
        if str="true" || str="false" then "bool" else
          "string"
      ) argl in
      (* debug *)
      (*Printf.printf "%s\n%s\n" args @@ String.concat ", " types;*)
      RelEvent.init op name types argl
  ) lines

let strings_of_test_role ~is_dist events =
  if is_dist then
    (str_make @@
      "trigger node_dummy(x:int) {} = ()"::
      "role switch {"::
        RelEvent.stream_s events@
      ["}";
       "role node {";
       "  ssource s_dummy : int = stream([1])";
       "  bindflow s_dummy -> node_dummy";
       "  sconsume s_dummy";
       "}"
      ])::
       "default role switch"::
      []
  else (* single-site *)
    if events <> [] then
      (str_make @@
        "srole switch {"::
        RelEvent.stream_s events@
        "}"::[])::
        "sdefault srole switch\n"::
        []
    else ["trigger dummy(x:int) {} = ()\n\n\
            srole switch {\n\
            ssource dummy : int = stream([1])\n\
            bindflow dummy -> dummy\n\
            sconsume dummy\n\
            }\n\
           default role switch\n"]

let string_of_test_role ~is_dist events =
  str_make @@ strings_of_test_role ~is_dist events

(* convert the maps to a list *)
let string_of_maps ~is_dist maps =
  StringMap.fold (fun name mapdata acc ->
    (name, dump_map ~is_dist name mapdata)::acc
  ) maps []

(* get the names and dimensions of maps *)
let map_names_and_dims maps =
  StringMap.fold (fun name mapdata acc ->
    (name, map_dims mapdata)::acc
  ) maps []

(* combine the map strings (left string, right string) into their final form *)
let dump_map_strings maps =
  let maps' = list_map (fun (left_s, right_s) ->
    String.concat "" @@ left_s::" = "::[right_s]
  ) maps
  in
  String.concat ", " maps'

(* Convert a file to a string representation *)
let string_of_file file ~is_dist =
  let events, maps, sys_ready = parse_trace file in
  (* debug *)
  (*print_endline @@ String.concat ", " @@ fst_many @@ string_of_maps maps;*)
  (* update all maps with the event data *)
  let maps = update_maps maps events in
  (* list of all maps and their data *)
  let mapl = string_of_maps ~is_dist maps in
  (*let trig_s = string_of_go_trig ~has_sys_ready:sys_ready events in*)
  let role_s = strings_of_test_role ~is_dist events in
  (* return the tree components we have to add *)
  str_make role_s, mapl


