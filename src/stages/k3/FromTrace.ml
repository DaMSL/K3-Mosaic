(* Convert a file from a DBToaster trace to a test of values *)
open Util

(* redefine sof (string_of_float) to write integers if possible *)
let sof f = 
  let i = iof f in
  if foi i = f then soi i
  else sof f

(* concat a list of floats into a string *)
let concat_f str f_list =
  let s_list = list_map (fun f -> sof f) f_list in
  String.concat str s_list

(* float of string list *)
let foss l = list_map fos l

module StrMap = Map.Make(struct type t = string let compare = compare end)

module SingletonMap = struct
  type t = {name:string; typ:string; v:float}

  let init name typ = {name=name; typ=typ; v=0.}

  let set map ivars ovars v = {map with v=v}

  let del map ivars ovars = {map with v=0.}

  let to_s map = map.name^"-Singleton"

  let val_s map = String.concat "" @: "("::sof map.v::[")"]
end

module OutputMap = struct
  type t = {
    name:string;
    typ:string;
    ovars:string list;
    otypes:string list;
    vs:(float list, float) Hashtbl.t;
  }

  let init mapname maptype ovars otypes =
    {name=mapname; 
    typ=maptype; 
    ovars=ovars;
    otypes=otypes;
    vs=Hashtbl.create 0}

  let set map ivars ovars v =
    Hashtbl.replace map.vs ovars v;
    map

  let del map ivar ovars =
    Hashtbl.remove map.vs ovars;
    map

  let to_s map =
    map.name^"["^String.concat ", " map.ovars^"]"

  let val_s map =
    let s_list = 
      Hashtbl.fold (fun ovars v acc ->
        let s = concat_f ", " (ovars@[v]) in
        s::acc) map.vs []
    in
    String.concat "; " s_list
    
end

module RelEvent = struct
  let next_evt_id = ref 0

  type op_t = Insert | Delete

  let string_of_op = function 
    Insert -> "insert" | Delete -> "delete"

                (*      ivars        ovars        val *)
  type hash_t = (op_t * float list * float list * float) list
  type t = {
    op:op_t;
    relname:string;
    vals:float list;
    (* mapname, effect *)
    effects: (string, hash_t) Hashtbl.t;
    id:int;
  }

  let init op relname vals =
    let r = {
        op = if op = "+" then Insert else Delete;
        relname=relname;
        vals=vals;
        effects=Hashtbl.create 0;
        id = !next_evt_id
      } in
    next_evt_id := !next_evt_id + 1;
    r

  let add_effect evt mapn ivars ovars v =
    let eff_list = 
      try Hashtbl.find evt.effects mapn
      with Not_found -> [] in
    let eff' = (Insert, ivars, ovars, v):: eff_list in
    Hashtbl.replace evt.effects mapn eff';
    evt

  let del_effect evt mapn ivars ovars =
    let eff_list = 
      try Hashtbl.find evt.effects mapn
      with Not_found -> [] in
    let eff' = (Delete, ivars, ovars, 0.):: eff_list in
    Hashtbl.replace evt.effects mapn eff';
    evt

  let to_s evt = string_of_op evt.op ^ evt.relname

  let dispatch_s ~last evt =
    let send = "send("^to_s evt^", me, "^concat_f ", " evt.vals^")" in
    if last then send else send^";"

end

(* possible map types *)
type map_t = SingletonMap of SingletonMap.t | OutputMap of OutputMap.t

(* update map values to match events *)
let update_maps (maps:(string, map_t) Hashtbl.t) events =
  if Hashtbl.length maps = 0 then failwith "empty maps";
  List.iter (fun evt ->
    Hashtbl.iter (fun mapname eff_list ->
      let map = Hashtbl.find maps mapname in
      List.iter (fun (op, ivars, ovars, v) ->
        (* debug *)
        print_endline @: mapname^" "^concat_f "," ovars^":= "^sof v;
        let m' = match map, op with
        | SingletonMap m, RelEvent.Insert -> 
            SingletonMap(SingletonMap.set m ivars ovars v)
        | SingletonMap m, RelEvent.Delete -> 
            SingletonMap(SingletonMap.del m ivars ovars)
        | OutputMap m, RelEvent.Insert -> 
            OutputMap(OutputMap.set m ivars ovars v)
        | OutputMap m, RelEvent.Delete ->
            OutputMap(OutputMap.del m ivars ovars)
        in
        Hashtbl.replace maps mapname m'
      ) eff_list
    ) evt.RelEvent.effects
  ) events

(* dump a map into a string *)
let dump_map mapname mapdata =
  String.concat "" @: 
    "\n"::
    mapname::" = {\n"::
    (match mapdata with
    | SingletonMap m -> SingletonMap.val_s m
    | OutputMap m    -> OutputMap.val_s m)::"\n"::
    "}\n"::
    []

let parse_trace file ~dist =
  let (maps : (string, map_t) Hashtbl.t) = Hashtbl.create 0 in
  let lines = read_file_lines file in

  let _, sys_ready, events =
    List.fold_left (fun (line, sys_ready, events) str ->
      let m = r_groups str ~n:4
        ~r:"DECLARE MAP \\([^(]+\\)(\\(int\\|float\\))\\[\\([^]]*\\)\\]\\[\\([^]]*\\)\\]"
      in
      if not @: null m then
        let mapname, maptype, ivars, ovars = at m 0, at m 1, at m 2, at m 3 in
        if is_some ivars && ivars <> Some "" then 
          failwith @: "input vars unsupported at line "^soi line;
        let new_map = match ovars, mapname, maptype with
          | None, Some mapname, Some maptype ->
              SingletonMap(SingletonMap.init mapname maptype)
          | Some ovars', Some mapname, Some maptype ->
              let ovars_s = r_split ", *" ovars' in
              let ovars, otypes = 
                List.split @: list_map (fun v -> 
                  let l = r_split ":" v in
                  hd l, at l 1) ovars_s in
              OutputMap(OutputMap.init mapname maptype ovars otypes)

          | _ -> failwith @: "missing values at line "^soi line
        in Hashtbl.add maps (unwrap_some mapname) new_map;
        line+1, sys_ready, events
      else
      if r_match "ON SYSTEM READY {\n[^}]*}" str then 
        line+1, true, events else
      let m = r_groups str ~n:3
        ~r:"ON \\(\\+\\|-\\) \\([^(]+\\)([^)]*) <- \\[\\([^]]*\\)\\]" in
      if not @: null m then
        match at m 0, at m 1, at m 2 with
        | Some op, Some relname, Some vals ->
            let vals = foss @: r_split "; *" vals in
            let evt = RelEvent.init op relname vals in
            line+1, sys_ready, evt::events
        | _ -> failwith @: "invalid input for ON at line "^soi line
      else
      let m = r_groups str ~n:4
        ~r:"UPDATE '\\([^']*\\)'\\[\\([^]]*\\)\\]\\[\\([^]]*\\)\\] := \
          \\(.*\\)$" in
      if not @: null m then match m with
          | (Some mapname)::(Some ivars)::(Some ovars)::(Some v)::_ ->
            let ivars_f = if ivars = "-" then [] else foss @: r_split "; " ivars in
            let ovars_f = if ovars = "-" then [] else foss @: r_split "; " ovars in
            let v_f = fos v in
            begin match events with
            | []    -> failwith @: "No event to update at line "^soi line
            | e::es -> 
                let e' = RelEvent.add_effect e mapname ivars_f ovars_f v_f in
                line+1, sys_ready, e'::es
            end
          | _ -> failwith @: "error in update at line "^soi line
      else
      let m = r_groups str ~n:3
        ~r:"REMOVE '\\([^']*\\)'\\[\\([^]]*\\)\\]\\[\\([^]]*\\)\\]" in
      if not @: null m then match m with
          | (Some mapname)::(Some ivars)::(Some ovars)::_ ->
            let ivars_f = if ivars = "-" then [] else foss @: r_split "; " ivars in
            let ovars_f = if ovars = "-" then [] else foss @: r_split "; " ovars in
            begin match events with
            | []    -> failwith @: "No event to update at line "^soi line
            | e::es -> 
                let e' = RelEvent.del_effect e mapname ivars_f ovars_f in
                line+1, sys_ready, e'::es
            end
          | _ -> failwith @: "error in update at line "^soi line
      else 
        line+1, sys_ready, events 
    ) (1, false, []) lines
  in
  let events = List.rev events in (* reverse events *)
  let s = ["trigger go(id : int) {} = do {\n"] in
  let s = if sys_ready then s@["  send(system_ready_event, mem 1);\n"] else s
  in
  let len = List.length events in
  let s2 = list_mapi (fun (i, x) ->
    let last = if i >= len - 1 then true else false in
    Printf.sprintf "  %s\n" @: RelEvent.dispatch_s x ~last
  ) events
  in
  let s2 = s2@["}"] in

  let s3 = 
    if dist then
      "\n"::
      "role switch {\n"::
      "  source s_on_init : int = stream([1])\n"::
      "  bind s_on_init -> on_init\n"::
      "  consume s_on_init\n"::
      "  source s1 : int = stream([1])\n"::
      "  bind s1 -> go\n"::
      "  consume s1\n"::
      "}\n"::
      "\n"::
      "role node {\n"::
      "  source s_on_init : int = stream([1])\n"::
      "  bind s_on_init -> on_init\n"::
      "  consume s_on_init\n"::
      "}\n"::
      "\n"::
      "default role switch\n"::
      "\n"::
      "network expected\n"::
      []
    else (* single-site *)
      "\n"::
      "role test {\n"::
      "  source s1 : int = stream([1])\n"::
      "  bind s1 -> go\n"::
      "  consume s1\n"::
      "}\n"::
      "\n"::
      "default role test\n"::
      "\n"::
      "expected\n"::
      []
  in
  update_maps maps events;
  (* list of all maps and their data *)
  let mapl = 
    Hashtbl.fold (fun name mapdata acc ->
      (dump_map name mapdata)::acc
    ) maps [] 
  in
  let map_s = String.concat ", " mapl in
  let s' = s@s2@s3@[map_s] in
  String.concat "" s'
