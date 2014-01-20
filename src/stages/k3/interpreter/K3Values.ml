open Util
open Printing

open K3.AST
open K3Util
open K3Printing
open K3Helpers

exception RuntimeError of int * string

(* Interpreter representation of values *)

module IdMap = Map.Make(struct type t = id_t let compare = String.compare end)

(* add to the id_map from a list *)
let add_from_list map l = 
  List.fold_left (fun acc (k, v) ->
    IdMap.add k v acc
  ) map l

let map_modify f key map =
  let oldval = 
    try 
      Some(IdMap.find key map)
    with Not_found -> None
  in
  match f oldval with
  | None   -> IdMap.remove key map
  | Some v -> IdMap.add key v map

let map_length map = IdMap.fold (fun _ _ sum -> sum + 1) map 0

type eval_t = VDeclared of value_t ref | VTemp of value_t

and foreign_func_t = env_t -> env_t * eval_t

and value_t
    = VUnknown
    | VUnit
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VByte of char
    | VString of string
    | VTuple of value_t list
    | VOption of value_t option
    | VSet of value_t list
    | VBag of value_t list
    | VList of value_t list
    | VFunction of arg_t * expr_t
    | VForeignFunction of arg_t * foreign_func_t
    | VAddress of address
    | VTarget of id_t

    (* arguments to a function/trigger *)
and frame_t = (id_t * value_t) list 

(* mutable environment, frame environment *)
and env_t = (value_t ref) IdMap.t * (frame_t list)

(* trigger env is where we store the trigger functions. These functions take the
 * address,
 * scheduler_state (parametrized here to prevent circular inclusion), the
 * environment, value_t of arguments, and produce unit *)
type trigger_env_t = (address -> env_t -> value_t -> unit) IdMap.t
type program_env_t = trigger_env_t * env_t

(* Value stringification *)
let unwrap opt = match opt with Some v -> v | _ -> failwith "invalid option unwrap"

(* Value comparison. *)
let rec equal_values a b = 
  let sort x = List.sort compare x in
  match a,b with
  | (VSet l | VBag l), (VSet r | VBag r) ->
      (try List.for_all2 equal_values (sort l) (sort r)
       with Invalid_argument _ -> false)
    (* consider float equality within a certain epsilon *)
  | VFloat x, VFloat y ->
      let e = 0.0001 in
      abs_float(x -. y) < e
  | a,b -> a = b

(* Value sorting *)
let rec sort_values v = 
  let sort x = List.sort compare x in
  match v with
  | VSet l          -> VSet(sort @: List.map sort_values l)
  | VBag l          -> VBag(sort @: List.map sort_values l)
  | VTuple l        -> VTuple(List.map sort_values l)
  | VOption(Some t) -> VOption(Some (sort_values t))
  | VList l         -> VList(List.map sort_values l)
  | x               -> x

let rec repr_of_value v = match v with
	| VUnknown  -> "VUnknown"
	| VUnit     -> "VUnit"
	| VBool b   -> "VBool("^ string_of_bool b^")"
	| VInt i    -> "VInt("^ string_of_int i^")"
	| VFloat f  -> "VFloat("^ string_of_float f^")"
	| VByte c   -> "VByte("^ string_of_int (Char.code c)^")"
	| VString s -> "VString("^s^")"
	| VTuple vs -> "VTuple("^ String.concat ", " (List.map repr_of_value vs)^")"
	
	| VOption vopt ->
	  "VOption("^(if vopt = None then "None" else repr_of_value (unwrap vopt))^")"
	
	| VSet vs  -> "VSet(["^ String.concat "; " (List.map repr_of_value vs)^"])"
	| VBag vs  -> "VBag(["^ String.concat "; " (List.map repr_of_value vs)^"])"
	| VList vs -> "VList(["^ String.concat "; " (List.map repr_of_value vs)^"])"
	
	| VFunction (a, b) -> "VFunction("^ string_of_arg a ^" -> "^(string_of_expr b)^")"
  | VForeignFunction (a, _) -> "VForeignFunction("^ string_of_arg a^")"
	| VAddress (ip,port) -> "VAddress("^ip^":"^ string_of_int port^")"
	| VTarget id -> "VTarget("^id^")"


(* mark_points are optional sorted counts of where we want markings *)
let rec print_value ?(mark_points=[]) v =
  let count = ref 0 in
  let rec loop ~mark_points v =
    incr count;
    (* debug *)
    (*ps @: Printf.sprintf "%d/" !count;*)
    (* mark if we're at the right point *)
    let mark_points = match mark_points with
      | x::xs when !count = x -> ps "%"; xs
      | xs                    -> xs
    in
    let lazy_value v = lazy(loop v ~mark_points) in
    let print_collection lb rb vs =
      pretty_tag_str ~lb:lb ~rb:rb ~sep:"; " CutHint "" "" (List.map lazy_value vs)
    in
    match v with
    | VUnknown  -> ps "??"
    | VUnit     -> ps "()"
    | VBool b   -> ps @: string_of_bool b
    | VInt i    -> ps @: string_of_int i
    | VFloat f  -> ps @: string_of_float f
    | VByte c   -> ps @: string_of_int (Char.code c)
    | VString s -> ps @: Printf.sprintf "\"%s\"" @: String.escaped s
    | VTuple vs -> pretty_tag_str CutHint "" "" (List.map lazy_value vs)
    | VOption None -> ps "None"
    | VOption vopt -> pretty_tag_str CutHint "" "Some" [lazy_value (unwrap vopt)]
    | VSet vs  -> print_collection "{" "}" vs
    | VBag vs  -> print_collection "{|" "|}" vs
    | VList vs -> print_collection "[" "]" vs
    | VFunction (a, b) -> ps "<fun>"
    | VForeignFunction (a, _) -> ps "<foreignfun>"
    | VAddress (ip,port) -> ps (ip^":"^ string_of_int port)
    | VTarget id -> ps ("<"^id^">")
  in loop ~mark_points v

let string_of_value ?mark_points v = wrap_formatter (fun () -> print_value ?mark_points v)

(* Environment stringification *)
let print_binding (id,v) = ob(); ps (id^" = "); pc(); print_value v; cb(); fnl()

(* for a map structure *)
let print_binding_m id v = ob(); ps (id^" = "); pc(); print_value v; cb(); fnl()
 
let print_frame frame = List.iter print_binding frame
  
let print_env skip_functions (globals, (frames:frame_t list)) =
  let filter_m e = IdMap.filter 
    (fun _ -> function
      | VFunction _        -> false
      | VForeignFunction _ -> false
      | _                  -> true) 
    e in
  let filter_l l = List.filter 
    (function
      | _, VFunction _        -> false
      | _, VForeignFunction _ -> false
      | _                     -> true) 
    l in
  let len l = string_of_int (List.length l) in
  let len_m e = string_of_int @: map_length e in
  ps ("----Globals("^(len_m globals)^")----"); fnl();
  let global_m = IdMap.map (fun ref_v -> !ref_v) globals in
  let global_m' = if not skip_functions then global_m
                  else filter_m global_m in
  IdMap.iter print_binding_m global_m';
  fnl();
  ps ("----Frames("^(len frames)^")----"); fnl();
  let frames' = List.map filter_l frames in
  List.iter print_frame frames'

let print_trigger_env env =
  ps ("----Triggers("^(string_of_int @: map_length env)^")----"); fnl();
  IdMap.iter (fun id _ -> ps id; fnl()) env

let print_program_env (trigger_env, val_env) =
  print_trigger_env trigger_env;
  print_env false val_env

let old_trig_env, old_val_env = ref [], ref []

let string_of_env ?(skip_functions=true) (env:env_t) = 
  wrap_formatter (fun () -> print_env skip_functions env)

let string_of_program_env env = wrap_formatter (fun () -> print_program_env env)

(* conversion of things to values *)
let value_of_const = function
  | CUnknown -> VUnknown
  | CUnit -> VUnit
  | CBool b -> VBool b
  | CInt i -> VInt i
  | CFloat f -> VFloat f
  | CString s -> VString s
  | CAddress (ip,port) -> VAddress (ip,port)
  | CTarget id -> VTarget id

(* limited conversion between expr and values *)
let rec value_of_const_expr e = match tag_of_expr e with
    | Tuple   -> let es = decompose_tuple e in
      VTuple(list_map value_of_const_expr es)
    | Const c -> value_of_const c
    | Neg     ->
        begin match tag_of_expr @: decompose_neg e with
        | Const(CInt i)   -> VInt (-i)
        | Const(CFloat f) -> VFloat (-.f)
        | _ -> failwith @: "Negative can only have int or float" 
        end
    | t -> failwith @: "value is too complex: "^soi @: Obj.tag @: Obj.repr t


let rec type_of_value uuid value = 
  let typ_fst = function
    | []   -> t_unit (* make up because we just don't know *)
    | v::_ -> type_of_value uuid v
  in
  match value with
  | VUnknown -> canonical TUnknown
  | VUnit -> t_unit
  | VBool b -> t_bool
  | VInt _ -> t_int
  | VFloat _ -> t_float
  | VByte _ -> t_string
  | VString _ -> t_string
  | VAddress (_,_) -> t_addr
  | VTarget id -> canonical @: TTarget(TUnknown) (* We don't have the ids *)
  | VOption None -> wrap_tmaybe @: canonical TUnknown
  | VOption (Some v) -> wrap_tmaybe @: type_of_value uuid v
  | VTuple vs -> wrap_ttuple @: List.map (type_of_value uuid ) vs
  | VSet vs -> wrap_tset @: typ_fst vs
  | VList vs -> wrap_tlist @: typ_fst vs
  | VBag vs -> wrap_tbag @: typ_fst vs
  | VFunction _ | VForeignFunction _ -> raise (RuntimeError (uuid, 
      "type_of_value: cannot apply to function"))

let rec expr_of_value uuid value = match value with
  | VUnknown -> mk_const CUnknown
  | VUnit -> mk_const CUnit
  | VBool b -> mk_const @: CBool b
  | VInt i -> mk_const @: CInt i
  | VFloat f -> mk_const @: CFloat f
  | VByte b -> mk_const @: CString(string_of_int @: Char.code b)
  | VString s -> mk_const @: CString s
  | VAddress (ip,port) -> mk_const @: CAddress (ip,port)
  | VTarget id -> mk_const @: CTarget id
  | VOption(None) -> mk_nothing t_unknown
  | VOption(Some v) -> mk_just @: expr_of_value uuid v
  | VTuple vs -> mk_tuple @: List.map (expr_of_value uuid) vs
  | VSet vs | VList vs | VBag vs -> 
     let l = List.map (expr_of_value uuid) vs in
     k3_container_of_list (type_of_value uuid value) l
  | VFunction _ | VForeignFunction _ -> raise (RuntimeError (uuid, 
      "expr_of_value: cannot apply to function"))

(* Value comparison. Returns a partial list of inequalities if there are any *)
let find_inequality a b = 
  let count = ref 0 in
  let sort x = List.sort compare x in
  let rec loop a b =
    let collect l r = 
      try List.fold_left2 (fun acc lval rval ->
        let unequal = loop lval rval in
        acc@unequal
      ) [] l r
      with Invalid_argument _ -> [!count]
    in
    incr count;
    (* collect the inequalities from some inner lists *)
    match a,b with
    | (VSet l | VBag l), (VSet r | VBag r) -> collect (sort l) (sort r)
    | VList l, VList r -> collect l r
      (* consider float equality within a certain epsilon *)
    | VFloat x, VFloat y ->
        let e = 0.0001 in
        if abs_float(x -. y) > e then [!count] else []
    | VTuple l, VTuple r -> collect l r
    | a, b -> if a <> b then [!count] else []
  in
  sort @: loop a b

