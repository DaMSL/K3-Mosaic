open Util
open Printing

open K3.AST
open K3Util
open K3Printing
open K3Helpers

exception RuntimeError of int * string

(* Interpreter representation of values *)

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

and frame_t = (id_t * value_t) list

(* mutable environment, frame environment *)
and env_t = (id_t * value_t ref) list * (frame_t list)

(* trigger env is where we store the trigger functions. These functions take the
 * scheduler_state (parametrized here to prevent circular inclusion), the
 * environment, value_t of arguments, and produce unit *)
type trigger_env_t = (id_t * (env_t -> value_t -> unit)) list
type program_env_t = trigger_env_t * env_t

(* Value stringification *)
let unwrap opt = match opt with Some v -> v | _ -> failwith "invalid option unwrap"

(* Value comparison *)
let rec equal_values a b = 
  let sort x = List.sort compare x in
  match a,b with
  | (VSet l | VBag l), (VSet r | VBag r) ->
      (try List.for_all2 equal_values (sort l) (sort r)
       with Invalid_argument _ -> false)
  | a,b -> a = b

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


let rec print_value v =
  let lazy_value v = lazy(print_value v) in
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
  | VString s -> ps @: String.escaped s
  | VTuple vs -> pretty_tag_str CutHint "" "" (List.map lazy_value vs)

  | VOption vopt ->
    if vopt = None then ps "None"
    else pretty_tag_str CutHint "" "Some" [lazy_value (unwrap vopt)]

  | VSet vs  -> print_collection "{" "}" vs
  | VBag vs  -> print_collection "{|" "|}" vs
  | VList vs -> print_collection "[" "]" vs
  | VFunction (a, b) -> ps "<fun>"
  | VForeignFunction (a, _) -> ps "<foreignfun>"
  | VAddress (ip,port) -> ps (ip^":"^ string_of_int port)
  | VTarget id -> ps ("<"^id^">")

let string_of_value v = wrap_formatter (fun () -> print_value v)

(* Environment stringification *)
let print_binding (id,v) = ob(); ps (id^" = "); pc(); print_value v; cb(); fnl()
 
let print_frame frame = List.iter print_binding frame
  
let print_env skip_functions (globals, frames) =
  let filter_env l = List.filter 
    (function 
      | _, VFunction _        -> false
      | _, VForeignFunction _ -> false
      | _                     -> true) 
    l in
  let len l = string_of_int (List.length l) in
    ps ("----Globals("^(len globals)^")----"); fnl();
    let global_l = List.map (fun (id, ref_v) -> id, !ref_v) globals in
    let global_l' = if not skip_functions then global_l
                    else filter_env global_l in
    List.iter print_binding global_l';
    fnl();
    ps ("----Frames("^(len frames)^")----"); fnl();
    let frames' = List.map filter_env frames in
    List.iter print_frame frames'

let print_trigger_env env =
  ps ("----Triggers("^(string_of_int @: List.length env)^")----"); fnl();
  List.iter (fun (id,_) -> ps id; fnl()) env

let print_program_env (trigger_env,val_env) =
  print_trigger_env trigger_env;
  print_env false val_env

let old_trig_env, old_val_env = ref [], ref []

let string_of_env (env:env_t) = wrap_formatter (fun () -> print_env true env)

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
    | Tuple -> let es = decompose_tuple e in
      VTuple(list_map value_of_const_expr es)
    | Const c -> value_of_const c
    | _ -> failwith "value is too complex"


let rec type_of_value uuid value = 
  let typ_fst vs = type_of_value uuid @: List.hd vs in
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
      "expr_of_value: cannot apply to
  function"))

