open Util
open Printing

open K3.AST
open K3Util
open K3Printing
open K3Typechecker

exception RuntimeError of int

(* Interpreter representation of values *)

type value_t
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
    | VAddress of address
    | VTarget of id_t

type frame_t = (id_t * value_t) list
type env_t = (id_t * value_t ref) list * (frame_t list)
type trigger_env_t = (id_t * (env_t -> value_t -> unit)) list
type program_env_t = trigger_env_t * env_t

(* consumeable id -> trigger id *)
type source_bindings_t = (id_t * id_t) list

(* Value stringification *)
let unwrap opt = match opt with Some v -> v | _ -> failwith "invalid option unwrap"

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
  | VAddress (ip,port) -> ps (ip^":"^ string_of_int port)
  | VTarget id -> ps ("<"^id^">")

let string_of_value v = wrap_formatter (fun () -> print_value v)

(* Environment stringification *)
let print_binding (id,v) = pc(); ps "id="; ps id; pc(); print_value v
 
let print_frame frame = List.iter print_binding frame
  
let print_env (globals, frames) =
  let len l = string_of_int (List.length l) in
    ps ("----Globals("^(len globals)^")----"); fnl();
    List.iter print_binding (List.map (fun (id,ref_v) -> id,!ref_v) globals);
    fnl();
    ps ("----Frames("^(len frames)^")----"); fnl();
    List.iter print_frame frames

let print_trigger_env env =
  List.iter (fun (id,_) -> ps id; fnl()) env

let print_program_env (trigger_env,val_env) =
  print_trigger_env trigger_env;
  print_env val_env
  
let string_of_env env = wrap_formatter (fun () -> print_env env)

let string_of_program_env env = wrap_formatter (fun () -> print_program_env env)

let string_of_source_bindings bindings = wrap_formatter (fun () ->
  List.iter (fun (src,trig) -> ps (src^" -> "^trig); fnl()) bindings)
