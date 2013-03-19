(* Standard library functions for K3, implemented as foreign functions *)

open Util
open K3Values
open K3Helpers
open K3.AST

let float_temp x = VTemp(VFloat(x))
let string_temp x = VTemp(VString(x))
let int_temp x = VTemp(VInt(x))
let unit_temp = VTemp(VUnit)

(* static hashtable for storing functions efficiently *)
type entry_t = type_t * arg_t * foreign_func_t
let (func_table : ((id_t, entry_t) Hashtbl.t)) = Hashtbl.create 10

(* retreive arguments from environment *)
let arg_of_env e = List.hd @: snd e

(* ------- Hashing Functions ------- *)
(* hash_float *)
let hash_float_fn e = 
  match arg_of_env e with 
   | [_,VFloat f] -> e, int_temp @: Hashtbl.hash f 
   | _ -> invalid_arg "hash_float"

let hash_float_name = "hash_float"
let hash_float_decl = wrap_tfunc t_float t_int
let hash_float_args = wrap_args ["f", t_float]
let _ = Hashtbl.add 
  func_table hash_float_name (hash_float_decl, hash_float_args, hash_float_fn)

(* hash_int *)
let hash_int_fn e = 
  match arg_of_env e with 
  | [_,VInt i] -> e, int_temp @: Hashtbl.hash i 
  | _ -> invalid_arg "hash_int"

let hash_int_name = "hash_int"
let hash_int_decl = wrap_tfunc t_int t_int
let hash_int_args = wrap_args ["i", t_int]
let _ = Hashtbl.add func_table 
  hash_int_name (hash_int_decl, hash_int_args, hash_int_fn)

(* hash_byte *)
let hash_byte_fn e = 
  match arg_of_env e with 
  | [_,VByte b] -> e, int_temp @: Hashtbl.hash b
  | _ -> invalid_arg "hash_byte"

let hash_byte_name = "hash_byte"
let hash_byte_decl = wrap_tfunc t_byte t_byte
let hash_byte_args = wrap_args ["b", t_byte]
let _ = Hashtbl.add func_table 
  hash_byte_name (hash_byte_decl, hash_byte_args, hash_byte_fn)

(* hash_string *)
let hash_string_fn e = 
  match arg_of_env e with 
  | [_,VString s] -> e, int_temp @: Hashtbl.hash s
  | _ -> invalid_arg "hash_string"

let hash_string_name = "hash_string"
let hash_string_decl = wrap_tfunc t_string t_int
let hash_string_args = wrap_args ["s", t_string]
let _ = Hashtbl.add func_table 
  hash_string_name (hash_string_decl, hash_string_args, hash_string_fn)

(* hash_addr *)
let hash_addr_fn e = 
  match arg_of_env e with 
  | [_,VAddress a] -> e, int_temp @: Hashtbl.hash a
  | _ -> invalid_arg "hash_addr"

let hash_addr_name = "hash_addr"
let hash_addr_decl = wrap_tfunc t_addr t_int
let hash_addr_args = wrap_args ["addr", t_addr]
let _ = Hashtbl.add func_table 
  hash_addr_name (hash_addr_decl, hash_addr_args, hash_addr_fn)


(* ------------ Math functions ------------- *)
(* float division *)
let divf_fn e = 
  match arg_of_env e with 
  | [_,VFloat x;_,VFloat y] -> e, float_temp @: x /. y
  | _ -> invalid_arg "divf"

let divf_name = "divf"
let divf_decl = wrap_tfunc (wrap_ttuple [t_float;t_float]) t_float
let divf_args = wrap_args ["x", t_float; "y", t_float]
let _ = Hashtbl.add func_table divf_name (divf_decl, divf_args, divf_fn)

(* int (truncated) division *)
let divi_fn e = 
  match arg_of_env e with 
  | [_,VInt x;_,VInt y] -> e, int_temp @: x / y
  | _ -> invalid_arg "divi"

let divi_name = "divi"
let divi_decl = wrap_tfunc t_int t_int
let divi_args = wrap_args ["x", t_int; "y", t_int]
let _ = Hashtbl.add func_table 
  divi_name (divi_decl, divi_args, divi_fn)

(* mod *)
let mod_fn e = 
  match arg_of_env e with 
  | [_,VInt x;_,VInt y] -> e, int_temp @: x mod y
  | _ -> invalid_arg "mod"

let mod_name = "mod"
let mod_decl = wrap_tfunc (wrap_ttuple [t_int;t_int]) t_int
let mod_args = wrap_args ["x", t_int; "y", t_int]
let _ = Hashtbl.add func_table 
  mod_name (mod_decl, mod_args, mod_fn)

(* maximum integer *)
let get_max_int_fn e = 
  match arg_of_env e with 
  | [_,VUnit] -> e, int_temp max_int
  | _ -> invalid_arg "get_max_int"

let get_max_int_name = "get_max_int"
let get_max_int_decl = wrap_tfunc t_unit t_int
let get_max_int_args = wrap_args ["_", t_unit]
let _ = Hashtbl.add func_table 
  get_max_int_name (get_max_int_decl, get_max_int_args, get_max_int_fn)


(* -------- casting -------- *)

(* float_of_int *)
let float_of_int_fn e = 
  match arg_of_env e with 
  | [_,VInt x] -> e, float_temp @: float_of_int x
  | _ -> invalid_arg "float_of_int_fn"

let float_of_int_name = "float_of_int"
let float_of_int_decl = wrap_tfunc t_int t_float
let float_of_int_args = wrap_args ["i", t_int]
let _ = Hashtbl.add func_table 
  float_of_int_name (float_of_int_decl, float_of_int_args, float_of_int_fn)

(* int_of_float *)
let int_of_float_fn e = 
  match arg_of_env e with 
  | [_,VFloat x] -> e, int_temp @: int_of_float x
  | _ -> invalid_arg "int_of_float_fn"

let int_of_float_name = "int_of_float"
let int_of_float_decl = wrap_tfunc t_float t_int
let int_of_float_args = wrap_args ["f", t_float]
let _ = Hashtbl.add func_table 
  int_of_float_name (int_of_float_decl, int_of_float_args, int_of_float_fn)

(* string_of_int *)
let string_of_int_fn e = 
  match arg_of_env e with 
  | [_,VInt x] -> e, string_temp @: string_of_int x
  | _ -> invalid_arg "string_of_int_fn"

let string_of_int_name = "string_of_int"
let string_of_int_decl = wrap_tfunc t_int t_string
let string_of_int_args = wrap_args ["i", t_int]
let _ = Hashtbl.add func_table 
  string_of_int_name (string_of_int_decl, string_of_int_args, string_of_int_fn)

(* string_of_float *)
let string_of_float_fn e = 
  match arg_of_env e with 
  | [_,VFloat x] -> e, string_temp @: string_of_float x
  | _ -> invalid_arg "string_of_float_fn"

let string_of_float_name = "string_of_float"
let string_of_float_decl = wrap_tfunc t_float t_string
let string_of_float_args = wrap_args ["f", t_float]
let _ = Hashtbl.add func_table 
  string_of_float_name (string_of_float_decl, string_of_float_args, string_of_float_fn)

(* -------- print functions -------- *)
let print_fn e =
  match arg_of_env e with
  | [_,VString s] -> print_string s; e, unit_temp
  | _ -> invalid_arg "print_fn"
let print_name = "print"
let print_decl = wrap_tfunc t_string t_unit
let print_args = wrap_args ["s", t_string]
let _ = Hashtbl.add func_table 
  print_name (print_decl, print_args, print_fn)

(* function-lookup functions *)
let lookup id = Hashtbl.find func_table id 
let lookup_value id = let (_,a,f) = lookup id in VForeignFunction (a,f)
let lookup_type id = let (t,_,_) = lookup id in t

