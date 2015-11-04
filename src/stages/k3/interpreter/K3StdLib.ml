(* Standard library functions for K3, implemented as foreign functions *)

open Util
open K3Values
open K3Values.Value
open K3Helpers
open K3.AST

(* load path for load_csv *)
let g_load_path = ref ""

let float_temp x  = VTemp(VFloat x)
let string_temp x = VTemp(VString x)
let int_temp x    = VTemp(VInt x)
let bool_temp x   = VTemp(VBool x)
let unit_temp     = VTemp VUnit

(* static hashtable for storing functions efficiently *)
type entry_t = type_t * arg_t * foreign_func_t
let func_table : ((id_t, entry_t) Hashtbl.t) = Hashtbl.create 10

(* add a function to the table. default is a unit dummy function *)
let add_fn ?(args=unit_arg) ?(ret=t_unit) ?(fn=fun env -> env, unit_temp) nm =
  let decl = wrap_tfunc (snd_many args) ret in
  Hashtbl.add func_table nm (decl, wrap_args args, fn)

(* retreive arguments from environment *)
let arg_of_env id env = hd @@ IdMap.find id env.locals
let args_of_env ids env = List.map (hd |- flip IdMap.find env.locals) ids

(* ------- Hashing Functions ------- *)
(* hash_float *)
let hash_float_fn e =
  match arg_of_env "f" e with
   | VFloat f -> e, int_temp @@ Hashtbl.hash f
   | _ -> invalid_arg "hash_float"

let hash_float_name = "hash_float"
let hash_float_decl = wrap_tfunc [t_float] t_int
let hash_float_args = wrap_args ["f", t_float]
let _ = Hashtbl.add
  func_table hash_float_name (hash_float_decl, hash_float_args, hash_float_fn)

(* hash_int *)
let hash_int_fn e =
  match arg_of_env "i" e with
  | VInt i -> e, int_temp @@ Hashtbl.hash i
  | _ -> invalid_arg "hash_int"

let hash_int_name = "hash_int"
let hash_int_decl = wrap_tfunc [t_int] t_int
let hash_int_args = wrap_args ["i", t_int]
let _ = Hashtbl.add func_table
  hash_int_name (hash_int_decl, hash_int_args, hash_int_fn)

(* hash_date -- implemented as int *)
let hash_date_fn e =
  match arg_of_env "d" e with
  | VInt i -> e, int_temp @@ Hashtbl.hash i
  | _ -> invalid_arg "hash_date"

let f_name = "hash_date"
let f_decl = wrap_tfunc [t_date] t_int
let f_args = wrap_args ["d", t_date]
let _ = Hashtbl.add func_table
  f_name (f_decl, f_args, hash_date_fn)

(* hash_byte *)
let hash_byte_fn e =
  match arg_of_env "b" e with
  | VByte b -> e, int_temp @@ Hashtbl.hash b
  | _ -> invalid_arg "hash_byte"

let hash_byte_name = "hash_byte"
let hash_byte_decl = wrap_tfunc [t_byte] t_byte
let hash_byte_args = wrap_args ["b", t_byte]
let _ = Hashtbl.add func_table
  hash_byte_name (hash_byte_decl, hash_byte_args, hash_byte_fn)

(* hash_string *)
let hash_string_fn e =
  match arg_of_env "s" e with
  | VString s -> e, int_temp @@ Hashtbl.hash s
  | _ -> invalid_arg "hash_string"

let hash_string_name = "hash_string"
let hash_string_decl = wrap_tfunc [t_string] t_int
let hash_string_args = wrap_args ["s", t_string]
let _ = Hashtbl.add func_table
  hash_string_name (hash_string_decl, hash_string_args, hash_string_fn)

(* hash_addr *)
let hash_addr_fn e =
  match arg_of_env "addr" e with
  | VAddress a -> e, int_temp @@ Hashtbl.hash a
  | _ -> invalid_arg "hash_addr"

let hash_addr_name = "hash_addr"
let hash_addr_decl = wrap_tfunc [t_addr] t_int
let hash_addr_args = wrap_args ["addr", t_addr]
let _ = Hashtbl.add func_table
  hash_addr_name (hash_addr_decl, hash_addr_args, hash_addr_fn)


(* ------------ Math functions ------------- *)
(* float division *)
let divf_fn e =
  match arg_of_env "x" e, arg_of_env "y" e with
  | VFloat x, VFloat y -> e, float_temp @@ x /. y
  | VInt x, VFloat y -> e, float_temp @@ (foi x) /. y
  | _ -> invalid_arg "divf"

let divf_name = "divf"
let divf_decl = wrap_tfunc [t_float; t_float] t_float
let divf_args = wrap_args ["x", t_float; "y", t_float]
let _ = Hashtbl.add func_table divf_name (divf_decl, divf_args, divf_fn)

(* int (truncated) division *)
let divi_fn e =
  match arg_of_env "x" e, arg_of_env "y" e with
  | VInt x, VInt y -> e, int_temp @@ x / y
  | _ -> invalid_arg "divi"

let divi_name = "divi"
let divi_decl = wrap_tfunc [t_int; t_int] t_int
let divi_args = wrap_args ["x", t_int; "y", t_int]
let _ = Hashtbl.add func_table
  divi_name (divi_decl, divi_args, divi_fn)

(* mod *)
let fn e =
  match arg_of_env "x" e, arg_of_env "y" e with
  | VInt x, VInt y -> e, int_temp @@ x mod y
  | _ -> invalid_arg "mod"
let name = "mod"
let decl = wrap_tfunc [t_int; t_int] t_int
let args = ["x", t_int; "y", t_int]
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

let name = "abs"
let args = ["x", t_int]
let ret  = t_int
let fn e = match args_of_env (fst_many args) e with
  | [VInt x] -> e, int_temp @@ abs x
  | _        -> invalid_arg name
let decl = wrap_tfunc (snd_many args) ret
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

(* reciprocal *)
let name = "reciprocali"
let args = ["x", t_int]
let fn e =
  match arg_of_env "x" e with
  | VInt x -> e, float_temp @@ 1. /. foi x
  | _      -> invalid_arg name
let decl = wrap_tfunc (snd_many args) t_float
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

(* reciprocal *)
let name = "reciprocal"
let args = ["x", t_float]
let fn e =
  match arg_of_env "x" e with
  | VFloat x -> e, float_temp @@ 1. /. x
  | _        -> invalid_arg name
let decl = wrap_tfunc (snd_many args) t_float
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

(* max int *)
let name = "maxi"
let fn e =
  match arg_of_env "x" e, arg_of_env "y" e with
  | VInt x, VInt y -> e, int_temp @@ max x y
  | _ -> invalid_arg name
let args = ["x", t_int; "y", t_int]
let decl = wrap_tfunc (snd_many args) t_int
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

(* max int float *)
let name = "maxif"
let fn e =
  match arg_of_env "x" e, arg_of_env "y" e with
  | VInt x, VFloat y -> e, float_temp @@ max (foi x) y
  | _ -> invalid_arg name
let args = ["x", t_int; "y", t_float]
let decl = wrap_tfunc (snd_many args) t_int
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

(* maximum integer *)
(* this is used for the maximum hash value
 * we'll fudge this and make it what the real max hash value is
 * ocaml limits the max hash value to be consistent between the 64 and 32 bit
 * runtimes *)
let get_max_int_fn e = e, int_temp 0x40000000

let get_max_int_name = "get_max_int"
let get_max_int_decl = wrap_tfunc [t_unit] t_int
let get_max_int_args = wrap_args ["_", t_unit]
let _ = Hashtbl.add func_table
  get_max_int_name (get_max_int_decl, get_max_int_args, get_max_int_fn)


(* -------- casting -------- *)

(* float_of_int *)
let float_of_int_fn e =
  match arg_of_env "i" e with
  | VInt x -> e, float_temp @@ float_of_int x
  | _ -> invalid_arg "float_of_int_fn"

let float_of_int_name = "float_of_int"
let float_of_int_decl = wrap_tfunc [t_int] t_float
let float_of_int_args = wrap_args ["i", t_int]
let _ = Hashtbl.add func_table
  float_of_int_name (float_of_int_decl, float_of_int_args, float_of_int_fn)

(* int_of_float *)
let int_of_float_fn e =
  match arg_of_env "f" e with
  | VFloat x -> e, int_temp @@ int_of_float x
  | _ -> invalid_arg "int_of_float_fn"

let int_of_float_name = "int_of_float"
let int_of_float_decl = wrap_tfunc [t_float] t_int
let int_of_float_args = wrap_args ["f", t_float]
let _ = Hashtbl.add func_table
  int_of_float_name (int_of_float_decl, int_of_float_args, int_of_float_fn)

(* string_of_int *)
let string_of_int_fn e =
  match arg_of_env "i" e with
  | VInt x -> e, string_temp @@ string_of_int x
  | _ -> invalid_arg "string_of_int_fn"

let string_of_int_name = "string_of_int"
let string_of_int_decl = wrap_tfunc [t_int] t_string
let string_of_int_args = wrap_args ["i", t_int]
let _ = Hashtbl.add func_table
  string_of_int_name (string_of_int_decl, string_of_int_args, string_of_int_fn)

(* string_of_float *)
let string_of_float_fn e =
  match arg_of_env "f" e with
  | VFloat x -> e, string_temp @@ string_of_float x
  | _ -> invalid_arg "string_of_float_fn"

let string_of_float_name = "string_of_float"
let string_of_float_decl = wrap_tfunc [t_float] t_string
let string_of_float_args = wrap_args ["f", t_float]
let _ = Hashtbl.add func_table
  string_of_float_name (string_of_float_decl, string_of_float_args, string_of_float_fn)

(* substring *)
let name = "substring"
let args = ["s", t_string; "f", t_int; "t", t_int]
let fn e =
  let aoe = List.map (fun x -> arg_of_env (fst x) e) args in
  match aoe with
  | [VString s; VInt f; VInt t] -> e, string_temp @@ String.sub s f t
  | _ -> invalid_arg name
let decl = wrap_tfunc (snd_many args) t_string
let () = Hashtbl.add func_table name (decl, wrap_args args, fn)

let () =
  let name = "concat" in
  let args = ["x", t_string; "y", t_string] in
  let fn env = match args_of_env (fst_many args) env with
    | [VString x; VString y] -> env, string_temp @@ x ^ y
    | _ -> invalid_arg name in
  add_fn ~args ~ret:t_string ~fn name

(* -------- print functions -------- *)
let () =
  let name = "print" in
  let args = ["s", t_string] in
  let fn env = match args_of_env (fst_many args) env with
    | [VString s] -> print_endline s;
                     env, unit_temp
    | _ -> invalid_arg name in
  add_fn ~args ~fn name

let () =
  let name = "log" in
  let args = ["s", t_string] in
  let fn env = match args_of_env (fst_many args) env with
    | [VString s] -> Log.log (lazy (s^"\n")) `Debug;
                     env, unit_temp
    | _ -> invalid_arg name in
  add_fn ~args ~fn name

(* parse SQL date format *)
let name = "parse_sql_date"
let fn e =
  match arg_of_env "s" e with
  | VString s -> e, int_temp @@ int_of_sql_date s
  | _ -> invalid_arg "parse_sql_date"
let decl = wrap_tfunc [t_string] t_int
let args = wrap_args ["s", t_string]
let _ = Hashtbl.add func_table name (decl, args, fn)

(* return a part of the date as int *)
let name = "date_part"
let args = ["part", t_string; "d", t_date]
let fn e =
  let aoe = List.map (fun x -> arg_of_env (fst x) e) args in
  match aoe with
  | [VString p; VInt d] -> e, int_temp @@ date_part p d
  | _ -> invalid_arg name
let decl = wrap_tfunc (snd_many args) t_int
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

(* global table for regexes *)
let regexes = Hashtbl.create 10

(* regex_match *)
let name = "regex_match_int"
let args = ["r", t_string; "s", t_string]
let fn e =
  match arg_of_env "r" e, arg_of_env "s" e with
  | VString rs, VString s -> e,
    let r = begin try
        Hashtbl.find regexes rs
      with Not_found ->
        let r' = Str.regexp rs in
        Hashtbl.add regexes rs r'; r'
      end
    in
    int_temp @@ if Str.string_match r s 0 then 1 else 0
  | _      -> invalid_arg name
let decl = wrap_tfunc (snd_many args) t_int
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

let r_pipe = Str.regexp "|"
let d         = "[0-9]"
let r_date    = Str.regexp (Printf.sprintf "^%s%s%s%s-%s%s-%s%s$" d d d d d d d d)
let r_float   = Str.regexp (Printf.sprintf {|^%s*\(\.%s+\)?$|} d d)
let r_int     = Str.regexp @@ "^[0-9]+$"
let r_bool    = Str.regexp @@ "^true$|^false$"

(* convert the data in a line *)
let read_data line =
  List.map (fun s ->
    if r_match r_date s then VInt(int_of_sql_date s)
    else if r_match r_bool s then VBool(bos s)
    else if r_match r_int s then VInt(ios s)
    else if r_match r_float s then VFloat(fos s)
    else VString s) line

let load_csv_fn err_fn args e =
  let aoe = List.map (fun x -> arg_of_env (fst x) e) args in
  match aoe with
  | (VString f)::_ ->
    let chan = open_in (!g_load_path^Filename.dir_sep^f) in
    let rec loop v =
      try
        let next_rec = Str.split r_pipe (input_line chan) in
        let l = read_data next_rec in
        loop (v_insert err_fn (VTuple l) v)
      with End_of_file -> v
    in
    let cont = loop (v_col_of_t TSet) in
    close_in chan;
    e, VTemp(cont)

  | _ -> invalid_arg name

(* csv loading function *)
let csv_loader_name = "load_csv_col"
let args = ["file", t_string]
let ret  = wrap_tbag t_top
let err_fn s s' = failwith @@ "load_csv: "^s^" "^s'
let fn e = load_csv_fn err_fn args e
let decl = wrap_tfunc (snd_many args) ret
let _ = Hashtbl.add func_table csv_loader_name (decl, wrap_args args, fn)

(* csv loading function, with dummy witness type var *)
let name = "load_csv_col2"
let args = ["file", t_string; "emptyCol", wrap_tbag t_top]
let ret  = wrap_tbag t_top
let err_fn s s' = failwith @@ name^": "^s^" "^s'
let fn e = load_csv_fn err_fn args e
let decl = wrap_tfunc (snd_many args) ret
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

(* get current time *)
let name = "now_int"
let args = ["_", t_unit]
let ret  = t_int
let fn e = e, VTemp(VInt(iof @@ Sys.time () *. 1000.))
let decl = wrap_tfunc (snd_many args) ret
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

(* sleep in ms *)
let name = "sleep"
let args = ["ms", t_int]
let ret  = t_unit
let fn e = e, VTemp VUnit
let decl = wrap_tfunc (snd_many args) ret
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

(* builtin function for k3new routing *)
let () =
  add_fn "free_buckets_builtin"
  ~args:["dim_bounds", K3Route.dim_bounds.t; "free_dims", K3Route.free_dims.t;
         "get_ring_node", wrap_tfunc [t_int; t_int] t_addr;
         "bound_bucket", t_int; "max_val", t_int]
  ~ret:(wrap_tset t_addr)
  ~fn:(fun _ -> failwith "not implemented")

(* print env *)
let () = add_fn "print_env"
  ~fn:(fun e ->
    Log.log (lazy (string_of_env ~accessed_only:false ~skip_empty:false e)) `Debug;
    e, VTemp VUnit)

(* dummy functions for *)
let () = add_fn "jemallocStart"
let () = add_fn "jemallocStop"
let () = add_fn "pcmStart"
let () = add_fn "pcmStop"
let () = add_fn "tcmallocStart"
let () = add_fn "tcmallocStop"

(* shutdown - dummy here. implemented in Runtime *)
let () = add_fn "haltEngine"

(* error function ---------- *)
let name = "error"
let fn env = failwith @@ "Error function called: "
let args = unit_arg
let ret = t_unknown
let decl = wrap_tfunc (snd_many args) ret
let _ = Hashtbl.add func_table name (decl, wrap_args args, fn)

(* function-lookup functions *)
let lookup id = Hashtbl.find func_table id
let lookup_value id = let (_,a,f) = lookup id in VForeignFunction (id,a,f)
let lookup_type id = let (t,_,_) = lookup id in t

(* avoid circular inclusion *)
let () = K3Typechecker.lookup_type := lookup_type

let funcs () = Hashtbl.fold (fun k v acc -> (k,v)::acc) func_table []

(* cross-reference foreign functions *)
let add_foreign_fns () =
  let l = funcs () in
  List.map (fun (nm, v) -> mk_foreign_short nm @@ fst3 v) l

let add_globals ds = K3Global.globals @ add_foreign_fns () @ ds
let remove_globals ds = list_drop (List.length K3Global.globals) ds

