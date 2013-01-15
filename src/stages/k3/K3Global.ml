(* Globals to be included in all k3 programs *)

open Util
open K3.AST
open K3.Annotation
open K3Helpers

module U = K3Util

let me_name = "me"

let me_code = mk_global_val me_name t_addr
let me_var = mk_var me_name

let globals = [me_code]

let add_globals ds = globals@ds
let remove_globals ds = list_drop (List.length globals) ds

