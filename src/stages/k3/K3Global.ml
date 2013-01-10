(* Globals to be included in all k3 programs *)

open Util
open K3.AST
open K3.Annotation
open K3Helpers

module U = K3Util

let me_name = "me"

let me_code = mk_global_val me_name t_addr
let me_var = mk_var me_name

let add_globals ds = me_code::ds

