(* Globals to be included in all k3 programs *)

open Util
open K3.AST
open K3.Annotation
open K3Helpers

module U = K3Util

let me_name = "me"

let me = mk_global_val me_name t_addr

let add_globals ds = me::ds

