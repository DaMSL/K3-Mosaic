(* Globals to be included in all k3 programs *)

open Util
open K3.AST
open K3.Annotation
open K3Helpers

module U = K3Util

let out = open_out "debug"
              (* ip,     role *)
type peer_t = (address * id_t)

let me = create_ds "me" t_addr
let me_var = mk_var me.id

let role = create_ds "role" @@ wrap_tset t_string

let peers =
  let e = ["addr", t_addr] in
  let t = wrap_tset' @@ snd_many e in
  create_ds "peers" t ~e


(* create k3 globals for the address and peers *)
let globals = List.map decl_global
  [ me;          (* me *)
    peers;       (* peers *)
    role;
  ]

