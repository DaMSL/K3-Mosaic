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

let peers ps =
  let e = ["addr", t_addr] in
  let t = wrap_tset' @@ snd_many e in
  let init =
    k3_container_of_list t @@
    List.map (mk_caddress |- fst) ps
  in
  create_ds "peers" t ~e ~init

let peers_num ps =
  let peers = peers ps in
  let init = mk_agg
    (mk_assoc_lambda' ["acc", t_int] peers.e @@
      mk_add (mk_var "acc") @@ mk_cint 1)
    (mk_cint 0) @@
    mk_var peers.id
  in
  create_ds "peers_num" (mut t_int) ~init

(* specifies the job of a node: master/switch/node *)
let job_none_v   = -1
let job_master_v = 0
let job_switch_v = 1
let job_node_v   = 2
let job_timer_v  = 3
let job = create_ds "job" (mut t_int) ~init:(mk_cint 0)
let job_master = create_ds "job_master" t_int ~init:(mk_cint job_master_v)
let job_switch = create_ds "job_switch" t_int ~init:(mk_cint job_switch_v)
let job_node   = create_ds "job_node" t_int ~init:(mk_cint job_node_v)
let job_timer  = create_ds "job_timer" t_int ~init:(mk_cint job_timer_v)

let job_of_str = function
  | "master" -> job_master_v
  | "switch" -> job_switch_v
  | "node"   -> job_node_v
  | "timer"  -> job_timer_v
  | _        -> job_none_v

let jobs (ps:peer_t list) =
  let e = ["addr", t_addr; "job", t_int] in
  let t = wrap_tmap' @@ snd_many e in
  let init = k3_container_of_list t @@
    List.map (fun (a,r) -> mk_tuple [mk_caddress a; mk_cint @@ job_of_str r]) ps
  in
  create_ds "jobs" t ~e ~init

(* create k3 globals for the address and peers *)
let globals (ps:peer_t list) = List.map decl_global
  [ me;          (* me *)
    job_master;
    job_switch;
    job_node;
    job_timer;
    peers ps; (* peers *)
    jobs ps;
  ] (* jobs - not removed *)

(* cross-reference foreign functions *)
let add_foreign_fn nm =
  try
    let t,_,_ = K3StdLib.lookup nm in
    mk_foreign_short nm t
  with Not_found -> failwith @@ "foreign function "^nm^" not found in stdlib"

(* k3 stdlib *)
let stdlib = [
  add_foreign_fn "divf";
  add_foreign_fn "maxi";
  add_foreign_fn "maxif";
  add_foreign_fn "reciprocali";
  add_foreign_fn "reciprocal";
  add_foreign_fn "regex_match_int";
  add_foreign_fn "substring";
  add_foreign_fn "print";
  add_foreign_fn "string_of_float";
  add_foreign_fn "date_part";
  add_foreign_fn "load_csv_bag";
  add_foreign_fn "now_int";
]

let add_globals_k3 k3_globals ds = k3_globals@ds
let add_globals peers ds = add_globals_k3 (globals peers @ stdlib) ds
(* leave jobs in there *)
let remove_globals peers ds = list_drop (List.length (globals peers) - 1) ds

