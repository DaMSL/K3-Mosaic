(* Globals to be included in all k3 programs *)

open Util
open K3.AST
open K3.Annotation
open K3Helpers

module U = K3Util

let out = open_out "debug"
              (* ip,     role *)
type peer_t = (address * id_t)

let me = {id="me"; e=[]; t=t_addr; init=None}
let me_var = mk_var me.id

let peers ps =
  let e = ["addr", t_addr] in
  let t = wrap_tset' @@ snd_many e in
  let init = some @@
    k3_container_of_list t @@
    List.map (mk_caddress |- fst) ps
  in
  {id="peers"; e; t; init}

let peers_num ps =
  let peers = peers ps in
  let init = some @@
    mk_agg
      (mk_assoc_lambda' ["acc", t_int] peers.e @@
        mk_add (mk_var "acc") @@ mk_cint 1)
      (mk_cint 0) @@
      mk_var peers.id
  in
  {id="peers_num"; e=[]; t=mut t_int; init}

(* specifies the job of a node: master/switch/node *)
let job = {id="job"; t=mut t_string; e=[]; init=None}

let jobs (ps:peer_t list) =
  let e = ["addr", t_addr; "job", t_string] in
  let t = wrap_tmap' @@ snd_many e in
  let init = some @@
    k3_container_of_list t @@
    List.map (fun (a,r) -> mk_tuple [mk_caddress a; mk_cstring r]) ps
  in
  {id="jobs"; e; t; init}


(* create k3 globals for the address and peers *)
let globals (ps:peer_t list) =
  decl_global me ::         (* me *)
  decl_global (peers ps) :: (* peers *)
  decl_global (jobs ps) ::  (* jobs - not removed *)
  []

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

