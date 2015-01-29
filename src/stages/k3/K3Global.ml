(* Globals to be included in all k3 programs *)

open Util
open K3.AST
open K3.Annotation
open K3Helpers

module U = K3Util

let out = open_out "debug"
              (* ip,     role,         alias *)
type peer_t = (address * id_t option * string option)

let me_name = "me"
let peers_name = "peers"
let peers_num_name = "peers_num"

(* me
 * --------------------------------------*)
let me_code = mk_global_val me_name t_addr
let me_var = mk_var me_name


(* peers_num : t_int
 * ---------------------------------------
 * Dealing with number of elements in peers
 *)
let peers_num_type = wrap_tset @: t_int

let peers_num_code peer_lst_len =
  mk_global_val_init peers_num_name peers_num_type @:
  mk_singleton peers_num_type @: mk_cint peer_lst_len

let peers_num_var = mk_var peers_num_name

(* update peers_num when peers add or remove. *)
let peers_num_update_code num_add =
  let old_num_var = mk_var "old_num" in
  mk_let "old_num" t_int (mk_peek peers_num_var) @:
  mk_block [
    mk_delete peers_num_name old_num_var;
    mk_insert peers_num_name @: mk_add (mk_cint num_add) old_num_var
  ]

let peers_num_incr_code = peers_num_update_code 1

let peers_num_dec_code = peers_num_update_code (-1)


(*
 * switches : [(idx:t_int, addr:t_addr)]
 * ---------------------------------------
 * Create a global set of addresses which conatins all the
 * address of switch node
 * *)
let switches_id_type_idx_name = "idx"
let switches_id_type_addr_name = "addr"
let switches_id_type_idx = t_int
let switches_id_type_addr = t_addr
let switches_id_args = [switches_id_type_idx_name, switches_id_type_idx;
                        switches_id_type_addr_name, switches_id_type_addr]
let switches_name = "switches" (* global var *)
let switches_var = mk_var switches_name
let switches_type_raw =
  [switches_id_type_idx; switches_id_type_addr]
let switches_type = wrap_tset @: wrap_ttuple switches_type_raw

let get_nodes_by_role peer_lst target_role =
  let switch_role_name = Some target_role in
  List.filter (fun (_,role,_) -> role = switch_role_name )
              peer_lst

let get_switches_lst peer_lst = get_nodes_by_role peer_lst "switch"

let switches_code switches_lst =
  mk_global_val_init switches_name switches_type @:
  snd(
    List.fold_left (fun (i,acc) (addr,_,_) ->
      (i+1,
       mk_combine
          (mk_singleton switches_type @:
            mk_tuple [mk_cint i; mk_caddress addr])
          acc)
      )
    (0,(mk_empty switches_type))
    switches_lst
  )


(*
(* switch_idx : t_int_mut
 * ------------------------
 * For switch node. The index of a switch node.
 * Alough this can be access through switches_lst, this
 * variable make it easier to access*)

let switch_idx_name = "switch_idx"
let switch_idx_var = mk_var switch_idx_name
let switch_idx_type = wrap_tset t_int_mut

let switch_idx_code =
  mk_global_val_init
    switch_idx_name
    switch_idx_type @:
    mk_singleton switch_idx_type @: mk_cint (-1)

let switch_idx_update_code new_idx = mk_update
    switch_idx_var
    (mk_peek switch_idx_var) @: mk_cint new_idx

*)

(*
 * peers
 * ---------------------------------------
 * peers is in a [(TAddress, Maybe String)] format, representing an address and
 * an alias (we don't care about role within k3 *)
let peers_role_name = "_role_"
let peers_id_type_addr_name = "addr"
let peers_id_type_name_name = "name"
let peers_id_type =
  [peers_id_type_addr_name, t_addr;
   peers_role_name, t_string;
   peers_id_type_name_name, t_string]
let peers_type = wrap_tset @: wrap_ttuple @: snd @: List.split peers_id_type
let peers_ids = fst @: List.split peers_id_type
let peers_empty = mk_global_val peers_name peers_type
let rec peers_code = function
  | [] -> failwith "peers_code: empty peer list!"
  | ps -> mk_global_val_init peers_name peers_type @:
    List.fold_right
      (fun (addr, mrole, mname) acc ->
        let name = match mname with
        | None   -> mk_cstring ""
        | Some s -> mk_cstring s
        in let role = match mrole with
        | None   -> mk_cstring ""
        | Some s -> mk_cstring s
        in
        mk_combine
          (mk_singleton peers_type @: mk_tuple [mk_caddress addr; role; name])
          acc
      )
      ps
      (mk_empty peers_type)

(* create k3 globals for the address and peers *)
let globals ps =
  me_code:: (* me *)
  (peers_num_code @: List.length ps)::                (* peers_num *)
  (peers_code ps)::                                    (* peers *)
  []

(* cross-reference foreign functions *)
let add_foreign_fn nm =
  try
    let (t,_,_) = K3StdLib.lookup nm in
    mk_foreign_short nm t
  with Not_found -> failwith @@ "foreign function "^nm^" not found in stdlib"

(* k3 stdlib *)
let stdlib =
  add_foreign_fn "divf"::
  add_foreign_fn "maxi"::
  add_foreign_fn "maxif"::
  add_foreign_fn "reciprocali"::
  add_foreign_fn "reciprocal"::
  add_foreign_fn "regex_match_int"::
  add_foreign_fn "substring"::
  add_foreign_fn "print"::
  add_foreign_fn "string_of_float"::
  add_foreign_fn "string_of_int"::
  add_foreign_fn "date_part"::
  add_foreign_fn "load_csv_bag"::
  []

let add_globals_k3 k3_globals ds = k3_globals@ds
let add_globals peers ds = add_globals_k3 (globals peers @ stdlib) ds
let remove_globals peers ds = list_drop (List.length @: globals peers) ds

