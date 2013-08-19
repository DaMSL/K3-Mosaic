(* Globals to be included in all k3 programs *)

open Util
open K3.AST
open K3.Annotation
open K3Helpers

module U = K3Util

              (* ip,     role,         alias *)
type peer_t = (address * id_t option * string option)

let me_name = "me"
let peers_name = "peers"

let me_code = mk_global_val me_name t_addr
let me_var = mk_var me_name

(* peers is in a [(TAddress, Maybe String)] format, representing an address and
 * an alias (we don't care about role within k3 *)
let peers_role_name = "_role_"
let peers_id_type = ["addr", t_addr; peers_role_name, t_string; "name", t_string]
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
let globals ps = me_code::[peers_code ps]

let add_globals_k3 k3_globals ds = k3_globals@ds
let add_globals peers ds = add_globals_k3 (globals peers) ds
let remove_globals peers ds = list_drop (List.length @: globals peers) ds

