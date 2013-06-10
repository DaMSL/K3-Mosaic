(* Globals to be included in all k3 programs *)

open Util
open K3.AST
open K3.Annotation
open K3Helpers

module U = K3Util

let me_name = "me"
let peers_name = "peers"

let me_code addr = mk_global_val_init me_name t_addr @: mk_const @: CAddress addr
let me_var = mk_var me_name

(* peers is in a [(TAddress, Maybe String)] format *)
let peers_id_type = ["addr", t_addr; "name", wrap_tmaybe t_string]
let peers_type = wrap_tset @: wrap_ttuple @: snd @: List.split peers_id_type
let peers_ids = fst @: List.split peers_id_type
let peers_empty = mk_global_val peers_name peers_type 
let rec peers_code addr = function 
  (* if we have no peers, just add ourselves in *)
  | [] -> peers_code addr [addr, None]
  | ps -> mk_global_val_init peers_name peers_type @: 
    List.fold_right
      (fun (addr, mname) acc -> let n = match mname with
        | None   -> mk_nothing_m t_string 
        | Some s -> mk_just @: mk_const @: CString s
        in mk_combine 
          (mk_singleton peers_type @: mk_tuple [mk_const @: CAddress addr; n]) 
          acc
      )
      ps
      (mk_empty peers_type)

(* create k3 globals for the address and peers *)
let globals addr ps = me_code addr::[peers_code addr ps]

let add_globals addr peers ds = globals addr peers@ds
let remove_globals addr peers ds = list_drop (List.length @: globals addr peers) ds

