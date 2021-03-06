(* Timestamp generation between switches using token ring style protocol *)
open K3.AST
open K3Helpers
open Util
open K3Dist

module D = K3Dist
module G = K3Global
module Std = K3StdLib
module P = ProgInfo

(* We establish a total ordering of switches
 * Every switch buffers its insertion/deletion triggers
 * When received, the token is incremented by the number of buffered messages and sent on
 *)

(* the address of the next switch in the chain *)
let sw_next_switch_addr =
  let d_init =
    mk_let ["addr_list"]
      (* get a total ordering: sort ascending by address *)
      (mk_sort
        (mk_lambda'' ["addr1", t_int; "addr2", t_int] @@
          mk_lt (mk_var "addr1") @@ mk_var "addr2") @@
        (* convert to list *)
        mk_convert_col' D.switches.t TList @@ mk_var D.switches.id) @@
    (* bind the first entry of the list (default option) *)
    mk_case_ns (mk_peek @@ mk_var "addr_list") "first_addr"
      (mk_error "no addresses in addr_list") @@
      (* fold and find the entry after the one that matches ours *)
      mk_apply' "addr_of_int" @@ singleton @@
      mk_snd @@ mk_agg
        (mk_assoc_lambda' ["take", t_bool; "result", t_int] ["x", t_int] @@
          mk_if (mk_var "take")
            (mk_tuple [mk_cfalse; mk_var "x"]) @@
             mk_if (mk_eq (mk_var "x") @@ mk_var D.me_int.id)
              (mk_tuple [mk_ctrue; mk_var "result"]) @@
               mk_tuple [mk_var "take"; mk_var "result"])
        (mk_tuple [mk_cfalse; mk_var "first_addr"]) @@
        mk_var "addr_list"
  in
  create_ds "sw_next_switch_addr" (mut t_addr) ~d_init

(* the address of the last switch, where init begins *)
let ms_last_switch_addr =
  let d_init =
    mk_let ["addr_list"]
      (* get a total ordering: sort descending by address *)
      (mk_sort
        (mk_lambda'' ["addr1", t_int; "addr2", t_int] @@
          mk_geq (mk_var "addr1") @@ mk_var "addr2") @@
        (* convert to list *)
        mk_convert_col' D.switches.t TList @@ mk_var D.switches.id) @@
    mk_case_ns (mk_peek @@ mk_var "addr_list") "x"
      (mk_error "missing switches") @@
      mk_apply' "addr_of_int" [mk_var "x"]
  in
  create_ds "ms_last_switch_addr" (mut t_addr) ~d_init

(* highest seen vid *)
let sw_highest_vid = create_ds "sw_highest_vid" (mut t_vid)

(* vector clock representing consecutive numbering per node *)
let sw_vector_clock = create_ds "sw_vector_clock" @@ wrap_tvector t_int

(* start the token ring on the last switch in the ring.
   Since we have 2 rings - one for the vid and one for the vector clock -
   we want to start the rings going in a way that doesn't allow for races.
   This means that the actual ring participants need to send the 2
   start messages, rather than an external entity *)
let sw_init_token_rings_nm = "sw_init_token_rings"
let sw_init_token_rings =
  mk_code_sink' sw_init_token_rings_nm [] [] @@
  mk_block [
    mk_send D.sw_rcv_token_trig_nm   (mk_var sw_next_switch_addr.id) [mk_var D.g_start_vid.id];
    mk_send D.sw_rcv_vector_clock_nm (mk_var sw_next_switch_addr.id)
      [mk_map (mk_lambda' unknown_arg @@ mk_cint 0) @@ mk_var D.my_peers.id]
  ]

(* only the master starts the protocol *)
let ms_init =
  mk_send sw_init_token_rings_nm (mk_var ms_last_switch_addr.id) []

(* --- End of code --- *)

let global_vars =
  List.map decl_global
  [ sw_next_switch_addr;
    ms_last_switch_addr;
    sw_highest_vid;
  ]

let functions = []
let triggers = [sw_init_token_rings]
