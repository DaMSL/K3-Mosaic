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

(* how many msgs need a vid *)
let sw_need_vid_ctr = create_ds "sw_need_vid_cntr" (mut t_int) ~init:(mk_cint 0)

(* list tracking available vids taken from token *)
let sw_token_vid_list =
  let e = ["vid", t_vid; "num", t_int] in
  create_ds "sw_token_vid_list" (wrap_tlist' @@ snd_many e) ~e

(* highest seen vid *)
let sw_highest_vid = create_ds "sw_highest_vid" (mut t_vid)

(* trigger for when we receive the token *)
let sw_rcv_token_nm = "sw_rcv_token"
let sw_rcv_token_trig sw_check_done =
  mk_code_sink' sw_rcv_token_nm ["vid", t_vid] [] @@
  (* if we have stuff to number *)
  mk_if (mk_gt (mk_var sw_need_vid_ctr.id) @@ mk_cint 0)
    (* add to the vid we got the number of needed vids *)
    (mk_let ["next_vid"]
      (mk_vid_add (mk_var "vid") @@ mk_var sw_need_vid_ctr.id) @@
      mk_block [
        (* send on the token *)
        mk_send sw_rcv_token_nm (mk_var sw_next_switch_addr.id) [mk_var "next_vid"];
        (* reserve a block of vids *)
        mk_insert sw_token_vid_list.id @@ [mk_var "vid"; mk_var sw_need_vid_ctr.id];
        (* clear counter of msgs needing vid *)
        mk_assign sw_need_vid_ctr.id @@ mk_cint 0;
        (* update highest seen vid *)
        mk_assign sw_highest_vid.id @@ mk_var "next_vid";
        (* check for switch end *)
        sw_check_done;
        (* start the driver *)
        mk_send_me D.sw_driver_trig_nm;
      ]) @@
    (* if we have nothing to number, pass the token on as is *)
    mk_send sw_rcv_token_nm (mk_var sw_next_switch_addr.id) [mk_var "vid"]

(* code inlined into driver *)
let sw_gen_vid none some =
  let vid_num, vid_new, num_new = "vid_num", "vid", "num_new" in
  (* if we have a vid, pop it *)
  mk_case_ns (mk_peek @@ mk_var sw_token_vid_list.id) vid_num
    (* if we have no vid available, do the none *)
    none @@
    (* else *)
    mk_let [num_new] (mk_sub (mk_snd @@ mk_var vid_num) @@ mk_cint 1) @@
    mk_let [vid_new] (vid_increment ~vid_expr:(mk_fst @@ mk_var vid_num) ()) @@
    mk_block [
      (* check if we've hit <=0 *)
      mk_if (mk_leq (mk_var num_new) @@ mk_cint 0)
        (* if so, delete the entry *)
        (mk_delete sw_token_vid_list.id [mk_var vid_num]) @@
        (* else, decrement the num and increment the vid *)
        mk_update sw_token_vid_list.id [mk_var vid_num]
          [mk_var vid_new; mk_var num_new];
      (* continue with code *)
      some
    ]

(* only the master starts the protocol *)
let ms_init = mk_send sw_rcv_token_nm (mk_var sw_next_switch_addr.id)
  [mk_var D.g_start_vid.id]

(* --- End of code --- *)

let global_vars =
  [ decl_global sw_next_switch_addr;
    decl_global sw_need_vid_ctr;
    decl_global sw_token_vid_list;
    decl_global sw_highest_vid;
  ]

let functions = []

let triggers sw_check_done =
  [ sw_rcv_token_trig sw_check_done ]
