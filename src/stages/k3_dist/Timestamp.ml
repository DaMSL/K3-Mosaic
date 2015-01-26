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
  let jobs = G.jobs [] in
  let init = some @@
    mk_let "addr_list" (wrap_tlist t_addr)
      (* get a total ordering: sort ascending by address *)
      (mk_sort
        (mk_lambda' ["addr1", t_vid; "addr2", t_vid] @@ mk_lt (mk_var "addr1") @@ mk_var "addr2") @@
        (* project out addr only *)
        mk_map (mk_lambda' jobs.e @@ mk_var "addr") @@
          (* get all the switches *)
          mk_filter
            (mk_lambda' jobs.e @@ mk_eq (mk_var "job") @@ mk_cstring "switch") @@
            mk_var jobs.id) @@
    (* bind the first entry of the list (default option) *)
    mk_case_ns (mk_peek @@ mk_var "addr_list") "first_addr"
      (mk_apply (mk_var "error") mk_cunit)
      (* fold and find the entry after the one that matches ours *)
      (mk_agg
        (mk_assoc_lambda' ["take", t_bool; "result", t_addr] ["x", t_addr] @@
          mk_if (mk_var "take")
            (mk_tuple [mk_cfalse; mk_var "x"]) @@
             mk_if (mk_eq (mk_var "x") G.me_var)
              (mk_tuple [mk_ctrue; mk_var "result"]) @@
               mk_tuple [mk_var "take"; mk_var "result"])
        (mk_tuple [mk_cfalse; mk_var "first_addr"]) @@
        mk_var "addr_list")
  in
  {id="sw_next_switch_addr"; t=mut t_addr; e=[]; init}

(* how many msgs need a vid *)
let sw_need_vid_ctr =
  {id="sw_need_vid_counter"; t=mut t_int; e=[]; init=some @@ mk_cint 0}

(* parameter: up to how many messages to process at once before letting queue continue (latency) *)
let sw_chunk_msgs_to_process =
  {id="sw_chunk_msgs_to_process"; t=mut t_int; e=[]; init=some @@ mk_cint 10}

(* list tracking available vids taken from token *)
let sw_token_vid_list =
  let e = ["vid", t_vid; "num", t_int] in
  {id="sw_token_vid_list"; e; t=wrap_tlist' @@ snd_many e; init=None}

(* highest seen vid *)
let sw_highest_vid =
  {id="sw_highest_vid"; e=[]; t=t_vid; init=None}

(* trigger for when we receive the token *)
let sw_rcv_token_nm = "sw_rcv_token"
let sw_rcv_token_trig =
  mk_code_sink' sw_rcv_token_nm ["vid", t_vid] [] @@
  (* if we have stuff to number *)
  mk_if (mk_gt (mk_var sw_need_vid_ctr.id) @@ mk_cint 0)
    (* add to the vid we got the number of needed vids *)
    (mk_let "next_vid" t_vid
      (mk_tuple [mk_subscript 1 @@ mk_var "vid";
                  mk_sub (mk_add (mk_subscript 2 @@ mk_var "vid") @@ mk_var sw_need_vid_ctr.id) @@
                        mk_cint 1]) @@
      mk_block [
        (* reserve a block *)
        mk_insert sw_token_vid_list.id @@ mk_tuple [mk_var "vid"; mk_var sw_need_vid_ctr.id];
        (* clear counter of msgs needing vid *)
        mk_assign sw_need_vid_ctr.id @@ mk_cint 0;
        (* update highest seen vid *)
        mk_assign sw_highest_vid.id @@ mk_var "next_vid";
        (* send on the token *)
        mk_send sw_rcv_token_nm (mk_var sw_next_switch_addr.id) @@ mk_var "next_vid"
      ]) @@
    (* if we have nothing to number, pass the token on as is *)
    mk_send sw_rcv_token_nm (mk_var sw_next_switch_addr.id) @@ mk_var "vid"

(* generate a vid, or pause *)
let sw_gen_vid_nm = "sw_gen_vid"
let sw_gen_vid =
  let vid_num, vid_new, num_new = "vid_num", "vid_new", "num_new" in
  mk_global_fn sw_gen_vid_nm ["_", t_unit] [t_bool; t_vid] @@
  (* if we have a vid, pop it *)
  mk_case_ns (mk_peek @@ mk_var sw_token_vid_list.id) vid_num
    (* if we have no vid available, return false *)
    (mk_tuple [mk_cfalse; min_vid_k3]) @@
    (* else *)
    mk_let num_new t_int (mk_sub (mk_subscript 2 @@ mk_var vid_num) @@ mk_cint 1) @@
    mk_let vid_new t_vid (vid_increment ~vid_expr:(mk_subscript 1 @@ mk_var vid_num)) @@
    mk_block [
      (* check if we've hit <=0 *)
      mk_if (mk_leq (mk_var num_new) @@ mk_cint 0)
        (* if so, delete the entry *)
        (mk_delete sw_token_vid_list.id @@ mk_var vid_num) @@
        (* else, decrement the num and increment the vid *)
        mk_update sw_token_vid_list.id (mk_var vid_num) @@
          mk_tuple [mk_var vid_new; mk_var num_new];
      (* return true and the vid *)
      mk_tuple [mk_ctrue; mk_subscript 1 @@ mk_var vid_num]
    ] 






