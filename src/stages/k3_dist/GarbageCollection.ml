open K3.AST
open K3Dist
open K3Helpers
open Util

module D = K3Dist
module G = K3Global
module Std = K3StdLib
module T = Timer
module TS = Timestamp

(* Description of GC protocol
 * --------------------------
 * Master switch must be aware of all nodes and switches
 * Master switch keeps track of min vid from each switch and node
 * Each node acks its received Put message to the sending switch.
 * Each switch keeps track of its max ack vid and sends it X seconds after GC.
 * Each node keeps track of its max executed vid and sends it X seconds after GC.
 * If a node/switch has no unacked/unexecuted vids, it sends the max vid seen X minutes after GC.
 * Master switch takes minimum vid (or same vid) and broadcasts a DoGC.
 * Nodes delete logs, data, stmt_ctr up to this vid
 *
 *     Possible realtime updates:
 *         Updated with stmt counter complete: may move forward in vid
 *         Updated with put/fetch: may move backwards in vid
 *)

(* prefixes:
  * sw: switch
  * ms: master switch
  * nd: node
  * tm: timer
*)

(* --- Acks between switches and nodes --- *)

(* Switch acks: track how many nodes replied with an ack *)
let sw_num_ack  = create_ds "sw_num_ack"  (mut t_int)
let sw_num_sent = create_ds "sw_num_sent" (mut t_int)
(* vids and numbers of sent at that vid *)
let sw_ack_log  =
  let e = ["vid", t_vid; "count", t_int] in
  create_ds "sw_ack_log" (wrap_tmap' @@ snd_many e) ~e

(* switch: trigger for receiving an ack from a node *)
let sw_ack_rcv_trig_nm = "sw_ack_rcv"
let sw_ack_rcv_trig sw_check_done =
  let address = "addr" in
  let ack_trig_args = [address, t_addr; "vid", t_vid] in
  mk_code_sink' sw_ack_rcv_trig_nm ack_trig_args [] @@
  (* increment ack num *)
  mk_block [
    mk_incr sw_num_ack.id;
    mk_delete_with sw_ack_log "x" ~k:[mk_var "vid"] ~delcond:(mk_eq (mk_snd @@ mk_var "x") @@ mk_cint 1)
      ~v:(mk_sub (mk_snd @@ mk_var "x") @@ mk_cint 1);
    sw_check_done
  ]

(* switch: code to update send data structures *)
let sw_update_send ~vid_nm =
  mk_block [
    mk_incr sw_num_sent.id;
    (* increment vid on sw_ack_log *)
    mk_upsert_with_sim sw_ack_log "x" ~k:[mk_var vid_nm] ~default:(mk_cint 1) ~v:(mk_add (mk_snd @@ mk_var "x") @@ mk_cint 1)
  ]

(* node: code to be incorporated in GenDist.rcv_put *)
(* send ack to switch *)
let nd_ack_send_code ~addr_nm ~vid_nm =
  mk_send sw_ack_rcv_trig_nm (mk_var addr_nm) [G.me_var; mk_var vid_nm]

(* master: gc delay in seconds *)
let ms_gc_interval = create_ds "ms_gc_interval" (mut t_int) ~init:(mk_cint 300000)

(* master: store the max vid received from each switch *)
let ms_gc_vid_map =
  let e = ["addr", t_addr; "vid", t_vid] in
  create_ds "ms_gc_vid_map" (mut @@ wrap_tmap' @@ snd_many e) ~e

(* master: counter for number of responses *)
let ms_gc_vid_ctr = create_ds "ms_gc_vid_ctr" (mut t_int) ~init:(mk_cint 0)

(* master: keep track of last gc vid so we don't issue an unneeded gc *)
let ms_last_gc_vid = create_ds "ms_last_gc_vid" (mut t_vid) ~init:(mk_var D.g_min_vid.id)

(* master: number of expected responses *)
let ms_num_gc_expected =
  (* delayed init after we have num of switched and nodes *)
  let d_init = mk_add (mk_var D.num_switches.id) @@ mk_var D.num_nodes.id in
  create_ds "ms_num_gc_expected" (mut t_int) ~d_init

let ms_gc_done_barrier_ctr = mk_counter "ms_gc_done_barrier_ctr"

(* NOTE: gc barrier is only on nodes *)
let ms_gc_done_barrier_nm = "ms_gc_done_barrier"
let ms_gc_done_barrier c = mk_barrier ms_gc_done_barrier_nm ~ctr:ms_gc_done_barrier_ctr.id
  ~total:(mk_var D.num_nodes.id)
  ~after:(* tell timer to ping us in X seconds *)
         (mk_send T.tm_insert_timer_trig_nm (mk_var D.timer_addr.id)
           [mk_var ms_gc_interval.id; mk_cint @@ T.num_of_trig c D.ms_send_gc_req_nm; G.me_var])

(* data structures needing gc *)
let ds_to_gc c =
  D.nd_log_master ::
  D.log_ds c @
  D.map_buffers c @
  D.maps c

(* functions to perform garbage collection *)
(* NOTE: TODO: for now, we use an intermediate ds. A filterInPlace would be better *)
(* NOTE: to be efficient, we need blind write optimization on write (delete) *)
let do_gc_nm = "do_gc"
(* to search for a vid field *)
let r_vid = Str.regexp ".*vid.*"
let do_gc_fns c =
  let min_vid = "gc_vid" in
  (* standard gc code for general data structures *)
  let gc_std ds =
    let fn_nm = "do_gc_"^ds.id in
    mk_global_fn fn_nm [min_vid, t_vid] [] @@
    match ds.map_id with
      | Some _ ->
          let map_deref = "map_d" in
          let pat   = D.pat_of_ds ~vid_nm:min_vid ds in
          (* local bind to prevent bind-in-bind *)
          let do_bind = mk_bind (mk_var ds.id) map_deref in
          (* get frontier *)
          do_bind @@
            mk_assign map_deref @@ U.add_property "Move" @@
          mk_let ["frontier"]
            (mk_slice_frontier (mk_var map_deref) @@ vid_and_unknowns' pat) @@
            (* delete all prefixes in ds. min_vid comes from pattern *)
            mk_aggv
              (mk_lambda3' ["acc", ds.t] ["_", t_vid] (ds_e ds) @@
                mk_block [
                  mk_delete_prefix "acc" @@ fst_many pat;
                  mk_var "acc"]
                )
              (mk_var map_deref) @@
              mk_var "frontier"
      | None -> (* non-map ds *)
        (* look for any entry in the ds containing vid *)
        let vid = fst @@ List.find (r_match r_vid |- fst) (ds_e ds) in
        let t' = ds.t in
        let ds_ids = fst_many @@ ds_e ds in
        let temp = "temp" in
        let agg_fn = if ds.id = nd_log_master.id
                     then mk_lambda2' ["acc", ds.t] (ds_e ds) @@
                            mk_block [
                              mk_insert "acc" [mk_var @@ hd ds_ids; mk_filter_geq' vid [mk_var min_vid]];
                              mk_var "acc"]
                     else mk_lambda2' ["acc", ds.t] (ds_e ds) @@
                            mk_if (mk_geq (mk_var vid) @@ mk_var min_vid)
                              (mk_block [
                                mk_insert "acc" @@ ids_to_vars @@ fst_many @@ ds_e ds;
                                mk_var "acc"]) @@
                              mk_var "acc"
        in
        (* delete any entry with a lower or matching vid *)
        mk_let [temp] (mk_empty t') @@
        mk_assign ds.id @@ U.add_property "Move" @@
        mk_agg agg_fn (mk_empty ds.t) @@ mk_var ds.id
  in
  List.map gc_std @@ ds_to_gc c

let do_gc_trig c =
  let min_vid = "gc_vid" in
  mk_code_sink' do_gc_nm [min_vid, t_vid] [] @@
    mk_block @@
      (* (mk_apply' "print_env" mk_cunit) :: (* debug *) *)
      (mk_apply' "print" [mk_cstring "Starting GC"]) ::
      (List.map (fun ds -> mk_apply' ("do_gc_"^ds.id) [mk_var min_vid]) @@ ds_to_gc c) @
      [D.mk_send_master ms_gc_done_barrier_nm]

(* master switch trigger to receive and add to the max vid map *)
let ms_rcv_gc_vid_nm = "ms_rcv_gc_vid"
let ms_rcv_gc_vid c =
  let data, min_vid = "data", "min_vid" in
  mk_code_sink' ms_rcv_gc_vid_nm
    [data, wrap_ttuple @@ snd_many ms_gc_vid_map.e] [] @@
    mk_block [
      (* insert into data struct *)
      mk_insert ms_gc_vid_map.id [mk_var data];
      (* increment count *)
      mk_assign ms_gc_vid_ctr.id @@
        mk_add (mk_var ms_gc_vid_ctr.id) @@ mk_cint 1;
      (* check if we have enough responses *)
      mk_if (mk_eq (mk_var ms_gc_vid_ctr.id) @@ mk_var ms_num_gc_expected.id)
        (* if so ... *)
        (mk_let [min_vid]
          (* get the min vid *)
          (mk_min_max min_vid (mk_var "vid") t_vid mk_lt (mk_var D.g_max_vid.id) ms_gc_vid_map) @@
          mk_block [
            (* clear the counter *)
            mk_assign ms_gc_vid_ctr.id @@ mk_cint 0;
            (* clear the data struct *)
            mk_assign ms_gc_vid_map.id @@ mk_empty ms_gc_vid_map.t;
            (* if we've advanced since last gc *)
            mk_if (mk_gt (mk_var min_vid) @@ mk_var ms_last_gc_vid.id)
              (mk_block [ (* then *)
                (* send gc notices to nodes only *)
                (* NOTE: currently there's no need to send do_gc to the switches. If that changes, this check needs
                 * to change as wel *)
                mk_send_all_nodes do_gc_nm [mk_var min_vid];
                (* overwrite last gc vid *)
                mk_assign ms_last_gc_vid.id @@ mk_var min_vid; ])
              mk_cunit; (* else nothing *)
          ])
        (* if not enough responses, do nothing *)
        mk_cunit
    ]

(* all nodes/switches: respond to request for vid info *)
let rcv_req_gc_vid_nm = "rcv_req_gc_vid"
let rcv_req_gc_vid =
  mk_code_sink' rcv_req_gc_vid_nm unit_arg [] @@
  (* if we're a switch *)
  mk_if (mk_or (mk_eq (mk_var D.job.id) @@ mk_var D.job_switch.id) @@
                mk_eq (mk_var D.job.id) @@ mk_var D.job_master.id)
    (* send our min vid: this would be much faster with a min function *)
    (D.mk_send_master ms_rcv_gc_vid_nm
      ~payload:[G.me_var;
        mk_min_max "min_vid" (mk_var "vid") t_vid mk_lt (mk_var TS.sw_highest_vid.id) sw_ack_log]) @@
    (* else, if we're a node *)
    mk_if (mk_eq (mk_var D.job.id) @@ mk_var D.job_node.id)
      (* send out node min vid: much faster if we had a min function *)
      (mk_send_master ms_rcv_gc_vid_nm
        (* default is max_vid (infinity), to allow anything to go on *)
        ~payload:[G.me_var; mk_min_max "min_vid" (mk_fst @@ mk_var "vid_stmt_id")
          t_vid mk_lt (mk_var D.g_max_vid.id) D.nd_stmt_cntrs])
      (* else, do nothing *)
      mk_cunit

(* master: trigger to request gc vids *)
(* called by the timer *)
let ms_send_gc_req =
  mk_code_sink' D.ms_send_gc_req_nm unit_arg [] @@
  mk_iter
    (mk_lambda' D.switches.e @@
      let addr_var = mk_var @@ fst @@ hd @@ D.switches.e in
      mk_send rcv_req_gc_vid_nm addr_var [mk_cunit]) @@
    (* send to all switches and nodes *)
    (mk_combine (mk_var D.switches.id) @@ mk_var D.nodes.id)

(* master: start the gc process *)
let ms_gc_init c =
  mk_send T.tm_insert_timer_trig_nm (mk_var D.timer_addr.id)
    [mk_var ms_gc_interval.id; mk_cint @@ T.num_of_trig c D.ms_send_gc_req_nm; G.me_var]

(* --- End of code --- *)

let global_vars _ = List.map decl_global
  [
   sw_num_ack;
   sw_num_sent;
   sw_ack_log;
   ms_gc_interval;
   ms_gc_vid_map;
   ms_gc_vid_ctr;
   ms_last_gc_vid;
   ms_num_gc_expected;
   ms_gc_done_barrier_ctr;
  ]

let functions c =
  do_gc_fns c

let triggers c sw_check_done =
  [sw_ack_rcv_trig sw_check_done;
   ms_rcv_gc_vid c;
   rcv_req_gc_vid;
   ms_send_gc_req;
   ms_gc_done_barrier c;
   do_gc_trig c;
  ]
