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

(* switch: max acknowledged vid *)
let sw_max_ack_vid = create_ds "sw_max_ack_vid" (mut t_vid) ~init:(mk_var D.g_min_vid.id)

(* switch: trigger for receiving an ack from a node *)
let sw_ack_rcv_trig_nm = "sw_ack_rcv"
let sw_ack_rcv_trig sw_check_done =
  let address = "addr" in
  let ack_trig_args = [address, t_addr; "vid", t_vid] in
  mk_code_sink' sw_ack_rcv_trig_nm ack_trig_args [] @@
  (* increment ack num *)
  mk_block [
    mk_incr sw_num_ack.id;
    mk_delete_with sw_ack_log "x" ~k:[mk_var "vid"] ~delcond:(mk_eq (mk_snd @@ mk_var "x") @@ mk_cint 0)
      ~v:(mk_sub (mk_snd @@ mk_var "x") @@ mk_cint 1);
    sw_check_done
  ]

(* switch: code to update send data structures *)
let sw_update_send ~vid_nm =
  mk_block [
    mk_incr sw_num_sent.id;
    (* increment vid on sw_ack_log *)
    mk_upsert_with sw_ack_log "x" ~k:[mk_var vid_nm] ~default:(mk_cint 0) ~v:(mk_add (mk_snd (mk_var "x")) @@ mk_cint 1)
  ]

(* node: code to be incorporated in GenDist.rcv_put *)
(* send ack to switch *)
let nd_ack_send_code ~addr_nm ~vid_nm =
  mk_send sw_ack_rcv_trig_nm (mk_var addr_nm) [G.me_var; mk_var vid_nm]

(* master: gc delay in seconds *)
let ms_gc_interval = create_ds "ms_gc_interval" (mut t_int) ~init:(mk_cint 20000)

(* master: store the max vid received from each switch *)
let ms_gc_vid_map =
  let e = ["addr", t_addr; "vid", t_vid] in
  create_ds "ms_gc_vid_map" (mut @@ wrap_tmap' @@ snd_many e) ~e

(* master: counter for number of responses *)
let ms_gc_vid_ctr = create_ds "ms_gc_vid_ctr" (mut t_int) ~init:(mk_cint 0)

(* master: number of expected responses *)
let ms_num_gc_expected =
  let init = mk_size_slow @@ G.peers in
  create_ds "ms_num_gc_expected" (mut t_int) ~init

(* function to perform garbage collection *)
(* NOTE: TODO: for now, we use an intermediate ds. A filterInPlace would be better *)
(* NOTE: to be efficient, we need blind write optimization on write (delete) *)
let do_gc_nm = "do_gc"
(* to search for a vid field *)
let r_vid = Str.regexp ".*vid.*"
let do_gc c =
  let min_vid = "min_gc_vid" in
  (* standard gc code for general data structures *)
  let gc_std ds =
    (* look for any entry in the ds containing vid *)
    let vid = fst @@ List.find (r_match r_vid |- fst) ds.e in
    let t' = unwrap_tind ds.t in
    (* handle the possiblity of indirections *)
    let do_bind, id =
      if is_tind ds.t then
        let unwrap = ds.id^"_unwrap" in
        (fun x -> mk_bind (mk_var ds.id) unwrap x), unwrap
      else id_fn, ds.id
    in
    let temp = "temp" in
    (* delete any entry with a lower or matching vid *)
    mk_let [temp] (mk_empty t') @@
    do_bind @@
    (* if we're in a map ds, we need to get the frontier at min_vid *)
    (match ds.map_id with
      | Some map_id ->
          mk_let ["frontier"]
            (map_latest_vid_vals c (mk_var id) None map_id ~keep_vid:true ~vid_nm:min_vid)
      | _ -> id_fn) @@
    mk_block @@
      (* add < vid to temporary collection *)
      (mk_iter
        (mk_lambda' ds.e @@
            mk_if (mk_lt (mk_var vid) @@ mk_var min_vid)
              (mk_insert temp @@ ids_to_vars @@ fst_many ds.e) @@
              mk_cunit) @@
          mk_var id) ::
      (* delete values from ds *)
      (mk_iter
        (mk_lambda' ["val", wrap_ttuple @@ snd_many ds.e] @@
          mk_delete id [mk_var "val"]) @@
        mk_var temp) ::
      (* if we have a mosaic map, insert back the frontier *)
      (if ds.map_id <> None then
        [mk_iter (mk_lambda' ["val", wrap_ttuple @@ snd_many ds.e] @@
            mk_insert id [mk_var "val"]) @@
          mk_var "frontier"]
      else [])
  in
  mk_code_sink' do_gc_nm [min_vid, t_vid] [] @@
    mk_block @@
      [ (* clean node data structures *)
        gc_std D.nd_log_master;
      ] @
      List.map gc_std (D.log_ds c) @
      List.map gc_std (D.map_buffers c) @
      List.map gc_std (D.maps c)

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
      mk_if (mk_geq (mk_var ms_gc_vid_ctr.id) @@ mk_var ms_num_gc_expected.id)
        (* if so ... *)
        (mk_let [min_vid]
          (* get the min vid *)
          (mk_min_max min_vid (mk_var "vid") t_vid mk_lt (mk_var D.g_min_vid.id) ms_gc_vid_map) @@
          mk_block [
            (* clear the counter *)
            mk_assign ms_gc_vid_ctr.id @@ mk_cint 0;
            (* clear the data struct *)
            mk_assign ms_gc_vid_map.id @@ mk_empty ms_gc_vid_map.t;
            (* send gc notices *)
            mk_iter (mk_lambda' G.peers.e @@
              mk_send do_gc_nm (mk_var @@ fst @@ hd @@ G.peers.e) [mk_var min_vid]) @@
              mk_var G.peers.id;
            (* tell timer to ping us in X seconds *)
            mk_send T.tm_insert_timer_trig_nm (mk_var D.timer_addr.id)
              [mk_var ms_gc_interval.id; mk_cint @@ T.num_of_trig c D.ms_send_gc_req_nm; G.me_var]
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
    (mk_send ms_rcv_gc_vid_nm (mk_var master_addr.id)
      [G.me_var; mk_min_max "min_vid" (mk_var "vid") t_vid mk_lt (mk_var TS.sw_highest_vid.id) sw_ack_log]) @@
    (* else, if we're a node *)
    mk_if (mk_eq (mk_var D.job.id) @@ mk_var D.job_node.id)
      (* send out node min vid: much faster if we had a min function *)
      (mk_send ms_rcv_gc_vid_nm (mk_var master_addr.id)
        (* default is max_vid, to allow anything to go on *)
        [G.me_var; mk_min_max "min_vid" (mk_fst @@ mk_var "vid_stmt_id")
          t_vid mk_lt (mk_var D.g_max_vid.id) D.nd_stmt_cntrs])
      (* else, do nothing *)
      mk_cunit

(* master: trigger to request gc vids *)
(* called by the timer *)
let ms_send_gc_req =
  mk_code_sink' D.ms_send_gc_req_nm unit_arg [] @@
  mk_iter
    (mk_lambda' G.peers.e @@
      mk_send rcv_req_gc_vid_nm (mk_var @@ fst @@ hd @@ G.peers.e) [mk_cunit]) @@
    mk_var G.peers.id

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
   sw_max_ack_vid;
   ms_gc_interval;
   ms_gc_vid_map;
   ms_gc_vid_ctr;
   ms_num_gc_expected;
  ]

let triggers c sw_check_done =
  [sw_ack_rcv_trig sw_check_done;
   ms_rcv_gc_vid c;
   rcv_req_gc_vid;
   ms_send_gc_req;
   do_gc c;
  ]
