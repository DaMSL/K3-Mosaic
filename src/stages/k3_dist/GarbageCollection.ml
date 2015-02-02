open K3.AST
open K3Dist
open K3Helpers
open Util

module D = K3Dist
module G = K3Global
module Std = K3StdLib

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
*)

(* --- Acks between switches and nodes --- *)

(* Switch acks: track which nodes replied with an ack *)
let sw_ack_log =
  let e = ["vid", t_vid; "addresses", wrap_tset t_addr] in
  {id="sw_ack_log"; t=wrap_tmap' @@ snd_many e; e; init=None} (* global val*)

(* switch: max acknowledged vid *)
let sw_max_ack_vid = {id="sw_max_ack_vid"; t=t_vid_mut; e=[]; init=Some min_vid_k3}

(* insert a record into the switch ack log, waiting for ack (in send_put) *)
let sw_ack_init_code ~addr_nm ~vid_nm =
  let inner_t = snd @@ list_last sw_ack_log.e in
  mk_case_sn
    (mk_peek @@ mk_slice (mk_var sw_ack_log.id) @@ mk_tuple [mk_var vid_nm; mk_cunknown])
    "old_val"
    (mk_insert sw_ack_log.id @@
      mk_tuple [mk_var "vid"; mk_combine (mk_subscript 2 @@ mk_var "old_val") @@
                                mk_singleton inner_t @@ mk_var addr_nm])
    mk_cunit

(* trigger for receiving an ack from a node *)
let sw_ack_rcv_trig =
  let ack_trig_args = ["vid", t_vid; "address", t_addr] in
  let inner_t = snd @@ list_last sw_ack_log.e in
  let old_col, old_val = "old_col", "old_val" in
  mk_code_sink' "ack_rcv" ack_trig_args [] @@
  mk_case_sn
    (mk_peek @@ mk_slice (mk_var sw_ack_log.id) @@ mk_tuple [mk_var "vid"; mk_cunknown])
    old_val
    (mk_let old_col inner_t (mk_subscript 2 @@ mk_var "old_val") @@
      mk_block [
        mk_delete old_col @@ mk_var "address";
        (* check if we need to delete entry *)
        mk_case_sn (mk_peek old_col)
          "some_left"

          (* delete the entry if we're the only thing left *)
          (mk_delete sw_ack_log.id @@ mk_tuple [mk_var "vid"; mk_singleton inner_t @@ mk_var "address"])



      mk_insert sw_ack_log

    mk_insert (sw_ack_log.id) @@ mk_tuple @@ modify_e sw_ack_log.e ["ack", mk_ctrue])
    mk_cunit

(* code to be incorporated in GenDist's rcv_put *)
(* assumes parameters "sender_ip" and "vid" *)
let nd_ack_send_code =
  mk_send (mk_ctarget "ack_rcv") (mk_var "sender_ip") @@ mk_tuple [mk_var "me"; mk_var "vid"]

(* Update the sw_max_ack_vid variable with the highest fully acknowledged vid,
 * and the sw_ack_vid_all *)
let sw_update_max_ack_vid_fn =
  let sw_ack1, sw_ack2 = id_t_add "1" sw_ack_log.e, id_t_add "2" sw_ack_log.e in
  mk_global_fn "update_max_ack_vid" unit_arg [] @@
    mk_subscript 2 @@
      (* fold ascending until we find an entry with no ack *)
      mk_agg
        (mk_assoc_lambda' ["last_max", t_vid; "cur_max", t_vid; "cont", t_bool]
                          sw_ack_log.e @@
          mk_if (mk_not @@ mk_var "ack")
            (mk_tuple [mk_var "last_max"; mk_var "last_max"; mk_cfalse]) @@
            mk_if (mk_and (mk_var "cont") @@ D.v_neq (mk_var "vid") (mk_var "cur_max"))
              (mk_tuple [mk_var "cur_max"; mk_var "vid"; mk_ctrue]) @@
              mk_tuple [mk_var "last_max"; mk_var "cur_max"; mk_cfalse])
        (mk_tuple [mk_var sw_max_ack_vid.id; mk_var sw_max_ack_vid.id; mk_ctrue]) @@
          (* sort ascending *)
          mk_sort
            (mk_assoc_lambda' sw_ack1 sw_ack2 @@
              D.v_lt (mk_var "vid1") (mk_var "vid2")) @@
            (* get values greater than max_ack_vid *)
            mk_filter
              (mk_lambda' sw_ack_log.e @@
                D.v_gt (mk_var "vid") @@ mk_var @@ sw_max_ack_vid.id)
              (mk_var sw_ack_log.id)

(* store the max vid received from each switch *)
let ms_max_sw_vid_map =
  let e = ["switch_addr", t_addr; "max_vid", t_vid] in
  {id="ms_max_sw_vid_map"; t=wrap_tmap' @@ snd_many e; e; init=None}

(* master switch trigger to receive and add to the max vid map *)
let ms_add_max_sw_vid = "ms_add_max_sw_vid"
let ms_add_max_sw_vid_trig =
  let max_vid = "addr_max_vid" in
  mk_code_sink' ms_add_max_sw_vid
    [max_vid, wrap_ttuple @@ snd_many ms_max_sw_vid_map.e] [] @@
    mk_insert ms_max_sw_vid_map.id @@ mk_var max_vid

(* store the max vid received from each node *)
let ms_max_nd_vid_map =
  let e = ["node_addr", t_addr; "max_vid", t_vid] in
  {id="ms_max_nd_vid_map"; e; t=wrap_tmap' @@ snd_many e; init=None}

(* master switch trigger to receive and add to the max node vid map *)
let ms_add_max_nd_vid_trig =
  let max_vid = "addr_max_vid" in
  mk_code_sink' "ms_add_max_nd_vid"
    [max_vid, wrap_ttuple @@ snd_many ms_max_nd_vid_map.e] [] @@
    mk_insert ms_max_nd_vid_map.id @@ mk_var max_vid

(* find min vid to send to all nodes and switches *)
let ms_send_min_vid = "ms_send_min_vid_trig"
let ms_send_min_vid_trig =
  let min_vid, min_vid_sw, min_vid_nd, max_vid = "min_vid", "min_vid_sw", "min_vid_nd", "max_vid" in
  mk_code_sink' ms_send_min_vid unit_arg [] @@
    (* get min of switch max vids *)
    mk_let min_vid_sw t_vid
      (mk_agg
        (mk_assoc_lambda' [min_vid, t_vid] ms_max_sw_vid_map.e @@
          mk_if (mk_lt (mk_var max_vid) @@ mk_var min_vid)
            (mk_var max_vid) @@
            mk_var min_vid)
      min_vid_k3 @@
      mk_var ms_max_sw_vid_map.id) @@
    (* get min of node max vids *)
    mk_let min_vid_nd t_vid
      (mk_agg
        (mk_assoc_lambda' [min_vid, t_vid] ms_max_nd_vid_map.e @@
          mk_if (mk_lt (mk_var "max_vid") @@ mk_var min_vid)
            (mk_var "max_vid") @@
            mk_var min_vid)
      min_vid_k3 @@
      mk_var ms_max_nd_vid_map.id) @@
    (* min_vid *)
    mk_let min_vid t_vid
      (mk_if (mk_lt (mk_var min_vid_sw) @@ mk_var min_vid_nd)
        (mk_var min_vid_sw) @@
         mk_var min_vid_nd) @@
    (* broadcast gc *)
    mk_iter
      (mk_lambda' peers.e @@
        mk_send (mk_ctarget "exec_gc") (mk_var "addr") @@ mk_var min_vid) @@
      mk_var peers.id

(* node: find max vid for which we have 0 stmt cntrs *)
let nd_max_done_vid = {id="nd_max_done_vid"; e=[]; t=t_vid_mut; init=Some min_vid_k3}

(* update nd_max_done_vid *)
(* NOTE: could be done in 'real time' when receiving puts/pushes *)
let nd_update_max_done_vid_fn =
  let done1, done2 = id_t_add "1" D.nd_stmt_cntrs.e, id_t_add "2" D.nd_stmt_cntrs.e in
  mk_global_fn "nd_update_max_done_vid" unit_arg [] @@
    mk_subscript 2 @@
      (* fold ascending until we find an entry with no 0 in any of its stmts *)
      mk_agg
        (mk_assoc_lambda' ["last_max", t_vid; "cur_max", t_vid; "cont", t_bool]
                          D.nd_stmt_cntrs.e @@
          mk_if (mk_neq (mk_var "counter") @@ mk_cint 0)
            (mk_tuple [mk_var "last_max"; mk_var "last_max"; mk_cfalse]) @@
            mk_if (mk_and (mk_var "cont") @@ D.v_neq (mk_var "vid") (mk_var "cur_max"))
              (mk_tuple [mk_var "cur_max"; mk_var "vid"; mk_ctrue]) @@
              mk_tuple [mk_var "last_max"; mk_var "cur_max"; mk_cfalse])
        (mk_tuple [mk_var nd_max_done_vid.id; mk_var nd_max_done_vid.id; mk_ctrue]) @@
          (* sort ascending *)
          mk_sort
            (mk_assoc_lambda' done1 done2 @@
              D.v_lt (mk_var "vid1") (mk_var "vid2")) @@
            (* get values greater than nd_max_done_vid *)
            mk_filter
              (mk_lambda' nd_stmt_cntrs.e @@
                D.v_gt (mk_var "vid") @@ mk_var @@ nd_max_done_vid.id) @@
              mk_var D.nd_stmt_cntrs.id

(* to search for a vid field *)
let r_vid = Str.regexp ".*vid.*"

(* function to perform garbage collection *)
(* NOTE: TODO: for now, we use an intermediate ds. A filterInPlace would be better *)
(* NOTE: to be efficient, we need blind write optimization on write (delete) *)
let do_gc_fn c ast =
  let min_vid = "min_gc_vid" in
  (* standard gc code for general data structures *)
  let gc_std ?do_bind ds =
    (* look for any entry containing vid *)
    let vid_str = fst @@ List.find (fun (s,_) -> r_match r_vid s) ds.e in
    let t' = unwrap_tind ds.t in
    (* handle the possiblity of indirections *)
    let do_bind, id =
      if is_tind ds.t then
        let unwrap = ds.id^"_unwrap" in
        (fun x -> mk_bind (mk_var ds.id) unwrap x), unwrap
      else id_fn, ds.id
    in
    let temp = "temp" in
    (* delete any entry with a lower vid *)
    mk_let [temp] (mk_empty t') @@
    mk_block [
      (* add to temp *)
      do_bind @@
        mk_iter
          (mk_lambda' ds.e @@
            mk_if (mk_lt (mk_var vid_str) @@ mk_var min_vid)
              (mk_insert temp @@ mk_tuple @@ ids_to_vars @@ fst_many ds.e) @@
              mk_cunit) @@
          mk_var id;
      (* delete from ds *)
      mk_iter
        (mk_lambda' ["val", wrap_ttuple @@ snd_many ds.e] @@
          do_bind @@ mk_delete id @@ mk_var "val") @@
        mk_var temp
    ]
  in
  mk_global_fn "do_gc" [min_vid, t_vid] [t_unit] @@
    mk_block @@
      (* clean master data structures *)
      [ gc_std ms_max_sw_vid_map;
        gc_std ms_max_nd_vid_map;
        (* clean switch data structures *)
        gc_std sw_ack_log;
        (* clean node data structures *)
        gc_std nd_max_done_vid;
        gc_std D.nd_stmt_cntrs;
        gc_std D.nd_log_master;
      ] @
      (* clean dynamic structures *)
      List.map gc_std (D.log_ds c) @
      List.map (gc_std |- fst) (D.map_buffers c) @
      List.map (gc_std |- fst) (D.maps c)

let ms_last_gc_time = {id="ms_last_gc_time"; t=mut t_int; e=[];
                       init=some @@ mk_apply (mk_var Std.now_int_name) mk_cunit}
let sw_last_send_time = {ms_last_gc_time with id="sw_last_send_time"}
let nd_last_send_time = {ms_last_gc_time with id="nd_last_send_time"}

let ms_gc_interval = {id="ms_gc_interval"; t=mut t_int; e=[]; init=some @@ mk_cint 300}
let sw_send_interval = {ms_gc_interval with id="sw_send_interval"}
let nd_send_interval = {ms_gc_interval with id="nd_send_interval"}

(* code to do with triggering the gc process. We perform gc in single-thread mode *)
(* insert this code into triggers *)
let check_time_code last_time interval_var trig_nm =
  mk_let "cur" t_int
    (mk_apply (mk_var Std.now_int_name) mk_cunit) @@
  mk_if
    (mk_gt
      (mk_sub
        (mk_var "cur") @@
        mk_var last_time.id) @@
      mk_var interval_var.id)
    (mk_send trig_nm G.me_var mk_cunit) @@
    mk_cunit

let ms_check_time_code = check_time_code ms_last_gc_time.id ms_gc_interval.id ms_send_min_vid_trig
let nd_check_time_code = check_time_code ms_last_gc_time.id ms_gc_interval.id ms_send_min_vid_trig

(* generic trigger to send variable to master switch *)
let send_var_trig trig_nm fn_nm var_nm =
    mk_code_sink' trig_nm ["_", t_unit] [] @@
      mk_block [
        mk_apply fn_nm mk_cunit;
        mk_send ms_add_max_sw_vid (mk_var master_switch_addr.id) @@
          mk_tuple [G.me_var; mk_var var_nm]
      ]

let nd_send_max_done_vid_trig =
  send_var_trig "nd_send_max_done_vid_trig" nd_update_max_done_vid_fn nd_max_done_vid.id

let sw_send_max_ack_vid_trig =
  send_var_trig "sw_send_max_ack_vid_trig" sw_update_max_ack_vid_fn nd_max_done_vid.id

let global_vars =
  [decl_global master_addr;
   decl_global is_master;
   decl_global ms_max_sw_vid_map;
   decl_global ms_max_nd_vid_map;
   decl_global sw_max_ack_vid;
   decl_global sw_ack_vid_all;
   decl_global sw_ack_log;
   decl_global nd_max_done_vid;
   decl_global last_gc_time;
  ]

let functions =
  [sw_update_max_ack_vid_fn;
   nd_update_max_done_vid_fn;
  ]

let triggers =
  [sw_ack_rcv_trig;
   ms_add_max_sw_vid_trig;
   ms_add_max_nd_vid_trig;
   nd_send_max_done_vid_trig;
   sw_send_max_ack_vid_trig;
  ]
