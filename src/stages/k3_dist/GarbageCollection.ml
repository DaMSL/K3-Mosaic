open K3.AST
open ProgInfo
open K3Dist
open K3Helpers
open Util

module D = K3Dist
module G = K3Global

(* Description of GC protocol
 * --------------------------
 * Switches keep track of the highest vid that's been acknowledged: max_vid
 *    Have to make sure received acks from every single node the switch sent data to.
 *      Counter per sent vid, or for error checking, keep track of each node that responded
 *    Report to a master switch their max_vid. Master switch finds lowest max_vid.
 * Nodes have a rcv buffer for fetches and puts
 *     Keep track of latest vid processed: stmt counter pointer
 *         Updated with stmt counter complete: may move forward in vid
 *         Updated with put/fetch: may move backwards in vid
 *     2 modes:
 *         Can either wait until max_vid is cleared, or send best achieved vid (best_vid) back to master switch.
 * Master switch finds lowest agreed upon vid and broadcasts GC up to this vid.
 * Nodes delete logs, data, stmt_ctr up to this vid
 * Potential problem: a put that hasn't been processed can insert a potential entry early in the stmt_ctrs
 *     Solution: scan input buffer to make sure nothing with lesser vid than max_vid
 *               Assumes: a received TPCIP packet will have been put in the buffer.
 *                        May need special external function (flush_network(trig_to_call_when_done)) to make this work.
 *)

(*
 * We send acks to the switches upon receiving puts
 *)

(*
 * hard code generate switches list in K3Global
(*
 * switch node list
 * --------------------------------
 * Iterate peers list to get all the switch list, and
 * generate the "switches" global variable
 *)
let switches_name = "switches"
let switches_var = mk_var switches_name

let switches_type = wrap_tset t_addr

let switches_code =
  mk_global_val_init switches_name switches_type @@
  mk_let "sw" t_string (mk_cstring "switch") @@
  mk_filtermap
    (* filter fun*)
    (mk_lambda (wrap_args K3Global.peers_id_type) @@
      mk_eq
        (mk_var K3Global.peers_id_type_name_name)
        (mk_just (mk_cstring "sw")) (* TODO the switch role name is hardcode*)
    )
    (* map fun *)
    (mk_lambda (wrap_args K3Global.peers_id_type) @@
                mk_var K3Global.peers_id_type_addr_name
    ) @@
    mk_var K3Global.peers_name
*)

let dummy_name = "dummy"
let dummy_trig_arg = [dummy_name, t_int]
let unit_arg = ["_", t_unit]

(* prefixes:
  * sw: switch
  * ms: master switch
  * nd: node
*)

(* determine the master switch by lowest address *)
let master_switch_addr =
  let init = some @@ mk_agg_fst
              (mk_assoc_lambda' ["min_addr", t_addr] G.peers_id_type @@
                mk_if (mk_and (mk_lt (mk_var "addr") @@ mk_var "min_addr") @@
                              (mk_eq (mk_var "job") @@ mk_cstring "switch"))
                  (mk_var "addr") @@
                  mk_var "min_addr") @@
              mk_var G.peers_name
  in
  {id="master_switch_addr"; t=t_addr; init; e=[]}

(* Switch acks: track which nodes replied with an ack *)
(* NOTE: candidate for map *)
let sw_ack_log =
  let e = ["vid", t_vid; "addr", t_addr; "ack", t_bool] in
  {id="sw_ack_log"; t=wrap_tset' @@ snd_many e; e; init=None} (* global val*)

(* max acknowledged vid on each switch *)
let sw_max_ack_vid = {id="max_ack_vid"; t=t_vid_mut; e=[]; init=Some min_vid_k3}

(* trigger for recieving an ack from a node *)
let sw_ack_rcv_trig =
  let ack_trig_args = list_drop_end 1 sw_ack_log.e in
  let slice_vars = ids_to_vars @@ fst_many ack_trig_args in
  mk_code_sink' "ack_rcv" ack_trig_args [] @@
    mk_block [
      (* set entry to true *)
      mk_update_slice (sw_ack_log.id) (slice_vars@[mk_cunknown]) @@
        mk_tuple @@ slice_vars@[mk_ctrue];
    ]

(* insert a record into the switch ack log, waiting for ack*)
let sw_ack_init_code =
  mk_insert sw_ack_log.id @@
    mk_tuple @@ modify_e sw_ack_log.e ["ack", mk_cbool false]

(* code to be incorporated in GenDist's rcv_put *)
(* assumes paramters "sender_ip" and "vid" *)
let nd_ack_send_code =
  mk_send (mk_ctarget "ack_rcv") (mk_var "sender_ip") @@ mk_tuple [mk_var "me"; mk_var "vid"]

(* Update the sw_max_ack_vid variable with the highest fully acknowledged vid.
 * The max_ack_vid will be sent to the master switch *)
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
              (mk_var @@ sw_ack_log.id)

(* store the max vid received from each switch *)
let ms_max_sw_vid_map =
  let e = ["switch_addr", t_addr; "max_vid", t_vid] in
  {id="ms_max_sw_vid_map"; t=wrap_tmap' @@ snd_many e; e; init=None}

(* master switch trigger to receive and add to the max vid map *)
let ms_add_max_sw_vid_trig =
  let max_vid = "addr_max_vid" in
  mk_code_sink' "ms_add_max_sw_vid"
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
      List.map gc_std (D.log_ds c) @
      List.map (gc_std |- fst) (D.map_buffers c) @
      List.map (gc_std |- fst) (D.maps c)

let globals =
  [decl_global master_switch_addr;
   decl_global ms_max_sw_vid_map;
   decl_global ms_max_nd_vid_map;
   decl_global sw_max_ack_vid;
   decl_global sw_ack_log;
   decl_global nd_max_done_vid;
  ]

let fns =
  [sw_update_max_ack_vid_fn;
   nd_update_max_done_vid_fn;
  ]

let triggers =
  [ms_add_max_sw_vid_trig;
   ms_add_max_nd_vid_trig;
  ]
