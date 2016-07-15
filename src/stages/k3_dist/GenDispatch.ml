open Util
open K3.AST
open K3Helpers
open K3Dist

module D = K3Dist
module G = K3Global
module U = K3Util
module T = K3Typechecker
module R = K3Route
module P = ProgInfo
module K3R = K3Route
module K3S = K3Shuffle
module K3N = K3NewPrint
module GC = GarbageCollection
module Proto = Protocol
module TS = Timestamp

open GenCommon
open GenPush
open GenRcvFetchPut

(* function to handle dispatch with push data *)
let nd_handle_uniq_poly_nm = "nd_handle_uniq_poly"
let nd_handle_uniq_poly c =
  let ts = P.get_trig_list c.p ~sys_init:true ~delete:c.gen_deletes in
  let s_rhs = List.flatten @@ List.map (P.s_and_over_stmts_in_t c.p P.rhs_maps_of_stmt) ts in
  let bufs_m = List.map (fun (s,m) -> P.buf_of_stmt_map_id c.p s m, m) s_rhs in
  let tag_bufs_m = List.map (fun (buf, m) ->
                      (List.find (fun ti -> ti.tag = buf) c.poly_tags).itag, buf, m) bufs_m in
  mk_global_fn nd_handle_uniq_poly_nm ["poly_queue", upoly_queue.t] [] @@
  (* iterate over all buffer contents *)
  mk_if_eq (mk_size @@ mk_var "poly_queue") (mk_cint 0)
    mk_cunit @@
  mk_let ["idx"; "offset"] (mk_tuple [mk_cint 0; mk_cint 0]) @@
  mk_ignore @@ mk_poly_iter' @@
    mk_lambda'' (p_tag @ p_idx @ p_off) @@
      do_trace "nhu"
                [t_int, mk_var "tag";
                 t_int, mk_var "idx";
                 t_int, mk_var "offset"] @@
      List.fold_left (fun acc_code (tag, buf, m) ->
        let map_ds = map_ds_of_id ~global:true c m in
        let tup_ds = map_ds_of_id ~global:false ~vid:true c m in
        let tup_pat = pat_of_ds tup_ds in
        let map_pat = pat_of_flat_e ~add_vid:true ~has_vid:true map_ds @@ fst_many tup_pat in
        mk_if_eq (mk_var "tag") (mk_cint tag)
          (mk_block [
            mk_bind (mk_var buf) "buf_d" @@
              (* assign a fold result back to the buffer *)
              mk_assign "buf_d" @@ U.add_property "Move" @@
                mk_poly_fold_tag' buf
                  (mk_lambda'' (["acc", map_ds.t] @ poly_args_partial @
                                ["x", wrap_ttuple @@ snd_many @@ ds_e tup_ds]) @@
                    mk_let (fst_many @@ ds_e tup_ds) (mk_var "x") @@
                    mk_insert_block "acc" map_pat) @@
              mk_var "buf_d";
            (* skip the tags we just handled *)
            mk_poly_skip_all' buf
            ])
          acc_code)
      (mk_error "unhandled unique tag")
      tag_bufs_m

(* arg order: poly_args, const_args(count etc), trig specific_args, batch_id, vid, trig_args *)
let sub_trig_dispatch c fn_nm t_args =
  List.fold_left
    (fun acc_code ti -> match ti.tag_typ with
        (* only match with the sub-triggers *)
        | SubTrig(has_ds, ts) when List.mem fn_nm ts ->
        mk_if_eq (mk_var "tag") (mk_cint ti.itag)
          (* look up this tag *)
          (mk_poly_at_with' ti.tag @@ mk_lambda' ti.args @@
            (* if we have subdata, the function must return the new idx, offset *)
            let batch_arg = if ti.batch_id then [mk_var "batch_id"] else [] in
            let trig_args = if ti.trig_args then t_args else [mk_var "vid"] in
            let call l = mk_apply' ti.fn @@
                l @ ti.const_args @ (ids_to_vars @@ fst_many @@ ti.args) @ batch_arg @ trig_args in
            if has_ds then call @@ ids_to_vars @@ fst_many D.poly_args
              (* otherwise we must skip ourselves *)
            else mk_block [call []; mk_poly_skip' ti.tag])
          acc_code
        | _ -> acc_code)
    (* the way to stop the loop is to keep the same idx, offset *)
    (mk_tuple [mk_var "idx"; mk_var "offset"])
    c.poly_tags

(* handle all sub-trigs that need only a vid (pushes) *)
let nd_no_arg_trig_sub_handler c t =
  let fn_nm = trig_no_arg_sub_handler_name_of_t t in
  mk_global_fn fn_nm (poly_args @ trig_no_arg_sub_handler_args) [t_int; t_int] @@
  (* skip over the entry tag *)
  mk_poly_skip_block fn_nm [
    (* clear the trig send bitmaps. These make sure we output a slim trig header
       for any sub-trigger output (ie. pushes) *)
    mk_clear_all D.send_trig_header_bitmap.id;
    (* dispatch the sub triggers (pushes) *)
    mk_poly_iter' @@
      mk_lambda'' (p_tag @ p_idx @ p_off) @@
        (* print trace if requested *)
        do_trace ("nstsh"^trace_trig t)
                  [t_int, mk_var "vid"] @@
        sub_trig_dispatch c fn_nm @@ args_of_t_as_vars_with_v c t;
  ]

(* handle all sub-trigs that need only a vid (pushes) *)
let nd_load_arg_trig_sub_handler c t =
  let fn_nm = trig_load_arg_sub_handler_name_of_t t in
  let load_args = D.args_of_t c t in
  mk_global_fn fn_nm (poly_args @ trig_load_arg_sub_handler_args) [t_int; t_int] @@
  (* skip over the entry tag *)
  mk_poly_skip_block fn_nm [
    (* clear the trig send bitmaps. These make sure we output a slim trig header
       for any sub-trigger output (ie. pushes) *)
    mk_clear_all D.send_trig_header_bitmap.id;
    (* load the trig args *)
    (if load_args <> [] then
      mk_let (fst_many @@ D.args_of_t c t)
        (mk_apply' (nd_log_get_bound_for t) [mk_var "vid"])
     else id_fn) @@
    (* dispatch the sub triggers *)
    mk_poly_iter' @@
      mk_lambda'' (p_tag @ p_idx @ p_off) @@
        (* print trace if requested *)
        do_trace ("nstsh"^trace_trig t)
                  [t_int, mk_var "vid"] @@
          sub_trig_dispatch c fn_nm @@ args_of_t_as_vars_with_v c t;
  ]

(*** central triggers to handle dispatch for nodes and switches ***)

(* handles rcv_fetch, rcv_put, rcv_do_complete, rcv_push, rcv_corrective, rcv_ack *)
(* we need to handle buffering fetches (rcv_fetch) at the aggregate level *)
let trig_dispatcher_nm = "trig_dispatcher"
let trig_dispatcher c =
  mk_global_fn trig_dispatcher_nm
    ["batch_id", t_vid; "poly_queue", poly_queue.t] [] @@
  mk_let_block ["is_isobatch"] (is_isobatch_id "batch_id") [
    mk_if (mk_and (mk_not @@ mk_var corrective_mode.id) @@ mk_var "is_isobatch")
      (* clear dses *)
      (mk_apply' clear_buffered_fetch_helper_nm [])
      mk_cunit;

    (* iterate over all buffer contents *)
    mk_let ["idx"; "offset"] (mk_tuple [mk_cint 0; mk_cint 0]) @@
    mk_ignore @@ mk_poly_iter' @@
      mk_lambda'' ["tag", t_int; "idx", t_int; "offset", t_int] @@
        (* print trace if requested *)
        do_trace "td"
                 [t_int, mk_var "tag";
                  t_int, mk_var "idx";
                  t_int, mk_var "offset"] @@
        List.fold_left
          (fun acc_code ti -> match ti.tag_typ with
             | Trig has_ds ->
              mk_if (mk_eq (mk_var "tag") @@ mk_cint ti.itag)
                (* look up this tag *)
                (mk_poly_at_with' ti.tag @@ mk_lambda' ti.args @@
                  (* if we have subdata, the function must return the new idx, offset *)
                  let batch_arg = if ti.batch_id then ["batch_id", t_vid] else [] in
                  let call l =
                    mk_apply' ti.fn @@
                      ti.const_args @ ids_to_vars @@ fst_many @@ l @ batch_arg @ ti.args in
                  if has_ds then call D.poly_args
                    (* otherwise we must skip ourselves *)
                  else mk_block [call []; mk_poly_skip' ti.tag])
                acc_code
             | _ -> acc_code)
          (mk_error' @@ mk_concat (mk_cstring "unmatched tag: ") @@ mk_soi @@ mk_var "tag")
          c.poly_tags;

    mk_if (mk_var "is_isobatch")
      (mk_block [
          mk_if (mk_not @@ mk_var corrective_mode.id)
            (* move the temporary buffered_fetch data into the permanent ds *)
            (mk_iter_bitmap' ~idx:stmt_ctr.id
              (mk_block [
                mk_let ["x"]
                  (mk_delete_at isobatch_buffered_fetch_helper_id @@ mk_var stmt_ctr.id) @@
                mk_update_at_with isobatch_buffered_fetch_vid_map_id (mk_var stmt_ctr.id) @@
                  mk_lambda' isobatch_vid_map_e @@
                    mk_insert_block "inner" [mk_var "batch_id"; mk_var "x"]
              ])
              rcv_fetch_isobatch_decision_bitmap_id)
            mk_cunit;
          (* for pushes, send the batch_push messages last, so all data pushes have
           * already been received *)
          mk_apply' nd_send_isobatch_push_meta_nm [mk_var "batch_id"];
        ])
      mk_cunit;

    send_poly_queues;
  ]

let trig_dispatcher_unique_nm = "trig_dispatcher_unique"
let trig_dispatcher_unique c =
  mk_global_fn trig_dispatcher_unique_nm
    ["batch_id", t_vid; "poly_queue", poly_queue.t; "upoly_queue", upoly_queue.t] [] @@
  mk_block [
    clear_poly_queues c;
    (* handle any unique poly data *)
    mk_apply' nd_handle_uniq_poly_nm [mk_var "upoly_queue"];
    (* continue with regular dispatching *)
    mk_apply' trig_dispatcher_nm [mk_var "batch_id"; mk_var "poly_queue"];
  ]

(* buffer for dispatcher to reorder poly msgs *)
let nd_dispatcher_buf =
  let e = ["seq", t_int; "poly_queue", poly_queue.t] in
  create_ds "dispatcher_buf" ~e @@ wrap_tmap @@ t_of_e e

(* we split the buffer for efficiency purposed *)
(* these 2 must be synchronized *)
let nd_dispatcher_bid_buf =
  let e' = ["batch_id", t_vid; "sender_ip", t_int] in
  let e = ["seq", t_int; "inner", t_of_e e'] in
  create_ds "nd_dispatcher_bid_buf" ~e @@ wrap_tmap @@ t_of_e e

(* remember the last number we've seen *)
let nd_dispatcher_last_seq =
  create_ds "nd_dispatcher_last_seq" t_int_mut

(* trigger version of dispatcher. Called by other nodes *)
let trig_dispatcher_trig c =
  mk_code_sink' trig_dispatcher_trig_nm
    ["batch_id", t_vid; "poly_queue", poly_queue.t] [] @@
  mk_block [
    mk_poly_unpack (mk_var "poly_queue");
    clear_poly_queues c;
    mk_apply' trig_dispatcher_nm [mk_var "batch_id"; mk_var "poly_queue"]
  ]

let trig_dispatcher_trig_unique c =
  mk_code_sink' trig_dispatcher_trig_unique_nm
    ["batch_id", t_vid; "poly_queue", poly_queue.t; "upoly_queue", upoly_queue.t] [] @@
  mk_block [
    mk_poly_unpack (mk_var "poly_queue");
    mk_poly_unpack (mk_var "upoly_queue");
    mk_apply' trig_dispatcher_unique_nm
      [mk_var "batch_id"; mk_var "poly_queue"; mk_var "upoly_queue"]
  ]

(* trig dispatcher from switch to node. accepts a msg number telling it how to order messages *)
(* @msg_num: number of consecutive message. Used to order buffers in corrective mode
   so we avoid having too many correctives. *)
let nd_dispatcher_next_seq = create_ds "nd_dispatcher_next_seq" @@ mut t_int

let clear_isobatch_stmt_helper_nm = "clear_isobatch_stmt_helper"
let clear_isobatch_stmt_helper =
  mk_global_fn clear_isobatch_stmt_helper_nm [] [] @@
    mk_block [
      (* replace, clear out the isobatch_map_helper *)
      mk_iter_bitmap' ~idx:stmt_ctr.id
        (mk_insert_at isobatch_stmt_helper_id (mk_var stmt_ctr.id) [mk_empty isobatch_map_inner2.t])
        isobatch_stmt_helper_bitmap_id;
      mk_clear_all isobatch_stmt_helper_bitmap_id;
      mk_assign isobatch_stmt_helper_has_content.id mk_cfalse;
      ]


(* check to see if we have another batch available to send *)
(* used to recurse in nd_from_sw_dispatcher and for getting args from the lm *)
let nd_send_next_batch_if_available =
    (* check if the next num is in the buffer and if we've already received an arg
     * notification for it *)
  let pat_next_seq = [mk_var "seq"; mk_cunknown] in
  mk_global_fn nd_send_next_batch_if_available_nm [] [] @@
    mk_let ["seq"] (mk_var nd_dispatcher_next_seq.id) @@
    mk_let ["do_delete"]
      (mk_case_ns (mk_peek @@ mk_lookup' nd_dispatcher_bid_buf.id pat_next_seq) "x"
         mk_cfalse @@ (* don't delete, no next sequence *)
         mk_let ["batch_id"; "sender_ip"] (mk_snd @@ mk_var "x") @@
          (* are args available for this trigger? *)
          mk_if (mk_is_member' nd_trig_arg_notifications.id @@ mk_var "batch_id")
            (mk_block [
              (* remove from notification set *)
              mk_delete nd_trig_arg_notifications.id [mk_var "batch_id"];
              (* get and delete stored polyqueue *)
              mk_delete_with nd_dispatcher_buf.id pat_next_seq
                (mk_lambda' unit_arg @@ mk_error "missing polyqueue!")
                (* recurse with the sequence *)
                (mk_lambda' nd_dispatcher_buf.e @@
                  mk_send nd_from_sw_trig_dispatcher_trig_nm G.me_var
                    [mk_var "seq"; mk_var "batch_id"; mk_var "sender_ip"; mk_var "poly_queue"]);
              mk_ctrue (* do_delete *)
              ])
            mk_cfalse) @@ (* do nothing. wait for notification *)
      mk_if (mk_var "do_delete")
        (mk_delete nd_dispatcher_bid_buf.id pat_next_seq)
        mk_cunit

(* sw -> nd: fetch/put/docomplete *)
let nd_from_sw_trig_dispatcher_trig c =
  mk_code_sink' nd_from_sw_trig_dispatcher_trig_nm
    ["seq", t_int; "batch_id", t_int; "sender_ip", t_int ; "poly_queue", poly_queue.t] [] @@
  mk_let_block ["is_isobatch"] (is_isobatch_id "batch_id")
  [
    mk_assign nd_dispatcher_next_seq.id @@
      (mk_if_eq (mk_var nd_dispatcher_last_seq.id) (mk_var g_max_int.id)
        (mk_cint 0) @@
        mk_add (mk_var nd_dispatcher_last_seq.id) @@ mk_cint 1);
    (* check if we're contiguous *)
    mk_if_eq (mk_var "seq") (mk_var nd_dispatcher_next_seq.id)
      (* then dispatch right away *)
      (mk_block [
          (* profiling: point of starting to deal with this batch at the node *)
          prof_property prof_tag_node_process @@ ProfLatency("batch_id", "-1");
          (* unpack the polyqueue *)
          mk_poly_unpack (mk_var "poly_queue");
          mk_assign nd_dispatcher_last_seq.id @@ mk_var nd_dispatcher_next_seq.id;
          mk_incr nd_dispatcher_next_seq.id;
          clear_poly_queues c;
          (* buffer the ack to the switch *)
          GC.nd_ack_send_code ~addr_nm:"sender_ip" ~vid_nm:"batch_id";

          mk_if (mk_var "is_isobatch")
            (mk_apply' clear_isobatch_stmt_helper_nm [])
            mk_cunit;

          (* debug_run: disable handling if requested. *)
          debug_run_test_var debug_run_sw_send_all
            ~default:send_poly_queues @@
            mk_apply' trig_dispatcher_nm [mk_var "batch_id"; mk_var "poly_queue"];

          (* check if we're a local master and therefore
             need to notify local peers about new batch trig_args *)
          mk_if_eq (mk_var job.id) (mk_var job_local_master.id)
            (mk_block [
                (* move trig args from buffer to log *)
                lm_move_trig_arg_from_buf c;
                (* notify localpeers that the args have been received *)
                mk_send_all_local_peers nd_rcv_trig_args_notify_nm [mk_var "batch_id"]
              ])
            mk_cunit;

          (* check if we need to move stuff from the isobatch stmt helper *)
          mk_if (mk_and (mk_var "is_isobatch") @@ mk_var isobatch_stmt_helper_has_content.id)
            (mk_apply' move_isobatch_stmt_helper_nm [mk_var "batch_id"])
            mk_cunit;
       ]) @@
      mk_block [
        (* else, stash the poly_queue in our buffer *)
        mk_insert nd_dispatcher_buf.id [mk_var "seq"; mk_var "poly_queue"];
        mk_insert nd_dispatcher_bid_buf.id
          [mk_var "seq"; mk_tuple [mk_var "batch_id"; mk_var "sender_ip"]];
        (* statistic: keep track of stashed number *)
        mk_assign nd_num_stashed.id @@ mk_add (mk_var @@ nd_num_stashed.id) @@ mk_cint 1
      ]
    ;
    (* recurse into next batch if possible *)
    mk_apply' nd_send_next_batch_if_available_nm [];
  ]

let sw_event_driver_isobatch_nm = "sw_event_driver_isobatch"
let sw_event_driver_isobatch c =
  let trig_list = StrSet.of_list @@ P.get_trig_list c.p in
  let tags = List.filter (fun ti ->
      ti.tag_typ = Event && (StrSet.mem ("insert_"^ti.tag) trig_list || ti.tag = "sentinel"))
    c.poly_tags in
  mk_global_fn sw_event_driver_isobatch_nm
    ["batch_id", t_vid; "poly_queue", poly_queue.t] [t_vid] @@
  mk_let ["idx"] (mk_cint 0) @@
  mk_let ["offset"] (mk_cint 0) @@
  mk_let ["tag"] (mk_poly_tag_at (mk_var "poly_queue") @@ mk_cint 0) @@
  List.fold_left (fun acc_code ti ->
      mk_if_eq (mk_var "tag") (mk_cint ti.itag)
        (if ti.tag = "sentinel" then
           mk_block [
             Proto.sw_seen_sentinel ~check_size:false;
             mk_var "batch_id"
           ]
        else
          mk_poly_at_with' ti.tag @@ mk_lambda' ti.args @@
          (* different if we produce deletes *)
          let wrap_check e =
            mk_if (mk_var "do_insert")
              e @@
              mk_apply' (send_fetches_isobatch_name_of_t @@ "delete_"^ti.tag)
                [mk_var "batch_id"; mk_var "poly_queue"]
          in
          let do_insert =
            mk_apply' (send_fetches_isobatch_name_of_t @@ "insert_"^ti.tag)
              [mk_var "batch_id"; mk_var "poly_queue"]
          in
          if c.gen_deletes then wrap_check do_insert else do_insert)
        acc_code)
    (mk_error "mismatch on event id")
    tags

let sw_event_driver_single_vid_nm = "sw_event_driver_single_vid"
let sw_event_driver_single_vid c =
  let trig_list = StrSet.of_list @@ P.get_trig_list c.p in
  mk_global_fn sw_event_driver_single_vid_nm
    ["batch_id", t_vid; "poly_queue", poly_queue.t] [t_vid] @@
  mk_poly_fold
    (mk_lambda4' ["vid", t_vid] p_tag p_idx p_off @@

      (* print trace if requested *)
      do_trace "sed"
        [t_int, mk_var "vid";
        t_int, mk_var "tag";
        t_int, mk_var "idx";
        t_int, mk_var "offset"] @@

        mk_block [
          (* clear the trig send bitmaps for each event *)
          mk_clear_all D.send_trig_header_bitmap.id ;

          List.fold_left (fun acc_code ti ->
            (* check if we match on the id *)
            mk_if_eq (mk_var "tag") (mk_cint ti.itag)
              (if ti.tag = "sentinel" then
                (* don't check size of event queue *)
                Proto.sw_seen_sentinel ~check_size:false
              else
                let send_fetches pref =
                  let ss = P.stmts_of_t c.p @@ pref^ti.tag in
                  (mk_block @@ List.map (fun s ->
                    mk_apply' (send_fetch_single_vid_name_of_t (pref^ti.tag) s) @@
                      ids_to_vars @@ "vid":: (fst_many @@ tl ti.args)) ss)
                in
                mk_poly_at_with' ti.tag @@
                  mk_lambda' ti.args @@
                      mk_if (mk_var "do_insert")
                        (send_fetches "insert_")
                        (if c.gen_deletes then send_fetches "delete_" else mk_cunit))
              acc_code)
          (mk_error "mismatch on event id") @@
          List.filter (fun ti ->
              ti.tag_typ = Event &&
              (StrSet.mem ("insert_"^ti.tag) trig_list || ti.tag = "sentinel"))
            c.poly_tags
        ;
        next_vid @@ mk_var "vid"
      ])
  (mk_var "batch_id") @@
  mk_var "poly_queue"

let clear_send_trig_args_map_nm = "clear_send_trig_args_map"
let clear_send_trig_args_map =
  mk_global_fn clear_send_trig_args_map_nm [] [] @@
  mk_block [
    mk_iter_bitmap'
      (mk_update_at_with send_trig_args_map.id (mk_var "ip") @@
        mk_lambda' send_trig_args_map.e @@
          mk_block [
            mk_clear_all send_trig_args_inner.id;
            mk_var send_trig_args_inner.id
          ])
      send_trig_args_bitmap.id;
    mk_clear_all send_trig_args_bitmap.id;
  ]

(* save the current_batch_id between receiving the token and vector clock *)
let sw_token_has_data = create_ds "sw_token_has_data" @@ mut t_bool
let sw_token_current_batch_id = create_ds "sw_token_current_batch_id" @@ mut t_vid

(* The rcv_token trigger: loop over the event data structures as long as we have spare vids *)
(* We essentially have 2 loops to allow concurrent switch operation: the next_vid loop (token),
   and the vector_clock loop *)
let sw_rcv_token_trig c =
  mk_code_sink' sw_rcv_token_trig_nm
    ["batch_id2", t_vid] [] @@
    (* check if we're done *)
    mk_if (mk_var Proto.sw_all_done.id) mk_cunit @@
    (* convert to isobatch batch id if needed *)
    mk_let_block ["batch_id"]
      (mk_if (mk_var isobatch_mode.id)
         (to_isobatch @@ mk_var "batch_id2") @@
         mk_var "batch_id2")
    [
      mk_assign sw_token_current_batch_id.id @@ mk_var "batch_id";
      mk_assign sw_token_has_data.id mk_cfalse;
      (* if we're initialized and we have stuff to send *)
      mk_if (mk_and (mk_var D.sw_init.id) @@
                    mk_gt (mk_size @@ mk_var D.sw_event_queue.id) @@ mk_cint 0)
        (mk_case_sn (mk_peek @@ mk_var D.sw_event_queue.id) "poly_queue"
          (mk_block [
            mk_poly_unpack @@ mk_var "poly_queue";
            clear_poly_queues c;
            mk_assign sw_token_has_data.id mk_ctrue;

            (* for debugging, sleep if we've been asked to *)
            mk_if (mk_neq (mk_var D.sw_event_driver_sleep.id) @@ mk_cint 0)
              (mk_apply' "usleep" [mk_var D.sw_event_driver_sleep.id])
              mk_cunit;
            (* for profiling, save the vid and time *)
            prof_property prof_tag_pre_send_fetch @@ ProfLatency("batch_id", "-1");

            (* clear out the trig arg map: it's batch-specific *)
            mk_apply' clear_send_trig_args_map_nm [];

            (* calculate the next vid using the size of the poly_queue and send it on *)
            mk_let_block ["next_vid"]
              (mk_mult
                (mk_add (mk_divi (mk_var "batch_id") @@ mk_cint 2) @@
                  mk_size @@ mk_var "poly_queue") @@
                mk_cint 2)
              [
                (* send the token to the next switch for low latency and parallelism *)
                mk_send sw_rcv_token_trig_nm (mk_var TS.sw_next_switch_addr.id) [mk_var "next_vid"];

                (* dispatch the events, preparing the poly_queues *)
                (let e = mk_apply' sw_event_driver_isobatch_nm [mk_var "batch_id"; mk_var "poly_queue"] in
                if not c.gen_single_vid then mk_ignore e
                else
                  mk_ignore @@ mk_if (mk_var isobatch_mode.id) e @@
                      mk_apply' sw_event_driver_single_vid_nm [mk_var "batch_id"; mk_var "poly_queue"]);

                (* finish popping the incoming queue *)
                mk_pop D.sw_event_queue.id;
                (* update highest vid seen *)
                mk_assign TS.sw_highest_vid.id @@ mk_var "next_vid";

                (* move the used poly_queues to the sw_poly_queues so global state
                   doesn't get interrupted until the rcv_vector_clock *)
                mk_clear_all sw_poly_queue_bitmap.id;
                mk_iter_bitmap'
                  (mk_block [
                    mk_let ["pq"] (mk_delete_at poly_queues.id @@ mk_var "ip") @@
                      mk_insert_at sw_poly_queues.id (mk_var "ip") [mk_var "pq"];
                    mk_insert sw_poly_queue_bitmap.id [mk_var "ip"];
                  ])
                 D.poly_queue_bitmap.id;

                (* for profiling, annotate with the last vid seen *)
                prof_property prof_tag_post_send_fetch @@ ProfLatency("next_vid", "-1");
            ]]) @@
          mk_error "oops") @@
        (* otherwise send the same vid *)
        mk_send sw_rcv_token_trig_nm (mk_var TS.sw_next_switch_addr.id) [mk_var "batch_id"]
    ]

(* The vector clock is sent separately after the token, to make sure that the switches
   don't have to wait for each other and can process in parallel *)
let sw_rcv_vector_clock_trig =
  mk_code_sink' sw_rcv_vector_clock_nm
    ["vector_clock2", TS.sw_vector_clock.t] [] @@
    (* check if we're done *)
    mk_if (mk_var Proto.sw_all_done.id) mk_cunit @@
    mk_if (mk_var sw_token_has_data.id)
      (* update the vector clock by bits in the outgoing poly_queues *)
      (* convert to isobatch batch id if needed *)
      (mk_let_block ["vector_clock"]
        (mk_agg_bitmap' ~move:true
          ["acc", TS.sw_vector_clock.t]
          (mk_update_at_with_block "acc" (mk_var "ip") @@
            mk_lambda' ["x", t_int] @@
              (* if we're at max_int, skip to 0 so we don't get negatives *)
              mk_if (mk_eq (mk_var "x") @@ mk_var g_max_int.id)
                (mk_cint 0) @@
                mk_add (mk_cint 1) @@ mk_var "x")
          (mk_var "vector_clock2")
          sw_poly_queue_bitmap.id)
        [
          (* send (move) the outgoing polyqueues *)
          mk_let ["send_count"]
            (mk_agg_bitmap' ["count", t_int]
              (* move and delete the sw_poly_queue and ship it out with the vector clock num
                 pull out the sw_poly_queue *)
                (mk_let_block ["pq"] (mk_delete_at sw_poly_queues.id @@ mk_var "ip")
                  [
                    prof_property 0 @@ ProfSendPoly(sw_token_current_batch_id.id, "ip", "pq");
                    mk_sendi nd_from_sw_trig_dispatcher_trig_nm (mk_var "ip")
                      [mk_at' "vector_clock" @@ mk_var "ip";
                       mk_var sw_token_current_batch_id.id;
                       mk_var D.me_int.id;
                       mk_var "pq"];
                    mk_add (mk_var "count") @@ mk_cint 1
                ])
              (mk_cint 0) @@
              sw_poly_queue_bitmap.id) @@
          (* save the latest ack send info *)
          GC.sw_update_send ~n:(mk_var "send_count") ~vid_nm:sw_token_current_batch_id.id;
          (* send the new (vid, vector clock). make sure it's after we use vector
              clock data so we can move *)
          mk_send sw_rcv_vector_clock_nm (mk_var TS.sw_next_switch_addr.id) [mk_var "vector_clock"];

          (* check if we're done *)
          Proto.sw_check_done ~check_size:true
        ]) @@
      (* if no data, just pass on vector clock *)
      mk_send sw_rcv_vector_clock_nm (mk_var TS.sw_next_switch_addr.id) [mk_var "vector_clock2"]

let sw_sent_demux_ctr = create_ds "sw_sent_demux_ctr" @@ mut t_int
let sw_total_demux_ctr = create_ds "sw_total_demux_ctr" @@ mut t_int
(* 2 poly queues to allow isobatch creation *)
let sw_demux_poly_queues =
  let e = ["inner", D.poly_queue.t] in
  let init = mk_map (mk_lambda' unknown_arg @@ mk_empty D.poly_queue.t) @@
    k3_container_of_list t_bool_vector @@ [mk_cfalse; mk_cfalse] in
  create_ds ~init ~e "sw_demux_poly_queues" @@ wrap_tvector D.poly_queue.t

(* for isobatch creation, keep track of last trigger/action (insert/delete) *)
let sw_demux_last_trig =
  create_ds ~init:(mk_cstring "first") "sw_demux_last_trig" @@ mut t_string
let sw_demux_last_action = create_ds "sw_demux_last_action" @@ mut t_int
(* switch polybuffers for isobatch creation *)
let sw_demux_poly_target = create_ds "sw_demux_poly_target" @@ mut t_int

(* the demux trigger takes the single stream and demuxes it *)
(* it pushes a certain number of events onto the polybuffer queue *)
(* this code is specific to the interpreter version *)
let sw_demux_nm = "sw_demux"
let sw_demux c =
  let combo_t, t_arg_map = D.combine_trig_args c in
  let combo_arr_t = Array.of_list combo_t in
  (* for dates, we need to parse to int *)
  let convert_date i =
    if combo_arr_t.(i).typ = TDate then
      mk_apply' "parse_sql_date" |- singleton
    else id_fn
  in
  let sentinel_code e =
    (* stash the sentinel index in the queue *)
    mk_if (mk_eq (mk_fst @@ mk_var "args") @@ mk_cstring "")
      (mk_block [
          (* switch target *)
          mk_assign sw_demux_poly_target.id @@
            mk_if_eq (mk_var sw_demux_poly_target.id) (mk_cint 0) (mk_cint 1) (mk_cint 0);
          mk_update_at_with sw_demux_poly_queues.id (mk_var sw_demux_poly_target.id) @@
            mk_lambda' sw_demux_poly_queues.e @@
              mk_poly_insert_block "sentinel" "inner" [];
          mk_tuple [mk_ctrue; mk_ctrue; mk_ctrue]
        ])
      e
  in
  mk_code_sink' sw_demux_nm ["args", wrap_ttuple @@ List.map str_of_date_t combo_t] [] @@
  (* create a poly queue with a given number of messages *)
    mk_let ["trig_id"] (mk_fst @@ mk_var "args") @@
    mk_let ["action"]  (mk_snd @@ mk_var "args") @@
    mk_let_block ["do_incr"; "do_send"; "force_split"]
      (sentinel_code @@
        (* check if this matches our switch id *)
       mk_if
          (mk_neq
            (mk_apply' "mod" [mk_var sw_total_demux_ctr.id; mk_var num_switches.id]) @@
            mk_var D.sw_csv_index.id)
          (* if we don't match on the mod, don't insert but increment *)
          (mk_tuple [mk_ctrue; mk_cfalse; mk_cfalse]) @@
          (* else, we match *)
          StrMap.fold (fun trig arg_indices acc_code ->
            let args =
              (* add 1 for tuple access *)
              List.map (fun i -> convert_date i @@ mk_subscript (i+1) @@ mk_var "args") arg_indices
            in
            (* check if we match on the trig id *)
            mk_if_eq (mk_var "trig_id") (mk_cstring trig)
              (mk_let ["do_insert"] (mk_eq (mk_var "action") @@ mk_cint 1) @@
               mk_if (mk_and (mk_var isobatch_mode.id) @@
                       mk_and (mk_neq (mk_var sw_demux_last_trig.id) @@ mk_cstring "first") @@
                         mk_or (mk_neq (mk_var "trig_id") @@ mk_var sw_demux_last_trig.id) @@
                           mk_neq (mk_var "action")  @@ mk_var sw_demux_last_action.id)
                (mk_block [
                    (* alternate to other buffer *)
                    mk_assign sw_demux_poly_target.id @@
                      mk_if_eq (mk_var sw_demux_poly_target.id) (mk_cint 0) (mk_cint 1) (mk_cint 0);
                    mk_update_at_with sw_demux_poly_queues.id (mk_var sw_demux_poly_target.id) @@
                      mk_lambda' sw_demux_poly_queues.e @@
                        mk_poly_insert_block trig "inner" @@ (mk_var "do_insert")::args;
                    (* update memory *)
                    mk_assign sw_demux_last_trig.id @@ mk_var "trig_id";
                    mk_assign sw_demux_last_action.id @@ mk_var "action";
                    mk_tuple [mk_ctrue; mk_ctrue; mk_ctrue]
                 ]) @@
                (* stay in current buffer *)
                 mk_block [
                  mk_assign sw_demux_last_trig.id @@ mk_var "trig_id";
                  mk_assign sw_demux_last_action.id @@ mk_var "action";
                  mk_update_at_with sw_demux_poly_queues.id (mk_var sw_demux_poly_target.id) @@
                    mk_lambda' sw_demux_poly_queues.e @@
                      mk_poly_insert_block trig "inner" @@ (mk_var "do_insert")::args;
                  mk_tuple [mk_ctrue; mk_ctrue; mk_cfalse]
                ]) @@
              acc_code)
            t_arg_map @@
            mk_error "unrecognized trigger") [
    (* increment counter if we saw a valid trigger *)
    mk_if (mk_var "do_incr") (mk_incr sw_total_demux_ctr.id) mk_cunit;
    mk_if (mk_var "do_send") (mk_incr sw_sent_demux_ctr.id) mk_cunit;
    (* ship the other buffer if we hit the count or got a split *)
    mk_if (mk_or (mk_eq (mk_cint 0) @@
                    mk_apply' "mod" [mk_var sw_sent_demux_ctr.id; mk_var D.sw_poly_batch_size.id]) @@
                  mk_var "force_split")
      (mk_let_block ["target"]
        (* alternate target if force_split *)
        (mk_if (mk_var "force_split")
            (mk_if_eq (mk_var sw_demux_poly_target.id) (mk_cint 0) (mk_cint 1) @@ mk_cint 0) @@
            (* else, same target *)
            mk_var sw_demux_poly_target.id) [
         (* check that we have some content *)
         mk_if (mk_at_with' sw_demux_poly_queues.id (mk_var "target") @@
                  mk_lambda' sw_demux_poly_queues.e @@
                    mk_neq (mk_size @@ mk_var "inner") @@ mk_cint 0)
          (mk_block [
            (* copy the polybuffer to the queue *)
            mk_insert sw_event_queue.id [mk_delete_at sw_demux_poly_queues.id (mk_var "target")];
            (* insert an empty poly_queue *)
            mk_insert_at sw_demux_poly_queues.id (mk_var "target") [mk_empty D.poly_queue.t];
           ]) mk_cunit;
          (* check for sentinel, in which case we need to send the new buffer as well *)
          mk_if_eq (mk_var "trig_id") (mk_cstring "")
            (mk_block [
              (* copy the extra sentinel polybuffer to the queue *)
              mk_insert sw_event_queue.id [mk_delete_at sw_demux_poly_queues.id @@ mk_var sw_demux_poly_target.id];
              (* insert an empty poly_queue *)
              mk_insert_at sw_demux_poly_queues.id (mk_var sw_demux_poly_target.id) [mk_empty D.poly_queue.t];
              ]
            )
            mk_cunit
        ])
      mk_cunit
  ]

(* demuxer that supports an eventq input *)
(* project out the stuff we don't use *)
let sw_demux_poly_nm = "sw_demux_poly"
let sw_demux_poly c =
  let event_trigs = P.get_trig_list c.p ~corrective:false ~delete:false ~sys_init:false in
  mk_code_sink' sw_demux_poly_nm ["poly_queue", D.poly_event_queue.t] [] @@
  mk_let ["acc"]
    (mk_poly_fold
      (mk_lambda4' ["acc", D.poly_queue.t] p_tag p_idx p_off @@
       (* tracing if requested *)
       do_trace "demux"
         [t_int, mk_var "tag";
          t_int, mk_var "idx";
          t_int, mk_var "offset"] @@
       List.fold_left (fun acc_code t ->
           let wide_args = ("do_insert", t_bool)::P.args_of_t c.p t in
           let args = ("do_insert", t_bool)::args_of_t c t in
           let stag = str_drop (String.length "insert_") t in
           let itag = fst @@ List.find (fun (_,(s,_)) -> stag = s) c.event_tags in
           mk_if (mk_eq (mk_var "tag") @@ mk_cint itag)
             (* take a copy hit here because we can't write to closure *)
             (mk_let (fst_many wide_args) (mk_poly_at' stag) @@
              mk_poly_insert_block stag "acc" @@ ids_to_vars @@ fst_many args)
             acc_code)
         (* base condition *)
         (mk_if (mk_eq (mk_var "tag") @@ mk_cint 0)
            (mk_poly_insert_block "sentinel" "acc" [mk_cunit]) @@
            mk_error "unrecognized tag") @@
         event_trigs)
      (mk_var D.empty_poly_queue.id) @@
     mk_var "poly_queue") @@

    mk_block [
      (* add the poly queue to our queue of queues *)
      mk_insert sw_event_queue.id [mk_var "acc"]
    ]

let functions = [
  clear_send_trig_args_map;
  clear_isobatch_stmt_helper
]

