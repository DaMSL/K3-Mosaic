open Util
open K3.AST
open K3Helpers
open K3Dist

module D = K3Dist
module C = GenCommon
module G = K3Global
module U = K3Util
module T = K3Typechecker
module R = K3Route
module P = ProgInfo
module K3R = K3Route
module K3S = K3Shuffle
module K3N = K3NewPrint

open GenPush
open GenCommon

(* ----- Receive trigger args ----- *)

(* trigger_id -> bool *)
(* index for quickly copying the correct trig arg buffers *)
let lm_rcv_args_bitmap = create_ds "lm_rcv_args_bitmap" t_bitset

(* Save the trigger arguments in a temporary buffer.
 * This allows us to reduce contention for the shared log ds *)
let lm_save_arg_trig c t =
  let fn_nm = trig_save_arg_name_of_t t in
  let t_args = trig_save_arg_args c t in
  mk_global_fn fn_nm t_args [] @@
    (* save the bound args for this vid *)
    mk_block [
      mk_apply' (lm_log_write_for c t) @@ args_of_t_as_vars_with_v c t;
      (* mark as having received arguments *)
      mk_insert lm_rcv_args_bitmap.id [mk_cint @@ P.trigger_id_for_name c.p t]
    ]

(* copy trig args from the buffer to the log ds *)
(* handle all trigger possiblities *)
let lm_move_trig_arg_from_buf c =
    (* always access at 0 -- just for move *)
  let trigs = P.get_trig_list ~sys_init:true ~delete:c.gen_deletes c.p in
  let gen_move t =
    let t_id = P.trigger_id_for_name c.p t in
    let log_buf = log_buffer_for c t in
    (* check for this trigger in the bitmap *)
    mk_if (mk_is_member' lm_rcv_args_bitmap.id @@ mk_cint t_id)
      (mk_block [
       (* move buf inner ds into log *)
       mk_let ["inner"] (mk_delete_at log_buf.id @@ mk_cint 0) @@
         (* insert an indirection to the moved inner ds *)
         mk_insert (lm_log_for_t t) [mk_var "batch_id"; mk_ind @@ mk_var "inner"];
       (* replace empty inner ds *)
       mk_insert_at log_buf.id (mk_cint 0) [mk_empty log_buf.t]
      ])
      mk_cunit
  in
  mk_block @@
    List.map gen_move trigs @
    [mk_clear_all lm_rcv_args_bitmap.id]

let nd_rcv_trig_args_notify_nm = "nd_rcv_trig_args_notify"
let nd_rcv_trig_args_notify =
  mk_code_sink' nd_rcv_trig_args_notify_nm
    ["batch_id", t_vid] [] @@
  mk_block [
    (* set in ds *)
    mk_insert D.nd_trig_arg_notifications.id [mk_var "batch_id"];
    (* check if we've already received a batch for this, and if so, handle it *)
    mk_apply' D.nd_send_buffered_batch_if_available_nm [mk_var "batch_id"];
  ]

(* ----- Receive fetch ----- *)

(* trigger_rcv_fetch_isobatch
 * -----------------------------------------
 * Receive a fetch at a node.
 * Reuses switch-side computation of which maps this node should read.
 * This could be done entirely at the node, but would repeat work done
 * at the switch anyway.
 * The assumption is that the "stmts_and_map_id" data is not large.
 * We have this as a multiplexer to the different fetch functions, because we
 * need to record only one trigger in the log, and because it reduces messages
 * between nodes.
 *)

(* send the push *)
let nd_rcv_fetch_isobatch_do_push_nm t s = sp "nd_%s_%d_rcv_fetch_isobatch_do_push" t s
let nd_rcv_fetch_isobatch_do_push c t s =
  let args = args_of_t_with_v c t in
  let rmaps = P.rhs_maps_of_stmt c.p s in
  mk_global_fn (nd_rcv_fetch_isobatch_do_push_nm t s)
    (["map_id", t_map_id]@args) [] @@
  mk_block [
    (* TODO: check if this is ok *)
    mk_apply' nd_log_master_write_nm @@ [mk_cint s; mk_var "vid"];
    (* apply all the per-map pushes *)
    List.fold_right
      (fun m acc_code2 ->
        mk_if_eq (mk_var "map_id") (mk_cint m)
          (mk_apply' (send_push_isobatch_name_of_t c t s m) @@
            (* @buffered: don't force a t header *)
            mk_cfalse::args_of_t_as_vars_with_v c t)
        acc_code2)
      rmaps
      (mk_error "nd_rcv_fetch: invalid map id")
    ]

(* buffer the isobatch push into the temporary helper ds. This is to prevent repeated hashtable access *)
let nd_rcv_fetch_isobatch_buffer_push_nm = "nd_rcv_fetch_isobatch_buffer_push"
let nd_rcv_fetch_isobatch_buffer_push =
  mk_global_fn nd_rcv_fetch_isobatch_buffer_push_nm
    ["vid", t_vid; "stmt_map_id", t_int] [] @@
    mk_block [
      mk_update_at_with isobatch_buffered_fetch_helper_id
        (mk_var "stmt_map_id") @@
        mk_lambda' isobatch_buffered_fetch_helper_e @@
          mk_insert_block "inner2" [mk_var "vid"]
    ]

(* whether we have an isobatch decision on push/buffer *)
(* {stmt_map_id} *)
let rcv_fetch_isobatch_bitmap_id = "rcv_fetch_isobatch_bitmap"
let rcv_fetch_isobatch_bitmap c = create_ds rcv_fetch_isobatch_bitmap_id t_bitset

(* the actual decision on buffer=true/push=false *)
(* {stmt_map_id} *)
let rcv_fetch_isobatch_decision_bitmap_id = "rcv_fetch_isobatch_decision"
let rcv_fetch_isobatch_decision_bitmap c =
  create_ds rcv_fetch_isobatch_decision_bitmap_id t_bitset

let clear_buffered_fetch_helper_nm = "clear_buffered_fetch_helper"
let clear_buffered_fetch_helper =
  mk_global_fn clear_buffered_fetch_helper_nm [] [] @@
  mk_block [
    mk_iter_bitmap' ~idx:stmt_ctr.id
      (mk_insert_at isobatch_buffered_fetch_helper_id (mk_var stmt_ctr.id)
         [mk_empty isobatch_map_inner2.t])
      rcv_fetch_isobatch_decision_bitmap_id;
    mk_clear_all rcv_fetch_isobatch_bitmap_id;
    mk_clear_all rcv_fetch_isobatch_decision_bitmap_id;
  ]

(* If we need to buffer the fetches, the ones that apply to us as a node
 * are very specific and are routed from the send_fetch node. We need to
 * preserve the exact vids that are requested to be sent for this stmt_map
 * so that we can reproduce it when we can finally send the desired push.
 *)
let nd_rcv_fetch_isobatch_trig c t s =
  let fn_name = rcv_fetch_isobatch_name_of_t t s in
  let trig_id = P.trigger_id_for_name c.p t in
  let rmaps = P.rhs_maps_of_stmt c.p s in
  let m_tags = List.filter (fun ti -> str_suffix "_id" ti.tag && ti.tag_typ = Ds false) c.poly_tags in
  let s_ms = P.stmt_map_ids c.p in
  let rmap_tags = filter_map (fun ti ->
      let m = P.map_id_of_name c.p @@ str_drop_end (String.length "_id") ti.tag in
      if not @@ List.mem m rmaps then None else
        let s_m = fst @@ List.find (fun (_,(s2,m2)) -> s2 = s && m2 = m) s_ms in
        Some(m, s_m, ti.itag, ti.tag)) m_tags
  in
  let var_args = args_of_t_as_vars_with_v c t in
  mk_global_fn fn_name
    (poly_args @ ["batch_id", t_vid] @ D.nd_rcv_fetch_args c t) (* stmt_map_ids are an inner ds *)
    [t_int; t_int] @@
    (* skip over the function tag *)
    mk_poly_skip_block fn_name [

      (* TODO: remove duplication. for profiling: mark the rcv fetch here *)
      prof_property prof_tag_rcv_fetch @@ ProfLatency("vid", soi trig_id);

      (* iterate over the buffered map_id data *)
      mk_poly_iter' @@
        mk_lambda3' p_tag p_idx p_off @@
          (* translate tag to map_id, and skip this tag *)
        mk_let ["map_id"; "stmt_map_id"; "idx"; "offset"]
            (List.fold_left (fun acc_code (m, s_m, itag, stag) ->
                mk_if_eq (mk_var "tag") (mk_cint itag)
                  (mk_poly_skip_block stag
                     [mk_tuple [mk_cint m; mk_cint s_m; mk_var "idx"; mk_var "offset"]])
                  acc_code)
                (mk_tuple [mk_cint (-1); mk_cint (-1); mk_var "idx"; mk_var "offset"])
                rmap_tags) @@
          (* check for termination *)
          mk_if_eq (mk_var "map_id") (mk_cint (-1)) (mk_tuple [mk_var "idx"; mk_var "offset"]) @@
            mk_block [
              mk_if
                (mk_var D.corrective_mode.id)
                (* then send the push right now *)
                (mk_apply' (nd_rcv_fetch_isobatch_do_push_nm t s) @@ [mk_var "map_id"]@var_args) @@
                (* check if we have a decision already made for this map *)
                mk_let ["made_decision"]
                  (mk_is_member' rcv_fetch_isobatch_bitmap_id @@ mk_var "stmt_map_id") @@
                mk_if (mk_var "made_decision")
                  (mk_let ["do_buffer"]
                    (mk_is_member' rcv_fetch_isobatch_decision_bitmap_id @@ mk_var "stmt_map_id") @@
                  (* else - no decision has been made, so make one *)
                  mk_if (mk_var "do_buffer")
                    (* buffer the push *)
                    (mk_apply' (nd_rcv_fetch_isobatch_buffer_push_nm) [mk_var "vid"; mk_var "stmt_map_id"]) @@
                    (* send the push right now *)
                     mk_apply' (nd_rcv_fetch_isobatch_do_push_nm t s) @@ [mk_var "map_id"]@var_args) @@
                  (* we need to make a decision regarding this map *)
                  mk_let_block ["do_buffer"]
                    (* check if the minimum entry in the per_map_stmt_cntrs has a > vid
                     * we *CAN'T* read at the same vid since that's an inner batch conflict *)
                    (mk_gt (mk_var "batch_id") @@
                      mk_at_with' D.nd_stmt_cntrs_per_map_id (mk_var "map_id") @@
                        mk_lambda' nd_stmt_cntrs_per_map_e @@
                          mk_min_with (mk_var "inner")
                            (* if empty, return max *)
                            (mk_lambda'' unit_arg @@ mk_var g_max_vid.id) @@
                            mk_lambda' nd_stmt_cntrs_per_map_inner.e @@ mk_var "vid")
                  [
                    (* save the decision *)
                    mk_insert rcv_fetch_isobatch_bitmap_id [mk_var "stmt_map_id"];
                    mk_if (mk_var "do_buffer")
                      (mk_insert rcv_fetch_isobatch_decision_bitmap_id [mk_var "stmt_map_id"])
                      mk_cunit;
                    (* act on the decision and save into the helper *)
                    mk_if (mk_var "do_buffer")
                      (* write a buffer decision *)
                      (mk_block [
                        mk_update_at_with nd_fetch_buffer_id (mk_var "map_id") @@
                          mk_lambda' nd_fetch_buffer_e @@
                            mk_upsert_with_block "inner" [mk_var "batch_id"; mk_cunknown]
                              (mk_lambda'' unit_arg @@ mk_tuple
                                [mk_var "batch_id"; mk_singleton D.nd_fetch_buffer_inner2.t [mk_var "stmt_map_id"]]) @@
                              mk_lambda' nd_fetch_buffer_inner.e @@
                                mk_block [
                                  mk_insert "stmt_map_ids" [mk_var "stmt_map_id"];
                                  tup_of_e nd_fetch_buffer_inner.e
                                ];
                        (* buffer into temporary helper ds *)
                        mk_apply' (nd_rcv_fetch_isobatch_buffer_push_nm) [mk_var "vid"; mk_var "stmt_map_id"]
                      ])
                      (mk_apply' (nd_rcv_fetch_isobatch_do_push_nm t s) @@ [mk_var "map_id"]@var_args)
                  ];
                  mk_tuple [mk_var "idx"; mk_var "offset"]
            ]
    ]

(* ----- Receive Put ----- *)

(* receive isobatch stmt list -- targeted by tag from a dispatcher *)
(* we buffer the vid/stmts in the helper ds *)
let nd_rcv_stmt_isobatch =
  mk_global_fn nd_rcv_stmt_isobatch_nm ["stmt_id", t_stmt_id; "vid", t_vid] [] @@
  mk_block [
      mk_assign isobatch_stmt_helper_has_content.id mk_ctrue;
      mk_insert isobatch_stmt_helper_bitmap_id [mk_var "stmt_id"];
      mk_update_at_with isobatch_stmt_helper_id (mk_var "stmt_id") @@
        mk_lambda' isobatch_stmt_helper_e @@
          mk_insert_block "inner2" [mk_var "vid"]
  ]


(* Receive Put Isobatch trigger
 * --------------------------------------- *
 * Update the statement counters with the received values
 * If the counter is 0, we need to call do_complete
 * - One of the triggers fed from the poly queue
 *)
let nd_rcv_put_isobatch_trig c t s =
  let fn_name = rcv_put_isobatch_name_of_t t s in
  let args = D.args_of_t c t in
  mk_global_fn fn_name
    (D.nd_rcv_put_isobatch_args c t) (* also pull inner ds *)
    [] @@ mk_block
    [
      mk_if
        (mk_apply' nd_check_stmt_cntr_index_nm
          (* false: no data is being sent *)
          [mk_var "batch_id"; mk_cint s; mk_var "count2"; mk_cfalse])
        (mk_block [
          (* move stmt helper content to main ds first if needed
              (if the pushes arrived first) *)
          mk_if (mk_var isobatch_stmt_helper_has_content.id)
            (mk_apply' D.move_isobatch_stmt_helper_nm [mk_var "batch_id"])
            mk_cunit;

          mk_at_with' isobatch_vid_map_id (mk_cint s) @@
            mk_lambda' isobatch_vid_map_e @@
              mk_case_ns (mk_lookup' "inner" [mk_var "batch_id"; mk_cunknown]) "vids"
                (mk_error "missing batch id") @@
                (* iterate over all the vids in this batch and complete them *)
                mk_iter
                  (mk_lambda' ["vid", t_vid] @@
                    (if args = [] then id_fn else nd_log_get_bound c t) @@
                      mk_apply' (do_complete_name_of_t t s) @@
                        mk_ctrue::args_of_t_as_vars_with_v c t) @@
                  mk_snd @@ mk_var "vids";

          (* do stmt_cntr_check, exec_buffered_fetches for the batch *)
          mk_apply' nd_complete_stmt_cntr_check_nm [mk_var "batch_id"; mk_cint s]
        ]) @@
        mk_cunit
    ]
