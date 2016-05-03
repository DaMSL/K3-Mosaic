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
module M = ModifyAst

open GenPush
open GenCommon
open GenCorrective

(* Receive Put trigger
 * --------------------------------------- *
 * Update the statement counters with the received values
 * If the counter is 0, we need to call do_complete
 * - One of the triggers fed from the poly queue
 *)
let nd_rcv_put_single_vid_trig c t s =
  let fn_name = rcv_put_single_vid_name_of_t t s in
  mk_global_fn fn_name
    (D.nd_rcv_put_args c t) (* also pull inner ds *)
    [] @@ mk_block
    [
      mk_if
        (mk_apply' nd_check_stmt_cntr_index_nm @@
          (* false: no data is being sent *)
          [mk_var "vid"; mk_cint s; mk_var "count2"; mk_cfalse])
        (mk_apply'
          (do_complete_name_of_t t s) @@ mk_cfalse::args_of_t_as_vars_with_v c t) @@
        mk_cunit
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
            (mk_apply' move_isobatch_stmt_helper_nm [mk_var "batch_id"])
            mk_cunit;

          mk_at_with' isobatch_vid_map_id (mk_cint s) @@
            mk_lambda' isobatch_vid_map_e @@
              mk_case_ns (mk_lookup' "inner" [mk_var "batch_id"; mk_cunknown]) "vids"
                (mk_error "missing batch id") @@
                (* iterate over all the vids in this batch and complete them *)
                mk_iter
                  (mk_lambda' ["vid", t_vid] @@
                    (if args <> [] then
                      mk_let (fst_many args)
                        (mk_apply' (nd_log_get_bound_for t) [mk_var "vid"])
                    else id_fn) @@
                      mk_apply' (do_complete_name_of_t t s) @@
                        mk_ctrue::args_of_t_as_vars_with_v c t) @@
                  mk_snd @@ mk_var "vids";

          (* do stmt_cntr_check, exec_buffered_fetches for the batch *)
          mk_apply' nd_complete_stmt_cntr_check_nm [mk_var "batch_id"; mk_cint s]
        ]) @@
        mk_cunit
    ]

(* receive isobatch push: load the vids for the batch id and run them if needed *)
let nd_rcv_push_isobatch_trig c t s =
  let fn_name = rcv_push_isobatch_name_of_t t s in
  let args = D.args_of_t c t in
  mk_global_fn fn_name ["count", t_int; "batch_id", t_vid] [] @@
    (* lookup the batch in the isobatch_map. true: has_data *)
    mk_if
      (mk_apply' nd_check_stmt_cntr_index_nm @@
        [mk_var "batch_id"; mk_cint s; mk_neg @@ mk_var "count"; mk_ctrue])
      (* get out the list of stmts to run *)
      (mk_at_with' isobatch_vid_map_id (mk_cint s) @@
        mk_lambda' isobatch_vid_map_e @@
          mk_case_ns (mk_lookup' "inner" [mk_var "batch_id"; mk_cunknown]) "vids"
            (mk_error "missing batch id") @@
            mk_block [
              mk_iter
                (mk_lambda' ["vid", t_vid] @@
                  (* apply local do_complete *)
                  (if args <> [] then
                    mk_let (fst_many args)
                      (mk_apply' (nd_log_get_bound_for t) [mk_var "vid"])
                    else id_fn) @@
                  mk_apply' (do_complete_name_of_t t s) @@
                    mk_ctrue::args_of_t_as_vars_with_v c t) @@
                mk_snd @@ mk_var "vids";
              (* do stmt_cntr_check, exec_buffered_fetches for the batch *)
              mk_apply' nd_complete_stmt_cntr_check_nm [mk_var "batch_id"; mk_cint s]
            ])
      mk_cunit

(* rcv_push_trig
 * --------------------------------------
 * Receive a push at a node
 * Also called for virtual pushes: local data transfers to allow tracking of
 * present data w/ counters params can be moved to the put statement, but it's
 * a good reminder to have it here
 * We write to specific buffer maps to prevent mixing of buffer and non-buffer
 * data, which can cause confusion when the time comes to compute.
 * A later optimization could be lumping maps between statements in a trigger,
 * which can be done on a rmap to lmap with binding basis (like shuffles). Care must be
 * paid to the corrective updates in this case, which are statement-specific *)
let nd_rcv_push_trig c t s =
    let fn_name = rcv_push_name_of_t t s in
    let args = args_of_t c t in
    mk_global_fn fn_name D.nd_rcv_push_args [] @@
    mk_block [
      (* inserting into the map has been moved to the aggregated version *)

      (* update and check statment counters to see if we should send a do_complete *)
      mk_if
        (mk_apply' nd_check_stmt_cntr_index_nm @@
          [mk_var "vid"; mk_cint s; mk_neg @@ mk_var "count"; mk_var "has_data"])
        (* apply local do_complete *)
        ((if args <> [] then
          mk_let
            (fst_many @@ D.args_of_t c t)
            (mk_apply'
              (nd_log_get_bound_for t) [mk_var "vid"])
          else id_fn) @@
         mk_apply' (do_complete_name_of_t t s) @@
          mk_cfalse::args_of_t_as_vars_with_v c t)
        mk_cunit
    ]

(* function versions of do_complete *)
let nd_do_complete_fns c ast trig_name =
  (* @has_rhs: whether this statement has rhs maps *)
  let do_complete_fn has_rhs stmt_id =
    mk_global_fn (do_complete_name_of_t trig_name stmt_id)
      (("is_isobatch", t_bool)::args_of_t_with_v c trig_name) [] @@
    let lmap = P.lhs_map_of_stmt c.p stmt_id in
    let fst_hop, snd_hop = mk_cint 1, mk_cint 2 in
    let delta = "delta_vals" in
    let is_col, ast = M.modify_ast c ast stmt_id trig_name in

    (* if we have no rhs maps, do nothing *)
    let no_corr_actions =
      if not has_rhs then mk_cunit
      else
        mk_if (mk_var "is_isobatch")
          mk_cunit @@
          mk_apply' nd_complete_stmt_cntr_check_nm [mk_var "vid"; mk_cint stmt_id]
    in

    (* check for complicated double loop vars which necessitate lmap filtering
     * if we have loop vars, we need to filter the lmap values here
     *)
    let_lmap_filtering c delta stmt_id lmap
      ast
      (mk_block [
        (* add delta *)
        do_add_delta c (mk_var delta) lmap ~corrective:false;

        (* for profiling: mark with tag for do_complete done, but only for non-isobatch *)
        prof_property prof_tag_do_complete_done @@ ProfLatency("vid", soi stmt_id);

        if c.gen_correctives && List.mem lmap c.corr_maps
        then
          let send_corr_t = send_corrective_name_of_t c lmap in
          mk_let ["sent_msgs"]
            (* we apply send_correctives with our original address, stmt_id, original vid
             * and hop + 1. We double up on vid since send_correctives is also called for
             * do_corrective which must send the new vid to be calculated as well as the
             * original complete's vid
             *)
            (mk_if
              (* don't do correctives in no-corrective mode *)
              (mk_var D.corrective_mode.id)
              (mk_apply' send_corr_t @@
                [mk_var D.me_int.id; mk_cint stmt_id; mk_var "vid"; snd_hop; mk_var "vid"; mk_var delta]) @@
              mk_cint 0) @@
            mk_if (mk_eq (mk_var "sent_msgs") @@ mk_cint 0)
              (if has_rhs then
                 mk_if (mk_var "is_isobatch")
                   mk_cunit @@
                   (* if our sent_msgs is 0, we need to delete the stmt cntr entry *)
                   mk_apply' nd_complete_stmt_cntr_check_nm [mk_var "vid"; mk_cint stmt_id]
                else
                  (* no rhs maps = no need to update anything, no stmt cntr entry *)
                  mk_cunit) @@
              (* otherwise we need to update the corrective counters *)
              (* update the corrective counters for hop 1 to the number of msgs. *)
              mk_ignore @@ mk_apply' nd_update_corr_map_nm @@
                [mk_var "vid"; mk_cint stmt_id; fst_hop; mk_var "sent_msgs"]
        (* no correctives are possible *)
        else no_corr_actions
      ])
      (* no valid data: do nothing *)
      ~alt:no_corr_actions
in
(List.map (do_complete_fn true)  @@ P.stmts_with_rhs_maps_in_t c.p trig_name) @
(List.map (do_complete_fn false) @@ P.stmts_without_rhs_maps_in_t c.p trig_name)

(* call from do_complete when done to check if fully done *)
let nd_complete_stmt_cntr_check c =
  mk_global_fn nd_complete_stmt_cntr_check_nm ["vid", t_vid; "stmt_id", t_stmt_id] [] @@
  (* if we have nothing to send, we can delete our stmt_cntr entry right away *)
  mk_block @@
    [mk_update_at_with nd_stmt_cntrs_id (mk_var "stmt_id") @@
      mk_lambda' nd_stmt_cntrs_e @@
        mk_delete_block nd_stmt_cntrs_inner.id [mk_var "vid"; mk_cunknown];
      mk_decr nd_stmt_cntr_size.id] @
    (* if we're in no-corrective mode, we need to execute batched fetches *)
    (if c.corr_maps = [] then [] else singleton @@
      mk_if (mk_var D.corrective_mode.id)
        mk_cunit @@
        mk_apply' nd_exec_buffered_fetches_nm [mk_var "vid"; mk_var "stmt_id"]) @
    (* check if we're done *)
    [Proto.nd_post_delete_stmt_cntr c]

(* trigger versions of do_complete: only for stmts with no rhs maps *)
let nd_do_complete_trigs c t s =
    let comp_nm = do_complete_name_of_t t s in
    let args = nd_do_complete_trig_args c t in
    mk_global_fn (comp_nm^"_trig") args [] @@
      mk_block [
        (* false: not isobatch. not relevant since no rhs maps *)
        mk_apply' comp_nm @@ mk_cfalse :: (ids_to_vars @@ fst_many @@ args);
      ]

