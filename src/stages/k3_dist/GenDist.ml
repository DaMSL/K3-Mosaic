(* Functions that take K3 code and generate distributed K3 code *)

(* Assumptions:
 * We assume a rhs map can only occur once per statement *)

(* Notes:
 * We currently carry the vid inside tuples all the way *)


(* Call Graph *
 * on_insert_<trigger>_switch
 *    on_put_<trigger>
 *    do_complete_<trigger> (* no maps *)
 *    on_insert_<trigger>_fetch
 *        <trigger>_fetch_<rhs_map> ->
 *            <trigger>_push_<rhs_map> ->
 *                get_completed_stmts
 *                do_complete_<trigger>_<stmt>
 *                    get_corrective_list
 *                    On_corrective_<trigger>_<delta_rhs_map>
 *                        get_completed_stmts_corrective
 *                        do_complete_corrective_<trigger>_<stmt>_<delta_rhs_map>
 *                            On_corrective_<trigger>_<delta_rhs_map>
 *                            ...
 *
 * Ordering invariant: the arrival of packets between a pair of nodes must be
 * ordered in one instance: correctives must arrive after the original push of
 * read data. Otherwise, the corrective will be erased.
 *
 * Protocol Description:
 * ---------------------
 * - Every put packet from a switch to a node must be acked. However, this can be done
 *   asynchronously since it's only needed for GC. A GC initiation request from master
 *   leads to each node sending the latest processed vid to the switch that sent him said
 *   vid. This reduces bandwidth requirements greatly.
 * - For GC purposes, we must keep track of when a vid and its resulting correctives are done.
 *   Again, because it's only needed for GC, at master's GC initiation, all nodes send the nodes
 *   that sent them correctives numbers signifying how many nodes they passed on correctives to.
 *   When receiving said message, the originating node deducts one for every received message. The
 *   result should be 0 before we count this vid as having fully completed and being ready for GC.
 * - In termination mode, we do the acks for GC right away.
 *)

open Util
open K3.AST
open K3Helpers
open K3Dist

module D = K3Dist
module G = K3Global
module M = ModifyAst
module U = K3Util
module GC = GarbageCollection
module T = K3Typechecker
module R = K3Route
module P = ProgInfo
module TS = Timestamp
module Proto = Protocol

let str_of_date_t t = match t.typ with
  | TDate -> {t with typ = TString}
  | x -> t

(**** global functions ****)

(* write to master log *)
(* TODO: make into ordered map *)
let nd_log_master_write_nm = "nd_log_master_write"
let nd_log_master_write =
  mk_global_fn nd_log_master_write_nm (ds_e D.nd_log_master) [] @@
    mk_insert D.nd_log_master.id @@ ids_to_vars @@ fst_many @@ ds_e D.nd_log_master

(* log_write - save the trigger's arguments *)
(* TODO: make into map, and reduce args for space *)
let nd_log_write_for p trig_nm = "nd_log_write_"^trig_nm (* varies with bound *)
let nd_log_write c t =
  mk_global_fn (nd_log_write_for c.p t) (args_of_t_with_v c t) [] @@
  (* write bound to trigger_specific log *)
  mk_insert (D.nd_log_for_t t) [mk_var "vid"; mk_tuple @@ args_of_t_as_vars c t]

(* get bound args *)
(* TODO: make into map *)
let nd_log_get_bound_for trig_nm = "nd_log_get_bound_"^trig_nm
let nd_log_get_bound c t =
  (* create a pattern for selecting vid alone *)
  let pat = [mk_var "vid"; mk_cunknown] in
  mk_global_fn (nd_log_get_bound_for t)
    ["vid", t_vid]
    (arg_types_of_t c t) @@
    mk_snd @@ mk_peek_or_error "failed to find log" @@
      mk_slice' (D.nd_log_for_t t) pat

(* log_read_geq -- get list of (t, vid) >= vid2 *)
(* TODO: make ordered or some other DS *)
let nd_log_read_geq_nm = "nd_log_read_geq"
let nd_log_read_geq =
  mk_global_fn nd_log_read_geq_nm
  ["vid2", t_vid]
  [nd_log_master.t] @@
  mk_filter
    (* get only >= vids *)
    (mk_lambda' (ds_e nd_log_master) @@ mk_geq (mk_var "vid") @@ mk_var "vid2") @@
    mk_var nd_log_master.id

(* function to check to see if we should execute a do_complete *)
(* params: vid, stmt, count_to_change *)
let nd_check_stmt_cntr_index_nm = "nd_check_stmt_cntr_index"
let nd_check_stmt_cntr_index =
  let lookup = "lookup_value" in
  let part_pat = ["vid", t_vid; "stmt_id", t_stmt_id] in
  let part_pat_as_vars = [mk_tuple @@ ids_to_vars @@ fst_many part_pat] in
  let query_pat = part_pat_as_vars @ [mk_cunknown] in
  let add_to_count, new_count = "add_to_count", "new_count" in
  mk_global_fn nd_check_stmt_cntr_index_nm
    (part_pat @ [add_to_count, t_int])
    [t_bool] @@ (* return whether we should send the do_complete *)
    (* check if the counter exists *)
    mk_case_sn
      (mk_peek @@ mk_slice' nd_stmt_cntrs.id query_pat) lookup
      (* calculate new_count *)
      (mk_let [new_count]
        (mk_add (mk_var add_to_count) @@ mk_fst @@ mk_snd @@ mk_var lookup) @@
        mk_block [
          (* upate the counter *)
          mk_update nd_stmt_cntrs.id [mk_var lookup] @@
            part_pat_as_vars @ [mk_tuple [mk_var new_count; mk_snd @@ mk_snd @@ mk_var lookup]];
          (* return whether the counter is 0 *)
          mk_eq (mk_cint 0) @@ mk_var new_count]) @@
      (* else: no value in the counter *)
      mk_block [
        (* Initialize *)
        mk_insert nd_stmt_cntrs.id @@
            part_pat_as_vars @ [mk_tuple [mk_var add_to_count; mk_empty nd_stmt_cntrs_corr_map.t]];
        mk_cbool false;
      ]

(* add_delta_to_buffer *)
(* adds the delta to all subsequent vids following in the buffer so that non
 * delta computations will be correct. Must be atomic ie. no other reads of the
 * wrong buffer value can happen.
 * This is the same procedure for both correctives and do_complete
 * it consists of 2 parts:
 * 1. Initialize the given vid using the delta values
 * 2. Add to all next vids *)
(* NOTE: this function assumes the initial value is always 0.
 * If we need another value, it must be handled via message from
 * m3tok3 *)
(* @idx: number to differentiate index pattern types *)
let nd_add_delta_to_buf c map_id =
  let delta_tuples, lookup_value, update_value, corrective, target_map, tmap_deref =
    "delta_tuples", "lookup_value", "update_value", "corrective", "target_map", "target_map_d" in
  let map_delta = D.map_ds_of_id c map_id ~global:false ~suffix:"_d" ~vid:false in
  let map_real  = D.map_ds_of_id c map_id ~global:true in
  (* flat pattern to read ds *)
  let real_pat  = D.pat_of_ds map_real in
  let delta_pat = D.pat_of_ds map_delta in
  (* a pattern mapping real map with delta vars *)
  let real_delta_pat =
    pat_of_flat_e map_real ~has_vid:false ~add_vid:true @@ fst_many delta_pat in
  (* function version of real pat (uses an expression) *)
  let real_pat_f e = D.pat_of_ds ~expr:e map_real in
  let regular_read =
    mk_block [
      mk_upsert_with_before "acc2"
        (D.unknown_val real_delta_pat)
        (mk_lambda'' unit_arg @@ mk_tuple @@ drop_vid real_delta_pat) @@
         mk_lambda' (ds_e map_real) @@
          mk_tuple @@ drop_vid @@ new_val real_delta_pat @@
            mk_add (get_val real_delta_pat) @@ get_val' real_pat;
      mk_var "acc2"]
  in
  mk_global_fn (D.nd_add_delta_to_buf_nm c map_id)
    (* corrective: whether this is a corrective delta *)
    ([target_map, wrap_tind map_real.t; corrective, t_bool; "vid", t_vid;
      delta_tuples, map_delta.t])
    [t_unit] @@
    mk_bind (mk_var target_map) tmap_deref @@
      mk_assign tmap_deref @@ U.add_property "Move" @@
        mk_agg
          (mk_lambda2' ["acc", map_real.t] (ds_e map_delta) @@
          (* add to future values for both correctives and regular updates *)
            mk_let ["acc2"]
              (mk_block [
                mk_update_suffix "acc" real_delta_pat @@
                  mk_lambda2' ["v", t_vid] (ds_e map_real) @@
                    mk_tuple @@
                      new_val (drop_vid' real_pat) @@
                        mk_add (get_val' real_pat) @@
                                get_val real_delta_pat;
                mk_var "acc"]) @@

            (* this part is just for correctives:
              * We need to check if there's a value at the particular version id
              * If so, we must add the value directly *)
              mk_if
                (mk_var corrective)
                (* corrective case *)
                (mk_case_sn
                  (mk_peek @@
                    mk_slice' "acc2" @@ D.unknown_val real_delta_pat) "val"
                  (* then just update the value *)
                  (mk_block [
                    mk_update "acc2"
                      [mk_var "val"] @@
                      D.new_val real_delta_pat @@
                        mk_add (get_val' @@ real_pat_f @@ mk_var "val") @@
                                get_val' delta_pat; mk_var "acc2"])
                  (* in the else case, we need to still do a regular read because
                  * there may not have been an initial write due to empty lookups on rhs
                  * maps *)
                  regular_read) @@
                (* non-corrective so do regular read *)
                regular_read)
      (mk_var tmap_deref) @@
    mk_var delta_tuples

(*
 * Return list of corrective statements we need to execute by checking
 * which statements were performed after time x that used a particular map
 * The only reason for this function is that we may have the same stmt
 * needing to execute with different vids
 * Optimization TODO: check also by ranges within the map ie. more fine grain
 *)
let nd_filter_corrective_list_nm = "nd_filter_corrective_list"
let nd_filter_corrective_list =
  let trig_stmt_list_t = wrap_tbag' [t_trig_id; t_stmt_id] in
  let return_type_base = [t_stmt_id; t_vid_list] in
  let return_type = wrap_tbag' return_type_base in
  mk_global_fn nd_filter_corrective_list_nm
  (* (trigger_id, stmt_id) list *)
  ["request_vid", t_vid; "trig_stmt_list", trig_stmt_list_t]
  [return_type]
  @@
  (* convert to bag *)
  mk_convert_col (wrap_tlist' return_type_base) return_type @@
    (* group the list by stmt_ids *)
    mk_gbagg
      (mk_lambda' ["_", t_vid; "stmt_id", t_stmt_id] @@ mk_var "stmt_id")
      (mk_assoc_lambda'
        ["vid_list", t_vid_list] ["vid", t_vid; "_", t_stmt_id] @@
        mk_block [
          mk_insert "vid_list" [mk_var "vid"];
          mk_var "vid_list" ])
      (mk_empty t_vid_list) @@
      mk_sort (* sort so early vids are generally sent out first *)
        (* get a list of vid, stmt_id pairs *)
        (** this is really a map, but make it a fold to convert to a list *)
        (mk_assoc_lambda' (* compare func *)
          ["vid1", t_vid; "stmt1", t_stmt_id]
          ["vid2", t_vid; "stmt2", t_stmt_id] @@
          mk_lt (mk_var "vid1") @@ mk_var "vid2") @@
        (* convert to list so we can sort *)
        mk_convert_col nd_log_master.t (wrap_tlist' @@ snd_many @@ ds_e nd_log_master) @@
          (* list of triggers >= vid *)
          mk_apply
            (mk_var nd_log_read_geq_nm) [mk_var "request_vid"]

(**** protocol code ****)

(* The driver trigger: loop over the trigger data structures as long as we have spare vids *)
let sw_driver_trig (c:config) =
  let trig_id = "trig_id" in
  (* dispatch code by trigger ids, for the trig buffers *)
  let dispatch_code =
    List.fold_left (fun acc (id, nm) ->
      (* check if we match on the id *)
      mk_if (mk_eq (mk_var trig_id) @@ mk_cint id)
        (mk_apply' (send_fetch_name_of_t nm) [mk_var "vid"])
        (* else, continue *)
        acc)
    (mk_error "mismatch on trigger id") @@
    P.for_all_trigs ~deletes:c.gen_deletes c.p (fun x -> P.trigger_id_for_name c.p x, x)
  in
  mk_code_sink' sw_driver_trig_nm unit_arg [] @@
  (* if we're initialized and somebody wants a vid *)
  mk_if
    (mk_and
      (mk_var D.sw_init.id) @@
       mk_gt (mk_size_slow D.sw_trig_buf_idx) @@ mk_cint 0)
    (TS.sw_gen_vid mk_cunit @@
      (* else -- we have a vid *)
      mk_pop D.sw_trig_buf_idx.id trig_id
        (* empty: no message to send *)
        mk_cunit @@
        (* we have a msg *)
        (* if it's the sentry, act *)
        mk_if (mk_eq (mk_var trig_id) @@ mk_cint (-1))
          Proto.sw_seen_sentry @@
          (* else *)
          mk_block [
            (* for debugging, sleep if we've been asked to *)
            mk_if (mk_neq (mk_var D.sw_driver_sleep.id) @@ mk_cint 0)
              (mk_apply' "sleep" [mk_var D.sw_driver_sleep.id])
              mk_cunit;
            (* send the msg using dispatch code *)
            dispatch_code;
            (* recurse, trying to get another message *)
            mk_send sw_driver_trig_nm G.me_var [mk_cunit];
          ])
    mk_cunit

(* The start function puts the message in a trig buffer *)
let sw_start_fn (c:config) trig =
  let args_t = snd_many @@ D.args_of_t c trig in
  let args = ["args", wrap_ttuple args_t] in
  mk_global_fn ("sw_start_"^trig) args [] @@
    mk_block [
      (* insert args into trig buffer *)
      mk_insert (D.sw_trig_buf_prefix^trig) [mk_var "args"];
      (* insert trig id into trig index buffer *)
      mk_insert D.sw_trig_buf_idx.id [mk_cint @@ P.trigger_id_for_name c.p trig];
      (* increment counters for msgs to get vids *)
      mk_incr TS.sw_need_vid_ctr.id;
    ]

(* the demux trigger takes the single stream and demuxes it *)
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
  let sentry_code =
    (* stash the sentry index in the queue *)
    mk_if (mk_eq (mk_fst @@ mk_var "args") @@ mk_cstring "")
      (mk_block [
        mk_insert D.sw_trig_buf_idx.id [mk_cint @@ -1];
        mk_incr TS.sw_need_vid_ctr.id]) @@
      mk_cunit
  in
  mk_code_sink' sw_demux_nm ["args", wrap_ttuple @@ List.map str_of_date_t combo_t] [] @@
  StrMap.fold (fun trig arg_indices acc ->
    let apply s =
      mk_apply' (s^trig) @@ singleton @@ mk_tuple @@
        (* add 1 for tuple access *)
        List.map (fun i ->
          convert_date i @@ mk_subscript (i+1) @@ mk_var "args") arg_indices
    in
    mk_if (mk_eq (mk_fst @@ mk_var "args") @@ mk_cstring trig)
      (mk_if (mk_eq (mk_snd @@ mk_var "args") @@ mk_cint 1)
        (apply "sw_start_insert_") @@
         apply "sw_start_delete_")
      acc)
    t_arg_map
    sentry_code

let stmt_map_ids =
  let e = ["stmt", t_stmt_id; "map", t_map_id] in
  create_ds ~e "stmt_map_ids" @@ wrap_tbag' @@ snd_many e

(* for puts *)
let stmt_cnt_list =
  let e = ["stmt", t_stmt_id; "count", t_map_id] in
  create_ds ~e "stmt_cnt_list" @@ wrap_tbag' @@ snd_many e

(* sending fetches is done from functions *)
(* each function takes a vid to plant in the arguments, which are in the trig buffers *)
let sw_send_fetch_fn c s_rhs_lhs s_rhs trig_name =
  let send_fetches_of_rhs_maps  =
    if null s_rhs then []
    else
    [mk_iter
      (mk_lambda'
        ["ip", t_addr; stmt_map_ids.id, stmt_map_ids.t] @@
        mk_send (rcv_fetch_name_of_t trig_name) (mk_var "ip") @@
          mk_var stmt_map_ids.id ::
          args_of_t_as_vars_with_v c trig_name) @@
      mk_gbagg
        (mk_lambda' (* Grouping function *)
          ["stmt_id", t_stmt_id; "map_id", t_map_id; "ip", t_addr] @@
          mk_var "ip")
        (mk_assoc_lambda' (* Agg function *)
          ["acc", wrap_tbag' [t_stmt_id; t_map_id]]
          ["stmt_id", t_stmt_id; "map_id", t_map_id; "ip", t_addr] @@
          mk_block [
            mk_insert "acc" @@ [mk_var "stmt_id"; mk_var "map_id"];
            mk_var "acc"])
        (mk_empty @@ wrap_tbag' [t_stmt_id; t_map_id])
        (* [] *) @@
        List.fold_left
          (fun acc_code (stmt_id, rhs_map_id) ->
            let route_fn = R.route_for c.p rhs_map_id in
            let key = P.partial_key_from_bound c.p stmt_id rhs_map_id in
            mk_combine
              (mk_map
                (mk_lambda' ["ip", t_addr] @@
                  mk_tuple @@ [mk_cint stmt_id; mk_cint rhs_map_id; mk_var "ip"]) @@
                mk_apply (mk_var route_fn) @@
                  (mk_cint rhs_map_id)::key)
              acc_code
          )
          (mk_empty @@ wrap_tbag' [t_stmt_id; t_map_id; t_addr])
          s_rhs
    ]
  in
  let send_completes_for_stmts_with_no_fetch =
    let s_no_rhs = P.stmts_without_rhs_maps_in_t c.p trig_name in
    if null s_no_rhs then []
    else
      List.fold_left
        (fun acc_code (stmt_id, lhs_map_id, do_complete_trig_name) ->
          let route_fn = R.route_for c.p lhs_map_id in
          let key = P.partial_key_from_bound c.p stmt_id lhs_map_id in
            acc_code@
            [mk_iter
              (mk_lambda' ["ip", t_addr] @@
                mk_send do_complete_trig_name (mk_var "ip") @@
                  args_of_t_as_vars_with_v c trig_name
              ) @@
              mk_apply (mk_var route_fn) @@
                (mk_cint lhs_map_id)::key
            ]
        )
        [] @@
        List.map
          (fun stmt_id ->
            stmt_id, P.lhs_map_of_stmt c.p stmt_id,
            do_complete_name_of_t trig_name stmt_id^"_trig")
          s_no_rhs
  in
  let send_puts =
    let address = "addr" in
    if null s_rhs_lhs then [] else
    (* send puts
    * count is generated by counting the number of messages going to a
    * specific IP *)
    [mk_iter
      (mk_lambda'
        [address, t_addr; stmt_cnt_list.id, stmt_cnt_list.t] @@
        mk_block @@
          (* send rcv_put *)
          (mk_send
            (rcv_put_name_of_t trig_name)
            (mk_var address) @@
            G.me_var :: mk_var stmt_cnt_list.id ::
              args_of_t_as_vars_with_v c trig_name) ::
            [GC.sw_update_send ~vid_nm:"vid"]) @@
      mk_gbagg
        (mk_lambda' (* grouping func *)
          ["ip_stmt_id", wrap_ttuple [t_addr; t_stmt_id]; "count", t_int] @@
          mk_subscript 1 @@ mk_var "ip_stmt_id")
        (mk_assoc_lambda' (* agg func *)
          ["acc", stmt_cnt_list.t]
          ["ip_and_stmt_id", wrap_ttuple [t_addr; t_stmt_id]; "count", t_int] @@
          mk_let (* break up because of the way inner gbagg forms tuples *)
            ["ip"; "stmt_id"] (mk_var "ip_and_stmt_id") @@
            mk_combine
              (mk_var "acc") @@
              mk_singleton
                stmt_cnt_list.t
                [mk_var "stmt_id"; mk_var "count"])
        (mk_empty @@ wrap_tbag' [t_stmt_id; t_int]) @@
        mk_gbagg (* inner gba *)
          (mk_lambda' (* group func *)
            ["ip", t_addr; "stmt_id", t_stmt_id; "count", t_int] @@
            mk_tuple [mk_var "ip"; mk_var "stmt_id"])
          (mk_assoc_lambda' (* agg func *)
            ["acc", t_int]
            ["ip", t_addr; "stmt_id", t_stmt_id; "count", t_int] @@
            mk_add
              (mk_var "acc") @@
              mk_var "count")
          (mk_cint 0) @@ (* [] *)
          List.fold_left
            (fun acc_code (stmt_id, (rhs_map_id, lhs_map_id)) ->
              (* shuffle allows us to recreate the path the data will take from
              * rhs to lhs *)
              let shuffle_fn = K3Shuffle.find_shuffle_nm c stmt_id rhs_map_id lhs_map_id in
              let shuffle_key = P.partial_key_from_bound c.p stmt_id lhs_map_id in
              (* route allows us to know how many nodes send data from rhs to lhs
              * *)
              let route_fn = R.route_for c.p rhs_map_id in
              let route_key = P.partial_key_from_bound c.p stmt_id rhs_map_id in
              (* we need the types for creating empty rhs tuples *)
              let rhs_map_types = P.map_types_with_v_for c.p rhs_map_id in
              let tuple_types = wrap_t_calc' rhs_map_types in
              mk_combine
                acc_code @@
                mk_let ["sender_count"]
                  (* count up the number of IPs received from route *)
                  (mk_agg
                    (mk_lambda''
                      ["count", t_int; "ip", t_addr] @@
                      mk_add (mk_var "count") @@ mk_cint 1)
                    (mk_cint 0) @@
                    mk_apply'
                      route_fn @@ mk_cint rhs_map_id::route_key) @@
                mk_map
                  (mk_lambda'
                    ["ip", t_addr; "tuples", tuple_types] @@
                      mk_tuple [mk_var "ip"; mk_cint stmt_id; mk_var "sender_count"]) @@
                  mk_apply' shuffle_fn @@
                    shuffle_key @ [mk_cbool true; mk_empty tuple_types]
            )
            (mk_empty @@ wrap_tbag' [t_addr; t_stmt_id; t_int]) @@
            s_rhs_lhs]
  in
  (* Actual SendFetch function *)
  (* We use functions rather than triggers to have better control over
  * latency *)
  let buf = D.sw_trig_buf_prefix^trig_name in
  mk_global_fn
    (send_fetch_name_of_t trig_name) ["vid", t_vid] [] @@
    let handle_args e =
      (* handle the case of no arguments (system_ready) *)
      if D.args_of_t c trig_name = [] then e
      else (* pop an argument out of the buffers *)
        mk_pop buf "args"
          (mk_error @@ "unexpected missing arguments in "^buf) @@
          (* decompose args *)
          mk_let (fst_many @@ D.args_of_t c trig_name)
            (mk_var "args") e
    in
    handle_args @@
      mk_block @@
        send_completes_for_stmts_with_no_fetch @
        send_puts @
        send_fetches_of_rhs_maps

(* trigger_rcv_fetch
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
let nd_rcv_fetch_trig c trig =
  mk_code_sink'
    (rcv_fetch_name_of_t trig)
    ((stmt_map_ids.id, stmt_map_ids.t):: args_of_t_with_v c trig)
    [] @@ (* locals *)
    mk_block [
      (* save the bound variables for this trigger so they're available later *)
      mk_apply
        (mk_var @@ nd_log_write_for c trig) @@
        args_of_t_as_vars_with_v c trig ;
      (* invoke generated send pushes. *)
      mk_iter
        (mk_lambda'
          ["stmt_id", t_stmt_id; "map_id", t_map_id] @@
          (* this send is not polymorphic. every fetch trigger expects
            * the same set of bound variables. *)
          List.fold_right
            (fun stmt acc_code -> mk_if
              (mk_eq (mk_var "stmt_id") @@ mk_cint stmt)
              (List.fold_right
                (fun map_id acc_code2 -> mk_if
                  (mk_eq (mk_var "map_id") @@ mk_cint map_id)
                  (* TODO: change to function *)
                  (mk_send (send_push_name_of_t c trig stmt map_id) G.me_var @@
                    args_of_t_as_vars_with_v c trig)
                  acc_code2)
                (P.rhs_maps_of_stmt c.p stmt) @@
                mk_error "nd_rcv_fetch: invalid map id")
              acc_code)
            (P.stmts_with_rhs_maps_in_t c.p trig) @@
            mk_error "nd_rcv_fetch: invalid stmt id") @@
        mk_var stmt_map_ids.id ]

(* Receive Put trigger
 * --------------------------------------- *
 * Update the statement counters with the received values
 * If the counter is 0, we need to call do_complete
 *)
let nd_rcv_put_trig c trig_name =
mk_code_sink'
  (rcv_put_name_of_t trig_name)
  (["sender_ip", t_addr; stmt_cnt_list.id, stmt_cnt_list.t]@
      (args_of_t_with_v c trig_name))
  [] @@
  mk_block
    [mk_iter
      (mk_lambda'
        ["stmt_id", t_stmt_id; "count", t_int] @@
        mk_if
          (mk_apply' nd_check_stmt_cntr_index_nm @@
            [mk_var "vid"; mk_var "stmt_id"; mk_var "count"])
          (* we need to create a possible send for each statement in the trigger *)
          (List.fold_left (fun acc_code stmt_id ->
            mk_if
              (mk_eq (mk_var "stmt_id") @@ mk_cint stmt_id)
              (mk_apply'
                (do_complete_name_of_t trig_name stmt_id) @@
                args_of_t_as_vars_with_v c trig_name) @@
              acc_code)
            mk_cunit @@
            P.stmts_of_t c.p trig_name) @@
          mk_cunit) @@
        mk_var stmt_cnt_list.id ;
      GC.nd_ack_send_code ~addr_nm:"sender_ip" ~vid_nm:"vid"]


(* Trigger_send_push_stmt_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 * Produces a list of triggers.
 * We're parametrized by stmt and map_id because we save repeating the
 * processing on the switch side to figure out which maps (in which stmts) we
 * have locally
 *)
let nd_send_push_stmt_map_trig c s_rhs_lhs trig_name =
  List.fold_left
    (fun acc_code (stmt_id, (rhs_map_id, lhs_map_id)) ->
      let rhs_map_types = P.map_types_with_v_for c.p rhs_map_id in
      let rhs_map_name = P.map_name_of c.p rhs_map_id in
      let rhsm_deref = rhs_map_name^"_deref" in
      let shuffle_fn = K3Shuffle.find_shuffle_nm c stmt_id rhs_map_id lhs_map_id in
      let partial_key = P.partial_key_from_bound c.p stmt_id lhs_map_id in
      let slice_key = P.slice_key_from_bound c.p stmt_id rhs_map_id in
      acc_code @
      [mk_code_sink'
        (send_push_name_of_t c trig_name stmt_id rhs_map_id)
        (args_of_t_with_v c trig_name)
        [] @@ (* locals *)
        (* don't convert this to fold b/c we only read *)
        mk_bind (mk_var rhs_map_name) rhsm_deref @@
        mk_block [
          (* save this particular statement execution in the master log
           * Note that we need to do it here to make sure nothing
           * else can stop us before we send the push *)
          mk_apply'
            nd_log_master_write_nm @@
            [mk_var "vid"; mk_cint stmt_id] ;
          mk_iter
            (mk_lambda'
              ["ip", t_addr;"tuples", wrap_t_calc' rhs_map_types] @@
              mk_send
                (rcv_push_name_of_t c trig_name stmt_id rhs_map_id)
                (mk_var "ip") @@
                mk_var "tuples" :: args_of_t_as_vars_with_v c trig_name) @@
            mk_apply'
              shuffle_fn @@
                partial_key @ [mk_ctrue] @
                (* we need the latest vid data that's less than the current vid *)
                [D.map_latest_vid_vals c (mk_var rhsm_deref)
                  (some slice_key) rhs_map_id ~keep_vid:true]]
      ]) (* trigger *)
    [] s_rhs_lhs


(* rcv_push_trig
 * --------------------------------------
 * Receive a push at a node
 * Also called for virtual pushes: local data transfers to allow tracking of
 * present data w/ counters params can be moved to the put statement, but it's
 * a good reminder to have it here
 * We write to specific buffer maps to prevent mixing of buffer and non-buffer
 * data, which can cause confusion when the time comes to compute.
 * A later optimization could be lumping maps between statements in a trigger *)

let nd_rcv_push_trig c s_rhs trig_name =
List.fold_left
  (fun acc_code (stmt_id, read_map_id) ->
    let rbuf_name = P.buf_of_stmt_map_id c.p stmt_id read_map_id in
    let rbuf_deref = "buf_d" in
    let map_ds = map_ds_of_id ~global:true c read_map_id in
    let tup_ds = map_ds_of_id ~global:false ~vid:true c read_map_id in
    let tup_pat = pat_of_ds tup_ds in
    let map_pat = pat_of_flat_e ~add_vid:true ~has_vid:true map_ds @@ fst_many tup_pat in
    acc_code @
    [mk_code_sink'
      (rcv_push_name_of_t c trig_name stmt_id read_map_id)
      (("tuples", tup_ds.t)::
        args_of_t_with_v c trig_name)
      [] @@ (* locals *)
      (* save the tuples *)
      mk_block
        (* save the bound variables for this vid. This is necessary for both the
         * sending and receiving nodes, which is why we're also doing it here *)
        [mk_apply' (nd_log_write_for c trig_name) @@
          args_of_t_as_vars_with_v c trig_name
        ;
        mk_bind (mk_var rbuf_name) rbuf_deref @@
         mk_assign rbuf_deref @@ U.add_property "Move" @@
         mk_agg
          (mk_lambda2' ["acc", map_ds.t] (ds_e tup_ds) @@
            mk_block [
              mk_insert "acc" map_pat;
              mk_var "acc" ])
          (mk_var rbuf_deref) @@
          mk_var "tuples" ;
         (* update and check statment counters to see if we should send a do_complete *)
         mk_if
           (mk_apply' nd_check_stmt_cntr_index_nm @@
             [mk_var "vid"; mk_cint stmt_id; mk_cint @@ -1])
           (* Send to local do_complete *)
           (mk_apply' (do_complete_name_of_t trig_name stmt_id) @@
             args_of_t_as_vars_with_v c trig_name)
           mk_cunit ] ])
  [] s_rhs

(* list of trig, stmt with a map on the rhs that's also on the lhs. These are
 * the potential corrective maps.
 * Inserts and Deletes really have 2 different graphs, so we return both *)
let maps_potential_corrective p =
  let insert_ts = P.get_trig_list ~kind:P.InsertTrigs p in
  let delete_ts = P.get_trig_list ~kind:P.DeleteTrigs p in
  let get_maps f = ListAsSet.uniq |- List.flatten |- List.map f |- List.flatten
    |- List.map (P.stmts_of_t p) in
  let lhs_maps = get_maps (singleton |- P.lhs_map_of_stmt p) in
  let rhs_maps = get_maps (P.rhs_maps_of_stmt p) in
  let intersect_maps ts = ListAsSet.inter (lhs_maps ts) (rhs_maps ts) in
  intersect_maps insert_ts, intersect_maps delete_ts

(* original values commonly used to send back to original do_complete *)
let orig_vals = ["orig_addr", t_addr; "orig_stmt_id", t_stmt_id; "orig_vid", t_vid; "hop", t_int]

(* send_corrective_trigs
 * --------------------------------------
 * Generate code to send data for all possible correctives.
 * When we compute, we produce lhs results. These might be used in other
 * statements as rhs inputs. We need to find those places, and then carry out
 * the shuffling of the changed data for those statements ie from rhs to lhs
 * This is a very coarse-grained method of finding correctives needed.
 *
 * We must make sure that we only send the delta once. However, all vids can be sent
 * for execution together.
 * We return the number of messages sent out. NOT the number of vids.
 *)
let send_corrective_fns c =
  (* for a given lhs map which we just changed, find all statements containing the
   * same map on the rhs. This is crude, but should be enough for a first try *)
  let send_correctives map_id =
    (* list of (trig,stmt) that have this map_id on the rhs *)
    let trigs_stmts_with_matching_rhs_map =
        List.filter
          (fun (trig, stmt_id) -> P.stmt_has_rhs_map c.p stmt_id map_id) @@
          List.flatten @@
            P.for_all_trigs ~deletes:c.gen_deletes c.p
              (fun trig ->
                List.map (fun stmt -> trig, stmt) @@ P.stmts_of_t c.p trig) in
    (* turn the ocaml list into a literal k3 list *)
    let trig_stmt_k3_list =
      let types = wrap_tbag' [t_trig_id; t_stmt_id] in
      k3_container_of_list types @@ List.map
        (fun (trig, stmt_id) ->
          mk_tuple [mk_cint (P.trigger_id_for_name c.p trig); mk_cint stmt_id])
        trigs_stmts_with_matching_rhs_map
    in
    match trigs_stmts_with_matching_rhs_map with [] -> [] | _ ->
    let map_ds = D.map_ds_of_id ~global:false c map_id ~vid:false in
    let tuple_type = wrap_ttuple @@ snd_many (ds_e map_ds) in
    let delta_tuples2 =
      D.map_ds_of_id ~global:false ~vid:true c map_id ~name:"delta_tuples2" in
    let args' = orig_vals @ ["corrective_vid", t_vid] in
    let args = args' @ ["delta_tuples", map_ds.t] in
    let sub_args = args' @ ["delta_tuples2", delta_tuples2.t; "vid_list", t_vid_list] in
    let fn_nm = send_corrective_name_of_t c map_id in
    let sub_fn_nm stmt = fn_nm^"_"^soi stmt in

    (* create sub functions for each stmt_id possible, to aid compilation *)
    let sub_fns =
      List.map
        (fun (target_trig, target_stmt) ->
              (* we already have the map vars for the rhs map in the
                * tuples. Now we try to get some more for the lhs map *)
              let target_map = P.lhs_map_of_stmt c.p target_stmt in
              let key = P.partial_key_from_bound c.p target_stmt target_map in
              let shuffle_fn = K3Shuffle.find_shuffle_nm c target_stmt map_id target_map in

              mk_global_fn (sub_fn_nm target_stmt) sub_args
              [t_int] @@ (* return num of sends *)
                mk_let ["ips_vids"]
                  (mk_gbagg
                    (* group by the IPs. We want all the vids we'll need to
                      * execute, and we concatenate the tuples since we're
                      * adding deltas anyway, so if there's no stale value,
                      * there's no harm done *)
                    (mk_lambda'
                      ["ip", t_addr; "vid", t_vid; "tuples", map_ds.t] @@
                        mk_var "ip")
                    (mk_assoc_lambda'
                      ["acc_vid", t_vid_list; "acc_tuples", map_ds.t]
                      ["ip", t_addr; "vid", t_vid; "tuples", map_ds.t] @@
                        mk_block [
                          mk_insert "acc_vid" [mk_var "vid"];
                          mk_tuple
                            [mk_var "acc_vid";
                              (* eliminate dups *)
                              mk_fst_many [tuple_type; t_unit] @@ mk_gbagg
                                (mk_lambda' ["tuple", tuple_type] @@ mk_var "tuple")
                                (mk_lambda'' ["_", t_unit; "_", tuple_type] mk_cunit)
                                mk_cunit @@
                                mk_combine (mk_var "acc_tuples") @@ mk_var "tuples"]])
                    (mk_tuple [mk_empty t_vid_list; mk_empty map_ds.t]) @@
                    mk_flatten @@ mk_map
                      (mk_lambda' ["vid", t_vid] @@
                        (* get bound vars from log so we can calculate shuffle *)
                        let args = D.args_of_t c target_trig in
                        (if args <> [] then
                          mk_let
                            (fst_many @@ D.args_of_t c target_trig)
                            (mk_apply'
                              (nd_log_get_bound_for target_trig) [mk_var "vid"])
                        else id_fn) @@
                        (* insert vid into the ip, tuples output of shuffle *)
                        mk_map (* (ip * vid * tuple list) list *)
                          (mk_lambda' ["ip", t_addr; "tuples", delta_tuples2.t] @@
                              mk_tuple [mk_var "ip"; mk_var "vid";
                                (* get rid of vid NOTE: to reduce number of shuffles *)
                                  mk_map (mk_lambda' (ds_e delta_tuples2) @@
                                      mk_tuple @@ ids_to_vars @@ fst_many @@ ds_e map_ds) @@
                                    mk_var "tuples"]) @@
                          mk_apply'
                            shuffle_fn @@
                              (* (ip * tuple list) list *)
                              key @ [mk_cfalse; mk_var "delta_tuples2"]) @@
                      mk_var "vid_list") @@
                  (* send to each ip, and count up the vids *)
                  mk_agg
                    (mk_assoc_lambda' ["acc_count", t_int]
                      ["ip", t_addr;
                        "vid_send_list_tup", wrap_ttuple [t_vid_list; map_ds.t]] @@
                      mk_block [
                        mk_send
                          (* we always send to the same map_id ie. the remote
                            * buffer of the same map we just calculated *)
                          (rcv_corrective_name_of_t c target_trig target_stmt map_id)
                          (mk_var "ip") @@
                          (* we send the vid where the update is taking place as
                            * well as the vids of the sites where corrections must
                            * be calculated. *)
                          (ids_to_vars @@ fst_many orig_vals) @
                          [mk_var "corrective_vid"; mk_fst @@ mk_var "vid_send_list_tup";
                                                    mk_snd @@ mk_var "vid_send_list_tup"];
                        mk_add (mk_var "acc_count") @@ mk_cint 1
                      ])
                    (mk_cint 0) @@
                    mk_var "ips_vids")
        trigs_stmts_with_matching_rhs_map
    in
    let fn = mk_global_fn fn_nm args
    (* return the number of new correctives generated *)
    [t_int] @@
    (* the corrective list tells us which statements were fetched
     * from us and when *)
    mk_let ["corrective_list"] (* (stmt_id * vid list) list *)
      (mk_apply'
        nd_filter_corrective_list_nm @@
        (* feed in list of possible stmts *)
          [mk_var "corrective_vid"; trig_stmt_k3_list]) @@
    (* if corrective list isn't empty, add vid inside delta tuples *)
    mk_is_empty (mk_var "corrective_list")
      ~y:(mk_cint 0)
      ~n:
      (* loop over corrective list and act for specific statements *)
      (* return the number of messages *)
      (* we need to add a vid value here to reduce the number of needed shuffle instantiations,
       * so we don't have shuffles with vid as well as ones without *)
      (mk_let [delta_tuples2.id]
        (mk_map (mk_lambda' (ds_e map_ds) @@ mk_tuple @@
                  (mk_var g_min_vid.id)::(ids_to_vars' @@ ds_e map_ds)) @@
          mk_var "delta_tuples") @@
      mk_agg
        (mk_assoc_lambda'
          ["acc_count", t_int] ["stmt_id", t_stmt_id; "vid_list", t_vid_list] @@
          List.fold_left
            (* loop over all possible read map matches *)
            (fun acc_code (_, target_stmt) ->
              mk_if (* if match, send data *)
                (mk_eq (mk_var "stmt_id") @@ mk_cint target_stmt)
                (* call the specific function for this statement *)
                (mk_add (mk_var "acc_count") @@
                  mk_apply' (sub_fn_nm target_stmt) @@ ids_to_vars' sub_args)
                acc_code)
            (mk_var "acc_count") (* base case *)
            trigs_stmts_with_matching_rhs_map)
            (* base number of msgs *)
          (mk_cint 0) @@
          mk_var "corrective_list")
    in
    sub_fns @ [fn]
  in
  List.flatten @@ List.map send_correctives @@
    (* combine maps from insert and delete *)
    ListAsSet.union (fst c.corr_maps) (snd c.corr_maps)

(* function to update the stmt_counters for correctives *)
(* current hop = hop - 1. next hop = hop *)
let nd_update_stmt_cntr_corr_map_nm = "nd_update_stmt_cntr_corr_map"
let nd_update_stmt_cntr_corr_map =
  let lookup_pat = [mk_var "vid"; mk_var "stmt_id"] in
  mk_global_fn nd_update_stmt_cntr_corr_map_nm
  ["vid", t_vid; "stmt_id", t_stmt_id; "hop", t_int; "count", t_int; "root", t_bool; "create", t_bool] [] @@
    mk_block [
      (* if requested, we first create an empty entry *)
      mk_if (mk_var "create")
        (mk_insert nd_stmt_cntrs.id [mk_tuple lookup_pat; mk_tuple [mk_cint 0; mk_empty nd_stmt_cntrs_corr_map.t]])
        mk_cunit;
      (* we need to decrement the previous hop's value by 1, and increment the hop's values by the count (if it's not 0) *)
      mk_upsert_with_sim nd_stmt_cntrs "lkup" ~k:lookup_pat
        ~default:(mk_error @@ Printf.sprintf "%s: missing stmt_cntrs value" nd_update_stmt_cntr_corr_map_nm)
        ~v:(mk_let [nd_stmt_cntrs_corr_map.id] (mk_snd @@ mk_snd @@ mk_var "lkup") @@
              mk_block [
                (* only do this part if count isn't 0 *)
                mk_if (mk_eq (mk_var "count") @@ mk_cint 0)
                  mk_cunit @@
                  (* increment the next hop by count *)
                  mk_case_ns (mk_peek @@ mk_slice' nd_stmt_cntrs_corr_map.id [mk_var "hop"; mk_cunknown]) "lkup2"
                      (* if no entry, set to count *)
                      (mk_insert nd_stmt_cntrs_corr_map.id [mk_var "hop"; mk_var "count"]) @@
                      (* else, calculate new corr count *)
                      mk_let ["new_corr_cnt"] (mk_add (mk_snd @@ mk_var "lkup2") @@ mk_var "count") @@
                      (* if we've hit 0 *)
                      mk_if (mk_eq (mk_var "new_corr_cnt") @@ mk_cint 0)
                        (* delete the entry altogether *)
                        (mk_delete nd_stmt_cntrs_corr_map.id @@ [mk_var "lkup2"]) @@
                        (* else, save the incremented entry *)
                        mk_update nd_stmt_cntrs_corr_map.id [mk_var "lkup2"] [mk_var "hop"; mk_var "new_corr_cnt"];
                (* check if this is from the root of the corrective tree *)
                mk_if (mk_var "root")
                  (* return as is *)
                  mk_cunit @@
                  mk_let ["hop2"] (mk_sub (mk_var "hop") @@ mk_cint 1) @@
                  (* else update the current hop *)
                  mk_case_ns (mk_peek @@ mk_slice' nd_stmt_cntrs_corr_map.id [mk_var "hop2"; mk_cunknown]) "lkup2"
                    (* if no entry, set to -1 *)
                    (mk_insert nd_stmt_cntrs_corr_map.id [mk_var "hop2"; mk_cint @@ -1]) @@
                    (* else, calculate new corr count *)
                    mk_let ["new_corr_cnt"] (mk_sub (mk_snd @@ mk_var "lkup2") @@ mk_cint 1) @@
                    (* if we've hit 0, *)
                    mk_if (mk_eq (mk_var "new_corr_cnt") @@ mk_cint 0)
                      (* delete the entry *)
                      (mk_delete nd_stmt_cntrs_corr_map.id @@ [mk_var "lkup2"]) @@
                      (* else, save the decremented entry *)
                      mk_update nd_stmt_cntrs_corr_map.id [mk_var "lkup2"] [mk_var "hop2"; mk_var "new_corr_cnt"];
                (* set the new tuple *)
                mk_tuple [mk_fst @@ mk_snd @@ mk_var "lkup"; mk_var nd_stmt_cntrs_corr_map.id]
              ])
    ]

(* call from do_complete when done to check if fully done *)
let nd_complete_stmt_cntr_check_nm = "nd_complete_stmt_cntr_check"
let nd_complete_stmt_cntr_check c =
  mk_global_fn nd_complete_stmt_cntr_check_nm ["vid", t_vid; "stmt_id", t_stmt_id] [] @@
  (* if we have nothing to send, we can delete our stmt_cntr entry right away *)
  mk_block [
    mk_delete_one nd_stmt_cntrs [mk_tuple [mk_var "vid"; mk_var "stmt_id"]; mk_cunknown];
    (* check if we're done *)
    Proto.nd_post_delete_stmt_cntr c;
  ]

(*
 * shared coded btw do_complete and do_corrective
 * we add the delta to all following vids
 *)
let do_add_delta c e lmap ~corrective =
  mk_apply' (D.nd_add_delta_to_buf_nm c lmap) @@
    [mk_var @@ P.map_name_of c.p lmap;
      if corrective then mk_ctrue else mk_cfalse; mk_var "vid"; e]

(* trigger versions of do_complete: only for stmts with no rhs maps *)
let nd_do_complete_trigs c trig_name =
  let do_complete_trig stmt_id =
    let comp_nm = do_complete_name_of_t trig_name stmt_id in
    let args = args_of_t_with_v c trig_name in
    mk_code_sink' (comp_nm^"_trig") args [] @@
    mk_apply' comp_nm @@ ids_to_vars @@ fst_many args;
in
List.map do_complete_trig @@ P.stmts_without_rhs_maps_in_t c.p trig_name

(* function versions of do_complete *)
let nd_do_complete_fns c ast trig_name corr_maps =
  (* @has_rhs: whether this statement has rhs maps *)
  let do_complete_fn has_rhs stmt_id =
    mk_global_fn (do_complete_name_of_t trig_name stmt_id)
      (args_of_t_with_v c trig_name) [] @@
    let lmap = P.lhs_map_of_stmt c.p stmt_id in
    let fst_hop = mk_cint 1 in
    let snd_hop = mk_cint 2 in
    let after_fn tup_ds =
      mk_block [
        (* add delta *)
        do_add_delta c tup_ds lmap ~corrective:false;
        (if c.gen_correctives && List.exists ((=) lmap) corr_maps
        then
          let send_corr_t = send_corrective_name_of_t c lmap in
          mk_let ["sent_msgs"]
            (* we apply send_correctives with our original address, stmt_id, original vid and hop + 1
             * we double up on vid since send_correctives is also called for do_corrective,
             * which must send the new vid to be calculated as well as the original complete's vid
             *)
            (mk_apply' send_corr_t @@
              [G.me_var; mk_cint stmt_id; mk_var "vid"; snd_hop; mk_var "vid"; tup_ds]) @@
            (* update the corrective counters for hop 1 to the number of msgs.
             * true: is a root, bool: create an entry *)
            let update_corr_code create =
              mk_apply' nd_update_stmt_cntr_corr_map_nm @@
                [mk_var "vid"; mk_cint stmt_id; fst_hop; mk_var "sent_msgs"; mk_ctrue; mk_cbool create]
            in
            if has_rhs then
              mk_if (mk_eq (mk_var "sent_msgs") @@ mk_cint 0)
                (* if our sent_msgs is 0, we need to delete the stmt cntr entry *)
                (mk_apply' nd_complete_stmt_cntr_check_nm
                  [mk_var "vid"; mk_cint stmt_id]) @@
                (* otherwise we need to update the corrective counters *)
                update_corr_code false
            else
              (* if we have no rhs maps, we may not need to update anything,
               * since no stmt cntr entry was created *)
              mk_if (mk_eq (mk_var "sent_msgs") @@ mk_cint 0)
                mk_cunit @@
                (* else, update and create a stmt counter *)
                update_corr_code true
        (* no correctives are possible *)
        else
          (* if we have no rhs maps, do nothing *)
          if not has_rhs then mk_cunit
          else mk_apply' nd_complete_stmt_cntr_check_nm [mk_var "vid"; mk_cint stmt_id])
      ]
    in
    M.modify_ast_for_s c ast stmt_id trig_name after_fn
in
(List.map (do_complete_fn true)  @@ P.stmts_with_rhs_maps_in_t c.p trig_name) @
(List.map (do_complete_fn false) @@ P.stmts_without_rhs_maps_in_t c.p trig_name)

(* rcv notification of a corrective finish from other nodes *)
let nd_rcv_corr_done_nm = "nd_rcv_corr_done"
let nd_rcv_corr_done c =
  let lookup_pat = [mk_tuple [mk_var "vid"; mk_var "stmt_id"]; mk_cunknown] in
  let args = ["vid", t_vid; "stmt_id", t_stmt_id; "hop", t_int; "count", t_int] in
  mk_code_sink' nd_rcv_corr_done_nm args [] @@
    mk_block [
      (* update the corrective map. false: not a root of the corrective tree *)
      mk_apply' nd_update_stmt_cntr_corr_map_nm @@
        (ids_to_vars @@ fst_many args) @ [mk_cfalse; mk_cfalse];
      (* check if the corr_cnt structure is empty. If so, we can delete the whole entry *)
      mk_case_ns (mk_lookup' nd_stmt_cntrs.id lookup_pat) "lkup"
        (mk_error @@ nd_rcv_corr_done_nm^": expected stmt_cntr value") @@
        (* if the corr_cnt map is empty *)
        mk_is_empty (mk_snd @@ mk_snd @@ mk_var "lkup")
          ~n:mk_cunit
          ~y:(mk_block [
              (* delete the whole stmt_cntr entry *)
              mk_delete nd_stmt_cntrs.id [mk_var "lkup"];
              Proto.nd_post_delete_stmt_cntr c])
    ]

(* receive_correctives:
 * ---------------------------------------
 * Function that gets called when a corrective message is received.
 * Receives bound variables and delta tuples for one map.
 *
 * Analogous to rcv_push for non-correctives
 * Has 2 parts: updating the local buffers, and executing the requested vid(s)
 * The local buffer is always updated at a new vid (one that was empty before),
 * adding to the value from an earlier vid, and propagating to future vids.
 *)
let nd_rcv_correctives_trig c s_rhs trig_name = List.map
  (fun (stmt_id, rmap) ->
    let buf_map_nm = P.buf_of_stmt_map_id c.p stmt_id rmap in
    mk_code_sink' (rcv_corrective_name_of_t c trig_name stmt_id rmap)
      (* we always send back acks to the original address, stmt_id, vid tuple *)
        (orig_vals @
        ["vid", t_vid; "compute_vids", t_vid_list;
         "delta_tuples", wrap_t_calc' @@ P.map_types_for c.p rmap])
      [] @@ (* locals *)
      mk_block [
        (* accumulate delta for this vid and all following vids. This is a very
         * sensitive point in the protocol and it's essential this only be done
         * once for a given corrective *)
        mk_apply'
          (D.nd_add_delta_to_buf_nm c rmap) @@
            (* pass the map indirection, false=not corrective *)
            [mk_var buf_map_nm; mk_cbool false; mk_var "vid"; mk_var "delta_tuples"];
        (* for every computation vid, only execute if we have all the updates *)
        mk_let ["sent_msgs"]
          (mk_agg
            (mk_assoc_lambda' ["acc_count", t_int] ["compute_vid", t_vid] @@
              (* get the stmt counter *)
              mk_let ["cntr"]
                (mk_case_ns (mk_lookup' D.nd_stmt_cntrs.id
                  [mk_tuple [mk_var "compute_vid"; mk_cint stmt_id]; mk_cunknown]) "lkup"
                  (* 0 if we have no value *)
                  (mk_cint 0) @@
                  mk_fst @@ mk_snd @@ mk_var "lkup") @@
                (* check if our stmt_counter is 0 *)
                mk_if (mk_eq (mk_var "cntr") @@ mk_cint 0)
                  (* if so, get bound vars from log *)
                  (let args = fst_many @@ D.args_of_t c trig_name in
                  (if args <> [] then
                    mk_let args
                      (mk_apply'
                        (nd_log_get_bound_for trig_name) [mk_var "compute_vid"])
                  else id_fn) @@
                    (* do_corrective, return number of msgs *)
                    mk_add (mk_var "acc_count") @@
                      mk_apply'
                        (do_corrective_name_of_t c trig_name stmt_id rmap) @@
                          (ids_to_vars @@ fst_many orig_vals) @
                          args_of_t_as_vars_with_v ~vid:"compute_vid" c trig_name @
                          [mk_var "delta_tuples"]) @@
                  (* else, do nothing *)
                  mk_var "acc_count")
              (mk_cint 0) @@
              mk_var "compute_vids") @@
          (* send an ack with how many msgs we sent on *)
          mk_send nd_rcv_corr_done_nm (mk_var "orig_addr")
            [mk_var "orig_vid"; mk_var "orig_stmt_id"; mk_var "hop"; mk_var "sent_msgs"]
        ]
  )
  s_rhs
;;

(* do corrective functions *)
(* very similar to do_complete, but we only have to do it for certain
 * (stmt, map) combinations.
 * Do_corrective , unlike do_complete, doesn't need to add to an earlier vid
 * value. Instead, it adds to the value that already resides at a specific
 * vid and propagates. *)
let nd_do_corrective_fns c s_rhs ast trig_name corrective_maps =
  let do_corrective_fn (stmt_id, map_id) =
    let tuple_typ = wrap_t_calc' @@ P.map_types_for c.p map_id in
    mk_global_fn (do_corrective_name_of_t c trig_name stmt_id map_id)
      (orig_vals @ args_of_t_with_v c trig_name @ ["delta_tuples", tuple_typ])
      [t_int] @@
        let lmap = P.lhs_map_of_stmt c.p stmt_id in
        let send_corr_fn = send_corrective_name_of_t c lmap in
        let args, ast =
          M.modify_corr_ast c ast map_id stmt_id trig_name id_fn
        in
        mk_let ["new_tuples"]
        (* We *can't* filter out 0 values, because they may add to a map
         * that didn't have any value, and initialize a value at that key *)
          (mk_flatten @@ mk_map (mk_lambda' args ast) @@ mk_var "delta_tuples") @@
            mk_block [
              (* add delta *)
              do_add_delta c (mk_var "new_tuples") lmap ~corrective:true;
              (* send correctives *)
              if List.exists ((=) lmap) corrective_maps
              (* send correctives with hop + 1, and return the num of correctives *)
              then mk_apply' send_corr_fn @@
                (modify_e orig_vals ["hop", mk_add (mk_cint 1) @@ mk_var "hop"]) @
                [mk_var "vid"; mk_var "new_tuples"]
              (* if we have no more correctives, return 0 *)
              else mk_cint 0
            ]
  in
  List.map do_corrective_fn s_rhs

let flatteners c =
  let l = snd_many @@ D.uniq_types_and_maps ~uniq_indices:false c in
  List.map (fun (t, maps) ->
    let map_ds = map_ds_of_id ~global:true ~vid:false c (hd maps) in
    let pat = pat_of_ds ~flatten:true ~drop_vid:true ~expr:(mk_var "tuple") map_ds in
    mk_global_fn ("flatten_"^strcatmap ~sep:"_" K3PrintSyntax.string_of_type t)
    ["tuple", snd @@ unwrap_tcol map_ds.t] [wrap_ttuple t] @@
    mk_tuple @@ fst_many pat) l

let str_of_date_t t = match t.typ with
  | TDate -> {t with typ = TString}
  | x -> t

(* we take the existing default role and prepend it with a one-shot to
 * call out on-init function *)
let roles_of c (ast:program_t) =
  (* extra flows for master *)
  let ms_flows = List.map mk_no_anno [
     Source(Resource("master",
       Stream(t_unit, ConstStream(mk_singleton (wrap_tlist t_unit) [mk_cunit]))));
     BindFlow("master", Proto.ms_send_addr_self_nm);
     Instruction(Consume("master")); ] in
  let sw_flows = List.map mk_no_anno [
    Source(Resource("switch",
      Handle(wrap_ttuple @@ List.map str_of_date_t @@ fst @@ combine_trig_args c,
        File c.stream_file,
        CSV)));
    BindFlow("switch", sw_demux_nm);
    Instruction(Consume("switch")); ]
  in
  List.map mk_no_anno [
    Role("master", ms_flows);
    Role("switch", sw_flows);
    Role("timer", []);
    Role("node", []);
  ]

(* loader vars for ast *)
let gen_loader_vars ast =
  let tables = ModifyAst.loader_tables ast in
  List.map (fun (s, f) ->
    mk_global_val_init (s^"_path") t_string @@ mk_cstring f) tables

let declare_global_vars c partmap ast =
  (* dummy switch path variable. Will be filled at cpp stage *)
  decl_global
    (create_ds "switch_path" t_string ~init:(mk_cstring "agenda.csv")) ::
  (* path variables for any csv loaders *)
  gen_loader_vars ast @
  Proto.global_vars c @
  D.global_vars c (ModifyAst.map_inits_from_ast c ast) @
  Timer.global_vars @
  TS.global_vars @
  K3Ring.global_vars @
  K3Route.global_vars c.p partmap @
  GC.global_vars c

let declare_global_funcs c partmap ast =
  flatteners c @
  nd_log_master_write ::
  (P.for_all_trigs ~sys_init:true ~deletes:c.gen_deletes c.p @@ nd_log_write c) @
  (P.for_all_trigs ~sys_init:true ~deletes:c.gen_deletes c.p @@ nd_log_get_bound c) @
  nd_log_read_geq ::
  nd_check_stmt_cntr_index ::
  nd_complete_stmt_cntr_check c ::
  nd_update_stmt_cntr_corr_map ::
  begin if c.gen_correctives then [nd_filter_corrective_list] else [] end @
  K3Ring.functions @
  (List.map (fun (i, (_, maps)) -> nd_add_delta_to_buf c (hd maps)) @@ D.uniq_types_and_maps c) @
  TS.functions @
  K3Route.functions c partmap @
  K3Shuffle.functions c @
  GC.functions c

(* Generate all the code for a specific trigger *)
let gen_dist_for_t c ast trig =
  (* (stmt_id, rhs_map_id)list *)
  let s_rhs = P.s_and_over_stmts_in_t c.p P.rhs_maps_of_stmt trig in
  (* for delete trigs, access different corrective list *)
  let access = if P.is_insert_t trig then fst else snd in
  (* stmts that can be involved in correctives *)
  let s_rhs_corr = List.filter (fun (s, map) -> List.mem map @@ access c.corr_maps) s_rhs in
  (* (stmt_id,rhs_map_id,lhs_map_id) list *)
  let s_rhs_lhs = P.s_and_over_stmts_in_t c.p P.rhs_lhs_of_stmt trig in
  (* functions *)
  let functions = [
    sw_start_fn c trig;
    sw_send_fetch_fn c s_rhs_lhs s_rhs trig;
  ] @
   nd_do_complete_fns c ast trig (access c.corr_maps) @
   (if c.gen_correctives then
     nd_do_corrective_fns c s_rhs_corr ast trig (access c.corr_maps)
   else [])
  in
  let trigs =
    (if null s_rhs then []
    else
      [nd_rcv_put_trig c trig;
      nd_rcv_fetch_trig c trig])
    @
    nd_send_push_stmt_map_trig c s_rhs_lhs trig @
    nd_rcv_push_trig c s_rhs trig @
    nd_do_complete_trigs c trig @
    (if c.gen_correctives then
      nd_rcv_correctives_trig c s_rhs_corr trig
    else [])
  in
  trigs, functions

(* Function to generate the whole distributed program *)
(* @param force_correctives Attempt to create dist code that encourages correctives *)
let gen_dist ?(gen_deletes=true)
             ?(gen_correctives=true)
             ~stream_file
             ~map_type
             ~(agenda_map: mapping_t)
             p partmap ast =
  let sys_init =
    try ignore(P.find_trigger p "system_ready_event"); true
    with Not_found | P.Bad_data _ -> false in
  let unused_trig_args = M.unused_trig_args ast in

  (* adjust agenda_map for unused trig args *)
  let agenda_map = second (StrMap.mapi @@ fun trig_nm l ->
    try
      let args = fst_many @@ P.args_of_t p ("insert_"^trig_nm) in
      let args = list_zip args l in
      let unused =
        begin try StrMap.find trig_nm unused_trig_args
        with Not_found -> StrSet.empty end in
      let args = List.filter (fun (nm,_) -> not @@ StrSet.mem nm unused) args in
      snd @@ list_unzip args
    (* trigger that's uninvolved in this query *)
    with P.Bad_data _ -> l) agenda_map in

  let c = {
      p;
      shuffle_meta=K3Shuffle.gen_meta p;
      map_type;
      (* collect all map access patterns for creating indexed maps *)
      gen_deletes;
      gen_correctives;
      corr_maps = maps_potential_corrective p;
      sys_init;
      stream_file;
      agenda_map;
      unused_trig_args;
      map_indices = P.map_access_patterns p;
    } in
  (* regular trigs then insert entries into shuffle fn table *)
  let proto_trigs, proto_funcs =
    (fun (a,b) -> List.flatten a, List.flatten b) @@ list_unzip @@
      P.for_all_trigs ~sys_init:true ~deletes:c.gen_deletes c.p @@ gen_dist_for_t c ast
  in
  let prog =
    declare_global_vars c partmap ast @
    declare_global_funcs c partmap ast @
    (if c.gen_correctives then send_corrective_fns c else []) @
    proto_funcs @
    [mk_flow @@
      Proto.triggers c @
      GC.triggers c Proto.sw_check_done @
      TS.triggers Proto.sw_check_done @
      Timer.triggers c @
      [sw_demux c;
       sw_driver_trig c;
       nd_rcv_corr_done c] @
      proto_trigs
    ] @
    roles_of c ast
  in
  snd @@ U.renumber_program_ids prog

