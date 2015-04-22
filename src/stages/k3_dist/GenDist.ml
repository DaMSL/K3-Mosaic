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

exception ProcessingFailed of string;;

(* global trigger names needed for generated triggers and sends *)
let send_fetch_name_of_t trig_nm = "sw_"^trig_nm^"_send_fetch"
let rcv_fetch_name_of_t trig_nm = "nd_"^trig_nm^"_rcv_fetch"
let rcv_put_name_of_t trig_nm = "nd_"^trig_nm^"_rcv_put"
let send_push_name_of_t c trig_nm stmt_id map_id =
  "nd_"^trig_nm^"_send_push_s"^soi stmt_id^"_m_"^P.map_name_of c.p map_id
let rcv_push_name_of_t c trig_nm stmt_id map_id =
  "nd_"^trig_nm^"_rcv_push_s"^soi stmt_id^"_m_"^P.map_name_of c.p map_id
let send_corrective_name_of_t c map_id =
  "nd_"^P.map_name_of c.p map_id^"_send_correctives"
let do_complete_name_of_t trig_nm stmt_id =
  "nd_"^trig_nm^"_do_complete_s"^string_of_int stmt_id
let rcv_corrective_name_of_t c trig_nm stmt_id map_id =
  trig_nm^"_rcv_corrective_s"^string_of_int stmt_id^"_m_"^P.map_name_of c.p map_id
let do_corrective_name_of_t c trig_nm stmt_id map_id =
  trig_nm^"_do_corrective_s"^string_of_int stmt_id^"_m_"^P.map_name_of c.p map_id

(**** global functions ****)

(* write to master log *)
(* TODO: make into ordered map *)
let nd_log_master_write_nm = "nd_log_master_write"
let nd_log_master_write =
  mk_global_fn nd_log_master_write_nm D.nd_log_master.e [] @@
    mk_insert D.nd_log_master.id @@ ids_to_vars @@ fst_many D.nd_log_master.e

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
    (mk_lambda' nd_log_master.e @@ mk_geq (mk_var "vid") @@ mk_var "vid2") @@
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
let nd_add_delta_to_buf c map_id =
  let delta_tuples, lookup_value, update_value = "delta_tuples", "lookup_value", "update_value" in
  let corrective, target_map = "corrective", "target_map" in
  let map_ds = P.map_ds_of_id c.p map_id in
  let map_ds_v = P.map_ds_of_id ~vid:true c.p map_id in
  let id_t_delta = id_t_add "_delta" map_ds.e in
  let vars_delta_unknown =
    P.map_add_v mk_cunknown @@ list_replace_i (-1) mk_cunknown @@ ids_to_vars @@ fst_many id_t_delta in
  let vars_delta_val = list_last @@ ids_to_vars @@ fst_many id_t_delta in
  let vars_no_val = list_drop_end 1 @@ ids_to_vars @@ fst_many map_ds.e in
  let idx = D.make_into_index vars_no_val in
  let vars_v = ids_to_vars @@ fst_many map_ds_v.e in
  let vars_val = list_last vars_v in
  let t_val = snd @@ list_last @@ map_ds.e in
  let id_val = fst @@ list_last @@ map_ds.e in
  let tmap_deref = target_map^"_d" in
  let zero = match t_val.typ with
    | TInt   -> mk_cint 0
    | TFloat -> mk_cfloat 0.
    | _ -> failwith @@ "Unhandled type "^K3PrintSyntax.string_of_type t_val
  in
  let pat e =
    list_replace_i (-1) e @@ modify_e map_ds_v.e ["vid", mk_var "min_vid"]
  in
  let min_vid_pat = pat mk_cunknown in
  let update_vars = pat @@ mk_var update_value in
  let regular_delta =
    mk_let [lookup_value]
      (map_latest_vid_vals ~vid_nm:"min_vid" c (mk_var tmap_deref)
        (Some(vars_no_val @ [mk_cunknown])) map_id ~keep_vid:true) @@
      mk_let [update_value]
        (* get either 0 to add or the value we read *)
        (mk_add (mk_var id_val) @@
          mk_case_ns
            (mk_peek @@ mk_var lookup_value) "val"
            zero @@
            mk_subscript (List.length map_ds_v.e) @@ mk_var "val") @@
        mk_insert tmap_deref update_vars
  in
  mk_global_fn (D.nd_add_delta_to_buf_nm c map_id)
    (* corrective: whether this is a corrective delta *)
    ([target_map, wrap_tind @@ wrap_t_map_idx' c map_id (snd_many map_ds_v.e);
      corrective, t_bool; "min_vid", t_vid;
      delta_tuples, map_ds.t])
    [t_unit] @@
    mk_block @@
      [mk_iter  (* loop over values in delta tuples *)
        (mk_lambda' map_ds.e @@
          (* careful to put bind in proper place *)
          mk_bind (mk_var target_map) tmap_deref @@
          (* this part is just for correctives:
            * We need to check if there's a value at the particular version id
            * If so, we must add the value directly *)
            mk_let [lookup_value]
              (mk_if
                (mk_var corrective)
                (if c.use_multiindex then
                  mk_slice_idx' ~idx ~comp:EQ (mk_var tmap_deref) min_vid_pat
                 else
                  mk_slice' tmap_deref @@
                    list_replace_i (-1) mk_cunknown min_vid_pat) @@
                mk_empty map_ds_v.t) @@
            mk_case_sn
              (mk_peek @@ mk_var lookup_value) "val"
              (* then just update the value *)
              (mk_let [update_value]
                (mk_add (mk_var id_val) @@ mk_subscript (List.length map_ds_v.e) @@ mk_var "val") @@
                mk_update tmap_deref [mk_var "val"] update_vars)
              (* else, if it's just a regular delta, read the frontier *)
              regular_delta) @@
        mk_var delta_tuples
      ;
      (* add to future values *)
      (* TODO: optimize *)
      mk_iter (* loop over values in the delta tuples *)
        (mk_lambda' id_t_delta @@
          mk_let ["filtered"]
          (mk_bind (mk_var target_map) tmap_deref @@
            (* slice for all values > vid with same key *)
            if c.use_multiindex then
              mk_slice_idx' ~idx ~comp:GTA (mk_var tmap_deref) vars_delta_unknown
            else
              mk_filter
                (mk_lambda' map_ds_v.e @@
                  mk_gt (mk_var "vid") @@ mk_var "min_vid") @@
                (* slice w/o vid and value *)
                mk_slice' tmap_deref vars_delta_unknown) @@
          mk_iter
            (mk_lambda' map_ds_v.e @@
              (* careful to put bind in proper place *)
              mk_bind (mk_var target_map) tmap_deref @@
                mk_update tmap_deref
                  (ids_to_vars @@ fst_many @@ map_ds_v.e) @@
                  list_replace_i (-1) (mk_add vars_val vars_delta_val) vars_v) @@
            mk_var "filtered") @@
        mk_var delta_tuples]

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
        mk_convert_col nd_log_master.t (wrap_tlist' @@ snd_many nd_log_master.e) @@
          (* list of triggers >= vid *)
          mk_apply
            (mk_var nd_log_read_geq_nm) @@ mk_var "request_vid"

(**** protocol code ****)

(* The driver trigger: loop over the trigger data structures as long as we have spare vids *)
let sw_driver_trig (c:config) =
  let trig_id = "trig_id" in
  (* dispatch code by trigger ids, for the trig buffers *)
  let dispatch_code =
    List.fold_left (fun acc (id, nm) ->
      (* check if we match on the id *)
      mk_if (mk_eq (mk_var trig_id) @@ mk_cint id)
        (mk_apply' (send_fetch_name_of_t nm) @@ mk_var "vid")
        (* else, continue *)
        acc)
    (mk_error "mismatch on trigger id") @@
    P.for_all_trigs ~deletes:c.gen_deletes c.p (fun x -> P.trigger_id_for_name c.p x, x)
  in
  mk_code_sink' sw_driver_trig_nm unit_arg [] @@
  (* if we're done, do nothing *)
  mk_if (mk_eq (mk_var D.sw_state.id) @@ mk_var D.sw_state_done.id)
    mk_cunit @@
    mk_case_ns (mk_apply' TS.sw_gen_vid_nm mk_cunit) "vid"
      (* if we don't have a vid, set state to waiting for vid *)
      (mk_assign D.sw_state.id @@ mk_var D.sw_state_wait_vid.id) @@
      (* else *)
      mk_pop D.sw_trig_buf_idx.id trig_id
        (* empty: no message to send -- set state to idle *)
        (mk_assign D.sw_state.id @@ mk_var D.sw_state_idle.id) @@
        (* we have a msg *)
        (* if it's the sentry, act *)
        mk_if (mk_eq (mk_var trig_id) @@ mk_cint (-1))
          Proto.sw_seen_sentry @@
          (* else *)
          mk_block [
            (* set state to sending *)
            mk_assign D.sw_state.id @@ mk_var D.sw_state_sending.id;
            (* send the msg using dispatch code *)
            dispatch_code;
            (* recurse, trying to get another message *)
            mk_send sw_driver_trig_nm G.me_var [mk_cunit];
          ]

(* The start trigger puts the message in a trig buffer *)
let sw_start_fn (c:config) trig =
  let args_t = snd_many @@ P.args_of_t c.p trig in
  let args = ["args", wrap_ttuple args_t] in
  mk_global_fn ("sw_"^trig) args [] @@
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
  let sentry_code =
    (* stash the sentry index in the queue *)
    mk_if (mk_eq (mk_fst @@ mk_var "args") (mk_cint @@ -1))
      (mk_block [
        mk_insert D.sw_trig_buf_idx.id [mk_cint @@ -1];
        mk_incr TS.sw_need_vid_ctr.id; ]) @@
      mk_error @@ "unidentified trig id"
  in
  mk_code_sink' sw_demux_nm ["args", wrap_ttuple combo_t] [] @@
  List.fold_right (fun (t_id, _, arg_indices) acc ->
    mk_if (mk_eq (mk_fst @@ mk_var "args") @@ mk_cint t_id)
      (mk_apply' ("sw_"^P.trigger_name_for_id c.p t_id) @@
        (* add 1 for tuple access *)
        mk_tuple @@ List.map (fun i -> mk_subscript (i+1) @@ mk_var "args") arg_indices)
      acc)
    t_arg_map
    sentry_code

(* sending fetches is done from functions *)
(* each function takes a vid to plant in the arguments, which are in the trig buffers *)
let sw_send_fetch_fn c s_rhs_lhs s_rhs trig_name =
  let send_fetches_of_rhs_maps  =
    if null s_rhs then []
    else
    [mk_iter
      (mk_lambda'
        ["ip", t_addr; "stmt_map_ids", wrap_tbag' [t_stmt_id; t_map_id]] @@
        mk_send (rcv_fetch_name_of_t trig_name) (mk_var "ip") @@
          mk_var "stmt_map_ids" ::
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
                  mk_tuple @@ (mk_cint rhs_map_id)::key)
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
                mk_tuple @@ (mk_cint lhs_map_id)::key
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
    let stmt_id_cnt_type = wrap_tbag' [t_stmt_id; t_int] in
    (* send puts
    * count is generated by counting the number of messages going to a
    * specific IP *)
    [mk_iter
      (mk_lambda'
        [address, t_addr; "stmt_id_cnt_list", stmt_id_cnt_type] @@
        mk_block @@
          (* send rcv_put *)
          (mk_send
            (rcv_put_name_of_t trig_name)
            (mk_var address) @@
            G.me_var :: mk_var "stmt_id_cnt_list"::
              args_of_t_as_vars_with_v c trig_name) ::
          if c.enable_gc then
            [GC.sw_ack_init_code ~addr_nm:address ~vid_nm:"vid"] else []
      ) @@
      mk_gbagg
        (mk_assoc_lambda' (* grouping func -- assoc because of gbagg tuple *)
          ["ip", t_addr; "stmt_id", t_stmt_id]
          ["count", t_int] @@
          mk_var "ip"
        )
        (mk_assoc_lambda' (* agg func *)
          ["acc", stmt_id_cnt_type]
          ["ip_and_stmt_id", wrap_ttuple [t_addr; t_stmt_id]; "count", t_int] @@
          mk_let (* break up because of the way inner gbagg forms tuples *)
            ["ip"; "stmt_id"] (mk_var "ip_and_stmt_id") @@
            mk_combine
              (mk_var "acc") @@
              mk_singleton
                stmt_id_cnt_type
                [mk_var "stmt_id"; mk_var "count"])
        (mk_empty @@ wrap_tbag' [t_stmt_id; t_int]) @@
        mk_gbagg (* inner gba *)
          (mk_lambda' (* group func *)
            ["ip", t_addr; "stmt_id", t_stmt_id; "count", t_int] @@
            mk_tuple [mk_var "ip"; mk_var "stmt_id"]
          )
          (mk_assoc_lambda' (* agg func *)
            ["acc", t_int]
            ["ip", t_addr; "stmt_id", t_stmt_id; "count", t_int] @@
            mk_add
              (mk_var "acc") @@
              mk_var "count"
          )
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
              let tuple_types = wrap_t_of_map' rhs_map_types in
              mk_combine
                acc_code @@
                mk_let ["sender_count"]
                  (* count up the number of IPs received from route *)
                  (mk_agg
                    (mk_lambda'
                      ["count", t_int; "ip", t_addr] @@
                      mk_add (mk_var "count") @@ mk_cint 1)
                    (mk_cint 0) @@
                    mk_apply
                      (mk_var route_fn) @@ mk_tuple @@ mk_cint rhs_map_id::route_key) @@
                mk_map
                  (mk_lambda'
                    ["ip", t_addr; "tuples", tuple_types] @@
                      mk_tuple [mk_var "ip"; mk_cint stmt_id; mk_var "sender_count"]) @@
                  mk_apply
                    (mk_var shuffle_fn) @@
                    mk_tuple @@
                        shuffle_key@
                        [mk_empty tuple_types;
                        mk_cbool true]
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
    (* pop an argument out of the buffers *)
    mk_pop buf "args"
      (mk_error @@ "unexpected missing arguments in "^buf) @@
      (* decompose args *)
      mk_let (fst_many @@ P.args_of_t c.p trig_name)
        (mk_var "args") @@
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
    (("stmts_and_map_ids", wrap_t_of_map' [t_stmt_id; t_map_id])::
      args_of_t_with_v c trig)
    [] @@ (* locals *)
    mk_block [
      (* save the bound variables for this trigger so they're available later *)
      mk_apply
        (mk_var @@ nd_log_write_for c trig) @@
        mk_tuple @@
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
        mk_var "stmts_and_map_ids" ]

(* Receive Put trigger
 * --------------------------------------- *
 * Update the statement counters with the received values
 * If the counter is 0, we need to call do_complete
 *)
let nd_rcv_put_trig c trig_name =
mk_code_sink'
  (rcv_put_name_of_t trig_name)
  (["sender_ip", t_addr; "stmt_id_cnt_list", wrap_t_of_map' [t_stmt_id; t_int]]@
      (args_of_t_with_v c trig_name))
  [] @@
  mk_block @@
    (mk_iter
      (mk_lambda'
        ["stmt_id", t_stmt_id; "count", t_int] @@
        mk_if
          (mk_apply' nd_check_stmt_cntr_index_nm @@
            mk_tuple [mk_var "vid"; mk_var "stmt_id"; mk_var "count"])
          (* we need to create a possible send for each statement in the trigger *)
          (List.fold_left (fun acc_code stmt_id ->
            mk_if
              (mk_eq (mk_var "stmt_id") @@ mk_cint stmt_id)
              (mk_apply'
                (do_complete_name_of_t trig_name stmt_id) @@
                mk_tuple @@ args_of_t_as_vars_with_v c trig_name) @@
              acc_code)
            mk_cunit @@
            P.stmts_of_t c.p trig_name) @@
          mk_cunit) @@
      mk_var "stmt_id_cnt_list") ::
      (if c.enable_gc then [GC.nd_ack_send_code ~addr_nm:"sender_ip" ~vid_nm:"vid"] else [])


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
        mk_bind (mk_var rhs_map_name) rhsm_deref @@
        mk_block [
          (* save this particular statement execution in the master log
           * Note that we need to do it here to make sure nothing
           * else can stop us before we send the push *)
          mk_apply
            (mk_var nd_log_master_write_nm) @@
            mk_tuple [mk_var "vid"; mk_cint stmt_id] ;
          mk_iter
            (mk_lambda'
              ["ip",t_addr;"tuples", wrap_t_of_map' rhs_map_types] @@
              mk_send
                (rcv_push_name_of_t c trig_name stmt_id rhs_map_id)
                (mk_var "ip") @@
                mk_var "tuples" :: args_of_t_as_vars_with_v c trig_name) @@
            mk_apply
              (mk_var shuffle_fn) @@
              mk_tuple @@
                partial_key @
                (* we need the latest vid data that's less than the current vid *)
                [D.map_latest_vid_vals c (mk_var rhsm_deref)
                  (some slice_key) rhs_map_id ~keep_vid:true; mk_ctrue]]
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
    let rbuf_deref = rbuf_name^"_d" in
    let tuple_types = P.map_types_with_v_for c.p read_map_id in
    (* remove value from tuple so we can do a slice *)
    let tuple_id_t = types_to_ids_types "_tup" tuple_types in
    let tuple_vars_no_val =
      ids_to_vars @@ (list_drop_end 1 @@ fst_many tuple_id_t) @ ["_"] in
    acc_code @
    [mk_code_sink'
      (rcv_push_name_of_t c trig_name stmt_id read_map_id)
      (("tuples", wrap_t_of_map' tuple_types)::
        args_of_t_with_v c trig_name)
      [] @@ (* locals *)
      (* save the tuples *)
      mk_block
        (* save the bound variables for this vid. This is necessary for both the
         * sending and receiving nodes, which is why we're also doing it here *)
        [mk_apply' (nd_log_write_for c trig_name) @@
          mk_tuple @@ args_of_t_as_vars_with_v c trig_name
        ;
         mk_iter
          (mk_lambda'
            ["tuple", wrap_ttuple tuple_types] @@
            (* be very careful with bind placement *)
            mk_bind (mk_var rbuf_name) rbuf_deref @@
            mk_let (fst_many tuple_id_t) (mk_var "tuple") @@
            mk_case_sn
              (mk_peek @@ mk_slice' rbuf_deref tuple_vars_no_val) "vals"
              (mk_update rbuf_deref [mk_var "vals"] [mk_var "tuple"]) @@
              mk_insert rbuf_deref [mk_var "tuple"]) @@
          mk_var "tuples" ;
         (* update and check statment counters to see if we should send a do_complete *)
         mk_if
           (mk_apply' nd_check_stmt_cntr_index_nm @@
             mk_tuple [mk_var "vid"; mk_cint stmt_id; mk_cint @@ -1])
           (* Send to local do_complete *)
           (mk_apply' (do_complete_name_of_t trig_name stmt_id) @@
             mk_tuple @@ args_of_t_as_vars_with_v c trig_name)
           mk_cunit ] ])
  [] s_rhs


(* list of trig, stmt with a map on the rhs that's also on the lhs. These are
 * the potential corrective maps *)
let maps_potential_corrective c =
  let lhs_maps = ListAsSet.uniq @@ P.for_all_stmts c.p @@ P.lhs_map_of_stmt c.p in
  let rhs_maps = ListAsSet.uniq @@ List.flatten @@
    P.for_all_stmts c.p @@ P.rhs_maps_of_stmt c.p in
  ListAsSet.inter lhs_maps rhs_maps

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
    let map_ds = P.map_ds_of_id c.p map_id in
    let tuple_type = wrap_ttuple @@ snd_many map_ds.e in
    let delta_tuples2 = {(P.map_ds_of_id ~vid:true c.p map_id) with id="delta_tuples2"}
    in
    singleton @@ mk_global_fn (send_corrective_name_of_t c map_id)
    (orig_vals @ ["corrective_vid", t_vid; "delta_tuples", map_ds.t])
    (* return the number of new correctives generated *)
    [t_int] @@
    (* the corrective list tells us which statements were fetched
     * from us and when *)
    mk_let ["corrective_list"] (* (stmt_id * vid list) list *)
      (mk_apply'
        nd_filter_corrective_list_nm @@
        mk_tuple (* feed in list of possible stmts *)
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
        (mk_map (mk_lambda' map_ds.e @@ mk_tuple @@
                  (mk_var g_min_vid.id)::(ids_to_vars @@ fst_many map_ds.e)) @@
          mk_var "delta_tuples") @@
      mk_agg
        (mk_assoc_lambda'
          ["acc_count", t_int] ["stmt_id", t_stmt_id; "vid_list", t_vid_list] @@
          List.fold_left
            (* loop over all possible read map matches *)
            (fun acc_code (target_trig, target_stmt) ->
              (* we already have the map vars for the rhs map in the
                * tuples. Now we try to get some more for the lhs map *)
              let target_map = P.lhs_map_of_stmt c.p target_stmt in
              let key = P.partial_key_from_bound c.p target_stmt target_map in
              let shuffle_fn = K3Shuffle.find_shuffle_nm c target_stmt map_id target_map in
              mk_if (* if match, send data *)
                (mk_eq (mk_var "stmt_id") @@ mk_cint target_stmt)
                (* save the grouped sends *)
                (mk_let ["ips_vids"]
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
                                (mk_lambda' ["_", t_unit; "_", tuple_type] mk_cunit)
                                mk_cunit @@
                                mk_combine (mk_var "acc_tuples") @@ mk_var "tuples"]])
                    (mk_tuple [mk_empty t_vid_list; mk_empty map_ds.t]) @@
                    mk_flatten @@ mk_map
                      (mk_lambda' ["vid", t_vid] @@
                        (* get bound vars from log so we can calculate shuffle *)
                        mk_let
                          (fst_many @@ P.args_of_t c.p target_trig)
                          (mk_apply
                            (mk_var @@ nd_log_get_bound_for target_trig) @@
                            mk_var "vid") @@
                        (* insert vid into the ip, tuples output of shuffle *)
                        mk_map (* (ip * vid * tuple list) list *)
                          (mk_lambda' ["ip", t_addr; "tuples", delta_tuples2.t] @@
                              mk_tuple [mk_var "ip"; mk_var "vid";
                                (* get rid of vid NOTE: to reduce number of shuffles *)
                                  mk_map (mk_lambda' delta_tuples2.e @@
                                      mk_tuple @@ ids_to_vars @@ fst_many map_ds.e) @@
                                    mk_var "tuples"]) @@
                          mk_apply'
                            shuffle_fn @@ mk_tuple @@
                              (* (ip * tuple list) list *)
                              key @ [mk_var "delta_tuples2"; mk_cbool false]) @@
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
                    (mk_var "acc_count") @@
                    mk_var "ips_vids")
                acc_code)
            (mk_var "acc_count") (* base case *)
            trigs_stmts_with_matching_rhs_map)
            (* base number of msgs *)
          (mk_cint 0) @@
          mk_var "corrective_list")
    in
    List.flatten @@ List.map send_correctives @@ maps_potential_corrective c

(* function to update the stmt_counters for correctives *)
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
      (* we need to decrement the hop's value by 1, and increment the next hop's values by the count *)
      mk_upsert_with nd_stmt_cntrs "lkup" ~k:lookup_pat
        ~default:(mk_error @@ Printf.sprintf "%s: missing stmt_cntrs value" nd_update_stmt_cntr_corr_map_nm)
        ~v:(mk_let [nd_stmt_cntrs_corr_map.id] (mk_snd @@ mk_snd @@ mk_var "lkup") @@
              mk_block [
                (* increment the next hop by count *)
                mk_upsert_with nd_stmt_cntrs_corr_map "lkup2"
                  (* if no value, set it to the count *)
                  ~k:[mk_var "hop"] ~default:(mk_var "count")
                  (* otherwise, add the count *)
                  ~v:(mk_add (mk_snd @@ mk_var "lkup2") @@ mk_var "count");
                (* check if this is from the root of the corrective tree *)
                mk_if (mk_var "root")
                  (* return as is *)
                  mk_cunit @@
                  (* else if the current hop is 0, delete it *)
                  mk_case_ns (mk_peek @@ mk_slice' nd_stmt_cntrs_corr_map.id [mk_var "hop"; mk_cunknown]) "lkup2"
                    (* if no entry, set to -1 *)
                    (mk_insert nd_stmt_cntrs_corr_map.id [mk_var "hop"; mk_cint @@ -1]) @@
                    (* else, calculate new corr count *)
                    mk_let ["new_corr_cnt"] (mk_sub (mk_snd @@ mk_var "lkup2") @@ mk_cint 1) @@
                    (* if we've hit 0 *)
                    mk_if (mk_eq (mk_var "new_corr_cnt") @@ mk_cint 0)
                      (* delete the entry altogether *)
                      (mk_delete nd_stmt_cntrs_corr_map.id @@ [mk_var "lkup2"]) @@
                      (* else, save the decremented entry *)
                      mk_update nd_stmt_cntrs_corr_map.id [mk_var "lkup2"] [mk_var "hop"; mk_var "new_corr_cnt"];
                (* set the new tuple *)
                mk_tuple [mk_fst @@ mk_snd @@ mk_var "lkup"; mk_var nd_stmt_cntrs_corr_map.id]
              ])
    ]

(* call from do_complete when done to check if fully done *)
let nd_complete_stmt_cntr_check_nm = "nd_complete_stmt_cntr_check"
let nd_complete_stmt_cntr_check =
  mk_global_fn nd_complete_stmt_cntr_check_nm ["vid", t_vid; "stmt_id", t_stmt_id] [] @@
  (* if we have nothing to send, we can delete our stmt_cntr entry right away *)
  mk_block [
    mk_delete_one nd_stmt_cntrs [mk_tuple [mk_var "vid"; mk_var "stmt_id"]; mk_cunknown];
    (* check if we're done *)
    Proto.nd_delete_stmt_cntr;
  ]

(*
 * shared coded btw do_complete and do_corrective
 * we add the delta to all following vids
 * *)
let do_add_delta c e lmap ~corrective =
  mk_apply' (D.nd_add_delta_to_buf_nm c lmap) @@ mk_tuple @@
    [mk_var @@ P.map_name_of c.p lmap;
      if corrective then mk_ctrue else mk_cfalse; mk_var "vid"; e]

(* trigger versions of do_complete: only for stmts with no rhs maps *)
let nd_do_complete_trigs c trig_name =
  let do_complete_trig stmt_id =
    let comp_nm = do_complete_name_of_t trig_name stmt_id in
    let args = args_of_t_with_v c trig_name in
    mk_code_sink' (comp_nm^"_trig") args [] @@
    mk_apply' comp_nm @@ mk_tuple @@ ids_to_vars @@ fst_many args;
in
List.map do_complete_trig @@ P.stmts_without_rhs_maps_in_t c.p trig_name

(* function versions of do_complete *)
let nd_do_complete_fns c ast trig_name =
  (* @has_rhs: whether this statement has rhs maps *)
  let do_complete_fn has_rhs stmt_id =
    mk_global_fn (do_complete_name_of_t trig_name stmt_id) (args_of_t_with_v c trig_name) [] @@
    let lmap = P.lhs_map_of_stmt c.p stmt_id in
    let fst_hop = mk_cint 1 in
    let after_fn tup_ds =
      mk_block [
        (* add delta *)
        do_add_delta c tup_ds lmap ~corrective:false;
        (if c.gen_correctives && List.exists ((=) lmap) @@ maps_potential_corrective c
        then
          let send_corr_t = send_corrective_name_of_t c lmap in
          mk_let ["sent_msgs"]
            (* we apply send_correctives with our original address, stmt_id, original vid and hop
             * we double up on vid since send_correctives is also called for do_corrective,
             * which must send the new vid to be calculated as well as the original complete's vid
             *)
            (mk_apply' send_corr_t @@
              mk_tuple @@ [G.me_var; mk_cint stmt_id; mk_var "vid"; fst_hop; mk_var "vid"; tup_ds]) @@
            (* update the corrective counters for hop 1 to the number of msgs.
             * true: is a root, bool: create an entry *)
            let update_corr_code create =
              mk_apply' nd_update_stmt_cntr_corr_map_nm @@
                mk_tuple [mk_var "vid"; mk_cint stmt_id; fst_hop; mk_var "sent_msgs"; mk_ctrue; mk_cbool create]
            in
            if has_rhs then
              mk_if (mk_eq (mk_var "sent_msgs") @@ mk_cint 0)
                (* if our sent_msgs is 0, we need to delete the stmt cntr entry *)
                (mk_apply' nd_complete_stmt_cntr_check_nm @@ mk_tuple [mk_var "vid"; mk_cint stmt_id]) @@
                (* otherwise we need to update the corrective counters *)
                update_corr_code false
            else
              (* if we have no rhs maps, we may not need to update anything, since no stmt cntr entry was created *)
              mk_if (mk_eq (mk_var "sent_msgs") @@ mk_cint 0)
                mk_cunit @@
                (* else, update and create a stmt counter *)
                update_corr_code true
        (* no correctives are possible *)
        else
          (* if we have no rhs maps, do nothing *)
          if not has_rhs then mk_cunit
          else mk_apply' nd_complete_stmt_cntr_check_nm @@ mk_tuple [mk_var "vid"; mk_cint stmt_id])
      ]
    in
    M.modify_ast_for_s c ast stmt_id trig_name after_fn
in
(List.map (do_complete_fn true)  @@ P.stmts_with_rhs_maps_in_t c.p trig_name) @
(List.map (do_complete_fn false) @@ P.stmts_without_rhs_maps_in_t c.p trig_name)

(* rcv notification of a corrective finish from other nodes *)
let nd_rcv_corr_done_nm = "nd_rcv_corr_done"
let nd_rcv_corr_done =
  let lookup_pat = [mk_tuple [mk_var "vid"; mk_var "stmt_id"]; mk_cunknown] in
  let args = ["vid", t_vid; "stmt_id", t_stmt_id; "hop", t_int; "count", t_int] in
  mk_code_sink' nd_rcv_corr_done_nm args [] @@
    mk_block [
      (* update the corrective map. false: not a root of the corrective tree *)
      mk_apply' nd_update_stmt_cntr_corr_map_nm @@ mk_tuple @@ (ids_to_vars @@ fst_many args) @ [mk_cfalse; mk_cfalse];
      (* check if the corr_cnt structure is empty. If so, we can delete the whole entry *)
      mk_case_ns (mk_lookup' nd_stmt_cntrs.id lookup_pat) "lkup"
        (mk_error @@ nd_rcv_corr_done_nm^": expected stmt_cntr value") @@
        (* if the corr_cnt map is empty *)
        mk_is_empty (mk_snd @@ mk_snd @@ mk_var "lkup")
          ~n:mk_cunit
          ~y:(mk_block [
              (* delete the whole stmt_cntr entry *)
              mk_delete nd_stmt_cntrs.id [mk_var "lkup"];
              Proto.nd_delete_stmt_cntr])
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
         "delta_tuples", wrap_t_of_map' @@ P.map_types_for c.p rmap])
      [] @@ (* locals *)
      mk_block [
        (* accumulate delta for this vid and all following vids. This is a very
         * sensitive point in the protocol and it's essential this only be done
         * once for a given corrective *)
        mk_apply'
          (D.nd_add_delta_to_buf_nm c rmap) @@
            (* pass the map indirection, false=not corrective *)
            mk_tuple [mk_var buf_map_nm; mk_cbool false; mk_var "vid";
                      mk_var "delta_tuples"];
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
                  (mk_let (fst_many @@ P.args_of_t c.p trig_name)
                    (mk_apply'
                      (nd_log_get_bound_for trig_name) @@ mk_var "compute_vid") @@
                    (* do_corrective, return number of msgs *)
                    mk_add (mk_var "acc_count") @@
                      mk_apply'
                        (do_corrective_name_of_t c trig_name stmt_id rmap) @@
                        mk_tuple @@
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

(* do corrective triggers *)
(* very similar to do_complete, but we only have to do it for certain
 * (stmt, map) combinations.
 * Do_corrective , unlike do_complete, doesn't need to add to an earlier vid
 * value. Instead, it adds to the value that already resides at a specific
 * vid and propagates. *)
let nd_do_corrective_fns c s_rhs ast trig_name corrective_maps =
  let do_corrective_fn (stmt_id, map_id) =
    let tuple_typ = wrap_t_of_map' @@ P.map_types_for c.p map_id in
    mk_global_fn (do_corrective_name_of_t c trig_name stmt_id map_id)
      (orig_vals @ args_of_t_with_v c trig_name @ ["delta_tuples", tuple_typ])
      [t_int] @@
        let lmap = P.lhs_map_of_stmt c.p stmt_id in
        let lmap_id_t = P.map_ids_types_for c.p lmap in
        let send_corr_fn = send_corrective_name_of_t c lmap in
        let args, ast =
          M.modify_corr_ast c ast map_id stmt_id trig_name id_fn
        in
        mk_let ["new_tuples"]
          (* filter out delta tuples that have 0 value *)
          (mk_filter (mk_lambda' lmap_id_t @@
            mk_neq (mk_cint 0) @@ mk_var @@ fst @@ list_last lmap_id_t) @@
            mk_flatten @@ mk_map (mk_lambda' args ast) @@ mk_var "delta_tuples") @@
              mk_block [
                (* add delta *)
                do_add_delta c (mk_var "new_tuples") lmap ~corrective:true;
                (* send correctives *)
                if List.exists ((=) lmap) corrective_maps
                (* send correctives with hop + 1, and return the num of correctives *)
                then mk_apply' send_corr_fn @@ mk_tuple @@
                      (modify_e orig_vals ["hop", mk_add (mk_cint 1) @@ mk_var "hop"]) @
                      [mk_var "vid"; mk_var "new_tuples"]
                (* if we have no more correctives, return 0 *)
                else mk_cint 0
              ]
  in
  List.map do_corrective_fn s_rhs

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
      Handle(wrap_ttuple @@ fst @@ combine_trig_args c,
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

(* Generate all the frontier functions *)
let emit_frontier_fns c =
  (* get a representative map of each type *)
  let fns = List.map (hd |- snd) @@ P.uniq_types_and_maps c.p in
  List.map (frontier_fn c) fns

let declare_global_vars c partmap ast =
  Proto.global_vars @
  D.global_vars c (ModifyAst.map_inits_from_ast c ast) @
  Timer.global_vars @
  TS.global_vars @
  K3Ring.global_vars @
  K3Route.global_vars c.p partmap @
  begin if c.enable_gc then GC.global_vars c else [] end

let declare_global_funcs c partmap ast =
  nd_log_master_write ::
  (P.for_all_trigs ~deletes:c.gen_deletes c.p @@ nd_log_write c) @
  (P.for_all_trigs ~deletes:c.gen_deletes c.p @@ nd_log_get_bound c) @
  nd_log_read_geq ::
  nd_check_stmt_cntr_index ::
  nd_complete_stmt_cntr_check ::
  nd_update_stmt_cntr_corr_map ::
  begin if c.gen_correctives then [nd_filter_corrective_list] else [] end @
  K3Ring.functions @
  begin if c.use_multiindex then [] else emit_frontier_fns c end @
  (List.map (nd_add_delta_to_buf c |- hd |- snd) @@ P.uniq_types_and_maps c.p) @
  TS.functions @
  K3Route.functions c.p partmap @
  K3Shuffle.functions c

(* Generate all the code for a specific trigger *)
let gen_dist_for_t c ast trig corr_maps =
  (* (stmt_id, rhs_map_id)list *)
  let s_rhs = P.s_and_over_stmts_in_t c.p P.rhs_maps_of_stmt trig in
  (* stmts that can be involved in correctives *)
  let s_rhs_corr = List.filter (fun (s, map) -> List.mem map corr_maps) s_rhs in
  (* (stmt_id,rhs_map_id,lhs_map_id) list *)
  let s_rhs_lhs = P.s_and_over_stmts_in_t c.p P.rhs_lhs_of_stmt trig in
  (* functions *)
  let functions = [
    sw_start_fn c trig;
    sw_send_fetch_fn c s_rhs_lhs s_rhs trig;
  ] @
   nd_do_complete_fns c ast trig @
   (if c.gen_correctives then
     nd_do_corrective_fns c s_rhs_corr ast trig corr_maps
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
let gen_dist ?(use_multiindex=false)
             ?(enable_gc=false)
             ?(stream_file="XXX")
             ?(gen_deletes=true)
             ?(gen_correctives=true)
             p partmap ast =
  (* collect all map access patterns for creating indexed maps *)
  let c = {
      p;
      shuffle_meta=K3Shuffle.gen_meta p;
      mapn_idxs=StrMap.empty;
      use_multiindex;
      enable_gc;
      map_idxs = M.get_map_access_patterns_ids p ast;
      stream_file;
      gen_deletes;
      gen_correctives;
    } in
  let potential_corr_maps = maps_potential_corrective c in
  (* regular trigs then insert entries into shuffle fn table *)
  let proto_trigs, proto_funcs =
    (fun (a,b) -> List.flatten a, List.flatten b) @@ list_unzip @@
      P.for_all_trigs ~deletes:c.gen_deletes c.p @@
        fun t -> gen_dist_for_t c ast t potential_corr_maps
  in
  let prog =
    declare_global_vars c partmap ast @
    declare_global_funcs c partmap ast @
    (if c.gen_correctives then send_corrective_fns c else []) @
    proto_funcs @
    [mk_flow @@
      Proto.triggers c @
      (if c.enable_gc then GC.triggers c Proto.sw_check_last_ack else []) @
      TS.triggers @
      Timer.triggers c @
      [sw_demux c;
       sw_driver_trig c;
       nd_rcv_corr_done] @
      proto_trigs
    ] @
    roles_of c ast
  in
  snd @@ U.renumber_program_ids prog

