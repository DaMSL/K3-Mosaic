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
module K3S = K3Shuffle

let str_of_date_t t = match t.typ with
  | TDate -> {t with typ = TString}
  | x -> t

(**** global functions ****)

(* write to master log *)
let nd_log_master_write_nm = "nd_log_master_write"
let nd_log_master_write =
  let ds_idt = ds_e D.nd_log_master in
  let ds_ts  = snd_many ds_idt in
  let fn_idt = ["wstmt_id", t_stmt_id; "wvid", t_vid] in
  mk_global_fn nd_log_master_write_nm fn_idt [] @@
    mk_upsert_with D.nd_log_master.id [mk_var "wstmt_id"; mk_cunknown]
      (mk_lambda'' unit_arg @@ mk_tuple
         [mk_var "wstmt_id"; mk_singleton (wrap_tsortedset' [t_vid]) [mk_var "wvid"]])
      (mk_lambda' ["x", wrap_ttuple ds_ts] @@
         mk_insert_block "x" ~path:[2] [mk_var "wvid"])

(* log_write - save the trigger's arguments *)
let nd_log_write_for p trig_nm = "nd_log_write_"^trig_nm (* varies with bound *)
let nd_log_write c t =
  mk_global_fn (nd_log_write_for c.p t) (args_of_t_with_v c t) [] @@
  (* write bound to trigger_specific log *)
  mk_insert (D.nd_log_for_t t) [mk_var "vid"; mk_tuple @@ args_of_t_as_vars c t]

(* get bound args *)
let nd_log_get_bound_for trig_nm = "nd_log_get_bound_"^trig_nm
let nd_log_get_bound c t =
  (* create a pattern for selecting vid alone *)
  let pat = [mk_var "vid"; mk_cunknown] in
  mk_global_fn (nd_log_get_bound_for t)
    ["vid", t_vid]
    (arg_types_of_t c t) @@
    mk_snd @@ mk_peek_or_error "failed to find log" @@
      mk_slice' (D.nd_log_for_t t) pat

(* function to check to see if we should execute a do_complete *)
(* params: vid, stmt, count_to_change *)
let nd_complete_stmt_cntr_check_nm = "nd_complete_stmt_cntr_check"

let nd_check_stmt_cntr_index_nm = "nd_check_stmt_cntr_index"
let nd_check_stmt_cntr_index c =
  let lookup = "lookup_value" in
  let part_pat = ["vid", t_vid; "stmt_id", t_stmt_id] in
  let part_pat_as_vars = [mk_tuple @@ ids_to_vars @@ fst_many part_pat] in
  let query_pat = part_pat_as_vars @ [mk_cunknown] in
  let add_to_count, new_count, has_data, new_modify =
    "add_to_count", "new_count", "has_data", "new_modify" in
  (* lmap -> stmts *)
  let h = Hashtbl.create 20 in
  List.iter (fun (s,m) ->
    hashtbl_replace h m @@ function None -> [s] | Some s' -> s::s') @@
    P.stmts_lhs_maps c.p;
  let lmap_stmts = list_of_hashtbl h in
  mk_global_fn nd_check_stmt_cntr_index_nm
    (part_pat @ [add_to_count, t_int; has_data, t_bool])
    [t_bool] @@ (* return whether we should send the do_complete *)
    (* check if the counter exists *)
    mk_case_sn
      (mk_peek @@ mk_slice' nd_stmt_cntrs.id query_pat) lookup
      (* calculate the new modify state *)
      (mk_let [new_modify]
        (mk_or (mk_var has_data) @@ mk_snd @@ mk_snd @@ mk_var lookup) @@
      (* calculate new_count *)
       mk_let [new_count]
        (mk_add (mk_var add_to_count) @@ mk_fst @@ mk_snd @@ mk_var lookup) @@
        mk_block [
          (* upate the counter *)
          mk_update nd_stmt_cntrs.id [mk_var lookup] @@
            part_pat_as_vars @ [mk_tuple
              [mk_var new_count; mk_var new_modify;
                mk_thd @@ mk_snd @@ mk_var lookup]]
          ;
          (* if counter is 0 and no modify, we need to delete here since do_complete
           * will never execute *)
          mk_if (mk_and (mk_eq (mk_var new_count) @@ mk_cint 0) @@
            mk_eq (mk_var new_modify) @@ mk_cfalse)
              (mk_apply' nd_complete_stmt_cntr_check_nm
                  [mk_var "vid"; mk_var "stmt_id"])
              mk_cunit
          ;
          (* return whether the counter is 0 and we have some data *)
          mk_and (mk_eq (mk_cint 0) @@ mk_var new_count) @@ mk_var new_modify]) @@
      (* else: no value in the counter *)
      mk_block [
        (* Initialize *)
        mk_insert nd_stmt_cntrs.id @@ part_pat_as_vars @
          [mk_tuple [mk_var add_to_count; mk_var has_data; mk_empty nd_stmt_cntrs_corr_map.t]];
        (* For no-corrective mode, add per-map *)
        mk_if (mk_var D.corrective_mode.id) mk_cunit @@
          List.fold_left (fun acc (m, ss) ->
            let mk_check_s s = mk_eq (mk_var "stmt_id") @@ mk_cint s in
            (* if we have any of the stmts with a given lmap *)
            mk_if
              (list_fold_to_last
                (fun acc s -> mk_or (mk_check_s s) acc) mk_check_s ss)
              (* add to per-map stmt counters *)
              (mk_upsert_with D.nd_stmt_cntrs_per_map.id [mk_cint m; mk_cunknown]
                (mk_lambda' unit_arg @@
                  mk_tuple [mk_cint m;
                    mk_singleton D.nd_stmt_cntrs_per_map_inner.t
                      [mk_var "vid"; mk_var "stmt_id"]]) @@
                mk_lambda' ["x", t_of_e D.nd_stmt_cntrs_per_map.e] @@
                  mk_insert_block "x" ~path:[2] @@ [mk_var "vid"; mk_var "stmt_id"])
              acc)
          mk_cunit
          lmap_stmts;
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
(* We have to get the poly,idx,offset triple and get the delta tuples out of that *)
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
  let zero =
    let t_val = snd @@ get_val real_pat in
    match t_val.typ with
    | TInt   -> mk_cint 0
    | TFloat -> mk_cfloat 0.
    | _ -> failwith @@ "Unhandled type "^K3PrintSyntax.string_of_type t_val
  in
  let regular_read =
    mk_upsert_with_before "acc"
      (D.unknown_val real_delta_pat)
      (mk_lambda'' unit_arg @@ mk_tuple @@ drop_vid real_delta_pat) @@
        mk_lambda' (ds_e map_real) @@
        mk_tuple @@ drop_vid @@ new_val real_delta_pat @@
          mk_add (get_val real_delta_pat) @@ get_val' real_pat
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
             mk_block [
                mk_update_suffix "acc" real_delta_pat @@
                  mk_lambda2' ["v", t_vid] (ds_e map_real) @@
                    mk_tuple @@
                      new_val (drop_vid' real_pat) @@
                        mk_add (get_val' real_pat) @@
                          get_val real_delta_pat
                ;
               (* this part is just for correctives:
                * We need to check if there's a value at the particular version id
                * If so, we must add the value directly *)
                mk_if
                  (mk_var corrective)
                  (* corrective case *)
                  (mk_let ["corr_do_update"; "val"]
                    (mk_case_sn
                      (mk_peek @@ mk_slice' "acc" @@
                        D.unknown_val real_delta_pat) "pval"
                      (mk_tuple [mk_ctrue; mk_var "pval"])
                      (mk_tuple [mk_cfalse; mk_tuple @@
                                  new_val (drop_vid real_delta_pat) zero]))
                    (mk_if (mk_var "corr_do_update")
                      (* then just update the value *)
                      (mk_update "acc"
                          [mk_var "val"] @@
                          D.new_val real_delta_pat @@
                            mk_add (get_val' @@ real_pat_f @@ mk_var "val") @@
                                    get_val' delta_pat)
                      (* in the else case, we need to still do a regular read because
                      * there may not have been an initial write due to empty lookups on rhs
                      * maps *)
                      regular_read)) @@
                  (* non-corrective so do regular read *)
                  regular_read
                ;
                mk_var "acc"])
      (mk_var tmap_deref) @@
    mk_var delta_tuples

(*
 * Return list of corrective statements we need to execute by checking
 * which statements were performed after time x that used a particular map
 * The only reason for this function is that we may have the same stmt
 * needing to execute with different vids
 *)
let nd_filter_corrective_list_nm = "nd_filter_corrective_list"
let nd_filter_corrective_list =
  let trig_stmt_list =
    let e = ["trig_id", t_trig_id; "t_stmt_id", t_stmt_id] in
    create_ds "trig_stmt_list" ~e @@ wrap_tbag' @@ snd_many e
  in
  mk_global_fn nd_filter_corrective_list_nm
  ["request_vid", t_vid; trig_stmt_list.id, trig_stmt_list.t]
  [nd_log_master.t] @@
  mk_equijoin
    (mk_var nd_log_master.id)
    (mk_var trig_stmt_list.id)
    (mk_lambda' nd_log_master.e @@ mk_var "stmt_id")
    (mk_lambda' trig_stmt_list.e @@ mk_var "t_stmt_id")
    (mk_lambda3' ["acc", nd_log_master.t] nd_log_master.e trig_stmt_list.e @@
     mk_insert_block "acc"
       [mk_var "stmt_id"; mk_filter_geq (mk_var "vid_set") [mk_var "request_vid"]]) @@
    mk_empty nd_log_master.t

(**** protocol code ****)


(* for puts *)
let stmt_cnt_list =
  (* indexed by stmt id *)
  let e = ["count", t_map_id] in
  create_ds ~e "stmt_cnt_list" @@ wrap_tvector' @@ snd_many e

let send_put_ip_map_id = "send_put_ip_map"
let send_put_ip_map_e = ["stmt_bitmap", wrap_tvector t_bool; stmt_cnt_list.id, stmt_cnt_list.t]
let send_put_ip_map p =
  let init =
    mk_let ["stmt_ids"]
      (k3_container_of_list (wrap_tvector t_int) @@
       List.map mk_cint @@ create_range @@ 1 + (List.length @@ P.get_stmt_list p)) @@
    mk_map (mk_lambda' unknown_arg @@ mk_tuple
              [mk_map (mk_lambda' unknown_arg mk_cfalse) @@ mk_var "stmt_ids";
               mk_map (mk_lambda' unknown_arg @@ mk_cint 0) @@ mk_var "stmt_ids"]) @@
      mk_var D.my_peers.id in
  let e = send_put_ip_map_e in
  create_ds ~init ~e send_put_ip_map_id @@ wrap_tvector' @@ snd_many e

let send_put_bitmap =
  let init = mk_map (mk_lambda' unknown_arg mk_cfalse) @@ mk_var D.my_peers.id in
  create_ds ~init "send_put_bitmap" @@ wrap_tvector t_bool

(* for fetches *)
let send_fetch_ip_map =
  (* map per ip *)
  let e = [stmt_map_ids.id, stmt_map_ids.t] in
  let init =
    mk_map (mk_lambda' unknown_arg @@ mk_empty stmt_map_ids.t) @@ mk_var D.my_peers.id in
  create_ds ~e ~init "send_fetch_ip_map" @@ wrap_tvector' @@ snd_many e

let send_fetch_bitmap =
  let init = mk_map (mk_lambda' unknown_arg mk_cfalse) @@ mk_var D.my_peers.id in
  create_ds ~init "send_fetch_bitmap" @@ wrap_tvector t_bool

(* sending fetches is done from functions *)
(* each function takes a vid to plant in the arguments, which are in the trig buffers *)
let sw_send_fetch_fn c s_rhs_lhs s_rhs t =
  let send_completes_for_stmts_with_no_fetch =
    let s_no_rhs = P.stmts_without_rhs_maps_in_t c.p t in
    if null s_no_rhs then []
    else
      let s_ls =
        List.map
          (fun stmt_id ->
            stmt_id, P.lhs_map_of_stmt c.p stmt_id,
            do_complete_name_of_t t stmt_id^"_trig")
          s_no_rhs in
      let count = List.length s_ls in
      List.flatten @@ List.map
        (fun (stmt_id, lhs_map_id, do_complete_t) ->
          let key, pat_idx = P.key_pat_from_bound c.p c.route_indices stmt_id lhs_map_id in
          [
            R.route_lookup c lhs_map_id
              (mk_cint lhs_map_id::key)
              (mk_cint pat_idx) @@
              (* the first do_complete should ack the switch *)
              mk_ignore @@ mk_agg_bitmap'
                ["ack", t_bool]
                  (mk_block [
                    buffer_for_send do_complete_t "ip" @@
                      [mk_var D.me_int.id; mk_var "ack"] @ args_of_t_as_vars_with_v c t;
                    mk_cfalse
                  ])
                mk_ctrue
                K3Route.route_bitmap.id
          ])
        s_ls @
        (* stmts without puts need to reply *)
        [GC.sw_update_send ~n:(mk_cint count) ~vid_nm:"vid"]
  in
  let send_puts =
    if null s_rhs_lhs then [] else
    (* send puts
    * count is generated by counting the number of messages going to a
    * specific IP *)

      (* clean the put globals *)
      [mk_iter_bitmap'
        (mk_update_at_with
          send_put_ip_map_id
          (mk_var "ip") @@
          mk_lambda' send_put_ip_map_e @@
            mk_let ["agg"]
              (mk_agg_bitmap' ~idx:"stmt_ctr"
                ["acc", stmt_cnt_list.t]
                (mk_insert_at_block "acc" (mk_var "stmt_ctr") [mk_cint 0])
                (mk_var stmt_cnt_list.id)
                "stmt_bitmap") @@
            mk_block [
              mk_set_all "stmt_bitmap" [mk_cfalse];
              mk_tuple [mk_var "stmt_bitmap"; mk_var "agg"]
            ])
        send_put_bitmap.id;
      (* clean bitmap *)
      mk_set_all send_put_bitmap.id [mk_cfalse]
      ] @

      List.map
        (fun (stmt_id, (rhs_map_id, lhs_map_id)) ->
          (* shuffle allows us to recreate the path the data will take from rhs to lhs *)
          let shuffle_fn = K3Shuffle.find_shuffle_nm c stmt_id rhs_map_id lhs_map_id in
          let shuffle_key, shuffle_empty_pat =
            P.key_pat_from_bound c.p c.route_indices stmt_id lhs_map_id in
          let shuffle_pat = P.get_shuffle_pat_idx c.p c.route_indices stmt_id lhs_map_id rhs_map_id in
          (* route allows us to know how many nodes send data from rhs to lhs *)
          let route_key, route_pat_idx = P.key_pat_from_bound c.p c.route_indices stmt_id rhs_map_id in
          (* we need the types for creating empty rhs tuples *)
          let rhs_map_types = P.map_types_with_v_for c.p rhs_map_id in
          let tuple_types = wrap_t_calc' rhs_map_types in
          mk_let ["sender_count"]
            (* count up the number of IPs received from route *)
            (R.route_lookup c rhs_map_id
              (mk_cint rhs_map_id::route_key)
              (mk_cint route_pat_idx) @@
              mk_agg_bitmap'
                ["acc", t_int]
                (mk_add (mk_var "acc") @@ mk_cint 1)
                (mk_cint 0)
                R.route_bitmap.id) @@
          mk_block [
            (* fill in shuffle globals *)
            mk_apply' shuffle_fn @@
              shuffle_key @ [mk_cint shuffle_pat; mk_cint shuffle_empty_pat; mk_empty tuple_types];
            (* loop over the shuffle bitmap *)
            mk_iter_bitmap'
              (mk_block [
                (* mark the put bitmap *)
                mk_insert_at send_put_bitmap.id (mk_var "ip") [mk_ctrue];
                (* update the relevant stmt_cnt_list *)
                mk_update_at_with send_put_ip_map_id (mk_var "ip") @@
                  mk_lambda' send_put_ip_map_e @@
                    mk_block [
                      mk_insert_at "stmt_bitmap" (mk_cint stmt_id) [mk_ctrue];
                      (* increment count *)
                      mk_update_at_with stmt_cnt_list.id (mk_cint stmt_id) @@
                        mk_lambda'' stmt_cnt_list.e @@
                          mk_add (mk_var "count") @@ mk_var "sender_count";
                      (* recreate tuple *)
                      mk_tuple @@ ids_to_vars @@ fst_many @@ send_put_ip_map_e
                    ]
                ])
              K3S.shuffle_bitmap.id
          ])
          s_rhs_lhs @
      (* iterate over the result bitmap and buffer. Also count the number of sent msgs for this vid *)
      [
        let rcv_put_nm = rcv_put_name_of_t t in
        mk_let ["add_count"]
         (mk_agg_bitmap'
           ["count", t_int]
           (mk_block [
             mk_at_with' send_put_ip_map_id (mk_var "ip") @@
              mk_lambda' send_put_ip_map_e @@
                mk_block [
                  (* send rcv_put *)
                  buffer_for_send rcv_put_nm "ip" @@
                    [mk_var D.me_int.id] @ args_of_t_as_vars_with_v c t;
                  (* now send stmt_cnt_list *)
                  (mk_iter_bitmap' ~idx:D.stmt_ctr.id
                    (* wr_bitmap: no need to, since we did so above *)
                    (buffer_for_send ~wr_bitmap:false stmt_cnt_list_ship.id "ip"
                      [mk_var "stmt_ctr"; mk_at' stmt_cnt_list.id @@ mk_var "stmt_ctr"])
                    "stmt_bitmap")
                ];
                mk_add (mk_var "count") @@ mk_cint 1
            ])
         (mk_cint 0) @@
         send_put_bitmap.id) @@ mk_block @@
       GC.sw_update_send ~vid_nm:"vid" ~n:(mk_var "add_count")]
  in
  (* to do this in batched form, we iterate over the send_fetch bitmap and immediately buffer
     the results *)
  let send_fetches_of_rhs_maps  =
    let args = args_of_t_as_vars_with_v c t in
    let target_t = rcv_fetch_name_of_t t in
    if null s_rhs then [] else
     (* clean send_fetch bitmap *)
      [mk_set_all send_fetch_bitmap.id [mk_cfalse]] @
     (* fill in global data structure from route *)
      (List.map
        (fun (stmt_id, rhs_map_id) ->
          let key, pat_idx = P.key_pat_from_bound c.p c.route_indices stmt_id rhs_map_id in
          R.route_lookup c rhs_map_id
            (mk_cint rhs_map_id::key)
            (mk_cint pat_idx) @@
          mk_iter_bitmap'
            (mk_block [
              (* if we haven't sent to this ip yet, add a rcv_fetch header *)
                mk_if (mk_not @@ mk_at' send_fetch_bitmap.id @@ mk_var "ip")
                  (mk_block [
                    (* mark the send_fetch_bitmap *)
                    mk_insert_at send_fetch_bitmap.id (mk_var "ip") [mk_ctrue];
                    (* buffer the trig args *)
                    buffer_for_send target_t "ip" args
                  ])
                  mk_cunit;
              (* buffer the stmt-map *)
                buffer_for_send ~wr_bitmap:false stmt_map_ids.id "ip"
                  [mk_cint stmt_id; mk_cint rhs_map_id]
            ])
            R.route_bitmap.id)
        s_rhs)
  in
  (* Actual SendFetch function *)
  (* We use functions rather than triggers to have better control over
  * latency *)
  mk_global_fn
    (send_fetch_name_of_t t) (args_of_t_with_v c t) [] @@
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
  let fn_name = rcv_fetch_name_of_t trig in
  mk_global_fn fn_name
    (poly_args @ D.nd_rcv_fetch_args c trig) (* stmt_map_ids are an inner ds *)
    [t_int; t_int] @@
    (* skip over the function tag *)
    mk_poly_skip_block fn_name [
      (* save the bound variables for this trigger so they're available later *)
      mk_apply
        (mk_var @@ nd_log_write_for c trig) @@
        args_of_t_as_vars_with_v c trig
      ;
      (* we *must* have a data structure here *)
      mk_check_tag' (ios_tag c stmt_map_ids.id) @@
      (* iterate over the buffered stmt_map data *)
      mk_poly_iter_tag' stmt_map_ids.id @@
        (mk_lambda3' p_idx p_off stmt_map_ids.e @@
          mk_if
            (mk_or
              (* check if we're in corrective mode *)
              (mk_var D.corrective_mode.id) @@
              (* or if the minimum entry in the stmt_cntrs has a >= vid
               * (we can read at the same vid since we read an earlier value *)
              mk_leq (mk_var "vid") @@
                mk_case_ns
                  (mk_peek @@ mk_slice' D.nd_stmt_cntrs_per_map.id
                    [mk_var "map_id"; mk_cunknown]) "x"
                  (mk_var D.g_max_vid.id) @@
                  mk_min_with (mk_snd @@ mk_var "x")
                  (* if empty, return max *)
                    (mk_lambda'' unit_arg @@ mk_var g_max_vid.id) @@
                    mk_lambda' nd_stmt_cntrs_per_map_inner.e @@ mk_var "vid")
            (* then send the push right now *)
            (List.fold_right
              (fun stmt acc_code -> mk_if
                (mk_eq (mk_var "stmt_id") @@ mk_cint stmt)
                (List.fold_right
                  (fun map_id acc_code2 -> mk_if
                    (mk_eq (mk_var "map_id") @@ mk_cint map_id)
                    (mk_apply' (send_push_name_of_t c trig stmt map_id) @@
                      args_of_t_as_vars_with_v c trig)
                    acc_code2)
                  (P.rhs_maps_of_stmt c.p stmt) @@
                  mk_error "nd_rcv_fetch: invalid map id")
                acc_code)
              (P.stmts_with_rhs_maps_in_t c.p trig) @@
              mk_error "nd_rcv_fetch: invalid stmt id") @@
            (* else, buffer the push *)
            mk_upsert_with nd_rcv_fetch_buffer.id [mk_var "map_id"; mk_cunknown]
              (mk_lambda' unit_arg @@ mk_tuple
                [mk_var "map_id"; mk_singleton D.nd_rcv_fetch_buffer_inner.t
                  [mk_var "vid";
                    mk_singleton D.nd_rcv_fetch_buffer_inner2.t [mk_var "stmt_id"]]]) @@
              mk_lambda' ["fetch_buf", t_of_e nd_rcv_fetch_buffer.e] @@
                mk_upsert_with_block "fetch_buf" ~path:[2] [mk_var "vid"; mk_cunknown]
                  (mk_lambda'' unit_arg @@ mk_tuple
                    [mk_var "vid"; mk_singleton D.nd_rcv_fetch_buffer_inner2.t
                      [mk_var "stmt_id"]]) @@
                    mk_lambda' ["inner", t_of_e nd_rcv_fetch_buffer_inner.e] @@
                      mk_insert_block "inner" ~path:[2] [mk_var "stmt_id"])
      ;
      (* skip the tags we iterated over *)
      mk_poly_skip_all' stmt_map_ids.id
    ]

(* Receive Put trigger
 * --------------------------------------- *
 * Update the statement counters with the received values
 * If the counter is 0, we need to call do_complete
 * - One of the triggers fed from the poly queue
 *)
let nd_rcv_put_trig c t =
  let fn_name = rcv_put_name_of_t t in
  mk_global_fn fn_name
    (poly_args @ D.nd_rcv_put_args c t) (* also pull inner ds *)
    [t_int; t_int] @@
    (* skip over the calling function slot *)
    mk_poly_skip_block fn_name
    [
      (* we *must* have a data structure here *)
      mk_check_tag' (ios_tag c stmt_cnt_list_ship.id) @@
      mk_poly_iter_tag' stmt_cnt_list_ship.id
        (mk_lambda3' p_idx p_off stmt_cnt_list_ship.e @@
          mk_if
            (mk_apply' nd_check_stmt_cntr_index_nm @@
              (* false: no data is being sent *)
              [mk_var "vid"; mk_var "stmt_id"; mk_var "count"; mk_cfalse])
            (* we need to create a possible send for each statement in the trigger *)
            (List.fold_left (fun acc_code stmt_id ->
              mk_if
                (mk_eq (mk_var "stmt_id") @@ mk_cint stmt_id)
                (mk_apply'
                  (do_complete_name_of_t t stmt_id) @@
                  args_of_t_as_vars_with_v c t) @@
                acc_code)
              mk_cunit @@
              P.stmts_of_t c.p t) @@
          mk_cunit);
      GC.nd_ack_send_code ~addr_nm:"sender_ip" ~vid_nm:"vid";
      mk_poly_skip_all' stmt_cnt_list_ship.id
    ]

(* Trigger_send_push_stmt_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 * Produces a list of triggers.
 * We're parametrized by stmt and map_id because we save repeating the
 * processing on the switch side to figure out which maps (in which stmts) we
 * have locally
 *)
let nd_send_push_stmt_map_trig c s_rhs_lhs t =
  List.map
    (fun (s, (rmap, lmap)) ->
      let rmap_nm = P.map_name_of c.p rmap in
      let rmap_deref = "rmap_d" in
      let shuffle_fn = K3S.find_shuffle_nm c s rmap lmap in
      let shuffle_key, empty_pat_idx = P.key_pat_from_bound c.p c.route_indices s lmap in
      let shuffle_pat_idx = P.get_shuffle_pat_idx c.p c.route_indices s lmap rmap in
      let slice_key = P.slice_key_from_bound c.p s rmap in
      let map_delta = D.map_ds_of_id c rmap ~global:false in
      let map_real  = D.map_ds_of_id c rmap ~global:true in
      let map_pat = D.pat_of_ds map_real ~flatten:true ~expr:(mk_var "tuple") in
      let rcv_trig = rcv_push_name_of_t c t s rmap in
      (* a pattern mapping real map with delta vars *)
      mk_global_fn
        (send_push_name_of_t c t s rmap)
        (args_of_t_with_v c t) [] @@
        (* don't convert this to fold b/c we only read *)
        mk_bind (mk_var rmap_nm) rmap_deref @@
        mk_let ["tuples"]
            (* check if we can use a lookup *)
            (if D.is_lookup_pat (mk_tuple slice_key) then
              let slice_key =
                mk_var "vid"::[mk_tuple @@ list_drop_end 1 slice_key]@[mk_cunknown] in
              mk_peek_with_vid
                (mk_slice_lt (mk_var rmap_deref) slice_key)
                (mk_lambda'' unit_arg @@ mk_empty map_delta.t) @@
                mk_lambda'' ["vid", t_vid; "tuple", snd @@ unwrap_tcol @@ map_real.t] @@
                  mk_singleton map_delta.t @@ fst_many map_pat
              else
              (* we need the latest vid data that's less than the current vid *)
              D.map_latest_vid_vals c (mk_var rmap_deref)
                (some slice_key) rmap ~keep_vid:true) @@
        mk_block [
          (* save this particular statement execution in the master log
           * Note that we need to do it here to make sure nothing
           * else can stop us before we send the push *)
          mk_apply'
            nd_log_master_write_nm @@ [mk_cint s; mk_var "vid"];
          (* fill in the global shuffle data structures *)
          mk_apply' shuffle_fn @@
            shuffle_key@[mk_cint shuffle_pat_idx; mk_cint empty_pat_idx; mk_var "tuples"];
          (* iterate and buffer *)
          mk_iter_bitmap'
            (mk_at_with' K3S.shuffle_results.id (mk_var "ip") @@
              mk_lambda' K3S.shuffle_results.e @@
                mk_block [
                  (* lookup and reconstruct tuples from shuffle results *)
                  (* send the 'header' *)
                  buffer_for_send rcv_trig "ip" @@
                    mk_var "has_data"::args_of_t_as_vars_with_v c t;
                  (* buffer the map data according to the indices *)
                  buffer_tuples_from_idxs "tuples" map_delta.t (rmap_nm^"_v") @@ mk_var "indices"
               ])
            K3S.shuffle_bitmap.id
        ]) (* trigger *)
    s_rhs_lhs


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

let nd_rcv_push_trig c s_rhs t =
List.map
  (fun (s, m) ->
    let rbuf_name = P.buf_of_stmt_map_id c.p s m in
    let rbuf_deref = "buf_d" in
    let map_ds = map_ds_of_id ~global:true c m in
    let tup_ds = map_ds_of_id ~global:false ~vid:true c m in
    let tup_pat = pat_of_ds tup_ds in
    let map_pat = pat_of_flat_e ~add_vid:true ~has_vid:true map_ds @@ fst_many tup_pat in
    let fn_name = rcv_push_name_of_t c t s m in
    mk_global_fn fn_name
      (* we also need to get (tuples, tup_ds.t) from the global polyqueue *)
      (poly_args @ D.nd_rcv_push_args c t)
      (* return idx, offset *)
      [t_int; t_int] @@
      (* skip over the function variant *)
      mk_poly_skip_block fn_name [
        (* save the bound variables for this vid. This is necessary for both the
         * sending and receiving nodes, which is why we're also doing it here *)
        mk_apply' (nd_log_write_for c t) @@ args_of_t_as_vars_with_v c t;

        (* check if we have an empty map *)
        mk_if_poly_end_ny
          (mk_if_tag' (ios_tag c @@ tup_ds.id^"_v")
            (mk_bind (mk_var rbuf_name) rbuf_deref @@
              (* assign a fold result back to the buffer *)
              mk_assign rbuf_deref @@ U.add_property "Move" @@
                mk_poly_fold_tag' (tup_ds.id^"_v")
                  (mk_lambda'' (["acc", map_ds.t] @ poly_args_partial @
                                ["x", wrap_ttuple @@ snd_many @@ ds_e tup_ds]) @@
                    mk_let (fst_many @@ ds_e tup_ds) (mk_var "x") @@
                    mk_insert_block "acc" map_pat) @@
                  mk_var rbuf_deref)
            mk_cunit)
          mk_cunit;

        (* update and check statment counters to see if we should send a do_complete *)
        mk_if
          (mk_apply' nd_check_stmt_cntr_index_nm @@
            [mk_var "vid"; mk_cint s; mk_cint @@ -1; mk_var "has_data"])
          (* apply local do_complete *)
          (mk_apply' (do_complete_name_of_t t s) @@
            args_of_t_as_vars_with_v c t)
          mk_cunit;

        (* skip over the data type *)
        mk_poly_skip_all' @@ tup_ds.id^"_v";
      ])
  s_rhs

(* list of potential corrective maps.
 * the potential corrective maps and the potential read/write conflicts.
 * Inserts and Deletes really have 2 different graphs, so we return both *)
let maps_potential_corrective p =
  let ts = P.get_trig_list ~sys_init:false ~delete:true ~corrective:true p in
  let get_maps f = ListAsSet.uniq |- List.flatten |- List.map f |- List.flatten
    |- List.map (P.stmts_of_t p) in
  let lhs_maps = get_maps (singleton |- P.lhs_map_of_stmt p) in
  let rhs_maps = get_maps (P.rhs_maps_of_stmt p) in
  let intersect_maps ts = ListAsSet.inter (lhs_maps ts) (rhs_maps ts) in
  intersect_maps ts

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
  let send_correctives m =
    (* list of (trig,stmt) that have this m on the rhs *)
    let trigs_stmts_with_matching_rhs_map =
        List.filter
          (fun (trig, stmt_id) -> P.stmt_has_rhs_map c.p stmt_id m) @@
          List.flatten @@
            P.for_all_trigs ~delete:c.gen_deletes c.p
              (fun trig ->
                List.map (fun stmt -> trig, stmt) @@ P.stmts_of_t c.p trig) in
    (* turn the ocaml list into a literal k3 list *)
    let trig_stmt_k3_list_nm = "nd_corr_"^P.map_name_of c.p m^"_list" in
    let trig_stmt_k3_list =
      let t = wrap_tbag' [t_trig_id; t_stmt_id] in
      mk_global_val_init trig_stmt_k3_list_nm t @@
        k3_container_of_list t @@ List.map
          (fun (trig, stmt_id) ->
            mk_tuple [mk_cint (P.trigger_id_for_name c.p trig); mk_cint stmt_id])
          trigs_stmts_with_matching_rhs_map
    in
    match trigs_stmts_with_matching_rhs_map with [] -> [] | _ ->
    let map_ds = D.map_ds_of_id ~global:false c m ~vid:false in
    let delta_tuples2 =
      D.map_ds_of_id ~global:false ~vid:true c m ~name:"delta_tuples2" in
    (* rename vid to corrective_vid for differentiation *)
    let args' = (list_drop_end 1 D.nd_rcv_corr_args)@["corrective_vid", t_vid] in
    let args = args' @ ["delta_tuples", map_ds.t] in
    let sub_args = args' @ ["delta_tuples2", delta_tuples2.t; "vid_list", t_vid_list] in
    let fn_nm = send_corrective_name_of_t c m in
    let sub_fn_nm stmt = fn_nm^"_"^soi stmt in
    (* collection by ip *)
    let result_ds =
      let e = ["vids", t_vid_list; "t_indices", wrap_tset t_int] in
      let t = wrap_tvector' @@ snd_many e in
      create_ds "result_col" ~e t
    in
    let t_bitmap = wrap_tvector t_bool in
    let acc_col_t = wrap_ttuple [t_bitmap; result_ds.t] in

    (* create sub functions for each stmt_id possible, to aid compilation *)
    let sub_fns =
      List.map (fun (t, s) ->
         (* we already have the map vars for the rhs map in the
          * tuples. Now we try to get some more for the lhs map *)
        let target_map = P.lhs_map_of_stmt c.p s in
        let key, _ = P.key_pat_from_bound c.p c.route_indices s target_map in
        let pat_idx = P.get_shuffle_pat_idx c.p c.route_indices s target_map m in
        let shuffle_fn = K3S.find_shuffle_nm c s m target_map in

        mk_global_fn (sub_fn_nm s) sub_args
        [t_int] @@ (* return num of sends *)
          (* TODO: if lvars have no impact, only shuffle once *)
          mk_let ["ips_vids"]
            (mk_agg
              (mk_lambda2' ["acc_col", acc_col_t] ["vid", t_vid] @@
                (* get bound vars from log so we can calculate shuffle *)
                let args = D.args_of_t c t in
                (if args <> [] then
                  mk_let
                    (fst_many @@ D.args_of_t c t)
                    (mk_apply'
                      (nd_log_get_bound_for t) [mk_var "vid"])
                else id_fn) @@
                mk_block [
                  mk_apply' shuffle_fn @@
                    key @ [mk_cint pat_idx; mk_cint (-1); mk_var "delta_tuples2"];
                  mk_agg_bitmap'
                    ["acc", acc_col_t]
                    (mk_block [
                      mk_insert_at ~path:[1] "acc" (mk_var "ip") [mk_ctrue];
                      (* lookup indices *)
                      mk_at_with' K3S.shuffle_results.id (mk_var "ip") @@
                        mk_lambda' K3S.shuffle_results.e @@
                          mk_update_at_with_block ~path:[2] "acc" (mk_var "ip") @@
                            mk_lambda' result_ds.e @@
                              mk_block [
                                mk_insert "vids" [mk_var "vid"];
                                (* insert indices into index set *)
                                mk_let ["t_indices"]
                                  (mk_agg
                                    (mk_lambda2' ["acc", wrap_tset t_int] ["i", t_int] @@
                                      mk_insert_block "acc" [mk_var "i"])
                                    (mk_var "t_indices") @@
                                    mk_var "indices") @@
                                mk_tuple [mk_var "vids"; mk_var "t_indices"]
                              ]
                    ])
                    (mk_var "acc_col") @@
                    K3S.shuffle_bitmap.id
                ])
              (mk_tuple [mk_empty t_bitmap; mk_empty result_ds.t]) @@
              mk_var "vid_list") @@
            (* send to each ip, and count up the msgs *)
            let rcv_trig = rcv_corrective_name_of_t c t s m in
            mk_agg_bitmap
              ["count", t_int]
              (* lookup ip in vector *)
              (mk_at_with (mk_snd @@ mk_var "ips_vids") (mk_var "ip") @@
                mk_lambda' result_ds.e @@
                  mk_block [
                    (* buffer header *)
                    buffer_for_send rcv_trig "ip" @@
                      (ids_to_vars @@ fst_many orig_vals) @ [mk_var "corrective_vid"];
                    (* buffer tuples from indices *)
                    buffer_tuples_from_idxs ~drop_vid:true
                      delta_tuples2.id delta_tuples2.t map_ds.id (mk_var "t_indices");
                    (* buffer vids *)
                    mk_iter (mk_lambda'' ["vid", t_vid] @@
                      buffer_for_send ~wr_bitmap:false "vids" "ip" [mk_var "vid"]) @@ mk_var "vids";
                    mk_add (mk_var "count") @@ mk_cint 1
                  ])
              (mk_cint 0) @@
          mk_fst @@ mk_var "ips_vids")
        trigs_stmts_with_matching_rhs_map
    in
    let fn = mk_global_fn fn_nm args
    (* return the number of new correctives generated *)
    [t_int] @@
    (* the corrective list tells us which statements were fetched
     * from us and when *)
    mk_let ["corrective_list"] (* (stmt_id * vid sortedset) map *)
      (mk_apply'
        nd_filter_corrective_list_nm @@
        (* feed in list of possible stmts *)
          [mk_var "corrective_vid"; mk_var trig_stmt_k3_list_nm]) @@
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
        (mk_lambda2'
          ["acc_count", t_int] ["stmt_id", t_stmt_id; "vid_set", t_vid_sortedset] @@
          mk_let ["vid_list"]
            (mk_agg
              (mk_lambda2' ["vid_acc", t_vid_list] ["v", t_vid] @@
                mk_insert_block "vid_acc" [mk_var "v"])
              (mk_empty t_vid_list) @@
              mk_var "vid_set")
          (List.fold_left
            (* loop over all possible read map matches *)
            (fun acc_code (_, s) ->
              mk_if (* if match, send data *)
                (mk_eq (mk_var "stmt_id") @@ mk_cint s)
                (* call the specific function for this statement *)
                (mk_add (mk_var "acc_count") @@
                  mk_apply' (sub_fn_nm s) @@ ids_to_vars' sub_args)
                acc_code)
            (mk_var "acc_count") (* base case *)
            trigs_stmts_with_matching_rhs_map))
            (* base number of msgs *)
          (mk_cint 0) @@
          mk_var "corrective_list")
    in
    trig_stmt_k3_list :: sub_fns @ [fn]
  in
  List.flatten @@ List.map send_correctives c.corr_maps

(* function to update the stmt_counters for correctives *)
(* current hop = hop - 1. next hop = hop *)
let nd_update_stmt_cntr_corr_map_nm = "nd_update_stmt_cntr_corr_map"
let nd_update_stmt_cntr_corr_map =
  let lookup_pat = [mk_var "vid"; mk_var "stmt_id"] in
  mk_global_fn nd_update_stmt_cntr_corr_map_nm
  ["vid", t_vid; "stmt_id", t_stmt_id; "hop", t_int; "count", t_int; "root", t_bool; "create", t_bool] [] @@
    mk_block [
      (* if requested, we first create an empty entry *)
      (* this is for stmts w/o rhs maps that still need to keep track of corrective chains *)
      mk_if (mk_var "create")
        (mk_insert nd_stmt_cntrs.id
           [mk_tuple lookup_pat; mk_tuple [mk_cint 0; mk_cfalse; mk_empty nd_stmt_cntrs_corr_map.t]])
        mk_cunit;
      (* we need to decrement the previous hop's value by 1, and increment the hop's values by the count (if it's not 0) *)
      mk_upsert_with_sim nd_stmt_cntrs "lkup" ~k:lookup_pat
        ~default:(mk_error @@ sp "%s: missing stmt_cntrs value" nd_update_stmt_cntr_corr_map_nm)
        ~v:(mk_let [nd_stmt_cntrs_corr_map.id] (mk_thd @@ mk_snd @@ mk_var "lkup") @@
              mk_block [
                (* only do this part if count isn't 0 *)
                mk_if (mk_eq (mk_var "count") @@ mk_cint 0)
                  mk_cunit @@
                  (* increment the next hop by count *)
                  mk_let ["delete_entry"; "new_count"]
                    (mk_case_ns
                      (mk_peek @@ mk_slice'
                          nd_stmt_cntrs_corr_map.id [mk_var "hop"; mk_cunknown]) "lkup2"
                      (* if no entry, return count *)
                      (mk_tuple [mk_cfalse; mk_var "count"])
                      (* else, calculate new corr count *)
                      (mk_let ["new_corr_cnt"]
                        (mk_add (mk_snd @@ mk_var "lkup2") @@ mk_var "count") @@
                        (* if we've hit 0 *)
                        mk_if (mk_eq (mk_var "new_corr_cnt") @@ mk_cint 0)
                          (* delete the entry *)
                          (mk_tuple [mk_ctrue; mk_cint 0])
                          (* else, return the incremented entry *)
                          (mk_tuple [mk_cfalse; mk_var "new_corr_cnt"])))
                    (mk_if (mk_var "delete_entry")
                      (mk_delete nd_stmt_cntrs_corr_map.id [mk_var "hop"; mk_cint 0])
                      (mk_insert nd_stmt_cntrs_corr_map.id [mk_var "hop"; mk_var "new_count"]));

                (* check if this is from the root of the corrective tree *)
                mk_if (mk_var "root")
                  (* return as is *)
                  mk_cunit @@
                  mk_let ["hop2"] (mk_sub (mk_var "hop") @@ mk_cint 1) @@
                  (* else update the current hop *)
                  mk_let ["delete_entry"; "new_count"]
                    (mk_case_ns
                      (mk_peek @@ mk_slice' nd_stmt_cntrs_corr_map.id
                                    [mk_var "hop2"; mk_cunknown]) "lkup2"
                      (* if no entry, return -1 *)
                      (mk_tuple [mk_cfalse; mk_cint @@ -1])
                      (* else, calculate new corr count *)
                      (mk_let ["new_corr_cnt"]
                        (mk_sub (mk_snd @@ mk_var "lkup2") @@ mk_cint 1) @@
                        (* if we've hit 0 *)
                        mk_if (mk_eq (mk_var "new_corr_cnt") @@ mk_cint 0)
                          (* delete the entry *)
                          (mk_tuple [mk_ctrue; mk_cint 0])
                          (* else, return the incremented entry *)
                          (mk_tuple [mk_cfalse; mk_var "new_corr_cnt"])))
                    (mk_if (mk_var "delete_entry")
                      (mk_delete nd_stmt_cntrs_corr_map.id [mk_var "hop2"; mk_cint 0])
                      (mk_insert nd_stmt_cntrs_corr_map.id [mk_var "hop2"; mk_var "new_count"]));

                (* set the new tuple *)
                mk_tuple [mk_fst @@ mk_snd @@ mk_var "lkup";
                          mk_snd @@ mk_snd @@ mk_var "lkup";
                          mk_var nd_stmt_cntrs_corr_map.id]
              ])
    ]

(* for no-corrective mode: execute buffered fetches *)
(* we have a balance between reads and writes. Reads are buffered in the
 * buffered_fetches, and writes are tracked in the stmt_cntrs. We split both
 * per-map to give us better granularity barriers, though even better ones could
 * be made if we incorporate values within the maps somehow (we have the same issue
 * for correctives). In no-corrective mode, the only state that is legal is to read
 * before or at the same time as a write to the same map (simultaneous reads read earlier
 * values, which is ok). We ensure this by checking the min_vid of the writes
 * and filtering all reads <= this min_vid. *)
let nd_exec_buffered_fetches_nm = "nd_exec_buffered_fetches"
let nd_exec_buffered_fetches c =
  let t_info = P.for_all_trigs c.p
    (fun t -> t, D.args_of_t c t, P.stmts_with_rhs_maps_in_t c.p t) in
  let t_info = List.filter (fun (_,_,x) -> x <> []) t_info in
  if t_info = [] then [] else singleton @@
  mk_global_fn nd_exec_buffered_fetches_nm ["vid", t_vid; "stmt_id", t_vid] [] @@
  (* get lmap id *)
  mk_let ["map_id"]
    (mk_snd @@ mk_peek_or_error "missing stmt_id" @@
      mk_slice' (D.nd_lmap_of_stmt_id c).id [mk_var "stmt_id"; mk_cunknown]) @@
  mk_block [
    (* delete the entry from the stmt_cntrs_per_map. this has to be done before we
     * get the min_vid *)
    mk_upsert_with D.nd_stmt_cntrs_per_map.id [mk_var "map_id"; mk_cunknown]
      (* default can create the entry -- it's ok to keep it around *)
      (mk_lambda'' unit_arg @@ mk_tuple [mk_var "map_id"; mk_empty D.nd_stmt_cntrs_per_map_inner.t]) @@
      mk_lambda' D.nd_stmt_cntrs_per_map.e @@
        mk_block [
          mk_delete "vid_stmt" [mk_var "vid"; mk_cunknown];
          mk_tuple @@ ids_to_vars @@ fst_many @@ D.nd_stmt_cntrs_per_map.e
        ]
    ;
    mk_let ["min_vid"]
      (mk_case_ns
        (mk_peek @@ mk_slice' D.nd_stmt_cntrs_per_map.id [mk_var "map_id"; mk_cunknown])
        "x"
        (mk_var D.g_max_vid.id) @@
        mk_min_with (mk_snd @@ mk_var "x")
          (mk_lambda'' unit_arg @@ mk_var D.g_max_vid.id) @@
          mk_lambda' D.nd_stmt_cntrs_per_map_inner.e @@ mk_var "vid") @@
    (* check if there are any pushes to send *)
    mk_let ["pending_fetches"]
      (mk_case_ns
        (mk_peek @@ mk_slice' D.nd_rcv_fetch_buffer.id [mk_var "map_id"; mk_cunknown]) "x"
        mk_cfalse @@
        mk_case_ns
          (mk_peek @@ mk_slice_leq (mk_snd @@ mk_var "x") [mk_var "min_vid"; mk_cunknown])
          "_u" mk_cfalse mk_ctrue) @@
      (* check if this is the min vid for the map in stmt_cntrs_per_map. if not, do nothing,
      * since we're not changing anything *)
      mk_if (mk_var "pending_fetches")
        (mk_block [
          (* execute any fetches that precede pending writes for a map *)
            mk_iter (mk_lambda' nd_rcv_fetch_buffer_inner.e @@
              mk_iter (mk_lambda'' ["stmt_id", t_stmt_id] @@
                (* check for all triggers *)
                List.fold_left (fun acc (t, args, stmts) ->
                  let mk_check_s s = mk_eq (mk_var "stmt_id") @@ mk_cint s in
                  (* check if the stmts are in this trigger *)
                  mk_if
                    (list_fold_to_last (fun acc s -> mk_or (mk_check_s s) acc) mk_check_s stmts)
                    (* pull arguments out of log (if needed) *)
                    ((if args = [] then id_fn else
                      mk_let (fst_many args) (mk_apply'
                        (nd_log_get_bound_for t) [mk_var "vid"])) @@
                    List.fold_left (fun acc s ->
                      mk_if (mk_eq (mk_var "stmt_id") @@ mk_cint s)
                        (let r_maps = P.rhs_maps_of_stmt c.p s in
                        List.fold_left (fun acc m ->
                          mk_if (mk_eq (mk_var "map_id") @@ mk_cint m)
                            (mk_apply' (send_push_name_of_t c t s m) @@
                              args_of_t_as_vars_with_v c t)
                            acc)
                          mk_cunit
                          r_maps)
                        acc)
                      mk_cunit
                      stmts)
                    acc)
                  mk_cunit
                t_info) @@
                mk_var "stmt_ids") @@
          (* filter only those entries we can run *)
          mk_case_ns (mk_peek @@ mk_slice' D.nd_rcv_fetch_buffer.id
            [mk_var "map_id"; mk_cunknown]) "x"
            (mk_error "empty fetch buffer!") @@
            mk_filter_leq (mk_snd @@ mk_var "x") [mk_var "min_vid"; mk_cunknown];
          (* delete these entries from the fetch buffer *)
          mk_upsert_with D.nd_rcv_fetch_buffer.id [mk_var "map_id"; mk_cunknown]
            (mk_lambda'' unit_arg @@ mk_error "whoops4") @@
            mk_lambda' D.nd_rcv_fetch_buffer.e @@
              mk_tuple [mk_var "map_id";
                mk_filter_gt (mk_var "vid_stmt") [mk_var "min_vid"; mk_cunknown]]
        ])
        (* else do nothing *)
        mk_cunit
    ]

(* call from do_complete when done to check if fully done *)
let nd_complete_stmt_cntr_check c =
  mk_global_fn nd_complete_stmt_cntr_check_nm ["vid", t_vid; "stmt_id", t_stmt_id] [] @@
  (* if we have nothing to send, we can delete our stmt_cntr entry right away *)
  mk_block @@
    [mk_delete nd_stmt_cntrs.id
      [mk_tuple [mk_var "vid"; mk_var "stmt_id"]; mk_cunknown]] @
    (* if we're in no-corrective mode, we need to execute batched fetches *)
    (if c.corr_maps = [] then [] else singleton @@
      mk_if (mk_var D.corrective_mode.id)
        mk_cunit @@
        mk_apply' nd_exec_buffered_fetches_nm [mk_var "vid"; mk_var "stmt_id"]) @
    (* check if we're done *)
    [Proto.nd_post_delete_stmt_cntr c]

(*
 * shared code btw do_complete and do_corrective
 * we add the delta to all following vids
 *)
let do_add_delta c e lmap ~corrective =
  mk_apply' (D.nd_add_delta_to_buf_nm c lmap) @@
    [mk_var @@ P.map_name_of c.p lmap;
      if corrective then mk_ctrue else mk_cfalse; mk_var "vid"; e]

(* trigger versions of do_complete: only for stmts with no rhs maps *)
let nd_do_complete_trigs c t =
  let do_complete_trig stmt_id =
    let comp_nm = do_complete_name_of_t t stmt_id in
    let args = nd_do_complete_trig_args c t in
    let args' = tl @@ tl @@ args in (* drop sender_ip and args *)
    mk_global_fn (comp_nm^"_trig") args [] @@
      mk_block [
        mk_apply' comp_nm @@ ids_to_vars @@ fst_many @@ args';
        (* if we're asked to acknowledge *)
        mk_if (mk_var "ack")
          (GC.nd_ack_send_code ~addr_nm:"sender_ip" ~vid_nm:"vid")
          mk_cunit
      ]
in
List.map do_complete_trig @@ P.stmts_without_rhs_maps_in_t c.p t

(* check for complicated double loop vars which necessitate lmap filtering *)
(* @alt: alternative return expression *)
let let_lmap_filtering c delta stmt_id lmap let_bind body ~alt =
    let lmap_i_ts = P.map_ids_types_for c.p lmap in
    let lmap_ts = snd_many lmap_i_ts in
    let _, pat_idx = P.key_pat_from_bound c.p c.route_indices stmt_id lmap in
    let do_action = "do_action" in
    if P.stmt_many_loop_vars c.p stmt_id <> None then
      mk_let [do_action; delta]
        (mk_agg
           (mk_lambda2' [do_action, t_bool; "acc", wrap_t_calc' lmap_ts]
                        [delta, wrap_ttuple lmap_ts] @@
             mk_let (fst_many lmap_i_ts) (mk_var delta) @@
             (* we make sure that a precise route leads to us *)
             (* if no match is found, we abort the action *)
             R.route_lookup c lmap
               (mk_cint lmap ::
                 (List.map (fun x -> mk_tuple [mk_ctrue; x]) @@
                 ids_to_vars @@ fst_many @@ list_drop_end 1 lmap_i_ts))
               (mk_cint pat_idx) @@
             mk_if (mk_at' R.route_bitmap.id @@ mk_var D.me_int.id)
               (mk_tuple [mk_ctrue; mk_insert_block "acc" [mk_var delta]]) @@
               mk_tuple [mk_var do_action; mk_var "acc"])
           (mk_tuple [mk_cfalse; mk_empty @@ wrap_t_calc' lmap_ts])
          let_bind) @@
        (* if we have no real value, do nothing *)
        mk_if (mk_var do_action) body alt
    else
      (* normal pathway - no complex loop vars *)
      mk_let [delta] let_bind body

(* function versions of do_complete *)
let nd_do_complete_fns c ast trig_name corr_maps =
  (* @has_rhs: whether this statement has rhs maps *)
  let do_complete_fn has_rhs stmt_id =
    mk_global_fn (do_complete_name_of_t trig_name stmt_id)
      (args_of_t_with_v c trig_name) [] @@
    let lmap = P.lhs_map_of_stmt c.p stmt_id in
    let fst_hop = mk_cint 1 in
    let snd_hop = mk_cint 2 in
    let delta = "delta_vals" in
    let is_col, ast = M.modify_ast c ast stmt_id trig_name in

    (* if we have no rhs maps, do nothing *)
    let no_corr_actions =
      if not has_rhs then mk_cunit
      else mk_apply' nd_complete_stmt_cntr_check_nm [mk_var "vid"; mk_cint stmt_id]
    in

    (* check for complicated double loop vars which necessitate lmap filtering
     * if we have loop vars, we need to filter the lmap values here
     *)
    let_lmap_filtering c delta stmt_id lmap
      ast
      (mk_block [
        (* add delta *)
        do_add_delta c (mk_var delta) lmap ~corrective:false;
        if c.gen_correctives && List.exists ((=) lmap) corr_maps
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
                (* else, update and create a stmt counter, since we need to keep track of any
                 *  correctives we sent on, but we have no rhs maps *)
                update_corr_code true
        (* no correctives are possible *)
        else no_corr_actions
      ])
      (* no valid data: do nothing *)
      ~alt:no_corr_actions
in
(List.map (do_complete_fn true)  @@ P.stmts_with_rhs_maps_in_t c.p trig_name) @
(List.map (do_complete_fn false) @@ P.stmts_without_rhs_maps_in_t c.p trig_name)

(* rcv notification of a corrective finish from other nodes *)
let nd_rcv_corr_done c =
  let lookup_pat = [mk_tuple [mk_var "vid"; mk_var "stmt_id"]; mk_cunknown] in
  let args = nd_rcv_corr_done_args in
  mk_global_fn nd_rcv_corr_done_nm args [] @@
    mk_block [
      (* update the corrective map. false: not a root of the corrective tree *)
      mk_apply' nd_update_stmt_cntr_corr_map_nm @@
        (ids_to_vars @@ fst_many args) @ [mk_cfalse; mk_cfalse];
      (* check if the corr_cnt structure is empty. If so, we can delete the whole entry *)
      mk_case_ns (mk_lookup' nd_stmt_cntrs.id lookup_pat) "lkup"
        (mk_error @@ nd_rcv_corr_done_nm^": expected stmt_cntr value") @@
        (* if the corr_cnt map is empty *)
        mk_is_empty (mk_thd @@ mk_snd @@ mk_var "lkup")
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
let nd_rcv_correctives_trig c s_rhs t = List.map
  (fun (s, m) ->
    let buf_map_nm = P.buf_of_stmt_map_id c.p s m in
    let map_delta = D.map_ds_of_id c m ~global:false ~vid:false in
    let fn_nm = rcv_corrective_name_of_t c t s m in
    let ds_tag = "vids" in
    mk_global_fn fn_nm
      (* we always send back acks to the original address, s, vid tuple *)
      (* we also have delta_tuples, as well as corrective vids to extract *)
      (poly_args @ D.nd_rcv_corr_args)
      [t_int; t_int] @@
      (* accumulate delta for this vid and all following vids. This is a very
        * sensitive point in the protocol and it's essential this only be done
        * once for a given corrective *)
    (* skip over function variant *)
    mk_poly_skip_block fn_nm [

      (* NOTE: don't check tag, because we need to convert to delta tuples, and if cannot
         return 2 different collections for materialization purposes *)
      (* convert to delta_tuples *)
      mk_let ["delta_tuples"]
        (mk_poly_fold_tag' map_delta.id
          (mk_lambda4' ["acc", map_delta.t] p_idx p_off map_delta.e @@
            mk_insert_block "acc" @@ ids_to_vars @@ fst_many map_delta.e) @@
          mk_empty map_delta.t) @@

      (* increment the idx, offsets of the map variant *)
      mk_poly_skip_all_block map_delta.id [
        mk_apply'
          (D.nd_add_delta_to_buf_nm c m) @@
            (* pass the map indirection, false=not corrective *)
          [mk_var buf_map_nm; mk_cbool false; mk_var "vid"; mk_var "delta_tuples"];

        (* this data structure is required *)
        mk_check_tag' (ios_tag c ds_tag) @@
        (* for every computation vid, only execute if we have all the updates *)
        mk_let ["sent_msgs"]
          (mk_poly_fold_tag' ds_tag
            (mk_lambda4' ["acc_count", t_int] p_idx p_off ["compute_vid", t_vid] @@
              (* get the stmt counter *)
              mk_let ["cntr"]
                (mk_case_ns (mk_lookup' D.nd_stmt_cntrs.id
                  [mk_tuple [mk_var "compute_vid"; mk_cint s]; mk_cunknown]) "lkup"
                  (* 0 if we have no value *)
                  (mk_cint 0) @@
                  mk_fst @@ mk_snd @@ mk_var "lkup") @@
                (* check if our stmt_counter is 0 *)
                mk_if (mk_eq (mk_var "cntr") @@ mk_cint 0)
                  (* if so, get bound vars from log *)
                  (let args = fst_many @@ D.args_of_t c t in
                  (if args <> [] then
                    mk_let args
                      (mk_apply'
                        (nd_log_get_bound_for t) [mk_var "compute_vid"])
                  else id_fn) @@
                    (* do_corrective, return number of msgs *)
                    mk_add (mk_var "acc_count") @@
                      mk_apply'
                        (do_corrective_name_of_t c t s m) @@
                          (ids_to_vars @@ fst_many orig_vals) @
                          args_of_t_as_vars_with_v ~vid:"compute_vid" c t @
                          [mk_var "delta_tuples"]) @@
                  (* else, do nothing *)
                  mk_var "acc_count")
              (mk_cint 0)) @@
          (* send an ack with how many msgs we sent on *)
          D.buffer_for_send nd_rcv_corr_done_nm "orig_addr"
            [mk_var "orig_vid"; mk_var "orig_stmt_id"; mk_var "hop"; mk_var "sent_msgs"];

          (* skip the vid section and return the new idx, offset *)
          mk_poly_skip_all' ds_tag
      ]
    ])
  s_rhs

(* do corrective functions *)
(* very similar to do_complete, but we only have to do it for certain
 * (stmt, map) combinations.
 * Do_corrective , unlike do_complete, doesn't need to add to an earlier vid
 * value. Instead, it adds to the value that already resides at a specific
 * vid and propagates. *)
let nd_do_corrective_fns c s_rhs ast trig_name corrective_maps =
  let do_corrective_fn (stmt_id, map_id) =
    let tuple_typ = wrap_t_calc' @@ P.map_types_for c.p map_id in
    let fn_nm = do_corrective_name_of_t c trig_name stmt_id map_id in
    let fn_args = D.orig_vals @ args_of_t_with_v c trig_name @ ["delta_tuples", tuple_typ] in
    mk_global_fn fn_nm fn_args [t_int] @@
        let lmap = P.lhs_map_of_stmt c.p stmt_id in
        let send_corr_fn = send_corrective_name_of_t c lmap in
        let args, is_col, ast = M.modify_corr_ast c ast map_id stmt_id trig_name in
        let delta = "delta_vals" in

        (* if we have loop vars, we need to filter the lmap values here *)
        let_lmap_filtering c delta stmt_id lmap
        (* We *can't* filter out 0 values, because they may add to a map
         * that didn't have any value, and initialize a value at that key *)
          (mk_flatten @@ mk_map (mk_lambda' args ast) @@ mk_var "delta_tuples")
            ~alt:(mk_cint 0) @@
            mk_block [
              (* add delta *)
              do_add_delta c (mk_var delta) lmap ~corrective:true;
              (* send correctives *)
              if List.exists ((=) lmap) corrective_maps
              (* send correctives with hop + 1, and return the num of correctives *)
              then mk_apply' send_corr_fn @@
                (modify_e orig_vals ["hop", mk_add (mk_cint 1) @@ mk_var "hop"]) @
                [mk_var "vid"; mk_var delta]
              (* if we have no more correctives, return 0 *)
              else mk_cint 0
            ]
  in
  List.map do_corrective_fn s_rhs

(*** central triggers to handle dispatch for nodes and switches ***)

let trig_dispatcher_nm = "trig_dispatcher"
let trig_dispatcher c =
  mk_global_fn trig_dispatcher_nm ["poly_queue", poly_queue.t] [] @@
  mk_block [
    (* replace all used slots with empty polyqueues *)
    mk_iter_bitmap'
      (mk_insert_at poly_queues.id (mk_var "ip") [mk_empty poly_queue.t])
      D.poly_queue_bitmap.id;

    (* clean out the send bitmaps *)
    mk_set_all D.poly_queue_bitmap.id [mk_cfalse];

    (* iterate over all buffer contents *)
    mk_poly_iter' @@
      mk_lambda'' ["tag", t_int; "idx", t_int; "offset", t_int] @@
        List.fold_left
          (fun acc_code (itag, (stag, typ, id_ts)) -> match typ with
             | Trig | DsTrig ->
              mk_if (mk_eq (mk_var "tag") @@ mk_cint itag)
                (* look up this tag *)
                (mk_poly_at_with' stag @@ mk_lambda' id_ts @@
                  (* if we have subdata, the function must return the new idx, offset *)
                  let call l =
                    mk_apply' stag @@ ids_to_vars @@ fst_many @@ l @ id_ts in
                  if typ = DsTrig then call D.poly_args
                    (* otherwise we must skip ourselves *)
                  else mk_block [call []; mk_poly_skip' stag])
                acc_code
             | _ -> acc_code)
          (mk_error "unmatched tag")
          c.poly_tags;

    (* send (move) the polyqueues *)
    mk_iter_bitmap'
      (* move and delete the poly_queue and ship it out *)
      (D.mk_sendi trig_dispatcher_trig_nm (mk_var "ip")
         [mk_delete_at poly_queues.id @@ mk_var "ip"])
      D.poly_queue_bitmap.id;
  ]

(* trigger version of dispatcher. Called by non-corrective mode *)
let trig_dispatcher_trig c =
  mk_code_sink' trig_dispatcher_trig_nm ["poly_queue", poly_queue.t] [] @@
    mk_apply' trig_dispatcher_nm [mk_var "poly_queue"]

(* buffer for dispatcher to reorder poly msgs *)
let nd_dispatcher_buf =
  let e = ["num", t_int; "poly_queue", poly_queue.t] in
  create_ds "dispatcher_buf" ~e @@ wrap_tmap' @@ snd_many e

(* remember the last number we've seen *)
let nd_dispatcher_last_num = create_ds "nd_dispatcher_last_num" @@ mut t_int

(* trig dispatcher from switch to node. accepts a msg number telling it how to order messages *)
(* @msg_num: number of consecutive message. Used to order buffers in corrective mode
   so we avoid having too many correctives. *)
let nd_trig_dispatcher_trig c =
  mk_code_sink' nd_trig_dispatcher_trig_nm ["num", t_int; "poly_queue", poly_queue.t] [] @@
  mk_let ["next_num"]
    (mk_if (mk_eq (mk_var nd_dispatcher_last_num.id) @@ mk_var g_max_int.id)
       (mk_cint 0) @@
       mk_add (mk_var nd_dispatcher_last_num.id) @@ mk_cint 1) @@
  mk_block [
    (* check if we're contiguous *)
    mk_if (mk_eq (mk_var "num") (mk_var "next_num"))
      (* then dispatch right away *)
      (mk_block [
          mk_assign nd_dispatcher_last_num.id @@ mk_var "next_num";
          mk_apply' trig_dispatcher_nm [mk_var "poly_queue"];
       ]) @@
      (* else, stash the poly_queue in our buffer *)
      mk_insert nd_dispatcher_buf.id [mk_var "num"; mk_var "poly_queue"]
    ;
    (* check if the next num is in the buffer *)
    mk_delete_with nd_dispatcher_buf.id [mk_var "next_num"; mk_cunknown]
      (mk_lambda' unit_arg @@ mk_cunit)
      (* recurse with the next number *)
      (mk_lambda'' ["x", wrap_ttuple @@ snd_many nd_dispatcher_buf.e] @@
        mk_send nd_trig_dispatcher_trig_nm G.me_var [mk_var "x"])
  ]

(* The driver trigger: loop over the even data structures as long as we have spare vids *)
(* This only fires when we get the token *)
let sw_event_driver_trig c =
  let trig_list = StrSet.of_list @@ P.get_trig_list c.p in
  mk_code_sink' sw_event_driver_trig_nm
    ["vid", t_vid; "vector_clock", TS.sw_vector_clock.t] [] @@
    (* if we're initialized and we have stuff to send *)
    mk_if (mk_and (mk_var D.sw_init.id) @@ mk_gt (mk_size @@ mk_var D.sw_event_queue.id) @@ mk_cint 0)
      (mk_case_sn (mk_peek @@ mk_var D.sw_event_queue.id) "poly_queue"
        (mk_block [
          (* replace all used send slots with empty polyqueues *)
          mk_iter_bitmap'
            (mk_insert_at poly_queues.id (mk_var "ip") [mk_empty poly_queue.t])
            D.poly_queue_bitmap.id;
          (* clean out the send bitmaps *)
          mk_set_all D.poly_queue_bitmap.id [mk_cfalse];
          (* for debugging, sleep if we've been asked to *)
          mk_if (mk_neq (mk_var D.sw_event_driver_sleep.id) @@ mk_cint 0)
            (mk_apply' "sleep" [mk_var D.sw_event_driver_sleep.id])
            mk_cunit;
          (* calculate the next vid *)
          mk_let ["next_vid"]
            (mk_poly_fold
              (mk_lambda4' ["vid", t_int] p_tag p_idx p_off @@
                mk_block [
                  List.fold_left (fun acc_code (i, (nm,_,id_ts)) ->
                    (* check if we match on the id *)
                    mk_if (mk_eq (mk_var "tag") @@ mk_cint i)
                      (if nm = "sentinel" then
                        (* don't check size of event queue *)
                        Proto.sw_seen_sentinel ~check_size:false
                      else
                        mk_poly_at_with' nm @@
                          (* buffer the message *)
                          mk_lambda' id_ts @@
                            mk_apply' (send_fetch_name_of_t nm) @@
                              ids_to_vars @@ "vid":: fst_many id_ts)
                      acc_code)
                  (mk_error "mismatch on event id") @@
                  List.filter (function (_,(nm,e,_)) ->
                      e = Event && (StrSet.mem nm trig_list || nm = "sentinel"))
                    c.poly_tags
                ;
                mk_add (mk_cint 1) @@ mk_var "vid"
                ])
              (mk_var "vid") @@
              mk_var "poly_queue") @@
          mk_block [
            (* update the vector clock by what we're sending *)
            mk_iter_bitmap'
              (mk_update_at_with "vector_clock" (mk_var "ip") @@
                mk_lambda' ["x", t_int] @@
                  (* if we're at max_int, skip to 0 so we don't get negatives *)
                  mk_if (mk_eq (mk_var "x") @@ mk_var g_max_int.id)
                    (mk_cint 0) @@
                    mk_add (mk_cint 1) @@ mk_var "x")
                  D.poly_queue_bitmap.id;
            (* send (move) the outgoing polyqueues *)
            mk_iter_bitmap'
              (* move and delete the poly_queue and ship it out with the vector clock num,
                 but only if we're in corrective mode *)
              (mk_if (mk_var D.corrective_mode.id)
                (D.mk_sendi nd_trig_dispatcher_trig_nm (mk_var "ip")
                  [mk_at' "vector_clock" @@ mk_var "ip";
                    mk_delete_at poly_queues.id @@ mk_var "ip"]) @@
                D.mk_sendi trig_dispatcher_trig_nm (mk_var "ip")
                  [mk_delete_at poly_queues.id @@ mk_var "ip"]) @@
              D.poly_queue_bitmap.id;
            (* send the new (vid, vector clock). make sure it's after we use vector
               clock data so we can move *)
            mk_send sw_event_driver_trig_nm (mk_var TS.sw_next_switch_addr.id)
              [mk_var "next_vid"; mk_var "vector_clock"];
            (* finish popping the incoming queue *)
            mk_delete D.sw_event_queue.id [mk_var "poly_queue"];
            (* update highest vid seen *)
            mk_assign TS.sw_highest_vid.id @@ mk_var "next_vid";
            (* check if we're done *)
            Proto.sw_check_done ~check_size:true;
            mk_var "next_vid"
          ]]) @@
        mk_error "oops") @@
     (* otherwise send the same vid and vector clock *)
     mk_send sw_event_driver_trig_nm (mk_var TS.sw_next_switch_addr.id)
       [mk_var "vid"; mk_var "vector_clock"]

let sw_demux_ctr = create_ds "sw_demux_ctr" @@ mut t_int
let sw_demux_max =
  let init = mk_cint 5 in
  create_ds ~init "sw_demux_max" @@ t_int
let sw_demux_poly_queue =
  create_ds "sw_demux_poly_queue" @@ poly_queue.t

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
  let sentinel_code =
    (* stash the sentinel index in the queue *)
    mk_if (mk_eq (mk_fst @@ mk_var "args") @@ mk_cstring "")
      (mk_poly_insert "sentinel" "sw_demux_poly_queue" [mk_cunit])
      (mk_error "unhandled event")
  in
  mk_code_sink' sw_demux_nm ["args", wrap_ttuple @@ List.map str_of_date_t combo_t] [] @@
  (* create a poly queue with a given number of messages *)
  mk_block [
    StrMap.fold (fun trig arg_indices acc ->
      let args =
        (* add 1 for tuple access *)
        List.map (fun i -> convert_date i @@ mk_subscript (i+1) @@ mk_var "args")
          arg_indices
      in
      mk_if (mk_eq (mk_fst @@ mk_var "args") @@ mk_cstring trig)
        (mk_if (mk_eq (mk_snd @@ mk_var "args") @@ mk_cint 1)
          (mk_poly_insert ("insert_"^trig) "sw_demux_poly_queue" @@ args) @@
           mk_poly_insert ("delete_"^trig) "sw_demux_poly_queue" @@ args)
        acc)
      t_arg_map
      sentinel_code;
    (* increment counter *)
    mk_assign sw_demux_ctr.id @@ mk_add (mk_cint 1) @@ mk_var sw_demux_ctr.id;
    (* ship off if we hit the count or saw the sentinel *)
    mk_if (mk_or (mk_geq (mk_var sw_demux_ctr.id) @@ mk_var sw_demux_max.id) @@
                  mk_eq (mk_fst @@ mk_var "args") @@ mk_cstring "")
      (mk_block [
        (* copy the polybuffer to the queue *)
        mk_insert sw_event_queue.id [mk_var sw_demux_poly_queue.id];
        (* clear the buffer *)
        mk_clear_all sw_demux_poly_queue.id;
        (* clear the counter *)
        mk_assign sw_demux_ctr.id @@ mk_cint 0;
        ])
      mk_cunit
  ]


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
  (* TODO: dummy map currently needed for MapE generation *)
  mk_global_val "dummy_map" (wrap_tmap' [t_int; wrap_tbag' [t_int; t_int]]) ::
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
  [K3Route.calc_dim_bounds] @ (* needed for global_vars *)
  K3Route.global_vars c partmap @
  K3Shuffle.global_vars @
  GC.global_vars c @
  List.map decl_global
    [send_put_bitmap;
     send_put_ip_map c.p;
     send_fetch_bitmap;
     send_fetch_ip_map;
     sw_demux_ctr;
     sw_demux_max;
     sw_demux_poly_queue;
     nd_dispatcher_buf;
     nd_dispatcher_last_num;
    ]

let declare_global_funcs c partmap ast =
  flatteners c @
  nd_log_master_write ::
  (P.for_all_trigs ~sys_init:true ~delete:c.gen_deletes c.p @@ nd_log_write c) @
  (P.for_all_trigs ~sys_init:true ~delete:c.gen_deletes c.p @@ nd_log_get_bound c) @
  nd_update_stmt_cntr_corr_map ::
  begin if c.gen_correctives then [nd_filter_corrective_list] else [] end @
  K3Ring.functions @
  (List.map (fun (i, (_, maps)) -> nd_add_delta_to_buf c (hd maps)) @@ D.uniq_types_and_maps c) @
  TS.functions @
  K3Route.functions c partmap @
  K3Shuffle.functions c @
  GC.functions c (Proto.sw_check_done ~check_size:true)

(* Generate all the code for a specific trigger *)
let gen_dist_for_t c ast trig =
  (* (stmt_id, rhs_map_id)list *)
  let s_rhs = P.s_and_over_stmts_in_t c.p P.rhs_maps_of_stmt trig in
  (* stmts that can be involved in correctives *)
  let s_rhs_corr = List.filter (fun (s, map) -> List.mem map c.corr_maps) s_rhs in
  (* (stmt_id,rhs_map_id,lhs_map_id) list *)
  let s_rhs_lhs = P.s_and_over_stmts_in_t c.p P.rhs_lhs_of_stmt trig in
  (* functions *)
  let functions = [
    sw_send_fetch_fn c s_rhs_lhs s_rhs trig;
  ] @
   nd_do_complete_fns c ast trig c.corr_maps @
   (if c.gen_correctives then
     nd_do_corrective_fns c s_rhs_corr ast trig c.corr_maps else [])
  in
  let functions2 = nd_send_push_stmt_map_trig c s_rhs_lhs trig in
  let ex_trigs =
    (if null s_rhs then []
    else
      [nd_rcv_put_trig c trig;
      nd_rcv_fetch_trig c trig])
    @
    nd_rcv_push_trig c s_rhs trig @
    nd_do_complete_trigs c trig @
    (if c.gen_correctives then
      nd_rcv_correctives_trig c s_rhs_corr trig
    else [])
  in
  ex_trigs, functions, functions2

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

  let c = { default_config with
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
      route_indices = P.route_access_patterns p;
    } in
  (* to get poly_tags, we need c *)
  let c = { c with poly_tags = D.calc_poly_tags c } in
  (* regular trigs then insert entries into shuffle fn table *)
  let proto_semi_trigs, proto_funcs, proto_funcs2 =
    (fun (x,y,z) -> let a = List.flatten in a x, a y, a z) @@ list_unzip3 @@
      P.for_all_trigs ~sys_init:true ~delete:c.gen_deletes c.p @@ gen_dist_for_t c ast
  in
  let prog =
    mk_typedef poly_queue_typedef_id (poly_queue_typedef c) ::
    declare_global_vars c partmap ast @
    declare_global_funcs c partmap ast @
    (if c.gen_correctives then send_corrective_fns c else []) @
    proto_funcs2 @
    (* we need this here for scope *)
    nd_exec_buffered_fetches c @
    nd_complete_stmt_cntr_check c ::
    nd_check_stmt_cntr_index c ::
    proto_funcs @
    proto_semi_trigs @
    nd_rcv_corr_done c ::
    trig_dispatcher c ::
    [mk_flow @@
      Proto.triggers c @
      GC.triggers c @
      TS.triggers @
      Timer.triggers c @
      [
        trig_dispatcher_trig c;
        nd_trig_dispatcher_trig c;
        sw_event_driver_trig c;
        sw_demux c;
      ]
    ] @
    roles_of c ast
  in
  snd @@ U.renumber_program_ids prog

