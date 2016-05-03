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

(* Generated functions common to a few parts of the protocol *)

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
  mk_global_fn ~wr_all:true (nd_log_write_for c.p t) (args_of_t_with_v c t) [] @@
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

let nd_check_stmt_cntr_do_delete = create_ds "nd_check_stmt_cntr_do_delete" @@ mut t_bool
let nd_check_stmt_cntr_ret = create_ds "nd_check_stmt_cntr_ret" @@ mut t_bool
let nd_check_stmt_cntr_init = create_ds "nd_check_stmt_cntr_init" @@ mut t_bool

let nd_check_stmt_cntr_index_nm = "nd_check_stmt_cntr_index"
let nd_check_stmt_cntr_index c =
  let add_to_count, new_count, has_data, new_modify =
    "add_to_count", "new_count", "has_data", "new_modify" in
  (* lmap -> stmts *)
  let h = Hashtbl.create 20 in
  List.iter (fun (s,m) ->
    hashtbl_replace h m @@ function None -> [s] | Some s' -> s::s') @@
    P.stmts_lhs_maps c.p;
  let lmap_stmts = list_of_hashtbl h in
  mk_global_fn nd_check_stmt_cntr_index_nm
    (["vid", t_vid; "stmt_id", t_int; add_to_count, t_int; has_data, t_bool])
    [t_bool] (* return whether we should send the do_complete *)
    @@ mk_block [

      (* set do_delete, ret, init to false *)
      mk_assign nd_check_stmt_cntr_do_delete.id @@ mk_cfalse;
      mk_assign nd_check_stmt_cntr_ret.id @@ mk_cfalse;
      mk_assign nd_check_stmt_cntr_init.id @@ mk_cfalse;

      mk_update_at_with nd_stmt_cntrs_id (mk_var "stmt_id") @@
        mk_lambda' nd_stmt_cntrs_e @@

          mk_upsert_with_block "nd_stmt_cntrs_inner" [mk_var "vid"; mk_cunknown]
            (mk_lambda' unit_arg @@ mk_block [
              mk_assign nd_check_stmt_cntr_init.id @@ mk_ctrue;
              mk_incr nd_stmt_cntr_size.id;
              mk_tuple [mk_var "vid";
                        mk_tuple [mk_var add_to_count; mk_var has_data]]]) @@

            mk_lambda' nd_stmt_cntrs_inner.e @@
              (* calculate the new modify state *)
              mk_let [new_modify] (mk_or (mk_var has_data) @@ mk_snd @@ mk_var "stmt_cntr_info") @@
              (* calculate new_count *)
              mk_let [new_count]
                (mk_add (mk_var add_to_count) @@ mk_fst @@ mk_var "stmt_cntr_info") @@
              mk_block [
                prof_property D.prof_tag_push_decr @@ ProfPushBarrier("vid", "stmt_id", new_count);
                (* if counter is 0 and no modify, we need to delete *)
                mk_if (mk_and (mk_eq (mk_var new_count) @@ mk_cint 0) @@
                        mk_not @@ mk_var new_modify)
                  (mk_assign nd_check_stmt_cntr_do_delete.id mk_ctrue)
                  mk_cunit;
                (* return whether the counter is 0 and we have some data *)
                mk_if (mk_and (mk_eq (mk_var new_count) @@ mk_cint 0) @@
                              mk_var new_modify)
                  (mk_block [
                    (* for profiling: mark push as done *)
                    prof_property D.prof_tag_push_done @@ ProfLatency("vid", "stmt_id");
                    mk_assign nd_check_stmt_cntr_ret.id mk_ctrue
                  ])
                  mk_cunit;
                (* update the counter *)
                mk_tuple [mk_var "vid"; mk_tuple [mk_var new_count; mk_var new_modify]]
              ]
      ;
      (* carry out delete if needed *)
      mk_if (mk_var nd_check_stmt_cntr_do_delete.id)
          (mk_apply' nd_complete_stmt_cntr_check_nm [mk_var "vid"; mk_var "stmt_id"])
          mk_cunit
      ;
      (* For no-corrective mode, add to per-map stmt cntrs *)
      mk_if (mk_and (mk_not @@ mk_var D.corrective_mode.id) @@
                    mk_var nd_check_stmt_cntr_init.id)
        (List.fold_left (fun acc (m, ss) ->
          let mk_check_s s = mk_eq (mk_var "stmt_id") @@ mk_cint s in
          (* if we have any of the stmts with a given lmap *)
          mk_if
            (list_fold_to_last
              (fun acc s -> mk_or (mk_check_s s) acc) mk_check_s ss)
            (* add to per-map stmt counters *)
            (mk_update_at_with nd_stmt_cntrs_per_map_id (mk_cint m) @@
              mk_lambda' nd_stmt_cntrs_per_map_e @@
                mk_insert_block "inner" [mk_var "vid"; mk_var "stmt_id"])
            acc)
          mk_cunit
          lmap_stmts)
        mk_cunit
      ;
      (* return value *)
      mk_var nd_check_stmt_cntr_ret.id
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


(* check for complicated double loop vars which necessitate lmap filtering *)
(* Used by do_complete and do_corrective *)
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
             mk_if (mk_is_member' R.route_bitmap.id @@ mk_var D.me_int.id)
               (mk_tuple [mk_ctrue; mk_insert_block "acc" [mk_var delta]]) @@
               mk_tuple [mk_var do_action; mk_var "acc"])
           (mk_tuple [mk_cfalse; mk_empty @@ wrap_t_calc' lmap_ts])
          let_bind) @@
        (* if we have no real value, do nothing *)
        mk_if (mk_var do_action) body alt
    else
      (* normal pathway - no complex loop vars *)
      mk_let [delta] let_bind body

(*
 * shared code btw do_complete and do_corrective
 * we add the delta to all following vids
 *)
let do_add_delta c e lmap ~corrective =
  mk_apply' (D.nd_add_delta_to_buf_nm c lmap) @@
    [mk_var @@ P.map_name_of c.p lmap;
      if corrective then mk_ctrue else mk_cfalse; mk_var "vid"; e]

(*** Poly queues ***)

(* common functionality to move out the poly queues *)
let send_poly_queues =
    (* send (move) the polyqueues *)
    mk_iter_bitmap'
      (* check if we have a upoly queue and act accordingly *)
        (mk_if (mk_is_member' upoly_queue_bitmap.id @@ mk_var "ip")
        (* move and delete the poly_queue and ship it out *)
          (mk_let ["pq"]
            (mk_delete_at poly_queues.id @@ mk_var "ip") @@
          mk_let_block ["upq"]
            (mk_delete_at upoly_queues.id @@ mk_var "ip")
            [ prof_property 0 @@ ProfSendUPoly("batch_id", "ip", "pq", "upq");
              mk_sendi trig_dispatcher_trig_unique_nm (mk_var "ip")
                [mk_tuple [mk_var "batch_id"; mk_var "pq"; mk_var "upq"]]
            ])
        (mk_let_block ["pq"]
          (mk_delete_at poly_queues.id @@ mk_var "ip")
          [ prof_property 0 @@ ProfSendPoly("batch_id", "ip", "pq");
            mk_sendi trig_dispatcher_trig_nm (mk_var "ip")
              [mk_var "batch_id"; mk_var "pq"]
          ]))
      poly_queue_bitmap.id

(* code to apply poly_reserve to every outgoing polybuffer *)
let reserve_poly_queue_code ?all c =
  mk_if (mk_var do_poly_reserve.id)
    (mk_ignore @@ mk_agg
      (mk_lambda2' ["ip", t_int] unknown_arg @@
       mk_block [
        mk_update_at_with poly_queues.id (mk_var "ip") @@
          mk_lambda' ["pqs", t_of_e poly_queues.e] @@ mk_block [
            mk_poly_reserve "pqs"
              (mk_mult (mk_var sw_poly_batch_size.id) @@ mk_cint reserve_mult)
              (mk_mult (mk_var sw_poly_batch_size.id) @@ mk_cint @@ max_poly_queue_csize c) @@
              mk_mult (mk_var sw_poly_batch_size.id) @@ mk_cint reserve_str_estimate;
            mk_var "pqs" ];
        mk_add (mk_cint 1) @@ mk_var "ip"
       ])
      (mk_cint 0) @@
      mk_var my_peers.id)
    mk_cunit

let clear_poly_queues_fn_nm = "clear_poly_queues"
let clear_poly_queues_fn c =
  mk_global_fn clear_poly_queues_fn_nm [] [] @@
  mk_block [
    (* replace all used send slots with empty polyqueues *)
    mk_iter_bitmap'
      (mk_insert_at poly_queues.id (mk_var "ip")
         [mk_var empty_poly_queue.id])
      poly_queue_bitmap.id;
    mk_iter_bitmap'
        (mk_insert_at upoly_queues.id (mk_var "ip")
          [mk_var empty_upoly_queue.id])
        upoly_queue_bitmap.id ;
    (* apply reserve to all the new polybufs *)
    reserve_poly_queue_code c;
    mk_clear_all poly_queue_bitmap.id;
    mk_clear_all upoly_queue_bitmap.id;
  ]

(* we must always clear both upoly and poly queues -- we don't know when we'll need them *)
let clear_poly_queues c = mk_apply' clear_poly_queues_fn_nm []

(*** Buffering snippets ***)

(* instead of sending directly, place in the send buffer *)
(* @bitmap: whether to mark the bitmap *)
let buffer_for_send ?(unique=false) ?(wr_bitmap=true) t addr args =
  let queue, bitmap =
    if unique then upoly_queues, upoly_queue_bitmap.id
    else poly_queues,  poly_queue_bitmap.id in
  mk_block @@
    (* mark the bitmap *)
    (if wr_bitmap then [mk_insert bitmap [mk_var addr]] else []) @
    (* insert into buffer *)
    [mk_update_at_with queue.id (mk_var addr) @@
      mk_lambda' ["pqs", t_of_e queue.e] @@
        mk_poly_insert_block t "pqs" args
    ]

(* code to check if we need to write trig args and a trig header, and if so, to buffer them *)
let buffer_trig_header_if_needed ?(force=false) ?(need_args=true) vid t addr args =
  (* normal condition for adding *)
  let save_handler = trig_save_arg_sub_handler_name_of_t t in
  let load_handler = trig_load_arg_sub_handler_name_of_t t in
  let no_arg_handler = trig_no_arg_sub_handler_name_of_t t in
  (* check bitmap for current header *)
  (if not force then
    mk_if (mk_is_member' send_trig_header_bitmap.id @@ mk_var addr) mk_cunit
   else id_fn) @@
    mk_block @@
      (* update the header bitmap *)
      [mk_insert send_trig_header_bitmap.id [mk_var addr]] @
      (
        if need_args && not force then
          [
            (* check map for the last batch to see if we need args *)
            mk_let ["has_args"]
              (mk_at_with' send_trig_args_map.id (mk_var addr) @@
               mk_lambda' send_trig_args_map.e @@
               mk_case_ns (mk_lookup' "inner" [vid; mk_cunknown]) "x" mk_cfalse mk_ctrue) @@
            mk_if (mk_var "has_args")
              (buffer_for_send load_handler addr [mk_var "vid"]) @@
               mk_block [
                  (* update map *)
                  mk_insert send_trig_args_bitmap.id [mk_var addr];
                  mk_update_at_with send_trig_args_map.id (mk_var addr) @@
                    mk_lambda' send_trig_args_map.e @@
                      mk_insert_block send_trig_args_inner.id [vid; mk_cunit];
                  (* use full handler (which also saves) *)
                  buffer_for_send save_handler addr args
              ]
          ]
      else
        [buffer_for_send no_arg_handler addr args])

(* insert tuples into polyqueues *)
let buffer_tuples_from_idxs ?(unique=false) ?(drop_vid=false) tuples_nm map_type map_tag indices =
  let col_t, tup_t = unwrap_tcol map_type in
  let bitmap = if unique then upoly_queue_bitmap else poly_queue_bitmap in
  let ts = unwrap_ttuple tup_t in
  (* handle dropping vid *)
  let may_drop e =
    if drop_vid then
      List.map (flip mk_subscript e) @@ tl @@ fst_many @@ insert_index_fst ~first:1 ts
    else [e]
  in
  (* check for empty collection *)
  mk_case_sn (mk_peek indices) "x"
    (* check for -1, indicating all tuples *)
    (mk_block [
      mk_insert bitmap.id [mk_var "ip"];
      mk_if (mk_eq (mk_var "x") @@ mk_cint (-1))
        (mk_iter
          (mk_lambda'' ["x", tup_t] @@
            (* no need to write bitmap - we did it above *)
            buffer_for_send ~unique ~wr_bitmap:false map_tag "ip" @@ may_drop @@ mk_var "x") @@
          mk_var tuples_nm) @@
        (* or just regular indices into tuples *)
        mk_iter
          (mk_lambda'' ["idx", t_int] @@
            (* get tuples at idx *)
            mk_at_with' tuples_nm (mk_var "idx") @@
              mk_lambda' ["x", tup_t] @@
            buffer_for_send ~unique ~wr_bitmap:false map_tag "ip" @@ may_drop @@ mk_var "x")
          indices])
    mk_cunit (* do nothing if empty. we're sending a header anyway *)

let functions c =
   clear_poly_queues_fn c
