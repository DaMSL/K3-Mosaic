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

open GenCommon

(* {ip} *)
let send_push_bitmap = create_ds "send_push_bitmap" t_bitset

(* indexed by ip *)
let send_push_cntrs =
  let e = ["count2", t_int; "has_data2", t_bool] in
  let init = mk_map (mk_lambda' unknown_arg @@ mk_tuple [mk_cint 0; mk_cfalse]) @@
    mk_var D.my_peers.id in
  create_ds ~e ~init "send_push_cntrs" @@ wrap_tvector @@ t_of_e e

(* Trigger_send_push_stmt_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 * Produces a list of triggers.
 * We're parametrized by stmt and map_id because we save repeating the
 * processing on the switch side to figure out which maps (in which stmts) we
 * have locally
 *)
let nd_send_push_stmt_map_trig c t s =
  List.map
    (fun (rmap, lmap) ->
      let rmap_nm = P.map_name_of c.p rmap in
      let rmap_deref = "rmap_d" in
      let buf_nm = P.buf_of_stmt_map_id c.p s rmap in
      let shuffle_fn = K3S.find_shuffle_nm c s rmap lmap in
      let shuffle_key, empty_pat_idx = P.key_pat_from_bound c.p c.route_indices s lmap in
      (* if we use optimized route, no need for conservative shuffling *)
      let empty_pat_idx = if is_opt_route_stmt_map c s rmap then -1 else empty_pat_idx in
      let shuffle_pat_idx = P.get_shuffle_pat_idx c.p c.route_indices s lmap rmap in
      let slice_key = P.slice_key_from_bound c.p s rmap in
      let map_delta = D.map_ds_of_id c rmap ~global:false in
      let map_real  = D.map_ds_of_id c rmap ~global:true in
      let map_pat = D.pat_of_ds map_real ~flatten:true ~expr:(mk_var "tuple") in
      let rcv_push_nm = rcv_push_name_of_t t s in
      (* for special routing *)
      let bound_params = insert_index_fst @@ bound_params_of_stmt c s in
      let idx_rmaps = insert_index_snd ~first:1 @@ P.nonempty_rmaps_of_stmt c.p s in
      let single_rmap = List.length idx_rmaps = 1 in
      let swallow_f f e = if single_rmap then e else f e in
      (* a pattern mapping real map with delta vars *)
      mk_global_fn
        (send_push_name_of_t c t s rmap)
        (("buffered", t_bool)::args_of_t_with_v c t) [] @@
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
        mk_block @@ [
          (* for profiling: mark this as a buffered push if needed *)
          mk_if (mk_var "buffered")
            (prof_property prof_tag_buffered_push @@ ProfLatency("vid", soi s)) mk_cunit;

          (* fill in the global shuffle data structures *)
          mk_apply' shuffle_fn @@
            shuffle_key@[mk_cint shuffle_pat_idx; mk_cint empty_pat_idx; mk_var "tuples"]] @

          (* for optimized route, we need to add the destination of the nodes
           * from the route_opt_push data structure to the shuffle bitmap
           * for sending empty messages *)
          (if is_opt_route_stmt_map c s rmap then singleton @@
            mk_let ["buckets"]
              (List.fold_left (fun acc_code (idx, (id, (m, m_idx))) ->
                  mk_let ["bucket_"^soi idx]
                    (R.get_dim_idx c.p m m_idx @@ mk_var id)
                    acc_code)
                (* string the buckets together *)
                (mk_tuple @@ List.map (fun idx -> mk_var @@ "bucket_"^soi idx) @@
                  fst_many bound_params)
                bound_params) @@
            (* lookup in the optimized route ds *)
            let m_idx = List.assoc rmap idx_rmaps in
            mk_case_ns (mk_lookup' (R.route_opt_push_ds_nm s)
                         [mk_var "buckets"; mk_cunknown]) "lkup"
              (mk_error "couldn't find buckets in optimized ds") @@
              (* look for our entry in the ds *)
              mk_case_ns (mk_lookup (swallow_f (mk_subscript m_idx) @@
                                     mk_snd @@ mk_var "lkup")
                          [mk_var D.me_int.id; mk_cunknown]) "lkup2"
                (* do nothing if we're not in the ds *)
                mk_cunit @@
                (* otherwise add to shuffle bitmap for empty msgs *)
                mk_iter_bitmap
                  (mk_insert K3S.shuffle_bitmap.id [mk_var "ip"]) @@
                  (* index the particular tuple that matches our map *)
                  mk_snd @@ mk_var "lkup2"
          else []) @
          [
          (* for profiling, count the number of empty and full push messages *)
          mk_if (mk_var do_profiling.id)
            (mk_block [
              mk_assign D.prof_num_empty.id @@ mk_cint 0;
              mk_assign D.prof_num_full.id @@ mk_cint 0;
              mk_iter_bitmap'
                (mk_at_with' K3S.shuffle_results.id (mk_var "ip") @@
                  mk_lambda' K3S.shuffle_results.e @@
                    mk_if (mk_var "has_data")
                      (mk_incr D.prof_num_full.id) @@
                       mk_incr D.prof_num_empty.id)
                K3S.shuffle_bitmap.id;
              prof_property prof_tag_push_cnts @@ ProfMsgCounts("vid", D.prof_num_empty.id, D.prof_num_full.id)
            ])
            mk_cunit;

          (* iterate and buffer *)
          mk_iter_bitmap'
            (mk_at_with' K3S.shuffle_results.id (mk_var "ip") @@
              mk_lambda' K3S.shuffle_results.e @@
                mk_block [
                  (* buffered pushes need to do a lot of the work themselves, rather than in rcv_fetch *)
                  mk_if (mk_var "buffered")
                    (mk_block [
                      (* we need to force a trig header *)
                      buffer_trig_header_if_needed ~force:true ~need_args:false (mk_var "vid") t "ip" [mk_var "vid"];
                      (* check what metadata we need to send: batch_push or push metadata *)
                      (* single vid metadata *)
                      mk_if (mk_var "has_data")
                        (buffer_for_send ~wr_bitmap:false (rcv_push_nm^"_1") "ip" []) @@
                        buffer_for_send ~wr_bitmap:false (rcv_push_nm^"_1_no_data") "ip" [];
                    ]) @@
                    (* unbuffered *)
                    mk_block [
                      (* mark the push data structure *)
                      mk_insert send_push_bitmap.id [mk_var "ip"];
                      mk_update_at_with send_push_cntrs.id (mk_var "ip") @@
                        mk_lambda' send_push_cntrs.e @@
                          mk_tuple [mk_add (mk_var "count2") @@ mk_cint 1;
                                    mk_or (mk_var "has_data2") @@ mk_var "has_data"]
                    ];

                  (* buffer the map data according to the indices *)
                  buffer_tuples_from_idxs ~unique:true "tuples" map_delta.t buf_nm @@ mk_var "indices"
               ])
            K3S.shuffle_bitmap.id
        ]) @@ (* trigger *)
    P.rhs_lhs_of_stmt c.p s

(* indexed by map_id *)
let send_push_isobatch_inner = create_ds "inner" t_bitset

(* dest_ip -> {stmt_map_id} *)
let send_push_isobatch_map_id = "send_push_isobatch_map"
let send_push_isobatch_map_e = ["inner", send_push_isobatch_inner.t]
let send_push_isobatch_map c =
  let e = send_push_isobatch_map_e in
  let init =
    mk_map (mk_lambda' unknown_arg @@ mk_empty t_bitset) @@
      mk_var D.my_peers.id in
  create_ds ~e ~init send_push_isobatch_map_id @@ wrap_tvector @@ t_of_e e

(* {ip} *)
let send_push_isobatch_bitmap = create_ds "send_push_isobatch_bitmap" t_bitset

let clear_send_push_isobatch_ds_nm = "clear_send_push_isobatch_ds"
let clear_send_push_isobatch_ds =
  mk_global_fn clear_send_push_isobatch_ds_nm [] [] @@
  mk_block [
    (* clear the data structures *)
    mk_iter_bitmap'
      (mk_update_at_with send_push_isobatch_map_id (mk_var ip.id) @@
        mk_lambda' send_push_isobatch_map_e @@
          mk_clear_all_block "inner")
      send_push_isobatch_bitmap.id;
    mk_clear_all send_push_isobatch_bitmap.id;
  ]


(* Trigger_isobatch_send_push_stmt_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 * Produces a list of triggers.
 * We're parametrized by stmt and map_id because we save repeating the
 * processing on the switch side to figure out which maps (in which stmts) we
 * have locally
 *)
let nd_isobatch_send_push_stmt_map_trig c t s =
  List.map
    (fun (rmap, lmap) ->
      let s_m = fst @@ List.find (fun (_,(s',m)) -> s = s' && m = rmap) @@ P.stmt_map_ids c.p in
      let rmap_nm = P.map_name_of c.p rmap in
      let rmap_deref = "rmap_d" in
      let buf_nm = P.buf_of_stmt_map_id c.p s rmap in
      let shuffle_fn = K3S.find_shuffle_nm c s rmap lmap in
      let shuffle_key, empty_pat_idx = P.key_pat_from_bound c.p c.route_indices s lmap in
      (* if we use optimized route, no need for conservative shuffling *)
      let empty_pat_idx = if is_opt_route_stmt_map c s rmap then -1 else empty_pat_idx in
      let shuffle_pat_idx = P.get_shuffle_pat_idx c.p c.route_indices s lmap rmap in
      let slice_key = P.slice_key_from_bound c.p s rmap in
      let map_delta = D.map_ds_of_id c rmap ~global:false in
      let map_real  = D.map_ds_of_id c rmap ~global:true in
      let map_pat = D.pat_of_ds map_real ~flatten:true ~expr:(mk_var "tuple") in
      (* for special routing *)
      let bound_params = insert_index_fst @@ bound_params_of_stmt c s in
      let idx_rmaps = insert_index_snd ~first:1 @@ P.nonempty_rmaps_of_stmt c.p s in
      let single_rmap = List.length idx_rmaps = 1 in
      let swallow_f f e = if single_rmap then e else f e in
      (* a pattern mapping real map with delta vars *)
      mk_global_fn
        (send_push_isobatch_name_of_t c t s rmap)
        (("buffered", t_bool)::args_of_t_with_v c t) [] @@
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
        mk_block @@ [
          (* for profiling: mark this as a buffered push if needed *)
          mk_if (mk_var "buffered")
            (prof_property prof_tag_buffered_push @@ ProfLatency("vid", soi s)) mk_cunit;

          (* fill in the global shuffle data structures *)
          mk_apply' shuffle_fn @@
            shuffle_key@[mk_cint shuffle_pat_idx; mk_cint empty_pat_idx; mk_var "tuples"]] @

          (* for optimized route, we need to add the destination of the nodes
           * from the route_opt_push data structure to the shuffle bitmap
           * for sending empty messages *)
          (if is_opt_route_stmt_map c s rmap then singleton @@
            mk_let ["buckets"]
              (List.fold_left (fun acc_code (idx, (id, (m, m_idx))) ->
                  mk_let ["bucket_"^soi idx]
                    (R.get_dim_idx c.p m m_idx @@ mk_var id)
                    acc_code)
                (* string the buckets together *)
                (mk_tuple @@ List.map (fun idx -> mk_var @@ "bucket_"^soi idx) @@
                  fst_many bound_params)
                bound_params) @@
            (* lookup in the optimized route ds *)
            let m_idx = List.assoc rmap idx_rmaps in
            mk_case_ns (mk_lookup' (R.route_opt_push_ds_nm s)
                         [mk_var "buckets"; mk_cunknown]) "lkup"
              (mk_error "couldn't find buckets in optimized ds") @@
              (* look for our entry in the ds *)
              mk_case_ns (mk_lookup (swallow_f (mk_subscript m_idx) @@
                                     mk_snd @@ mk_var "lkup")
                          [mk_var D.me_int.id; mk_cunknown]) "lkup2"
                (* do nothing if we're not in the ds *)
                mk_cunit @@
                (* otherwise add to shuffle bitmap for empty msgs *)
                mk_iter_bitmap
                  (mk_insert K3S.shuffle_bitmap.id [mk_var "ip"]) @@
                  (* index the particular tuple that matches our map *)
                  mk_snd @@ mk_var "lkup2"
          else []) @
          [
          (* for profiling, count the number of empty and full push messages *)
          mk_if (mk_var do_profiling.id)
            (mk_block [
              mk_assign D.prof_num_empty.id @@ mk_cint 0;
              mk_assign D.prof_num_full.id @@ mk_cint 0;
              mk_iter_bitmap'
                (mk_at_with' K3S.shuffle_results.id (mk_var "ip") @@
                  mk_lambda' K3S.shuffle_results.e @@
                    mk_if (mk_var "has_data")
                      (mk_incr D.prof_num_full.id) @@
                       mk_incr D.prof_num_empty.id)
                K3S.shuffle_bitmap.id;
              prof_property prof_tag_push_cnts @@ ProfMsgCounts("vid", D.prof_num_empty.id, D.prof_num_full.id)
            ])
            mk_cunit;

          (* iterate, mark and buffer. we'll need a batch_push at the end *)
          mk_iter_bitmap'
            (mk_at_with' K3S.shuffle_results.id (mk_var "ip") @@
              mk_lambda' K3S.shuffle_results.e @@
                mk_block [
                  (* mark the push data structure *)
                  mk_insert send_push_isobatch_bitmap.id [mk_var "ip"];
                  (* insert the relevant stmt_map *)
                  mk_update_at_with send_push_isobatch_map_id (mk_var "ip") @@
                    mk_lambda' send_push_isobatch_map_e @@
                      mk_insert_block "inner" [mk_cint s_m];

                  (* buffer the map data according to the indices *)
                  buffer_tuples_from_idxs ~unique:true "tuples" map_delta.t buf_nm @@ mk_var "indices"
               ])
            K3S.shuffle_bitmap.id
        ]) @@ (* trigger *)
    P.rhs_lhs_of_stmt c.p s

let nd_send_isobatch_push_sent = create_ds "nd_send_isobatch_push_meta_sent" @@ mut t_bool

(* generic function to send the metadata of the batch pushes that have been built up so far *)
let nd_send_isobatch_push_meta_nm = "nd_send_isobatch_push_meta"
let nd_send_isobatch_push_meta c =
  let s_m = P.stmt_map_ids c.p in
  mk_global_fn nd_send_isobatch_push_meta_nm ["batch_id", t_vid] [] @@
    mk_block [
      mk_assign nd_send_isobatch_push_sent.id mk_cfalse;
      mk_iter_bitmap'
        (mk_at_with' send_push_isobatch_map_id (mk_var "ip") @@
            mk_lambda' send_push_isobatch_map_e @@ mk_block @@
            List.flatten @@ List.map (fun s ->
                let t = P.trigger_of_stmt c.p s in
                let num_rmaps = create_range ~first:1 @@ List.length @@
                  P.rhs_maps_of_stmt c.p s in
                if t = D.sys_init then [] else
                if str_prefix "delete_" t && not c.gen_deletes then [] else
                singleton @@
                mk_let ["count"]
                  (List.fold_left (fun acc_code (s_m, (_, m)) ->
                      mk_add
                        (mk_if (mk_is_member' "inner" @@ mk_cint s_m) (mk_cint 1) @@ mk_cint 0)
                        acc_code)
                    (mk_cint 0) @@
                    List.filter (fun (_,(s',_)) -> s' = s) s_m) @@
                mk_if_eq (mk_var "count") (mk_cint 0)
                  mk_cunit @@
                  mk_block [
                    mk_assign nd_send_isobatch_push_sent.id mk_ctrue;
                    List.fold_left (fun acc_code n ->
                      mk_if_eq (mk_var "count") (mk_cint n)
                        (buffer_for_send (rcv_push_isobatch_name_of_t t s^"_"^soi n) "ip"
                              [mk_var "batch_id"])
                        acc_code)
                      (mk_error "oops")
                      num_rmaps;
                  ]) @@
              P.get_stmt_list c.p)
        send_push_isobatch_bitmap.id;

      (* prepare the ds for next time if we used it *)
      mk_if (mk_var nd_send_isobatch_push_sent.id)
        (mk_apply' clear_send_push_isobatch_ds_nm []) mk_cunit
    ]

(* For no-corrective mode: execute buffered fetches *)
(* we have a balance between reads and writes. Reads are buffered in the
 * buffered_fetches, and writes are tracked in the stmt_cntrs. We split both
 * per-map to give us better granularity barriers, though even better ones could
 * be made if we incorporate values within the maps somehow (we have the same issue
 * for correctives). In no-corrective mode, the only state that is legal is to read
 * before or at the same time as a write to the same map (simultaneous reads read earlier
 * values, which is ok). We ensure this by checking the min_vid of the writes
 * and filtering all reads <= this min_vid.
 *
 * The big problem with buffered pushes is that they disturb the packet that's formed.
 * In Isobatch mode, this isn't a huge issue for rcv_push_isobatch since that happens at
 * the end of the packet, and the same applies for rcv_put_isobatch: we're not producing
 * any outgoing packet at this point *)
let nd_exec_buffered_fetches_nm = "nd_exec_buffered_fetches"
let nd_exec_buffered_fetches c =
  (* stmt_map_ids are for rhs maps only. group by the statement *)
  let s_ms = list_groupby (fun (_,(s,_)) -> s) (fun (s_m,(_,m)) l -> (s_m, m)::l) [] @@
            P.stmt_map_ids c.p in
  let t_info =
    (* get the relevant map_ids/stmt_map_ids for each statement in the trigger *)
    List.map (fun (x,y,ss) -> x,y, List.map (fun s -> s, List.assoc s s_ms) ss) @@
    List.filter (fun (_,_,ss) -> ss <> []) @@
    P.for_all_trigs ~delete:c.gen_deletes c.p @@
      fun t -> t, D.args_of_t c t, P.stmts_with_rhs_maps_in_t c.p t in
  mk_global_fn nd_exec_buffered_fetches_nm ["vid", t_vid; "stmt_id", t_vid] [] @@
  if t_info = [] then mk_cunit else
  (* get lmap id *)
  mk_let ["map_id"] (mk_at' nd_lmap_of_stmt_id_id @@ mk_var "stmt_id") @@
  mk_block [
    (* delete the entry from the stmt_cntrs_per_map. this has to be done before we
     * get the min_vid *)
    mk_update_at_with nd_stmt_cntrs_per_map_id (mk_var "map_id") @@
      mk_lambda' nd_stmt_cntrs_per_map_e @@
        mk_delete_block "inner" [mk_var "vid"; mk_cunknown];
    mk_let ["min_vid"]
      (mk_at_with' nd_stmt_cntrs_per_map_id (mk_var "map_id") @@
        mk_lambda' nd_stmt_cntrs_per_map_e @@
          mk_min_with (mk_var "inner")
            (mk_lambda'' unit_arg @@ mk_var D.g_max_vid.id) @@
            mk_lambda' D.nd_stmt_cntrs_per_map_inner.e @@ mk_var "vid") @@
    (* check if this is the min vid for the map in stmt_cntrs_per_map. if not, do nothing,
    * since we're not changing anything *)
    mk_if (mk_gt (mk_var "vid") @@ mk_var "min_vid") mk_cunit @@
    (* a crude estimation: if my min_vid (pending write) is an isobatch *)
    mk_let ["min_is_isobatch"] (is_isobatch_id "min_vid") @@
    (* check if there are any pushes to send *)
    mk_let ["pending_fetches"]
      (mk_at_with' nd_fetch_buffer_id (mk_var "map_id") @@
        mk_lambda' nd_fetch_buffer_e @@
       (* if the min_vid(write) is an isobatche, we need to be stricter in our tests,
          since an isobatch can be split between statements *)
        mk_if (mk_var "min_is_isobatch")
          (mk_case_ns
            (mk_peek @@ mk_slice_lt  (mk_var "inner") [mk_var "min_vid"; mk_cunknown])
            "_u" mk_cfalse mk_ctrue) @@
          mk_case_ns
            (mk_peek @@ mk_slice_leq (mk_var "inner") [mk_var "min_vid"; mk_cunknown])
            "_u" mk_cfalse mk_ctrue) @@
      mk_if (mk_var "pending_fetches")
        (mk_block [
          (* filter only those entries we can run *)
          mk_at_with' nd_fetch_buffer_id (mk_var "map_id") @@
            mk_lambda' nd_fetch_buffer_e @@
              (* execute any fetches that precede pending writes for a map *)
              mk_iter (mk_lambda' nd_fetch_buffer_inner.e @@
                mk_block [
                  mk_let ["is_isobatch"] (is_isobatch_id "vid") @@
                  mk_iter (mk_lambda' ["stmt_map_id", t_stmt_map_id] @@
                    (* check for all triggers *)
                    List.fold_left (fun acc_code (t, args, (s_sm_m:((P.stmt_id_t * (int * P.map_id_t) list) list))) ->
                      (* get all stmt_maps in the trigger *)
                      let t_sms = List.flatten @@ List.map (fun (s, sm_m) -> fst_many sm_m) s_sm_m in
                      let mk_check_s s_m = mk_eq (mk_var "stmt_map_id") @@ mk_cint s_m in
                      (* common code *)
                      let push_code isobatch =
                          let push_fn = if isobatch then send_push_isobatch_name_of_t
                                        else send_push_name_of_t in
                          (* pull arguments out of log (if needed) *)
                          (if args = [] then id_fn else
                            mk_let (fst_many args)
                              (mk_apply' (nd_log_get_bound_for t) [mk_var "vid"])) @@
                          List.fold_left (fun acc_code2 (s, sm_m) ->
                              List.fold_left (fun acc_code (sm, m) ->
                                mk_if (mk_eq (mk_var "stmt_map_id") @@ mk_cint sm)
                                  (mk_apply' (push_fn c t s m) @@
                                   (* @buffered=true: force output of a trigger header for single_vid mode
                                    * isobatch mode doesn't need trig header for send_push *)
                                    mk_ctrue::args_of_t_as_vars_with_v c t)
                                  acc_code)
                                acc_code2
                                sm_m)
                              (mk_error "unhandled stmt_map_id")
                              s_sm_m
                      in
                      (* check if the stmts_maps are in this trigger *)
                      mk_if
                        (list_fold_to_last (fun acc s_m -> mk_or (mk_check_s s_m) acc) mk_check_s t_sms)
                        (* handle isobatches *)
                        (mk_if (mk_var "is_isobatch")
                          (mk_block [
                            (* pull the list of batch vids out *)
                            mk_at_with' isobatch_buffered_fetch_vid_map_id (mk_var "stmt_map_id") @@
                              mk_lambda' isobatch_buffered_fetch_vid_map_e @@
                                mk_case_ns (mk_lookup' "inner" [mk_var "vid"; mk_cunknown]) "vids"
                                  (mk_error' @@ mk_concat (mk_cstring "missing buffered batch ") @@
                                                           mk_soi @@ mk_var "vid") @@
                                  mk_iter
                                    (mk_lambda' ["vid", t_vid] @@ push_code true) @@
                                    mk_snd @@ mk_var "vids";
                            (* send all built up metadata for this batch *)
                            mk_apply' nd_send_isobatch_push_meta_nm [mk_var "vid"];
                            (* delete the buffered_fetch batch data, including requested fetch vids in the batch *)
                            mk_update_at_with isobatch_buffered_fetch_vid_map_id (mk_var "stmt_map_id") @@
                              mk_lambda' isobatch_buffered_fetch_vid_map_e @@
                                mk_delete_block "inner" [mk_var "vid"; mk_cunknown];
                          ]) @@
                          (* single vids *)
                          push_code false)
                        (* else, we didn't match here *)
                        acc_code)
                    (mk_error "unhandled stmt_map_id")
                  t_info) @@
                  mk_var "stmt_map_ids"
                ]) @@
                (* if the min_vid(write) is an isobatche, we need to be stricter in our tests,
                    since an isobatch can be split between statements *)
                mk_if (mk_var "min_is_isobatch")
                  (mk_filter_lt  (mk_var "inner") [mk_var "min_vid"; mk_cunknown]) @@
                   mk_filter_leq (mk_var "inner") [mk_var "min_vid"; mk_cunknown]
          ;
          (* delete these entries from the fetch buffer *)
          mk_update_at_with nd_fetch_buffer_id (mk_var "map_id") @@
            mk_lambda' nd_fetch_buffer_e @@
              mk_if (mk_var "min_is_isobatch")
                (mk_filter_geq (mk_var "inner") [mk_var "min_vid"; mk_cunknown]) @@
                mk_filter_gt   (mk_var "inner") [mk_var "min_vid"; mk_cunknown]
        ])
        (* else do nothing *)
        mk_cunit
    ]

