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

(* indexed by ip *)
let send_push_cntrs =
  let e = ["count2", t_int; "has_data2", t_bool] in
  let init = mk_map (mk_lambda' unknown_arg @@ mk_tuple [mk_cint 0; mk_cfalse]) @@
    mk_var D.my_peers.id in
  create_ds ~e ~init "send_push_cntrs" @@ wrap_tvector @@ t_of_e e

(* {ip} *)
let send_push_bitmap = create_ds "send_push_bitmap" t_bitset

let clear_send_push_ds_nm = "clear_send_push_ds"
let clear_send_push_ds =
  mk_global_fn clear_send_push_ds_nm [] [] @@
  mk_block [
    (* clear the data structures *)
    mk_iter_bitmap'
      (mk_update_at_with send_push_cntrs.id (mk_var "ip") @@
        mk_lambda' send_push_cntrs.e @@ mk_tuple [mk_cint 0; mk_cfalse])
      send_push_bitmap.id;
    mk_clear_all send_push_bitmap.id;
  ]

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
let nd_rcv_fetch_single_vid_trig c t s =
  let fn_name = rcv_fetch_single_vid_name_of_t t s in
  let trig_id = P.trigger_id_for_name c.p t in
  let rmaps = P.rhs_maps_of_stmt c.p s in
  let rmap_rng = create_range ~first:1 @@ List.length @@ rmaps in
  (* filter to get only the map tags *)
  let m_tags = List.filter (fun ti -> str_suffix "_id" ti.tag && ti.tag_typ = Ds false) c.poly_tags in
  let s_ms = P.stmt_map_ids c.p in
  let rmap_tags = filter_map (fun ti ->
      let m = P.map_id_of_name c.p @@ str_drop_end (String.length "_id") ti.tag in
      if not @@ List.mem m rmaps then None else
      let s_m = fst @@ List.find (fun (_,(s2,m2)) -> s2 = s && m2 = m) s_ms in
      Some(m, s_m, ti.itag, ti.tag)) m_tags
  in
  mk_global_fn fn_name
    (poly_args @ D.nd_rcv_fetch_args c t) (* stmt_map_ids are an inner ds *)
    [t_int; t_int] @@
    (* skip over the function tag *)
    mk_poly_skip_block fn_name [

      (* TODO: remove duplication. for profiling: mark the rcv fetch here *)
      prof_property prof_tag_rcv_fetch @@ ProfLatency("vid", soi trig_id);

      mk_apply' clear_send_push_ds_nm [];

      (* iterate over the buffered map_id data *)
      mk_let ["idx"; "offset"]
        (mk_poly_iter' @@
          mk_lambda3' p_tag p_idx p_off @@
            (* translate tag to map_id, and skip this tag *)
            mk_let ["map_id"; "stmt_map_id"; "idx"; "offset"]
              (List.fold_left (fun acc_code (m, s_m, itag, stag) ->
                  mk_if_eq (mk_var "tag") (mk_cint itag)
                    (mk_poly_skip_block stag
                       [mk_tuple [mk_cint m; mk_cint s_m; mk_var "idx"; mk_var "offset"]])
                    acc_code)
                  (mk_tuple [mk_cint @@ -1; mk_cint @@ -1; mk_var "idx"; mk_var "offset"])
                  rmap_tags) @@
            mk_if_eq (mk_var "map_id") (mk_cint (-1))
              (mk_tuple [mk_var "idx"; mk_var "offset"]) @@
              mk_block [
                mk_if
                  (mk_or
                    (* check if we're in corrective mode *)
                    (mk_var D.corrective_mode.id) @@
                    (* or if the minimum entry in the per_map_stmt_cntrs has a >= vid
                    * (we can read at the same vid since we read an earlier value *)
                    mk_lt (mk_var "vid") @@
                      mk_at_with' nd_stmt_cntrs_per_map_id (mk_var "map_id") @@
                        mk_lambda' nd_stmt_cntrs_per_map_e @@
                          mk_min_with (mk_var "inner")
                            (* if empty, return max *)
                            (mk_lambda'' unit_arg @@ mk_var g_max_vid.id) @@
                            mk_lambda' nd_stmt_cntrs_per_map_inner.e @@ mk_var "vid")
                  (* then send the push right now *)
                  (mk_block [
                    mk_apply' nd_log_master_write_nm @@ [mk_cint s; mk_var "vid"];
                    (* apply all the per-map pushes *)
                      List.fold_right
                        (fun m acc_code2 ->
                          mk_if_eq (mk_var "map_id") (mk_cint m)
                            (mk_apply' (send_push_name_of_t c t s m) @@
                              (* @buffered: don't force a t header *)
                              mk_cfalse::args_of_t_as_vars_with_v c t)
                          acc_code2)
                        rmaps
                        (mk_error "nd_rcv_fetch: invalid map id")
                    ]) @@
                  (* else, buffer the push *)
                  mk_update_at_with nd_fetch_buffer_id (mk_var "map_id") @@
                    mk_lambda' nd_fetch_buffer_e @@
                      mk_upsert_with_block "inner" [mk_var "vid"; mk_cunknown]
                        (mk_lambda'' unit_arg @@ mk_tuple
                          [mk_var "vid"; mk_singleton nd_fetch_buffer_inner2.t [mk_var "stmt_map_id"]]) @@
                        mk_lambda' nd_fetch_buffer_inner.e @@
                          mk_block [
                            mk_insert "stmt_map_ids" [mk_var "stmt_map_id"];
                            tup_of_e nd_fetch_buffer_inner.e
                          ]
                ;
                mk_tuple [mk_var "idx"; mk_var "offset"]
            ]) @@

      mk_block [
        (* send the stmt cnts we built up in send_pushes *)
        mk_iter_bitmap'
          (mk_at_with' send_push_cntrs.id (mk_var "ip") @@
          mk_lambda' send_push_cntrs.e @@
            mk_block [
              buffer_trig_header_if_needed ~need_args:false (mk_var "vid") t "ip" [mk_var "vid"];
              (* convert counts to messages *)
              List.fold_left (fun acc_code n ->
                  let rcv_push_nm = rcv_push_name_of_t t s in
                  mk_if_eq (mk_var "count2") (mk_cint n)
                    (mk_if (mk_var "has_data2")
                      (buffer_for_send ~wr_bitmap:false (rcv_push_nm^"_"^soi n) "ip" []) @@
                       buffer_for_send ~wr_bitmap:false (rcv_push_nm^"_"^soi n^"_no_data") "ip" [])
                    acc_code)
                  (mk_error "oops") @@
                  rmap_rng
              ]) @@
          send_push_bitmap.id
        ;
        (* return the next idx, offset *)
        mk_tuple [mk_var "idx"; mk_var "offset"]
      ]
    ]

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

