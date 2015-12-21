(* Functions that take K3 code and generate distributed K3 code *)

(*
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
 *
 * - Isobatch mode:
 *  - we run the whole batch in reverse statement order to control dependencies and eliminate intra-batch dependencies
 *  - isobatch_map: stmt_id -> isobatch_id -> bag of stmts
 *   - This is how we maintain the connection between isobatch ids and vids
 *  - isobatch_helper: singleton array to move out of, to update the isobatch_map in the sw->nd trigger handler
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
module K3R = K3Route
module K3S = K3Shuffle
module K3N = K3NewPrint

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
                        mk_tuple [mk_var add_to_count;
                                  mk_var has_data]]]) @@

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
     mk_let ["filtered_vids"]
       (mk_filter_gt (mk_var "vid_set") [mk_var "request_vid"]) @@
     mk_if (mk_eq (mk_size @@ mk_var "filtered_vids") @@ mk_cint 0)
       (* if the set is empty, don't add anything *)
       (mk_var "acc") @@
        mk_insert_block "acc" [mk_var "stmt_id"; mk_var "filtered_vids"]) @@
    mk_empty nd_log_master.t

(**** protocol code ****)


(*** for puts ***)

(* normal put data structure *)
let send_put_ip_map_id = "send_put_ip_map"
let send_put_ip_map_e = ["count", t_int]
let send_put_ip_map p =
  let init = mk_map (mk_lambda' unknown_arg @@ mk_cint 0) @@ mk_var D.my_peers.id in
  let e = send_put_ip_map_e in
  create_ds ~init ~e send_put_ip_map_id @@ wrap_tvector' @@ snd_many e

let send_put_bitmap =
  let init = mk_map (mk_lambda' unknown_arg mk_cfalse) @@ mk_var D.my_peers.id in
  create_ds ~init "send_put_bitmap" @@ wrap_tvector t_bool

let sw_send_puts_nm t s = sp "sw_%s_%d_send_puts" t s
let sw_send_puts c t s =
  let rmap_lmaps = P.rhs_lhs_of_stmt c.p s in
  let lmap = P.lhs_map_of_stmt c.p s in
  let rmaps = P.rhs_maps_of_stmt c.p s in
  let t_args = args_of_t_as_vars_with_v c t in
  mk_global_fn (sw_send_puts_nm t s) (args_of_t_with_v c t) [] @@
    if null rmap_lmaps then mk_cunit else
    (* send puts
    * count is generated by counting the number of messages going to a
    * specific IP *)
    mk_block @@

      (* clean the put globals *)
      [mk_iter_bitmap'
        (mk_insert_at send_put_ip_map_id (mk_var "ip") [mk_cint 0])
        send_put_bitmap.id ;
        (* clean bitmap *)
        mk_set_all send_put_bitmap.id [mk_cfalse]
      ] @
      (* optimized route - check if we can do special 1:1 optimized routing *)
      (if special_route_stmt c s then singleton @@
        (* we need to isolate each of the bound params separately *)
        let bound_params = insert_index_fst @@ bound_params_of_stmt c s in
        mk_let ["buckets"]
          (List.fold_left (fun acc_code (idx, (id, (m, m_idx))) ->
              mk_let ["bucket_"^soi idx]
                (R.get_dim_idx c.p m m_idx @@ mk_var id)
                acc_code)
            (* string the buckets together *)
            (mk_tuple @@ List.map (fun idx -> mk_var @@ "bucket_"^soi idx) @@
              fst_many bound_params)
            bound_params) @@
        (* lookup in the optimized route s *)
        mk_case_ns (mk_lookup' (R.route_opt_ds_nm s) [mk_var "buckets"; mk_cunknown]) "lkup"
          (mk_error "couldn't find buckets in optimized ds") @@
          mk_iter (mk_lambda' R.route_opt_inner.e @@
            mk_block [
              (* mark the put bitmap *)
              mk_insert_at send_put_bitmap.id (mk_var "node") [mk_ctrue];
              (* update the relevant stmt_cnt_list *)
              mk_update_at_with send_put_ip_map_id (mk_var "node") @@
                mk_lambda' send_put_ip_map_e @@
                  (* increment count *)
                  mk_add (mk_var "count") @@ mk_var "sender_count"
            ]) @@
            mk_snd @@ mk_var "lkup"

          (* normal 1:n conservative routing *)
        else
          List.map (fun rmap ->
            (* shuffle allows us to recreate the path the data will take from rhs to lhs *)
            let shuffle_fn = K3Shuffle.find_shuffle_nm c s rmap lmap in
            let shuffle_key, shuffle_empty_pat =
              P.key_pat_from_bound c.p c.route_indices s lmap in
            let shuffle_pat = P.get_shuffle_pat_idx c.p c.route_indices s lmap rmap in
            (* route allows us to know how many nodes send data from rhs to lhs *)
            let route_key, route_pat_idx = P.key_pat_from_bound c.p c.route_indices s rmap in
            (* we need the types for creating empty rhs tuples *)
            let rhs_map_types = P.map_types_with_v_for c.p rmap in
            let tuple_types = wrap_t_calc' rhs_map_types in
            mk_let ["sender_count"]
              (* count up the number of IPs received from route *)
              (R.route_lookup c rmap
                (mk_cint rmap::route_key)
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
                      (* increment count *)
                      mk_add (mk_var "count") @@ mk_var "sender_count"
                ])
                K3S.shuffle_bitmap.id
            ])
            rmaps) @
      (* iterate over the result bitmap and buffer *)
      [
        let rcv_put_nm = rcv_put_name_of_t t s in
        mk_iter_bitmap'
           (mk_at_with' send_put_ip_map_id (mk_var "ip") @@
              mk_lambda' send_put_ip_map_e @@
                mk_block [
                  (* buffer the trig args if needed *)
                  buffer_trig_header_if_needed (mk_var "vid") t "ip" t_args;
                  (* property *)
                  mk_let ["stmt_id"] (mk_cint s) @@
                  prof_property D.prof_tag_send_put
                    @@ ProfSendPut("vid", "stmt_id", "ip", "count");
                  (* send rcv_put header *)
                  buffer_for_send rcv_put_nm "ip" [mk_var "count"];
                ])
         send_put_bitmap.id
      ]

(* vector of maps *)
let send_put_isobatch_inner2 = create_ds "inner2" t_bool_vector

(* vector of source ips *)
let send_put_isobatch_inner =
  let e = ["inner2", send_put_isobatch_inner2.t] in
  create_ds ~e "inner" @@ wrap_tvector @@ t_of_e e

(* (dest ip -> source ip vector) *)
let send_put_isobatch_map_id = "send_put_isobatch_map"
let send_put_isobatch_map_e = ["bitmap", t_bool_vector; "inner", send_put_isobatch_inner.t]
let send_put_isobatch_map c =
  let e = send_put_isobatch_map_e in
  let init =
    mk_map (mk_lambda' unknown_arg @@
      mk_tuple [
        mk_map (mk_lambda' unknown_arg @@ mk_cfalse) @@ mk_var D.my_peers.id;
        mk_map (mk_lambda' unknown_arg @@
            k3_container_of_list t_bool_vector @@
              List.map (const mk_cfalse) @@ 0::P.get_map_list c.p) @@
          mk_var D.my_peers.id
      ]) @@
      mk_var D.my_peers.id in
  create_ds ~init ~e "send_put_isobatch_map" @@ wrap_tvector @@ t_of_e e

let clear_send_put_isobatch_map_nm = "clear_send_put_isobatch_map"
let clear_send_put_isobatch_map =
  mk_global_fn clear_send_put_isobatch_map_nm [] [] @@
  mk_block [
    mk_iter_bitmap'
      (mk_update_at_with send_put_isobatch_map_id (mk_var "ip") @@
        mk_lambda' send_put_isobatch_map_e @@
          mk_let_block ["inner"]
            (mk_agg_bitmap' ~idx:"ip2" ["acc", send_put_isobatch_inner.t]
              (mk_update_at_with_block "acc" (mk_var "ip2") @@
                mk_lambda' send_put_isobatch_inner.e @@
                  mk_set_all_block "inner2" [mk_cfalse])
              (mk_var "inner")
              "bitmap") [
            mk_set_all "bitmap" [mk_cfalse];
            tup_of_e send_put_isobatch_map_e
          ])
      send_put_bitmap.id;
    mk_set_all send_put_bitmap.id [mk_cfalse];
  ]

let sw_send_puts_isobatch_nm t s = sp "sw_%s_%d_send_puts_isobatch" t s
let sw_send_puts_isobatch c t s =
  let rmap_lmaps = P.rhs_lhs_of_stmt c.p s in
  let lmap, rmaps = P.lhs_map_of_stmt c.p s, P.rhs_maps_of_stmt c.p s in
  let t_args = args_of_t_as_vars_with_v c t in
  mk_global_fn (sw_send_puts_isobatch_nm t s) (args_of_t_with_v c t) [] @@
    if null rmap_lmaps then mk_cunit else
    (* for isobatch, we need to track the exact ips sending to other ips *)
    mk_block @@
      (* route allows us to know how many nodes send data from rhs to lhs *)
      List.map (fun rmap ->
        let route_key, route_pat_idx = P.key_pat_from_bound c.p c.route_indices s rmap in
        R.route_lookup c rmap
          (mk_cint rmap::route_key) (mk_cint route_pat_idx) @@
          (* common block *)
          (let update_and_send ip_src ip_dest =
             mk_block [
              mk_insert_at send_put_bitmap.id (mk_var ip_dest) [mk_ctrue];
              mk_update_at_with send_put_isobatch_map_id (mk_var ip_dest) @@
                mk_lambda' send_put_isobatch_map_e @@
                  mk_block [
                    mk_insert_at "bitmap" (mk_var ip_src) [mk_ctrue];
                    mk_update_at_with "inner" (mk_var ip_src) @@
                      mk_lambda' send_put_isobatch_inner.e @@
                        mk_insert_at "inner2" (mk_cint rmap) [mk_ctrue];
                    tup_of_e send_put_isobatch_map_e;
                  ];
              (* buffer the trig args if needed *)
              buffer_trig_header_if_needed (mk_var "vid") t ip_dest t_args;
              (* buffer this stmt *)
              buffer_for_send (rcv_stmt_name_of_t t s) ip_dest [];
             ]
          in

          (* optimized route - check if we can do special 1:1 optimized routing *)
          if special_route_stmt c s then
            (* we need to isolate each of the bound params separately *)
            let idx_rmaps = insert_index_snd ~first:1 @@ P.nonempty_rmaps_of_stmt c.p s in
            let m_idx = List.assoc rmap idx_rmaps in
            let bound_params = insert_index_fst @@ bound_params_of_stmt c s in
            let single_rmap = List.length idx_rmaps = 1 in
            let swallow_f f e = if single_rmap then e else f e in
            mk_let ["buckets"]
              (List.fold_left (fun acc_code (idx, (id, (m, m_idx))) ->
                  mk_let ["bucket_"^soi idx]
                    (R.get_dim_idx c.p m m_idx @@ mk_var id)
                    acc_code)
                (* string the buckets together *)
                (mk_tuple @@ List.map (fun idx -> mk_var @@ "bucket_"^soi idx) @@
                  fst_many bound_params)
                bound_params) @@
            (* iterate over sending nodes *)
            mk_iter_bitmap'
              (* lookup in the optimized route ds *)
              (mk_case_ns (mk_lookup' (R.route_opt_push_ds_nm s)
                         [mk_var "buckets"; mk_cunknown]) "lkup"
                (mk_error "couldn't find buckets in optimized ds") @@
                (* look for our entry in the ds *)
                mk_case_ns (mk_lookup (swallow_f (mk_subscript m_idx) @@
                                      mk_snd @@ mk_var "lkup")
                            [mk_var "ip"; mk_cunknown]) "lkup2"
                  (* do nothing if we're not in the ds *)
                  mk_cunit @@
                  (* otherwise add to result bitmap *)
                  mk_let ["ip_dest"] (mk_fst @@ mk_var "lkup2") @@
                    update_and_send "ip" "ip_dest")
              R.route_bitmap.id
           else
             (* shuffle allows us to recreate the path the data will take from rhs to lhs,
              * but it's only a 1:n relationship *)
              let shuffle_fn = K3Shuffle.find_shuffle_nm c s rmap lmap in
              let shuffle_key, shuffle_empty_pat =
                P.key_pat_from_bound c.p c.route_indices s lmap in
              let shuffle_pat = P.get_shuffle_pat_idx c.p c.route_indices s lmap rmap in
              (* we need the types for creating empty rhs tuples *)
              let rhs_map_types = P.map_types_with_v_for c.p rmap in
              let tuple_types = wrap_t_calc' rhs_map_types in
              mk_iter_bitmap'
                (mk_block [
                  (* fill in shuffle globals *)
                  mk_apply' shuffle_fn @@
                    shuffle_key @ [mk_cint shuffle_pat; mk_cint shuffle_empty_pat; mk_empty tuple_types];
                  (* loop over the shuffle bitmap and insert into the ds *)
                  mk_iter_bitmap' ~idx:"ip2"
                    (update_and_send "ip2" "ip")
                    K3S.shuffle_bitmap.id
                  ])
                  R.route_bitmap.id)
      ) rmaps

(* for fetches *)
let send_fetch_bitmap =
  let init = mk_map (mk_lambda' unknown_arg mk_cfalse) @@ mk_var D.my_peers.id in
  create_ds ~init "send_fetch_bitmap" @@ wrap_tvector t_bool

(* per map *)
let send_fetch_isobatch_inner = create_ds "inner" @@ wrap_tvector t_bool

(* map -> peer -> bool *)
let send_fetch_isobatch_bitmap c =
  let e = [send_fetch_isobatch_inner.id, send_fetch_isobatch_inner.t] in
  let init =
    mk_map (mk_lambda' unknown_arg @@
      mk_map (mk_lambda' unknown_arg mk_cfalse) @@ mk_var D.my_peers.id) @@
        k3_container_of_list (wrap_tvector t_bool) @@ List.map (const mk_cfalse) @@ 0::P.get_map_list c.p
  in
  create_ds ~e ~init "send_fetch_isobatch_bitmap" @@ wrap_tvector @@ t_of_e e

(* for isobatches, we need to create a batch_fetch message and make decisions at the batch level *)
let sw_send_rhs_fetches_nm t s = sp "sw_%s_%d_send_rhs_fetches" t s
let sw_send_rhs_fetches c t s =
  (* to do this in batched form, we iterate over the send_fetch bitmap and immediately buffer
     the results. since we accumulate over many possible statements and don't do any
     inermediate materialization, we can't automatically
     know when to send the send_fetch header, and that comes from the bitmap *)
    let rmaps = P.rhs_maps_of_stmt c.p s in
    let t_args = args_of_t_as_vars_with_v c t
    in
    mk_global_fn (sw_send_rhs_fetches_nm t s) (["is_isobatch", t_bool]@args_of_t_with_v c t) [] @@
    if null rmaps then mk_cunit else
      let m_tags = List.filter (fun (_, ti) -> str_suffix "_id" ti.tag && ti.tag_typ = Ds false) c.poly_tags in
      let rmap_tags = filter_map (fun (_, ti) ->
          let m = P.map_id_of_name c.p @@ str_drop_end (String.length "_id") ti.tag in
          if List.mem m rmaps then Some(m, ti.tag) else None) m_tags in
      mk_block @@
     (* clean send_fetch bitmap *)
      [mk_set_all send_fetch_bitmap.id [mk_cfalse]] @
     (* fill in global data structure from route *)
      (List.map
        (fun (rmap, stag) ->
          let key, pat_idx = P.key_pat_from_bound c.p c.route_indices s rmap in
          R.route_lookup c rmap (mk_cint rmap::key) (mk_cint pat_idx) @@
          mk_iter_bitmap'
            (mk_block [
              prof_property D.prof_tag_fetch_route
                @@ ProfFetchRoute("vid",
                      K3N.string_of_expr (mk_cint s),
                      K3N.string_of_expr (mk_tuple @@ mk_cint rmap::key),
                      K3N.string_of_expr (mk_apply' (K3R.route_for ~bound:true c.p rmap) (mk_cint rmap::key)),
                      "ip");
              (* if we haven't sent to this ip yet, add a rcv_fetch header *)
                mk_if (mk_not @@ mk_at' send_fetch_bitmap.id @@ mk_var "ip")
                  (mk_block [
                    (* mark the send_fetch_bitmap *)
                    mk_insert_at send_fetch_bitmap.id (mk_var "ip") [mk_ctrue];
                    (* buffer the trig args if needed *)
                    buffer_trig_header_if_needed (mk_var "vid") t "ip" t_args;
                    mk_if (mk_var "is_isobatch")
                      (buffer_for_send (rcv_fetch_isobatch_name_of_t t s) "ip" []) @@
                      buffer_for_send (rcv_fetch_name_of_t t s) "ip" []
                  ])
                  mk_cunit;
                (* buffer the map id *)
                buffer_for_send ~wr_bitmap:false stag "ip" []
            ])
            R.route_bitmap.id)
        rmap_tags)

let sw_send_rhs_completes_nm t s = sp "sw_%s_%d_send_rhs_completes" t s
let sw_send_rhs_completes c t s =
  let t_args = args_of_t_as_vars_with_v c t in
  let rmaps = P.rhs_maps_of_stmt c.p s in
  (* must be last in the trigger handler *)
  mk_global_fn (sw_send_rhs_completes_nm t s) (args_of_t_with_v c t) [] @@
    if rmaps <> [] then mk_cunit
    else
      let lmap = P.lhs_map_of_stmt c.p s in
      let do_complete_t = do_complete_name_of_t t s^"_trig" in
      let key, pat_idx = P.key_pat_from_bound c.p c.route_indices s lmap in
      mk_block [
        R.route_lookup c lmap
          (mk_cint lmap::key)
          (mk_cint pat_idx) @@
          mk_ignore @@ mk_iter_bitmap'
              (mk_block [
                (* buffer the trig args if needed. do_complete_ts don't save the args,
                    so if we add the header here, we must make sure we're the only msg to
                    this node by being last in the send fetch
                *)
                buffer_trig_header_if_needed (mk_var "vid") t "ip" t_args;
                buffer_for_send do_complete_t "ip" []
              ])
            K3Route.route_bitmap.id
      ]


(* sending fetches is done from functions *)
(* each function takes a vid to plant in the arguments, which are in the trig buffers *)
let sw_send_fetch_fn c t s =
  let args = args_of_t_with_v c t in
  let arg_vars = ids_to_vars @@ fst_many args in
  mk_global_fn
    (send_fetch_name_of_t t s) (("batch_id", t_vid)::args_of_t_with_v c t) [] @@
    mk_block [
        mk_apply' (sw_send_rhs_fetches_nm t s) @@ mk_cfalse::arg_vars;
        mk_apply' (sw_send_puts_nm t s) arg_vars;
        mk_apply' (sw_send_rhs_completes_nm t s) arg_vars;
    ]

(* loop over the statements of a trigger for all vids, working backwards from last
 * stmt to first *)
let sw_send_fetches_isobatch c t =
  let ss = List.rev @@ P.stmts_of_t c.p t in (* reverse to do last stmt first *)
  let drop_insert t = str_drop (String.length "insert_") t in
  let t_args = ("do_insert", t_bool)::args_of_t c t in
  let call_args = ids_to_vars @@ fst_many @@ args_of_t_with_v c t in
  mk_global_fn (send_fetches_isobatch_name_of_t t)
    ["batch_id", t_vid; "poly_queue", poly_queue.t]
    [t_vid]
  @@
  mk_block @@ List.flatten @@ List.map (fun s ->
    (* clear the full ds *)
    [
      mk_apply' clear_send_put_isobatch_map_nm [];

      (* execute all statements *)
      mk_poly_fold
        (mk_lambda4' ["vid", t_vid] p_tag p_idx p_off @@
          mk_block [
            mk_poly_at_with' (drop_insert t) @@
              mk_lambda' t_args @@
                mk_block [
                  (* true: is_isobatch *)
                  mk_apply' (sw_send_rhs_fetches_nm t s) @@ mk_ctrue::call_args;
                  mk_apply' (sw_send_rhs_completes_nm t s) call_args;
                  mk_apply' (sw_send_puts_isobatch_nm t s) call_args;
                ];
            next_vid (mk_var "vid")
          ])
        (mk_var "batch_id") @@
        mk_var "poly_queue"
    ] @
    (* calculate counts, send puts for the batch based on the ds *)
    (if P.rhs_maps_of_stmt c.p s = [] then [] else singleton @@
      mk_iter_bitmap'
          (mk_let ["count"]
            (mk_at_with' send_put_isobatch_map_id (mk_var "ip") @@
              mk_lambda' send_put_isobatch_map_e @@
                mk_agg_bitmap' ~idx:"ip2" ["count", t_int]
                  (mk_at_with' "inner" (mk_var "ip2") @@
                    mk_lambda' send_put_isobatch_inner.e @@
                      mk_agg_bitmap' ~idx:map_ctr.id ["count2", t_int]
                        (mk_add (mk_var "count2") @@ mk_cint 1)
                        (mk_var "count")
                        "inner2")
                  (mk_cint 0)
                  "bitmap") @@
          buffer_for_send (rcv_put_name_of_t t s) "ip" [mk_var "count"])
        send_put_bitmap.id)
    ) ss


(* indexed by ip *)
let send_push_cntrs =
  let e = ["count2", t_int; "has_data2", t_bool] in
  let init = mk_map (mk_lambda' unknown_arg @@ mk_tuple [mk_cint 0; mk_cfalse]) @@
    mk_var D.my_peers.id in
  create_ds ~e ~init "send_push_cntrs" @@ wrap_tvector @@ t_of_e e

let send_push_bitmap =
  let init = mk_map (mk_lambda' unknown_arg mk_cfalse) @@ mk_var D.my_peers.id in
  create_ds ~init "send_push_bitmap" @@ wrap_tvector t_bool

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
let nd_rcv_fetch_trig c t s =
  let fn_name = rcv_fetch_name_of_t t s in
  let trig_id = P.trigger_id_for_name c.p t in
  let rmaps = P.rhs_maps_of_stmt c.p s in
  let rmap_rng = create_range ~first:1 @@ List.length @@ rmaps in
  let m_tags = List.filter (fun (_,ti) -> str_suffix "_id" ti.tag && ti.tag_typ = Ds false) c.poly_tags in
  let s_ms = P.stmt_map_ids c.p in
  let rmap_tags = filter_map (fun (itag, ti) ->
      let m = P.map_id_of_name c.p @@ str_drop_end (String.length "_id") ti.tag in
      let s_m = fst @@ List.find (fun (_,(s2,m2)) -> s2 = s && m2 = m) s_ms in
      if List.mem m rmaps then Some (m, s_m, itag, ti.tag) else None) m_tags
  in
  mk_global_fn fn_name
    (poly_args @ D.nd_rcv_fetch_args c t) (* stmt_map_ids are an inner ds *)
    [t_int; t_int] @@
    (* skip over the function tag *)
    mk_poly_skip_block fn_name [

      (* TODO: remove duplication. for profiling: mark the rcv fetch here *)
      prof_property prof_tag_rcv_fetch @@ ProfLatency("vid", soi trig_id);

      (* clear the data structures *)
      mk_iter_bitmap'
        (mk_update_at_with send_push_cntrs.id (mk_var "ip") @@
         mk_lambda' send_push_cntrs.e @@ mk_tuple [mk_cint 0; mk_cfalse])
        send_push_bitmap.id;
      mk_set_all send_push_bitmap.id [mk_cfalse];

      (* iterate over the buffered map_id data *)
      mk_let ["idx"; "offset"]
        (mk_poly_iter' @@
          mk_lambda3' p_tag p_idx p_off @@
            (* translate tag to map_id, and skip this tag *)
            mk_let ["map_id"; "stmt_map_id"; "idx"; "offset"]
              (List.fold_left (fun acc_code (m, s_m, itag, stag) ->
                  mk_if_eq (mk_var "tag") (mk_cint itag)
                    (mk_poly_skip_block stag
                        [mk_tuple [mk_cint m; mk_var "idx"; mk_var "offset"]])
                    acc_code)
                  (mk_tuple [mk_cint (-1); mk_var "idx"; mk_var "offset"])
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
                      mk_case_ns
                        (mk_peek @@ mk_slice' D.nd_stmt_cntrs_per_map_id
                          [mk_var "map_id"; mk_cunknown]) "x"
                        (mk_var D.g_max_vid.id) @@
                        mk_min_with (mk_snd @@ mk_var "x")
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

(* whether we have a decision already on sending a push *)
let rcv_fetch_isobatch_bitmap_id = "rcv_fetch_isobatch_bitmap"
let rcv_fetch_isobatch_bitmap c =
  let init = k3_container_of_list (wrap_tvector t_bool) @@
    List.map (const mk_cfalse) @@ 0::P.get_map_list c.p in
  create_ds ~init rcv_fetch_isobatch_bitmap_id @@ wrap_tvector t_bool

(* what our decision is regarding a map for the whole isobatch *)
(* true = send the pushes now *)
let rcv_fetch_decision_isobatch_bitmap_id = "rcv_fetch_decision_isobatch_bitmap2"
let rcv_fetch_decision_isobatch_bitmap c =
  let init = k3_container_of_list (wrap_tvector t_bool) @@
    List.map (const mk_cfalse) @@ 0::P.get_map_list c.p in
  create_ds ~init rcv_fetch_decision_isobatch_bitmap_id @@ wrap_tvector t_bool

(* indexed by map_id *)
let send_push_isobatch_inner = create_ds "inner" t_bool_vector

(* ip -> stmt_map_id -> bool *)
let send_push_isobatch_map_id = "send_push_isobatch_map"
let send_push_isobatch_map_e = ["inner", send_push_isobatch_inner.t]
let send_push_isobatch_map c =
  let e = send_push_isobatch_map_e in
  let init =
    mk_map (mk_lambda' unknown_arg @@ k3_container_of_list t_bool_vector @@
          List.map (const mk_cfalse) @@ 0::(fst_many @@ P.stmt_map_ids c.p)) @@
    mk_var D.my_peers.id in
  create_ds ~e ~init send_push_isobatch_map_id @@ wrap_tvector @@ t_of_e e

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
(* stmt_map_id -> bool *)
let rcv_fetch_isobatch_bitmap_id = "rcv_fetch_isobatch_bitmap"
let rcv_fetch_isobatch_bitmap c =
  let init = k3_container_of_list t_bool_vector @@
    List.map (const mk_cfalse) @@ 0::(fst_many @@ P.stmt_map_ids c.p) in
  create_ds ~init rcv_fetch_isobatch_bitmap_id t_bool_vector

(* the actual decision on buffer=true/push=false *)
let rcv_fetch_isobatch_decision_bitmap_id = "rcv_fetch_isobatch_decision"
let rcv_fetch_isobatch_decision_bitmap c =
  let init = k3_container_of_list t_bool_vector @@
    List.map (const mk_cfalse) @@ 0::(fst_many @@ P.stmt_map_ids c.p) in
  create_ds ~init rcv_fetch_isobatch_decision_bitmap_id t_bool_vector

let clear_buffered_fetch_helper_nm = "clear_buffered_fetch_helper"
let clear_buffered_fetch_helper =
  mk_global_fn clear_buffered_fetch_helper_nm [] [] @@
  mk_block [
    mk_iter_bitmap' ~idx:stmt_ctr.id
      (mk_insert_at isobatch_buffered_fetch_helper_id (mk_var stmt_ctr.id)
          [mk_empty isobatch_map_inner2.t]) rcv_fetch_isobatch_decision_bitmap_id;
    mk_set_all rcv_fetch_isobatch_bitmap_id [mk_cfalse];
    mk_set_all rcv_fetch_isobatch_decision_bitmap_id [mk_cfalse];
  ]

let nd_rcv_fetch_trig_isobatch c t s =
  let fn_name = rcv_fetch_isobatch_name_of_t t s in
  let trig_id = P.trigger_id_for_name c.p t in
  let rmaps = P.rhs_maps_of_stmt c.p s in
  let m_tags = List.filter (fun (_,ti) -> str_suffix "_id" ti.tag && ti.tag_typ = Ds false) c.poly_tags in
  let s_ms = P.stmt_map_ids c.p in
  let rmap_tags = filter_map (fun (itag, ti) ->
      let m = P.map_id_of_name c.p @@ str_drop_end (String.length "_id") ti.tag in
      let s_m = fst @@ List.find (fun (_,(s2,m2)) -> s2 = s && m2 = m) s_ms in
      if List.mem m rmaps then Some (m, s_m, itag, ti.tag) else None) m_tags
  in
  let var_args = args_of_t_as_vars_with_v c t in
  mk_global_fn fn_name
    (poly_args @ D.nd_rcv_fetch_args c t) (* stmt_map_ids are an inner ds *)
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
          mk_if_eq (mk_var "map_id") (mk_cint (-1))
            (mk_tuple [mk_var "idx"; mk_var "offset"]) @@
            mk_block [
              mk_if
                (mk_var D.corrective_mode.id)
                (* then send the push right now *)
                (mk_apply' (nd_rcv_fetch_isobatch_do_push_nm t s) @@ [mk_var "map_id"]@var_args) @@
                (* check if we have a decision already made for this map *)
                mk_let ["made_decision"]
                  (mk_at' rcv_fetch_isobatch_bitmap_id @@ mk_var "stmt_map_id") @@
                mk_if (mk_var "made_decision")
                  (mk_let ["do_buffer"]
                    (mk_at' rcv_fetch_decision_isobatch_bitmap_id @@ mk_var "stmt_map_id") @@
                  mk_if (mk_var "do_buffer")
                    (* buffer the push *)
                    (mk_apply' (nd_rcv_fetch_isobatch_buffer_push_nm) [mk_var "vid"]) @@
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
                    mk_insert_at rcv_fetch_isobatch_bitmap_id (mk_var "stmt_map_id") [mk_ctrue];
                    mk_insert_at rcv_fetch_decision_isobatch_bitmap_id (mk_var "stmt_map_id") [mk_var "do_buffer"];
                    (* act on the decision and save into the helper *)
                    mk_if (mk_var "do_buffer")
                      (* write a buffer decision *)
                      (mk_block [
                        mk_update_at_with nd_fetch_buffer_id (mk_var "map_id") @@
                          mk_lambda' nd_fetch_buffer_e @@
                            mk_upsert_with_block "inner" [mk_var "vid"; mk_cunknown]
                              (mk_lambda'' unit_arg @@ mk_tuple
                                [mk_var "vid"; mk_singleton D.nd_fetch_buffer_inner2.t [mk_var "stmt_map_id"]]) @@
                              mk_lambda' nd_fetch_buffer_inner.e @@
                                mk_block [
                                  mk_insert "stmt_map_ids" [mk_var "stmt_map_id"];
                                  tup_of_e nd_fetch_buffer_inner.e
                                ];
                        (* buffer into temporary helper ds *)
                        mk_apply' nd_rcv_fetch_isobatch_buffer_push_nm []
                      ])
                      (mk_apply' (nd_rcv_fetch_isobatch_do_push_nm t s) @@ [mk_var "map_id"]@var_args)
                    ;
                    mk_tuple [mk_var "idx"; mk_var "offset"]
                  ]
            ]
    ]

(* Receive Put trigger
 * --------------------------------------- *
 * Update the statement counters with the received values
 * If the counter is 0, we need to call do_complete
 * - One of the triggers fed from the poly queue
 *)
let nd_rcv_put_trig c t s =
  let fn_name = rcv_put_name_of_t t s in
  mk_global_fn fn_name
    (D.nd_rcv_put_args c t) (* also pull inner ds *)
    [] @@ mk_block
    [
      mk_if
        (mk_apply' nd_check_stmt_cntr_index_nm @@
          (* false: no data is being sent *)
          [mk_var "vid"; mk_cint s; mk_var "count2"; mk_cfalse])
        (* check if this is an isobatch vid *)
        (mk_if (is_isobatch_id "vid")
           (mk_at_with' isobatch_vid_map_id (mk_cint s) @@
              mk_lambda' isobatch_vid_map_e @@
                mk_case_ns (mk_lookup (mk_var "inner") [mk_var "vid"; mk_cunknown])
                  "vids"
                  (mk_error "missing vid in isobatch_vid_map") @@
                  (* iterate over all the vids in this batch and complete them *)
                  mk_iter
                    (mk_lambda' ["vid", t_vid] @@
                    mk_apply' (do_complete_name_of_t t s) @@ args_of_t_as_vars_with_v c t) @@
                    mk_snd @@ mk_var "vids") @@
           (* else, just apply the one vid *)
           mk_apply'
            (do_complete_name_of_t t s) @@ args_of_t_as_vars_with_v c t) @@
        mk_cunit
    ]

(* receive isobatch stmt list *)
(* we buffer the vid/stmts in the helper ds *)
let nd_rcv_stmt =
  mk_global_fn nd_rcv_stmt_nm ["stmt_id", t_stmt_id; "vid", t_vid] [] @@
  mk_block [
      mk_insert_at isobatch_stmt_helper_bitmap_id (mk_var "stmt_id") [mk_ctrue];
      mk_update_at_with isobatch_stmt_helper_id (mk_var "stmt_id") @@
        mk_lambda' isobatch_stmt_helper_e @@
          mk_insert_block "inner2" [mk_var "vid"]
  ]

let nd_send_batch_push_bitmap =
  let init = mk_map (mk_lambda' unknown_arg mk_cfalse) @@ mk_var D.my_peers.id in
  create_ds ~init "nd_send_batch_push_bitmap" @@ wrap_tvector t_bool

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
      let empty_pat_idx = if special_route_stmt c s then -1 else empty_pat_idx in
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
          (if special_route_stmt c s &&
            (* to handle empty key maps on the rhs *)
            List.mem_assoc rmap idx_rmaps then singleton @@
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
                  (mk_insert_at K3S.shuffle_bitmap.id (mk_var "ip") [mk_ctrue]) @@
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
                      mk_insert_at send_push_bitmap.id (mk_var "ip") [mk_ctrue];
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
      let empty_pat_idx = if special_route_stmt c s then -1 else empty_pat_idx in
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
          (if special_route_stmt c s &&
            (* to handle empty key maps on the rhs *)
            List.mem_assoc rmap idx_rmaps then singleton @@
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
                  (mk_insert_at K3S.shuffle_bitmap.id (mk_var "ip") [mk_ctrue]) @@
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
                  mk_insert_at send_push_bitmap.id (mk_var "ip") [mk_ctrue];
                  (* insert the relevant stmt_map *)
                  mk_update_at_with send_push_isobatch_map_id (mk_var "ip") @@
                    mk_lambda' send_push_isobatch_map_e @@
                      mk_insert_at_block "inner" (mk_cint s_m) [mk_ctrue];

                  (* buffer the map data according to the indices *)
                  buffer_tuples_from_idxs ~unique:true "tuples" map_delta.t buf_nm @@ mk_var "indices"
               ])
            K3S.shuffle_bitmap.id
        ]) @@ (* trigger *)
    P.rhs_lhs_of_stmt c.p s

(* generic function to send the metadata of the batch pushes *)
let nd_send_isobatch_push_meta_nm = "nd_send_isobatch_push_meta"
let nd_send_isobatch_push_meta c =
  let s_m = P.stmt_map_ids c.p in
  mk_global_fn nd_send_isobatch_push_meta_nm ["batch_id", t_vid] [] @@
    mk_iter_bitmap'
      (mk_at_with' send_push_isobatch_map_id (mk_var "ip") @@
          mk_lambda' send_push_isobatch_map_e @@ mk_block @@
          List.map (fun s ->
              let t = P.trigger_of_stmt c.p s in
              let num_rmaps = create_range ~first:1 @@ List.length @@ P.rhs_maps_of_stmt c.p s in
              mk_let ["count"]
                (List.fold_left (fun acc_code (s_m, (_, m)) ->
                    mk_add
                      (mk_if (mk_at' "inner" @@ mk_cint s_m) (mk_cint 1) @@ mk_cint 0)
                      acc_code)
                  (mk_cint 0) @@
                  List.filter (fun (_,(s',_)) -> s' = s) s_m) @@
              mk_if_eq (mk_var "count") (mk_cint 0)
                mk_cunit @@
                List.fold_left (fun acc_code n ->
                  mk_if_eq (mk_var "count") (mk_cint n)
                    (buffer_for_send (rcv_push_isobatch_name_of_t t s^"_"^soi n) "ip"
                       [mk_var "batch_id"]) @@
                    acc_code) (mk_error "oops") num_rmaps) @@
            P.get_stmt_list c.p)
      send_push_bitmap.id

(* receive isobatch push: load the vids for the batch id and run them if needed *)
let nd_rcv_isobatch_push_trig c t s =
  let fn_name = rcv_push_isobatch_name_of_t t s in
  let args = D.args_of_t c t in
  mk_global_fn fn_name ["batch_id", t_vid; "count", t_int] [] @@
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
                    mk_cfalse::args_of_t_as_vars_with_v c t) @@
                mk_snd @@ mk_var "vids";
              (* do stmt_cntr_check for the batch *)
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
          mk_apply' (do_complete_name_of_t t s) @@ args_of_t_as_vars_with_v c t)
        mk_cunit
    ]

(* list of potential corrective maps.
 * the potential corrective maps and the potential read/write conflicts.
 * Inserts and Deletes really have 2 different graphs, so we return both *)
let maps_potential_corrective c =
  let ts = P.get_trig_list ~sys_init:false ~delete:c.gen_deletes ~corrective:false c.p in
  let get_maps f = ListAsSet.uniq |- List.flatten |- List.map f |- List.flatten
    |- List.map (P.stmts_of_t c.p) in
  let lhs_maps = get_maps (singleton |- P.lhs_map_of_stmt c.p) in
  let rhs_maps = get_maps (P.rhs_maps_of_stmt c.p) in
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

(* used by send correctives *)
let send_corrective_bitmap =
  (* indexed by ip *)
  let init = mk_map (mk_lambda' unknown_arg mk_cfalse) @@ mk_var D.my_peers.id in
  create_ds ~init "send_corrective_bitmap" @@ wrap_tvector t_bool

let send_corrective_ip_map =
  (* indexed by ip *)
  let intset_t = wrap_tset t_int in
  let e = ["vids", t_vid_list; "t_indices", intset_t] in
  let t = wrap_tvector' @@ snd_many e in
  let init = mk_map (mk_lambda' unknown_arg @@
                     mk_tuple [mk_empty t_vid_list; mk_empty intset_t]) @@
              mk_var D.my_peers.id in
  create_ds "send_corrective_ip_map" ~init ~e t

let send_corrective_fns c =
  (* for a given lhs map which we just changed, find all statements containing the
   * same map on the rhs. This is crude, but should be enough for a first try *)
  let send_correctives rmap =
    (* list of (trig,stmt) that have this rmap on the rhs *)
    let trigs_stmts_with_matching_rhs_map =
        List.filter
          (fun (t, s) -> P.stmt_has_rhs_map c.p s rmap) @@
          P.get_trig_stmt_list c.p ~delete:c.gen_deletes
    in
    (* turn the ocaml list into a literal k3 list *)
    let trig_stmt_k3_list_nm = "nd_corr_"^P.map_name_of c.p rmap^"_list" in
    let trig_stmt_k3_list =
      let t = wrap_tbag' [t_trig_id; t_stmt_id] in
      mk_global_val_init trig_stmt_k3_list_nm t @@
        k3_container_of_list t @@ List.map
          (fun (trig, stmt_id) ->
            mk_tuple [mk_cint (P.trigger_id_for_name c.p trig); mk_cint stmt_id])
          trigs_stmts_with_matching_rhs_map
    in
    match trigs_stmts_with_matching_rhs_map with [] -> [] | _ ->
    let map_ds = D.map_ds_of_id ~global:false c rmap ~vid:false in
    let delta_tuples2 =
      D.map_ds_of_id ~global:false ~vid:true c rmap ~name:"delta_tuples2" in
    (* rename vid to corrective_vid for differentiation *)
    let args' = (list_drop_end 1 D.nd_rcv_corr_args)@["corrective_vid", t_vid] in
    let args = args' @ ["delta_tuples", map_ds.t] in
    let sub_args = args' @ ["delta_tuples2", delta_tuples2.t; "vid_list", t_vid_list] in
    let fn_nm = send_corrective_name_of_t c rmap in
    let sub_fn_nm stmt = fn_nm^"_"^soi stmt in

    (* create sub functions for each stmt_id possible, to aid compilation *)
    let sub_fns =
      List.map (fun (t, s) ->
         (* we already have the map vars for the rhs map in the
          * tuples. Now we try to get some more for the lhs map *)
        let target_map = P.lhs_map_of_stmt c.p s in
        let key, _ = P.key_pat_from_bound c.p c.route_indices s target_map in
        let pat_idx = P.get_shuffle_pat_idx c.p c.route_indices s target_map rmap in
        let shuffle_fn = K3S.find_shuffle_nm c s rmap target_map in
        (* TODO: fix for query 4
          let info = IntMap.find s c.freevar_info in
        (* no bound variables are used for the lhs or rhs *)
        let no_bound =
          snd info.P.lmap_bound = [] && List.assoc rmap info.P.rmaps_bound = []
        in*)

        mk_global_fn (sub_fn_nm s) sub_args
        [t_int] @@ (* return num of sends *)
        mk_block [
          (* clean the bitmap/collection *)
          mk_iter_bitmap'
             (mk_block [
               mk_update_at_with send_corrective_ip_map.id
                 (mk_var "ip") @@
                 mk_lambda' send_corrective_ip_map.e @@
                  mk_tuple @@ List.map mk_empty @@ snd_many send_corrective_ip_map.e;
              ])
             send_corrective_bitmap.id;
          mk_set_all send_corrective_bitmap.id [mk_cfalse];

          (* TODO: if lvars have no impact, only shuffle once *)
          mk_iter
            (mk_lambda' ["vid", t_vid] @@
              (* get bound vars from log so we can calculate shuffle *)
              let args = D.args_of_t c t in
              (if args <> [] (* && not no_bound *) then
                mk_let
                  (fst_many @@ D.args_of_t c t)
                  (mk_apply'
                    (nd_log_get_bound_for t) [mk_var "vid"])
              else id_fn) @@
              mk_block [
                mk_apply' shuffle_fn @@
                  key @ [mk_cint pat_idx; mk_cint (-1); mk_var "delta_tuples2"];
                mk_iter_bitmap'
                  (mk_block [
                    mk_insert_at send_corrective_bitmap.id (mk_var "ip") [mk_ctrue];
                    (* lookup indices *)
                    mk_at_with' K3S.shuffle_results.id (mk_var "ip") @@
                      mk_lambda' K3S.shuffle_results.e @@
                        mk_update_at_with send_corrective_ip_map.id
                          (mk_var "ip") @@
                          mk_lambda' send_corrective_ip_map.e @@
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
                  K3S.shuffle_bitmap.id
              ]) @@
              mk_var "vid_list"
          ;
            (* send to each ip, and count up the msgs *)
            let rcv_trig = rcv_corrective_name_of_t c t s rmap in
            mk_agg_bitmap'
              ["count", t_int]
              (* lookup ip in vector *)
              (mk_block [
                  mk_at_with' send_corrective_ip_map.id (mk_var "ip") @@
                    mk_lambda' send_corrective_ip_map.e @@
                      mk_block [
                        (* buffer header *)
                        buffer_for_send rcv_trig "ip" @@
                          (ids_to_vars @@ fst_many orig_vals) @ [mk_var "corrective_vid"];
                        (* buffer tuples from indices *)
                        buffer_tuples_from_idxs ~drop_vid:true delta_tuples2.id delta_tuples2.t
                          (map_ds.id^"_map") (mk_var "t_indices");
                        (* buffer vids *)
                        mk_iter (mk_lambda'' ["vid", t_vid] @@
                          buffer_for_send ~wr_bitmap:false "vids" "ip" [mk_var "vid"]) @@
                          mk_var "vids";
                      ];
                  mk_add (mk_var "count") @@ mk_cint 1
                ])
              (mk_cint 0) @@
              send_corrective_bitmap.id
        ])
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
      mk_let ["count"]
        (mk_agg
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
            mk_var "corrective_list") @@
       mk_block [
         prof_property prof_tag_corr_send @@ ProfLatency("corrective_vid", "count");
         mk_var "count"
       ])
    in
    trig_stmt_k3_list :: sub_fns @ [fn]
  in
  List.flatten @@ List.map send_correctives c.corr_maps

let nd_update_corr_delete = create_ds "nd_update_corr_delete" @@ mut t_bool

(* function to update the stmt_counters for correctives *)
(* current hop = hop - 1. next hop = hop *)
let nd_update_corr_map_nm = "nd_update_corr_map"
let nd_update_corr_map =
  mk_global_fn nd_update_corr_map_nm
    ["vid", t_vid; "stmt_id", t_stmt_id;
     "hop", t_int; "count", t_int] [t_bool] @@
    mk_block [
      (* clear delete flag *)
      mk_assign nd_update_corr_delete.id mk_cfalse;
      (* check for case where we do nothing *)
      mk_if
        (mk_and (mk_eq (mk_var "hop") @@ mk_cint 1) @@
                 mk_eq (mk_var "count") @@ mk_cint 0)
        mk_cunit @@
      (* we need to decrement the previous hop's value by 1, and
       * increment the hop's values by the count (if it's not 0) *)
      mk_upsert_with nd_corr_map.id
        [mk_tuple [mk_var "vid"; mk_var "stmt_id"]; mk_cunknown]
        (* handle case of no previous value *)
        (mk_lambda' unit_arg @@
          mk_let_block ["m"] (mk_empty nd_corr_map_inner.t)
          [
            (* for root, don't decrement previous hop (0) *)
            mk_if_eq (mk_var "hop") (mk_cint 1) mk_cunit @@
              mk_insert "m" [mk_sub (mk_var "hop") @@ mk_cint 1; mk_cint (-1)] ;
            mk_if_eq (mk_var "count") (mk_cint 0) mk_cunit @@
              mk_insert "m" [mk_var "hop"; mk_var "count"] ;
            mk_tuple [mk_tuple [mk_var "vid"; mk_var "stmt_id"]; mk_var "m"]
          ]) @@
        (* case of existing value *)
        mk_lambda' nd_corr_map.e @@
          mk_block [
            (* only add to hop if count isn't 0 *)
            mk_if_eq (mk_var "count") (mk_cint 0) mk_cunit @@
              mk_upsert_with "tree" [mk_var "hop"; mk_cunknown]
                (mk_lambda' unit_arg @@ mk_tuple [mk_var "hop"; mk_var "count"]) @@
                (mk_lambda' nd_corr_map_inner.e @@
                mk_block [
                  mk_if_eq (mk_var "count2") (mk_neg @@ mk_var "count")
                    (mk_assign nd_update_corr_delete.id mk_ctrue)
                    mk_cunit;
                  mk_tuple [mk_var "hop"; mk_add (mk_var "count") @@ mk_var "count2"]
                ])
              ;
            (* delete entry if needed *)
            mk_if (mk_var nd_update_corr_delete.id)
              (mk_block [
                  mk_delete "tree" [mk_var "hop"; mk_cunknown];
                  mk_assign nd_update_corr_delete.id mk_cfalse
                ])
              mk_cunit
            ;

            (* only subtract for last hop if not root *)
            mk_if_eq (mk_var "hop") (mk_cint 1) mk_cunit @@
              mk_let_block ["hop_sub"] (mk_sub (mk_var "hop") @@ mk_cint 1)
              [
                (* update the tree *)
                mk_upsert_with "tree" [mk_var "hop_sub"; mk_cunknown]
                  (mk_lambda' unit_arg @@ mk_tuple [mk_var "hop_sub"; mk_cint (-1)]) @@
                  mk_lambda' nd_corr_map_inner.e @@
                    mk_block [
                      mk_if_eq (mk_var "count2") (mk_cint 1)
                        (mk_assign nd_update_corr_delete.id mk_ctrue)
                        mk_cunit;
                      mk_tuple [mk_var "hop2"; mk_sub (mk_var "count2") @@ mk_cint 1]
                    ];
                (* delete entry if needed *)
                mk_if (mk_var nd_update_corr_delete.id)
                  (mk_block [
                      mk_delete "tree" [mk_var "hop_sub"; mk_cunknown];
                      mk_assign nd_update_corr_delete.id @@ mk_cfalse
                  ])
                  mk_cunit
              ]
            ;
            (* check if we need to delete the whole vid,stmt entry *)
            mk_if_eq (mk_size @@ mk_var "tree") (mk_cint 0)
              (mk_assign nd_update_corr_delete.id @@ mk_ctrue)
              mk_cunit
            ;
            mk_tuple [mk_tuple [mk_var "vid"; mk_var "stmt_id"]; mk_var "tree"]
        ];
      (* delete entry if needed *)
      mk_if (mk_var nd_update_corr_delete.id)
        (mk_delete nd_corr_map.id [mk_tuple [mk_var "vid"; mk_var "stmt_id"]; mk_cunknown])
        mk_cunit;

      (* return whether we should delete *)
      mk_var nd_update_corr_delete.id
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
  let s_m = P.stmt_map_ids c.p in
  let t_info =
    let find_s_m s = fst @@ List.find (fun (_,(s',_)) -> s = s') s_m in
    List.map (fun (x,y,ss) -> x,y,ss,List.map find_s_m ss) @@
    List.filter (fun (_,_,x) -> x <> []) @@
    P.for_all_trigs ~delete:c.gen_deletes c.p
    (fun t -> t, D.args_of_t c t, P.stmts_with_rhs_maps_in_t c.p t) in
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
    (* check if there are any pushes to send *)
    mk_let ["pending_fetches"]
      (mk_at_with' nd_fetch_buffer_id (mk_var "map_id") @@
        mk_lambda' nd_fetch_buffer_e @@
        mk_case_ns
          (mk_peek @@ mk_slice_leq (mk_var "inner") [mk_var "min_vid"; mk_cunknown])
          "_u" mk_cfalse mk_ctrue) @@
      (* check if this is the min vid for the map in stmt_cntrs_per_map. if not, do nothing,
      * since we're not changing anything *)
      mk_if (mk_var "pending_fetches")
        (mk_block [
          (* filter only those entries we can run *)
          mk_at_with' nd_fetch_buffer_id (mk_var "map_id") @@
            mk_lambda' nd_fetch_buffer_e @@
              (* execute any fetches that precede pending writes for a map *)
              mk_iter (mk_lambda' nd_fetch_buffer_inner.e @@
                mk_let ["is_isobatch"] (is_isobatch_id "vid") @@
                mk_iter (mk_lambda' ["stmt_map_id", t_stmt_map_id] @@
                  (* check for all triggers *)
                  List.fold_left (fun acc_code (t, args, ss, s_ms) ->
                    let mk_check_s s_m = mk_eq (mk_var "stmt_map_id") @@ mk_cint s_m in
                    (* common code *)
                    let push_code =
                        (* pull arguments out of log (if needed) *)
                        (if args = [] then id_fn else
                          mk_let (fst_many args)
                            (mk_apply' (nd_log_get_bound_for t) [mk_var "vid"])) @@
                        List.fold_left (fun acc (s, s_m) ->
                          mk_if (mk_eq (mk_var "stmt_map_id") @@ mk_cint s_m)
                            (let rmaps = P.rhs_maps_of_stmt c.p s in
                            List.fold_left (fun acc m ->
                              mk_if (mk_eq (mk_var "map_id") @@ mk_cint m)
                                (mk_apply' (send_push_name_of_t c t s m) @@
                                  (* @buffered: force output of a trigger header *)
                                  mk_ctrue::args_of_t_as_vars_with_v c t)
                                acc)
                              mk_cunit
                              rmaps)
                            acc)
                          mk_cunit @@
                          list_zip ss s_ms
                    in
                    (* check if the stmts_maps are in this trigger *)
                    mk_if
                      (list_fold_to_last (fun acc s_m -> mk_or (mk_check_s s_m) acc) mk_check_s s_ms)
                      (* handle isobatches *)
                      (mk_if (mk_var "is_isobatch")
                        (mk_block [
                          (* pull the list of batch vids out *)
                          mk_at_with' isobatch_buffered_fetch_vid_map_id (mk_var "stmt_map_id") @@
                            mk_lambda' isobatch_buffered_fetch_vid_map_e @@
                              mk_case_ns (mk_lookup' "inner" [mk_var "vid"; mk_cunknown]) "vids"
                                (mk_error "missing buffered batch") @@
                                mk_iter
                                  (mk_lambda' ["vid", t_vid] push_code) @@
                                  mk_var "vids";
                          (* send metadata for this batch *)
                          mk_apply' nd_send_isobatch_push_meta_nm [mk_var "vid"];
                          (* delete the batch data *)
                          mk_update_at_with isobatch_buffered_fetch_vid_map_id (mk_var "stmt_map_id") @@
                            mk_lambda' isobatch_buffered_fetch_vid_map_e @@
                              mk_delete "inner" [mk_var "vid"; mk_cunknown];
                        ])
                        (* single vids *)
                        push_code)
                      acc_code)
                    mk_cunit
                  t_info) @@
                  mk_var "stmt_map_ids") @@
                mk_filter_leq (mk_var "inner") [mk_var "min_vid"; mk_cunknown]
          ;
          (* delete these entries from the fetch buffer *)
          mk_update_at_with nd_fetch_buffer_id (mk_var "map_id") @@
            mk_lambda' nd_fetch_buffer_e @@
              mk_filter_gt (mk_var "inner") [mk_var "min_vid"; mk_cunknown]
        ])
        (* else do nothing *)
        mk_cunit
    ]

(* call from do_complete when done to check if fully done *)
let nd_complete_stmt_cntr_check c =
  mk_global_fn nd_complete_stmt_cntr_check_nm ["vid", t_vid; "stmt_id", t_stmt_id] [] @@
  (* if we have nothing to send, we can delete our stmt_cntr entry right away *)
  mk_block @@
    [mk_update_at_with nd_stmt_cntrs_id (mk_var "stmt_id") @@
      mk_lambda' nd_stmt_cntrs_e @@ mk_block [
        mk_delete nd_stmt_cntrs_inner.id [mk_var "vid"; mk_cunknown];
        mk_var nd_stmt_cntrs_inner.id
      ] ;
      mk_decr nd_stmt_cntr_size.id] @
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
let nd_do_complete_trigs c t s =
    let comp_nm = do_complete_name_of_t t s in
    let args = nd_do_complete_trig_args c t in
    mk_global_fn (comp_nm^"_trig") args [] @@
      mk_block [
        (* false: not isobatch. not relevant since no rhs maps *)
        mk_apply' comp_nm @@ mk_cfalse :: (ids_to_vars @@ fst_many @@ args);
      ]

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
let nd_do_complete_fns c ast trig_name =
  (* @has_rhs: whether this statement has rhs maps *)
  let do_complete_fn has_rhs stmt_id =
    mk_global_fn (do_complete_name_of_t trig_name stmt_id)
      (["is_isobatch", t_bool]@args_of_t_with_v c trig_name) [] @@
    let lmap = P.lhs_map_of_stmt c.p stmt_id in
    let fst_hop, snd_hop = mk_cint 1, mk_cint 2 in
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

        (* for profiling: mark with tag for do_complete done *)
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
                  (* if our sent_msgs is 0, we need to delete the stmt cntr entry *)
                  mk_if (mk_var "is_isobatch")
                    mk_cunit @@
                    mk_apply' nd_complete_stmt_cntr_check_nm
                      [mk_var "vid"; mk_cint stmt_id]
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

(* rcv notification of a corrective finish from other nodes *)
let nd_rcv_corr_done c =
  let args = nd_rcv_corr_done_args in
  mk_global_fn nd_rcv_corr_done_nm args [] @@
    mk_block [
      (* update the corrective map *)
      mk_let ["do_delete"]
        (mk_apply' nd_update_corr_map_nm @@ ids_to_vars @@ fst_many args) @@

      mk_if (mk_var "do_delete")
        (mk_block [
          (* for profiling: mark tag with corrective done *)
          prof_property prof_tag_corr_done @@ ProfLatency("vid", "stmt_id");

          (* delete the whole stmt_cntr entry *)
          mk_update_at_with nd_stmt_cntrs_id (mk_var "stmt_id") @@
            mk_lambda' nd_stmt_cntrs_e @@
              mk_delete_block nd_stmt_cntrs_inner.id [mk_var "vid"; mk_cunknown];
          mk_decr nd_stmt_cntr_size.id;
          Proto.nd_post_delete_stmt_cntr c
        ])
        mk_cunit
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
let nd_rcv_correctives_trig c t s =
  let corr_rmaps = List.filter (fun m -> List.mem m c.corr_maps) @@
    P.rhs_maps_of_stmt c.p s in
  List.map (fun m ->
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
        (mk_poly_fold_tag' (map_delta.id^"_map")
          (mk_lambda4' ["acc", map_delta.t] p_idx p_off map_delta.e @@
            mk_insert_block "acc" @@ ids_to_vars @@ fst_many map_delta.e) @@
          mk_empty map_delta.t) @@

      (* increment the idx, offsets of the map variant *)
      mk_poly_skip_all_block (map_delta.id^"_map") [
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
                (mk_at_with' D.nd_stmt_cntrs_id (mk_cint s) @@
                 mk_lambda' D.nd_stmt_cntrs_e @@
                  mk_case_ns (mk_lookup' D.nd_stmt_cntrs_inner.id
                               [mk_var "compute_vid"; mk_cunknown]) "lkup"
                    (* 0 if we have no value (already deleted) *)
                    (mk_cint 0) @@
                    mk_fst @@ mk_snd @@ mk_var "lkup") @@
                (* check if our stmt_counter is 0 *)
                mk_if_eq (mk_var "cntr") (mk_cint 0)
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
    ]) corr_rmaps

(* do corrective functions *)
(* very similar to do_complete, but we only have to do it for certain
 * (stmt, map) combinations.
 * Do_corrective , unlike do_complete, doesn't need to add to an earlier vid
 * value. Instead, it adds to the value that already resides at a specific
 * vid and propagates. *)
let nd_do_corrective_fns c t s ast =
  let corr_rmaps = List.filter (fun m -> List.mem m c.corr_maps) @@
    P.rhs_maps_of_stmt c.p s in
  List.map (fun m ->
    let tuple_typ = wrap_t_calc' @@ P.map_types_for c.p m in
    let fn_nm = do_corrective_name_of_t c t s m in
    let fn_args = D.orig_vals @ args_of_t_with_v c t @ ["delta_tuples", tuple_typ] in
    mk_global_fn fn_nm fn_args [t_int] @@
        let lmap = P.lhs_map_of_stmt c.p s in
        let send_corr_fn = send_corrective_name_of_t c lmap in
        let args, is_col, ast = M.modify_corr_ast c ast m s t in
        let delta = "delta_vals" in

        (* if we have loop vars, we need to filter the lmap values here *)
        let_lmap_filtering c delta s lmap
        (* We *can't* filter out 0 values, because they may add to a map
         * that didn't have any value, and initialize a value at that key *)
          (mk_flatten @@ mk_map (mk_lambda' args ast) @@ mk_var "delta_tuples")
            ~alt:(mk_cint 0) @@
            mk_block [
              (* add delta *)
              do_add_delta c (mk_var delta) lmap ~corrective:true;
              (* send correctives *)
              if List.exists ((=) lmap) c.corr_maps
              (* send correctives with hop + 1, and return the num of correctives *)
              then mk_apply' send_corr_fn @@
                (modify_e orig_vals ["hop", mk_add (mk_cint 1) @@ mk_var "hop"]) @
                [mk_var "vid"; mk_var delta]
              (* if we have no more correctives, return 0 *)
              else mk_cint 0
            ]
    ) corr_rmaps

(* function to write push data *)
let nd_handle_uniq_poly_nm = "nd_handle_uniq_poly"
let nd_handle_uniq_poly c =
  let ts = P.get_trig_list c.p ~sys_init:true ~delete:c.gen_deletes in
  let s_rhs = List.flatten @@ List.map (P.s_and_over_stmts_in_t c.p P.rhs_maps_of_stmt) ts in
  let bufs_m = List.map (fun (s,m) -> P.buf_of_stmt_map_id c.p s m, m) s_rhs in
  let tag_bufs_m = List.map (fun (buf, m) -> fst @@
                              List.find (fun (i, ti) -> ti.tag = buf) c.poly_tags, buf, m) bufs_m in
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

let sub_trig_dispatch c fn_nm t_args =
  List.fold_left
    (fun acc_code (itag, ti) -> match ti.tag_typ with
        (* only match with the sub-triggers *)
        | SubTrig(has_ds, ts) when List.mem fn_nm ts ->
        mk_if (mk_eq (mk_var "tag") @@ mk_cint itag)
          (* look up this tag *)
          (mk_poly_at_with' ti.tag @@ mk_lambda' ti.args @@
            (* if we have subdata, the function must return the new idx, offset *)
            let call l = mk_apply' ti.fn @@
                ti.const_args @ ids_to_vars @@ fst_many @@ l @ ti.args @ t_args in
            if has_ds then call D.poly_args
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
  let t_args = trig_no_arg_sub_handler_args in
  mk_global_fn fn_nm (poly_args @ t_args) [t_int; t_int] @@
  (* skip over the entry tag *)
  mk_poly_skip_block fn_nm [
    (* clear the trig send bitmaps. These make sure we output a slim trig header
       for any sub-trigger output (ie. pushes) *)
    mk_set_all D.send_trig_header_bitmap.id [mk_cfalse];
    (* dispatch the sub triggers (pushes) *)
    mk_poly_iter' @@
      mk_lambda'' (p_tag @ p_idx @ p_off) @@
        (* print trace if requested *)
        do_trace ("nstsh"^trace_trig t)
                  [t_int, mk_var "vid"] @@
        sub_trig_dispatch c fn_nm t_args;
  ]

(* handle all sub-trigs that need only a vid (pushes) *)
let nd_load_arg_trig_sub_handler c t =
  let fn_nm = trig_load_arg_sub_handler_name_of_t t in
  let t_args = trig_load_arg_sub_handler_args in
  let trig_args = args_of_t_with_v c t in
  let load_args = D.args_of_t c t in
  mk_global_fn fn_nm (poly_args @ t_args) [t_int; t_int] @@
  (* skip over the entry tag *)
  mk_poly_skip_block fn_nm [
    (* clear the trig send bitmaps. These make sure we output a slim trig header
       for any sub-trigger output (ie. pushes) *)
    mk_set_all D.send_trig_header_bitmap.id [mk_cfalse];
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
        sub_trig_dispatch c fn_nm trig_args;
  ]

(* handle all sub-trigs that need trigger arguments *)
let nd_save_arg_trig_sub_handler c t =
  let fn_nm = trig_save_arg_sub_handler_name_of_t t in
  let t_args = trig_save_arg_sub_handler_args c t in
  mk_global_fn fn_nm (poly_args @ t_args) [t_int; t_int] @@
  (* skip over the entry tag *)
  mk_poly_skip_block fn_nm [
    (* save the bound args for this vid *)
    mk_apply' (nd_log_write_for c t) @@ args_of_t_as_vars_with_v c t;
    (* clear the trig send bitmaps. These make sure we output a trig header
       for any sub-trigger output (ie. pushes) *)
    mk_set_all D.send_trig_header_bitmap.id [mk_cfalse];
    (* dispatch the sub triggers *)
    mk_poly_iter' @@
      mk_lambda'' (p_tag @ p_idx @ p_off) @@
        (* print trace if requested *)
        do_trace ("ntsh"^trace_trig t)
                 [t_int, mk_var "vid";
                  t_int, mk_var "tag";
                  t_int, mk_var "idx";
                  t_int, mk_var "offset"] @@
        sub_trig_dispatch c fn_nm t_args;
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
      (mk_apply' clear_buffered_fetch_helper_nm []) mk_cunit;

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
          (fun acc_code (itag, ti) -> match ti.tag_typ with
             | Trig has_ds ->
              mk_if (mk_eq (mk_var "tag") @@ mk_cint itag)
                (* look up this tag *)
                (mk_poly_at_with' ti.tag @@ mk_lambda' ti.args @@
                  (* if we have subdata, the function must return the new idx, offset *)
                  let call l =
                    mk_apply' ti.fn @@
                      ti.const_args @ ids_to_vars @@ fst_many @@ l @ ti.args in
                  if has_ds then call D.poly_args
                    (* otherwise we must skip ourselves *)
                  else mk_block [call []; mk_poly_skip' ti.tag])
                acc_code
             | _ -> acc_code)
          (mk_error "unmatched tag")
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

let nd_dispatcher_buf_inner =
  let e = ["batch_id", t_vid; "poly_queue", poly_queue.t] in
  create_ds ~e "inner_dispatcher_buf" @@ t_of_e e

(* buffer for dispatcher to reorder poly msgs *)
let nd_dispatcher_buf =
  let e = ["num", t_int; "batch_info", nd_dispatcher_buf_inner.t] in
  create_ds "dispatcher_buf" ~e @@ wrap_tmap' @@ snd_many e

(* remember the last number we've seen *)
let nd_dispatcher_last_num = create_ds "nd_dispatcher_last_num" @@ mut t_int

(* trigger version of dispatcher. Called by other nodes *)
let trig_dispatcher_trig c =
  mk_code_sink' trig_dispatcher_trig_nm
    ["batch_id", t_vid; "poly_queue", poly_queue.t] [] @@
  mk_block [
    mk_poly_unpack (mk_var "poly_queue");
    clear_poly_queues ~unique:false c;
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
let nd_dispatcher_next_num = create_ds "nd_dispatcher_next_num" @@ mut t_int

let nd_from_sw_trig_dispatcher_trig c =
  mk_code_sink' nd_from_sw_trig_dispatcher_trig_nm
    ["sender_ip", t_int; "num", t_int; "batch_data", nd_dispatcher_buf_inner.t] [] @@
  mk_let ["batch_id"; "poly_queue"] (mk_var "batch_data") @@
  mk_let_block ["is_isobatch"] (is_isobatch_id "batch_id")
  [
    mk_assign nd_dispatcher_next_num.id @@
      (mk_if (mk_eq (mk_var nd_dispatcher_last_num.id) @@ mk_var g_max_int.id)
        (mk_cint 0) @@
        mk_add (mk_var nd_dispatcher_last_num.id) @@ mk_cint 1);
    (* check if we're contiguous *)
    mk_if (mk_eq (mk_var "num") (mk_var nd_dispatcher_next_num.id))
      (* then dispatch right away *)
      (mk_block [
          (* unpack the polyqueue *)
          mk_poly_unpack (mk_var "poly_queue");
          mk_assign nd_dispatcher_last_num.id @@ mk_var nd_dispatcher_next_num.id;
          mk_incr nd_dispatcher_next_num.id;
          clear_poly_queues c;
          (* buffer the ack to the switch *)
          mk_if_eq (mk_var "sender_ip") (mk_cint @@ -1) mk_cunit @@
            GC.nd_ack_send_code ~addr_nm:"sender_ip" ~vid_nm:"batch_id";

          mk_if (mk_var "is_isobatch")
            (mk_block [
              (* replace, clear out the isobatch_map_helper *)
              mk_iter_bitmap' ~idx:stmt_ctr.id
                (mk_insert_at isobatch_stmt_helper_id (mk_var stmt_ctr.id) [mk_empty isobatch_map_inner2.t])
                isobatch_stmt_helper_bitmap_id;
              mk_set_all isobatch_stmt_helper_bitmap_id [mk_cfalse];
              ])
            mk_cunit;

          mk_apply' trig_dispatcher_nm [mk_var "batch_id"; mk_var "poly_queue"];

          mk_if (mk_var "is_isobatch")
            (* iterate over the isobatch map helper, and move its contents to the isobatch_map *)
            (mk_iter_bitmap' ~idx:stmt_ctr.id
              (mk_let ["x"] (mk_delete_at isobatch_stmt_helper_id @@ mk_var stmt_ctr.id) @@
                mk_update_at_with isobatch_vid_map_id (mk_var stmt_ctr.id) @@
                mk_lambda' isobatch_vid_map_e @@
                  mk_insert_at "inner" (mk_var "batch_id") [mk_var "x"])
              isobatch_stmt_helper_bitmap_id)
             mk_cunit;
       ]) @@
      (* else, stash the poly_queue in our buffer *)
      mk_insert nd_dispatcher_buf.id
        [mk_var "num"; mk_tuple [mk_var "batch_id"; mk_var "poly_queue"]]
    ;
    (* check if the next num is in the buffer *)
    mk_delete_with nd_dispatcher_buf.id [mk_var nd_dispatcher_next_num.id; mk_cunknown]
      (mk_lambda' unit_arg @@ mk_cunit)
      (* recurse with the next number *)
      (mk_lambda' nd_dispatcher_buf.e @@
       mk_send nd_from_sw_trig_dispatcher_trig_nm G.me_var
         [mk_cint (-1); mk_var "num"; mk_var "batch_info"])
  ]

let sw_event_driver_isobatch_nm = "sw_event_driver_isobatch"
let sw_event_driver_isobatch c =
  let trig_list = StrSet.of_list @@ P.get_trig_list c.p in
  let tags = List.filter (fun (_, ti) ->
      ti.tag_typ = Event && (StrSet.mem ("insert_"^ti.tag) trig_list || ti.tag = "sentinel"))
    c.poly_tags in
  mk_global_fn sw_event_driver_isobatch_nm
    ["batch_id", t_vid; "poly_queue", poly_queue.t] [t_vid] @@
  mk_let ["idx"] (mk_cint 0) @@
  mk_let ["offset"] (mk_cint 0) @@
  mk_let ["tag"] (mk_poly_tag_at (mk_var "poly_queue") @@ mk_cint 0) @@
  List.fold_left (fun acc_code (i, ti) ->
      mk_if_eq (mk_var "tag") (mk_cint i)
        (if ti.tag = "sentinel" then
           Proto.sw_seen_sentinel ~check_size:false
        else
          mk_poly_at_with' ti.tag @@ mk_lambda' ti.args @@
          mk_if (mk_var "do_insert")
            (mk_apply' (send_fetches_isobatch_name_of_t @@ "insert_"^ti.tag)
               [mk_var "batch_id"; mk_var "poly_queue"]) @@
             mk_apply' (send_fetches_isobatch_name_of_t @@ "delete_"^ti.tag)
               [mk_var "batch_id"; mk_var "poly_queue"])
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
          mk_set_all D.send_trig_header_bitmap.id [mk_cfalse] ;

          List.fold_left (fun acc_code (i, ti) ->
            (* check if we match on the id *)
            mk_if_eq (mk_var "tag") (mk_cint i)
              (if ti.tag = "sentinel" then
                (* don't check size of event queue *)
                Proto.sw_seen_sentinel ~check_size:false
              else
                let send_fetches pref =
                  let ss = P.stmts_of_t c.p @@ pref^ti.tag in
                  (mk_block @@ List.map (fun s ->
                    mk_apply' (send_fetch_name_of_t (pref^ti.tag) s) @@
                      ids_to_vars @@ "vid":: (fst_many @@ tl ti.args)) ss)
                in
                mk_poly_at_with' ti.tag @@
                  mk_lambda' ti.args @@
                      mk_if (mk_var "do_insert")
                        (send_fetches "insert_")
                        (if c.gen_deletes then send_fetches "delete_" else mk_cunit))
              acc_code)
          (mk_error "mismatch on event id") @@
          List.filter (fun (_, ti) ->
              ti.tag_typ = Event &&
              (StrSet.mem ("insert_"^ti.tag) trig_list || ti.tag = "sentinel"))
            c.poly_tags
        ;
        mk_add (mk_cint 1) @@ mk_var "vid"
      ])
  (mk_var "batch_id") @@
  mk_var "poly_queue"

(* The driver trigger: loop over the even data structures as long as we have spare vids *)
(* This only fires when we get the token *)
let sw_event_driver_trig c =
  mk_code_sink' sw_event_driver_trig_nm
    ["batch_id", t_vid; "vector_clock", TS.sw_vector_clock.t] [] @@
    (* if we're initialized and we have stuff to send *)
    mk_if (mk_and (mk_var D.sw_init.id) @@ mk_gt (mk_size @@ mk_var D.sw_event_queue.id) @@ mk_cint 0)
      (mk_case_sn (mk_peek @@ mk_var D.sw_event_queue.id) "poly_queue"
        (mk_block [
          (* unpack the incoming polyqueue *)
          mk_poly_unpack @@ mk_var "poly_queue";
          clear_poly_queues ~unique:false c;

          (* for debugging, sleep if we've been asked to *)
          mk_if (mk_neq (mk_var D.sw_event_driver_sleep.id) @@ mk_cint 0)
            (mk_apply' "usleep" [mk_var D.sw_event_driver_sleep.id])
            mk_cunit;
          (* for profiling, save the vid and time *)
          prof_property prof_tag_pre_send_fetch @@ ProfLatency("batch_id", "-1");

          (* clear out the trig arg map *)
          mk_iter_bitmap'
            (mk_update_at_with send_trig_args_map.id (mk_var "ip") @@
              mk_lambda' send_trig_args_map.e @@
                mk_block [
                  mk_clear_all send_trig_args_inner.id;
                  mk_var send_trig_args_inner.id
                ])
            send_trig_args_bitmap.id;
          mk_set_all send_trig_args_bitmap.id [mk_cfalse];

          (* calculate the next vid *)
          mk_let ["next_vid"]
            (mk_if (mk_var isobatch_mode.id)
              (mk_apply' sw_event_driver_isobatch_nm [mk_var "batch_id"; mk_var "poly_queue"]) @@
               mk_apply' sw_event_driver_single_vid_nm [mk_var "batch_id"; mk_var "poly_queue"]) @@
          (* update the vector clock by bits in the outgoing poly_queues *)
          mk_let ["vector_clock"]
            (mk_agg_bitmap'
              ["acc", TS.sw_vector_clock.t]
              (mk_update_at_with_block "acc" (mk_var "ip") @@
                mk_lambda' ["x", t_int] @@
                  (* if we're at max_int, skip to 0 so we don't get negatives *)
                  mk_if (mk_eq (mk_var "x") @@ mk_var g_max_int.id)
                    (mk_cint 0) @@
                    mk_add (mk_cint 1) @@ mk_var "x")
              (mk_var "vector_clock")
              D.poly_queue_bitmap.id) @@
          mk_block [
            (* send (move) the outgoing polyqueues *)
            mk_let ["send_count"]
              (mk_agg_bitmap' ["count", t_int]
                (* move and delete the poly_queue and ship it out with the vector clock num *)
                  (* pull out the poly queue *)
                  (mk_let_block ["pq"] (mk_delete_at poly_queues.id @@ mk_var "ip")
                    [
                      prof_property 0 @@ ProfSendPoly("batch_id", "ip", "pq");
                      mk_sendi nd_from_sw_trig_dispatcher_trig_nm (mk_var "ip")
                        [mk_var D.me_int.id;
                         mk_at' "vector_clock" @@ mk_var "ip";
                         mk_tuple [mk_var "batch_id"; mk_var "pq"]];
                      mk_add (mk_var "count") @@ mk_cint 1
                    ])
                (mk_cint 0) @@
               D.poly_queue_bitmap.id) @@
            (* save the latest ack info *)
            GC.sw_update_send ~n:(mk_var "send_count") ~vid_nm:"batch_id";
            (* send the new (vid, vector clock). make sure it's after we use vector
               clock data so we can move *)
            mk_send sw_event_driver_trig_nm (mk_var TS.sw_next_switch_addr.id)
              [mk_var "next_vid"; mk_var "vector_clock"];
            (* finish popping the incoming queue *)
            mk_pop D.sw_event_queue.id;
            (* update highest vid seen *)
            mk_assign TS.sw_highest_vid.id @@ mk_var "next_vid";

            (* for profiling, annotate with the last vid seen *)
            prof_property prof_tag_post_send_fetch @@ ProfLatency("next_vid", "-1");

            (* check if we're done *)
            Proto.sw_check_done ~check_size:true
          ]]) @@
        mk_error "oops") @@
     (* otherwise send the same vid and vector clock *)
     mk_send sw_event_driver_trig_nm (mk_var TS.sw_next_switch_addr.id)
       [mk_var "batch_id"; mk_var "vector_clock"]

let sw_sent_demux_ctr = create_ds "sw_sent_demux_ctr" @@ mut t_int
let sw_total_demux_ctr = create_ds "sw_total_demux_ctr" @@ mut t_int
let sw_demux_poly_queue =
  create_ds "sw_demux_poly_queue" @@ D.poly_queue.t

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
      (mk_block [
          mk_poly_insert "sentinel" "sw_demux_poly_queue" [mk_cunit];
          mk_tuple [mk_ctrue; mk_ctrue]
        ]) @@
      mk_tuple [mk_cfalse; mk_cfalse]
  in
  mk_code_sink' sw_demux_nm ["args", wrap_ttuple @@ List.map str_of_date_t combo_t] [] @@
  (* create a poly queue with a given number of messages *)
    mk_let_block ["do_incr"; "do_send"]
      (StrMap.fold (fun trig arg_indices acc ->
        let args =
          (* add 1 for tuple access *)
          List.map (fun i -> convert_date i @@ mk_subscript (i+1) @@ mk_var "args")
            arg_indices
        in
        (* check if we match on the trig id *)
        mk_if_eq (mk_fst @@ mk_var "args") (mk_cstring trig)
          (mk_if_eq
            (mk_apply' "mod" [mk_var sw_total_demux_ctr.id; mk_var num_switches.id])
              (mk_var D.sw_csv_index.id)
              (mk_let_block ["do_insert"]
                 (mk_eq (mk_snd @@ mk_var "args") @@ mk_cint 1) [
                mk_poly_insert trig "sw_demux_poly_queue" @@ (mk_var "do_insert")::args;
                mk_tuple [mk_ctrue; mk_ctrue]
               ]) @@
              (* if we don't match on the mod, don't insert but increment *)
            mk_tuple [mk_ctrue; mk_cfalse])
          acc)
        t_arg_map
        sentinel_code) [
    (* increment counter if we saw a valid trigger *)
    mk_if (mk_var "do_incr") (mk_incr sw_total_demux_ctr.id) mk_cunit;
    mk_if (mk_var "do_send") (mk_incr sw_sent_demux_ctr.id) mk_cunit;
    (* ship if we hit the count or saw the sentinel *)
    mk_if (mk_or (mk_eq (mk_cint 0) @@
                    mk_apply' "mod" [mk_var sw_sent_demux_ctr.id; mk_var D.sw_poly_batch_size.id]) @@
                  mk_eq (mk_fst @@ mk_var "args") @@ mk_cstring "")
      (mk_block [
        (* copy the polybuffer to the queue *)
        mk_insert sw_event_queue.id [mk_var sw_demux_poly_queue.id];
        (* clear the buffer *)
        mk_clear_all sw_demux_poly_queue.id;
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
     Instruction(Consume("master"));
    ] in
  let sw_flows_old = List.map mk_no_anno [
    Source(Resource("switch_old",
      Handle(wrap_ttuple @@ List.map str_of_date_t @@ fst @@ combine_trig_args c,
        File c.stream_file,
        CSV)));
    BindFlow("switch_old", sw_demux_nm);
    Instruction(Consume("switch_old"));
    ] in
  let sw_flows_new = List.map mk_no_anno [
    Source(Resource("switch",
      Handle(D.poly_event_queue.t, PolyFileSeq("seqfiles", "inorder"), BIN)));
    BindFlow("switch", sw_demux_poly_nm);
    Instruction(Consume("switch"));
  ] in

  List.map mk_no_anno [
    Role("master", ms_flows);
    Role("switch_old", sw_flows_old);
    Role("switch", sw_flows_new);
    Role("timer", []);
    Role("node", []);
  ]

(* loader vars for ast *)
let gen_loader_vars ast =
  let tables = ModifyAst.loader_tables ast in
  List.map (fun (s, f) ->
    mk_global_val_init (s^"_path") t_string @@ mk_cstring f) tables

let declare_global_vars c ast =
  (* TODO: dummy map currently needed for MapE generation *)
  mk_global_val "dummy_map" (wrap_tmap' [t_string; wrap_tbag' [t_int; t_int]]) ::
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
  K3Route.global_vars c @
  K3Shuffle.global_vars @
  GC.global_vars c @
  List.map decl_global
    [send_put_bitmap;
     send_put_ip_map c.p;
     send_put_isobatch_map c;
     send_fetch_bitmap;
     send_push_bitmap;
     send_push_cntrs;
     send_push_isobatch_map c;
     send_corrective_bitmap;
     send_corrective_ip_map;
     sw_total_demux_ctr;
     sw_sent_demux_ctr;
     sw_demux_poly_queue;
     nd_dispatcher_buf;
     nd_dispatcher_last_num;
     nd_dispatcher_next_num;
     nd_check_stmt_cntr_do_delete;
     nd_check_stmt_cntr_ret;
     nd_check_stmt_cntr_init;
     nd_update_corr_delete;
     (* delayed buffered fetch decisions *)
     rcv_fetch_isobatch_bitmap c;
     rcv_fetch_isobatch_decision_bitmap c;
    ]

let declare_global_funcs c ast =
  flatteners c @
  nd_log_master_write ::
  (P.for_all_trigs ~sys_init:true ~delete:c.gen_deletes c.p @@ nd_log_write c) @
  (P.for_all_trigs ~sys_init:true ~delete:c.gen_deletes c.p @@ nd_log_get_bound c) @
  nd_update_corr_map ::
  begin if c.gen_correctives then [nd_filter_corrective_list] else [] end @
  K3Ring.functions @
  (List.map (fun (i, (_, maps)) -> nd_add_delta_to_buf c (hd maps)) @@ D.uniq_types_and_maps c) @
  TS.functions @
  K3Route.functions c @
  K3Shuffle.functions c @
  GC.functions c (Proto.sw_check_done ~check_size:true)

(* Generate all the code for a specific trigger *)
let gen_dist_for_t c ast t =
  let s_r = P.stmts_with_rhs_maps_in_t c.p t in
  let s_no_r = P.stmts_without_rhs_maps_in_t c.p t in
  let ss = P.stmts_of_t c.p t in
  let fns1 =
  (List.flatten @@ List.map (fun s ->
        [sw_send_rhs_fetches c t s;
        sw_send_puts c t s;
        sw_send_puts_isobatch c t s;
        sw_send_rhs_completes c t s;
        sw_send_fetch_fn c t s;
        ]) ss) @
   (if c.gen_correctives then
      List.flatten @@ List.map (fun s -> nd_do_corrective_fns c t s ast) s_r
    else []) @
   (List.flatten @@ List.map (fun s ->
      nd_send_push_stmt_map_trig c t s) s_r)
  in
  let fns2 =
    nd_do_complete_fns c ast t @
    (List.flatten @@ List.map (fun s ->
        [nd_rcv_put_trig c t s;
         nd_rcv_fetch_trig c t s;
         nd_rcv_push_trig c t s]) s_r) @
    (List.map (fun s -> nd_do_complete_trigs c t s) s_no_r) @
    [sw_send_fetches_isobatch c t;
     nd_save_arg_trig_sub_handler c t;
     nd_load_arg_trig_sub_handler c t;
     nd_no_arg_trig_sub_handler c t] @
    (if c.gen_correctives then
       List.flatten @@ List.map (fun s -> nd_rcv_correctives_trig c t s) @@
        P.stmts_with_rhs_maps_in_t c.p t
    else [])
  in
  fns1, fns2

(* Function to generate the whole distributed program *)
(* @param force_correctives Attempt to create dist code that encourages correctives *)
let gen_dist ?(gen_deletes=true)
             ?(gen_correctives=true)
             ?(use_opt_route=true)
             ~stream_file
             ~map_type
             ~(agenda_map: mapping_t)
             p ast =
  let sys_init =
    try ignore(P.find_trigger p "system_ready_event"); true
    with Not_found | P.Bad_data _ -> false in
  let unused_trig_args = M.unused_trig_args ast in

  (* adjust agenda_map for unused trig args *)
  let reduced_agenda_map = second (StrMap.mapi @@ fun trig_nm l ->
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
      sys_init;
      stream_file;
      agenda_map;
      reduced_agenda_map;
      unused_trig_args;
      map_indices = D.Bindings.multi_idx_access_patterns p ast;
      route_indices = D.Bindings.route_access_patterns p;
      freevar_info = IntMap.of_list @@ List.map (fun s -> s, P.free_bound_vars p s) @@ P.get_stmt_list p;
      use_opt_route;
    } in
  let c = {c with corr_maps = maps_potential_corrective c} in
  (* to get poly_tags, we need c *)
  let c = { c with poly_tags = D.calc_poly_tags c;
                   event_tags = D.calc_event_tags c} in
  (* regular trigs then insert entries into shuffle fn table *)
  let fns1, fns2 =
    (fun (x,y) -> let a = List.flatten in a x, a y) @@ list_unzip @@
      P.for_all_trigs ~sys_init:true ~delete:c.gen_deletes c.p @@ gen_dist_for_t c ast
  in
  let prog =
    mk_typedef poly_queue_typedef_id (poly_queue_typedef c) ::
    mk_typedef upoly_queue_typedef_id (upoly_queue_typedef c) ::
    mk_typedef poly_event_typedef_id (poly_event_typedef c) ::
    declare_global_vars c ast @
    declare_global_funcs c ast @
    (if c.gen_correctives then send_corrective_fns c else []) @
    (* we need this here for scope *)
    clear_send_put_isobatch_map ::
    fns1 @
    [nd_exec_buffered_fetches c;  (* depends: send_push *)
     nd_complete_stmt_cntr_check c; (* depends: exec_buffered *)
     nd_check_stmt_cntr_index c] @
    fns2 @
    [
     nd_rcv_corr_done c;
     nd_handle_uniq_poly c;
     clear_buffered_fetch_helper;
     nd_send_isobatch_push_meta c;
     trig_dispatcher c;
     trig_dispatcher_unique c;
     sw_event_driver_isobatch c;
     sw_event_driver_single_vid c;
     nd_rcv_stmt;
    ] @
    [mk_flow @@
      Proto.triggers c @
      GC.triggers c @
      TS.triggers @
      Timer.triggers c @
      [
        trig_dispatcher_trig c;
        trig_dispatcher_trig_unique c;
        nd_from_sw_trig_dispatcher_trig c;
        sw_event_driver_trig c;
        sw_demux c;
        sw_demux_poly c;
      ]
    ] @
    roles_of c ast
  in
  snd @@ U.renumber_program_ids prog

