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
module Proto = Protocol
module M =  ModifyAst

open GenCommon

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

(* used by send correctives: {ip} *)
let send_corrective_bitmap = create_ds "send_corrective_bitmap" t_bitset

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
          mk_clear_all send_corrective_bitmap.id;

          (* TODO: if lvars have no impact, only shuffle once *)
          mk_iter
            (mk_lambda' ["vid", t_vid] @@
              (* get bound vars from log so we can calculate shuffle *)
              let args = D.args_of_t c t in
              (if args <> [] (* && not no_bound *) then
                nd_log_get_bound c t
              else id_fn) @@
              mk_block [
                mk_apply' shuffle_fn @@
                  key @ [mk_cint pat_idx; mk_cint (-1); mk_var "delta_tuples2"];
                mk_iter_bitmap'
                  (mk_block [
                    mk_insert send_corrective_bitmap.id [mk_var "ip"];
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
                        C.buffer_for_send rcv_trig "ip" @@
                          (ids_to_vars @@ fst_many orig_vals) @ [mk_var "corrective_vid"];
                        (* buffer tuples from indices *)
                        buffer_tuples_from_idxs ~drop_vid:true delta_tuples2.id delta_tuples2.t
                          (map_ds.id^"_map") (mk_var "t_indices");
                        (* buffer vids *)
                        mk_iter (mk_lambda'' ["vid", t_vid] @@
                          C.buffer_for_send ~wr_bitmap:false "vids" "ip" [mk_var "vid"]) @@
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
                  (* TODO: no access to batch id/vid here *)
                  ((if D.args_of_t c t <> [] then
                    nd_log_get_bound_immed ~batch_nm:"compute_vid" c t
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
          C.buffer_for_send nd_rcv_corr_done_nm "orig_addr"
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
