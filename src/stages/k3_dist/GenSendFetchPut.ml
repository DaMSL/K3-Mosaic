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

(*** Switch -> Node ***)
(*** Puts, Fetches, Do_completes ***)

let debug_run_test_tuples = debug_run_test_var debug_run_sw_send_tuples

(* {ip} *)
let send_put_bitmap = create_ds "send_put_bitmap" t_bitset

(* vector of maps *)
let send_put_isobatch_inner2 = create_ds "inner2" t_bitset

(* vector of source ips *)
let send_put_isobatch_inner =
  let e = ["inner2", send_put_isobatch_inner2.t] in
  create_ds ~e "inner" @@ wrap_tvector @@ t_of_e e

(* dest ip -> source ip -> map vector, for a whole batch *)
let send_put_isobatch_map_id = "send_put_isobatch_map"
let send_put_isobatch_map_e = ["bitmap", t_bitset; "inner", send_put_isobatch_inner.t]
let send_put_isobatch_map c =
  let e = send_put_isobatch_map_e in
  let init =
    mk_map (mk_lambda' unknown_arg @@
      mk_tuple [
        mk_empty t_bitset; (* {ips} *)
        mk_map (mk_lambda' unknown_arg @@ mk_empty t_bitset) @@ (* {maps} *)
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
          mk_let_block ["inner3"]
            (mk_agg_bitmap' ~idx:"ip2" ["acc", send_put_isobatch_inner.t]
              (mk_update_at_with_block "acc" (mk_var "ip2") @@
                mk_lambda' send_put_isobatch_inner.e @@
                  mk_clear_all_block "inner2")
              (mk_var "inner")
              "bitmap") [
            mk_clear_all "bitmap";
            mk_tuple [mk_var "bitmap"; mk_var "inner3"]
          ])
      send_put_bitmap.id;
    mk_clear_all send_put_bitmap.id;
  ]

(* {ip} *)
let sw_send_stmt_bitmap = create_ds "sw_send_stmt_bitmap" t_bitset

(* constraint: puts must be sent after all the statements, so that if the pushes are already done,
   the puts can execute (pull out the batches batch_id -> vid mapping) *)
let sw_send_puts_isobatch_nm t s = sp "sw_%s_%d_send_puts_isobatch" t s
let sw_send_puts_isobatch c t s =
  let rmap_lmaps = P.rhs_lhs_of_stmt c.p s in
  let lmap, rmaps = P.lhs_map_of_stmt c.p s, P.rhs_maps_of_stmt c.p s in
  let t_args = args_of_t_as_vars_with_v c t in
  mk_global_fn (sw_send_puts_isobatch_nm t s) (args_of_t_with_v c t) [] @@
    if null rmap_lmaps then mk_cunit else
    (* for isobatch, we need to track the exact ips sending to other ips *)
    mk_block @@
      [mk_clear_all sw_send_stmt_bitmap.id] @
      (* route allows us to know how many nodes send data from rhs to lhs *)
      List.map (fun rmap ->
        let route_key, route_pat_idx = P.key_pat_from_bound c.p c.route_indices s rmap in
        R.route_lookup c rmap
          (mk_cint rmap::route_key) (mk_cint route_pat_idx) @@
          (* common block
             for each destination, we mark up the sources *)
          (let update_and_send ip_src ip_dest =
             mk_block [
              mk_insert send_put_bitmap.id [mk_var ip_dest];
              mk_update_at_with send_put_isobatch_map_id (mk_var ip_dest) @@
                mk_lambda' send_put_isobatch_map_e @@
                  mk_block [
                    mk_insert "bitmap" [mk_var ip_src];
                    mk_update_at_with "inner" (mk_var ip_src) @@
                      mk_lambda' send_put_isobatch_inner.e @@
                        mk_insert_block "inner2" [mk_cint rmap];
                    tup_of_e send_put_isobatch_map_e;
                  ];
              (* buffer the trig args if needed *)
              C.buffer_trig_header_if_needed (mk_var "vid") t ip_dest t_args;

              (* if we haven't sent this stmt, buffer it *)
              mk_if
                (mk_is_member' sw_send_stmt_bitmap.id @@ mk_var ip_dest)
                  mk_cunit @@
                  mk_block [
                    debug_run_test_tuples @@
                      C.buffer_for_send (rcv_stmt_isobatch_name_of_t t s) ip_dest [];
                    (* mark this ip *)
                    mk_insert sw_send_stmt_bitmap.id [mk_var ip_dest];
                  ]
             ]
          in

          (* optimized route - check if we can do special 1:1 optimized routing *)
          if is_opt_route_stmt_map c s rmap then
            (* we need to isolate each of the bound params separately *)
            let idx_rmaps = insert_index_snd ~first:1 @@ P.nonempty_rmaps_of_stmt c.p s in
            let m_idx_in_s = List.assoc rmap idx_rmaps in
            let bound_params = insert_index_fst @@ bound_params_of_stmt c s in
            let single_rmap = List.length idx_rmaps = 1 in
            let swallow_f f e = if single_rmap then e else f e in
            (* @idx is the number of bound param in the trigger
               @m_idx is the location of the variable in the map *)
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
                mk_case_ns (mk_lookup (swallow_f (mk_subscript m_idx_in_s) @@
                                      mk_snd @@ mk_var "lkup")
                            [mk_var ip.id; mk_cunknown]) "lkup2"
                  (* do nothing if we're not in the ds *)
                  mk_cunit @@
                  (* otherwise add all dest ips to result bitmap *)
                  mk_iter_bitmap ~idx:ip2.id
                    (update_and_send ip.id ip2.id) @@
                    mk_snd @@ mk_var "lkup2")
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
                    (update_and_send "ip" "ip2")
                    K3S.shuffle_bitmap.id
                  ])
                  R.route_bitmap.id)
      ) rmaps

(* {ip} for fetches *)
let rcv_fetch_header_bitmap = create_ds "rcv_fetch_header_bitmap" t_bitset

(* for isobatches, we need to create a batch_fetch message and make decisions at the batch level *)
let sw_send_rhs_fetches_nm t s = sp "sw_%s_%d_send_rhs_fetches" t s
let sw_send_rhs_fetches c t s =
  (* to do this in batched form, we iterate over the send_fetch bitmap and immediately buffer
     the results. since we accumulate over many possible statements and don't do any
     inermediate materialization, we can't automatically
     know when to send the send_fetch header, and that comes from the bitmap *)
    let rmaps = P.rhs_maps_of_stmt c.p s in
    let t_args = args_of_t_as_vars_with_v c t in

    mk_global_fn (sw_send_rhs_fetches_nm t s) (["is_isobatch", t_bool]@args_of_t_with_v c t) [] @@
    if null rmaps then mk_cunit else
      let m_tags = List.filter (fun ti -> str_suffix "_id" ti.tag && ti.tag_typ = Ds false) c.poly_tags in
      let rmap_tags = filter_map (fun ti ->
          let m = P.map_id_of_name c.p @@ str_drop_end (String.length "_id") ti.tag in
          if List.mem m rmaps then Some(m, ti.tag) else None) m_tags in
      mk_block @@
      [mk_clear_all rcv_fetch_header_bitmap.id] @
     (* fill in global data structure from route *)
      (List.map
        (fun (rmap, stag) ->
          let key, pat_idx = P.key_pat_from_bound c.p c.route_indices s rmap in
          R.route_lookup c rmap (mk_cint rmap::key) (mk_cint pat_idx) @@
          mk_iter_bitmap'
            (mk_block [
              prof_property D.prof_tag_fetch_route @@ ProfFetchRoute("vid",
                K3N.string_of_expr (mk_cint s),
                K3N.string_of_expr (mk_tuple @@ mk_cint rmap::key),
                K3N.string_of_expr (mk_apply' (K3R.route_for ~bound:true c.p rmap) (mk_cint rmap::key)),
                "ip");
              (* if we haven't sent to this ip yet, add a rcv_fetch header *)
                mk_if (mk_not @@ mk_is_member' rcv_fetch_header_bitmap.id @@ mk_var "ip")
                  (mk_block [
                    (* mark the send_fetch_bitmap *)
                    mk_insert rcv_fetch_header_bitmap.id [mk_var "ip"];
                    (* buffer the trig args if needed *)
                    C.buffer_trig_header_if_needed (mk_var "vid") t "ip" t_args;
                    debug_run_test_tuples @@
                        C.buffer_for_send (rcv_fetch_isobatch_name_of_t t s) "ip" []
                    ]
                  )
                  mk_cunit;
                (* buffer the map id *)
                debug_run_test_tuples @@
                  C.buffer_for_send ~wr_bitmap:false stag "ip" []
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
                C.buffer_trig_header_if_needed (mk_var "vid") t "ip" t_args;
                debug_run_test_tuples @@ C.buffer_for_send do_complete_t "ip" []
              ])
            K3Route.route_bitmap.id
      ]


let sw_send_fetch_isobatch_next_vid = create_ds "sw_send_fetch_isobatch_next_vid" @@ mut t_vid

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
  mk_let ["first_vid"] (C.next_vid @@ mk_var "batch_id") @@
  mk_block @@ List.flatten (List.map (fun s ->
    let has_rhs = P.rhs_maps_of_stmt c.p s <> [] in
    (if has_rhs then [mk_apply' clear_send_put_isobatch_map_nm []] else []) @
    [
      (* execute all statements *)
      mk_let ["next_vid"]
        (mk_poly_fold
          (mk_lambda4' ["vid", t_vid] p_tag p_idx p_off @@
            mk_block [
              mk_poly_at_with' (drop_insert t) @@
                mk_lambda' t_args @@
                  mk_block @@
                    (* true: is_isobatch *)
                    if has_rhs then
                      [mk_clear_all D.send_trig_header_bitmap.id;
                       mk_apply' (sw_send_rhs_fetches_nm t s) @@ mk_ctrue::call_args;
                       mk_apply' (sw_send_puts_isobatch_nm t s) call_args;
                      ] else
                      [ mk_clear_all D.send_trig_header_bitmap.id;
                        mk_apply' (sw_send_rhs_completes_nm t s) call_args
                      ]
                  ;
              C.next_vid (mk_var "vid")
            ])
          (mk_var "first_vid") @@
          mk_var "poly_queue") @@
      mk_assign sw_send_fetch_isobatch_next_vid.id (mk_var "next_vid")
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
         mk_block [
           prof_property D.prof_tag_send_put
             @@ ProfSendPut("batch_id", soi s, "ip", "count");
           debug_run_test_tuples @@
             C.buffer_for_send (rcv_put_isobatch_name_of_t t s) "ip" [mk_var "count"]
         ])
        send_put_bitmap.id)
  ) ss) @
  (* return next vid *)
  [mk_var sw_send_fetch_isobatch_next_vid.id]

(* clear the map of sending trig args. Done on a per-batch basis *)
let sw_clear_send_trig_args_map_nm = "sw_clear_send_trig_args_map"
let sw_clear_send_trig_args_map =
  mk_global_fn sw_clear_send_trig_args_map_nm [] [] @@
  mk_block [
    mk_iter_bitmap'
      (mk_update_at_with send_trig_args_map.id (mk_var "ip") @@
        mk_lambda' send_trig_args_map.e @@
          mk_block [
            mk_clear_all send_trig_args_inner.id;
            mk_var send_trig_args_inner.id
          ])
      send_trig_args_bitmap.id;
    mk_clear_all send_trig_args_bitmap.id;
  ]

