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
 *)

open Util
open K3.AST
open K3Helpers
open K3Dist
open ProgInfo
open K3Route
open K3Shuffle

module G = K3Global
module M = ModifyAst
module U = K3Util

exception ProcessingFailed of string;;

(* global trigger names needed for generated triggers and sends *)
let send_fetch_name_of_t p trig_nm = trig_nm^"_send_fetch"
let rcv_fetch_name_of_t p trig_nm = trig_nm^"_rcv_fetch"
let rcv_put_name_of_t p trig_nm = trig_nm^"_rcv_put"
let send_push_name_of_t p trig_nm stmt_id map_id =
  trig_nm^"_send_push_s"^string_of_int stmt_id^"_m_"^map_name_of p map_id
let rcv_push_name_of_t p trig_nm stmt_id map_id =
  trig_nm^"_rcv_push_s"^string_of_int stmt_id^"_m_"^map_name_of p map_id
let send_corrective_name_of_t p map_id = 
  map_name_of p map_id^"_send_correctives"
let do_complete_name_of_t p trig_nm stmt_id =
  trig_nm^"_do_complete_s"^string_of_int stmt_id
let filter_corrective_list_name = "filter_corrective_list"
let rcv_corrective_name_of_t p trig_nm stmt_id map_id =
  trig_nm^"_rcv_corrective_s"^string_of_int stmt_id^"_m_"^map_name_of p map_id
let do_corrective_name_of_t p trig_nm stmt_id map_id =
  trig_nm^"_do_corrective_s"^string_of_int stmt_id^"_m_"^map_name_of p map_id

(* log, buffer names *)
let log_write_for p trig_nm = "log_write_"^trig_nm (* varies with bound *)
let log_get_bound_for p trig_nm = "log_get_bound_"^trig_nm 
let log_read_geq = "log_read_geq" (* takes vid, returns (trig, vid)list >= vid *)
(* adds the delta to all subsequent vids following in the buffer so that non
 * delta computations will be correct. Must be atomic ie. no other reads of the
 * wrong buffer value can happen *)
let add_delta_to_buffer_for_map p map_id = 
  "add_delta_to_buffer_"^map_name_of p map_id

(* foreign functions *)
let hash_addr = "hash_addr"
let foreign_hash_addr = mk_foreign_fn hash_addr t_addr t_int
let declare_foreign_functions p = foreign_hash_addr::[]

(* global data structures --------- *)

(* vid counter used to assign vids *)
let vid_counter_name = "__vid_counter__"
let vid_counter = mk_var vid_counter_name
let vid_counter_t = wrap_tset @: t_int_mut

(* stmt_cntrs - (vid, stmt_id, counter) *)
let stmt_cntrs_name = "__stmt_cntrs__"
let stmt_cntrs = mk_var stmt_cntrs_name

(* names for log *)
let log_for_t t = "log_"^t
let log_master = "log__master"

let declare_global_vars p ast =
  (* vid_counter to generate vids. 
   * We use a singleton because refs aren't ready *)
  let vid_counter_code = 
    mk_global_val_init vid_counter_name vid_counter_t @:
    mk_singleton vid_counter_t @: mk_cint 1 in

  let global_map_decl_code =
    M.modify_map_decl_ast p ast in

  (* stmt counters, used to make sure we've received all msgs *)
  let stmt_cntrs_type = wrap_tset_mut @: wrap_ttuple_mut 
      [t_vid_mut; t_int_mut; t_int_mut] in
  let stmt_cntrs_code = mk_global_val stmt_cntrs_name stmt_cntrs_type in
  (* structures used for logs *)
  let log_structs_code =
    let log_master_code = mk_global_val
      log_master @:
      wrap_tset @: wrap_ttuple [t_vid; t_trig_id] in
    let log_struct_code_for t = mk_global_val
      (log_for_t t) @:
      wrap_tset @: 
        wrap_ttuple @: extract_arg_types @: args_of_t_with_v p t
    in 
    let log_structs = for_all_trigs p log_struct_code_for in
    log_master_code::log_structs
  in
  vid_counter_code ::
  global_map_decl_code @
  stmt_cntrs_code :: 
  log_structs_code

(* global functions *)
(* most of our global functions come from the shuffle/route code *)
let declare_global_funcs partmap p = 
  let global_vid_ops = 
      mk_global_vid_op vid_eq VEq ::
      mk_global_vid_op vid_neq VNeq ::
      mk_global_vid_op vid_lt VLt ::
      mk_global_vid_op vid_gt VGt ::
      mk_global_vid_op vid_leq VLeq ::
      mk_global_vid_op vid_geq VGeq ::
      [] in
  (* log_write *)
  let log_write_code t = mk_global_fn 
    (log_write_for p t)
    (args_of_t_with_v p t)
    [t_unit] @:
    mk_block
      [mk_insert (mk_var log_master) @: (* write to master log *)
        mk_tuple [mk_var "vid"; mk_cint (trigger_id_for_name p t)]
      ;
      mk_insert (mk_var @: log_for_t t) @: (* write to trigger_specific log *)
        mk_tuple @: args_of_t_as_vars_with_v p t
      ]
  in
  (* log_get_bound -- necessary since each trigger has different args *)
  let log_get_bound_code t = 
    let pat_tuple = args_of_t_with_v p t in
    let pat_unknown = List.map (fun (id,_) -> match id with
                                  | "vid" -> mk_var "vid"
                                  | _     -> mk_cunknown
                               ) pat_tuple
    in mk_global_fn
      (log_get_bound_for p t)
      ["vid", t_vid]
      (arg_types_of_t_with_v p t) @:
      mk_peek @: mk_slice (mk_var @: log_for_t t) @: mk_tuple pat_unknown
  in
  (* log_read_geq -- get list of (t,vid) >= vid *)
  let log_read_geq_code = mk_global_fn
    log_read_geq
    ["vid", t_vid]
    [wrap_tset @: wrap_ttuple [t_vid; t_trig_id]] @:
    mk_filtermap
      (mk_lambda 
        (wrap_args ["vid2", t_vid; "trig", t_trig_id]) @:
        v_geq (mk_var "vid2") @: mk_var "vid"
      )
      (mk_lambda
        (wrap_args ["vid2", t_vid; "trig", t_trig_id]) @:
        mk_tuple [mk_var "vid2"; mk_var "trig"]
      ) @:
      mk_var log_master
 in
  (* add_delta_to_buffer -- add delta to vid and all future vids *)
  let add_delta_to_buffer_code map =
    let map_name = map_name_of p map in
    let val_type = list_last @: map_types_for p map in
    let val_name = list_last @: extract_arg_names @: map_ids_types_for p map in
    let types_v = map_types_with_v_for p map in
    let ids_types_v = map_ids_types_with_v_for p map in
    (* make ids and type list for arguments *)
    let ids_types_arg = map_ids_types_for ~prefix:"__arg_" p map in
    let arg_val_id = list_last @: extract_arg_names @: ids_types_arg in
    let ids_types_arg_v = map_ids_types_add_v ~vid:"vid_arg" ids_types_arg in
    let ids_types_v2 = map_ids_types_with_v_for ~vid:"vid2" p map in
    let ids_no_val = extract_arg_names @: map_ids_types_no_val_for p map in
    let val_result_id = "val_result" in

    mk_global_fn (add_delta_to_buffer_for_map p map)
      ["vid", t_vid; "delta_tuples", wrap_tset @: wrap_ttuple types_v]
      [t_unit] @:
      mk_iter (* loop over vids >= in the map *)
        (mk_lambda (wrap_args ids_types_v) @:
          mk_iter (* loop over entries in the delta_tuples arg *)
            (mk_lambda (wrap_args ids_types_arg_v) @:
              mk_let val_result_id val_type
                (mk_add
                  (mk_var arg_val_id) @:
                  mk_var val_name
                ) @:
              mk_update
                (mk_var map_name)
                (mk_tuple @: ids_to_vars @: extract_arg_names @: ids_types_v) @:
                mk_tuple @: ids_to_vars @: map_ids_add_v @:
                  ids_no_val@[val_result_id]
            ) @:
            mk_var "delta_tuples"
        ) @:
        mk_filtermap (* filter all vids >= given vid *)
          (mk_lambda (wrap_args ids_types_v2) @:
            v_geq (mk_var "vid2") (mk_var "vid")
          )
          (mk_id types_v) @:
          mk_var map_name
  in
  global_vid_ops @
  [log_read_geq_code] @
  for_all_maps p add_delta_to_buffer_code @
  for_all_trigs p log_write_code @
  for_all_trigs p log_get_bound_code @
  gen_shuffle_route_code p partmap


(* ---- start of protocol code ---- *)

(* Trigger that's called once, on system init *)
let on_init_trig p =
  mk_code_sink "on_init" (wrap_args ["x", t_int]) [] @:
    mk_block
      [mk_iter 
        (mk_lambda 
          (wrap_args K3Global.peers_id_type) @:
          mk_apply (mk_var K3Ring.add_node_name) @: 
            mk_tuple @: ids_to_vars K3Global.peers_ids
        ) @:
          mk_var K3Global.peers_name]

(* The start trigger inserts a vid into each message *)
let start_trig p t =
  mk_code_sink t (wrap_args @: args_of_t p t) [] @:
    mk_let "vid" t_vid
      (mk_tuple [
        mk_cint 0; (* epoch not implemented yet *)
        mk_peek vid_counter;
        mk_apply (mk_var hash_addr) G.me_var
      ]) @:
      mk_block [
         mk_send 
           (mk_ctarget(send_fetch_name_of_t p t)) G.me_var @: 
           mk_tuple @: args_of_t_as_vars_with_v p t;
         mk_update vid_counter (mk_peek vid_counter) @:
           mk_add (mk_cint 1) (mk_peek vid_counter)
        ]

let send_fetch_trig p trig_name =
  let send_fetches_of_rhs_maps  =
    let s_rhs_maps = s_and_over_stmts_in_t p rhs_maps_of_stmt trig_name in
    if null s_rhs_maps then []
    else
    [mk_iter
      (mk_lambda 
        (wrap_args ["ip", t_addr; 
          "stmt_map_ids", wrap_tset @: wrap_ttuple [t_stmt_id; t_map_id]]
        ) @:
        mk_send 
          (mk_ctarget (rcv_fetch_name_of_t p trig_name))
          (mk_var "ip") @:
          mk_tuple @:
            mk_var "stmt_map_ids"::
            args_of_t_as_vars_with_v p trig_name
      ) @:
      mk_gbagg
        (mk_lambda (* Grouping function *)
          (wrap_args ["stmt_id", t_stmt_id; "map_id", t_map_id; "ip", t_addr])
          (mk_var "ip")
        )
        (mk_assoc_lambda (* Agg function *)
          (wrap_args ["acc", wrap_tset @: wrap_ttuple [t_stmt_id; t_map_id]])
          (wrap_args 
            ["stmt_id", t_stmt_id; "map_id", t_map_id; "ip", t_addr]) @:
          mk_combine
            (mk_var "acc") @:
            mk_singleton 
              (wrap_tset @: wrap_ttuple [t_stmt_id; t_map_id]) @:
              mk_tuple [mk_var "stmt_id";mk_var "map_id"]
          
        ) 
        (mk_empty @: wrap_tset @: wrap_ttuple [t_stmt_id; t_map_id])
        (* [] *) @:
        List.fold_left
          (fun acc_code (stmt_id, rhs_map_id) ->
            let route_fn = route_for p rhs_map_id in
            let key = partial_key_from_bound p stmt_id rhs_map_id in
            mk_combine
              (mk_map 
                (mk_lambda (wrap_args["ip", t_addr]) @:
                  mk_tuple @:
                    [mk_cint stmt_id; mk_cint rhs_map_id; 
                      mk_var "ip"]
                ) @:
                mk_apply 
                  (mk_var route_fn) @:
                  mk_tuple key
              )
              acc_code
          )
          (mk_empty @: wrap_tset @: wrap_ttuple [t_stmt_id; t_map_id; t_addr])
          s_rhs_maps
    ]
in
let send_completes_for_stmts_with_no_fetch =
  let s_no_rhs = stmts_without_rhs_maps_in_t p trig_name in
  if null s_no_rhs then []
  else
    List.fold_left
      (fun acc_code (stmt_id, lhs_map_id, do_complete_trig_name) -> 
        let route_fn = route_for p lhs_map_id in
        let key = partial_key_from_bound p stmt_id lhs_map_id in
          acc_code@
          [mk_iter 
            (mk_lambda (wrap_args["ip", t_addr]) @:
              mk_send
                (mk_ctarget(do_complete_trig_name))
                (mk_var "ip") @:
                mk_tuple @: args_of_t_as_vars_with_v p trig_name
            ) @:
            mk_apply (mk_var route_fn) @: mk_tuple key
          ]
      ) 
      [] @:
      List.map 
        (fun stmt_id -> 
          (stmt_id, lhs_map_of_stmt p stmt_id, 
          do_complete_name_of_t p trig_name stmt_id)
        )
        s_no_rhs
in
let send_puts =
  (* send puts
   * count is generated by counting the number of messages going to a
   * specific IP *)
  mk_iter
    (mk_lambda 
      (wrap_args ["ip", t_addr; 
        "stmt_id_cnt_list", wrap_tset @: wrap_ttuple [t_stmt_id; t_int]]) @:
      mk_send
        (mk_ctarget(rcv_put_name_of_t p trig_name))
        (mk_var "ip") @:
        mk_tuple @: mk_var "stmt_id_cnt_list"::
          args_of_t_as_vars_with_v p trig_name
    ) @:
    mk_gbagg
      (mk_assoc_lambda (* grouping func -- assoc because of gbagg tuple *)
        (wrap_args ["ip", t_addr; "stmt_id", t_stmt_id])
        (wrap_args ["count", t_int]) @:
        mk_var "ip"
      )
      (mk_assoc_lambda (* agg func *)
        (wrap_args ["acc", wrap_tset @: wrap_ttuple [t_stmt_id; t_int]])
        (wrap_args 
          ["ip_and_stmt_id", wrap_ttuple [t_addr; t_stmt_id]; "count", t_int]
        ) @:
        mk_apply (* break up because of the way inner gbagg forms tuples *)
          (mk_lambda (wrap_args ["ip", t_addr; "stmt_id", t_stmt_id]) @:
            mk_combine
              (mk_var "acc") @:
              mk_singleton
                (wrap_tset @: wrap_ttuple [t_stmt_id; t_int]) @:
                mk_tuple [mk_var "stmt_id"; mk_var "count"]
          ) @:
          mk_var "ip_and_stmt_id"
      )
      (mk_empty @: wrap_tset @: wrap_ttuple [t_stmt_id; t_int]) @:
      mk_gbagg (* inner gba *)
        (mk_lambda (* group func *)
          (wrap_args ["ip", t_addr; "stmt_id", t_stmt_id]) @:
          mk_tuple [mk_var "ip"; mk_var "stmt_id"]
        )
        (mk_assoc_lambda (* agg func *)
          (wrap_args ["acc", t_int]) 
          (wrap_args ["ip", t_addr; "stmt_id", t_stmt_id]) @:
          mk_add
            (mk_var "acc") @:
            mk_cint 1
        )
        (mk_cint 0) @: (* [] *)
        List.fold_left
          (fun acc_code (stmt_id, (rhs_map_id, lhs_map_id)) ->
            let shuffle_fn = find_shuffle stmt_id rhs_map_id lhs_map_id in
            let key = partial_key_from_bound p stmt_id lhs_map_id in
            (* we need the types for creating empty rhs tuples *)
            let rhs_map_types = map_types_with_v_for p rhs_map_id in
            mk_combine
              acc_code @:
              mk_map
                (mk_lambda
                  (wrap_args  ["ip", t_addr;
                    "tuples", wrap_tset @: wrap_ttuple rhs_map_types]
                  ) @:
                  mk_tuple [mk_var "ip"; mk_cint stmt_id]
                ) @:
                mk_apply
                  (mk_var shuffle_fn) @:
                  mk_tuple @:
                      (mk_tuple key)::
                      [mk_empty @: wrap_tset @: wrap_ttuple rhs_map_types]@
                      [mk_cbool true]
          )
          (mk_empty @: wrap_tset @: wrap_ttuple [t_addr; t_stmt_id]) @:
          s_and_over_stmts_in_t p rhs_lhs_of_stmt trig_name
in
(* Actual SendFetch function *)
mk_code_sink
  (send_fetch_name_of_t p trig_name)
  (wrap_args (args_of_t_with_v p trig_name))
  [] @: (* locals *)
  mk_block @:
    send_fetches_of_rhs_maps@
    send_completes_for_stmts_with_no_fetch@ 
    [send_puts]
   
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
let rcv_fetch_trig p trig =
  mk_code_sink
    (rcv_fetch_name_of_t p trig)
    (wrap_args @: ("stmts_and_map_ids", 
      wrap_tset @: wrap_ttuple [t_stmt_id; t_map_id])::
      args_of_t_with_v p trig
    )
    [] @: (* locals *)
    mk_block
      [mk_apply
        (mk_var @: log_write_for p trig) @:
        mk_tuple @: args_of_t_as_vars_with_v p trig
      ;
      (* invoke generated send pushes.$ *)
      mk_iter
        (mk_lambda
          (wrap_args ["stmt_id", t_stmt_id; "map_id", t_map_id]) @:
          (* this send is not polymorphic. every fetch trigger expects
           * the same set of bound variables. *)
          List.fold_right 
            (fun stmt acc_code -> mk_if
              (mk_eq
                (mk_var "stmt_id") @:
                mk_cint stmt
              )
              (List.fold_right 
                (fun map_id acc_code2 -> mk_if
                  (mk_eq
                    (mk_var "map_id") @:
                    mk_cint map_id
                  )
                  (mk_send (* send to local send push trigger *)
                    (mk_ctarget @:
                      send_push_name_of_t p trig stmt map_id)
                    G.me_var @:
                    mk_tuple @: args_of_t_as_vars_with_v p trig
                  )
                  acc_code2
                )
                (rhs_maps_of_stmt p stmt) @:
                mk_cunit (* zero: do nothing but really exception *)
              )
              acc_code
            )
            (stmts_of_t p trig) @:
            mk_cunit (* really want exception here *)
        ) @:
        mk_var "stmts_and_map_ids"
      ]

(* Receive Put trigger
 * --------------------------------------- *
 * Update the statement counters with the received values
 *)
let rcv_put_trig p trig_name =
mk_code_sink
  (rcv_put_name_of_t p trig_name)
  (wrap_args @:
    ("stmt_id_cnt_list", wrap_tset @: wrap_ttuple [t_stmt_id; t_int])::
    args_of_t_with_v p trig_name
  )
  [] @:
  let part_pat = ["vid", t_vid; "stmt_id", t_stmt_id] in
  let counter_pat = ["count", t_int] in
  let full_pat = part_pat @ counter_pat in
  let full_types = wrap_ttuple @: extract_arg_types full_pat in
  let part_pat_as_vars = ids_to_vars @: extract_arg_names part_pat in
  let query_pat = mk_tuple @: part_pat_as_vars @ [mk_cunknown] in
  mk_iter
    (mk_lambda
      (wrap_args ["stmt_id", t_stmt_id; "count", t_int]) @:
      mk_if (* do we already have a tuple for this? *)
        (mk_has_member stmt_cntrs query_pat full_types)
        (mk_update (* really an error -- shouldn't happen. Raise exception? *)
          stmt_cntrs
          (mk_peek @: mk_slice stmt_cntrs query_pat) @:
          mk_tuple @: part_pat_as_vars@[mk_var "count"]
        ) @:
        mk_insert
          stmt_cntrs @:
          mk_tuple @: part_pat_as_vars@[mk_var "count"]
    ) @:
    mk_var "stmt_id_cnt_list"


(* Trigger_send_push_stmt_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 * Produces a list of triggers.
 * We're parametrized by stmt and map_id because we save repeating the
 * processing on the switch side to figure out which maps (in which stmts) we
 * have locally
 *)
let send_push_stmt_map_trig p trig_name = 
  List.fold_left
    (fun acc_code (stmt_id, (rhs_map_id, lhs_map_id)) ->
      let rhs_map_types = map_types_with_v_for p rhs_map_id in 
      let rhs_map_name = map_name_of p rhs_map_id in
      let shuffle_fn = find_shuffle stmt_id rhs_map_id lhs_map_id in
      let partial_key = partial_key_from_bound p stmt_id lhs_map_id in
      let slice_key = 
        mk_var "vid" :: slice_key_from_bound p stmt_id rhs_map_id in
      acc_code@
      [mk_code_sink 
        (send_push_name_of_t p trig_name stmt_id rhs_map_id)
        (wrap_args @: args_of_t_with_v p trig_name)
        [] @: (* locals *)
          mk_iter
            (mk_lambda 
              (wrap_args 
                ["ip",t_addr;"tuples",wrap_tset @: wrap_ttuple rhs_map_types]
              ) @:
              mk_send
                (mk_ctarget @:
                  rcv_push_name_of_t p trig_name stmt_id rhs_map_id)
                (mk_var "ip") @:
                mk_tuple @: mk_var "tuples"::args_of_t_as_vars_with_v p trig_name
            ) @:
            mk_apply
              (mk_var shuffle_fn) @:
              mk_tuple
                (mk_tuple partial_key::
                  (mk_slice 
                    (mk_var rhs_map_name) @:
                    mk_tuple @: slice_key
                  )::[mk_cbool false]
                )
      ] (* trigger *)
    ) (* fun *)
    [] @:
    s_and_over_stmts_in_t p rhs_lhs_of_stmt trig_name


(* rcv_push_trig
 * --------------------------------------
 * Receive a push at a node
 * Also called for virtual pushes: local data transfers to allow tracking of 
 * present data w/ counters params can be moved to the put statement, but it's
 * a good reminder to have it here 
 * A later optimization could be lumping maps between statements in a trigger *)

let rcv_push_trig p trig_name = 
List.fold_left
  (fun acc_code (stmt_id, read_map_id) ->
    let map_name = map_name_of p read_map_id in
    let tuple_types = map_types_with_v_for p read_map_id in
    (* remove value from tuple so we can do a slice *)
    let tuple_pat = tuple_make_pattern tuple_types in
    let reduced_pat = slice_pat_take (List.length tuple_pat - 1) tuple_pat in
    let reduced_code = mk_rebuild_tuple "tuple" tuple_types reduced_pat in
    acc_code@
    [mk_code_sink 
      (rcv_push_name_of_t p trig_name stmt_id read_map_id)
      (wrap_args @: ("tuples", wrap_tset @: wrap_ttuple @: tuple_types)::
        args_of_t_with_v p trig_name
      )
      [] @: (* locals *)
      (* save the tuples *)
      mk_block
        [mk_iter
          (mk_lambda
            (wrap_args ["tuple", wrap_ttuple tuple_types]) @:
            mk_if
              (mk_has_member 
                (mk_var map_name)
                reduced_code @:
                wrap_ttuple tuple_types
              )
              (mk_update
                (mk_var map_name)
                (mk_peek @: mk_slice
                  (mk_var map_name)
                  reduced_code
                ) @:
                mk_var "tuple"
              ) @:
              mk_insert
                (mk_var map_name) @:
                mk_var "tuple"
          ) @:
          mk_var "tuples"
         ;
         (* check statment counters to see if we can process *)
         let part_pat = ["vid", t_vid; "stmt_id", t_stmt_id] in
         let counter_pat = ["count", t_int] in
         let full_pat = part_pat @ counter_pat in
         let full_types = wrap_ttuple @: extract_arg_types full_pat in
         let part_pat_as_vars = [mk_var "vid"; mk_cint stmt_id] in
         let query_pat = mk_tuple @: part_pat_as_vars @ [mk_cunknown] in
         let stmt_cntrs_slice = mk_slice stmt_cntrs query_pat in
         mk_if (* check if the counter exists *)
           (mk_has_member stmt_cntrs query_pat full_types)
           (mk_block
             [mk_update
               stmt_cntrs
               (mk_peek stmt_cntrs_slice) @: (* oldval *)
               mk_let_many (* newval *)
                 full_pat
                 (mk_peek stmt_cntrs_slice) @:
                 mk_tuple @:
                   part_pat_as_vars @ 
                   [mk_sub (mk_var "count") (mk_cint 1)]
               
             ;mk_if (* check if the counter is 0 *)
               (mk_eq
                 (mk_peek stmt_cntrs_slice) @:
                 mk_tuple @: part_pat_as_vars @ [mk_cint 0]
               ) 
               (* Send to local do_complete *)
               (mk_send
                 (mk_ctarget @: 
                   do_complete_name_of_t p trig_name stmt_id)
                 G.me_var @:
                 mk_tuple @: args_of_t_as_vars_with_v p trig_name
               ) @:
               mk_cunit (* do nothing *)
             ]
           ) @:
           mk_update (* else: no value in the counter *)
             stmt_cntrs
             (mk_peek stmt_cntrs_slice) @:
             (* Initialize if the push arrives before the put. *)
             mk_tuple @: part_pat_as_vars @ [mk_cint(-1)]
         ]
    ]
  )
  [] @: (* empty code *)
  s_and_over_stmts_in_t p rhs_maps_of_stmt trig_name
    
 
(* list of trig, stmt with a map on the rhs that's also on the lhs. These are
 * the potential corrective maps *)
let maps_potential_corrective p =
  let lhs_maps = ListAsSet.uniq @: for_all_stmts p @: lhs_map_of_stmt p in
  let rhs_maps = ListAsSet.uniq @: List.flatten @: 
    for_all_stmts p @: rhs_maps_of_stmt p in
  ListAsSet.inter lhs_maps rhs_maps

(* send_corrective_trigs
 * --------------------------------------
 * Generate code to send data for all possible correctives.
 * When we compute, we produce lhs results. These might be used in other
 * statements as rhs inputs. We need to find those places, and then carry out
 * the shuffling of the changed data for those statements ie from rhs to lhs 
 *)
let send_corrective_trigs p =
  (* for a given lhs map which we just changed, find all statements containing the
   * same map on the rhs *)
  let send_correctives map_id = 
    (* list of (trig,stmt) that have this map_id on the rhs *)
    let trigs_stmts_with_matching_rhs_map =
        List.filter
          (fun (trig, stmt_id) -> stmt_has_rhs_map p stmt_id map_id) @:
          List.flatten @:
            for_all_trigs p 
              (fun trig -> 
                List.map (fun stmt -> (trig, stmt)) @: stmts_of_t p trig) in
    (* predefined K3 list of stmts with rhs maps (ie. the above) *)
    let trig_stmt_k3_list = 
      let types = wrap_tset @: wrap_ttuple [t_trig_id; t_stmt_id] in
      List.fold_left 
        (fun acc_code (trig, stmt_id) -> 
          mk_combine 
            (mk_singleton types @:
              mk_tuple @: [mk_cint (trigger_id_for_name p trig); 
               mk_cint stmt_id]
            )
            acc_code
        )
        (mk_empty types)
        trigs_stmts_with_matching_rhs_map
    in
    match trigs_stmts_with_matching_rhs_map with [] -> [] | _ ->
    (* we transfer with vid so we don't need to strip *)
    let tuple_types = wrap_ttuple @: map_types_with_v_for p map_id in
    [mk_code_sink 
      (send_corrective_name_of_t p map_id)
      (wrap_args ["vid", t_vid; "delta_tuples", wrap_tset tuple_types])
      [] @:
      (* the corrective list tells us which statements were really executed *)
      mk_let "corrective_list" (* (vid * stmt_id) list *)
        (wrap_tset @: wrap_ttuple [t_vid; t_stmt_id])
        (mk_apply
          (mk_var filter_corrective_list_name) @:
          mk_tuple @:
            mk_var "vid"::[trig_stmt_k3_list] (* feed in list of possible stmts *)
        ) @:
        mk_iter  (* loop over corrective list *)
          (mk_lambda
            (wrap_args ["vid", t_vid; "stmt_id", t_stmt_id]) @:
            List.fold_left  
              (* loop over all possible read map matches *)
              (fun acc_code (target_trig, target_stmt) ->
                (* we already have the map vars for the rhs map in the
                 * tuples. Now we try to get some more for the lhs map *)
                let target_map = lhs_map_of_stmt p target_stmt in
                let key = partial_key_from_bound p target_stmt target_map in
                let shuffle_fn = find_shuffle target_stmt map_id target_map in
                mk_if (* if match, send data *)
                  (mk_eq
                    (mk_var "stmt_id") @:
                    mk_cint target_stmt
                  )
                  (mk_iter 
                    (mk_lambda 
                      (wrap_args ["ip", t_addr; "tuples", wrap_tset tuple_types]) @:
                      mk_send
                        (* we always send to the same map_id ie. the remote
                         * buffer of the same map we just calculated *)
                        (mk_ctarget (rcv_corrective_name_of_t p target_trig
                          target_stmt map_id)) 
                        (mk_var "ip") @:
                        mk_tuple [mk_var "vid"; mk_var "tuples"]
                    ) @:
                    (* get bound vars from log so we can calculate shuffle *)
                    mk_let_many 
                      (args_of_t_with_v p target_trig)
                      (mk_apply 
                        (mk_var @: log_get_bound_for p target_trig) @:
                        mk_var "vid"
                      ) @:
                      mk_apply
                        (mk_var shuffle_fn) @:
                        mk_tuple @:
                          mk_tuple key::
                          mk_var "delta_tuples"::
                          [mk_cbool false]
                  )
                  acc_code (* just another branch on the if *)
              )
              mk_cunit (* base case *)
              trigs_stmts_with_matching_rhs_map 
          ) @:
          mk_var "corrective_list"
    ]
  in
  List.flatten @: List.map send_correctives @: maps_potential_corrective p

 
let do_complete_trigs p ast trig_name =
let do_complete_trig stmt_id =
mk_code_sink (do_complete_name_of_t p trig_name stmt_id)
  (wrap_args @: args_of_t_with_v p trig_name)
  [] @: (* locals *)
    let lmap = lhs_map_of_stmt p stmt_id in
    let send_to = 
        if List.exists (fun m -> m = lmap) @: maps_potential_corrective p
        then Some(send_corrective_name_of_t p lmap)
        else None
    in
    M.modify_ast_for_s p ast stmt_id trig_name send_to
in
List.map (fun stmt -> do_complete_trig stmt) @: stmts_of_t p trig_name


(* get_corrective_list: 
 * ---------------------------------------
 * Return list of corrective statements we need to execute by checking
 * which statements were performed after time x that used a particular map 
 * The only reason for this function is that we may have the same stmt
 * needing to execute with different vids
 * Optimization TODO: check also by ranges within the map.
 *)
let filter_corrective_list = mk_global_fn filter_corrective_list_name
  (* (trigger_id, stmt_id) list *)
  ["vid", t_vid; "trig_stmt_list", wrap_tset @: wrap_ttuple
    [t_trig_id; t_stmt_id]
  ]
  [wrap_tset @: wrap_ttuple [t_vid; t_stmt_id]]
  (mk_sort (* sort so that early vids are first for performance *)
    (mk_flatten @: mk_map
      (mk_lambda 
        (wrap_args ["trig_id", t_trig_id; "stmt_id", t_stmt_id]) @:
        mk_filtermap
          (mk_lambda (wrap_args ["vid", t_vid; "trig_id2", t_trig_id]) @:
            mk_eq (mk_var "trig_id2") @: mk_var "trig_id"
          )
          (mk_lambda (wrap_args ["vid", t_vid; "trig_id2", t_trig_id]) @:
            mk_tuple [mk_var "vid"; mk_var "stmt_id"]
          ) @:
          mk_apply  (* list of triggers >= vid *)
            (mk_var log_read_geq) @: (* TODO *)
            mk_var "vid"  
      ) @:
      mk_var "trig_stmt_list"
    ) @:
    mk_assoc_lambda (* compare func *)
      (wrap_args ["vid1", t_vid; "stmt1", t_stmt_id])
      (wrap_args ["vid2", t_vid; "stmt2", t_stmt_id]) @:
      v_lt (mk_var "vid1") @: mk_var "vid2"
  )

    
(* receive_correctives:
 * ---------------------------------------
 * Function that gets called when a corrective message is received.
 * Receives bound variables and delta tuples for one map.  
 *
 * Analogous to rcv_push for non-correctives
 * Note: this can be rolled into the do_corrective function
 *)
let rcv_correctives_trig p trig_name = 
List.map
  (fun (stmt_id, map_id) ->
    mk_code_sink (rcv_corrective_name_of_t p trig_name stmt_id map_id)
      (wrap_args 
        ["vid", t_vid; 
        "delta_tuples", wrap_tset @: wrap_ttuple @: map_types_with_v_for p map_id]
      )
      [] @: (* locals *)
      mk_block
        (* accumulate delta for this vid and all following vids *)
        [mk_apply
          (mk_var @: add_delta_to_buffer_for_map p map_id) @:
          mk_tuple [mk_var "vid"; mk_var "delta_tuples"]
          ;
          (* only execute if we have all the updates *)
          mk_if
            (mk_eq
              (mk_peek @: mk_slice stmt_cntrs @:
                mk_tuple
                  [mk_var "vid"; mk_cint stmt_id; mk_cunknown]
              ) @: (* error if we get more than one result *)
              mk_tuple
                [mk_var "vid"; mk_cint stmt_id; mk_cint 0]
            )
            (* get bound vars from log *)
            (mk_let_many 
              (args_of_t_with_v p trig_name)
              (mk_apply 
                (mk_var @: log_get_bound_for p trig_name)
                (mk_var "vid")
              ) @:
              mk_send
                (mk_ctarget @: 
                  do_corrective_name_of_t p trig_name stmt_id map_id
                )
                G.me_var @:
                mk_tuple @: args_of_t_as_vars_with_v p trig_name @
                  [mk_var "delta_tuples"]
            ) @:
            mk_cunit (* else *)
        ]
  ) @:
  s_and_over_stmts_in_t p rhs_maps_of_stmt trig_name
;;

(* do corrective triggers *)
let do_corrective_trigs p ast trig_name =
List.map
  (fun (stmt_id, map_id) ->
    mk_code_sink 
      (do_corrective_name_of_t p trig_name stmt_id map_id)
      (wrap_args @: args_of_t_with_v p trig_name@
        ["delta_tuples", wrap_tset @: wrap_ttuple @: map_types_with_v_for p map_id]
      )
      [] @: (* locals *)
        (*mk_cunit*)
        let (args, ast) = M.modify_corr_ast p ast map_id stmt_id trig_name in
        let args_v = map_ids_types_add_v args in
        mk_iter (mk_lambda (wrap_args args_v) ast) @:
          mk_var "delta_tuples"
  ) @:
  s_and_over_stmts_in_t p rhs_maps_of_stmt trig_name

(* this is hardwired for the m3tok3 module, which produces demux triggers *)
let demux_trigs ast =
  let trigs = U.triggers_of_program ast in
  let demux_s = "demux_" in
  let demux_ts = 
    List.filter (fun c -> 
      String.sub (U.id_of_code c) 0 (String.length demux_s) = demux_s)
      trigs
  (* assume all demux are flow sinks too *)
  in List.map (fun c -> mk_no_anno @: Sink(c)) demux_ts

(* we take the existing default role and prepend it with a one-shot to
 * call out on-init function *)
let modified_roles ast = 
  let roles = List.filter (fun d -> U.is_role d || U.is_def_role d) ast in
  let def_role_id = U.id_of_role @: List.find U.is_def_role roles in
  let pred = fun d -> U.is_role d && U.id_of_role d = def_role_id in
  let def_role = List.find pred roles in 
  let other_roles = List.filter (not |- pred) roles in
  let (_, flow_prog) = U.decompose_role def_role in
  (mk_role def_role_id @:
    mk_const_stream "s_on_init" t_int [mk_cint 1] ::
    mk_bind "s_on_init" "on_init" ::
    mk_consume "s_on_init" :: flow_prog) :: other_roles

(* Generate all the code for a specific trigger *)
let gen_dist_for_t p ast trig =
    start_trig p trig::
    send_fetch_trig p trig::
    rcv_put_trig p trig::
    rcv_fetch_trig p trig::
    send_push_stmt_map_trig p trig@
    rcv_push_trig p trig@
    do_complete_trigs p ast trig@
    rcv_correctives_trig p trig@
    do_corrective_trigs p ast trig@
    []

(* Function to generate the whole distributed program *)
let gen_dist p partmap ast =
  (* because this uses state, need it initialized here *)
  (* TODO: change to not require state *)
  let global_funcs = declare_global_funcs partmap p in (* init shuffles *)
  let regular_trigs = List.flatten @:
    for_all_trigs p @: fun t -> gen_dist_for_t p ast t in
  let prog =
    declare_global_vars p ast @
    global_funcs @ (* maybe make this not order-dependent *)
    declare_foreign_functions p @
    filter_corrective_list ::  (* global func *)
    (mk_flow @:
      on_init_trig p::
      regular_trigs@
      send_corrective_trigs p@
      demux_trigs ast)::    (* per-map basis *)
      modified_roles ast in
  let foreign = List.filter (fun d -> U.is_foreign d) prog in
  let rest = List.filter (fun d -> not @: U.is_foreign d) prog in
  U.renumber_program_ids (foreign @ rest)

