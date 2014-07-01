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
 * data. Otherwise, the corrective will be erased.
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
module GC = GarbageCollection
module T = K3Typechecker

(* control whether gc code is emitted *)
let enable_gc = false

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

(* function to check index *)
let check_stmt_cntr_index = "check_and_update_stmt_cntr_index"

let declare_global_vars p ast =
  (* mapping of map_id to map_name and dimensionality *)
  let map_list =
    let t_map_list = wrap_tbag' [t_int; t_string ;t_int] in
    let content = P.for_all_maps p (fun i ->
      mk_tuple [mk_cint i;
                mk_cstring @: map_name_of p i;
                mk_cint @: List.length @: map_types_for p i]) in
    mk_global_val_init K3Dist.map_ids t_map_list @:
      k3_container_of_list t_map_list content
  in
  (* vid_counter to generate vids.
   * We use a singleton because refs aren't ready *)
  let vid_counter_code =
    mk_global_val_init vid_counter_name vid_counter_t @:
    mk_singleton vid_counter_t @: mk_cint 1 in

  (* epoch code *)
  let epoch_code =
    mk_global_val_init epoch_name epoch_t @:
    mk_singleton epoch_t @: mk_cint 0 in

  let global_map_decl_code = M.modify_map_decl_ast p ast in

  (* we need to make buffer versions of rhs maps based on their statement *)
  let map_buffers_decl_code =
    (* for all rhs, lhs map pairs *)
    let make_map_decl (stmt, map) =
      let map_name = P.buf_of_stmt_map_id p stmt map in
      mk_global_val map_name @:
        wrap_tind @: wrap_t_of_map' @: map_types_with_v_for p map
    in
    for_all_stmts_rhs_maps p make_map_decl
  in

  let stmt_cntrs_code = mk_global_val stmt_cntrs_name stmt_cntrs_type in
  (* structures used for logs *)
  let log_structs_code =
    let log_master_code = mk_global_val
      log_master @:
      wrap_tbag' [t_vid; t_trig_id; t_stmt_id]
    in
    let log_struct_code_for t = mk_global_val
      (log_for_t t) @:
      wrap_tbag' @: snd_many @: args_of_t_with_v p t
    in
    let log_structs = for_all_trigs p log_struct_code_for in
    log_master_code::log_structs
  in
  map_list ::
  vid_counter_code ::
  epoch_code ::
  global_map_decl_code @
  map_buffers_decl_code @
  stmt_cntrs_code ::
  (if enable_gc then
    GC.acks_code ::
    GC.vid_rcv_cnt_code GC.vid_rcv_cnt_name ::
    GC.vid_rcv_cnt_code GC.vid_rcv_cnt_2_name ::
    GC.vid_buf_code ::
    GC.vid_buf_2_code ::
    GC.min_max_acked_vid_code ::
    [GC.gc_vid_code]
  else []) @
  log_structs_code

(* global functions *)
(* most of our global functions come from the shuffle/route code *)
let declare_global_prims =
  let global_vid_ops =
      mk_global_vid_op vid_eq VEq ::
      mk_global_vid_op vid_neq VNeq ::
      mk_global_vid_op vid_lt VLt ::
      mk_global_vid_op vid_gt VGt ::
      mk_global_vid_op vid_leq VLeq ::
      mk_global_vid_op vid_geq VGeq ::
      []
  in
  global_vid_ops

let declare_global_funcs partmap p ast =
  (* log_master_write *)
  let log_master_write_code () = mk_global_fn
    log_master_write_nm
    log_master_id_t
    [t_unit] @:
    mk_insert (mk_var log_master) @: (* write to master log *)
      mk_tuple @:
        ids_to_vars @: fst_many log_master_id_t
  in
  (* log_write *)
  let log_write_code t = mk_global_fn
    (log_write_for p t)
    (args_of_t_with_v p t)
    [t_unit] @:
      (* write bound to trigger_specific log *)
      mk_insert (mk_var @: log_for_t t) @:
        mk_tuple @: args_of_t_as_vars_with_v p t
  in
  (* log_get_bound -- necessary since each trigger has different args *)
  let log_get_bound_code t =
    let pat_tuple = args_of_t_with_v p t in
    (* create a pattern for selecting vid alone *)
    let pat_unknown = List.map (fun (id, _) ->
      if id = "vid" then mk_var "vid" else mk_cunknown) pat_tuple
    in mk_global_fn
      (log_get_bound_for p t)
      ["vid", t_vid]
      (arg_types_of_t_with_v p t) @:
      mk_peek @: mk_slice' (mk_var @: log_for_t t) pat_unknown
  in
  (* log_read_geq -- get list of (t,vid) >= vid *)
  let log_read_geq_code = mk_global_fn
    log_read_geq
    ["vid2", t_vid]
    [wrap_tbag' @: snd_many log_master_id_t] @:
    mk_filtermap
      (* get only >= vids *)
      (mk_lambda'
        log_master_id_t @:
        v_geq (mk_var "vid") @: mk_var "vid2"
      )
      (mk_id @: snd_many log_master_id_t) @:
      mk_var log_master
  in
  (* check_stmt_cntr_indx *)
  (* Check to see if we should send a do_complete message *)
  let check_stmt_cntr_index_fn =
    let part_pat = list_drop_end 1 stmt_cntrs_id_t in
    let counter = fst @: hd @: list_take_end 1 stmt_cntrs_id_t in
    let full_types = snd_many stmt_cntrs_id_t in
    let part_pat_as_vars = ids_to_vars @: fst_many part_pat in
    let query_pat = part_pat_as_vars @ [mk_cunknown] in
    let stmt_cntrs_slice = mk_slice' stmt_cntrs query_pat in
    mk_global_fn
      check_stmt_cntr_index
      part_pat
      [t_bool] @: (* return whether we should send the do_complete *)
      mk_if (* check if the counter exists *)
        (mk_has_member' stmt_cntrs query_pat @: stmt_cntrs_wrap full_types)
        (mk_block
          [mk_update
            stmt_cntrs
            (mk_peek stmt_cntrs_slice) @: (* oldval *)
            mk_let_many (* newval *)
              stmt_cntrs_id_t
              (mk_peek stmt_cntrs_slice) @:
              mk_tuple @:
                part_pat_as_vars @
                [mk_sub (mk_var counter) (mk_cint 1)]

          ;mk_if (* check if the counter is 0 *)
            (mk_eq
              (mk_peek stmt_cntrs_slice) @:
              mk_tuple @: part_pat_as_vars @ [mk_cint 0]
            )
            (* Return true - send a do_complete *)
            (mk_cbool true)
            (mk_cbool false)
          ]
        ) @:
        mk_block
          [mk_insert (* else: no value in the counter *)
            stmt_cntrs @:
            (* Initialize if the push arrives before the put. *)
            mk_tuple @: part_pat_as_vars @ [mk_cint(-1)];
          mk_cbool false
          ]
  in

  (* add_delta_to_buffer *)
  (* this is the same procedure for both correctives and do_complete *
   * it consists of 2 parts:
    * 1. Initialize the given vid using the delta values
    * 2. Add to all next vids *)
  (* NOTE: this function assumes the initial value is always 0.
   * If we need another value, it must be handled via message from
   * m3tok3 *)
  let add_delta_to_buffer_code map_id =
    let func_name = add_delta_to_map p map_id in
    let delta_tuples_nm = "delta_tuples" in
    let ids_types_arg = map_ids_types_for ~prefix:"__arg_" p map_id in
    let ids_types_arg_v = map_ids_types_add_v ~vid:"vid_arg" ids_types_arg in
    let ids_types_v = map_ids_types_with_v_for p map_id in
    let types_v = snd_many ids_types_v in
    let t_col_v = wrap_t_of_map' types_v in
    let len_types_v = List.length types_v in
    let vars_v = ids_to_vars @: fst_many ids_types_v in
    let vars_arg_v = ids_to_vars @: fst_many ids_types_arg_v in
    let vars_no_val = list_drop_end 1 @:
      ids_to_vars @: fst_many @: map_ids_types_for p map_id in
    let vars_v_no_val = list_drop_end 1 vars_v in
    let vars_val = hd @: list_take_end 1 vars_v in
    let vars_arg_val = hd @: list_take_end 1 vars_arg_v in
    let vars_arg_no_v_no_val =
      ids_to_vars @: fst_many @: list_drop_end 1 ids_types_arg in
    let t_val = hd @: list_take_end 1 types_v in
    let id_val = hd @: list_take_end 1 @: fst_many @: ids_types_v in
    let lookup_value, update_value = "lookup_value", "update_value" in
    let corrective, target_map = "corrective", "target_map" in
    let update_vars = list_drop_end 1 vars_v @ [mk_var update_value] in
    let zero = match T.base_of t_val () with
      | TInt   -> mk_cint 0
      | TFloat -> mk_cfloat 0.
      | t      -> failwith @:
        "Unhandled value type "^K3PrintSyntax.string_of_base_type t
    in
    let tuple_projection =
      project_from_tuple types_v (mk_peek @: mk_var lookup_value)
        ~total:len_types_v
        ~choice:len_types_v
    in
    let regular_delta =
      mk_let lookup_value t_col_v
        (map_latest_vid_vals p (mk_deref @: mk_var target_map)
          (Some(vars_no_val @ [mk_cunknown])) map_id ~keep_vid:true) @:
        mk_let update_value t_val
          (* get either 0 to add or the value we read *)
          (mk_add (mk_var id_val) @:
            mk_if
              (mk_is_empty (mk_var lookup_value) t_col_v)
              zero
              tuple_projection) @:
          mk_insert
            (mk_deref @: mk_var target_map) @:
            mk_tuple update_vars
    in
    mk_global_fn func_name
      (* corrective: whether this is a corrective delta *)
      ([target_map, wrap_tind @: wrap_t_of_map' types_v;
        corrective, t_bool; "min_vid", t_vid;
        delta_tuples_nm, wrap_t_of_map' types_v])
      [t_unit] @:
      mk_block @:
        [mk_iter  (* loop over values in delta tuples *)
          (mk_lambda' ids_types_v @:
            (* this part is just for correctives:
             * We need to check if there's a value at the particular version id
             * If so, we must add the value directly *)
              mk_let lookup_value t_col_v
                (mk_if
                  (mk_var corrective)
                  (mk_slice' (mk_deref @: mk_var target_map) @:
                    vars_v_no_val @ [mk_cunknown]) @:
                  mk_empty @: wrap_t_of_map' types_v) @:
              mk_if
                (mk_not @: mk_is_empty (mk_var lookup_value) t_col_v)
                (* then just update the value *)
                (mk_let update_value t_val
                  (mk_add (mk_var id_val) @: tuple_projection) @:
                  mk_insert
                    (mk_deref @: mk_var target_map) @:
                    mk_tuple update_vars)
                (* else, if it's just a regular delta, read the frontier *)
                regular_delta) @:
          mk_var delta_tuples_nm
        ;
        (* add to future values *)
        mk_iter (* loop over values in the delta tuples *)
         (mk_lambda' ids_types_arg_v @:
           mk_let "filtered" (wrap_t_of_map' types_v)
             (mk_filtermap (* only greater vid for this part *)
               (mk_lambda' ids_types_v @:
                 mk_gt (mk_var "vid") @: mk_var "min_vid")
               (mk_id types_v) @:
               (* slice w/o vid and value *)
               mk_slice' (mk_deref @: mk_var target_map) @:
                 mk_cunknown::vars_arg_no_v_no_val@[mk_cunknown]) @:
              mk_iter
                (mk_lambda' ids_types_v @:
                  mk_update (mk_deref @: mk_var target_map) (mk_tuple vars_v) @:
                    mk_tuple @: vars_v_no_val@
                      [mk_add vars_val vars_arg_val]
                ) @:
                mk_var "filtered"
         ) @:
         mk_var delta_tuples_nm]
  in
  let global_inits =
    (* global initialization that should happen once *)
      mk_global_val_init "init" t_unit @:
        mk_block
          [mk_iter
            (mk_lambda'
              K3Global.peers_id_type @:
              (* only add to node list if role <> switch *)
              mk_if
              (mk_neq
                (mk_var K3Global.peers_role_name) (mk_cstring "switch"))
              (mk_apply (mk_var K3Ring.add_node_name) @:
                  mk_tuple @: ids_to_vars K3Global.peers_ids)
              mk_cunit
            ) @:
              mk_var K3Global.peers_name
        ]
  in
  log_read_geq_code ::
  check_stmt_cntr_index_fn ::
  List.map (fun (_,maps) ->
    add_delta_to_buffer_code @: hd maps)
    (uniq_types_and_maps p) @
  [log_master_write_code ()] @
  for_all_trigs p log_write_code @
  for_all_trigs p log_get_bound_code @
  gen_shuffle_route_code p partmap @
  [global_inits]


(* ---- start of protocol code ---- *)


(* The start trigger inserts a vid into each message *)
let start_trig ~force_correctives p t =
  let update_epoch =
    if force_correctives then
      (* force correctives by changing the epoch *)
      [mk_update epoch_var (mk_peek epoch_var) @:
        mk_apply (mk_var "mod") @:
          mk_tuple [mk_add (mk_peek epoch_var) @: mk_cint 1; mk_cint 2]]
    else []
  in
  mk_code_sink' t (args_of_t p t) [] @:
    mk_block @:
      update_epoch@
      [mk_let "vid" t_vid
         (mk_tuple [
           mk_peek epoch_var;
           mk_peek vid_counter;
           mk_apply (mk_var hash_addr) G.me_var]) @:
       mk_block @: [
         mk_send
           (mk_ctarget(send_fetch_name_of_t p t)) G.me_var @:
           mk_tuple @: args_of_t_as_vars_with_v p t;

        (* increase vid_counter *)
         mk_update vid_counter (mk_peek vid_counter) @:
           mk_add (mk_cint 1) (mk_peek vid_counter)] @

        (if enable_gc then
          (* start garbage collection every 10
          * TODO maybe need to change by GC every few seconds *)
          (* disable gc for now so we can test properly *)
          [mk_if
            (mk_eq
              (mk_apply
                (mk_var "mod") @:
                mk_tuple[mk_peek vid_counter;mk_cint 10])
              (mk_cint 0)) (*end eq*)
            (mk_send (* send to max_acked_vid_send to statr GC *)
              (mk_ctarget GC.max_acked_vid_send_trig_name)
              K3Global.me_var
              (mk_cint 1))
            mk_cunit]
        else [])
      ]

let send_fetch_trig p s_rhs_lhs s_rhs trig_name =
  let send_fetches_of_rhs_maps  =
    if null s_rhs then []
    else
    [mk_iter
      (mk_lambda'
        ["ip", t_addr;
          "stmt_map_ids", wrap_tbag' [t_stmt_id; t_map_id]] @:
        mk_send
          (mk_ctarget (rcv_fetch_name_of_t p trig_name))
          (mk_var "ip") @:
          mk_tuple @:
            mk_var "stmt_map_ids"::
            args_of_t_as_vars_with_v p trig_name
      ) @:
      mk_gbagg
        (mk_lambda' (* Grouping function *)
          ["stmt_id", t_stmt_id; "map_id", t_map_id; "ip", t_addr]
          (mk_var "ip")
        )
        (mk_assoc_lambda' (* Agg function *)
          ["acc", wrap_tbag' [t_stmt_id; t_map_id]]
          ["stmt_id", t_stmt_id; "map_id", t_map_id; "ip", t_addr] @:
          mk_combine
            (mk_var "acc") @:
            mk_singleton
              (wrap_tbag' [t_stmt_id; t_map_id]) @:
              mk_tuple [mk_var "stmt_id";mk_var "map_id"]

        )
        (mk_empty @: wrap_tbag' [t_stmt_id; t_map_id])
        (* [] *) @:
        List.fold_left
          (fun acc_code (stmt_id, rhs_map_id) ->
            let route_fn = route_for p rhs_map_id in
            let key = partial_key_from_bound p stmt_id rhs_map_id in
            mk_combine
              (mk_map
                (mk_lambda' ["ip", t_addr] @:
                  mk_tuple @:
                    [mk_cint stmt_id; mk_cint rhs_map_id;
                      mk_var "ip"]
                ) @:
                mk_apply
                  (mk_var route_fn) @:
                  mk_tuple @: (mk_cint rhs_map_id)::key
              )
              acc_code
          )
          (mk_empty @: wrap_tbag' [t_stmt_id; t_map_id; t_addr])
          s_rhs
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
            (mk_lambda' ["ip", t_addr] @:
              mk_send
                (mk_ctarget(do_complete_trig_name))
                (mk_var "ip") @:
                mk_tuple @: args_of_t_as_vars_with_v p trig_name
            ) @:
            mk_apply (mk_var route_fn) @:
              mk_tuple @: (mk_cint lhs_map_id)::key
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
  if null s_rhs_lhs then [] else
  let stmt_id_cnt_type = wrap_tbag' [t_stmt_id; t_int] in
  (* send puts
   * count is generated by counting the number of messages going to a
   * specific IP *)
  [mk_iter
    (mk_lambda'
      ["ip", t_addr; "stmt_id_cnt_list", stmt_id_cnt_type] @:
      mk_block @: [
        (* send rcv_put *)
        mk_send
          (mk_ctarget(rcv_put_name_of_t p trig_name))
          (mk_var "ip") @:
          mk_tuple @: G.me_var ::  mk_var "stmt_id_cnt_list"::
            args_of_t_as_vars_with_v p trig_name] @

        (if enable_gc then [
          (* insert a record into the switch ack log, waiting for ack*)
          mk_insert GC.switch_ack_log @:
                mk_tuple [mk_var "ip"; mk_var "vid"; mk_cbool false]
        ] else [])
    ) @:
    mk_gbagg
      (mk_assoc_lambda' (* grouping func -- assoc because of gbagg tuple *)
        ["ip", t_addr; "stmt_id", t_stmt_id]
        ["count", t_int] @:
        mk_var "ip"
      )
      (mk_assoc_lambda' (* agg func *)
        ["acc", stmt_id_cnt_type]
        ["ip_and_stmt_id", wrap_ttuple [t_addr; t_stmt_id]; "count", t_int] @:
        mk_let_many (* break up because of the way inner gbagg forms tuples *)
          ["ip", t_addr; "stmt_id", t_stmt_id]
          (mk_var "ip_and_stmt_id") @:
          mk_combine
            (mk_var "acc") @:
            mk_singleton
              stmt_id_cnt_type @:
              mk_tuple [mk_var "stmt_id"; mk_var "count"]
      )
      (mk_empty @: wrap_tbag' [t_stmt_id; t_int]) @:
      mk_gbagg (* inner gba *)
        (mk_lambda' (* group func *)
          ["ip", t_addr; "stmt_id", t_stmt_id; "count", t_int] @:
          mk_tuple [mk_var "ip"; mk_var "stmt_id"]
        )
        (mk_assoc_lambda' (* agg func *)
          ["acc", t_int]
          ["ip", t_addr; "stmt_id", t_stmt_id; "count", t_int] @:
          mk_add
            (mk_var "acc") @:
            mk_var "count"
        )
        (mk_cint 0) @: (* [] *)
        List.fold_left
          (fun acc_code (stmt_id, (rhs_map_id, lhs_map_id)) ->
            (* shuffle allows us to recreate the path the data will take from
             * rhs to lhs *)
            let shuffle_fn = find_shuffle stmt_id rhs_map_id lhs_map_id in
            let shuffle_key = partial_key_from_bound p stmt_id lhs_map_id in
            (* route allows us to know how many nodes send data from rhs to lhs
             * *)
            let route_fn = route_for p rhs_map_id in
            let route_key = partial_key_from_bound p stmt_id rhs_map_id in
            (* we need the types for creating empty rhs tuples *)
            let rhs_map_types = map_types_with_v_for p rhs_map_id in
            let tuple_types = wrap_t_of_map' rhs_map_types in
            mk_combine
              acc_code @:
              mk_let "sender_count" t_int
                (* count up the number of IPs received from route *)
                (mk_agg
                  (mk_lambda'
                    ["count", t_int; "ip", t_addr] @:
                    mk_add (mk_var "count") (mk_cint 1)
                  )
                  (mk_cint 0) @:
                  (mk_apply
                    (mk_var route_fn) @:
                      mk_tuple @: (mk_cint rhs_map_id)::route_key
                  )
                ) @:
              mk_map
                (mk_lambda'
                  ["ip", t_addr; "tuples", tuple_types] @:
                    mk_tuple
                      [mk_var "ip"; mk_cint stmt_id; mk_var "sender_count"]
                ) @:
                mk_apply
                  (mk_var shuffle_fn) @:
                  mk_tuple @:
                      shuffle_key@
                      [mk_empty tuple_types;
                       mk_cbool true]
          )
          (mk_empty @: wrap_tbag' [t_addr; t_stmt_id; t_int]) @:
          s_rhs_lhs
  ]
in
(* Actual SendFetch function *)
mk_code_sink'
  (send_fetch_name_of_t p trig_name)
  (args_of_t_with_v p trig_name)
  [] @: (* locals *)
  mk_block @:
    send_completes_for_stmts_with_no_fetch@
    send_puts@
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
let rcv_fetch_trig p trig =
  mk_code_sink'
    (rcv_fetch_name_of_t p trig)
    (("stmts_and_map_ids",
      wrap_t_of_map' [t_stmt_id; t_map_id])::
      args_of_t_with_v p trig)
    [] @: (* locals *)
    mk_block [
      (* save the bound variables for this trigger so they're available later *)
      mk_apply
        (mk_var @: log_write_for p trig) @:
        mk_tuple @:
          args_of_t_as_vars_with_v p trig
      ;
      (* invoke generated send pushes. *)
      mk_iter
        (mk_lambda'
          ["stmt_id", t_stmt_id; "map_id", t_map_id] @:
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
            (stmts_with_rhs_maps_in_t p trig) @:
            mk_cunit (* really want exception here *)
        ) @:
        mk_var "stmts_and_map_ids"
    ]

(* Receive Put trigger
 * --------------------------------------- *
 * Update the statement counters with the received values
 * If the counter is 0, we need to call do_complete
 *)
let rcv_put_trig p trig_name =
mk_code_sink'
  (rcv_put_name_of_t p trig_name)
  (["sender_ip",t_addr; "stmt_id_cnt_list", wrap_t_of_map' [t_stmt_id; t_int]]@
      (args_of_t_with_v p trig_name))
  [] @:
  let part_pat = ["vid", t_vid; "stmt_id", t_stmt_id] in
  let counter_pat = ["count", t_int] in
  let full_pat = part_pat @ counter_pat in
  let full_types = snd_many full_pat in
  let part_pat_as_vars = ids_to_vars @: fst_many part_pat in
  let query_pat = part_pat_as_vars @ [mk_cunknown] in
  mk_block @: [
    mk_iter
      (mk_lambda'
        ["stmt_id", t_stmt_id; "count", t_int] @:
        mk_if (* do we already have a tuple for this? *)
          (mk_has_member' stmt_cntrs query_pat @: stmt_cntrs_wrap full_types)
          (mk_let_deep' ["_", t_unit; "_", t_unit; "old_count", t_int]
            (mk_peek @: mk_slice' stmt_cntrs query_pat) @:
            (* update the count *)
            mk_let "new_count" t_int
              (mk_add (mk_var "old_count") @: mk_var "count") @:
            mk_block [
              mk_update
                stmt_cntrs
                (mk_tuple @:
                  part_pat_as_vars@[mk_var "old_count"]) @:
                mk_tuple @:
                  part_pat_as_vars@[mk_var "new_count"]
              ;
              mk_if
                (mk_eq (mk_var "new_count") @: mk_cint 0)
                (* we need to create a possible send for each statement in the trigger *)
                (List.fold_left (fun acc_code stmt_id ->
                  mk_if
                    (mk_eq (mk_var "stmt_id") @: mk_cint stmt_id)
                    (mk_send
                      (mk_ctarget @:
                        do_complete_name_of_t p trig_name stmt_id)
                      G.me_var @:
                      mk_tuple @: args_of_t_as_vars_with_v p trig_name
                    ) @:
                    acc_code
                  )
                  (mk_cunit) @:
                  P.stmts_of_t p trig_name
                ) @:
                mk_cunit
            ]
          ) @:
          mk_insert
            stmt_cntrs @:
            mk_tuple @: part_pat_as_vars@[mk_var "count"]
      ) @:
        mk_var "stmt_id_cnt_list"] @

    (if enable_gc then
    (* ack send for GC *)
    [mk_send (mk_ctarget "ack_send")  G.me_var @:
      mk_tuple [mk_var "sender_ip"; mk_var "vid"]]
    else [])


(* Trigger_send_push_stmt_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 * Produces a list of triggers.
 * We're parametrized by stmt and map_id because we save repeating the
 * processing on the switch side to figure out which maps (in which stmts) we
 * have locally
 *)
let send_push_stmt_map_trig p s_rhs_lhs trig_name =
  List.fold_left
    (fun acc_code (stmt_id, (rhs_map_id, lhs_map_id)) ->
      let rhs_map_types = map_types_with_v_for p rhs_map_id in
      let rhs_map_name = map_name_of p rhs_map_id in
      let shuffle_fn = find_shuffle stmt_id rhs_map_id lhs_map_id in
      let partial_key = partial_key_from_bound p stmt_id lhs_map_id in
      let slice_key = slice_key_from_bound p stmt_id rhs_map_id in
      acc_code@
      [mk_code_sink'
        (send_push_name_of_t p trig_name stmt_id rhs_map_id)
        (args_of_t_with_v p trig_name)
        [] @: (* locals *)
        mk_block [
          (* save this particular statement execution in the master log
           * Note that we need to do it here to make sure nothing
           * else can stop us before we send the push *)
          mk_apply
            (mk_var log_master_write_nm) @:
            mk_tuple [mk_var "vid"; mk_cint @:
              trigger_id_for_name p trig_name; mk_cint stmt_id]
          ;
          mk_iter
            (mk_lambda'
              ["ip",t_addr;"tuples", wrap_t_of_map' rhs_map_types] @:
              mk_send
                (mk_ctarget @:
                  rcv_push_name_of_t p trig_name stmt_id rhs_map_id)
                (mk_var "ip") @:
                mk_tuple @: mk_var "tuples"::args_of_t_as_vars_with_v p trig_name
            ) @:
            mk_apply
              (mk_var shuffle_fn) @:
              mk_tuple @:
                partial_key@
                (* we need the latest vid data that's less than the current vid *)
                [K3Dist.map_latest_vid_vals p (mk_deref @: mk_var rhs_map_name)
                  (some slice_key) rhs_map_id ~keep_vid:true;
                mk_cbool true]
          ]
      ] (* trigger *)
    ) (* fun *)
    []
    s_rhs_lhs


(* rcv_push_trig
 * --------------------------------------
 * Receive a push at a node
 * Also called for virtual pushes: local data transfers to allow tracking of
 * present data w/ counters params can be moved to the put statement, but it's
 * a good reminder to have it here
 * We write to specific buffer maps to prevent mixing of buffer and non-buffer
 * data, which can cause confusion when the time comes to compute.
 * A later optimization could be lumping maps between statements in a trigger *)

let rcv_push_trig (p:P.prog_data_t) s_rhs trig_name =
List.fold_left
  (fun acc_code (stmt_id, read_map_id) ->
    let rbuf_name = buf_of_stmt_map_id p stmt_id read_map_id in
    let tuple_types = map_types_with_v_for p read_map_id in
    (* remove value from tuple so we can do a slice *)
    let tuple_pat = tuple_make_pattern tuple_types in
    let reduced_pat = slice_pat_take (List.length tuple_pat - 1) tuple_pat in
    let reduced_code = mk_rebuild_tuple "tuple" tuple_types reduced_pat in
    acc_code@
    [mk_code_sink'
      (rcv_push_name_of_t p trig_name stmt_id read_map_id)
      (("tuples", wrap_t_of_map' tuple_types)::
        args_of_t_with_v p trig_name)
      [] @: (* locals *)
      (* save the tuples *)
      mk_block
        (* save the bound variables for this vid. This is necessary for both the
         * sending and receiving nodes, which is why we're also doing it here *)
        [mk_apply
          (mk_var @: log_write_for p trig_name) @:
            mk_tuple @:
              args_of_t_as_vars_with_v p trig_name
        ;
         mk_iter
          (mk_lambda'
            ["tuple", wrap_ttuple tuple_types] @:
            mk_if
              (mk_has_member
                (mk_deref @: mk_var rbuf_name)
                reduced_code @:
                wrap_t_of_map' tuple_types
              )
              (mk_update
                (mk_deref @: mk_var rbuf_name)
                (mk_peek @: mk_slice
                  (mk_deref @: mk_var rbuf_name)
                  reduced_code
                ) @:
                mk_var "tuple"
              ) @:
              mk_insert
                (mk_deref @: mk_var rbuf_name) @:
                mk_var "tuple"
          ) @:
          mk_var "tuples"
         ;
         (* check and update statment counters to see if we should send a do_complete *)
         mk_if
           (mk_apply (mk_var check_stmt_cntr_index) @:
             mk_tuple [mk_var "vid"; mk_cint stmt_id]
           )
           (* Send to local do_complete *)
           (mk_send
             (mk_ctarget @:
               do_complete_name_of_t p trig_name stmt_id)
             G.me_var @:
             mk_tuple @: args_of_t_as_vars_with_v p trig_name
           )
           mk_cunit
         ]
    ]
  )
  [] (* empty code *)
  s_rhs


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
 * This is a very coarse-grained method of finding correctives needed.
 *
 * We must make sure that we only send the delta once. However, all vids can be sent
 * for execution together.
 *)
let send_corrective_trigs p =
  (* for a given lhs map which we just changed, find all statements containing the
   * same map on the rhs. This is crude, but should be enough for a first try *)
  let send_correctives map_id =
    (* list of (trig,stmt) that have this map_id on the rhs *)
    let trigs_stmts_with_matching_rhs_map =
        List.filter
          (fun (trig, stmt_id) -> stmt_has_rhs_map p stmt_id map_id) @:
          List.flatten @:
            for_all_trigs p
              (fun trig ->
                List.map (fun stmt -> trig, stmt) @: stmts_of_t p trig) in
    (* turn the ocaml list into a static k3 list *)
    let trig_stmt_k3_list =
      let types = wrap_tbag' [t_trig_id; t_stmt_id] in
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
    let tuple_types = map_types_with_v_for p map_id in
    let t_tuple_bag = wrap_t_of_map' tuple_types in
    let tuple_type = wrap_ttuple tuple_types in
    [mk_code_sink'
      (send_corrective_name_of_t p map_id)
      ["corrective_vid", t_vid; "delta_tuples", t_tuple_bag]
      [] @:
      (* the corrective list tells us which statements were fetched
       * from us and when *)
      mk_let "corrective_list" (* (stmt_id * vid list) list *)
        (wrap_tbag' [t_stmt_id; t_vid_list])
        (mk_apply
          (mk_var filter_corrective_list_name) @:
          mk_tuple (* feed in list of possible stmts *)
            [mk_var "corrective_vid"; trig_stmt_k3_list]
        ) @:
      mk_iter  (* loop over corrective list and act for specific statements *)
        (mk_lambda'
          ["stmt_id", t_stmt_id; "vid_list", t_vid_list] @:
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
                  mk_cint target_stmt)
                (mk_iter
                  (mk_assoc_lambda'
                    ["ip", t_addr]
                    ["vid_send_list", t_vid_list; "tuple", t_tuple_bag] @:
                    mk_send
                      (* we always send to the same map_id ie. the remote
                        * buffer of the same map we just calculated *)
                      (mk_ctarget @: rcv_corrective_name_of_t p target_trig
                        target_stmt map_id)
                      (mk_var "ip") @:
                      (* we send the vid where the update is taking place as
                       * well as the vids of the sites where corrections must
                       * be calculated *)
                      mk_tuple
                        [mk_var "corrective_vid"; mk_var "vid_send_list";
                        mk_var "tuple"]
                  ) @:
                  mk_gbagg
                    (* group by the IPs. We want all the vids we'll need to
                     * execute, and we concatenate the tuples since we're
                     * adding deltas anyway, so if there's no stale value,
                     * there's no harm done *)
                    (mk_lambda'
                      ["ip", t_addr; "vid", t_vid; "tuples", t_tuple_bag] @:
                        mk_var "ip"
                    )
                    (mk_assoc_lambda'
                      ["acc_vid", t_vid_list; "acc_tuples", t_tuple_bag]
                      ["ip", t_addr; "vid", t_vid; "tuples", t_tuple_bag] @:
                        mk_tuple
                          [mk_combine (mk_var "acc_vid") @:
                            mk_singleton t_vid_list (mk_var "vid");
                            (* eliminate dups *)
                            mk_fst_many [tuple_type; t_unit] @:
                              mk_gbagg
                                (mk_lambda' ["tuple", tuple_type] @: mk_var "tuple")
                                (mk_lambda' ["_", t_unit; "_", tuple_type] mk_cunit)
                                mk_cunit @:
                                mk_combine (mk_var "acc_tuples") @: mk_var "tuples"]
                    )
                    (mk_tuple [mk_empty t_vid_list; mk_empty t_tuple_bag]) @:
                    mk_flatten @: mk_map
                      (mk_lambda' ["vid", t_vid] @:
                        (* get bound vars from log so we can calculate shuffle *)
                        mk_let_many
                          (args_of_t_with_v p target_trig)
                          (mk_apply
                            (mk_var @: log_get_bound_for p target_trig) @:
                            mk_var "vid"
                          ) @:
                        (* insert vid into the ip, tuples output of shuffle *)
                        mk_map (* (ip * vid * tuple list) list *)
                          (mk_lambda'
                            ["ip", t_addr; "tuples", t_tuple_bag] @:
                              mk_tuple
                                [mk_var "ip"; mk_var "vid"; mk_var "tuples"]
                          ) @:
                          mk_apply
                            (mk_var shuffle_fn) @:
                            mk_tuple @:
                              key @
                               [mk_var "delta_tuples";
                               mk_cbool false] (* (ip * tuple list) list *)
                      ) @:
                      mk_var "vid_list"
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
    mk_code_sink' (do_complete_name_of_t p trig_name stmt_id)
      (args_of_t_with_v p trig_name)
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


(*
 * Return list of corrective statements we need to execute by checking
 * which statements were performed after time x that used a particular map
 * The only reason for this function is that we may have the same stmt
 * needing to execute with different vids
 * Optimization TODO: check also by ranges within the map ie. more fine grain
 *)
let filter_corrective_list =
  let trig_stmt_list_t = wrap_tbag' [t_trig_id; t_stmt_id] in
  mk_global_fn filter_corrective_list_name
  (* (trigger_id, stmt_id) list *)
  ["request_vid", t_vid; "trig_stmt_list", trig_stmt_list_t]
  [wrap_tbag' [t_stmt_id; t_vid_list]]
  @:
  mk_let "log_entries"
    (wrap_tbag' @: snd_many log_master_id_t)
    (mk_apply (* list of triggers >= vid *)
      (mk_var log_read_geq) @: mk_var "request_vid") @:
  (* group the list by stmt_ids *)
  mk_gbagg
    (mk_lambda' ["_", t_vid; "stmt_id", t_stmt_id] @: mk_var "stmt_id")
    (mk_assoc_lambda'
      ["vid_list", t_vid_list]
      ["vid", t_vid; "_", t_stmt_id] @:
      mk_combine (mk_var "vid_list") (mk_singleton t_vid_list @: mk_var "vid")
    )
    (mk_empty t_vid_list) @:
    let vid_stmt_id_t  = ["vid", t_vid; "stmt_id", t_stmt_id] in
    let vid_stmt_col_t = wrap_tlist' @: snd_many vid_stmt_id_t in
    mk_sort (* sort so early vids are generally sent out first *)
      (* get a list of vid, stmt_id pairs *)
      (** this is reall a map, but make it a fold to convert to a list *)
      (mk_agg
        (mk_assoc_lambda'
          ["acc", vid_stmt_col_t]
          log_master_id_t @:
          (* convert to vid, stmt *)
          mk_combine
            (mk_var "acc") @:
            mk_singleton
              vid_stmt_col_t @:
              mk_tuple @: ids_to_vars @: fst_many vid_stmt_id_t)
        (mk_empty vid_stmt_col_t) @:
        mk_var "log_entries"
      ) @:
      mk_assoc_lambda' (* compare func *)
        ["vid1", t_vid; "stmt1", t_stmt_id]
        ["vid2", t_vid; "stmt2", t_stmt_id] @:
        v_lt (mk_var "vid1") @: mk_var "vid2"


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
let rcv_correctives_trig p s_rhs trig_name =
List.map
  (fun (stmt_id, rmap) ->
    let buf_map_nm = buf_of_stmt_map_id p stmt_id rmap in
    mk_code_sink' (rcv_corrective_name_of_t p trig_name stmt_id rmap)
      ["vid", t_vid;
        "compute_vids", t_vid_list;
        "delta_tuples", wrap_t_of_map' @: map_types_with_v_for p rmap]
      [] @: (* locals *)
      mk_block
        (* accumulate delta for this vid and all following vids. This is a very
         * sensitive point in the protocol and it's essential this only be done
         * once for a given corrective *)
        [mk_apply
          (mk_var @: add_delta_to_map p rmap) @:
            (* pass the map indirection, false=not corrective *)
            mk_tuple [mk_var buf_map_nm; mk_cbool false; mk_var "vid"; mk_var "delta_tuples"]
          ;
          (* for every computation vid, only execute if we have all the updates *)
          mk_iter
            (mk_lambda' ["compute_vid", t_vid] @:
              mk_if
                (mk_eq
                  (mk_peek @:
                    (* We'll crash if we can't find the right stmt here, but this is
                    * desired behavior since it makes sure a corrective can't happen
                    * without an earlier push/put *)
                    mk_slice' stmt_cntrs
                      [mk_var "compute_vid"; mk_cint stmt_id; mk_cunknown]
                  ) @: (* error if we get more than one result *)
                  mk_tuple
                    [mk_var "compute_vid"; mk_cint stmt_id; mk_cint 0]
                )
                (* get bound vars from log *)
                (mk_let_many
                  (args_of_t_with_v p trig_name)
                  (mk_apply
                    (mk_var @: log_get_bound_for p trig_name)
                    (mk_var "compute_vid")
                  ) @:
                  mk_send
                    (mk_ctarget @:
                      do_corrective_name_of_t p trig_name stmt_id rmap
                    )
                    G.me_var @:
                    mk_tuple @: args_of_t_as_vars_with_v ~vid:"compute_vid" p trig_name @
                      [mk_var "delta_tuples"]
                ) @:
                mk_cunit (* else *)
            ) @:
            mk_var "compute_vids"
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
let do_corrective_trigs p s_rhs ast trig_name corrective_maps =
  let do_corrective_trig (stmt_id, map_id) =
    mk_code_sink' (do_corrective_name_of_t p trig_name stmt_id map_id)
      (args_of_t_with_v p trig_name@
        ["delta_tuples",
          wrap_t_of_map' @: map_types_with_v_for p map_id])
      [] @: (* locals *)
        let lmap = lhs_map_of_stmt p stmt_id in
        let send_to =
          if List.exists ((=) lmap) corrective_maps
          then Some(send_corrective_name_of_t p lmap)
          else None
        in
        let args, ast =
          M.modify_corr_ast p ast map_id stmt_id trig_name send_to in
        let args_v = map_ids_types_add_v ~vid:"_" args in
        mk_iter (mk_lambda' args_v ast) @:
          mk_var "delta_tuples"
  in
  List.map do_corrective_trig s_rhs

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
let roles_of ast =
  List.filter (fun d -> U.is_role d || U.is_def_role d) ast

(* Generate all the frontier functions *)
let emit_frontier_fns p =
  (* get a representative map of each type *)
  let fns = List.map (hd |- snd) @: uniq_types_and_maps p in
  List.map (frontier_fn p) fns

(* Generate all the code for a specific trigger *)
let gen_dist_for_t ~force_correctives p ast trig corr_maps =
  (* (stmt_id,rhs_map_id)list *)
  let s_rhs =
    s_and_over_stmts_in_t p rhs_maps_of_stmt trig in
  (* stmts that can be involved in correctives *)
  let s_rhs_corr = List.filter (fun (s,map) -> List.mem map corr_maps) s_rhs in
  (* (stmt_id,rhs_map_id,lhs_map_id)list *)
  let s_rhs_lhs =
    s_and_over_stmts_in_t p rhs_lhs_of_stmt trig
  in
  start_trig ~force_correctives p trig::
  send_fetch_trig p s_rhs_lhs s_rhs trig::
  (if null s_rhs then []
  else
    rcv_put_trig p trig::
    [rcv_fetch_trig p trig])@
  send_push_stmt_map_trig p s_rhs_lhs trig@
  rcv_push_trig p s_rhs trig@
  do_complete_trigs p ast trig@
  rcv_correctives_trig p s_rhs_corr trig@
  do_corrective_trigs p s_rhs_corr ast trig corr_maps@
  []

(* Function to generate the whole distributed program *)
(* @param force_correctives Attempt to create dist code that encourages correctives *)
let gen_dist ?(force_correctives=false) p partmap ast =
  (* because this uses state, need it initialized here *)
  (* TODO: change to not require state *)
  let global_funcs = declare_global_funcs partmap p ast in
  let potential_corr_maps = maps_potential_corrective p in
  let regular_trigs = List.flatten @:
    for_all_trigs p @: fun t ->
      gen_dist_for_t ~force_correctives p ast t potential_corr_maps in
  let prog =
    declare_global_vars p ast @
    declare_global_prims @
    emit_frontier_fns p @
    global_funcs @ (* maybe make this not order-dependent *)
    declare_foreign_functions p @
    filter_corrective_list ::  (* global func *)
    (mk_flow @:
      (if enable_gc then GC.triggers p ast else []) @
      regular_trigs @
      send_corrective_trigs p @
      demux_trigs ast)::    (* per-map basis *)
      roles_of ast in
  let foreign = List.filter (fun d -> U.is_foreign d) prog in
  let rest = List.filter (fun d -> not @: U.is_foreign d) prog in
  snd @: U.renumber_program_ids (foreign @ rest)

