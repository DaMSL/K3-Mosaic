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

open GenCommon
open GenDispatch
open GenFetchPut
open GenPush
open GenComplete
open GenCorrective
open GenWarmup

(**** protocol code ****)

(*  TODO: unused? *)
(* receive isobatch stmt list *)
(* we buffer the vid/stmts in the helper ds *)
let nd_rcv_stmt_isobatch =
  mk_global_fn nd_rcv_stmt_isobatch_nm ["stmt_id", t_stmt_id; "vid", t_vid] [] @@
  mk_block [
      mk_assign isobatch_stmt_helper_has_content.id mk_ctrue;
      mk_insert isobatch_stmt_helper_bitmap_id [mk_var "stmt_id"];
      mk_update_at_with isobatch_stmt_helper_id (mk_var "stmt_id") @@
        mk_lambda' isobatch_stmt_helper_e @@
          mk_insert_block "inner2" [mk_var "vid"]
  ]

(*  TODO: unused? *)
let nd_send_batch_push_bitmap =
  let init = mk_map (mk_lambda' unknown_arg mk_cfalse) @@ mk_var D.my_peers.id in
  create_ds ~init "nd_send_batch_push_bitmap" @@ t_bool_vector

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

(* dummy do_reads to handle typechecking etc *)
let do_reads c =
  let m_nm_ts = List.map (fun m -> m, P.map_name_of c.p m, P.map_ids_types_for c.p m) @@
    P.get_maps_with_keys c.p in
  List.map (fun (m, m_nm, id_ts) ->
      let k_t, v_t = list_split (-1) @@ snd_many id_ts in
      let ts = [wrap_ttuple k_t; wrap_ttuple v_t] in
      mk_global_fn ("doRead"^m_nm) ["addr", t_addr; "s", t_string] ts @@
        default_value_of_t @@ wrap_ttuple ts)
    m_nm_ts

(* Used by translation to NewK3 to easily convert between types *)
let flatteners c =
  let l = snd_many @@ D.uniq_types_and_maps ~uniq_indices:false c in
  List.map (fun (t, maps) ->
    let map_ds = map_ds_of_id ~global:true ~vid:false c (hd maps) in
    let pat = pat_of_ds ~flatten:true ~drop_vid:true ~expr:(mk_var "tuple") map_ds in
    mk_global_fn ("flatten_"^strcatmap ~sep:"_" K3PrintSyntax.string_of_type t)
    ["tuple", snd @@ unwrap_tcol map_ds.t] [wrap_ttuple t] @@
    mk_tuple @@ fst_many pat) l

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
    [sw_send_stmt_bitmap;
     send_put_bitmap;
     send_put_ip_map c.p;
     send_put_isobatch_map c;
     rcv_fetch_header_bitmap;
     send_push_bitmap;
     send_push_isobatch_bitmap;
     send_push_cntrs;
     send_push_isobatch_map c;
     nd_send_isobatch_push_sent;
     send_corrective_bitmap;
     send_corrective_ip_map;
     sw_total_demux_ctr;
     sw_sent_demux_ctr;
     sw_demux_poly_queues;
     sw_demux_poly_target;
     sw_demux_last_trig;
     sw_demux_last_action;
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
     sw_send_fetch_isobatch_next_vid;
     sw_token_has_data;
     sw_token_current_batch_id;
     sw_warmup_push_bitmap;
    ]

let declare_global_funcs c ast =
  flatteners c @
  nd_log_master_write ::
  (P.for_all_trigs ~sys_init:true ~delete:c.gen_deletes c.p @@ nd_log_write c) @
  (P.for_all_trigs ~sys_init:true ~delete:c.gen_deletes c.p @@ nd_log_get_bound c) @
  (if c.gen_correctives then [nd_update_corr_map; nd_filter_corrective_list] else []) @
  D.functions c @
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
  let sre = t = "system_ready_event" in
  let fns1 =
    (List.flatten @@ List.map (fun s ->
         [sw_send_rhs_fetches c t s] @
          (if c.gen_single_vid then [sw_send_puts_single_vid c t s] else []) @
          (if sre then [] else [sw_send_puts_isobatch c t s])
       ) s_r) @
    (List.map (sw_send_rhs_completes c t) s_no_r) @
    (if c.gen_single_vid then List.map (sw_send_fetch_single_vid_fn c t) ss else []) @
    (if c.gen_correctives then
       List.flatten @@ List.map (fun s -> nd_do_corrective_fns c t s ast) s_r else []) @
    (List.flatten @@ List.map (nd_send_push_stmt_map_trig c t) s_r) @
    (List.flatten @@ List.map (nd_isobatch_send_push_stmt_map_trig c t) s_r)
  in
  (* split for scope *)
  let fns2 =
    nd_do_complete_fns c ast t @
    (List.flatten @@ List.map (fun s ->
        (* no isobatch functions for system_ready_event *)
        (if sre then [] else
        [
         nd_rcv_put_isobatch_trig c t s;
         nd_rcv_fetch_isobatch_do_push c t s;
         nd_rcv_fetch_isobatch_trig c t s;
         nd_rcv_push_isobatch_trig c t s;
        ]) @
        (if c.gen_single_vid then
           [nd_rcv_put_single_vid_trig c t s;
            nd_rcv_fetch_single_vid_trig c t s] else []) @
        [nd_rcv_push_trig c t s]
       )
        s_r) @
    (List.map (fun s -> nd_do_complete_trigs c t s) s_no_r) @
    (if sre then [] else [sw_send_fetches_isobatch c t]) @
    [
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
(* @warmup_p: warmup program *)
let gen_dist ?(gen_deletes=true)
             ?(gen_correctives=true)
             ?(gen_single_vid=true)
             ?(use_opt_route=true)
             ~stream_file
             ~map_type
             ~(agenda_map: mapping_t)
             p warmup_p ast =
  let sys_init =
    try ignore(P.find_trigger p "system_ready_event"); true
    with Not_found | P.Bad_data _ -> false in

  (* if we need sys_init, we must generate single_vid *)
  let gen_single_vid = gen_single_vid || sys_init in

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
      gen_single_vid;
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
    clear_send_push_ds ::
    clear_send_push_isobatch_ds ::
    fns1 @
    [nd_send_isobatch_push_meta c;
     nd_exec_buffered_fetches c;    (* depends: send_push *)
     nd_complete_stmt_cntr_check c; (* depends: exec_buffered *)
     nd_check_stmt_cntr_index c;
     nd_rcv_fetch_isobatch_buffer_push;
     nd_rcv_stmt_isobatch;
    ] @
    fns2 @
    (if c.gen_correctives then [nd_rcv_corr_done c] else []) @
    [
     nd_handle_uniq_poly c;
     nd_rcv_warmup_push c;
     clear_buffered_fetch_helper;
     trig_dispatcher c;
     trig_dispatcher_unique c;
     sw_event_driver_isobatch c;
    ] @
    do_reads c @
    (if c.gen_single_vid then [sw_event_driver_single_vid c] else []) @
    [mk_flow @@
      Proto.triggers c @
      GC.triggers c @
      TS.triggers @
      Timer.triggers c @
      sw_warmup_loops c @
      [
        trig_dispatcher_trig c;
        trig_dispatcher_trig_unique c;
        nd_from_sw_trig_dispatcher_trig c;
        sw_rcv_token_trig c;
        sw_rcv_vector_clock_trig;
        sw_demux c;
        sw_demux_poly c;
        sw_warmup c
      ]
    ] @
    roles_of c ast
  in
  let warmup = M.modify_warmup c warmup_p in

  warmup, snd @@ U.renumber_program_ids prog

let sw_demux_nm = sw_demux_nm
