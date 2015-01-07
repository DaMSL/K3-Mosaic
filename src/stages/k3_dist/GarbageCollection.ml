open K3.AST
open ProgInfo
open K3Dist
open K3Helpers
open Util

module D = K3Dist
module G = K3Global

(* Description of GC protocol
 * --------------------------
 * Switches keep track of the highest vid that's been acknowledged: max_vid
 *    Have to make sure received acks from every single node the switch sent data to.
 *      Counter per sent vid, or for error checking, keep track of each node that responded
 *    Report to a master switch their max_vid. Master switch finds lowest max_vid.
 * Nodes have a rcv buffer for fetches and puts
 *     Keep track of latest vid processed: stmt counter pointer
 *         Updated with stmt counter complete: may move forward in vid
 *         Updated with put/fetch: may move backwards in vid
 *     2 modes:
 *         Can either wait until max_vid is cleared, or send best achieved vid (best_vid) back to master switch.
 * Master switch finds lowest agreed upon vid and broadcasts GC up to this vid.
 * Nodes delete logs, data, stmt_ctr up to this vid
 * Potential problem: a put that hasn't been processed can insert a potential entry early in the stmt_ctrs
 *     Solution: scan input buffer to make sure nothing with lesser vid than max_vid
 *               Assumes: a received TPCIP packet will have been put in the buffer.
 *                        May need special external function (flush_network(trig_to_call_when_done)) to make this work.
 *)

(*
 * We send acks to the switches upon receiving puts
 *)

(*
 * hard code generate switches list in K3Global
(*
 * switch node list
 * --------------------------------
 * Iterate peers list to get all the switch list, and
 * generate the "switches" global variable
 *)
let switches_name = "switches"
let switches_var = mk_var switches_name

let switches_type = wrap_tset t_addr

let switches_code =
  mk_global_val_init switches_name switches_type @@
  mk_let "sw" t_string (mk_cstring "switch") @@
  mk_filtermap
    (* filter fun*)
    (mk_lambda (wrap_args K3Global.peers_id_type) @@
      mk_eq
        (mk_var K3Global.peers_id_type_name_name)
        (mk_just (mk_cstring "sw")) (* TODO the switch role name is hardcode*)
    )
    (* map fun *)
    (mk_lambda (wrap_args K3Global.peers_id_type) @@
                mk_var K3Global.peers_id_type_addr_name
    ) @@
    mk_var K3Global.peers_name
*)

let dummy_name = "dummy"
let dummy_trig_arg = [dummy_name, t_int]
let unit_arg = ["_", t_unit]

(* prefixes:
  * sw: switch
  * ms: master switch
  * nd: node
*)

(* determine the master switch by lowest address *)
let master_switch_addr =
  let init = some @@ mk_agg_fst
              (mk_assoc_lambda' ["min_addr", t_addr] G.peers_id_type @@
                mk_if (mk_and (mk_lt (mk_var "addr") @@ mk_var "min_addr") @@
                              (mk_eq (mk_var "job") @@ mk_cstring "switch"))
                  (mk_var "addr") @@
                  mk_var "min_addr") @@
              mk_var G.peers_name
  in
  {id="master_switch_addr"; t=t_addr; init; e=[]}

(* Switch acks: track which nodes replied with an ack *)
(* NOTE: candidate for map *)
let sw_ack_log =
  let e = ["vid", t_vid; "addr", t_addr; "ack", t_bool] in
  {id="switch_ack_log"; t=wrap_tset' @@ snd_many e; e; init=None} (* global val*)

(* max acknowledged vid on each switch *)
let sw_max_ack_vid = {id="max_ack_vid"; t=t_vid_mut; e=[]; init=Some min_vid_k3}

(* trigger for recieving an ack from a node *)
let sw_ack_rcv_trig =
  let ack_trig_args = list_drop_end 1 sw_ack_log.e in
  let slice_vars = ids_to_vars @@ fst_many ack_trig_args in
  mk_code_sink' "ack_rcv" ack_trig_args [] @@
    mk_block [
      (* set entry to true *)
      mk_update_slice (sw_ack_log.id) (slice_vars@[mk_cunknown]) @@
        mk_tuple @@ slice_vars@[mk_ctrue];
    ]

(* code to be incorporated in GenDist's rcv_put *)
(* assumes paramters "sender_ip" and "vid" *)
let nd_ack_send_code =
  mk_send (mk_ctarget "ack_rcv") (mk_var "sender_ip") @@ mk_tuple [mk_var "me"; mk_var "vid"]

(* Update the sw_max_ack_vid variable with the highest fully acknowledged vid.
 * The max_ack_vid will be sent to the master switch *)
let sw_update_max_ack_vid_fn =
  let sw_ack1, sw_ack2 = id_t_add "1" sw_ack_log.e, id_t_add "2" sw_ack_log.e in
  mk_global_fn "update_max_ack_vid" unit_arg [] @@
    mk_subscript 2 @@
      (* fold ascending until we find an entry with no ack *)
      mk_agg
        (mk_assoc_lambda' ["last_max", t_vid; "cur_max", t_vid; "cont", t_bool]
                          sw_ack_log.e @@
          mk_if (mk_not @@ mk_var "ack")
            (mk_tuple [mk_var "last_max"; mk_var "last_max"; mk_cfalse]) @@
            mk_if (mk_and (mk_var "cont") @@ D.v_neq (mk_var "vid") (mk_var "cur_max"))
              (mk_tuple [mk_var "cur_max"; mk_var "vid"; mk_ctrue]) @@
              mk_tuple [mk_var "last_max"; mk_var "cur_max"; mk_cfalse])
        (mk_tuple [mk_var sw_max_ack_vid.id; mk_var sw_max_ack_vid.id; mk_ctrue]) @@
          (* sort ascending *)
          mk_sort
            (mk_assoc_lambda' sw_ack1 sw_ack2 @@
              D.v_lt (mk_var "vid1") (mk_var "vid2")) @@
            (* get values greater than max_ack_vid *)
            mk_filter
              (mk_lambda' sw_ack_log.e @@
                D.v_gt (mk_var "vid") @@ mk_var @@ sw_max_ack_vid.id)
              (mk_var @@ sw_ack_log.id)

(* store the max vid received from each switch *)
let ms_max_sw_vid_map =
  let e = ["switch_addr", t_addr; "max_vid", t_vid] in
  {id="ms_max_sw_vid_map"; t=wrap_tmap' @@ snd_many e; e; init=None}

(* master switch trigger to receive and add to the max vid map *)
let ms_add_max_sw_vid_trig =
  let max_vid = "addr_max_vid" in
  mk_code_sink' "ms_add_max_sw_vid"
    [max_vid, wrap_ttuple @@ snd_many ms_max_sw_vid_map.e] [] @@
    mk_insert ms_max_sw_vid_map.id @@ mk_var max_vid

(* store the max vid received from each node *)
let ms_max_nd_vid_map =
  let e = ["node_addr", t_addr; "max_vid", t_vid] in
  {id="ms_max_nd_vid_map"; e; t=wrap_tmap' @@ snd_many e; init=None}

(* master switch trigger to receive and add to the max node vid map *)
let ms_add_max_nd_vid_trig =
  let max_vid = "addr_max_vid" in
  mk_code_sink' "ms_add_max_nd_vid"
    [max_vid, wrap_ttuple @@ snd_many ms_max_nd_vid_map.e] [] @@
    mk_insert ms_max_nd_vid_map.id @@ mk_var max_vid

(* node: find max vid for which we have 0 stmt cntrs *)
let nd_max_done_vid = {id="nd_max_done_vid"; e=[]; t=t_vid_mut; init=Some min_vid_k3}

(* update nd_max_done_vid *)
(* NOTE: could be done in 'real time' when receiving puts/pushes *)
let nd_update_max_done_vid_fn =
  let done1, done2 = id_t_add "1" D.stmt_cntrs_id_t, id_t_add "2" D.stmt_cntrs_id_t in
  mk_global_fn "nd_update_max_done_vid" unit_arg [] @@
    mk_subscript 2 @@
      (* fold ascending until we find an entry with no 0 in any of its stmts *)
      mk_agg
        (mk_assoc_lambda' ["last_max", t_vid; "cur_max", t_vid; "cont", t_bool]
                          stmt_cntrs_id_t @@
          mk_if (mk_neq (mk_var "counter") @@ mk_cint 0)
            (mk_tuple [mk_var "last_max"; mk_var "last_max"; mk_cfalse]) @@
            mk_if (mk_and (mk_var "cont") @@ D.v_neq (mk_var "vid") (mk_var "cur_max"))
              (mk_tuple [mk_var "cur_max"; mk_var "vid"; mk_ctrue]) @@
              mk_tuple [mk_var "last_max"; mk_var "cur_max"; mk_cfalse])
        (mk_tuple [mk_var nd_max_done_vid.id; mk_var nd_max_done_vid.id; mk_ctrue]) @@
          (* sort ascending *)
          mk_sort
            (mk_assoc_lambda' done1 done2 @@
              D.v_lt (mk_var "vid1") (mk_var "vid2")) @@
            (* get values greater than nd_max_done_vid *)
            mk_filter
              (mk_lambda' stmt_cntrs_id_t @@
                D.v_gt (mk_var "vid") @@ mk_var @@ nd_max_done_vid.id) @@
              mk_var D.stmt_cntrs_name

(* to search for a vid field *)
let r_vid = Str.regexp ".*vid.*"

(* function to perform garbage collection *)
let do_gc_fn =
  let min_vid = "min_gc_vid" in
  (* standard gc code for general data structures *)
  let gc_std ds =
    (* look for any entry containing vid *)
    let vid_str = fst @@ List.find (fun (s,_) -> r_match r_vid s) ds.e in
    (* delete any entry with a lower vid *)
    mk_iter
      (mk_lambda' ds.e @@
        mk_if (mk_lt (mk_var vid_str) @@ mk_var min_vid)
          (mk_delete ds.id @@ mk_tuple @@ ids_to_vars @@ fst_many ds.e) @@
          mk_cunit) @@
    mk_var ds.id
  in
  mk_global_fn "do_gc" [min_vid, t_vid] [t_unit] @@
    mk_block [
      (* clean master data structures *)
      gc_std ms_max_sw_vid_map;
      gc_std ms_max_nd_vid_map;
      (* clean switch data structures *)
      gc_std sw_ack_log;
      (* clean node data structures *)
      gc_std nd_max_done_vid;
    ]

let globals =
  [decl_global master_switch_addr;
   decl_global ms_max_sw_vid_map;
   decl_global ms_max_nd_vid_map;
   decl_global sw_max_ack_vid;
   decl_global sw_ack_log;
   decl_global nd_max_done_vid;
  ]

let fns =
  [sw_update_max_ack_vid_fn;
   nd_update_max_done_vid_fn;
  ]

let trigs =
  [ms_add_max_sw_vid_trig;
   ms_add_max_nd_vid_trig;
  ]


(*


(* number of vids received *)
let vid_rcv_cnt_name = "vid_rcv_cnt" (* global val *)
let vid_rcv_cnt_var = mk_var vid_rcv_cnt_name

let vid_rcv_cnt_2_name = "vid_rcv_cnt_2" (* two vid_buf *)
let vid_rcv_cnt_2_var = mk_var vid_rcv_cnt_2_name

let vid_rcv_cnt_type = wrap_tset t_int_mut

let vid_trig_arg_vid = "vid"
let vid_trig_args = [vid_trig_arg_vid, t_vid]

let vid_rcv_cnt_code name = mk_global_val_init name t_int_mut @@ mk_cint 0

let vid_rcv_cnt_incr_code x = mk_update x
    (mk_peek @@ mk_var x) @@
    mk_add (mk_cint 1) (mk_peek @@ mk_var x)

let vid_rcv_cnt_reset_code x = mk_update x
    (mk_peek @@ mk_var x) @@
    mk_cint 0

(* send ,receive vid and cal the biggest vid *)
let vid_buf_name = "vid_buf" (* global val*)
let vid_buf_var = mk_var vid_buf_name

let vid_buf_2_name = "vid_buf_2" (* two buf is needed*)
let vid_buf_2_var = mk_var vid_buf_2_name

let vid_buf_type = wrap_tset @@ wrap_ttuple [t_vid]

let vid_buf_code = mk_global_val vid_buf_name vid_buf_type
let vid_buf_2_code = mk_global_val vid_buf_2_name vid_buf_type

let vid_buf_empty_code buf_var= (* empty vid_buf *)
  mk_iter
    (mk_lambda
      (wrap_args ["vid", t_vid]) @@
      mk_delete vid_buf_name @@ mk_var "vid"
    )
    buf_var


(* min of the max acked vid among each switch *)
let min_max_acked_vid_name = "min_max_acked_vid" (* global val *)
let min_max_acked_vid_var = mk_var min_max_acked_vid_name
let min_max_acked_vid_id_type = t_vid_mut
let min_max_acked_vid_type = wrap_tset min_max_acked_vid_id_type

let min_max_acked_vid_code = (* global val*)
  mk_global_val_init
    min_max_acked_vid_name
    min_max_acked_vid_type @@
    mk_singleton min_max_acked_vid_type min_vid_k3

let min_max_acked_vid_update_code new_vid_var =
  mk_update
    min_max_acked_vid_name
    (mk_peek min_max_acked_vid_var) @@
    new_vid_var

(* gc_vid
 * --------------
 * the agreed vid to delete up to *)
let gc_vid_name = "gc_vid"
let gc_vid_var = mk_var gc_vid_name
let gc_vid_id_type = t_vid_mut
let gc_vid_type = wrap_tset gc_vid_id_type

let gc_vid_code =
  mk_global_val_init
    gc_vid_name
    gc_vid_type @@
    mk_singleton
      gc_vid_type
      min_vid_k3

(* my_idx code
 * -----------------
 * For switch node, get my_idx in the switches list*)
let my_idx_name = "my_idx"

let slice_my_idx_code =
  let idx_slice_pattern = mk_tuple @@ [mk_cunknown; K3Global.me_var] in
  (* let (my_idx:int, _) = peek(switches[_, me])  *)
  mk_let_deep (* figure out which switch idx myself is *)
    (wrap_args [my_idx_name,t_int;"_",t_unit])
    (mk_peek @@ mk_slice K3Global.switches_var idx_slice_pattern)

(* slice length and data_nodes_group
 * -------------------------------*)
let data_nodes_group_name = "data_nodes_group"
let slice_data_nodes_len_and_data_nodes_group_code =
  let group_slice_pattern =
        mk_tuple @@ [mk_var my_idx_name; mk_cunknown; mk_cunknown]
  in
  (*  let (_, data_nodes_group:{address}) = peek(data_nodes[my_idx, _]) *)
  mk_let_deep (* figure out the group of data nodes need to contact *)
    (wrap_args
      ["_", t_unit;
       K3Global.data_nodes_id_type_len_name, K3Global.data_nodes_id_type_len;
       data_nodes_group_name, K3Global.data_nodes_id_type_addrs])
     (mk_peek @@ mk_slice K3Global.data_nodes_var group_slice_pattern)

(* get_min_vid_code
 * ----------------
 * fold given vid buf to find the biggest vid
 * *)
let min_vid_name = "min_vid"

let get_min_vid_code vid_lst_var =
  let temp_vid_name = "temp_vid" in
  mk_let
    min_vid_name
    t_vid
    (mk_agg
      (mk_assoc_lambda
      (wrap_args [min_vid_name,t_vid])
      (wrap_args [temp_vid_name, t_vid])
      (mk_if
        (K3Dist.v_lt  (mk_var temp_vid_name) @@ mk_var min_vid_name)
        (mk_var temp_vid_name)
        (mk_var min_vid_name)
        )
      )
      max_vid_k3
      vid_lst_var)

(* do_garbage_collection
 * -----------------------
 * Receive the safe_vid_to_delete_final, and start to
 * delete the log all up to that vid
 * *)
let do_garbage_collection_trig_name =
  "do_garbage_collection"

let do_garbage_collection_trig_code (c:config) ast =
  (* help function to delete collection up to given vid *)
  (* delete vid that is < safe_vid  *)
  let delete_collection_up_to_vid safe_vid log_arg log_args_names log =
    mk_iter
      (mk_lambda
        (wrap_args log_arg ) @@
        mk_if
          (K3Dist.v_gt (mk_var safe_vid) (mk_var "vid"))
          (mk_delete log @@
            (mk_tuple @@ ids_to_vars log_args_names )
          )
          mk_cunit (*else fo nothing *)
      )
     (mk_var log)
  in
  (* helper, clear log structure *)
  let clear_all_log safe_vid=
    let clear_all_trig_log_code =
       List.fold_left
        (fun acc trig ->
          (delete_collection_up_to_vid
            safe_vid
            (args_of_t_with_v c trig)
            ("vid":: (arg_names_of_t c trig) )
            (log_for_t trig))::
          acc )
        []
        (P.get_trig_list c.p)
    in
    (* helper, clear global map decleration *)
    let clear_global_map_decl safe_vid =
      let decls = K3Util.globals_of_program ast in
      List.fold_left
        (fun acc decl ->
          match decl with
          | Global(name, TValue typ, m_expr),_ ->
          begin try
            let map_id = ProgInfo.map_id_of_name c.p name in
            (*[("vid", t_vid);("__map_0", type)...("__map_val", type)]*)
            let map_id_t_v = ProgInfo.map_ids_types_with_v_for
                              ~prefix:"__map_" ~vid:"vid" c.p map_id
            in
            let map_ids_v = fst_many map_id_t_v in
            let map_ts_v = snd_many map_id_t_v in
            let delc_types = wrap_tset @@ wrap_ttuple @@ map_ts_v in
            let map_id_t_no_val = ProgInfo.map_ids_types_no_val_for
                                  ~prefix:"__map_" c.p map_id in
            let map_id_no_val = fst_many @@ map_id_t_no_val in
            let map_t_no_val = snd_many @@ map_id_t_no_val in
            let key_num = List.length map_id_t_no_val in
            let leq_than_vid_name = "stuff_leq_than_vid_"^name in
            let cmp_unkown =
              (List.combine
                (make_lst "_" @@ key_num + 1 )
                (make_lst t_unknown @@ key_num + 1))
            in
            (* use iterate if the map does not have keys*)
            if key_num = 0 then
              (* get the element with max_vid in the map first, temporary
               * delete it from the map. Call delete_collection_up_to_vid
               * to the map, and then add the element with max_vid back. This
               * will make sure there is at least one element after GC. *)
             (
              mk_if (mk_is_empty (mk_var name) delc_types)
                mk_cunit (* if map is empty do nothing *)
                (
                  (* get the element with max vid *)
                  (mk_let
                  "element_with_max_vid"
                  (wrap_ttuple map_ts_v) @@
                  mk_peek
                    (mk_sort
                      (mk_assoc_lambda (* compare func *)
                        (wrap_args @@ ("vid1", t_vid) :: cmp_unkown)
                        (wrap_args @@ ("vid2", t_vid) :: cmp_unkown) @@
                          v_gt (mk_var "vid1") @@ mk_var "vid2") )
                      (mk_var name)
                ) @@
                mk_block [
                  (* delete the element with max vid from map *)
                  mk_delete name (mk_var "element_with_max_vid");

                  (* clear map *)
                  (delete_collection_up_to_vid
                    safe_vid
                    map_id_t_v
                    map_ids_v
                    name
                  );
                  (* insert the element with max vid into map *)
                  mk_insert name (mk_var "element_with_max_vid")
               ]) (* else *)
            ) :: acc
            else (* more than one key, need gbagg *)
            (* delete elements that are <=  than the agreed vid from
             * map struture, and store them in "stuff_leq_than_vid " *)
            let map_t_val_only = list_take_end 1 map_ts_v in
            let map_val_maybe_type = wrap_tmaybe (List.nth map_t_val_only 0) in
            (*(map_t_1,map_t_2..), (t_vid, (map_val))*)
            let leq_than_vid_cleared_type =
              [wrap_ttuple map_t_no_val; wrap_ttuple [t_vid; map_val_maybe_type]]
            in
            let leq_than_vid_cleared_set_type =
              wrap_tset @@ wrap_ttuple leq_than_vid_cleared_type
            in
            (* map_id_no_val_tuple(map_id_1, map_id_2...) *)
            (*
            let leq_than_vid_cleared_type =
              map_t_no_val @ [wrap_ttuple [t_vid; map_val_maybe_type]]
            in
            let leq_than_vid_cleared_set_type =
              wrap_tset @@ wrap_ttuple leq_than_vid_cleared_type
            in
            *)
            (
            (mk_let
              leq_than_vid_name
              delc_types
              (mk_agg
                (mk_assoc_lambda
                  (wrap_args ["leq_than_vid",delc_types])
                  (wrap_args map_id_t_v)
                  (mk_if
                    (K3Dist.v_geq (mk_var safe_vid) (mk_var "vid"))
                    (mk_block [
                      (* delete from map structure *)
                      (mk_delete name @@
                          (mk_tuple @@ ids_to_vars map_ids_v )
                      );
                      mk_combine
                        (mk_var "leq_than_vid")
                        (mk_singleton
                          delc_types @@ (mk_tuple @@ ids_to_vars map_ids_v)
                        )
                      ])
                    (mk_var "leq_than_vid")
                  )
                )
                (mk_empty delc_types )
                (mk_var name)
              ) (*end of agg*)
            ) @@ (* end of let stuff_bigger_than_vid *)
           (* groupby leq_than_vid by map key, for each map key,
            * only keep the element with max vid *)
           (mk_let
            "leq_than_vid_cleared" (*[map_0,..,(vid,maybe map_val)]*)
            leq_than_vid_cleared_set_type
            (mk_gbagg
              (mk_lambda (* group fun, groupby map key *)
                (wrap_args map_id_t_v)
                (mk_tuple @@ ids_to_vars map_id_no_val)
              )
              (mk_assoc_lambda (* agg func *)
                (wrap_args (List.combine
                  ["max_vid";"val"]
                  [t_vid; map_val_maybe_type])
                )
                (wrap_args map_id_t_v)
                (mk_if
                  (K3Dist.v_lt (mk_var "max_vid") (mk_var "vid") )
                  (mk_tuple @@ [mk_var "vid";
                                mk_just @@ mk_var ("__map_"^"val")])
                  (mk_tuple @@ [mk_var "max_vid";mk_var "val"])
                )
              )
              (mk_tuple [min_vid_k3; mk_nothing map_val_maybe_type])
              (mk_var leq_than_vid_name)
            )(* end of mk_gbagg*)
           ) @@
          (* iterate leq_than_vid_cleared, and add it back to map structure *)
          mk_iter
            (mk_lambda
              (wrap_args @@ List.combine
                  (["map_id_tuple";"vid_maybeVal"])
                  leq_than_vid_cleared_type
              ) @@
              (
                (*let (vid:(int, int, int), __map_val:maybe int) = vid_maybeVal*)
                (mk_let_deep
                  (wrap_args ["vid",t_vid;"__map_val",map_val_maybe_type])
                  (mk_var "vid_maybeVal")
                )@@
                (mk_let_deep
                  (wrap_args map_id_t_no_val)
                  (mk_var "map_id_tuple")
                )@@
                mk_unwrap_maybe ["__map_val", map_val_maybe_type] @@
                mk_insert
                  name
                  (mk_tuple @@ ids_to_vars
                    (["vid"] @ map_id_no_val @ ["__map_val"^"_unwrap"])
                  )
              )
            )
            (mk_var "leq_than_vid_cleared")
          )
          :: acc
          with Not_found -> acc
          end
          | _ -> acc
      )
      []
      decls
    in
    (* real trig *)
    mk_block (
      (* clear stmt_cntrs  *)
      (delete_collection_up_to_vid
        safe_vid
        stmt_cntrs_id_t
        (fst_many stmt_cntrs_id_t)
        stmt_cntrs_name ) ::

      (* clear acks *)
      (delete_collection_up_to_vid
        safe_vid
        switch_ack_log_id_type
        switch_ack_log_id_names
        switch_ack_log_name ) ::

      (* clear log master [t_vid;t_trig_id]*)
      (delete_collection_up_to_vid
        safe_vid
        ["vid",t_vid;"trig_id",t_trig_id; "stmt_id",t_stmt_id]
        ["vid";"trig_id";"stmt_id"]
        log_master) ::

      (* clear each trig log *)
      clear_all_trig_log_code @

      (clear_global_map_decl safe_vid) @

      (* update epoch *)
      [mk_update epoch_name (mk_peek epoch_var) @@
        mk_add (mk_cint 1) (mk_peek epoch_var)]
    )
  in
  mk_code_sink
  do_garbage_collection_trig_name
  (wrap_args ["safe_vid_to_delete_final", t_vid])
  [] @@
  (* TODO delete log up to the vid *)
  mk_block [
    mk_update
      gc_vid_name
      (mk_peek gc_vid_var) @@
      (mk_var "safe_vid_to_delete_final");

      clear_all_log "safe_vid_to_delete_final"

  ]



(* safe_vid_to_delete_rcv_trig
 * -------------------------------
 * switch node recive the final_vid_to_delete
 * from the first switch node, and send it to
 * the data nodes in its group *)
let final_safe_vid_to_delete_rcv_trig_name =
  "final_safe_vid_to_delete_rcv"

let final_safe_vid_to_delete_rcv_trig_code =
  mk_code_sink
  final_safe_vid_to_delete_rcv_trig_name
  (wrap_args ["final_safe_vid_to_delete", t_vid])
  [] @@
 (* send safe_vid_to_delete_final to the group of data nodes it responsbile for *)
  slice_my_idx_code @@
  slice_data_nodes_len_and_data_nodes_group_code @@
  mk_block[
  (* iterete each nodes in the group and send them the safe_vid_to_delete_final *)
   mk_iter
     (mk_lambda
     (wrap_args ["addr", t_addr])@@
        (mk_send (mk_ctarget do_garbage_collection_trig_name)
            (mk_var "addr")
            (mk_var "final_safe_vid_to_delete")))
        (mk_var data_nodes_group_name) ;
    (* send myself *)
    mk_send (mk_ctarget do_garbage_collection_trig_name)
      K3Global.me_var
      (mk_var "final_safe_vid_to_delete")
  ]

(* min_safe_vid_to_delete_rcv_trig
 * -------------------------------
 * The first switch node recieve the minimal safe vid
 * to delete from all the switch node.
 * 1. Calculate the min of the min,
 * 2. send the min of min_safe_vid_to_delete
 *    back to each switch node.
 *
 * *)
let min_safe_vid_to_delete_rcv_trig_name =
  "min_safe_vid_to_delete_rcv"

let min_safe_vid_to_delete_rcv_trig_code =
 mk_code_sink
  min_safe_vid_to_delete_rcv_trig_name
  (wrap_args ["min_safe_vid_to_delete", t_vid])
  [] @@
  mk_block [
    (* update vid_rcv_cnt_2 *)
    vid_rcv_cnt_incr_code vid_rcv_cnt_2_name;

    (* store the vid into vid_buf_2*)
    mk_insert vid_buf_2_name @@ mk_var "min_safe_vid_to_delete";

    mk_if
        (mk_eq
          (mk_peek vid_rcv_cnt_2_var)
          (mk_peek K3Global.switches_num_var)
        )
        (* get min vid *)
        (get_min_vid_code vid_buf_2_var @@ (mk_block[
          (* reset *)
          vid_rcv_cnt_reset_code vid_rcv_cnt_2_name;
          vid_buf_empty_code vid_buf_2_var;

          (* send the decided vid to all switch nodes*)
          mk_iter
          (mk_lambda
          (wrap_args K3Global.switches_id_args) @@
            (mk_send (mk_ctarget final_safe_vid_to_delete_rcv_trig_name)
                    (mk_var K3Global.switches_id_type_addr_name)
                    (mk_var min_vid_name))
          )
          K3Global.switches_var;
        ]))
      mk_cunit
  ]


(* safe_vid_to_delete_rcv_trig
 * ------------------------
 * Switch node receive the safe_vid_to_delete from
 * each individual data node in its group.
 * 1. buffer each safe_vid_to_delete
 * 2. When receive all the safe_vid_to_delete from
 *    its data node group, it calculate the smallest
 *    vid and send to the first switch node
 * NOTE: reuse vid_rcv_cnt and vid_buf
 * *)
let safe_vid_to_delete_rcv_trig_name =
  "safe_vid_to_delete_rcv"

let safe_vid_to_delete_rcv_trig_code =
  mk_code_sink
    safe_vid_to_delete_rcv_trig_name
    (wrap_args ["safe_vid_to_delete", t_vid])
    [] @@
    mk_block [
      (* update vid_rcv_cnt *)
      vid_rcv_cnt_incr_code vid_rcv_cnt_name;

      (* store the vid into buffer *)
      mk_insert vid_buf_name @@ mk_var "safe_vid_to_delete";

      (* if receive all vid from each switch *)
      slice_my_idx_code @@
      slice_data_nodes_len_and_data_nodes_group_code @@
      mk_if
        (mk_eq
          (mk_peek vid_rcv_cnt_var)
          (mk_add
            (mk_var K3Global.data_nodes_id_type_len_name)
            (mk_cint 1))
          )
       (mk_block[
          (* get min vid *)
          get_min_vid_code vid_buf_var @@
          (* send the min vid to the first switch node *)
          K3Global.switch_get_nth_code 0 @@

          mk_send
            (mk_ctarget min_safe_vid_to_delete_rcv_trig_name) (* target *)
            (mk_var K3Global.switch_get_nth_addr_name)
            (mk_var min_vid_name);

          (* reset buf*)
          vid_rcv_cnt_reset_code vid_rcv_cnt_name;
          vid_buf_empty_code vid_buf_var
        ])
        (* else do nothing *)
        mk_cunit
    ]

(* min_max_acked_vid_rcv_node_trig
 * ---------------------------
 * Data nodes receive the min_max_acked_vid from switch node.
 * 1. update the local min_max_acked_vid
 * 2. find the local safe vid (stmt_cnt before the vid are all zero)
 * 3. compare the safe vid with min_max_acked_vid, send the
 *    smaller one back to switch
 *
 *  NOTE bug fixed
 *  1. if the node never get message, its local stmt_cntrs is empty,
 *     will send vid (0,0,0). Should send min_max_acked_vid.
 *
 *  2. if after a GC, one node is not getting any message and all its counter
 *      in stmt_cntrs is = 0, it will send its max vid in stmt_cntrs to others.
 *      This could casue the next GC not to delte anything. it should send
 *      min_max_acked_vid
 * *)
let min_max_acked_vid_rcv_node_trig_name =
  "min_max_acked_vid_rcv_node"

let min_max_acked_vid_rcv_node_trig =
(* Code to get the vid that the stmt_cnt before it are all zero,meaning
 * it is safe to delete log up to that vid *)
let vid_all_finish_up_to = "vid_all_finish_up_to"
in
(* NOTE stmt_cntrs are not sorted *)
let get_vid_all_finish_up_to_code =
  mk_let_deep
    (wrap_args
        [vid_all_finish_up_to, t_vid;
         "found", t_bool] )
    (mk_agg
     (mk_assoc_lambda
        (wrap_args [vid_all_finish_up_to,t_vid;
                    "found",t_bool])
        (wrap_args stmt_cntrs_id_t)
        (mk_if
          (* already found *)
          (mk_eq (mk_var "found") (mk_cbool true))
          (mk_tuple [mk_var vid_all_finish_up_to;
                    mk_cbool true])
          (* else not yet found *)
          (mk_if
            (* if the current vid counter is 0 search *)
            (mk_eq
              (mk_var stmt_cntrs_counter_name)
              (mk_cint 0) )
            (mk_tuple [mk_var stmt_cntrs_vid_name;
                       mk_cbool false])
            (mk_tuple [mk_var vid_all_finish_up_to;
                       mk_cbool true])
          )(*end of inner if*)
        ) (* end of outer if *)
      )
      (mk_tuple [min_vid_k3;  mk_cbool false])
      (mk_sort (* stmt_cntrs sort by vid *)
        (mk_assoc_lambda (* compare func *)
          (wrap_args ["vid1", t_vid; "_",t_unknown; "_",t_unknown])
          (wrap_args ["vid2", t_vid; "_",t_unknown; "_",t_unknown]) @@
          v_lt (mk_var "vid1") @@ mk_var "vid2") )
        (mk_var stmt_cntrs_name)
    ) (* end of agg *)
in
let send_vid_finish_up_to_node2switch_code addr vid_finish_up_to =
  mk_let
    "safe_vid_to_delete"
    t_vid
     (mk_if
       (* if the "found" is false, it means either
        *   1. the stmt_cntrs is empty or
        *   2. all the stmt in the stmt_cntrs has cnt = 0
        * in this case, the safe_vid_to_delete should
        * set to min_max_acked_vid *)
       (mk_eq (mk_var "found") (mk_cbool false))
       (mk_peek min_max_acked_vid_var)
       (mk_if
          (mk_gt (mk_peek min_max_acked_vid_var) (mk_var vid_finish_up_to))
          (mk_var vid_finish_up_to)
          (mk_peek min_max_acked_vid_var)
       )(* end inner if *)
     ) (*end if *)@@
  mk_send (mk_ctarget safe_vid_to_delete_rcv_trig_name)
           (mk_var addr)  @@
           mk_tuple [ mk_var "safe_vid_to_delete"]
in
mk_code_sink
    min_max_acked_vid_rcv_node_trig_name
    (wrap_args ["vid",t_vid; "switch_addr",t_addr])
    [] @@
    mk_block [
      (* update max_vid *)
      min_max_acked_vid_update_code (mk_var "vid");

      get_vid_all_finish_up_to_code @@
      send_vid_finish_up_to_node2switch_code
        "switch_addr"
        vid_all_finish_up_to
    ]


(* min_max_vid_rcv_switch_trig
 * ----------------------------
 * Switch receive the max vid from the first switch node.
 * Update the local max vid and send it to the group
 * of data node it need to contact with. *)
let min_max_acked_vid_rcv_switch_trig_name =
  "min_max_acked_vid_rcv_swicth"

let min_max_acked_vid_rcv_switch_trig =
  mk_code_sink
    min_max_acked_vid_rcv_switch_trig_name
    (wrap_args vid_trig_args)
    [] @@
    (* get the group of data nodes this switch node is for*)
    slice_my_idx_code @@
    slice_data_nodes_len_and_data_nodes_group_code @@

    mk_block [
      (* send myself. Switch node is not in the data node group *)
      mk_send (mk_ctarget min_max_acked_vid_rcv_node_trig_name)
        K3Global.me_var
        (mk_tuple
          [(mk_var vid_trig_arg_vid);
          K3Global.me_var]
        );

      (* iterete each nodes in the group and send them the min_max_acked_vid *)
      mk_iter
        (mk_lambda
        (wrap_args ["addr", t_addr])@@
          (mk_send (mk_ctarget min_max_acked_vid_rcv_node_trig_name)
                    (mk_var "addr")
                    (mk_tuple
                      [(mk_var vid_trig_arg_vid);
                        K3Global.me_var]
                      ))
        )
        (mk_var data_nodes_group_name)
    ]

(* vid_rcv_trig
 * -------------------------------
 * 1. Receive max_acked_vid send from other switch nodes.
 * 2. When receive all the vid from other switch nodes,
 *    find other the MIN of max_acked_ vid and send to
 *    all other switch nodes.
 * 3. reset receive buffer *)

(* vid send and recive trigger*)
let vid_rcv_trig_name = "vid_rcv"

let vid_rcv_trig =
  mk_code_sink
  vid_rcv_trig_name
  (wrap_args vid_trig_args)
  [] @@
  mk_block [
    vid_rcv_cnt_incr_code vid_rcv_cnt_name; (* update vid_rcv_cnt *)
    mk_insert vid_buf_name @@ mk_var "vid"; (* store the vid into buffer *)
    mk_if (* if receive all vid from each switch *)
      (mk_eq (mk_peek vid_rcv_cnt_var) (mk_peek K3Global.switches_num_var))
      (mk_block [
        (* fold vid_buf to find the biggest vid *)
        get_min_vid_code vid_buf_var @@

        (* send the min_max_acked_ vid to all switch nodes*)
        mk_iter
          (mk_lambda
          (wrap_args K3Global.switches_id_args) @@
            (mk_send (mk_ctarget min_max_acked_vid_rcv_switch_trig_name)
                    (mk_var K3Global.switches_id_type_addr_name)
                    (mk_var min_vid_name ))
          )
          K3Global.switches_var;

        (* reset vid_buf and count *)
        vid_rcv_cnt_reset_code vid_rcv_cnt_name;
        vid_buf_empty_code vid_buf_var
      ])
      (mk_cunit)
  ]

(* max_acked_vid_send
 * -----------------------
 * For switch node. The start point of GC.
 * Send my current max acked vid in the switch_ack_log
 * to the first switch node
 * *)
let max_acked_vid_send_trig_name = "max_acked_vid_send"

let max_acked_vid_send_trig vid_cnt_var epoch_var hash_addr =
  let tmp_addr = "tmp_addr" in
  (* fold code to get max_acked_vid *)
  let max_acked_vid_code =
    mk_let
      "max_acked_vid"
      t_vid @@
      mk_agg
        (mk_assoc_lambda
          (wrap_args ["max_acked_vid", t_vid])
          (wrap_args switch_ack_log_id_type)
          (mk_if
            (* if current vid is acked && > max_vid then update max_acked_vid *)
            (mk_and
              (K3Dist.v_gt
                (mk_var switch_ack_log_id_vid_name) @@
                mk_var "max_acked_vid")
              (mk_eq (mk_var switch_ack_log_id_is_acked_name) (mk_cbool true))
              )
            (mk_var switch_ack_log_id_vid_name)
            (mk_var "max_acked_vid")
            )
          )
        min_vid_k3
        (mk_var switch_ack_log_name)
  in
  mk_code_sink
    max_acked_vid_send_trig_name
    (wrap_args dummy_trig_arg)
    [] @@
      mk_let  (* get addr of the first switch node*)
       tmp_addr
       K3Global.switches_id_type_addr
       (mk_snd
            K3Global.switches_type_raw
            (mk_peek K3Global.switches_var)
       ) @@
      (* get max_acked_vid *)
      max_acked_vid_code @@
      mk_send
        (mk_ctarget vid_rcv_trig_name) (* target *)
        (mk_var tmp_addr)
        (mk_var "max_acked_vid")

let triggers c ast =
  ack_rcv_trig ::
  ack_send_trig ::
  do_garbage_collection_trig_code c ast ::
  min_max_acked_vid_rcv_node_trig ::
  min_max_acked_vid_rcv_switch_trig ::
  final_safe_vid_to_delete_rcv_trig_code ::
  min_safe_vid_to_delete_rcv_trig_code ::
  safe_vid_to_delete_rcv_trig_code ::
  vid_rcv_trig ::
  max_acked_vid_send_trig vid_counter epoch_var hash_addr ::
  []

*)
