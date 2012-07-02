(* Functions that take K3 code and generate distributed K3 code *)


(* Basic types *
 * map_id is an int referring to a specific map
 * map_name is another way to refer to a map
 * stmt_id is a unique id:int for each statement in the program
 * trig_name is the name of a trigger and is what we usually use to refer to
 *   triggers.
 * trigger_id is a unique id:int for each trigger. We need it only inside K3
 *   code for efficiency. 
 *)

(* Assumptions:
 * We assume a rhs map can only occur once per statement 
 *)

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

(* on_insert_<trigger>_switch:
 * -----------------------------------------------
 * The switch starts the process
 *)

(* TODO:
  *     - How does update work? After the slice, do I need to give the full
  *     tuple or just the remaining value?
  *     - Worry about map values vs keys: need whole tuples
  *     - Rcv_put needs to add, not save, counters
  * - slice needs CUnknown, not "_"
 *      - Need to make names clearer (S_ for statement)
 *      - Wrapping in a list should recursively change all contained stuff.
 *       - Problem: don't think shuffle handles map[b] = map[c; b] ie. b->b
 *       mapping when b is unbound. Need to make it fully stmt_id specific.
 *       - Change data structure p to remove map arg names, and add loop vars.
 *       - declare global maps that have vid as a first attribute
        * - declare local buffers for node 
 *       - add rcv_put trigger
 *       - key, pat -> just pat
 *       - make sure shuffle only deals with RHS types (it's just splitting
 *       them)
 *       - may need some types that are subsets of the maps rather than whole
 *       map types. need sub-functions to deal with this!!
 *        - Possibly split up the data into a bunch of maybe's in tuples
 *        (present, not-present)
 *          - shuffle needs to have a version for each binding (ie each trigger)
 *            - Will have a tuple of maybes
 *          - will only have tuples with fields not in the bound pattern
 *)

open Util
open K3
open K3Helpers
open ProgInfo

exception ProcessingFailed of string;;


(* set type of vid here. Currently it's TInt, but we may need something better
 * *)
let vid_type = t_int
let vid_type_mut = t_int_mut
let ip_type = t_int

(* argument manipulation convenience functions *)
let arg_types_of_t p trig_nm = extract_arg_types (args_of_t p trig_nm)
let arg_names_of_t p trig_nm = extract_arg_names (args_of_t p trig_nm)
let args_of_t_as_vars p trig_nm = convert_names_to_vars (arg_names_of_t p trig_nm)

let args_of_t_with_v p trig_nm = args_of_t p trig_nm@["vid", vid_type]
let arg_types_of_t_with_v p trig_nm = arg_types_of_t p trig_nm@[vid_type]
let args_of_t_as_vars_with_v p trig_nm = 
  args_of_t_as_vars p trig_nm@[mk_var "vid"]

(* global trigger names needed for generated triggers and sends *)
let send_fetch_name_of_t p trig_nm = trig_nm^"_send_fetch"
let rcv_fetch_name_of_t p trig_nm = trig_nm^"_rcv_fetch"
let rcv_put_name_of_t p trig_nm = trig_nm^"_rcv_put"
let send_push_name_of_t p trig_nm stmt_id map_id =
  trig_nm^"_send_push_s"^string_of_int stmt_id^"_"^map_name_of p map_id
let rcv_push_name_of_t p trig_nm stmt_id map_id =
  trig_nm^"_rcv_push_s"^string_of_int stmt_id^"_"^map_name_of p map_id
let send_corrective_name_of_t p map_id = 
  map_name_of p map_id^"_send_correctives"
let do_complete_name_of_t p trig_nm stmt_id =
  trig_nm^"_do_complete_s"^string_of_int stmt_id
let filter_corrective_list_name = "filter_corrective_list"
let rcv_corrective_name_of_t p trig_nm stmt_id map_id =
  trig_nm^"_rcv_corrective_s"^string_of_int stmt_id^"_"^map_name_of p map_id
let do_corrective_name_of_t p trig_nm stmt_id =
  trig_nm^"_do_corrective_s"^string_of_int stmt_id

(* route and shuffle function names *)
let route_for p map_id = "route_to_"^map_name_of p map_id
(* we don't really need the lhs_map to the shuffle but it makes things a little
 * clearer in the code *)
let shuffle_for p stmt_id rhs_map_id lhs_map_id = 
  "shuffle_s"^string_of_int stmt_id^"_"^map_name_of p rhs_map_id^
  map_name_of p lhs_map_id

(* foreign function names *)
(* note: most buffer and storage functions are done using native accesses *)
let log_write_for p trig_nm = "log_write_"^trig_nm (* varies with bound *)
let log_get_bound_for p trig_nm = "log_get_bound_"^trig_nm 
let log_read_geq = "log_read_geq" (* takes vid, returns (trig, vid)list >= vid *)
(* adds the delta to all subsequent vids following in the buffer so that non
 * delta computations will be correct. Must be atomic ie. no other reads of the
 * wrong buffer value can happen *)
let add_delta_to_buffer_for_map p map_id = 
  "add_delta_to_buffer_"^map_name_of p map_id

(* data structures ---- *)
  (* stmt_cntrs - (vid, stmt_id, counter) *)
let stmt_cntrs_name = "stmt_cntrs"
let stmt_cntrs = mk_var stmt_cntrs_name
let stmt_cntrs_type = wrap_tlist_mut @: wrap_ttuple_mut @: 
    [vid_type_mut; t_int_mut; t_int_mut]
let stmt_cntrs_code = Global (stmt_cntrs_name, TValue(stmt_cntrs_type), None)  


(* k3 functions needed *)
(*
"route_to_map_"^map_id returns ip
"shuffle_map_"^map_id^"_to_map_"map_id takes maybe tuples and a pattern maybe
  tuple and returns a tuple
"promote_address" -> Local() -> ip -> address_t
"addr_for_send_push": takes stmt_id, map_id and returns addr of send_push
  trigger
 *)

let send_fetch_trig p trig_name =
  let send_fetches_of_rhs_maps  =
    (mk_iter
      (mk_lambda 
        (ATuple["ip", t_int;
          "stmt_map_ids", wrap_tlist @: wrap_ttuple [t_int; t_int]]
        )
        (mk_send 
          (mk_apply 
            (mk_var "promote_address")
            (mk_tuple [mk_var @: rcv_fetch_name_of_t p trig_name; mk_var "ip"])
          ) 
          (mk_tuple @:
            args_of_t_as_vars_with_v p trig_name@ 
            [mk_var "stmt_map_ids"]
          )
        )
      )
      (mk_gbagg
        (mk_assoc_lambda (* Agg function *)
          (ATuple["stmt_id", t_int; "map_id", t_int; "ip", ip_type])
          (AVar("acc", wrap_tlist @: wrap_ttuple [t_int; t_int]))
          (mk_combine
            (mk_var "acc")
            (mk_singleton 
              (wrap_ttuple [t_int; t_int])
              (mk_tuple [mk_var "stmt_id";mk_var "map_id"])
            )
          )
        ) 
        (mk_lambda (* Grouping function *)
          (ATuple["stmt_id", t_int; "map_id", t_int; "ip", ip_type])
          (mk_var "ip")
        )
        (mk_empty (wrap_tlist @: wrap_ttuple [t_int; t_int]))
        (* [] *)
        (List.fold_left
          (fun acc_code (stmt_id, rhs_map_id) ->
            let route_fn = route_for p rhs_map_id in
            let key = partial_key_from_bound p stmt_id rhs_map_id in
            (mk_combine
              (mk_map 
                (mk_lambda (AVar("ip", t_int))
                  (mk_tuple 
                    [mk_const @: CInt stmt_id; mk_const @: CInt rhs_map_id; 
                      mk_var "ip"
                    ]
                  )
                )
                (mk_apply 
                  (mk_var route_fn)
                  (mk_tuple key)
                )
              )
              acc_code
            )
          )
          (mk_empty @: wrap_tlist @: wrap_ttuple [t_int; t_int])
          (s_and_over_stmts_in_t p read_maps_of_stmt trig_name) 
        ) 
      )
    )
in
let send_completes_for_stmts_with_no_fetch =
  List.fold_left
    (fun acc_code (stmt_id, lhs_map_id, complete_trig_name) -> 
      let route_fn = route_for p lhs_map_id in
      let key = partial_key_from_bound p stmt_id lhs_map_id in
        acc_code@
        [mk_iter 
          (mk_lambda (AVar("ip", ip_type))
            (mk_send
              (mk_apply 
                (mk_var "promote_address")
                (mk_tuple 
                  [mk_var complete_trig_name; mk_var "ip"]
                )
              )
              (mk_tuple @:
                args_of_t_as_vars p trig_name
              )
            )
          )
          (mk_apply (mk_var route_fn) (mk_tuple key))
        ]
    ) 
    []
    (List.map 
      (fun stmt_id -> 
        (stmt_id, lhs_map_of_stmt p stmt_id, 
        do_complete_name_of_t p trig_name stmt_id)
      )
      (stmts_without_rhs_maps_in_t p trig_name)
    )
in
let send_puts =
  (* send puts
   * count is generated by counting the number of messages going to a
   * specific IP *)
  mk_iter
    (mk_lambda 
      (ATuple["ip", ip_type; 
        "stmt_id_cnt_list", wrap_tlist @: wrap_ttuple [t_int; t_int]])
      (mk_send
        (mk_apply
          (mk_var "promote_address")
          (mk_tuple
            [mk_var @: rcv_put_name_of_t p trig_name; mk_var "ip"]
          )
        )
        (mk_tuple @: mk_var "stmt_id_cnt_list"::
          args_of_t_as_vars_with_v p trig_name
        )
      )
    )
    (mk_gbagg
      (mk_assoc_lambda (* agg func *)
        (ATuple["ip", ip_type; "stmt_id", t_int; "count", t_int])
        (AVar("acc", wrap_tlist @: wrap_ttuple [t_int; t_int]))
        (mk_combine
          (mk_var "acc")
          (mk_singleton
            (wrap_ttuple [t_int; t_int])
            (mk_tuple [mk_var "stmt_id"; mk_var "count"])
          )
        )
      )
      (mk_lambda (* grouping func *)
        (ATuple["ip", ip_type; "stmt_id", t_int; "count", t_int])
        (mk_var "ip")
      )
      (mk_empty @: wrap_tlist @: wrap_ttuple [t_int; t_int])
      (mk_gbagg (* inner gba *)
        (mk_assoc_lambda (* agg func *)
          (ATuple["ip", ip_type; "stmt_id", t_int])
          (AVar("acc", t_int)) 
          (mk_add
            (mk_var "acc")
            (mk_const @: CInt 1)
          )
        )
        (mk_lambda (* group func *)
          (ATuple["ip", ip_type; "stmt_id", t_int])
          (mk_tuple [mk_var "ip"; mk_var "stmt_id"])
        )
        (mk_const @: CInt 0) (* [] *)
        (List.fold_left
          (fun acc_code (stmt_id, (rhs_map_id, lhs_map_id)) ->
            let shuffle_fn = shuffle_for p stmt_id rhs_map_id lhs_map_id in
            let key = partial_key_from_bound p stmt_id lhs_map_id in
            (* we need the types for creating empty rhs tuples *)
            let rhs_map_types = map_types_for p rhs_map_id in
            let lhs_map_types = map_types_for p lhs_map_id in
            (mk_combine
              acc_code
              (mk_map
                (mk_lambda
                  (ATuple["ip", t_int; "tuples", wrap_ttuple lhs_map_types])
                  (mk_tuple [mk_var "ip"; mk_const @: CInt stmt_id])
                )
                (mk_apply
                  (mk_var shuffle_fn)
                  (mk_tuple @:
                    mk_tuple key::
                      [mk_empty @: wrap_tlist @: wrap_ttuple rhs_map_types]@
                      [mk_const @: CBool true]
                  ) 
                )
              ) 
            ) (* mk_combine *)
          ) (* fun *)
          (mk_empty @: wrap_tlist @: wrap_ttuple [t_int; t_int])
          (s_and_over_stmts_in_t p lhs_rhs_of_stmt trig_name)
        )
      ) (* gbagg *)
    ) (* gbagg *)
in
(* Actual SendFetch function *)
Trigger(
  send_fetch_name_of_t p trig_name,
  ATuple(args_of_t_with_v p trig_name),
  [], (* locals *)
  mk_block @:
    send_fetches_of_rhs_maps::
    send_completes_for_stmts_with_no_fetch@ 
    [send_puts]
  
)
   
(* trigger_rcv_fetch
 * -----------------------------------------
 * Receive a fetch at a node.
 * Reuses switch-side computation of which maps this node should read.
 * This could be done entirely at the node, but would repeat work done
 * at the switch anyway.
 * The assumption is that the "stmts_and_map_names" data is not large.
 * We have this as a multiplexer to the different fetch functions, because we
 * need to record only one trigger in the log, and because it reduces messages
 * between nodes.
 *)
let rcv_fetch_trig p trig_name =
  Trigger(
    rcv_fetch_name_of_t p trig_name, 
    ATuple(("stmts_and_map_ids", 
      wrap_tlist @: wrap_ttuple [t_int; t_int])::
      args_of_t_with_v p trig_name),
    [], (* locals *)
    (mk_block
      [mk_apply
        (mk_var @: log_write_for p trig_name)
        (mk_tuple @:
          (mk_const @: CString trig_name)::args_of_t_as_vars_with_v p trig_name
        )
      ;
      (* invoke generated fetch triggers, which in turn send pushes. *)
      mk_iter
        (mk_lambda
          (ATuple["stmt_id", t_int; "map_id", t_int])
          (* this send is not polymorphic. every fetch trigger expects
           * the same set of bound variables. *)
          (mk_send
            (mk_apply
              (mk_var "addr_for_send_push") (* global func *)
              (mk_tuple [mk_var "stmt_id"; mk_var "map_id"])
            )
            (mk_tuple @: args_of_t_as_vars_with_v p trig_name)
          )
        )
        (mk_var "stmts_and_map_ids")
      ]
    )
  )

(* Receive Put trigger
 * --------------------------------------- *
 * Update the statement counters with the received values
 *)
let rcv_put_trig p trig_name =
Trigger(
  (rcv_put_name_of_t p trig_name),
  ATuple(("stmt_id_cnt_list", wrap_tlist @: wrap_ttuple [t_int; t_int])::
    args_of_t_with_v p trig_name),
  [],
  let part_pat = ["vid", vid_type; "stmt_id", t_int] in
  let counter_pat = ["count", t_int] in
  let full_pat = part_pat @ counter_pat in
  let full_types = wrap_ttuple @: extract_arg_types full_pat in
  let part_pat_as_vars = convert_names_to_vars @: extract_arg_names
    part_pat in
  let query_pat = mk_tuple @: part_pat_as_vars @ [mk_const CUnknown] in
  let stmt_cntrs_slice = mk_slice stmt_cntrs query_pat in
  mk_iter
    (mk_lambda
      (ATuple["stmt_id", t_int; "count", t_int])
      (mk_if (* do we already have a tuple for this? *)
        (mk_has_member stmt_cntrs query_pat full_types)
        (mk_update (* really an error -- shouldn't happen. Raise exception? *)
          stmt_cntrs
          stmt_cntrs_slice
          (mk_tuple @: part_pat_as_vars@[mk_var "count"])
        )
        (mk_insert
          stmt_cntrs
          (mk_tuple @: part_pat_as_vars@[mk_var "count"])
        )
      )
    )
    (mk_var "stmt_id_cnt_list")
)


(* Trigger_send_push_stmt_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 * Produces a list of triggers.
 *)
let send_push_stmt_map_trig p trig_name = 
  (List.fold_left
    (fun acc_code (stmt_id, (lhs_map_id, rhs_map_id)) ->
      let rhs_map_types = wrap_tlist @: wrap_ttuple @:
        map_types_for p rhs_map_id in 
      let rhs_map_name = map_name_of p rhs_map_id in
      let shuffle_fn = shuffle_for p stmt_id rhs_map_id lhs_map_id in
      let rkey = partial_key_from_bound p stmt_id rhs_map_id in
      acc_code@
      [Trigger (send_push_name_of_t p trig_name stmt_id rhs_map_id, 
        ATuple(args_of_t_with_v p trig_name),
        [] (* locals *),
          (mk_iter
            (mk_lambda (ATuple["ip", t_int; "tuples", rhs_map_types])
              (mk_send
                (mk_apply
                  (mk_var "promote_address")
                  (mk_tuple [mk_var @: rcv_push_name_of_t p trig_name stmt_id
                    rhs_map_id; mk_var "ip"]
                  )
                )
                (mk_tuple @: mk_var "tuples"::args_of_t_as_vars p trig_name)
              )
            )
            (mk_apply
              (mk_var shuffle_fn)
              (mk_tuple
                (mk_tuple 
                  rkey::
                  (mk_slice 
                    (mk_var rhs_map_name) 
                    (mk_tuple @: mk_var "vid"::rkey)
                  )::[mk_const @: CBool false]
                )
              )
            )
          ) (* mk_iter *)
        )
      ] (* Trigger *)
    ) (* fun *)
    []
    (s_and_over_stmts_in_t p lhs_rhs_of_stmt trig_name)
  ) 


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
    let tuple_types = map_types_for p read_map_id in
    let value_type = List.nth tuple_types (List.length tuple_types - 1) in
    (* remove value from tuple, add vid *)
    let reduced_tuple_with_v = mk_reduced_tuple "tuples" 
      tuple_types (List.length tuple_types -1) ["vid", vid_type] in
    let tuples_with_v = mk_reduced_tuple "tuples"
      tuple_types (List.length tuple_types) ["vid", vid_type] in
    acc_code@
    [Trigger(rcv_push_name_of_t p trig_name stmt_id read_map_id,
      ATuple(("tuples", wrap_tlist @: wrap_ttuple @: tuple_types)::
        args_of_t_with_v p trig_name), 
      [], (* locals *)
      (mk_block
        [mk_iter
          (mk_lambda
            (ATuple["tuple", wrap_ttuple tuple_types])
            (mk_if
              (mk_has_member 
                (mk_var map_name)
                reduced_tuple_with_v
                value_type
              )
              (mk_update
                (mk_var map_name)
                (mk_slice
                  (mk_var map_name)
                  reduced_tuple_with_v
                )
                tuples_with_v
              )
              (mk_insert
                (mk_var map_name)
                tuples_with_v
              )
            )
          )
          (mk_var "tuples")
         ;
         (* check statment counters to see if we can process *)
         let part_pat = ["vid", vid_type; "stmt_id", t_int] in
         let counter_pat = ["count", t_int] in
         let full_pat = part_pat @ counter_pat in
         let full_types = wrap_ttuple @: extract_arg_types full_pat in
         let part_pat_as_vars = convert_names_to_vars @: extract_arg_names
           part_pat in
         let query_pat = mk_tuple @: part_pat_as_vars @ [mk_const CUnknown] in
         let stmt_cntrs_slice = mk_slice stmt_cntrs query_pat in
         mk_if (* check if the counter exists *)
           (mk_has_member stmt_cntrs query_pat full_types)
           (mk_block
             [mk_update
               stmt_cntrs
               stmt_cntrs_slice
               (mk_let_many full_pat
                 stmt_cntrs_slice
                 (mk_tuple @:
                   part_pat_as_vars @ 
                   [mk_sub (mk_var "count") (mk_const @: CInt 1)]
                 )
               )
               
             ;mk_if (* check if the counter is 0 *)
               (mk_eq
                 stmt_cntrs_slice
                 (mk_tuple @: part_pat_as_vars @ [mk_const @: CInt 0])
               ) 
               (* Send to local do_complete *)
               (mk_send
                 (mk_var @: do_complete_name_of_t p trig_name stmt_id)
                 (mk_tuple @: args_of_t_as_vars_with_v p trig_name)
               )
               (mk_const CUnit) (* do nothing *)
             ]
           )
           (mk_update (* else: no value in the counter *)
             stmt_cntrs
             stmt_cntrs_slice
             (* Initialize if the push arrives before the put. *)
             (mk_tuple @: part_pat_as_vars @ [mk_const @: CInt(-1)])
           )
         ]
       )
    )]
  )
  [] (* empty code *)
  (s_and_over_stmts_in_t p read_maps_of_stmt trig_name)
    
 
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

let trigs_stmts_with_rhs_map =
    List.filter
      (fun (trig, stmt_id) -> stmt_has_rhs_map p stmt_id map_id)
      (List.flatten @:
        List.map
          (fun trig -> 
            List.map (fun stmt -> (trig, stmt)) (stmts_of_t p trig))
          (get_trig_list p)
      )
in
(* predefined K3 list of stmts with rhs maps *)
let trig_stmt_k3_list = 
  List.fold_left 
    (fun acc_code (trig, stmt_id) -> 
      (mk_combine 
        (mk_tuple @:
          [mk_const @: CInt (trigger_id_for_name p trig); 
           mk_const @: CInt stmt_id]
        )
        acc_code
      )
    )
    (mk_empty @: wrap_tlist @: wrap_ttuple [t_int; t_int])
    trigs_stmts_with_rhs_map
in
match trigs_stmts_with_rhs_map with [] -> [] | _ ->
  let tuple_types = wrap_ttuple @: map_types_for p map_id in
  [Trigger(send_corrective_name_of_t p map_id,
    ATuple["delta_tuples", wrap_tlist tuple_types; "vid", vid_type],
    [],
    (* the corrective list tells us which statements were really executed *)
    (mk_let "corrective_list" (* (vid * stmt_id) list *)
      (wrap_tlist @: wrap_ttuple [vid_type; t_int])
      (mk_apply
        (mk_var filter_corrective_list_name)
        trig_stmt_k3_list (* feed in list of possible stmts *)
      )
      (mk_iter  (* loop over corrective list *)
        (mk_lambda
          (ATuple["vid", vid_type; "stmt_id", t_int]) 
          (List.fold_left  
            (* loop over all possible read map matches *)
            (fun acc_code (target_trig, target_stmt) ->
              let target_map = lhs_map_of_stmt p target_stmt in
              let key = partial_key_from_bound p target_stmt map_id in
              let shuffle_fn = shuffle_for p target_stmt map_id target_map in
              mk_if (* if match, send data *)
                (mk_eq
                  (mk_var "stmt_id")
                  (mk_const @: CInt target_stmt)
                )
                (mk_iter 
                  (mk_lambda 
                    (ATuple["ip", ip_type; "delta_tuples", tuple_types])
                    (mk_send
                      (mk_apply
                        (mk_var "promote_address")
                        (mk_tuple
                          [mk_var @: rcv_corrective_name_of_t p target_trig
                            target_stmt target_map; mk_var "ip"]
                        )
                      )
                      (mk_var "delta_tuples")
                    )
                  )
                  (* get bound vars from log *)
                  (mk_let_many 
                    (args_of_t p target_trig)
                    (mk_apply 
                      (mk_var @: log_get_bound_for p target_trig)
                      (mk_var "vid")
                    )
                    (mk_apply
                      (mk_var shuffle_fn)
                      (mk_tuple @:
                        mk_tuple key::
                        mk_var "delta_tuples"::
                        [mk_const @: CBool false]
                      )
                    )
                  )
                )
                acc_code (* just another branch on the if *)
            )
            (mk_const CUnit) (* base case *)
            trigs_stmts_with_rhs_map 
          )
        )
        (mk_var "corrective_list")
      )
    )
  )]
in
let unique_lhs_maps = 
  ListAsSet.uniq @:
  List.fold_left 
    (fun acc stmt -> lhs_map_of_stmt p stmt::acc) [] (get_stmt_list p)
in
List.flatten @: List.map send_correctives unique_lhs_maps


(* Debug -----
 *
 
let do_complete_name = trig_name^"_do_complete_"^stmt_id in
Trigger
  (do_complete_name, ATuple(trig_args_with_v), [] (* locals *),
    (* in terms of substitution, we need to 
     * a. switch read maps for buffers
     * b. inject a send to the send_correctives trigger by either sending a
     * single delta or sending a cse representing a slice of calculated data. We
     * need to take the variable in K3 and transform it by adding in the bound
     * variables so it matches the format of the lhs map *)
    let ast_stmt2 = subst_buffers (ast_of_stmt stmt_id) (read_maps_of_stmt stmt_id)
    in
    let delta_in_lhs_map = to_lhs_map_form (delta_var_of_stmt stmt_id)
      (partial_key_from_bound stmt_id map_id trig_args)
    in
    inject_call_forward_correctives ast_stmt2 delta_in_lhs_map
  )

*)

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
  ["trig_stmt_list", wrap_tlist @: wrap_ttuple @: [t_int; t_int]] 
  [wrap_tlist @: wrap_ttuple @: [t_int; t_int]] (* (vid, stmt_id) list *)
  (mk_sort (* sort so that early vids are first for performance *)
    (mk_map
      (mk_lambda (ATuple["trig_id", t_int; "stmt_id", t_int])
        (mk_filtermap
          (mk_lambda (ATuple["vid", vid_type; "trig_id2", t_int])
            (mk_eq (mk_var "trig_id2") (mk_var "trig_id"))
          )
          (mk_lambda (ATuple["trig_id2", t_int; "vid", vid_type])
            (mk_tuple [mk_var "vid"; mk_var "stmt_id"])
          )
          (mk_apply  (* list of triggers >= vid *)
            (mk_var log_read_geq) (* TODO *)
            (mk_var "vid")  
          )
        )
      )
      (mk_var "trig_stmt_list")
    )
    (mk_assoc_lambda (* compare func *)
      (ATuple["vid1", vid_type; "stmt1", t_int])
      (ATuple["vid2", vid_type; "stmt2", t_int])
      (mk_lt (mk_var "vid1") (mk_var "vid2"))
    )
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
List.fold_left 
  (fun acc_code (stmt_id, map_id) ->
    acc_code@ 
    [Trigger(rcv_corrective_name_of_t p trig_name stmt_id map_id,
      ATuple[
        "delta_tuples", wrap_tlist @: wrap_ttuple @: map_types_for p map_id; 
        "vid", vid_type], 
      [], (* locals *)
      mk_block
        (* accumulate delta for this vid and all following vids *)
        [mk_apply
          (mk_var @: add_delta_to_buffer_for_map p map_id)
          (mk_tuple [mk_var "vid"; mk_var "delta_tuples"])
          ;
          (* only execute if we have all the updates *)
          mk_if
            (mk_eq
              (mk_slice stmt_cntrs @:
                mk_tuple
                  [mk_var "vid"; mk_const @: CInt stmt_id; mk_const CUnknown]
              )
              (mk_tuple
                [mk_var "vid"; mk_const @: CInt stmt_id; mk_const @: CInt 0]
              )
            )
            (* get bound vars from log *)
            (mk_let_many 
              (args_of_t p trig_name)
              (mk_apply 
                (mk_var @: log_get_bound_for p trig_name)
                (mk_var "vid")
              )
              (mk_send
                (mk_var @: do_corrective_name_of_t p trig_name stmt_id) @:
                mk_tuple @:
                  args_of_t_as_vars_with_v p trig_name @
                  [mk_var "delta_tuples"]
              )
            )
            (mk_const CUnit) (* else *)
        ]
    )]
  )
  []
  (s_and_over_stmts_in_t p read_maps_of_stmt trig_name)
;;

 (* debug

(* NOTE: note sure if this function will be much different from regular
 * do_completes once we have the right generated K3 "shadow functions" *)
let do_corrective_name = trig_name^"_do_complete_"^stmt_id in
mk_global_fn do_corrective_name
  (trig_args_with_v)
  TUnit (* output *)
  (* in terms of substitution, we need to 
   * a. switch read maps for buffers
   * b. inject a send to the send_correctives trigger by either sending a
   * single delta or sending a cse representing a slice of calculated data. We
   * need to take the variable in K3 and transform it by adding in the bound
   * variables so it matches the format of the lhs map *)
  (let ast_stmt2 = subst_buffers (ast_of_stmt stmt_id) (read_maps_of_stmt stmt_id)
    in
    let delta_in_lhs_map = to_lhs_map_form (delta_var_of_stmt stmt_id)
      (partial_key_from_bound stmt_id map_id trig_args)
    in
    inject_call_forward_correctives ast_stmt2 delta_in_lhs_map
  )

  *)

(* Generate all the code for a specific trigger *)
let gen_dist_for_t p trig =
    send_fetch_trig p trig::
    rcv_put_trig p trig::
    rcv_fetch_trig p trig::
    send_push_stmt_map_trig p trig@
    rcv_push_trig p trig@
    rcv_correctives_trig p trig

(* Function to generate the whole distributed program *)
let gen_dist p ast =
  (* we'll start by defining everything as foreign functions so we typecheck *) 
  let triggers = get_trig_list p in
  let regular_trigs = List.flatten @:
    List.map
      (fun trig -> gen_dist_for_t p trig)
      triggers
  in
  List.map
    (fun x -> Declaration x)
    ( stmt_cntrs_code ::         (* global data struct *)
      filter_corrective_list ::  (* global func *)
      regular_trigs@
      send_corrective_trigs p (* per-map basis *)
    )
  

