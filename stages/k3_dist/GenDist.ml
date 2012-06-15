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

(* TODO: - slice needs CUnknown, not "_"
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

open K3
open K3Helpers
open ProgInfo

exception ProcessingFailed of string;;


(* set type of vid here. Currently it's TInt, but we may need something better
 * *)
let vid_type = t_int
let ip_type = t_int

(* argument manipulation convenience functions *)
let arg_types_of_t p trig_nm = extract_arg_types (args_of_t p trig_nm)
let arg_names_of_t p trig_nm = extract_arg_names (args_of_t p trig_nm)
let args_of_t_as_vars p trig_nm = convert_names_to_vars (arg_names_of_t p trig_nm)

let args_of_t_with_v p trig_nm = (args_of_t p trig_nm)@["vid", vid_type]
let arg_types_of_t_with_v p trig_nm = (arg_types_of_t p trig_nm)@[vid_type]
let args_of_t_as_vars_with_v p trig_nm = 
  (args_of_t_as_vars p trig_nm)@[mk_var "vid"]

(* global trigger info needed for generated triggers and sends *)
let send_fetch_name_of_t p trig_nm = trig_nm^"_send_fetch"
let rcv_fetch_name_of_t p trig_nm = trig_nm^"_rcv_fetch"
let send_push_name_of_t p trig_nm stmt_id map_id =
      trig_nm^"_send_push_"^(string_of_int stmt_id)^"_"^(map_name_of p map_id)
let rcv_put_name_of_t p trig_nm = trig_nm^"_rcv_put"
let rcv_put_args_of_t p trig_nm = 
  ATuple((("stmt_id_cnt_list", 
    (wrap_tlist (wrap_ttuple [t_int; t_int])))::
    (args_of_t_with_v p trig_nm)))
let rcv_push_name_of_t p trig_nm stmt_id map_id =
      trig_nm^"_rcv_push_"^(string_of_int stmt_id)^"_"^(map_name_of p map_id)
let do_complete_name_of_t p trig_nm stmt_id =
  trig_nm^"_do_complete_"^(string_of_int stmt_id)

(* route and shuffle function names *)
let route_for p map_id = "route_to_"^(map_name_of p map_id)
let shuffle_for p stmt_id rhs_map_id lhs_map_id = 
  "shuffle_"^(string_of_int stmt_id)^(map_name_of p rhs_map_id)^
  (map_name_of p lhs_map_id)

(* k3 functions needed *)
(*
"route_to_map_"^map_id returns ip
"shuffle_map_"^map_id^"_to_map_"map_id takes maybe tuples and a pattern maybe
  tuple and returns a tuple
"promote_address" -> Local() -> ip -> address_t
 *)

let send_fetch_trigger p trig_name =
  let send_fetches_of_rhs_maps  =
    (mk_iter
      (mk_lambda 
        (ATuple["ip", t_int;
          "stmt_map_ids", wrap_tlist (wrap_ttuple [t_int; t_int])]
        )
        (mk_send 
          (mk_apply 
            (mk_var "promote_address")
            (mk_tuple [mk_var (rcv_fetch_name_of_t p trig_name); mk_var "ip"])
          ) 
          (mk_tuple 
            ((args_of_t_as_vars_with_v p trig_name)@
              [mk_var "stmt_map_ids"]
            )
          )
        )
      )
      (mk_gbagg
        (mk_assoc_lambda (* Agg function *)
          (ATuple(["stmt_id", t_int; "map_id", t_int; 
            "ip", ip_type])
          )
          (AVar("acc", wrap_tlist (wrap_ttuple [t_int; t_int])))
          (mk_combine
            (mk_var "acc")
            (mk_singleton 
              (wrap_ttuple [t_int; t_int])
              (mk_tuple [mk_var "stmt_id";mk_var "map_id"])
            )
          )
        ) 
        (mk_lambda (* Grouping function *)
          (ATuple(["stmt_id", t_int; "map_id", t_int; "ip", ip_type]))
          (mk_var "ip")
        )
        (mk_empty (wrap_tlist (wrap_ttuple [t_int; t_int])))
        (* [] *)
        (List.fold_left
          (fun acc_code (stmt_id, rhs_map_id) ->
            let route_fn = route_for p rhs_map_id in
            let key = partial_key_from_bound p stmt_id rhs_map_id in
            (mk_combine
              (mk_map 
                (mk_lambda (AVar("ip", t_int))
                  (mk_tuple 
                    [mk_const (CInt(stmt_id)); mk_const (CInt(rhs_map_id)); 
                      mk_var "ip"
                    ]
                  )
                )
                (mk_apply 
                  (mk_var route_fn)
                  (mk_tuple (key))
                )
              )
              acc_code
            )
          )
          (mk_empty (wrap_tlist (wrap_ttuple [t_int; t_int])))
          (over_stmts_in_t p read_maps_of_stmt trig_name) 
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
              (mk_tuple
                (args_of_t_as_vars p trig_name)
              )
            )
          )
          (mk_apply (mk_var route_fn)
            (mk_tuple key)
          )
        ]
    ) 
    []
    (List.map 
      (fun (stmt_id:stmt_id_t) -> 
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
      (ATuple(["ip", ip_type; "stmt_id_cnt_list", wrap_tlist (wrap_ttuple
        [t_int; t_int])]))
      (mk_send
        (mk_apply
          (mk_var "promote_address")
          (mk_tuple
            [mk_var (rcv_put_name_of_t p trig_name); mk_var "ip"]
          )
        )
        (mk_tuple ((mk_var "stmt_id_cnt_list")::
          (args_of_t_as_vars_with_v p trig_name))
        )
      )
    )
    (mk_gbagg
      (mk_assoc_lambda (* agg func *)
        (ATuple(["ip", ip_type; "stmt_id", t_int; "count", t_int]))
        (AVar("acc", wrap_tlist (wrap_ttuple [t_int; t_int])))
        (mk_combine
          (mk_var "acc")
          (mk_singleton
            (wrap_ttuple [t_int; t_int])
            (mk_tuple [mk_var "stmt_id"; mk_var "count"])
          )
        )
      )
      (mk_lambda (* grouping func *)
        (ATuple(["ip", ip_type; "stmt_id", t_int; "count", t_int]))
        (mk_var "ip")
      )
      (mk_empty
        (wrap_tlist (wrap_ttuple [t_int; t_int]))
      )
      (mk_gbagg (* inner gba *)
        (mk_assoc_lambda (* agg func *)
          (ATuple(["ip", ip_type; "stmt_id", t_int]))
          (AVar("acc", t_int)) 
          (mk_add
            (mk_var "acc")
            (mk_const (CInt(1)))
          )
        )
        (mk_lambda (* group func *)
          (ATuple(["ip", ip_type; "stmt_id", t_int]))
          (mk_tuple
            [mk_var "ip"; mk_var "stmt_id"]
          )
        )
        (mk_const (CInt(0))) (* [] *)
        (List.fold_left
          (fun acc_code (stmt_id, ((rhs_map_id:map_id_t), (lhs_map_id:map_id_t))) ->
            let shuffle_fn = shuffle_for p stmt_id rhs_map_id lhs_map_id in
            let key = partial_key_from_bound p stmt_id lhs_map_id in
            (* we need the types for creating empty rhs tuples *)
            let rhs_map_types = map_types_for p rhs_map_id in
            let lhs_map_types = map_types_for p lhs_map_id in
            (mk_combine
              acc_code
              (mk_map
                (mk_lambda
                  (ATuple(["ip", t_int; 
                    "tuples", wrap_ttuple lhs_map_types])
                  )
                  (mk_tuple
                    [mk_var "ip"; mk_const (CInt(stmt_id))]
                  )
                )
                (mk_apply
                  (mk_var shuffle_fn)
                  (mk_tuple 
                    (key@
                      [mk_empty 
                        (wrap_tlist (wrap_ttuple rhs_map_types))
                      ]@
                      [mk_const (CBool(true))]
                    )
                  ) (* mk_tuple *)
                ) (* mk_apply *)
              ) (* mk_map *)
            ) (* mk_combine *)
          ) (* fun *)
          (mk_empty (* [] *)
            (wrap_tlist (wrap_ttuple [t_int; t_int]))
          )
          (over_stmts_in_t p lhs_rhs_of_stmt trig_name)
        )
      )
    )
in
(* Actual SendFetch function *)
Trigger(
  send_fetch_name_of_t p trig_name,
  ATuple(args_of_t_with_v p trig_name),
  [], (* locals *)
  (mk_block
    (send_fetches_of_rhs_maps::
      (send_completes_for_stmts_with_no_fetch@
      [send_puts])
    )
  )
)
   

(* Trigger_send_push_stmt_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 * Produces a list of triggers.
 *)
let send_push_stmt_map_funcs p trig_name = 
  (List.fold_left
    (fun acc_code (stmt_id, (lhs_map_id, (rhs_map_id:map_id_t))) ->
      let rhs_map_types = wrap_tlist (wrap_ttuple 
        (map_types_for p rhs_map_id)) in 
      let rhs_map_name = map_name_of p rhs_map_id in
      let shuffle_fn = shuffle_for p stmt_id rhs_map_id lhs_map_id in
      let rkey = partial_key_from_bound p stmt_id rhs_map_id in
      acc_code@
      [Trigger (send_push_name_of_t p trig_name stmt_id rhs_map_id, 
        ATuple(args_of_t_with_v p trig_name),
        [] (* locals *),
          (mk_iter
            (mk_lambda (ATuple(["ip", t_int; "tuples", rhs_map_types]))
              (mk_send
                (mk_apply
                  (mk_var "promote_address")
                  (mk_tuple [mk_var (rcv_push_name_of_t p trig_name stmt_id
                    rhs_map_id); mk_var "ip"]
                  )
                )
                (mk_tuple ((mk_var "tuples")::(args_of_t_as_vars p trig_name)))
              )
            )
            (mk_apply
              (mk_var shuffle_fn)
              (mk_tuple
                (rkey@
                  [mk_slice 
                    (mk_var rhs_map_name) 
                    (mk_tuple ((mk_var "vid")::rkey))
                  ]@
                  [mk_const (CBool(false))]
                )
              )
            )
          ) (* mk_iter *)
        )
      ] (* Trigger *)
    ) (* fun *)
    []
    (over_stmts_in_t p lhs_rhs_of_stmt trig_name)
  ) 


(* Debug -----
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
let t_rcv_fetch p trig_name =
  Trigger(
    rcv_fetch_name_of_t p trig_name, 
    ATuple((("stmts_and_map_ids", 
      wrap_tlist (wrap_ttuple [t_int; t_int]))::
      (args_of_t_with_v p trig_name))),
    [], (* locals *)
    (let log_fn = "log_write_"^trig_name in (* varies by bound vars *)
      (mk_block
        [mk_apply
          (mk_var log_fn)
          (mk_tuple
            (mk_const CString(trig_name))::(args_of_t_as_vars_with_v trig_name)
          )
        ;
        (* invoke generated fetch triggers, which in turn send pushes. *)
        (mk_iter
          (mk_lambda
            (ATuple(["stmt_id", BaseT(TInt); "map_id", BaseT(TInt)]))
            (* this send is not polymorphic. every fetch trigger expects
             * the same set of bound variables. *)
            (mk_send
              (send_push_name_of_t trig_name stmt_id map_id)
              (args_of_t_as_vars_with_v trig_name)
            )
          )
          (mk_var "stmts_and_map_ids")
        )]
      )
    )
  )
              
 
(* on_insert_trigger_push_stmt_map
 * --------------------------------------
 * Receive a push at a node
 * Also called for virtual pushes: local data transfers to allow tracking of 
 * present data w/ counters params can be moved to the put statement, but it's
 * a good reminder to have it here 
 * A later optimization could be lumping maps between statements in a trigger *)

let rcv_push trig_name = 
List.fold_left
  (fun acc_code (stmt_id, read_map_id) ->
     let read_map_name = map_name_of read_map_id in
     let buffer_fn = "add_"^read_map_name in
     acc_code@
     Trigger(rcv_push_name_of_t trig_name,
       ATuple(("tuples", map_types_for read_map_id)::
         (args_of_t_with_v trig_name)), 
       [], (* locals *)
       (mk_block
          [mk_apply (* write to buffer *) 
            (mk_var buffer_fn)
            (mk_var "tuples")
            (mk_var "vid")
          ;
          (* check statment counters to see if we can process *)
          let stmt_counters_pattern =
              (mk_tuple [mk_var "vid"; mk_var "stmt_id"; mk_val "_"]) in
          let stmt_cntrs = mk_var "stmt_counters" in
          let stmt_counters_slice = mk_slice stmt_counters stmt_counters_pattern
          in
          mk_ifthenelse
            (mk_has_member (* check if the counter exists *)
              stmt_cntrs
              stmt_counters_pattern
              (BaseT(TTuple[BaseT(TInt)]))
            )
            (mk_block
              [mk_update
                stmt_cntrs
                stmt_counters_slice
                (mk_add
                  stmt_counters_slice
                  (mk_const CInt(-1))
                )
              ;mk_ifthenelse
                (mk_eq
                  stmt_counters_slice
                  (mk_const CInt(0))
                ) 
                (* Send to local do_complete *)
                (mk_send
                  (Local(do_complete_name_of_t trig_name))
                  (args_of_t_with_v trig_name)
                )
                (mk_const CUnit)
              ]
            )
            (mk_update (* else *)
              stmt_counters_slice
              (* Initialize if the push arrives before the put. *)
              mk_const CInt(-1)
            )
          ]
        )
     )
  )
  [] (* empty code *)
  (over_stmts_in_t read_maps_in_stmt trig_name)
    
(* do_complete_stmt_id
 * --------------------------------------
 * Functions that perform the actual calculations of the statement
 * And are called mostly by pushes. 
 * We generate them per statement so they can handle the data types *)

(* Generate code to send data for all possible correctives *)
(* We send the new lhs_map results to any statement that could be affected *)
(* TODO: just generate this all over the code without connection to a stmt but
 * to maps *)
(* delta_tuples is in the format of the lhs_map *)

let trigger_stmt_and_rhs_map_list rhs_map_id =
  (List.map (* lhs_maps of stmts with given rhs_map *)
    (fun (trigger, stmt_id) -> (trigger, stmt_id, lhs_map_of_stmt stmt_id))
    (List.filter
      (fun (trigger, stmt_id) -> stmt_has_rhs_map stmt_id rhs_map_id)
      (List.map
        (fun trigger -> (triggers_stmts_in_trigger trigger))
        (triggers)
      )
    )
  )
in
let lhs_maps_in_trigger trigger_id =
    List.map 
      (fun stmt_id -> lhs_map_of_stmt stmt_id) 
      (stmts_in_trigger trigger_id)
in
let forward_correctives_lambda map_id tuple_types = 
  let forward_name = "forward_correctives_"^(map_name_of map_id) in
  mk_global forward_name  
    [("delta_tuples", map_types_of map_id); ("vid", BaseT(TInt))]
    [TUnit] (* return type *)
    (mk_let "corrective_list" (* (stmt_id * vid) list *)
      (BaseT(TCollection(TList, BaseT(TTuple([BaseT(TInt); BaseT(TInt)]))))) 
      (mk_apply
        (mk_var "get_corrective_list")
        (mk_tuple [mk_const CInt(map_id); mk_var "vid"])
      )
      (mk_iter  (* loop over corrective list *)
        (mk_lambda
          (ATuple(["stmt_id", BaseT(TInt); "vid", BaseT(TInt)])) 
          (List.fold_left  
            (* loop over all possible read map matches *)
            (fun acc_code (target_trigger, target_stmt, rhs_map_id) ->
              let corrective_addr = 
                target_trigger^"_corrective_"^target_stmt^"_"^rhs_map_id 
              in
              let target_bound = bound_vars_for_t target_trigger in
              let key = partial_key_from_bound target_stmt rhs_map_id target_bound 
              in
              let shuffle_fn = 
                shuffle_for rhs_map_id (lhs_map_of_stmt stmt_id) in
              let log_get_bound_fn = "log_get_bound"^target_trigger 
              in
              mk_if
                (mk_eq
                  (mk_var "stmt_id")
                  (mk_const CInt(stmt_id))
                )
                (mk_iter (* if match, send data *)
                  (mk_lambda 
                    (ATuple(["ip", BaseT(TInt); "delta_tuples", tuple_types]))
                    (mk_send
                      (mk_apply
                        (mk_var promote_address)
                        (mk_tuple
                          [mk_var corrective_addr; mk_var "ip"]
                        )
                      )
                      (mk_var "delta_tuples")
                    )
                  )
                  (mk_let_many (target_bound)
                    (mk_apply (* get bound vars from log *)
                      (mk_var log_get_bound_fn)
                      (mk_var "vid")
                    )
                    (mk_apply
                      (mk_var shuffle_fn
                        (mk_tuple
                          (key)@
                          (mk_var "delta_tuples")@
                          (mk_const CBool(false))
                        )
                      )
                    )
                  )
                )
                (acc_code) (* just another branch on the if *)
            )
            (mk_const CUnit) (* base case *)
            (trigger_stmt_and_rhs_map_list map_id)
          )
        )
        (mk_var "corrective_list")
      )
    )
in
let forward_correctives =
  List.fold_left (* loop over all possible write maps in this trigger(program) *)
    (fun acc_code map_id ->
      let tuple_types = types_for_map map_id in
        forward_correctives_lambda map_id tuple_types
    )
    (lhs_maps_in_trigger trigger_id)
;;

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

(* get_corrective_list: 
 * ---------------------------------------
 * Return list of corrective statements we need to execute by checking
 * which statements were performed after time x that used a particular map 
 * The only reason for this function is that we may have the same stmt
 * needing to execute with different vids
 * Optimization TODO: check also by ranges within the map.
 *)
let get_corrective_list = mk_global "get_corrective_list"
  [("map_id", BaseT(TInt)); ("vid", BaseT(TInt))]
  [(BaseT(TCollection(TList, TTuple([BaseT(TInt); BaseT(TInt)]))))] 
  (* (stmt_id, vid) list *)
  (mk_sort (* sort so that early vids are first for performance *)
    (mk_filtermap (* filter out irrelevant statements *)
      (mk_lambda (* filter func *)
        (ATuple(["stmt_id", BaseT(TInt); "vid", BaseT(TInt)]))
        (mk_apply
          (mk_var "stmt_has_rhs_map") (* TODO *)
          (mk_tuple
            [mk_var "stmt_id"; mk_var "map_id"]
          )
        )
      )
      (mk_lambda (* identify map func *)
        (ATuple(["stmt_id", BaseT(TInt); "vid", BaseT(TInt)]))
        (mk_tuple
          [mk_var "stmt_id"; mk_var "vid"]
        )
      )
      (mk_flatten
        (mk_map
          (mk_lambda
            (ATuple(["trigger_id", BaseT(TInt); "vid", BaseT(TInt)]))
            (mk_map
              (mk_lambda
                (AVar("stmt_id", BaseT(TInt)))
                (mk_tuple
                  [mk_var "stmt_id"; mk_var "vid"]
                )
              )
              (mk_apply
                (mk_var "stmts_of_trigger") (* TODO *)
                (mk_var "trigger_id")
              )
            )
          )
          (mk_apply
            (mk_var "log_read_geq") (* TODO *)
            (mk_var "vid")  (* produces triggers >= vid *)
          )
        )
      )
    )
    (mk_assoc_lambda (* compare func *)
      (ATuple(["vid1", BaseT(TInt); "stmt1", BaseT(TInt)]))
      (ATuple(["vid2", BaseT(TInt); "stmt2", BaseT(TInt)]))
      (mk_lt
        (mk_var "vid1")
        (mk_var "vid2")
      )
    )
  )

    
(* receive_correctives:
 * ---------------------------------------
 * Function that gets called when a corrective message is received.
 * Receives bound variables and delta tuples for one map.  *)

let receive_correctives = 
List.fold_left 
  (fun acc_code (stmt_id, map_id) ->
    let map_name = map_name_of map_id in
    let rcv_corrective_name =
      trig_name^"_rcv_corrective_"^stmt_id^"_"^map_name in
    let do_corrective_name =
      trig_name^"_do_corrective_"^stmt_id^"_"^map_name in
    let add_delta_to_buffer_fn = "add_delta_to_buffer_"^map_name in

    acc_code@ (* TODO : do this properly *)

    Trigger(rcv_corrective_name,
      ATuple(["delta_tuples", map_types_of map_id; "vid", BaseT(TInt)]), (* TODO *)
      [], (* locals *)
      (let bound_names_types = bound_names_types_for_t trigger_id in (* TODO *)
        let bound_vars = to_vars bound_type_names in (* TODO *)
        let log_get_bound_fn = "log_get_bound"^trigger_id in

        mk_block
          (* accumulate delta for this vid and all following vids *)
          (* NOTE: this is one thing we can't do efficiently with local collections *)
          [mk_apply
            (mk_var add_delta_to_buffer_fn)
            (mk_tuple
              [mk_var "delta_tuples"; mk_var "vid"]
            )
          ;
           
          mk_let "corrected_updates"
            (BaseT(TCollection(TList, BaseT(TInt)))) (* vid list *)
            (mk_filtermap
              (mk_lambda  (* find newer vids with counter=0 *)
                (ATuple(["vid2", BaseT(TInt); "stmt2", BaseT(TInt); 
                  "cnt", BaseT(TInt)]))
                (mk_and
                  (mk_leq
                    (mk_var "vid")
                    (mk_var "vid2")
                  )
                  (mk_eq
                    (mk_var "cnt")
                    (mk_const CInt(0))
                  )
                )
              )
              (mk_lambda 
                (ATuple(["vid2", BaseT(TInt); "stmt2", BaseT(TInt); 
                  "cnt", BaseT(TInt)]))
                (mk_var "vid")
              )
              (mk_slice
                (mk_var "stmt_counters")
                (mk_tuple
                  [mk_var "_"; mk_const CInt(stmt_id); mk_var "_"]
                )
              )
            )
            (mk_let_many bound_names_types
              (mk_apply
                log_get_bound_fn
                (mk_var "vid")
              )
              (* Send messages to do_complete_correctives: we might have several
               * vids to send to *)
              (mk_iter
                (mk_lambda
                  (AVar("vid", BaseT(TInt)))
                  (mk_apply (* NOTE: instead of send *)
                    (mk_var "do_corrective_addr")
                    (mk_tuple
                      (mk_var "delta_tuples"@bound_vars@[mk_var "vid"])
                    )
                  )
                )
                (mk_var "corrected_updates")
              )
            )
          ]
      )
    )
  )
  [] (* TODO: do this properly *)
  (over_stmts_in_t rhs_maps_of_stmt trigger_id)
;;

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

(* Function to generate the whole distributed program *)
let gen_dist p ast =
  let triggers = get_trig_list p in
  List.map
  (fun trig_nm -> send_fetch_trigger p trig_nm)
  triggers


