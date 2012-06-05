(* Functions that take K3 code and create more or less the code in pseudocode.ml  *)

(* Notes: PCValueUpdate is now just Update
 *
 *
 *)

(* Basic types *
 * maps are numbered globally and implemented as int
 * trigger is implemented as int
 * statement is implemented as (int * int) or (trigger, stmt) *)

(* Triggers generated ------------------------------------------------- *
 * for each trigger               : on_insert_<trigger_name>_switch
 * for each map read by a trigger : <trigger_name>_<read_map_name>_fetch *)

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

(* Data structures we need -------------------------------------------- *)

(* A data structure holding trigger addresses of fetch triggers per
 trigger and map pair.
 * let fetch_map_triggers : ((int * int) * address_t) list = ... *)

(* A data structure converting from (trigger_id, stmt_id) to do_complete address 
 * let do_complete_stmts : ((int * int) * address_t) list = ... *)

(* Data structure checking which statements within a trigger can be completed when a rhs map's
 * data arrives 
 * let map_possible_completions : 
     * (map_id : int, trigger_id : int, stmt_id : * int) list *)

(* stmt_cnt is a list of counters for each statement keeping track of msgs received. 
 * It's initialized to 0 whenever a statement starts executing (via a put). 
 * let stmt_counters : (vid : int, stmt_id : int, count : int)
 *)

(* 
build_switch
    - metadata: arguments, rhs_map_name, rhs_map_key, rhs_map_pat 

      (* Example:
       * [1,2] -> [Const(CInt(1)); Const(CInt(2))] *)
    - k3_of_int_list : int list -> expr_t list
      
      (* Promotes a trigger address to be remote as necessary.
       * Note, this is a function available in K3 code. *)
    - @K3:promote_address : address_t -> ip_t -> address_t 

      (* Returns an identifier, as a K3 constant, for a K3 statement *)
    - id_of_stmt : expr_t -> expr_t

    - route_for: 
      (* rhs_map_name -> routing function variable *)
      string -> id_t

    - shuffle_for:
      (* Returns a compiled shuffle function for a given pair of maps
       * rhs_map_name -> lhs_map_name -> shuffle_func_name *)
      string -> string -> id_t

    - map_pairs_of_statement:
      (* Returns all pairs of read/write map accesses for a given statement.
       * statement -> (rhs_map_name, rhs_map_key, rhs_map_pat,
       *               lhs_map_name) list
       *)
       statement -> (string, expr_t list, int list, string) list

    - read_maps_of_trigger:
      (* Returns unique map accesses across all statements in the trigger
       * trigger -> (rhs_map_name, rhs_map_key, rhs_map_pat) list
       *) 
      trigger -> (string, expr_t list, int list) list

    - direct_completions:
      (* Returns a list of completion statements, separated into their
       * own triggers for execution, along with the map access for
       * the map being updated.
       *
       * trigger -> (lhs_map_name, lhs_map_key, lhs_map_pat,
       *             completion_trigger_addr) list
       *)
      trigger -> (string * expr_t list * int list * address_t) list
   
    - stmts_of_trigger: 
      (* return the list of statements for a given trigger *)
      trigger -> statement list : (int * int) list
     
    - stmt_has_rhs_map: 
      (* return whether a statement has a particular map on the rhs *)
      statement -> map_id -> bool

    - map_name_of map_id:
      (* return map name of a given map id *)
 *)

(* on_insert_<trigger>_switch:
 * -----------------------------------------------
 * The switch starts the process
 *)
(* we assume at this point that we receive the trigger *)
(* TODO: declare local buffers for node *)

Exception ProcessingFailed of string;;

(* extract trigger name and args *)
let (trigger_name, trig_args) = match trigger with
    | Trigger(name, ATuple(args), _, _) -> (name, args)
    | _ -> raise ProcessingFailed("Trigger not found")
;;

(* extract argument types *)
let trig_arg_types = List.Map (fun (_,typ) -> typ) trig_args;;
let trig_arg_names = List.Map (fun (nam, _) -> nam) trig_args;;
let trig_arg_vars = List.Map (fun x -> mk_var x) trig_args;;
let trig_args_with_v = trig_args@("vid", BaseT(TInt));; 

let send_fetches_rhs =
  (* send fetches for all maps appearing on statement RHS.*)
  (mk_iter
    (mk_lambda 
      (ATuple["ip", BaseT(TInt);"stmt_map_ids", BaseT(TCollection(TList,
        BaseT(TTuple([BaseT(TInt), BaseT(TInt)]))))]
      )
      (mk_send 
        (mk_apply 
          (mk_var "promote_address")
          (mk_tuple [Local(trigger_name^"_Fetch"); mk_var ("ip")])
        )
        (mk_tuple [mk_var "bound_with_v"; mk_var "stmt_map_ids"])
      )
    )
    (mk_groupbyaggregate
      (mk_assoc_lambda (* Agg function *)
        (ATuple(["stmt_id", BaseT(TInt); "map_id", BaseT(TInt); 
          "ip",BaseT(TInt)])
        )
        (AVar("acc", BaseT(TCollection(TList, BaseT(TTuple([BaseT(TInt);
          BaseT(TInt)])))))
        )
        (mk_combine
          (mk_var "acc")
          (mk_singleton 
            (mk_tuple [mk_var "stmt_id";mk_var "map_id"])
          )
        )
      ) 
      (mk_lambda (* Grouping function *)
        (ATuple([("stmt_id", BaseT(TInt); ("map_id", BaseT(TInt)); 
          ("ip", BaseT(TInt)))]))
        (mk_var "ip")
      )
      (mk_empty (BaseT(TCollection(BaseT(TTuple([BaseT(TInt);BaseT(TInt)])))))) (* [] *)
      (List.fold_left
        (fun acc_code (stmt_id, rhs_map_id) ->
          let route_fn = route_for rhs_map_id in 
          (* note: assumes max one instance of a map per statement *)
          let key, pat = key_and_pat_from_bound stmt_id rhs_map_id trig_args in
            (mk_combine
              (mk_map 
                (mk_lambda (AVar("ip", BaseT(TInt)))
                  (mk_tuple 
                    [mk_const (CInt(stmt_id)); mk_const (CInt(rhs_map_id)); mk_var("ip")]
                  )
                )
                (mk_apply 
                  (mk_var route_fn)
                  (mk_tuple [key; pat])
                )
              )
              acc_code
            )
        )
        (mk_empty (BaseT(TCollection(TList, BaseT(TTuple([BaseT(TInt);
          BaseT(TInt)]))))))
        (over_stmts_in_t read_maps_of_stmt trigger_name)
      ) 
    )
  )
in
let send_completes_no_fetch =
  (* send completes for statements that do not perform fetch. *)
  List.fold_left
    (fun acc_code 
      (stmt_id, lhs_map_id, complete_trig_addr) -> 
      let route_fn = route_for lhs_map_name in
      let key, pat = key_and_pat_from_bound stmt_id lhs_map_id trig_args in
        acc_code@
        [mk_iter 
          (mk_lambda (AVar("ip", BaseT(TInt)))
            (mk_send
              (mk_apply 
                (mk_var "promote_address")
                (mk_tuple 
                  [mk_var "complete_trig_addr"; mk_var "ip"]
                )
              )
              (mk_var "bound_with_v")
            )
          )
          (mk_apply (mk_var route_fn)
            (mk_tuple [key; pat])
          )
        ]
    ) 
    []
    (direct_completions trigger_name) (* need to implement *)
in
let send_puts =
  (* send puts
   * count is generated by counting the number of messages going to a
   * specific IP *)

  mk_iter
    (mk_lambda 
      (ATuple(["ip", BaseT(TInt); "stmt_id_cnt_list", BaseT(TCollection(TList,
        BaseT(TTuple([BaseT(TInt); BaseT(TInt)]))))]))
      (mk_send
        (mk_apply
          (mk_var "promote_address")
          (mk_tuple
            [mk_var trigger_name^"_Put"; mk_var "ip"]
          )
        )
        (mk_tuple [mk_var "bound_with_v"; mk_var "stmt_id_cnt_list"])
      )
      (mk_groupbyaggregate
        (mk_assoc_lambda (* agg func *)
          (ATuple(["ip", BaseT(TInt); "stmt_id", BaseT(TInt); "count",
            BaseT(TInt)])
          )
          (AVar("acc", BaseT(TCollection(TList, BaseT(TTuple([BaseT(TInt); 
             BaseT(TInt)]))))) 
          )
          (mk_combine
            (mk_var "acc")
            (mk_singleton
              (mk_tuple [mk_var "stmt_id"; mk_var "count"])
            )
          )
        )
        (mk_lambda (* grouping func *)
          (ATuple(["ip", BaseT(TInt); "stmt_id", BaseT(TInt); "count",
            BaseT(TInt))
          )
          (mk_var "ip")
        )
        (mk_empty
          (BaseT(TCollection(TList, BaseT(TTuple([BaseT(TInt); 
            BaseT(TInt)])))))
        )
        (mk_groupbyaggregate (* inner gba *)
          (mk_assoc_lambda (* agg func *)
            (ATuple(["ip", BaseT(TInt); "stmt_id", BaseT(TInt)]))
            (AVar("acc", BaseT(TInt))) 
            (mk_add
              (mk_var "acc")
              (mk_const CInt(1))
            )
          )
          (mk_lambda (* group func *)
            (ATuple(["ip", BaseT(TInt); "stmt_id", BaseT(TInt)]))
            (mk_tuple
              [mk_var "ip"; mk_var "stmt_id"]
            )
          )
          0
          (List.fold_left
            (fun acc_code stmt_id ->
              (List.fold_left
                (fun acc_code (rhs_map_id, lhs_map_id) ->
                  let shuffle_fn = shuffle_for rhs_map_name lhs_map_name in (* todo *)
                  let key, pat = key_and_pat_from_bound stmt_id lhs_map_id trig_args in
                  (* we need the types for creating empty rhs tuples *)
                  let rhs_map_types = map_types_for rhs_map_id in (* todo *)
                  let lhs_map_types = map_types_for lhs_map_id in (* todo *)
                  (mk_combine
                    acc_code
                    (mk_map
                      (mk_lambda
                        (ATuple(["ip", BaseT(TInt); "tuples", lhs_map_types)])
                        (mk_tuple
                          [mk_var "ip"; mk_const CInt(stmt_id)]
                        )
                      )
                      (mk_apply
                        (mk_var shuffle_fn)
                        (mk_tuple 
                          [key; pat; 
                            mk_empty BaseT(TCollection(TList, rhs_map_types));
                            mk_const CBool(true)
                          ]
                        )
                      )
                    )
                  )
                )
                acc_code
                (map_pairs_of_statement stmt)
              )
              (mk_empty (* [] *)
                (BaseT(TCollection(TList, BaseT(TTuple([BaseT(TInt);
                    BaseT(TInt)])))))
              )
              (stmts_of_trigger trigger_name) (* TODO: implement *)
            )
          )
        )
      )
    )
in
(* Actual Switch function *)
let t_switch = 
Trigger(trigger_name^"_Switch", 
  Atuple(trig_args@("vid", BaseT(TInt))), (* args *)
  [], (* locals *)
(mk_let "bound" (BaseT(TTuple(trig_arg_types))) 
  (mk_tuple(trig_args_vars))
  (mk_let "bound_with_v" (BaseT(TTuple(trig_arg_types@BaseT(TInt))))
    (mk_tuple(trig_args_vars@(mk_var "vid")))
    (mk_block
      (send_fetches_rhs::
        send_completes_no_fetch@
        send_puts
      )
    )
  )
)
   
(* Trigger_fetch_stmt_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 *)
let t_fetch_stmt_map_funcs = 
  List.fold_left
    (fun acc_code (stmt_id, rhs_map_id, lhs_map_id) ->
      let rhs_map_name = map_name_of rhs_map_id in
      let lhs_map_name = map_name_of lhs_map_id in
      let rhs_map_types = map_types_for rhs_map_id in (* todo *)
      let lhs_map_types = map_types_for lhs_map_id in (* todo *)
      let fetch_map_trigger_name = trigger_name^"_Fetch_"^stmt_id^"_"^rhs_map_name in
      let push_map_trigger_name = trigger_name^"_Push_"^stmt_id^"_"^rhs_map_name in
      let shuffle_fn = shuffle_for rhs_map_name lhs_map_name in
      let slice_fn = slice_^rhs_map_name in
      let lkey, lpat = key_and_pat_from_bound stmt_id lhs_map_id trig_args in
      let rkey, rpat = key_and_pat_from_bound stmt_id rhs_map_id trig_args in

        acc_code@
        (Trigger
          (fetch_map_trigger_name, ATuple(trig_args@("vid", BaseT(TInt))),
            [] (* locals *),
            (mk_let "bound_with_v" (BaseT(TTuple(trig_arg_types@BaseT(TInt))))
              (mk_tuple(trig_args_vars@(mk_var "vid")))
                (mk_iter
                  (mk_lambda ATuple(["ip", BaseT(TInt); "tuples", lhs_map_types])
                    (mk_send
                      (mk_apply
                        (mk_var "promote_address")
                        (mk_tuple [mk_var push_map_trigger_name; mk_var "ip"])
                      )
                      (mk_tuple [mk_var "tuples"; mk_var "bound_with_v"])
                    )
                  )
                  (mk_apply
                    (mk_var shuffle_fn)
                    (mk_tuple
                      [key; pat]@
                        (mk_apply
                          (mk_var slice_fn)
                          (rhs_map_id, rkey, rpat)
                        )@
                        [(mk_const CBool(false))]
                    )
                  )
                )
            )
          )
        )
        []
        (over_stmts_in_t map_pairs_of_stmts trigger_name)
    ) 


(* on_insert_trigger_fetch
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
let t_fetch =
  let fetch_trigger_name = trig_name^"_Fetch" in
  Trigger(fetch_trigger_name, ATuple("stmts_and_map_ids",
    BaseT(TCollection(TList, TTuple([BaseT(TInt); BaseT(TInt)])))
    ::trig_args@("vid", BaseT(TInt)),
    [], (* locals *)

    let log_fn = "log_write_"^trig_name in (* varies by bound vars *)
    
    (mk_block
      [mk_apply
        (mk_var log_fn)
        (mk_tuple
          [mk_const CString(trig_name); mk_var "bound_with_v"]
        )
      ;

      (* invoke generated fetch triggers, which in turn send pushes. *)
      (mk_iter
        (mk_lambda
          (ATuple(["stmt_id", BaseT(TInt); "map_id", BaseT(TInt)]))
          (* this send is not polymorphic. every fetch trigger expects
           * the same set of bound variables. *)
          (mk_send
            (trig_name^"_"^stmt_id^"_"^(map_name_of map_id))
            (mk_var "bound_with_v")
          )
        )
        (mk_var "stmts_and _map_ids")
      )]
    )
              
(* on_insert_trigger_push_stmt_map
 * --------------------------------------
 * Receive a push at a node
 * Also called for virtual pushes: local data transfers to allow tracking of 
 * present data w/ counters params can be moved to the put statement, but it's
 * a good reminder to have it here 
 * A later optimization could be lumping maps between statements in a trigger *)

let rcv_push = 
List.fold_left
  (fun acc_code (stmt_id, read_map_id) ->
     let read_map_name = map_name_of read_map_id in
     let rcv_push_stmt_map_trigger_name =
         trigger_name^"_push_"^stmt_id^"_"^read_map_name in
     let do_complete_name = trigger_name^"_do_complete_"^stmt_id in
     let buffer_fn = "add_"^read_map_name in
     Trigger(rcv_push_stmt_map_trigger_name,
         ATuple(("tuples", map_types_for read_map_id)::
             trig_args@("vid", BaseT(TInt))),
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
          let stmt_cntrs = (mk_var "stmt_counters") in
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
                  (mk_var do_complete_name)
                  (mk_var "bound_with_v")
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
  (over_stmts_in_t read_maps_in_stmt T_trigger)
    
(* on_insert_T_do_complete_stmt_id
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
          (List.fold_left  (* loop over all possible read map matches *)
            (fun acc_code (target_trigger, target_stmt, rhs_map_id) ->
              let corrective_addr = 
                target_trigger^"_corrective_"^target_stmt^"_"^rhs_map_id 
              in
              let target_bound = bound_vars_for_t target_trigger in
              let key, pat = 
                key_and_pat_from_bound stmt_id rhs_map_id target_bound in
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
                          (key_and_pat_from_bound stmt_id map_id bound)@
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

let do_complete_name = trigger_name^"_do_complete_"^stmt_id in
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
      (key_and_pat_from_bound stmt_id map_id trig_args)
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
      trigger_name^"_rcv_corrective_"^stmt_id^"_"^map_name in
    let do_corrective_name =
      trigger_name^"_do_corrective_"^stmt_id^"_"^map_name in
    let add_delta_to_buffer_fn = "add_delta_to_buffer_"^map_name in

    Trigger(rcv_corrective_name,
      ATuple(["delta_tuples", map_types_of map_id; "vid", BaseT(TInt)]),
      [], (* locals *)
      (let bound_vars = bound_vars_for_t trigger_id in (* TODO *)
        bound_types = bound_types_for_t trigger_id in (* TODO *)
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
          (mk_let_many bound_vars
            bound_types
            (mk_apply
              log_get_bound_fn
              (mk_var "vid")
            )
            (* Send messages to do_complete_correctives: we might have several
             * vids to send to *)
            (mk_iter
              (mk_lambda
                (AVar("vid", BaseT(TInt)))
                (mk_apply
                  (mk_var do_corrective_addr)
                  (delta_tuples@bound_var_names@(mk_var "vid"))
                )
              )
              (mk_var "corrective_updates")
            )


