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
 * let stmt_counters : (vid : int, trigger_id: int, stmt_id : int, count : int)
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

let on_push = 
List.fold_left
  (fun acc_code (stmt_id, read_map_id) ->
     let read_map_name = map_name_of read_map_id in
     let push_stmt_map_trigger_name =
         trigger_name^"_push_"^stmt_id^"_"^read_map_name in
     let do_complete_name = trigger_name^"_do_complete_"^stmt_id in
     let buffer_fn = "add_"^read_map_name in
     Trigger(push_stmt_map_trigger_name,
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
let send_correctives map_id delta_var =
  List.fold_left (* loop over all possible write maps in this trigger *)
    (fun acc_code stmt_id
      List.fold_left 
            (fun acc_code trigger_id ->
                let stmts = stmts_of_trigger trigger_id in
                List.fold_left 
                    (fun acc_code stmt_id ->
                        let corrective_addr = trigger_id^"_corrective_"^stmt_id^"_d"^map_id in
                        (* Our lhs_map is now our rhs_map *)
                        if stmt_has_rhs_map stmt_id map_id then
                            let shuffle_fn =
                              shuffle_for map_id (lhs_map stmt_id)
                            in
                            .< ~.acc_code 
                            if corrective_list[.~stmt_id]? then do {
                                iter(\(ip,delta_tuples) ->
                                    send(promote_address(~.corrective_addr, ip), delta_tuples),
                                    .~shuffle_fn(.~key_and_pat_from_bound(stmt_id,
                                        map_id, bound), delta_tuples, false)
                                )
                            }
                            >.
                        else acc_code
                    )
                    acc_code
                    stmts
            )
            .< let corrective_list = get_corrective_list(.~lhs_map_id, vid) in >.
            triggers
    (List.map 
      (fun stmt_id -> lhs_maps_in_stmt stmt_id) 
      (stmts_in_trigger trigger_id))

let do_complete_name = trigger_name^"_do_complete_"^stmt_id in
Trigger
  (do_complete_name, ATuple(trig_args_with_v), [] (* locals *),
    (* in terms of substitution, we need to 
     * a. switch read maps for buffers
     * b. inject a send to the send_correctives trigger by either sending a
     * single delta or sending a cse representing a slice of calculated data *)
    let ast_stmt2 = subst_buffers (ast_of_stmt stmt_id) (read_maps_of_stmt stmt_id)
    in
    let delta_in_lhs_map = to_lhs_map_form (ast_stmt2) (delta_var_of_stmt stmt_id)
    in
    inject_send_to_send_correctives (ast_stmt2 delta_in_lhs_map)
  )


    

