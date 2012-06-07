(* Functions that take K3 code and generate distributed K3 code *)

(* Basic types *
 * map_id is an int referring to a specific map
 * map_name is another way to refer to a map
 * stmt_id is a unique id:int for each statement in the program
 * trigger_name is the name of a trigger and is what we usually use to refer to
 *   triggers.
 * trigger_id is a unique id:int for each trigger. We need it only inside K3
 *   code for efficiency. 
 *)

(* Assumptions:
  * We assume a rhs map can only occur once per statement 
  *)
 
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

(* TODO: - declare local buffers for node 
 *       - add rcv_put trigger
 *       - key, pat -> just pat
 *       - make sure shuffle only deals with RHS types (it's just splitting
 *       them)
 *       - may need some types that are subsets of the maps rather than whole
 *       map types. need functions to deal with this.
 *)

Exception ProcessingFailed of string;;

(* set type of vid here. Currently it's TInt, but we may need something better
 * *)
let vid_type = BaseT(TInt)

(* Needed extraction functions: *)
let trigger_id_for_name trig_name = ;; (* TODO *)
let args_of_t trig_name = ;; (* TODO *)
let route_for map_id = ;;
let shuffle_for rhs_map_id lhs_map_id = ;;
let over_stmts_in_t stmt_func trig_name = ;; (* output (stmt_id, stmt) list *)
let read_maps_of_stmt stmt_id = ;; 
let key_and_pat_from_bound stmt_id map_id bound_k3_var_names 
  -> map_tuple, pat_list;;
let stmts_without_rhs_maps_of_t trig_name = ;;
let lhs_rhs_of_stmt stmt_id = ;; 
let map_name_of map_id = ;;
let map_types_for map_id = ;;
let read_maps_of_stmt stmt_id = ;;
let stmts_of_trigger trig_nm = ;;

let name_and_args_of_t trigger_ast =  (* extract name and args from AST *)
    match trigger_ast with
    | Trigger(name, ATuple(args), _, _) -> (name, args)
    | Trigger(name, AVar(arg), _, _) -> name, [arg]
    | _ -> raise ProcessingFailed("Trigger tag not found")
;;


(* argument manipulation convenience functions *)
let extract_arg_types = List.Map (fun (_,typ) -> typ);;
let extract_arg_names = List.Map (fun (nam,_) -> nam);;
let convert_names_to_vars = List.Map (fun x -> mk_var x);;

let arg_types_of_t trig_nm = 
  extract_arg_types (trig_args_of trig_nm)
;;
let arg_names_of_t trig_nm =
  extract_arg_names (trig_args_of trig_nm)
;;
let args_of_t_as_vars trig_nm = 
   convert_names_to_vars (trig_names_of_t trig_nm)
;;

let args_of_t_with_v trig_nm = (args_of_t trig_nm)@("vid", vid_type)
;;
let arg_types_of_t_with_v trig_nm = (arg_types_of_t trig_nm)@vid_type
;;
let args_of_t_as_vars_with_v trig_nm = 
  (args_of_t_as_vars trig_nm)@(mk_var "vid")
;;

(* trigger info needed for generated triggers and sends *)
let send_fetch_name_of_t trig_nm = trig_nm^"_send_fetch"
;;
let send_fetch_args_of_t trig_nm = 
  Atuple(trig_args_with_v_of trig_nm)
;;
let send_fetch_arg_types_of_t = extract_arg_types send_fetch_args_of_t
;;
let rcv_fetch_name_of_t trig_nm = trig_nm^"_rcv_fetch";;
let rcv_fetch_args_of_t trig_nm = 
  ATuple(("stmts_and_map_ids", 
    BaseT(TCollection(TList, TTuple([BaseT(TInt); BaseT(TInt)]))))::
      (args_of_t_with_v trig_nm))
;;
let rcv_fetch_arg_types_of_t = extract_arg_types rcv_fetch_args_of_t 
;;
let rcv_put_name_of_t trig_nm = trig_nm^"_rcv_put"
;;
let rcv_put_args_of_t trig_nm = 
  ATuple("stmt_id_cnt_list", 
    BaseT(TCollection(TList, BaseT(TTuple([BaseT(TInt); BaseT(TInt)]))))::
    (args_of_t_with_v trig_nm))
;;
let rcv_put_arg_types_of_t = extract_arg_types rcv_put_args_of_t 
;;
let send_push_name_of_t trig_nm stmt_id map_id =
      trig_nm^"_send_push_"^stmt_id^"_^"map_id
;;
let send_push_args_of_t trig_nm = 
  Atuple(trig_args_with_v_of trig_nm)
;;
let send_push_arg_types_of_t = extract_arg_types send_push_args_of_t
;;
let rcv_push_name_of_t trig_nm stmt_id map_id =
      trig_nm^"_rcv_push_"^stmt_id^"_^"map_id
;;
let rcv_push_args_of_t trig_nm =  (* todo - incorrect *)
  Atuple(trig_args_with_v_of trig_nm)
;;
let rcv_push_arg_types_of_t = extract_arg_types rcv_push_args_of_t
;;
let do_complete_name_of_t trig_nm stmt_id = trig_nm^"_do_complete_"^stmt_id
;;
let do_complete_args_of_t trig_nm = ATuple(args_of_t_with_v trig_nm)
;;
let do_complete_arg_types_of_t = extract_arg_types do_complete_args_of_t 
;;

(* k3 functions needed *)
"promote_address" -> Local() -> ip -> address_t


let send_fetch_trigger trigger_name =
  let send_fetches_of_rhs_maps  =
    (mk_iter
      (mk_lambda 
        (ATuple["ip", BaseT(TInt);"stmt_map_ids", BaseT(TCollection(TList,
          BaseT(TTuple([BaseT(TInt), BaseT(TInt)]))))]
        (mk_send 
          (mk_apply 
            (mk_var "promote_address")
            (mk_tuple [Local(rcv_fetch_name_of_t trigger_name); mk_var ("ip")])
          ) 
          (mk_tuple (args_of_t_as_vars_with_v trigger_name)@
            (mk_var "stmt_map_ids")
          )
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
        (mk_empty (BaseT(TCollection(BaseT(TTuple([BaseT(TInt);BaseT(TInt)])))))) 
        (* [] *)
        (List.fold_left
          (fun acc_code (stmt_id, rhs_map_id) ->
            let route_fn = route_for rhs_map_id in 
            let key, pat = 
              key_and_pat_from_bound stmt_id rhs_map_id (arg_names_of_t trigger_name) 
            in
              (mk_combine
                (mk_map 
                  (mk_lambda (AVar("ip", BaseT(TInt)))
                    (mk_tuple 
                      [mk_const (CInt(stmt_id)); mk_const (CInt(rhs_map_id)); 
                        mk_var "ip"
                      ]
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
          (over_stmts_in_t read_maps_of_stmt trigger_name) (* TODO *)
        ) 
      )
    )
in
let send_completes_for_stmts_with_no_fetch =
  List.fold_left
    (fun acc_code (stmt_id, lhs_map_id, complete_trig_name) -> 
      let route_fn = route_for lhs_map_name in
      let key, pat = key_and_pat_from_bound stmt_id lhs_map_id trig_args in
        acc_code@
        [mk_iter 
          (mk_lambda (AVar("ip", BaseT(TInt)))
            (mk_send
              (mk_apply 
                (mk_var "promote_address")
                (mk_tuple 
                  [Local(mk_var complete_trig_name); mk_var "ip"]
                )
              )
              (mk_tuple
                (args_of_t_as_vars trigger_name)
              )
            )
          )
          (mk_apply (mk_var route_fn)
            (mk_tuple [key; pat])
          )
        ]
    ) 
    []
    (List.map 
      (fun stmt_id -> 
        (stmt_id, lhs_map_of_stmt stmt_id, 
        do_complete_name_of_t trigger_name stmt_id)
      )
      (stmts_without_rhs_of_t trigger_name) (* TODO *)
    )
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
            [Local(rcv_put_name_of_t trigger_name); mk_var "ip"]
          )
        )
        (mk_tuple (mk_var "stmt_id_cnt_list")::
          (args_of_t_as_vars_with_v trigger_name)
        )
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
            BaseT(TInt)])
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
            (fun acc_code (_, (rhs_map_id, lhs_map_id)) ->
              let shuffle_fn = shuffle_for rhs_map_id lhs_map_id in (* todo *)
              let key, pat = key_and_pat_from_bound stmt_id lhs_map_id trig_args in
              (* we need the types for creating empty rhs tuples *)
              let rhs_map_types = map_types_for rhs_map_id in (* TODO *)
              let lhs_map_types = map_types_for lhs_map_id in (* TODO *)
              (mk_combine
                acc_code
                (mk_map
                  (mk_lambda
                    (ATuple(["ip", BaseT(TInt); "tuples", lhs_map_types]))
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
          )
          (mk_empty (* [] *)
            (BaseT(TCollection(TList, BaseT(TTuple([BaseT(TInt);
                BaseT(TInt)])))))
          )
          (over_stmts_in_t map_pairs_of_stmt trigger_name) (* TODO: implement *)
        )
      )
    )
in
(* Actual SendFetch function *)
Trigger(
  send_fetch_name_of_t trigger_name,
  send_fetch_args_of_t trigger_name,
  [], (* locals *)
    (mk_block
      (send_fetches_of_rhs_maps::
        (send_completes_for_stmts_with_no_fetch@
        send_puts)
      )
    )
  )
   
(* Trigger_rcv_fetch_stmt_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 * Produces a list of triggers.
 *)
let send_push_stmt_map_funcs trigger_name = 
  (List.fold_left
    (fun acc_code (stmt_id, (lhs_map_id, rhs_map_id)) ->
      let rhs_map_types = map_types_for rhs_map_id in (* todo *)
      let shuffle_fn = shuffle_for rhs_map_id lhs_map_id in
      let rkey, rpat = 
        key_and_pat_from_bound stmt_id rhs_map_id (arg_names_of_t trigger_name) 
      in
      acc_code@
      (Trigger (send_push_name_of_t trigger_name, 
        send_push_args_of_t trigger_name,
        [] (* locals *),
          (mk_iter
            (mk_lambda ATuple(["ip", BaseT(TInt); "tuples", rhs_map_types])
              (mk_send
                (mk_apply
                  (mk_var "promote_address")
                  (mk_tuple [Local(rcv_push_name_of_t trigger_name stmt_id
                    rhs_map_id); mk_var "ip"]
                  )
                )
                (mk_tuple (mk_var "tuples")::(args_of_t_as_vars trigger_name))
              )
            )
            (mk_apply
              (mk_var shuffle_fn)
              (mk_tuple
                (key::pat::
                  (mk_slice rhs_map_id rkey)@[mk_const CBool(false)]
                )
              )
            )
          )
        )
      )
    )
    []
    (over_stmts_in_t lhs_rhs_of_stmts trigger_name)
  ) 


(* on_insert_trigger_rcv_fetch
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
let t_rcv_fetch =
  let fetch_trigger_name = trig_name^"_rcv_fetch" in
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
let do_corrective_name = trigger_name^"_do_complete_"^stmt_id in
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
      (key_and_pat_from_bound stmt_id map_id trig_args)
    in
    inject_call_forward_correctives ast_stmt2 delta_in_lhs_map
  )

