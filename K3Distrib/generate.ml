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
let trig_args_with_v = trig_args@("vid", BaseT(TFloat));; 

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
            (mk_combine
              (mk_map 
                (mk_lambda (AVar("ip", BaseT(TInt)))
                  (mk_tuple 
                    [Const(CInt(stmt_id)); Const(CInt(rhs_map_id)); mk_var("ip")]
                  )
                )
                (mk_apply 
                  (mk_var route_fn)
                  (k3_of_int_list rhs_map_pat)
                )
              )
              acc_code
            )
        )
        (mk_empty (BaseT(TCollection(TList, BaseT(TTuple([TInt; TInt]))))))
        (read_maps_of_trigger trigger_name)
      ) 
    )
  )
in
let send_completes_no_fetch =
  (* send completes for statements that do not perform fetch. *)
  List.fold_left
    (fun acc_code 
      (lhs_map_name, lhs_map_key, lhs_map_pat, complete_trig_addr) -> 
      let route_fn = route_for lhs_map_name in
        [(mk_iter 
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
            (lhs_map_key) (k3_of_int_list lhs_map_pat)
          )
        )]@acc_code
    ) 
    []
    (direct_completions trigger_name) (* need to implement *)
in
let puts =
  (* send puts
   * count is generated by counting the number of messages going to a
   * specific IP *)

  mk_iter
    (mk_lambda 
      (ATuple(["ip", BaseT(TInt); "id_cnt_list", BaseT(TInt)]))
      (mk_send
        (mk_apply
          (mk_var "promote_address")
          (mk_tuple
            [mk_var trigger_name^"_Put"; mk_var "ip"]
          )
        )
        (mk_tuple [mk_var "bound_with_v"; mk_var "id_cnt_list"])
      )
      (mk_groupbyaggregate
        (mk_assoc_lambda
          (ATuple(["ip", BaseT(TInt); "stmt_id", BaseT(TInt); "count",
            BaseT(TInt)])
          )
          (AVar("acc", BaseT(TCollection(TList, BaseT(TTuple([BaseT(TInt); 
             BaseT(TInt)]))))) 
          )
          (mk_combine
            (mk_var "acc")
            (mk_tuple
              [mk_var "stmt_id"; mk_var "count"]
            )
          )
        )
        (mk_lambda 
          (ATuple(["ip", BaseT(TInt); "stmt_id", BaseT(TInt); "count",
            BaseT(TInt))
          )
          (mk_var "ip")
        )
        (mk_empty
          (BaseT(TCollection(TList, BaseT(TTuple([BaseT(TInt); 
          BaseT(TInt)])))))
        )
        (mk_groupbyaggregate
          (mk_assoc_lambda
            (ATuple(["ip", BaseT(TInt); "stmt_id", BaseT(TInt)]))
            (AVar("acc", BaseT(TInt)) 
            (mk_add
              (mk_var "acc")
              (mk_const CInt(1))
            )
          )
          (mk_lambda
            (ATuple(["ip", BaseT(TInt); "stmt_id", BaseT(TInt)]))
            )
            (mk_tuple
              [mk_var "ip"; mk_var "stmt_id"]
            )
          )
          0
          (List.fold_left
            (fun acc_code stmt ->
              (List.fold_left
                (fun acc_code (rhs_map_name, rhs_map_key, rhs_map_pat,
                rhs_map_types, lhs_map_name) ->
                  let stmt_id = id_of_stmt stmt in  (* implement ***)
                  let shuffle_fn_name = shuffle_for rhs_map_name lhs_map_name in (* todo ***)
                  (mk_combine
                    acc_code
                    (mk_map
                      (mk_lambda
                        (* **** Problem: even though we send only an empty list
                         * of tuples, the type for that list must come from 
                         * somewhere, and we don't have it. Solution: 
                         * get types as well *)
                        (ATuple(["ip", BaseT(TInt); "tuples", rhs_map_types)])
                        (mk_tuple
                          [mk_var "ip"; mk_const CInt(stmt_id)]
                        )
                      )
                      (mk_apply
                        (mk_var shuffle_fn_name)
                        (mk_tuple
                          [mk_const CBool(true); mk_var "bound"; 
                          mk_empty BaseT(TCollection(TList, rhs_map_types))]
                        )
                      )
                    )
                  )
                )
                acc_code
                (* Note: This function also has to return types for rhs map in this case *)
                (map_pairs_of_statement stmt)
              )
              (mk_empty 
                (BaseT(TCollection(TList, BaseT(TTuple([BaseT(TInt),
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
  Atuple(trig_args@("vid", BaseT(TFloat))), (* args *)
  [], (* locals *)
(mk_let "bound" (BaseT(TTuple(trig_arg_types))) (mk_tuple(trig_args_vars))
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
   
(* Trigger_fetch_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 *)
let t_fetch_map_funcs = 
  List.fold_left
    (fun acc_code (rhs_map_name, rhs_map_key, rhs_map_pat, lhs_map_name)
      let fetch_map_trigger_name = trigger_name^"_Fetch_"^rhs_map_name in
      let push_map_trigger_name = trigger_name^"_Push_"^rhs_map_name in
      let shuffle_fn = shuffle_for rhs_map_name lhs_map_name in
        Trigger(fetch_map_trigger_name, ATuple(trig_args@("vid, BaseT(TInt))),
          [] (* locals *),
          (mk_iter
            (mk_lambda ATuple(["ip", BaseT(TInt); "tuples",  
          iter(\(ip, tuples) ->
               send(promote_address(push_map_trigger_name, ip),
                   .~map_name, bound, tuples, vid, false),
               .~shuffle_fn(false, bound, .~map_access))
        ) 
         
         .~acc_code>.)
    empty_code
    (List.flatten 
      (List.map map_pairs_of_statement (stmts_of_trigger T_trigger)))

.<
                                    

(* on_insert_trigger_fetch
 * -----------------------------------------
 * Receive a fetch at a node.
 * Reuses switch-side computation of which maps this node should read.
 * This could be done entirely at the node, but would repeat work done
 * at the switch anyway.
 * The assumption is that the "map_names" data is not large.
 *)
let t_fetch =
  Trigger(trigger_name^"_Fetch", ATuple(["map_names", BaseT(TCollection(TList,
    BaseT(TInt))::trig_args@("vid, BaseT(TInt))]),
    [] (* locals *),

(mk_let "bound" (BaseT(TTuple(trig_arg_types))) (mk_tuple(trig_args_vars))
  (mk_let "bound_with_v" (BaseT(TTuple(trig_arg_types@BaseT(TInt))))
    (mk_block
      [(mk_tuple(trig_args_vars@(mk_var "vid")))
      (mk_apply
        (mk_var "log_operation")
        (mk_tuple ([trigger_name, mk_var "bound_with_v"]))
      )
      ;

      (* invoke generated fetch triggers, which in turn send pushes.*)
      (mk_iter
        (mk_lambda AVar("map_name", BaseT(TInt))
          (* this send is not polymorphic. every fetch trigger expects
           * the same set of bound variables.*)
          (mk_apply
            (mk_var "send")
            (
     

   // invoke generated fetch triggers, which in turn send pushes.
   iter(\map_name ->
          send(fetch_map_triggers["T", map_name], bound, vid),
        map_names)
}
>.

