(* Functions that take K3 code and create more or less the code in pseudocode.ml  *)

(* Notes: PCValueUpdate is now just Update
 *
 *
 *)

(* Basic types *)
maps are numbered globally and implemented as int
trigger is implemented as int
statement is implemented as (int * int) or (trigger, stmt)

(* Triggers generated ------------------------------------------------- *)
for each trigger               : on_insert_<trigger_name>_switch
for each map read by a trigger : <trigger_name>_<read_map_name>_fetch

(* Call Graph *)
on_insert_<trigger>_switch  
    on_put_<trigger>
    do_complete_<trigger> (* no maps *) 
    on_insert_<trigger>_fetch 
        <trigger>_fetch_<rhs_map> -> 
            <trigger>_push_<rhs_map> -> 
                get_completed_stmts
                do_complete_<trigger>_<stmt>
                    get_corrective_list
                    On_corrective_<trigger>_<delta_rhs_map>
                        get_completed_stmts_corrective
                        do_complete_corrective_<trigger>_<stmt>_<delta_rhs_map>
                            On_corrective_<trigger>_<delta_rhs_map>
                            ...

(* Data structures we need -------------------------------------------- *)

(* A data structure holding trigger addresses of fetch triggers per
 trigger and map pair.*)
let fetch_map_triggers : ((int * int) * address_t) list = ...

(* A data structure converting from (trigger_id, stmt_id) to do_complete address *)
let do_complete_stmts : ((int * int) * address_t) list = ...

(* Data structure checking which statements within a trigger can be completed when a rhs map's
 * data arrives *)
let map_possible_completions : (map_id : int, trigger_id : int, stmt_id : int) 
    list

(* stmt_cnt is a list of counters for each statement keeping track of msgs received. 
 * It's initialized to 0 whenever a statement starts executing (via a put). *)
let stmt_counters : (vid : int, trigger_id: int, stmt_id : int, count : int)

(* *)
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

(* on_insert_<trigger>_switch:
 * -----------------------------------------------
 * The switch starts the process
 *)
.<
on_insert_T_switch(QUERY_1_1_pR1_pS1T_T__C, QUERY_1_1_pR1_pS1T_D) {

   let bound = tuple(QUERY_1_1_pR1_pS1T_T__C, QUERY_1_1_pR1_pS1T_D) in
   let bound_with_v = tuple(QUERY_1_1_pR1_pS1T_T__C, QUERY_1_1_pR1_pS1T_D, vid) in
   
   // send fetches for all maps appearing on statement RHS.
   iter(\(ip, map_names) ->
          send(promote_address(fetch_trigger_T,ip),
               bound_with_v, map_names),
     groupby(
       \(map_name, ip, acc) -> combine(acc, [map_name]),
       [],
       \(map_name, ip) -> ip,
         .~ (List.fold_left
           (fun ((rhs_map_name, rhs_map_key, rhs_map_pat), acc_code) ->
             let route_fn = route_for rhs_map_name in
             .<combine(
                 map(\ip -> (.~rhs_map_name, ip),
                     .~route_fn(.~rhs_map_key,
                                .~(k3_of_int_list rhs_map_pat))),
               .~acc_code)>.),
           .<[]>.,
           (read_maps_of_trigger T_trigger))))
   
   // send completes for statements that do not perform a fetch.
   .~ (List.fold_left
        (fun ((lhs_map_name, lhs_map_key, lhs_map_pat, complete_trig_addr),
              acc_code) -> 
          let route_fn = route_for lhs_map_name in
          .<iter(\ip ->
              send(promote_address(complete_trig_addr,ip), bound_with_v),
              .~route_fn(.~lhs_map_key, .~(k3_of_int_list lhs_map_pat)))>.)
        .<[]>.,
        (direct_completions T_trigger))

   // send puts
   // count is generated by counting the number of messages going to a specific IP
   iter(\(ip, id_cnt_list) ->
          send(promote_address(On_put_T,ip), bound_with_v, id_cnt_list)
     groupby(
       \(ip, stmt_id, count, acc) -> acc@[stmt_id, count]
       [], 
       groupby(
         \(ip, stmt_id, acc) -> acc+1,
         0,
        \(ip, stmt_id) -> (ip, stmt_id),
          .~ (List.fold_left
              (fun (acc_code, stmt) ->
                List.fold_left
                  (fun ((rhs_map_name, rhs_map_key, rhs_map_pat, lhs_map_name),
                        acc_code) ->
                    let stmt_id = id_of_stmt stmt in 
                    let shuffle_fn = shuffle_for rhs_map_name lhs_map_name in
                    .<combine(
                         map(\(ip, tuples) -> (ip, .~stmt_id),
                            .~shuffle_fn(true, bound, [])), 
                        .~acc_code)>.)
                  acc_code,
                  (map_pairs_of_statement stmt)),
              .<[]>.,
              (stmts_of_trigger T_trigger)))))
}
>.

(* Trigger_fetch_map
 * ----------------------------------------
 * Generated code to respond to fetches by sending a push message
 * Circumvents polymorphism.
 *)
List.fold_left
  (fun ((rhs_map_name, rhs_map_key, rhs_map_pat, lhs_map_name),
        acc_code) ->
    let fetch_map_trigger_name = trigger_name^"_fetch_"^rhs_map_name in
    let push_map_trigger_name = trigger_name^"_push_"^rhs_map_name  in
    let shuffle_fn = shuffle_for rhs_map_name lhs_map_name in
    let map_access = access_for rhs_map_key rhs_map_pat in
    .<
      .~fetch_map_trigger_name(bound_with_v) {
        iter(\(ip, tuples) ->
             send(promote_address(push_map_trigger_name, ip),
                 .~map_name, bound, tuples, vid, false),
             .~shuffle_fn(false, bound, .~map_access))
       }
       
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
on_insert_T_fetch(map_names, QUERY_1_1_pR1_pS1T_T__C, QUERY_1_1_pR1_pS1T_D, vid) {

   let bound = tuple(QUERY_1_1_pR1_pS1T_T__C, QUERY_1_1_pR1_pS1T_D) in
   let bound_with_v = tuple(QUERY_1_1_pR1_pS1T_T__C, QUERY_1_1_pR1_pS1T_D, vid) in  

   foreign(log_operation, ("T", bound_with_v))

   // invoke generated fetch triggers, which in turn send pushes.
   iter(\map_name ->
          // this send is not polymorphic. every fetch trigger expects
          // the same set of bound variables.
          send(fetch_map_triggers["T", map_name], bound, vid),
        map_names)
}
>.

(* trigger_push_map
 * --------------------------------------
 * Receive a push at a node
 * Also called for virtual pushes: local data transfers to allow tracking of present data w/ counters
 * params can be moved to the put statement, but it's a good reminder to have it here
 *)

(* Note: we need to think about this. Can we really send per map only? *)
List.fold_left
  (fun acc_code (rhs_map_name, rhs_map_key, rhs_map_pat) ->
     let push_map_trigger_name = trigger_name^"_push_"^rhs_map_name  in
     let trigger_id = const_of_name(trigger_name) in 
     let buffer_fn_name = "add_"^rhs_map_name in
    .<
      .~push_map_trigger_name(map_id, bound, tuples, vid) {
          let stmts = get_completed_stmts(.~trigger_id, map_id, vid) in
          foreign(buffer_fn_name, tuples, vid);
          if stmts != [] then do {
            // Send to local do_complete
            iter(\stmt_addr -> send(stmt_addr, bound, vid), stmts)
          }
        }>.
  )
  empty_code
  (rhs_maps T_trigger)

(* get_completed_stmts
 * ----------------------------------------
 * Utility function that maps push functions (which are per-map) to 
 * do_complete_stmts (which are statement specific).
 * Uses the per statement counters
 *)
.<
// Decrement the counters and find completed statements
get_completed_stmts(trigger_id, map_id, vid) -> stmt_addr list
   // Decrement the counters 
   iter(\s -> 
        if stmt_counters[vid, trigger_id, s]? then do {
          stmt_counters[vid, trigger_id, s] <- stmt_counters[vid, s]-1;
        } else {
          // Initialize if the push arrives before the put.
          stmt_counters[vid, trigger_id, s] = -1;
        },
        map_possible_completions[map_id, trigger_id, _])   // slice

   // Find the statements that have a counter of 0.
   filtermap(\(stmt_id) -> stmt_counters[vid, trigger_id, stmt_id]=0,
   \(stmt_id) -> do_complete_stmts[trigger_id, stmt_id],
             map_possible_completions[map_id, trigger_id, _])
>.            

(* do_complete_trigger_stmt_id
 * --------------------------------------
 * Functions that perform the actual calculations of the statement
 * And are called mostly by pushes. 
 * We generate them per statement so they can handle the data types *)

(* Generate code to send data for all possible correctives *)
(* We send the new lhs_map results to any statement that is affected *)

let send_correctives map_id =
    List.fold_left 
        (fun acc_code trigger_id ->
            let stmts = stmts_of_trigger trigger_id in
            let corrective_addr = "On_corrective_"^trigger_id^map_id in
            List.fold_left 
                (fun acc_code stmt_id ->
                    (* Our lhs_map is now our rhs_map *)
                    if stmt_has_rhs_map stmt_id map_id then
                        let shuffle_func_msrc_mdst =
                          shuffle_for map_id (lhs_map stmt_id)
                        in
                        .< ~.acc_code 
                        if corrective_list[.~stmt_id]? then do {
                            iter(\(ip,delta_tuples) ->
                                send(promote_address(~.corrective_addr, ip), delta_tuples),
                                .~shuffle_func_msrc_mdst
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
in
(* do_complete and do_complete_corrective differ in the statement
   that they rewrite.
  In this case we rewrite the original M3/K3 statement (see original_code below)
*)
let delta_map_expr, update_expr = extract_delta_map original_code in
let delta_using_storage_expr =
  substitute_accesses delta_map_expr (rhs_maps trigger_name stmt_id)
in
let update_and_fire_correctives =
  .<Apply(\delta_tuples -> do {iter(.~update_expr, delta_tuples);
                              .~(send_correctives (lhs_map stmt_id)); },
         .~delta_using_storage_expr)>.
in
let completion_name = "do_complete_"^trigger_name^"_"^stmt_id in
.<
~.completion_name(vid, params) {
  ~.map_accesses
  ~.update_and_fire_correctives
}
>.


(* get_corrective_list: 
 * ---------------------------------------
 * Return list of corrective statements we need to execute by checking
 * which statements were performed after time x that used a particular map 
 * The only reason for this function is that we may have the same stmt
 * needing to execute with different vids
 *)
.<
get_corrective_list(map_id, vid) -> (int * int) list    // statement list
   filtermap( 
       \stmt -> stmt_has_rhs_map(stmt, map_id),         // filter out irrelevant triggers
       \stmt -> stmt,                                   // get the list of numbers of statements
       flatten(
           map(\trigger -> stmts_of_trigger(trigger),
               foreign(operations_read_geq(vid))             // produces triggers >= vid 
           )
       )
   )
>.

(* On_corrective:
 * ---------------------------------------
 * Function that gets called when a corrective message is received.
 * Receives bound variables and deltas for one map.
 *)

List.fold_left
  (fun acc_code trigger_id ->
    let stmts = stmts_of_trigger trigger_id in
    let corrective_trigger_name =
      "On_corrective_"^trigger_id^"_"^stmt_id^"_"^map_id
    in
    let do_corrective_addr =
      "do_corrective_complete_"^trigger_id^"_"^stmt_id^"_"^map_id
    in
    let add_delta_to_buffer_fn = "add_delta_to_buffer_"^map_id
    in
      List.fold_left (List.fold_left 
        (fun acc_code stmt_id ->
          (fun acc_code map_id ->
          .< .~acc_code
          
           .~corrective_trigger_name(delta_tuples, vid) {

             // accumulate delta for this vid and all following vids
             foreign(.~add_delta_to_buffer_fn(delta_tuples, vid))     

             let corrected_updates : vid list =
               filtermap(
                 \(vid2,tid2,stid2,cnt) -> vid <= vid2 && c == 0,
                 \(vid2,tid2,stid2,cnt) -> vid,
                 stmt_counters[_, .~trigger_id, .~stmt_id, _])
             in

             (* Send messages to do_complete_correctives: we might have several
              * vids to send to *)
             iter(\vid ->
                 send(.~do_corrective_addr, delta_tuples, vid),
               corrected_updates)
          }
          >.
        )
        acc_code rhs_maps stmt_id)
        acc_code stmts
  )
  empty_code
  triggers


(* do_complete_corrective_trigger_stmt_map
 * -------------------------------------------
 * Handles the delta calculation performed by the corrective
 * And sends out more corrective messages if needed
 *)

let send_correctives map_id =
    List.fold_left
      (fun acc_code trigger_id ->
        let stmts = stmts_of_trigger trigger_id in
        let corrective_addr = "On_corrective_"^trigger_id^map_id in
          List.fold_left
            (fun acc_code stmt_id ->
              (* Our lhs_map is now our rhs_map *)
              if stmt_has_rhs_map stmt_id map_id then
                  let shuffle_func_msrc_mdst =
                    shuffle_for map_id (lhs_map stmt_id)
                  in
                  .< ~.acc_code 
                  if corrective_list[.~stmt_id]? then do {
                      iter(\(ip,delta_tuples) ->
                          send(promote_address(~.corrective_addr, ip), delta_tuples),
                          .~shuffle_func_msrc_mdst
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
in
(* do_complete and do_complete_corrective differ in the statement
   that they rewrite.
  In this case we rewrite the delta M3/K3 statement (see delta_code below)
*)
let delta_map_expr, update_expr = extract_delta_map delta_code in
let delta_using_storage_expr =
  substitute_accesses delta_map_expr (rhs_maps trigger_name stmt_id)
in
let update_and_fire_correctives =
  .<Apply(\delta_tuples -> do {iter(.~update_expr, delta_tuples);
                              .~send_correctives; },
         .~delta_using_storage_expr)>.
in
let completion_name =
  "do_corrective_complete_"^trigger_name^"_"^stmt_id^"_"^map_id^"delta"
in
.<
~.completion_name(vid, params) {
  ~.map_accesses
  ~.update_and_fire_correctives
}
>.


