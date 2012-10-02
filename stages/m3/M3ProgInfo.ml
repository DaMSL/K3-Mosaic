open ProgInfo

let translate_vars = List.map (fun (x, t) -> (x, M3ToK3.m3_type_to_k3_type t))

let translate_external (map_id:string -> map_id_t) (mapn, iv, ov, _, _) = 
  let i = ref (-1) in
    (map_id mapn, List.map (fun (x,_) -> i := !i + 1; (x, !i)) (iv @ ov))

let rec get_externals expr = 
  Calculus.CalcRing.fold (List.flatten) (List.flatten) (fun x->x) (function 
    | Calculus.Value _
    | Calculus.Rel _
    | Calculus.Cmp _ -> []
       
    | Calculus.AggSum(_, subexp)
    | Calculus.Lift(_, subexp)
    | Calculus.Exists(subexp) -> get_externals subexp
    
    | Calculus.External(edata) -> [edata]
  ) expr

let prog_data_of_m3 (prog:M3.prog_t): prog_data_t =
  let (map_data, map_mapping) = snd (
    List.fold_right (fun map (i, (map_data, map_mapping)) ->
      let (map_name, map_vars) = begin match map with 
        | M3.DSView({Plan.ds_name = name}) ->
          ( List.hd (Calculus.externals_of_expr name), 
            Calculus.all_vars name )
        
        | M3.DSTable(name, vars, _) ->
          ( name, vars )
      end in
        ( i - 1, 
          ( (i, map_name, List.map snd (translate_vars map_vars))::map_data,
            (map_name, i)::map_mapping 
        ))
      
    ) !(prog.M3.maps) ((List.length !(prog.M3.maps)), ([], [])))
  in
  
  let mk_bindings = 
    translate_external (ListAsFunction.apply_strict map_mapping) 
  in
  
  let (trig_data, stmt_data) = 
    List.fold_left (fun (trig_data, stmt_data) 
                        ({M3.event = event; M3.statements = stmts}) ->
      let trig_id = List.length trig_data in
      let first_stmt_id = List.length stmt_data in
      let (_, new_stmt_data, trig_stmts) = 
        List.fold_left (fun (stmt_id, new_stmt_data, trig_stmts) 
                            ({Plan.target_map = tgt; Plan.update_expr = expr})->
          let (lhs_name, lhs_bindings) = 
            mk_bindings (Plan.expand_ds_name tgt)
          in
            ( stmt_id + 1,
              new_stmt_data @ [
                stmt_id, trig_id, lhs_name, lhs_bindings,
                List.map mk_bindings (get_externals expr)
              ],
              trig_stmts @ [stmt_id]
            )
        ) (first_stmt_id, [], []) !stmts
      in
        ( trig_data @ [
            trig_id, 
            Schema.name_of_event event, 
            translate_vars (Schema.event_vars event), 
            trig_stmts
          ],
          stmt_data @ new_stmt_data
        )
    ) ([], []) !(prog.M3.triggers)
  in
    (trig_data, stmt_data, map_data)