open Util
open ProgInfo

let translate_vars = List.map (fun (x, t) -> (x, M3ToK3.m3_type_to_k3_type t))

let translate_external (map_id:string -> map_id_t) (mapn, iv, ov, mt, _) =
  let i = ref (-1) in
    (map_id mapn, List.map (fun (x,_) -> i := !i + 1; (x, !i))
                  (iv @ ov @ ["value", M3Type.TInt]))

let rec get_externals expr =
  Calculus.CalcRing.fold (List.flatten) (List.flatten) (fun x->x) (function
    | Calculus.Value _
    | Calculus.Cmp _ -> []

    | Calculus.AggSum(_, subexp)
    | Calculus.Lift(_, subexp)
    | Calculus.Exists(subexp) -> get_externals subexp

    | Calculus.Rel(rn, rv) -> [rn, [], rv, M3Type.TInt, None]

    | Calculus.External(edata) -> [edata]
  ) expr

let prog_data_of_m3 (prog:M3.prog_t): prog_data_t =
  let map_data, map_mapping = snd @@
    List.fold_right (fun map (i, (map_data, map_mapping)) ->
      let map_nm, map_vars = match map with
        | M3.DSView({Plan.ds_name = name}) ->
          List.hd @@ Calculus.externals_of_expr name,
            Calculus.all_vars name@["value", Calculus.type_of_expr name]

        | M3.DSTable(name, vars, _) -> name, vars@["value", M3Type.TInt]
      in
        i - 1,
        ({map=i;
          map_nm;
          map_types=snd_many @@ translate_vars map_vars}::map_data,
            (map_nm, i)::map_mapping)
    ) !(prog.M3.maps) (List.length !(prog.M3.maps), ([], []))
  in

  let mk_bindings =
    translate_external @@ ListAsFunction.apply_strict map_mapping
  in

  let trig_data, stmt_data =
    List.fold_left (fun (trig_data, stmt_data)
                        ({M3.event = event; M3.statements = stmts}) ->
      if !stmts = [] then trig_data, stmt_data else
      let trig = List.length trig_data in
      let first_stmt_id = List.length stmt_data in
      let _, new_stmt_data, trig_stmts =
        List.fold_left (fun (stmt, new_stmt_data, trig_stmts)
                         ({Plan.target_map = tgt;
                           Plan.update_expr = expr;
                           Plan.update_type = update_type})->
          let is_update = update_type = Plan.UpdateStmt in
          let lmap, lmap_binds = mk_bindings @@ Plan.expand_ds_name tgt
          in
            stmt + 1
            ,
            {stmt; trig; lmap; lmap_binds; is_update;
              rmap_binds=List.map mk_bindings (get_externals expr);
            }::new_stmt_data
            ,
            stmt::trig_stmts)
          (first_stmt_id, [], []) !stmts
      in
      let new_stmt_data = List.rev new_stmt_data in
      let stmts = List.rev trig_stmts in
      { trig;
        trig_nm=Schema.name_of_event event;
        args=translate_vars (Schema.event_vars event);
        stmts;
      }::trig_data
      ,
      stmt_data @ new_stmt_data)
      ([], [])
      !(prog.M3.triggers)
  in
  List.rev trig_data, stmt_data, map_data
