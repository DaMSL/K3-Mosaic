open Lazy
open Util
open Printing
open Symbols
open Tree
open K3
open K3Util
open K3Typechecker

(* Two bools are declared and assigned respectively *)
type 'a reified_node_t = (id_t * type_t * bool * bool) * ('a texpr_t)
type 'a reified_expr_t = ('a reified_node_t) tree_t

(* Reified expression stringification *)
let print_reified_expr t =
  print_tree
    (fun lazy_ch t ->
      let (decl_id, decl_t, decl, assign), expr = node_data t in
      match decl, assign with
        | true, true
        | false, true ->
          begin
            ps_list Line force lazy_ch;
            ps ((if decl then "declare" else "assign")^
                " "^decl_id^" : "^(flat_string_of_type decl_t)^" = ");
            print_expr expr
          end
         
        | true, false ->
          begin
            ps ("declare "^decl_id^" : "^(flat_string_of_type decl_t)^"\n");
            ps_list Line force lazy_ch;
            fnl();
            ps ("reified "^" "^decl_id^" = ");
            print_expr expr
          end

        | false, false ->
          begin
            ps_list Line force lazy_ch;
            fnl();
            print_expr expr
          end)
    t

let string_of_reified_expr t = wrap_formatter (fun () -> print_reified_expr t)

(* Type helpers. Should be moved elsewhere *)
let is_unit t = match t with
  | TValue(TIsolated(TImmutable(TUnit))) -> true
  | _ -> false

let get_type e =
  try type_of_texpr e 
  with TypeError _ -> failwith ("type error: "^(string_of_expr e))

let mk_typed_var id t meta = mk_tree (((0, Var id), meta), []) 
  
(* Top-down folder for expression reification *)
let name_of_reification (reified_ancestors : (int * (id_t * type_t * bool * bool)) list)
                        (e : 'a texpr_t)
                        : (int * (id_t * type_t * bool * bool)) list =
  let unwrap opt = match opt with
    | Some x -> x | _ -> failwith "invalid option"
  in
  let gensym tag = gen_string_sym default_class tag in
  
  (* Reification helpers *)
  
  (* Generates a new reification symbol *)
  let new_name tag declare assign e =
    Some(gensym tag, (get_type e), declare, assign) in   
  
  (* Reifies the list of expressions to the given symbol *)
  let reify_list (id,t) e_l =
    List.map (fun e -> id_of_expr e, (id, t, false, true)) e_l
  in
  
  (* Generates a new reification for and expression, and uses it
   * for all expressions in the given list *)
  let declare_and_reify tag_str e e_l =
    let e_name_opt = new_name tag_str true false e
    in begin match e_name_opt with
      | Some((e_id, e_t, _, _) as e_name) ->
        [id_of_expr e, e_name]@(reify_list (e_id, e_t) e_l)        
      | _ -> failwith ("invalid reification symbol for "^tag_str)
    end
  in
  
  (* Handles reifications of expressions that are dependent on their parent *)
  (* TODO: handle more general subexpressions at reification points, specifically
   * lambdas returned from conditionals and blocks, and reification for 
   * global function invocations *)
  let reify_from_parent e =
    let e_name =
      if List.mem_assoc (id_of_expr e) reified_ancestors
      then Some(List.assoc (id_of_expr e) reified_ancestors) else None
    in
    match tag_of_expr e with
    | Apply ->
      let fn_e, arg_e = decompose_apply e in
      let reify_lambda =
        match is_unit (get_type e), (tag_of_expr fn_e), e_name with
        | false, Lambda _, None ->
            declare_and_reify "apply" e [decompose_lambda fn_e] 
        | false, Lambda _, Some (e_id, e_t, _, _) ->
            reify_list (e_id, e_t) [decompose_lambda fn_e]
        | false, _, None -> declare_and_reify "apply" e []
        | _, _, _ -> []
      in
      let reify_arg =
        let reify_expr e name = id_of_expr e, name in
        match arg_of_lambda fn_e with
          | Some(AVar (id,t)) -> [reify_expr arg_e (id, TValue(t), true, true)]   
          | Some(ATuple(vt_l)) -> 
            (* Directly reify to tuple bindings if the arg is a tuple value *)
            begin match K3Util.tag_of_expr arg_e with
              | Tuple ->
                List.map2 (fun (id,t) e ->
                  reify_expr e (id, TValue(t), true, true)) vt_l (sub_tree arg_e)
              | Var _ -> []
              | _ -> [reify_expr arg_e (unwrap (new_name "apply" true true arg_e))]
            end

          | _ ->
            (* TODO: retrieve global fn arg from declaration, and directly
             * reify to tuple bindings if the arg is a tuple value *)
            [reify_expr arg_e (unwrap (new_name "apply" true true arg_e))]
      in reify_lambda@reify_arg

    | Block ->
      let child_e =
        let c = sub_tree e in List.nth c (List.length c - 1)
      in
      if is_unit (get_type e) then []
      else begin match e_name with
           | None -> declare_and_reify "block" e [child_e]
           | Some (e_id, e_t, _, _) -> reify_list (e_id, e_t) [child_e]
           end
      
    | IfThenElse ->
      let _, then_e, else_e = decompose_ifthenelse e in
      if is_unit (get_type e) then []
      else begin match e_name with
           | None -> declare_and_reify "ifelse" e [then_e; else_e]
           | Some (e_id, e_t, _, _) -> reify_list (e_id, e_t) [then_e; else_e]
           end
      
    | Aggregate ->
      let fn_e, init_e, _ = decompose_aggregate e in
      begin
        let children_to_reify =
          let fn_reifications = match tag_of_expr fn_e with
            | Lambda _ -> [decompose_lambda fn_e] | _ -> []
          in fn_reifications@[init_e]
        in
        match e_name with
        | None -> declare_and_reify "agg" e children_to_reify
        | Some (e_id, e_t, _, _) -> reify_list (e_id, e_t) children_to_reify
      end
    
    | _ -> []
  in
  let reify_child reifications e =
    if List.mem_assoc (id_of_expr e) reifications then reifications
    else
    let child_name = match tag_of_expr e with
      | Empty     c_t -> new_name "empty" true false e
      | Singleton c_t -> new_name "singleton" true false e
      | Combine       -> new_name "combine" true false e

      (* TODO: combining ranges needs to explicitly materialize the range,
       * that is we need to pass down when we need copied collections *)
      | Range     c_t -> failwith "range reification not yet implemented"

      | Map                -> new_name "map" true false e 
      | FilterMap          -> new_name "filtermap" true false e
      | Flatten            -> new_name "flatten" true false e
      | Aggregate          -> new_name "agg" true false e
      | GroupByAggregate   -> new_name "gbagg" true false e
      | Sort               -> new_name "sort" true false e

      | Slice -> new_name "slice" true true e
      | Peek  -> new_name "peek" true true e

      | _ -> None
    in
    match child_name with
      | None -> reifications
      | Some(c_id, c_t, c_decl, c_assign) ->
        reifications@[id_of_expr e, (c_id, c_t, c_decl, c_assign)] 
  in
  let parent_reifications = reified_ancestors@(reify_from_parent e)
  in List.fold_left reify_child parent_reifications (sub_tree e)

(* Bottom-up folder for expression reification *)
let reify_node (reified_ancestors : (int * (id_t * type_t * bool * bool)) list)
               (reified_subtrees : ('a reified_expr_t * bool) list list)
               (e : 'a texpr_t) =
  let extract_expr_and_nodes (e_acc,n_acc) 
                             ((rs, used) : ('a reified_expr_t * bool)) 
                             : 'a texpr_t list * 'a reified_expr_t list =
    let rs_data, rs_ch = decompose_tree rs in
    let rs_expr, rs_nodes = 
      let (id, t, _, _), e = rs_data in
      if used then mk_typed_var id t (meta_of_expr e), [rs] else e, rs_ch
    in e_acc@[rs_expr], n_acc@rs_nodes
  in
  let c_exprs, c_nodes =
    List.fold_left extract_expr_and_nodes ([],[])
      (List.flatten reified_subtrees)
  in
  let e_id = id_of_expr e in
  let e_name, e_used =
    if not(List.mem_assoc e_id reified_ancestors)
    then ("dnr", get_type e, false, false), false
    else (List.assoc e_id reified_ancestors), true
  in 
     [mk_tree ((e_name, recompose_tree e c_exprs), c_nodes), e_used]  

let reify_expr e =
  let r = fold_tree name_of_reification reify_node [] [] e
  in match r with
    | [rt,_] -> rt 
    | _ -> failwith "invalid reified root"

let unreify_expr rt =
  let inline_reification _ sub_exprs rt =
    let ((id,t,_,_),e) = node_data rt in
    let e_of_rt = fst (substitute_expr (List.flatten sub_exprs) e)
    in [mk_typed_var id t (meta_of_expr e), e_of_rt] 
  in 
  let r = fold_tree (fun x _ -> x) inline_reification None [] rt
  in match r with
    | [x,y] -> y
    | _ -> failwith "invalid unreification root"
    
