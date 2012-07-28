open Util
open Symbols
open Tree
open K3
open K3Util
open K3Typechecker
open ReifiedK3
open Imperative

(* Helpers *)

let init l = List.rev (List.tl (List.rev l))

let back l = List.hd (List.rev l)

let split_last l = 
  let x = List.rev l in List.rev (List.tl x), List.hd x

(* Specialized unwrappers *)
let unwrap opt = match opt with | Some x -> x | _ -> failwith "invalid option"

let expr_of_decl_args da =
  match da with | Init e -> e | _ -> failwith "invalid expr"

let unwrap_pair o1_f o2_f both_f (o1, o2) = match o1,o2 with
    | Some x, Some y -> both_f x y
    | Some x, None -> o1_f x
    | None, Some x -> o2_f x
    | None, None -> failwith "invalid option pair"

let unwrap_expr_opt_and_cmds da_cmd_pair = unwrap_pair
    (fun da -> Some(expr_of_decl_args da), [])
    (fun cmds -> None, cmds)
    (fun da cmds -> Some(expr_of_decl_args da), cmds)
  da_cmd_pair

let unwrap_expr_and_cmds da_cmd_pair = unwrap_pair
    (fun da -> expr_of_decl_args da, [])
    (fun cmds -> failwith "invalid subexpression")
    (fun da cmds -> expr_of_decl_args da, cmds)
  da_cmd_pair

(* Symbol helpers *)
let expr_sym_class = "IEXPR"
let cmd_sym_class = "ICMD"
let gen_expr_sym () = gen_int_sym expr_sym_class
let gen_cmd_sym () = gen_int_sym cmd_sym_class   
let _ =
  register_symbol expr_sym_class "__";
  register_symbol cmd_sym_class "__";;

(* Symbol constructors *)
let mk_loop_sym () = gen_string_sym expr_sym_class "elem"
let mk_gb_key_sym () = gen_string_sym expr_sym_class "gbkey"
let mk_gb_val_sym () = gen_string_sym expr_sym_class "gbagg"

(* Declaration constructors *)
let mk_var_decl id t e_opt = DVar(id, t, e_opt)

(* Expression constructors *)
(* TODO: this should validate expected #children against tag *)
let mk_iexpr e_tag e_meta children =
  mk_tree (((gen_expr_sym (), e_tag), e_meta), children)

let mk_const meta const = mk_iexpr (Const const) meta []

let mk_var meta id = mk_iexpr (Var id) meta []
 
let mk_tuple meta fields = mk_iexpr Tuple meta fields

let mk_fn meta fn_tag args = mk_iexpr (Fn fn_tag) meta args
 
(* Command constructors *)
(* TODO: this should validate expected #children against tag *)
let mk_cmd c_tag c_meta children =
  mk_tree (((gen_cmd_sym (), c_tag), c_meta), children)

let mk_assign meta id e = mk_cmd (Assign (id, e)) meta []

let mk_decl meta decl = mk_cmd (Decl decl) meta []

let mk_expr meta e = mk_cmd (Expr e) meta []

let mk_block meta sub = match sub with
  | [] -> failwith "invalid block body"
  | [x] -> x
  | _ -> mk_cmd Block meta sub
 
let mk_for meta id t e body = mk_cmd (For (id,t,e)) meta body

let mk_ifelse meta pred branches = mk_cmd (IfThenElse pred) meta branches



(* Conversion methods *)

let imperative_of_expr_node mk_meta (id,t,_,_) children e =
  (*
  let e_type = type_of_texpr e in 
  *)
  let e_meta = meta_of_texpr e in
  
  (* TODO: unify with typechecker errors *)
  let type_failure () = failwith "invalid type" in
  
  (* Return helpers *)
  let ret_common ?(sub=children) expr_and_cmd_f =
    let sub_da, sub_cmd = List.split children in
    let pre_cmds = List.flatten (List.map (function None -> [] | Some(c) -> c) sub_cmd) in
    if List.mem None sub_da then failwith "invalid pure expression reification"
    else
    let sub_expr = List.map expr_of_decl_args (List.map unwrap sub_da)
    in expr_and_cmd_f sub_expr pre_cmds
  in
  let ret_expr ?(sub=children) e tag =
    ret_common ~sub:sub (fun exprs pre_cmds ->
      let e_meta = meta_of_texpr e 
      in Some(Init(mk_iexpr tag e_meta exprs)), Some(pre_cmds))
  in
  let ret_cmd_expr expr_f = ret_common (fun exprs pre_cmds ->
    None, Some(pre_cmds@[mk_expr (mk_meta()) (expr_f exprs)]))
  in 
  let ret_cstr () =
    ret_common (fun exprs pre_cmds -> Some(Constructor(exprs)), Some(pre_cmds))
  in
  let ret_term e tag = ret_expr e tag ~sub:[] in
  let ret_binop e op_tag = ret_expr e (Op op_tag) in
  let ret_cmds cmds = None, Some(cmds) in
  
  (* Lambda binding declarations *)
  let mk_position_decl id t pos src_v =
    let init = Init (mk_fn (mk_meta()) (Composite(Position pos)) [src_v])
    in mk_var_decl id (TInternal(TValue t)) (Some init)
  in
  let mk_elem_bindings fn_e elem_id elem_t =
    let tvars = typed_vars_of_lambda fn_e in
    let elem_v = mk_var (mk_meta()) elem_id in
    snd (List.fold_left (fun (i,decl_acc) (v,t) ->
      let decl = mk_position_decl v t i elem_v
      in i+1, (decl_acc@[mk_decl (mk_meta()) decl])) (0,[]) tvars)
  in
  let mk_agg_elem_bindings fn_e agg_id elem_id elem_t = 
    let tvars = typed_vars_of_lambda fn_e in
    let agg_v = Init(mk_var (mk_meta()) agg_id) in
    let elem_v = Init(mk_var (mk_meta()) elem_id) in
    match tvars with
      | [(aid,at); (eid,et)] ->
        let agg_decl = mk_var_decl aid (TInternal(TValue at)) (Some agg_v) in
        let elem_decl = mk_var_decl eid (TInternal(TValue et)) (Some elem_v)
        in [mk_decl (mk_meta()) agg_decl; mk_decl (mk_meta()) elem_decl]
      | _ -> failwith "invalid aggregate bindings"
  in

  (* Expression and command construction *)
  let assign_da id da = mk_assign (mk_meta()) id (expr_of_decl_args da) in
  let assign_expr id e = mk_assign (mk_meta()) id e in 
  let append_expr id da =
    (* TODO: is this really append?
     * Shouldn't this depend on the return type of the collection? *)
    let ae = mk_fn (mk_meta()) (Named "append") [mk_var (mk_meta()) id; expr_of_decl_args da]
    in mk_expr (mk_meta()) ae
  in
  let cond_expr pred branches = mk_ifelse (mk_meta()) pred branches in
  let coll_expr coll_fn_tag children = mk_fn (mk_meta()) (Collection coll_fn_tag) children in
  let expr_cmd e = mk_expr (mk_meta()) e in

  (* Loop body construction *)
  (* TODO: handle pre_cmds that may come from the body_cmd_fn, for example
   * side-effects that arise when constructing lambdas for collection transformers *)
  let collection_loop c_type c_pair body_cmd_fn =
    let _, elem_t = collection_of (base_of (value_of c_type type_failure)) type_failure in
    let loop_id, loop_t = mk_loop_sym(), TInternal(TValue elem_t) in
    let loop_body = body_cmd_fn loop_id loop_t in
    let loop_target, pre_cmds = unwrap_pair
      (fun da -> (expr_of_decl_args da), [])
      (fun cmds -> failwith "invalid reified map collection")
      (fun da cmds -> (expr_of_decl_args da), cmds)
      c_pair
    in pre_cmds@[mk_for e_meta loop_id loop_t loop_target loop_body]
  in

  let mk_appfn_loop_body_cmds is_map result_id fn_e fn_pair loop_id loop_t =
    let bindings = mk_elem_bindings fn_e loop_id loop_t in
    bindings@(unwrap_pair
      (fun da -> if is_map then [append_expr result_id da] 
                 else failwith "invalid reified iterate function result")
      (fun cmds -> if is_map then failwith "invalid reified map function result"
                   else cmds)
      (fun da cmds -> if is_map then cmds@[append_expr result_id da]
                      else failwith "invalid reified iterate function result")
      fn_pair)
  in 

  let mk_agg_loop_body_cmds result_id agg_fn_e agg_fn_pair loop_id loop_t =
    let bindings = mk_agg_elem_bindings agg_fn_e result_id loop_id loop_t in
    let acc_cmds = unwrap_pair 
      (fun da -> [assign_da result_id da])
      (fun cmds -> cmds)
      (fun da cmds -> cmds@[assign_da result_id da])
      agg_fn_pair
    in bindings@acc_cmds
  in

  (* Start of conversion method *)
  match K3Util.tag_of_expr e with
  | K3.Const c -> ret_term e (Const c)
  | K3.Var id  -> ret_term e (Var id)
  | K3.Tuple   -> ret_expr e Tuple
  | K3.Just    -> ret_expr e Just

  | K3.Empty v_t     -> ret_cmds []
  | K3.Singleton v_t -> ret_cstr ()
  | K3.Combine       ->
      ret_common (fun exprs pre_cmds ->
        let cmds = pre_cmds@(List.map (fun e ->
            expr_cmd (coll_expr Combine [mk_var (mk_meta()) id; e])
          ) exprs)
        in None, Some(cmds))

  | K3.Range c_t -> 
    (* TODO pipelined for loop, unless this range is marked to be materialized.
     *
     * When we generate a loop, we must indicate the reified symbol should be
     * lifted outside the loop scope to ensure its liveness afterwards.
     *
     * Also, the functional expression containing the loop must be evaluated
     * inside the loop's body, that is we must invert control flow when
     * generating the outer expression.
     *)
    failwith "imperative range not yet implemented"

  | K3.Add  -> ret_binop e Add
  | K3.Mult -> ret_binop e Mult
  | K3.Neg  -> ret_binop e Neg

  | K3.Eq  -> ret_binop e Eq
  | K3.Neq -> ret_binop e Neq
  | K3.Lt  -> ret_binop e Lt
  | K3.Leq -> ret_binop e Leq

  | Lambda arg -> List.hd children
  | Apply ->
    begin match children with
      | [(fn_da_opt, None); (None, Some arg_cmds)] ->
          (fn_da_opt, Some arg_cmds)
      
      | [(fn_da_opt, Some fn_cmds); (None, Some arg_cmds)] ->
          (fn_da_opt, Some (arg_cmds@fn_cmds))

      | _ -> failwith "invalid apply reification"
    end

  | K3.Block ->
    let init, back = split_last children in
    let block_init_cmds =
      List.fold_left
        (fun cmd_acc dac_pair ->
          unwrap_pair
            (fun da -> failwith "invalid block expression element")
            (fun cmds -> cmd_acc@cmds)
            (fun da cmds -> failwith "invalid block expression element")
            dac_pair)
        [] init
    in
      unwrap_pair
        (fun da -> (Some da, Some block_init_cmds)) 
        (fun cmds -> (None, Some (block_init_cmds@cmds)))
        (fun da cmds -> (Some da, Some(block_init_cmds@cmds)))
        back

  | K3.IfThenElse ->
    (* Note this blindly assumes every ifelse is materialized, and thus the
     * reified sym is always associated with the ifelse expression.
     *
     * We could fix this by also passing down the expression that is intended
     * to be the source for the reified sym, and explicitly using an equality
     * test prior to code generating for an expression node. *)

    begin match children with
      | [(x, None); (y, None); (z, None)] -> ret_expr e (Op Ternary)
      | [pred_pair; then_pair; else_pair] -> 
        let pred_expr, pre_cmds = unwrap_expr_and_cmds pred_pair in
        let branches = List.flatten (List.map (unwrap_pair
              (fun da -> [assign_da id da])
              (fun cmds -> cmds)
              (fun da cmds -> cmds@[assign_da id da]))
          [then_pair; else_pair])
        in
        let ret_cmds = [cond_expr pred_expr branches]
        in None, Some(ret_cmds)

      | _ -> failwith "invalid reified ifelse children"
    end

  (* TODO: pre commands from iterate function *)
  | Iterate ->
    begin match children with
      | [fn_pair; c_pair] -> 
        let fn_e, c_e = decompose_iterate e in
        let iter_body_fn = mk_appfn_loop_body_cmds false id fn_e fn_pair in
        let iter_body = collection_loop (type_of_texpr c_e) c_pair iter_body_fn
        in None, Some(iter_body)
      | _ -> failwith "invalid reified iterate children"
    end

  (* TODO: pre commands from map function *)
  | Map ->
    begin match children with
      | [fn_pair; c_pair] ->
        let fn_e, c_e = decompose_map e in
        let map_body_fn = mk_appfn_loop_body_cmds true id fn_e fn_pair in
        let map_body = collection_loop (type_of_texpr c_e) c_pair map_body_fn
        in None, Some(map_body)
      | _ -> failwith "invalid reified map children"
    end

  (* TODO: pre commands from filter and map function *)
  | FilterMap ->
    begin match children with
      | [filter_fn_pair; map_fn_pair; c_pair] ->
        let f_fn_e, m_fn_e, c_e = decompose_filter_map e in
        let filter_body_cmds loop_id loop_t =
          let filter_bindings = mk_elem_bindings f_fn_e loop_id loop_t in
          let map_body = mk_appfn_loop_body_cmds true id m_fn_e map_fn_pair loop_id loop_t in
          let map_branch = [mk_block (mk_meta()) map_body; expr_cmd (mk_const (mk_meta()) CUnit)] in
          let filter_cmds =
            unwrap_pair
              (fun da -> [cond_expr (expr_of_decl_args da) map_branch])
              (fun cmds -> failwith "invalid filtermap predicate expression")
              (fun da cmds -> cmds@[cond_expr (expr_of_decl_args da) map_branch])
              filter_fn_pair
          in filter_bindings@filter_cmds
        in
        let filter_body = collection_loop (type_of_texpr c_e) c_pair filter_body_cmds
        in None, Some(filter_body) 
      | _ -> failwith "invalid reified filter-map children"
    end

  | Flatten ->
    (* Loop over nested collection and concat, essentially ext(id)/flatmap *)
    let c_e = List.hd (sub_tree e) in
    let c_pair = List.hd children in
    let flatten_body_cmds loop_id loop_t =
      [expr_cmd (coll_expr Combine [mk_var (mk_meta()) id; mk_var (mk_meta()) loop_id])]
    in
    let flatten_body = collection_loop (type_of_texpr c_e) c_pair flatten_body_cmds
    in None, Some(flatten_body)
  
  (* TODO: pre commands from agg function *)
  | Aggregate ->
    begin match children with
      | [fn_pair; init_pair; c_pair] ->
        let agg_fn_e, init_e, c_e = decompose_aggregate e in
        (* TODO: can init_expr ever be a constructor rather than an expr? *)
        let init_expr, init_cmds = unwrap_expr_and_cmds init_pair in
        let mk_agg_body_fn = mk_agg_loop_body_cmds id agg_fn_e fn_pair in
        let cmds = init_cmds@(
          collection_loop (type_of_texpr c_e) c_pair mk_agg_body_fn)
        in Some(Init(init_expr)), Some(cmds)
      | _ -> failwith "invalid reified aggregate children"
    end

  (* TODO: pre commands from gb and agg function *)  
  | GroupByAggregate ->
    begin match children with
      | [gb_fn_pair; agg_fn_pair; init_pair; c_pair] ->
        let gb_fn_e, agg_fn_e, init_e, c_e = decompose_gbagg e in
        let init_expr, init_cmds = unwrap_expr_and_cmds init_pair in
        
        let gbagg_body_cmd_fn loop_id loop_t =

          let gb_key_id, gb_val_id = mk_gb_key_sym (), mk_gb_val_sym () in
          let gb_key_t, gb_val_t =
            let result_type = base_of (value_of (type_of_texpr e) type_failure) in
            match base_of (snd (collection_of result_type type_failure)) with
              | TTuple([gb_t; a_t]) -> (TInternal (TValue gb_t)), (TInternal (TValue a_t)) 
              | _ -> failwith "invalid groupby collection element type"
          in

          let old_val = mk_tuple (mk_meta()) [mk_var (mk_meta()) gb_key_id; mk_var (mk_meta()) gb_val_id] in
          let new_val e = mk_tuple (mk_meta()) [mk_var (mk_meta()) gb_key_id; e] in
          
          let find_expr = coll_expr Find [mk_var (mk_meta()) id; mk_var (mk_meta()) gb_key_id] in
          let contains_expr = coll_expr Contains [mk_var (mk_meta()) id; mk_var (mk_meta()) gb_key_id] in
          
          let assign_gbval_cmd =
            let branches = [assign_expr gb_val_id find_expr;
                            assign_expr gb_val_id init_expr]
            in cond_expr contains_expr branches 
          in
          
          let update_cmd e = expr_cmd (coll_expr Update [mk_var (mk_meta()) id; old_val; new_val e]) in
          let new_cmd e = expr_cmd (coll_expr Insert [mk_var (mk_meta()) id; new_val e]) in

          (* Declarations *)
          let decls = List.map (fun (id,t) -> 
              mk_decl (mk_meta()) (mk_var_decl id t None)
            ) [gb_key_id, gb_key_t; gb_val_id, gb_val_t]
          in

          (* Bindings *)
          let gb_bindings = mk_elem_bindings gb_fn_e loop_id loop_t in
          let bindings = mk_agg_elem_bindings agg_fn_e gb_val_id loop_id loop_t in
          
          (* Grouping function evaluation *)
          let mk_assign_gb_key_val da = [assign_da gb_key_id da; assign_gbval_cmd] in
          let gb_cmds = unwrap_pair
            mk_assign_gb_key_val
            (fun cmds -> failwith "invalid reified group function result")
            (fun da cmds -> cmds@(mk_assign_gb_key_val da))
            gb_fn_pair
          in

          (* Aggregate function evaluation *)
          let agg_cmds = 
            let replace_group_cmd da =
              let branches =
                let e = expr_of_decl_args da in [update_cmd e; new_cmd e]
              in cond_expr contains_expr branches
            in
				    let acc_cmds = unwrap_pair 
				      (fun da -> [replace_group_cmd da])
				      (fun cmds -> failwith "invalid reified groupby aggregation function result")
				      (fun da cmds -> cmds@[replace_group_cmd da])
				      agg_fn_pair
				    in bindings@acc_cmds
          in
          
          (* Loop body *)
          decls@gb_bindings@gb_cmds@agg_cmds
            
        in
        (* TODO: handle pre cmds for gb and agg fn *)
        let cmds = init_cmds@(collection_loop (type_of_texpr c_e) c_pair gbagg_body_cmd_fn)
        in None, Some(cmds)

      | _ -> failwith "invalid reified group-by children"
    end

  | K3.Sort -> ret_cmd_expr (coll_expr Sort)

  | K3.Peek -> ret_expr e (Fn (Collection Peek))
  | K3.Slice -> ret_expr e (Fn (Collection Slice))
  | K3.Insert -> ret_cmd_expr (coll_expr Insert)
  | K3.Delete -> ret_cmd_expr (coll_expr Delete)
  | K3.Update -> ret_cmd_expr (coll_expr Update)

  (* TODO: serialize and use side-effecting send primitive *)
  | K3.Send -> failwith "imperative send not yet implemented"

  (* TODO: generates a side-effecting assignment command *) 
  | K3.Assign -> failwith "imperative assign not yet implemented"

  (* TODO: primitive target language operation *)
  | Deref -> failwith "imperative deref not yet implemented"
  

let imperative_of_expr mk_meta ((id,t,decl,assign) as reified_sym) e =
  let declare assign id t node_pair =
    let d_f da_opt = [mk_decl (mk_meta()) (mk_var_decl id (TInternal t) da_opt)] in
    unwrap_pair
      (fun da -> d_f (Some da))
      (fun cmds -> (d_f None)@cmds)
      (fun da cmds ->
        if not assign then (d_f (Some da))@cmds
        else
          let e = expr_of_decl_args da
          in (d_f None)@cmds@[mk_assign (ImperativeUtil.meta_of_expr e) id e])
      node_pair
  in
  let assign_if_expr id t node_pair =
    unwrap_pair
      (fun da ->
        let e = expr_of_decl_args da
        in [mk_assign (ImperativeUtil.meta_of_expr e) id e])
      (fun cmds -> cmds)
      (fun da cmds ->
        let e = expr_of_decl_args da
        in cmds@[mk_assign (ImperativeUtil.meta_of_expr e) id e])
      node_pair
  in
  let ensure_cmd node_pair = match node_pair with
      | None, Some(x) -> x
      | _,_ -> failwith "invalid cmd"
  in
  let (node_pair : ('a decl_args_t option) * ('a cmd_t list option))  =
    fold_tree (fun _ _ -> reified_sym) (imperative_of_expr_node mk_meta)
      reified_sym (None,None) e
  in
  match decl, assign with
  | true, _ -> declare assign id t node_pair
  | false, true -> assign_if_expr id t node_pair
  | false, false -> ensure_cmd node_pair


let imperative_of_reified_node mk_meta _ sub_cmds rt =
  let reified_sym, e = node_data rt
  in (List.flatten sub_cmds)@(imperative_of_expr mk_meta reified_sym e)


let imperative_of_reified_expr mk_meta rt =
  fold_tree (fun _ _ -> None) (imperative_of_reified_node mk_meta) None [] rt


let imperative_of_declaration mk_meta d =
  let is_function t = match t with | TFunction _ -> true | _ -> false in
  let decompose_function_type t = match t with 
    | TFunction (arg,ret) -> arg, ret
    | _ -> failwith "invalid function type" 
  in
  match d with
  | Global  (id, t, init) ->
      if is_function t then
        let arg_t, ret_t = decompose_function_type t in
        (* TODO: more general function handling *)
        begin match init with
          | Some(e) ->
            let arg = match arg_of_lambda e with
              | Some(a) -> a 
              | None -> failwith "invalid global function declaration" 
            in
            let cmds = [mk_block (mk_meta()) (imperative_of_reified_expr mk_meta (reify_expr e))]
            in Some(DFn(id, arg, TInternal(TValue ret_t), cmds)), []
          | _ -> failwith "invalid function body"
        end
      else
        let cmds = match init with
          | Some(e) -> imperative_of_reified_expr mk_meta (reify_expr e)
          | _ -> [] 
        in Some(DVar(id, TInternal(t), None)), cmds
        
  | Foreign (id,t) ->
    (* TODO: special handling for fixed foreign functions *) 
    begin match id with
      | "sin" -> None, []
      | _ -> failwith ("unsupported foreign function: "^id)
    end

  | Trigger (id,a,decls,body) ->
    let cmds = [mk_block (mk_meta()) (imperative_of_reified_expr mk_meta (reify_expr body))]
    in Some(DFn(id, a, TInternal(TValue(canonical TUnit)), cmds)), []
  
  | Bind    (src,dest) -> failwith "bind declarations not implemented"
  | Consumable c -> failwith "consumable declaration not implemented"

let imperative_of_instruction mk_meta i = match i with
  | Consume id ->
      (* TODO: named function *)
      mk_expr (mk_meta()) (mk_fn (mk_meta()) (Named ("consume_"^id))[])

let imperative_of_program mk_meta p =
  let main_body, decls =
    List.fold_left (fun (main_cmd_acc, decl_acc) stmt ->
      match stmt with
      | Declaration d -> 
        let d_opt, init_cmds = imperative_of_declaration mk_meta d in
        (main_cmd_acc@init_cmds),
        (match d_opt with | Some c -> decl_acc@[c] | None -> decl_acc)

      | Instruction i -> 
          (main_cmd_acc@[imperative_of_instruction mk_meta i]), decl_acc)
      ([],[]) p
  in
  let int_t = TInternal (TValue (canonical TInt)) in
  let main_fn = DFn("main", AVar("argc", (canonical TInt)), int_t, main_body) 
  in decls@[main_fn]
