open Util
open Tree
open K3.AST
open K3.Annotation
open K3Util
open K3Printing
open K3Typechecker
open K3Streams
open K3Helpers
open ReifiedK3
open Imperative

module Make = functor (Lang : TargetLanguage) ->
struct

(* Import AST and utilities *)
module ASTImport = struct module AST = Imperative.AST(Lang) end
open ASTImport.AST

module U = ImperativeUtil.Util(Lang)

(* Helpers *)

let init l = List.rev (List.tl (List.rev l))

let back l = List.hd (List.rev l)

let split_last l = 
  let x = List.rev l in List.rev (List.tl x), List.hd x

(* Specialized unwrappers *)
let unwrap opt = match opt with | Some x -> x | _ -> failwith "invalid option"

let expr_of_decl_args da =
  match da with | Init e -> e | _ -> failwith "invalid expression, found constructor"

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


(* Symbol constructors *)
let mk_bind_sym () = U.gen_expr_name "bind"
let mk_loop_sym () = U.gen_expr_name "elem"
let mk_gb_key_sym () = U.gen_expr_name "gbkey"
let mk_gb_val_sym () = U.gen_expr_name "gbagg"

(* Basic types *)
let unit_t = TInternal(TValue(canonical TUnit))
let int_t  = TInternal (TValue (canonical TInt))
let bool_t = TInternal (TValue (canonical TBool))


(* Conversion methods *)

(* TODO: this relies on range and lambda normalization. *)
let imperative_of_expr_node mk_meta fn_arg_env 
                            (reify_cmds_and_ids, (id,t,_,_)) children e
=
  let error s =
    print_endline ("Error building imperative AST: "^s);
    print_endline (string_of_expr e);
    failwith ("Error building imperative AST: "^s)
  in
  
  (* TODO: unify with typechecker errors *)
  let type_failure () = failwith "invalid type" in
  
  let iv_type vt = TInternal(TValue vt) in
  let i_type t = TInternal t in
  let i_meta e = (i_type (type_of_expr e)), meta_of_expr e in
  
  let result_var = U.mk_var ((i_type t), mk_meta()) id in

  let e_meta = i_meta e in

  let vars_of_pair ec_pair =
    let vars_of_da da = match da with
      | Init(e) -> U.var_ids_of_expr e
      | Constructor exprs -> List.flatten (List.map U.var_ids_of_expr exprs)
    in
    let vars_of_cmds cmds = List.flatten (List.map U.var_ids_of_cmd cmds)
    in unwrap_pair
         (fun da -> vars_of_da da)
         (fun cmds -> vars_of_cmds cmds)
         (fun da cmds -> (vars_of_cmds cmds)@(vars_of_da da))
         ec_pair
  in
  let reify_cmds = List.flatten (fst (List.split reify_cmds_and_ids)) in
  let reify_cmds_by_children =
    List.map (fun ec_pair ->
      let ec_ids = match ec_pair with | None, None -> [] | _ -> vars_of_pair ec_pair in
      let ec_cmds = List.map (fun (cmds,id) ->
          if List.mem id ec_ids then cmds else []
        ) reify_cmds_and_ids
      in ec_pair, ec_cmds 
    ) children
  in

  (* Return helpers *)
  let ret_common ?(sub=children) expr_and_cmd_f =
    let sub_da, sub_cmds = 
      let x, y = List.split children in
        (List.flatten (List.map (function None -> [] | Some(c) -> [c]) x)),
        (List.flatten (List.map (function None -> [] | Some(c) -> c) y))
    in
    (* We expect subexpressions here, so any constructors returned will fail. *)
    let sub_expr = List.map expr_of_decl_args sub_da
    in expr_and_cmd_f sub_expr sub_cmds
  in
  let ret_expr ?(sub=children) e tag =
    ret_common ~sub:sub (fun exprs pre_cmds ->
      if not (check_tag_arity (tag_of_expr e) exprs) then
        error "invalid pure expression reification"
      else
      let cmd_opt = match reify_cmds@pre_cmds with [] -> None | x -> Some(x) in
      Some(Init(U.mk_iexpr tag (i_meta e) exprs)), cmd_opt)
  in
  let ret_cmd_expr expr_f = ret_common (fun exprs pre_cmds ->
    None, Some(reify_cmds@pre_cmds@[U.mk_expr (unit_t, mk_meta()) (expr_f exprs)]))
  in 
  let ret_cstr () =
    ret_common (fun exprs pre_cmds -> 
      Some(Constructor(exprs)), (match pre_cmds with [] -> None | x -> Some(x)))
  in
  let ret_term e tag = ret_expr e tag ~sub:[] in
  let ret_binop e op_tag = ret_expr e (Op op_tag) in
  let ret_cmds cmds = None, Some(cmds) in
  
  (* Lambda binding declarations *)
  let mk_bind_decl id t var =
    let dmeta = unit_t, mk_meta() in
    U.mk_decl dmeta (U.mk_var_decl id t (Some (Init var)))
  in
  let mk_position_decl id t pos src_v =
    let meta = t, mk_meta() in
    let init = Init (U.mk_fn meta (Member (Position pos)) [src_v])
    in U.mk_var_decl id t (Some init)
  in
  
  let get_fn_app_typed_vars fn_e = match K3Util.tag_of_expr fn_e with
    | Lambda _ -> typed_vars_of_lambda fn_e
    | K3.AST.Var fn_id -> 
      (* Replace global declaration args with new symbols before function call *)
      (try List.map (fun (id,t) -> mk_bind_sym(),t) 
             (typed_vars_of_arg (List.assoc fn_id fn_arg_env))
       with Not_found -> failwith ("no declaration found for function "^fn_id))
    | _ -> failwith "invalid function for bindings"
  in 

  (* Note this will re-evaluate the given expression for each positional
   * binding, so the expression should be reified *)
  let mk_positional_bindings tvars e =
    snd (List.fold_left (fun (i,(decl_acc, var_acc)) (v,vt) ->
        let t = iv_type vt in 
        let decl = mk_position_decl v t i e
        in i+1, ((decl_acc@[U.mk_decl (unit_t, mk_meta()) decl]),
                 (var_acc@[U.mk_var (t, mk_meta()) v]))
      ) (0,([],[])) tvars)
  in

  let mk_app_fn_bindings tvars exprs =
    match tvars with
      | [] -> 
        (* TODO: reconcile with unit function application *)
        failwith "invalid function args"

      | [v,vt] ->
        let t = iv_type vt in 
        let bound = [U.mk_var (t, mk_meta()) v] in
        begin match exprs with
          | [] -> failwith "invalid function args"
          | [e] -> [mk_bind_decl v t e], bound
          | _ -> [mk_bind_decl v t (U.mk_tuple (t, mk_meta()) exprs)], bound
        end

      | _ ->
        begin match exprs with
          | [] -> failwith "insufficient function args"
          | [e] -> mk_positional_bindings tvars e
          | args when List.length exprs = List.length tvars ->
            List.split (List.map2 (fun (id,t) e -> 
              let t = iv_type t in 
              (mk_bind_decl id t e), (U.mk_var (t, mk_meta()) id)) tvars exprs)

          | _ -> failwith "mismatched function args" 
        end
  in

  let mk_elem_bindings fn_e elem_id elem_t =
    let tvars = get_fn_app_typed_vars fn_e in
    let elem_meta = (iv_type elem_t), mk_meta() in
    let elem_v = U.mk_var elem_meta elem_id
    in mk_app_fn_bindings tvars [elem_v]
  in

  let mk_agg_elem_bindings fn_e agg_id agg_t elem_id elem_t = 
    let tvars = get_fn_app_typed_vars fn_e in
    let agg_meta = agg_t, mk_meta() in
    let elem_meta = elem_t, mk_meta() in
    let agg_v = Init(U.mk_var agg_meta agg_id) in
    let elem_v = Init(U.mk_var elem_meta elem_id) in
    match tvars with
      | [(aid,at); (eid,et)] ->
        let dmeta = unit_t, mk_meta() in
        let agg_decl = U.mk_var_decl aid (iv_type at) (Some agg_v) in
        let elem_decl = U.mk_var_decl eid (iv_type et) (Some elem_v)
        in [U.mk_decl dmeta agg_decl; U.mk_decl dmeta elem_decl],
           [U.mk_var agg_meta aid; U.mk_var elem_meta eid]
      | _ -> failwith "invalid aggregate bindings"
  in

  (* Expression and command construction *)
  let assign_da id da = U.mk_assign (unit_t, mk_meta()) id (expr_of_decl_args da) in
  let assign_expr id e = U.mk_assign (unit_t, mk_meta()) id e in 
  let cond_expr pred branches = U.mk_ifelse (unit_t, mk_meta()) pred branches in
  let expr_cmd e = U.mk_expr (unit_t, mk_meta()) e in
  let coll_expr t coll_fn_tag children = U.mk_fn (t, mk_meta()) (Collection coll_fn_tag) children in
  let insert_expr id t e = expr_cmd (coll_expr unit_t Insert [U.mk_var ((i_type t), mk_meta()) id; e]) in

  (* Loop body construction *)
  let collection_loop c_type c_pair c_reify_cmds body_cmd_fn =
    let _, elem_t = collection_of (base_of (value_of c_type type_failure)) type_failure in
    let loop_id, loop_t = mk_loop_sym(), elem_t in
    let loop_body = body_cmd_fn loop_id loop_t in
    let loop_target, pre_cmds = unwrap_pair
      (fun da -> (expr_of_decl_args da), [])
      (fun cmds -> failwith "invalid reified map collection")
      (fun da cmds -> (expr_of_decl_args da), cmds)
      c_pair
    in c_reify_cmds@pre_cmds@[U.mk_for e_meta loop_id (iv_type loop_t) loop_target loop_body]
  in

  (* Handles application of both pure lambda and global functions for
   * collection transformers *)
  let apply_if_function da fn_args = expr_of_decl_args (match da with
    (* Pure lambdas are tagged as constructors *)
    | Constructor([e]) -> Init(e)
    
    (* Global functions yield vars *)
    | Init(e) ->
      begin match U.tag_of_expr e with
        | Var fn -> Init(U.mk_fn (U.meta_of_expr e) (Named fn) fn_args)
        | _ -> Init(e)
      end

    | _ -> failwith "invalid reified function")
  in

  let mk_appfn_loop_body_cmds is_map result_id result_t fn_e fn_pair fn_reify_cmds loop_id loop_t =
    let bindings, bound_vars = mk_elem_bindings fn_e loop_id loop_t in
    bindings@fn_reify_cmds@(unwrap_pair
      (fun da ->
        let applied_expr = apply_if_function da bound_vars in 
        if is_map then [insert_expr result_id result_t applied_expr] 
        else [expr_cmd applied_expr])
      (fun cmds -> if is_map then failwith "invalid reified map function result"
                   else cmds)
      (fun da cmds -> if is_map then cmds@[insert_expr result_id result_t (expr_of_decl_args da)]
                      else failwith "invalid reified iterate function result")
      fn_pair)
  in 
  
  let mk_appfn_loop_cmds is_map fn_e c_e fn_pair c_pair =
    let fn_reify_cmds = List.flatten (List.assoc fn_pair reify_cmds_by_children) in
    let c_reify_cmds = List.flatten (List.assoc c_pair reify_cmds_by_children) in
    let body_fn = mk_appfn_loop_body_cmds is_map id t fn_e fn_pair fn_reify_cmds in
    let body = collection_loop (type_of_expr c_e) c_pair c_reify_cmds body_fn
    in None, Some(body)
  in

  let mk_agg_loop_body_cmds result_id result_t
                            agg_fn_e agg_fn_pair agg_fn_reify_cmds
                            loop_id loop_t
  =
    let bindings, bound_vars =
      mk_agg_elem_bindings agg_fn_e result_id (i_type result_t) loop_id (iv_type loop_t) in
    let acc_cmds = unwrap_pair 
      (fun da -> 
        let applied_expr = apply_if_function da bound_vars in
        match U.tag_of_expr applied_expr with
          | Fn _ -> [expr_cmd applied_expr] (* Global function invocation *)
          | _ -> [] (* Everything else *)
      )
      (fun cmds -> cmds)
      (fun da cmds -> cmds)
      agg_fn_pair
    in bindings@agg_fn_reify_cmds@acc_cmds
  in

  (* Start of conversion method *)
  match K3Util.tag_of_expr e with
  | K3.AST.Const c -> ret_term e (Const c)
  | K3.AST.Var id  -> ret_term e (Var id)
  | K3.AST.Tuple   -> ret_expr e Tuple
  | K3.AST.Just    -> ret_expr e Just

  | K3.AST.Empty v_t     -> ret_cmds []
  | K3.AST.Singleton v_t -> ret_cstr ()
  | K3.AST.Combine       ->
      ret_common (fun exprs pre_cmds ->
        let cmds = reify_cmds@pre_cmds@(List.map (fun e ->
            expr_cmd (coll_expr (i_type t) Combine [result_var; e])
          ) exprs)
        in None, Some(cmds))

  | K3.AST.Range c_t ->
    if reify_cmds = [] then ret_expr e (Fn (Collection Range))
    else
    begin match children with
      | [start_pair; stride_pair; steps_pair] ->
        let exprs, pre_cmds = List.fold_left (fun (e_acc,cmd_acc) pair ->
            unwrap_pair
              (fun da -> e_acc@[expr_of_decl_args da], cmd_acc)
              (fun cmds -> failwith "invalid sub expression for range")
              (fun da cmds -> e_acc@[expr_of_decl_args da], cmd_acc@cmds)
              pair
          ) ([],[]) children
        in
        let body_fn loop_id loop_t =
          (* TODO: this code should be generated when handling the Range function.
          let start_e, stride_e = (List.nth exprs 0), (List.nth exprs 1) in
          let val_expr =
            U.mk_op (U.meta_of_expr start_e) Add
              [start_e; (U.mk_op (U.meta_of_expr stride_e) Mult [U.mk_var loop_id; stride_e])]
          in
          *)
          let loop_var = U.mk_var ((iv_type loop_t), mk_meta()) loop_id
          in [insert_expr id t loop_var]
        in
        let e_t = type_of_expr e in
        let c_pair = Some(Init(coll_expr (TInternal e_t) Range exprs)), Some(pre_cmds) in
        let cmds = collection_loop e_t c_pair reify_cmds body_fn
        in None, Some(cmds)

      | _ -> failwith "invalid range reification"
    end

  | K3.AST.Add  -> ret_binop e Add
  | K3.AST.Mult -> ret_binop e Mult
  | K3.AST.Neg  -> ret_binop e Neg

  | K3.AST.Eq  -> ret_binop e Eq
  | K3.AST.Neq -> ret_binop e Neq
  | K3.AST.Lt  -> ret_binop e Lt
  | K3.AST.Leq -> ret_binop e Leq

  | Lambda arg -> 
    let ch_pair = List.hd children in
    unwrap_pair
      (fun da -> 
        (* Convert to constructor to track this as a pure lambda body
         * This is a hack for now, and needs a more specialized return type
         * from imperative_of_expr_node *)
        match da with | Init e -> (Some(Constructor [e]), None)
                      | _ -> Some(da), None)
      (fun cmds -> ch_pair)
      (fun da cmds -> ch_pair) 
      ch_pair  

  | Apply ->
    let fn_e, arg_e = decompose_apply e in
    
    (* Bindings helpers *) 
    let needs_bindings tvars arg_exprs =
      let arg_ids = List.flatten (List.map (fun e ->
        match U.tag_of_expr e with | Var id -> [id] | _ -> []
        ) arg_exprs)
      in
      let arg_matches = List.length (List.filter (fun (id,t) -> List.mem id arg_ids) tvars) in
      if arg_matches > 0 && arg_matches <> (List.length tvars) then
        failwith "unexpected partial matches for function application bindings"
      else arg_matches = 0
    in
    let get_arg_cmds is_global_fn arg_pair =
      let unwrap_if_tuple e = match U.tag_of_expr e with
        | Tuple -> sub_tree e
        | _ -> [e]
      in  
      let mk_bindings da =
        let tvars = get_fn_app_typed_vars fn_e in
        let arg_types = List.map (fun (_,vt) -> iv_type vt) tvars in
        let arg_exprs = unwrap_if_tuple (expr_of_decl_args da) in
        let add_bindings =
          if is_global_fn then List.length tvars > List.length arg_exprs 
          else needs_bindings tvars arg_exprs
        in 
        if add_bindings then (mk_app_fn_bindings tvars arg_exprs), arg_types
        else ([], []), arg_types
      in
        unwrap_pair
          (fun da -> mk_bindings da)
          (fun cmds -> (cmds, []), []) 
          (fun da cmds -> let (x,y),z = mk_bindings da in ((cmds@x), y), z)
          arg_pair
    in

    (* Subcommand helpers *)
    let extra_apply_cmds() = match reify_cmds_by_children with
      | [(_,x);(_,y)] -> (List.flatten x), (List.flatten y)
      | [(_,x)] -> [], (List.flatten x)
      | [] -> [], []
      | _ -> failwith "invalid apply reification children"
    in
    let apply_result fn_da_opt arg_cmds fn_cmds =
      let reify_fn_cmds, reify_arg_cmds = extra_apply_cmds() in
      (fn_da_opt, Some (reify_arg_cmds@arg_cmds@reify_fn_cmds@fn_cmds))
    in

    (* Imperative AST generation *)
    begin match children with
      (* Pure lambda functions. These do not need to propagate their expression
       * bodies, since they are guaranteed to be reified. *)
      | [(Some(Constructor([e])), None); arg_pair] ->
          apply_result None (fst (fst (get_arg_cmds false arg_pair))) []
      
      (* Global function calls *)
      | [(Some(Init(e)), None); arg_pair] ->
        begin match U.tag_of_expr e with
          | Var fn ->
            let (arg_cmds, possible_args), arg_types = get_arg_cmds true arg_pair in 
            let fn_args = match possible_args with
              | [] -> 
                (try List.map2 (fun id t -> U.mk_var (t, mk_meta()) id)
                               (List.map snd reify_cmds_and_ids) arg_types
                 with Invalid_argument _ -> failwith "invalid global function application")
              | a -> a
            in
            let fn_call = U.mk_fn (U.meta_of_expr e) (Named fn) fn_args
            in apply_result (Some(Init(fn_call))) arg_cmds []

          | _ -> apply_result (Some(Init(e))) (fst (fst (get_arg_cmds false arg_pair))) []
        end

      (* Everything else *)
      | [(fn_da_opt, Some fn_cmds); arg_pair] ->
          apply_result fn_da_opt (fst (fst (get_arg_cmds false arg_pair))) fn_cmds

      | _ -> failwith "invalid apply reification"
    end

  | K3.AST.Block ->
    let init, back = split_last children in
    let block_init_cmds =
      (List.fold_left
        (fun cmd_acc dac_pair ->
          unwrap_pair
            (fun da -> failwith "invalid block expression element")
            (fun cmds -> cmd_acc@cmds)
            (fun da cmds -> failwith "invalid block expression element")
            dac_pair)
        [] init)@reify_cmds
    in
      unwrap_pair
        (fun da -> (Some da, Some (block_init_cmds))) 
        (fun cmds -> (None, Some (block_init_cmds@cmds)))
        (fun da cmds -> (Some da, Some(block_init_cmds@cmds)))
        back

  | K3.AST.IfThenElse ->
    (* Note this blindly assumes every ifelse is materialized, and thus the
     * reified sym is always associated with the ifelse expression.
     *
     * We could fix this by also passing down the expression that is intended
     * to be the source for the reified sym, and explicitly using an equality
     * test prior to code generating for an expression node. *)

    let then_reify_cmds, else_reify_cmds =    
      match reify_cmds_and_ids with
        | [] -> [], []
        | [a,_;b,_] -> a, b 
        | _ -> failwith "invalid ifelse reified sub cmds"
    in
    begin match children with
      | [(x, None); (y, None); (z, None)] ->
        begin match then_reify_cmds, else_reify_cmds with
          | [], [] -> ret_expr e (Op Ternary)
          | a, b ->
		        let pred_expr = expr_of_decl_args (unwrap x) in
            let branches = [U.mk_block (unit_t, mk_meta()) (a@[assign_da id (unwrap y)]);
                            U.mk_block (unit_t, mk_meta()) (b@[assign_da id (unwrap z)])]
            in None, Some([cond_expr pred_expr branches])
        end

      | [pred_pair; then_pair; else_pair] -> 
        let pred_expr, pre_cmds = unwrap_expr_and_cmds pred_pair in
        let branches = List.map (fun (ec_pair, reify_cmds) ->
            U.mk_block (unit_t, mk_meta()) (unwrap_pair
              (fun da -> reify_cmds@[assign_da id da])
              (fun cmds -> reify_cmds@cmds)
              (fun da cmds -> reify_cmds@cmds@[assign_da id da])
              ec_pair))
          [then_pair, then_reify_cmds; else_pair, else_reify_cmds]
        in
        let ret_cmds = pre_cmds@[cond_expr pred_expr branches]
        in None, Some(ret_cmds)

      | _ -> failwith "invalid reified ifelse children"
    end

  | Iterate ->
    begin match children with
      | [fn_pair; c_pair] -> 
        let fn_e, c_e = decompose_iterate e
        in mk_appfn_loop_cmds false fn_e c_e fn_pair c_pair
      | _ -> failwith "invalid reified iterate children"
    end

  | Map ->
    begin match children with
      | [fn_pair; c_pair] ->
        let fn_e, c_e = decompose_map e
        in mk_appfn_loop_cmds true fn_e c_e fn_pair c_pair
      | _ -> failwith "invalid reified map children"
    end

  | FilterMap ->
    begin match children with
      | [filter_fn_pair; map_fn_pair; c_pair] ->
        let f_fn_e, m_fn_e, c_e = decompose_filter_map e in
        let filter_fn_reify_cmds, map_fn_reify_cmds, c_reify_cmds =
          match List.map (fun p -> List.flatten (List.assoc p reify_cmds_by_children)) children with
            | [a;b;c] -> a,b,c
            | _ -> failwith "invalid filtermap reified sub cmds"
        in
        let filter_body_cmds loop_id loop_t =
          let filter_bindings, filter_bound_vars = mk_elem_bindings f_fn_e loop_id loop_t in
          let map_body = mk_appfn_loop_body_cmds true id t m_fn_e map_fn_pair map_fn_reify_cmds loop_id loop_t in
          let map_branches = [U.mk_block (unit_t, mk_meta()) map_body;
                              expr_cmd (U.mk_const (unit_t, mk_meta()) CUnit)] in
          let pred_expr apply da =
            (* Global function application for filter function *)
            if apply then apply_if_function da filter_bound_vars
            else expr_of_decl_args da 
          in
          let filter_cmds =
            unwrap_pair
              (fun da -> [cond_expr (pred_expr true da) map_branches])
              (fun cmds -> failwith "invalid filtermap predicate expression")
              (fun da cmds -> cmds@[cond_expr (pred_expr false da) map_branches])
              filter_fn_pair
          in filter_bindings@filter_fn_reify_cmds@filter_cmds
        in
        let filter_body = collection_loop (type_of_expr c_e) c_pair c_reify_cmds filter_body_cmds
        in None, Some(filter_body) 
      | _ -> failwith "invalid reified filter-map children"
    end

  | Flatten ->
    (* Loop over nested collection and concat, essentially ext(id)/flatmap *)
    let c_e = List.hd (sub_tree e) in
    let c_pair = List.hd children in
    let flatten_body_cmds loop_id loop_t =
      let loop_var = U.mk_var ((iv_type loop_t), mk_meta()) loop_id
      in [expr_cmd (coll_expr (i_type t) Combine [result_var; loop_var])]
    in
    let flatten_body = collection_loop (type_of_expr c_e) c_pair reify_cmds flatten_body_cmds
    in None, Some(flatten_body)
  
  | Aggregate ->
    begin match children with
      | [fn_pair; init_pair; c_pair] ->
        let agg_fn_e, init_e, c_e = decompose_aggregate e in
        let init_cmds = match snd init_pair with None -> [] | Some(x) -> x in
        let fn_reify_cmds, init_reify_cmds, c_reify_cmds =
          match K3Util.tag_of_expr agg_fn_e with
            | Lambda _ ->
              begin match reify_cmds_and_ids with
                | fn_sub :: (init_sub :: rest) ->
                  (fst fn_sub), (fst init_sub), List.flatten (List.map fst rest)
                | _ -> failwith "invalid aggregate sub cmds"
              end

            | _ ->
              begin match reify_cmds_and_ids with
                | init_sub :: rest ->
                  [], (fst init_sub), List.flatten (List.map fst rest) 
                | _ -> failwith "invalid aggregate sub cmds"
              end
        in
        
        let mk_agg_body_fn = mk_agg_loop_body_cmds id t agg_fn_e fn_pair fn_reify_cmds in
        let cmds = init_reify_cmds@init_cmds@(
          collection_loop (type_of_expr c_e) c_pair c_reify_cmds mk_agg_body_fn)
        in None, Some(cmds)
      | _ -> failwith "invalid reified aggregate children"
    end

  | GroupByAggregate ->
    begin match children with
      | [gb_fn_pair; agg_fn_pair; init_pair; c_pair] ->
        let gb_fn_e, agg_fn_e, init_e, c_e = decompose_gbagg e in
        let init_expr, init_cmds = unwrap_expr_and_cmds init_pair in
        
        let gb_fn_reify_cmds, agg_fn_reify_cmds, init_reify_cmds, c_reify_cmds =
          match List.map (fun p -> List.flatten (List.assoc p reify_cmds_by_children)) children with
            | [a;b;c;d] -> a,b,c,d
            | _ -> failwith "invalid groupby reified sub cmds"
        in

        let gbagg_body_cmd_fn loop_id loop_t =

          let gb_key_id, gb_val_id = mk_gb_key_sym (), mk_gb_val_sym () in
          let gb_tuple_t, gb_key_t, gb_val_t =
            let result_type = base_of (value_of (type_of_expr e) type_failure) in
            match base_of (snd (collection_of result_type type_failure)) with
              | TTuple([gb_t; a_t]) as t -> (TInternal (TValue(canonical t))), (TInternal (TValue gb_t)), (TInternal (TValue a_t)) 
              | _ -> failwith "invalid groupby collection element type"
          in

          let tuple_meta = gb_tuple_t, mk_meta() in
          let key_meta, key_var =
            let x = gb_key_t, mk_meta() in x, (U.mk_var x gb_key_id)
          in
          let val_meta, val_var =
            let x = gb_val_t, mk_meta() in x, (U.mk_var x gb_val_id)
          in
          let old_val = U.mk_tuple tuple_meta [key_var; val_var] in
          let new_val e = U.mk_tuple tuple_meta [key_var; e] in
          
          let find_expr = coll_expr gb_val_t Find [result_var; key_var] in
          let contains_expr = coll_expr bool_t Contains [result_var; key_var] in
          
          let assign_gbval_cmd =
            let branches = [assign_expr gb_val_id find_expr;
                            assign_expr gb_val_id init_expr]
            in cond_expr contains_expr branches 
          in
          
          let update_cmd e = expr_cmd (coll_expr unit_t Update [result_var; old_val; new_val e]) in
          let new_cmd e = expr_cmd (coll_expr unit_t Insert [result_var; new_val e]) in

          (* Declarations *)
          let decls = List.map (fun (id,t) -> 
              U.mk_decl (unit_t, mk_meta()) (U.mk_var_decl id t None)
            ) [gb_key_id, gb_key_t; gb_val_id, gb_val_t]
          in

          (* Bindings *)
          let gb_bindings, gb_bound_vars = mk_elem_bindings gb_fn_e loop_id loop_t in
          let bindings, agg_bound_vars =
            mk_agg_elem_bindings agg_fn_e gb_val_id gb_val_t loop_id (iv_type loop_t)
          in
          
          (* Grouping function evaluation *)
          let mk_assign_gb_key_val apply da =
            (* Global function application for grouping function *)
            let e = if apply then apply_if_function da gb_bound_vars
                    else expr_of_decl_args da
            in [assign_expr gb_key_id e; assign_gbval_cmd]
          in
          let gb_cmds = unwrap_pair
            (mk_assign_gb_key_val true)
            (fun cmds -> failwith "invalid reified group function result")
            (fun da cmds -> cmds@(mk_assign_gb_key_val false da))
            gb_fn_pair
          in

          (* Aggregate function evaluation *)
          let agg_cmds = 
            let replace_group_cmd e =
              let branches = [update_cmd e; new_cmd e]
              in cond_expr contains_expr branches
            in
				    let acc_cmds = unwrap_pair
              (* Global function application for aggregate function *) 
				      (fun da -> [replace_group_cmd (apply_if_function da agg_bound_vars)])
				      (fun cmds -> failwith "invalid reified groupby aggregation function result")
				      (fun da cmds -> cmds@[replace_group_cmd (expr_of_decl_args da)])
				      agg_fn_pair
				    in bindings@agg_fn_reify_cmds@acc_cmds
          in
          
          (* Loop body *)
          decls@gb_bindings@gb_fn_reify_cmds@gb_cmds@agg_cmds
            
        in
        let cmds = init_reify_cmds@init_cmds@(
          collection_loop (type_of_expr c_e) c_pair c_reify_cmds gbagg_body_cmd_fn)
        in None, Some(cmds)

      | _ -> failwith "invalid reified group-by children"
    end

  | K3.AST.Sort -> ret_cmd_expr (coll_expr (TInternal (type_of_expr e)) Sort)

  | K3.AST.Peek -> ret_expr e (Fn (Collection Peek))
  | K3.AST.Slice -> ret_expr e (Fn (Collection Slice))
  | K3.AST.Insert -> ret_cmd_expr (coll_expr unit_t Insert)
  | K3.AST.Delete -> ret_cmd_expr (coll_expr unit_t Delete)
  | K3.AST.Update -> ret_cmd_expr (coll_expr unit_t Update)

  | K3.AST.Send ->
    let _, _, send_args_e = decompose_send e in
    let type_tag = String.concat "_"
      (List.map (fun e -> signature_of_type (type_of_expr e)) send_args_e)
    in ret_cmd_expr (U.mk_fn (unit_t, mk_meta()) (Send type_tag))

  (* TODO: generates a side-effecting assignment command *) 
  | K3.AST.Assign -> failwith "imperative assign not yet implemented"

  (* TODO: primitive target language operation *)
  | Deref -> failwith "imperative deref not yet implemented"
  

let imperative_of_expr mk_meta fn_arg_env 
                       reify_cmds_and_ids ((id,t,decl,assign) as reified_sym) e
=
  let declare assign id t node_pair =
    let d_f da_opt = [U.mk_decl (unit_t, mk_meta()) (U.mk_var_decl id (TInternal t) da_opt)] in
    unwrap_pair
      (fun da -> d_f (Some da))
      (fun cmds -> (d_f None)@cmds)
      (fun da cmds ->
        if not assign then (d_f (Some da))@cmds
        else
          let e = expr_of_decl_args da
          in (d_f None)@cmds@[U.mk_assign (U.meta_of_expr e) id e])
      node_pair
  in
  let assign_if_expr id t node_pair =
    unwrap_pair
      (fun da ->
        let e = expr_of_decl_args da
        in [U.mk_assign (U.meta_of_expr e) id e])
      (fun cmds -> cmds)
      (fun da cmds ->
        let e = expr_of_decl_args da
        in cmds@[U.mk_assign (U.meta_of_expr e) id e])
      node_pair
  in
  let ensure_cmd node_pair = match node_pair with
      | None, Some(x) -> x
      | _,_ -> failwith "invalid cmd"
  in
  (*let (node_pair : ('a decl_args_t option) * ('a cmd_t list option))  =*)
  let node_pair =
    fold_tree
      (fun _ e2 -> (if e = e2 then reify_cmds_and_ids else []), reified_sym)
      (imperative_of_expr_node mk_meta fn_arg_env)
      (reify_cmds_and_ids, reified_sym) (None,None) e
  in
  match decl, assign with
  | true, _ -> declare assign id t node_pair
  | false, true -> assign_if_expr id t node_pair
  | false, false -> ensure_cmd node_pair


let imperative_of_reified_node mk_meta fn_arg_env _ sub_cmds_and_ids rt =
  let reified_sym, e = node_data rt in
  let (id,_,_,_) = reified_sym
  in (imperative_of_expr mk_meta fn_arg_env sub_cmds_and_ids reified_sym e), id 


let imperative_of_reified_expr mk_meta fn_arg_env rt =
  fst (fold_tree (fun _ _ -> None)
        (imperative_of_reified_node mk_meta fn_arg_env) None ([],"") rt)

let imperative_of_instruction mk_meta i = match i with
  | Consume id ->
      (* TODO: named function for consume *)
      U.mk_expr (unit_t, mk_meta()) (U.mk_fn (unit_t, mk_meta()) (Named ("consume_"^id))[])

let imperative_of_stream_statement mk_meta ss = match ss with
  | Stream s -> failwith "stream declaration not implemented"
  | Bind (src,dest) -> failwith "bind declarations not implemented"
  | Instruction i -> imperative_of_instruction mk_meta i


let imperative_of_declaration mk_meta fn_arg_env (d,m) =
  let is_function t = match t with | TFunction _ -> true | _ -> false in
  let decompose_function_type t = match t with 
    | TFunction (arg,ret) -> arg, ret
    | _ -> failwith "invalid function type" 
  in
  let cmds_of_reified_expr e = 
    imperative_of_reified_expr mk_meta fn_arg_env (reify_expr fn_arg_env e)
  in
  match d with
  | Global  (id, t, init) ->
      if is_function t then
        let arg_t, ret_t = decompose_function_type t in
        let iret_t = TInternal(TValue ret_t) in
        begin match init with
          | Some(e) ->
            let arg = match arg_of_lambda e with
              | Some(a) -> a 
              | None -> failwith "invalid global function declaration" 
            in
            let cmds = cmds_of_reified_expr e in
            let fn_body = [U.mk_block (iret_t, mk_meta()) cmds]
            in Some(DFn(id, arg, iret_t, fn_body), (unit_t,m)), [], [id,arg]
          | _ -> failwith "invalid function body"
        end
      else
        let cmds = match init with
          | Some(e) -> cmds_of_reified_expr e
          | _ -> [] 
        in Some(DVar(id, TInternal(t), None), (unit_t,m)), cmds, []
        
  | Foreign (id,t) ->
    (* TODO: special handling for fixed foreign functions *) 
    begin match id with
      | "sin" -> None, [], []
      | _ -> failwith ("unsupported foreign function: "^id)
    end

  | Trigger (id,a,decls,body) ->
    let local_decl_cmds = List.map (fun (id, t, ann) ->
        U.mk_decl (unit_t,ann) (DVar (id, TInternal(TValue t), None)) 
      ) decls
    in
    let cmds = cmds_of_reified_expr body in
    let trig_body = [U.mk_block (unit_t, mk_meta()) (local_decl_cmds@cmds)]
    in Some(DFn(id, a, unit_t, trig_body), (unit_t,m)), [], [id,a]

  (* Roles are handled separately and compiled to functions *)
  | Role (id, sp) -> None, [], []
  | DefaultRole(id) -> None, [], []


(* TODO: file_stream and network_stream objects in backend runtime, or
 * add these as required types in the target language *)
let type_and_constructor_of_channel mk_meta (ct,cf) = match ct,cf with
	| File(f), CSV -> 
	  let meta = TInternal(TValue(canonical TString)), mk_meta()
	  in TNamed("file_stream"), Some(Constructor([U.mk_const meta (CString f)]))
	
	| Network(addr), CSV -> 
	  let cstr_args = 
	    let ip,port = addr in
	    let ip_meta = TInternal(TValue(canonical TString)), mk_meta() in
	    let port_meta = TInternal(TValue(canonical TInt)), mk_meta() in
	    [U.mk_const ip_meta (CString ip); U.mk_const port_meta (CInt port)]
	  in TNamed("network_stream"), Some(Constructor(cstr_args))
	
	| _, _ -> failwith "unsupported stream channel"

let imperative_of_stream mk_meta (id, stream) =
  let t, da_opt = match stream with
    | Source(id,t,ch) -> type_and_constructor_of_channel mk_meta ch
    | Sink(id,t,ch) -> type_and_constructor_of_channel mk_meta ch
    | _ -> failwith "cannot generate code for a derived stream"
  in DVar(id, t, da_opt)

(* FSM variable construction helpers *)
let mk_fsm_id_var fsm_id meta suffix = 
  let x = "fsm_"^fsm_id^suffix in x, U.mk_var meta x

let mk_state_var_pair fsm_meta fsm_id = mk_fsm_id_var fsm_id fsm_meta "_current_state"
let mk_event_var_pair event_meta fsm_id = mk_fsm_id_var fsm_id event_meta "_event" 
  

(* Function declaration to run an FSM.
 * The function accepts an argument of a state to run, and is implemented as a
 * sequenced conditional that testing whether to apply the actions of each state.
 * For action, we check whether the operation succeeds and then return the
 * next state according to the FSM's transition table.
 *)  
(* TODO: state transition body generation, and trigger dispatch on value generation *)
(* TODO: reset, read, write, schedule functions *)
let imperative_of_fsm mk_meta prog fsm_env stream_bindings (id,fsm) =
  let triggers = List.map snd (List.filter (fun (x,y) -> x = id) stream_bindings) in
  let event_type = 
    match triggers with
    | [] -> TInternal (TValue (canonical TUnknown))
    | h::t ->
      let error() = failwith ("invalid trigger declaration for "^h) in
      try begin match fst (trigger_of_program h prog) with
        | Trigger(_,arg,_,_) -> TInternal (TValue (canonical (TMaybe (deduce_arg_type arg))))
        | _ -> error()
      end
      with Not_found -> error()
  in 

  let unit_meta, fsm_meta, pred_meta, event_meta =
    (unit_t, mk_meta()), (int_t, mk_meta()), (bool_t, mk_meta()), (event_type, mk_meta()) 
  in
  
  let state_var_id, state_var = mk_state_var_pair fsm_meta id in
  let state_event_id, state_event_var = mk_event_var_pair event_meta id in

  let return state_id = U.mk_return unit_meta (U.mk_const fsm_meta (CInt state_id)) in
  let error = return (-1) in
  
  let decl_event e =
    U.mk_decl unit_meta 
      (U.mk_var_decl state_event_id event_type (Some (Init e)))
  in
  
  let event_access_fn endpoint_meta id ch =
	  let fn_tag = match ch with In(_) -> Named "read" | Out(_) -> Named "write"
	  in decl_event (U.mk_fn event_meta fn_tag [U.mk_var endpoint_meta id])
  in

  let schedule_exprs = 
    let str_meta = TInternal (TValue (canonical TString)), mk_meta() in
    let expr trig_id = U.mk_expr unit_meta
      (U.mk_fn unit_meta (Named "schedule") [U.mk_const str_meta (CString trig_id); state_event_var])
    in List.map expr triggers 
  in

  let fsm_body =
    List.fold_left (fun else_branch (sid, (a,nid,fid)) ->
      let fsm_pred = U.mk_op pred_meta Eq [state_var; U.mk_const fsm_meta (CInt sid)] in
      let action_cmd =
        let cmds = match a with
	      | AEndpoint (id,_,sc,ch) ->
	        let endpoint_meta = (fst (type_and_constructor_of_channel mk_meta sc)), mk_meta() in 
	        let endpoint_cmd = event_access_fn endpoint_meta id ch in
	        let succeed_or_fail_cmd =
	          let valid_pred = U.mk_op pred_meta Neq [state_event_var; U.mk_const event_meta CNothing] in
	          let succeed_cmd = U.mk_block unit_meta (schedule_exprs@[return nid]) in
	          U.mk_ifelse pred_meta valid_pred [succeed_cmd; return fid] 
	        in [endpoint_cmd; succeed_or_fail_cmd]
	
	      | ADerived fsm_id ->
          (* TODO: handle change of executing FSM *) 
			    let fsm = try List.assoc fsm_id fsm_env with Not_found -> 
			      failwith ("no fsm named "^fsm_id^" found when compiling consume instruction")
			    in
          let init = U.mk_const fsm_meta (CInt (fst (List.hd fsm))) in
	        [U.mk_return unit_meta (U.mk_fn unit_meta (Named ("fsm_"^fsm_id)) [init])]
	
	      | AEOF -> [error]
        in U.mk_block unit_meta cmds
      in
      U.mk_ifelse unit_meta fsm_pred [action_cmd; else_branch]
    ) error fsm
  in
  let fsm_arg = AVar(state_var_id, canonical TInt) in
  let fsm_ret_t = TInternal(TValue(canonical TInt))
  in [DFn("fsm_"^id, fsm_arg, fsm_ret_t, [fsm_body])]


(* Generates a role body for the given instructions *)
let imperative_of_instruction mk_meta fsm_env instr = match instr with
  | Consume id ->
    let unit_meta, int_meta, bool_meta = (unit_t, mk_meta()), (int_t, mk_meta()), (bool_t, mk_meta()) in
    let fsm = try List.assoc id fsm_env with Not_found -> 
      failwith ("no fsm named "^id^" found when compiling consume instruction")
    in
    let state_var_id, state_var = mk_state_var_pair int_meta id in
    let decl = 
      let init_e = U.mk_const int_meta (CInt (fst (List.hd fsm)))
      in U.mk_decl unit_meta (DVar(state_var_id, int_t, Some(Init(init_e))))
    in
    let body =
      let consume_expr = 
        U.mk_assign unit_meta state_var_id
          (U.mk_fn unit_meta (Named ("fsm_"^id)) [state_var])
      in
      let valid_state_pred = U.mk_op bool_meta Leq [U.mk_const int_meta (CInt 0); state_var]
      in U.mk_while unit_meta valid_state_pred [consume_expr]
    in
    U.mk_block unit_meta [decl; body]


(* Generates imperative code for an event loop, including its stream declarations,
 * functions for FSMs, and a role body for the given instructions *)
let imperative_of_event_loop mk_meta prog (senv, fsm_env, bindings, instrs) =
  let stream_decls = List.map (imperative_of_stream mk_meta) senv in
  let fsm_decls = 
    let fsm_gen = imperative_of_fsm mk_meta prog fsm_env bindings
    in List.flatten (List.map fsm_gen fsm_env)
  in
  let r = List.map (imperative_of_instruction mk_meta fsm_env) instrs in
  (stream_decls@fsm_decls), r  


(* Imperative AST generation for all roles in an imperative program.
 * This function returns the necessary declarations for a role, and a role
 * dispatch body for the query engine's entry point. *)
let imperative_of_roles mk_meta prog =
  let unit_meta, int_meta, bool_meta, str_meta =
    (unit_t, mk_meta()), (int_t, mk_meta()), (bool_t, mk_meta()),
    (TInternal (TValue (canonical TString)), mk_meta())
  in
  let roles, default_role = roles_of_program prog in
  let role_decls = List.fold_left (fun acc (id,evt_loop) ->
      let loop_decls, role_body = imperative_of_event_loop mk_meta prog evt_loop in
      (* TODO: change this to AIgnored when Shyam has implemented it *)
      let role_arg = AVar("_", canonical TUnit)
      in acc@loop_decls@[DFn("role_"^id, role_arg, unit_t, role_body)]
    ) [] roles
  in
  let dispatch_body =
    (* TODO: check command line argument to dispatch role *)
    let role_var = U.mk_var str_meta "user_role" in 
    let default =
	    match default_role with 
	      | None -> U.mk_return unit_meta (U.mk_const int_meta (CInt 1))
	      | Some(id,_) -> 
	        let role_fn = 
	          U.mk_fn unit_meta (Named ("role_"^id)) [U.mk_const unit_meta CUnit]
	        in U.mk_expr unit_meta role_fn
    in
    List.fold_left (fun case_acc (id,_) ->
	    let role_fn = U.mk_fn unit_meta (Named ("role_"^id)) [U.mk_const unit_meta CUnit] in
      let pred_e = U.mk_op bool_meta Eq [role_var; U.mk_const str_meta (CString id)]
      in U.mk_ifelse unit_meta pred_e [U.mk_expr unit_meta role_fn; case_acc]
      ) default roles  
  in
    role_decls, [dispatch_body; U.mk_return unit_meta (U.mk_const int_meta (CInt 0))]
  
let imperative_of_program mk_meta p =
  let main_body, decls, _ =
    List.fold_left (fun (main_cmd_acc, decl_acc, fn_arg_env) (d,m) ->
	    let d_opt, init_cmds, fn_args = imperative_of_declaration mk_meta fn_arg_env (d,m) in
	    (main_cmd_acc@init_cmds),
	    (match d_opt with | Some c -> decl_acc@[c] | None -> decl_acc),
	    (fn_arg_env@fn_args)
    ) ([],[],[]) p
  in
  let role_decls, dispatch_body = 
    let x, y = imperative_of_roles mk_meta p in
    (List.map (fun d -> d, (unit_t, mk_meta())) x), y
  in
  let main_arg = wrap_args ["argc", canonical TInt; "argv", canonical TString] in
  let main_fn = DFn("main", main_arg, int_t, main_body@dispatch_body) 
  in decls@role_decls@[main_fn, (unit_t,mk_meta())]

end
