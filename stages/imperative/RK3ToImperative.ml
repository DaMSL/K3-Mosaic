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
open Runtime
open Remoting

module Make = functor (Lang : TargetLanguage) ->
struct

(* Import AST and utilities *)
module ASTImport = struct module AST = Imperative.AST(Lang) end
open ASTImport.AST

module R = Runtime.Make(Lang)
module C = Remoting.Make(Lang)
module U = ImperativeUtil.Util(Lang)

open R
open C

(* Helpers *)
let init l = List.rev (List.tl (List.rev l))
let back l = List.hd (List.rev l)
let split_last l = let x = List.rev l in List.rev (List.tl x), List.hd x

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

(* Naming helpers for code generation *)
let mk_role_class_id role_id = "role_"^role_id

let mk_source_ids_class_id role_id = "source_"^role_id
let mk_source_ids_var_id role_id = "source_ids_"^role_id
  
let mk_resource_id resource_id = resource_id^"_id"

let mk_resource_var mk_meta role_id resource_id =
  let meta t = t, mk_meta() in
  let int_meta = meta (U.ib_type TInt) in
  let source_ids_meta = meta (TNamed (mk_source_ids_class_id role_id)) in
  U.mk_fn int_meta
    (Member (Field (mk_resource_id resource_id)))
    [U.mk_var source_ids_meta (mk_source_ids_var_id role_id)]

let mk_options_id () = "options"
let mk_options_var mk_meta =
  U.mk_var (TNamed options_class_id, mk_meta()) (mk_options_id())

let mk_fsm_id_var fsm_id meta suffix = 
  let x = "fsm_"^fsm_id^suffix in x, U.mk_var meta x

let mk_state_var_pair fsm_meta fsm_id = mk_fsm_id_var fsm_id fsm_meta "_current_state"
let mk_event_var_pair event_meta fsm_id = mk_fsm_id_var fsm_id event_meta "_event" 
let mk_source_var_pair source_meta fsm_id = mk_fsm_id_var fsm_id source_meta "_source"


(* Conversion methods *)

(* TODO: this relies on range and lambda normalization. *)
(* Returns: ('a decl_args_t option * 'a cmd_t list option) * protocol_spec *)
let imperative_of_expr_node mk_meta fn_arg_env 
                            (reify_cmds_and_ids_with_proto, (id,t,_,_))
                            children_with_protospecs e
=
  let error s =
    print_endline ("Error building imperative AST: "^s);
    print_endline (string_of_expr e);
    failwith ("Error building imperative AST: "^s)
  in
  
  (* TODO: unify with typechecker errors *)
  let type_failure () = failwith "invalid type" in
  
  let i_meta e = (U.i_type (type_of_expr e)), meta_of_expr e in
  let result_var = U.mk_var ((U.i_type t), mk_meta()) id in
  let e_meta = i_meta e in

  let reify_cmds_and_ids, rt_proto = List.split reify_cmds_and_ids_with_proto in
  
  let children = List.map fst children_with_protospecs in
  let children_protospecs =
    let x,y = List.split (List.map snd children_with_protospecs) in
    let fx, fy = List.flatten x, List.flatten y in
    let rx, ry = List.split rt_proto in
    fx@(List.flatten rx), fy@(List.flatten ry)
  in

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
  let ret_with_proto ?(proto_spec=children_protospecs) p = p, proto_spec in
  let ret_common ?(sub=children) ?(proto_spec=children_protospecs) expr_and_cmd_f =
    let sub_da, sub_cmds = 
      let x, y = List.split sub in
        (List.flatten (List.map (function None -> [] | Some(c) -> [c]) x)),
        (List.flatten (List.map (function None -> [] | Some(c) -> c) y))
    in
    (* We expect subexpressions here, so any constructors returned will fail. *)
    let sub_expr = List.map expr_of_decl_args sub_da
    in ret_with_proto ~proto_spec:proto_spec (expr_and_cmd_f sub_expr sub_cmds)
  in
  let ret_expr ?(sub=children) e tag =
    ret_common ~sub:sub (fun exprs pre_cmds ->
      if not (check_tag_arity (tag_of_expr e) exprs) then
        error "invalid pure expression reification"
      else
      let cmd_opt = match reify_cmds@pre_cmds with [] -> None | x -> Some(x) in
      Some(Init(U.mk_iexpr tag (i_meta e) exprs)), cmd_opt)
  in
  let ret_cmd_expr ?(proto_spec=children_protospecs) expr_f =
    ret_common ~proto_spec:proto_spec (fun exprs pre_cmds ->
      None, Some(reify_cmds@pre_cmds@[U.mk_expr (U.unit_t, mk_meta()) (expr_f exprs)]))
  in 
  let ret_cstr () =
    ret_common (fun exprs pre_cmds -> 
      Some(Constructor(exprs)), (match pre_cmds with [] -> None | x -> Some(x)))
  in
  let ret_term e tag = ret_expr e tag ~sub:[] in
  let ret_binop e op_tag = ret_expr e (Op op_tag) in
  let ret_cmds cmds = ret_with_proto (None, Some(cmds)) in
  
  (* Lambda binding declarations *)
  let mk_bind_decl id t var =
    let dmeta = U.unit_t, mk_meta() in
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
        let t = U.iv_type vt in 
        let decl = mk_position_decl v t i e
        in i+1, ((decl_acc@[U.mk_decl (U.unit_t, mk_meta()) decl]),
                 (var_acc@[U.mk_var (t, mk_meta()) v]))
      ) (0,([],[])) tvars)
  in

  let mk_app_fn_bindings tvars exprs =
    match tvars with
      | [] -> 
        (* TODO: reconcile with unit function application *)
        failwith "invalid function args"

      | [v,vt] ->
        let t = U.iv_type vt in 
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
              let t = U.iv_type t in 
              (mk_bind_decl id t e), (U.mk_var (t, mk_meta()) id)) tvars exprs)

          | _ -> failwith "mismatched function args" 
        end
  in

  let mk_elem_bindings fn_e elem_id elem_t =
    let tvars = get_fn_app_typed_vars fn_e in
    let elem_meta = (U.iv_type elem_t), mk_meta() in
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
        let dmeta = U.unit_t, mk_meta() in
        let agg_decl = U.mk_var_decl aid (U.iv_type at) (Some agg_v) in
        let elem_decl = U.mk_var_decl eid (U.iv_type et) (Some elem_v)
        in [U.mk_decl dmeta agg_decl; U.mk_decl dmeta elem_decl],
           [U.mk_var agg_meta aid; U.mk_var elem_meta eid]
      | _ -> failwith "invalid aggregate bindings"
  in

  (* Expression and command construction *)
  let assign_da id da = U.mk_assign (U.unit_t, mk_meta()) id (expr_of_decl_args da) in
  let assign_expr id e = U.mk_assign (U.unit_t, mk_meta()) id e in 
  let cond_expr pred branches = U.mk_ifelse (U.unit_t, mk_meta()) pred branches in
  let expr_cmd e = U.mk_expr (U.unit_t, mk_meta()) e in
  let coll_expr t coll_fn_tag children = U.mk_fn (t, mk_meta()) (Collection coll_fn_tag) children in
  let insert_expr id t e = expr_cmd (coll_expr U.unit_t Insert [U.mk_var ((U.i_type t), mk_meta()) id; e]) in

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
    in c_reify_cmds@pre_cmds@[U.mk_for e_meta loop_id (U.iv_type loop_t) loop_target loop_body]
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
      mk_agg_elem_bindings agg_fn_e result_id (U.i_type result_t) loop_id (U.iv_type loop_t) in
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
            expr_cmd (coll_expr (U.i_type t) Combine [result_var; e])
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
          let loop_var = U.mk_var ((U.iv_type loop_t), mk_meta()) loop_id
          in [insert_expr id t loop_var]
        in
        let e_t = type_of_expr e in
        let c_pair = Some(Init(coll_expr (U.i_type e_t) Range exprs)), Some(pre_cmds) in
        let cmds = collection_loop e_t c_pair reify_cmds body_fn
        in ret_with_proto (None, Some(cmds))

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
    ret_with_proto (unwrap_pair
      (fun da -> 
        (* Convert to constructor to track this as a pure lambda body
         * This is a hack for now, and needs a more specialized return type
         * from imperative_of_expr_node *)
        match da with | Init e -> (Some(Constructor [e]), None)
                      | _ -> Some(da), None)
      (fun cmds -> ch_pair)
      (fun da cmds -> ch_pair) 
      ch_pair)  

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
        let arg_types = List.map (fun (_,vt) -> U.iv_type vt) tvars in
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
    let apply_pair = match children with
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
    
    in ret_with_proto apply_pair

  | K3.AST.Block ->
    (* Imperative construction inlines reifications in the order in
     * which they are used by children *)
    let pre_cmds ec_pair =
      try List.flatten (List.assoc ec_pair reify_cmds_by_children)
      with Not_found -> []
    in
    let init, back = split_last children in
    let block_init_cmds =
      List.fold_left
        (fun cmd_acc dac_pair ->
          cmd_acc@(pre_cmds dac_pair)@(unwrap_pair
            (fun da -> failwith "invalid block expression element")
            (fun cmds -> cmds)
            (fun da cmds -> failwith "invalid block expression element")
            dac_pair))
        [] init
    in
      ret_with_proto (unwrap_pair
        (fun da -> (Some da, Some (block_init_cmds@(pre_cmds back)))) 
        (fun cmds -> (None, Some (block_init_cmds@(pre_cmds back)@cmds)))
        (fun da cmds -> (Some da, Some(block_init_cmds@(pre_cmds back)@cmds)))
        back)

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
            let branches = [U.mk_block (U.unit_t, mk_meta()) (a@[assign_da id (unwrap y)]);
                            U.mk_block (U.unit_t, mk_meta()) (b@[assign_da id (unwrap z)])]
            in ret_with_proto (None, Some([cond_expr pred_expr branches]))
        end

      | [pred_pair; then_pair; else_pair] -> 
        let pred_expr, pre_cmds = unwrap_expr_and_cmds pred_pair in
        let branches = List.map (fun (ec_pair, reify_cmds) ->
            U.mk_block (U.unit_t, mk_meta()) (unwrap_pair
              (fun da -> reify_cmds@[assign_da id da])
              (fun cmds -> reify_cmds@cmds)
              (fun da cmds -> reify_cmds@cmds@[assign_da id da])
              ec_pair))
          [then_pair, then_reify_cmds; else_pair, else_reify_cmds]
        in
        let ret_cmds = pre_cmds@[cond_expr pred_expr branches]
        in ret_with_proto (None, Some(ret_cmds))

      | _ -> failwith "invalid reified ifelse children"
    end

  | Iterate ->
    begin match children with
      | [fn_pair; c_pair] -> 
        let fn_e, c_e = decompose_iterate e
        in ret_with_proto (mk_appfn_loop_cmds false fn_e c_e fn_pair c_pair)
      | _ -> failwith "invalid reified iterate children"
    end

  | Map ->
    begin match children with
      | [fn_pair; c_pair] ->
        let fn_e, c_e = decompose_map e
        in ret_with_proto (mk_appfn_loop_cmds true fn_e c_e fn_pair c_pair)
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
          let map_branches = [U.mk_block (U.unit_t, mk_meta()) map_body;
                              expr_cmd (U.mk_const (U.unit_t, mk_meta()) CUnit)] in
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
        in ret_with_proto (None, Some(filter_body)) 
      
      | _ -> failwith "invalid reified filter-map children"
    end

  | Flatten ->
    (* Loop over nested collection and concat, essentially ext(id)/flatmap *)
    let c_e = List.hd (sub_tree e) in
    let c_pair = List.hd children in
    let flatten_body_cmds loop_id loop_t =
      let loop_var = U.mk_var ((U.iv_type loop_t), mk_meta()) loop_id
      in [expr_cmd (coll_expr (U.i_type t) Combine [result_var; loop_var])]
    in
    let flatten_body = collection_loop (type_of_expr c_e) c_pair reify_cmds flatten_body_cmds
    in ret_with_proto (None, Some(flatten_body))
  
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
        in ret_with_proto (None, Some(cmds))
      
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
              | TTuple([gb_t; a_t]) as t -> (U.iv_type (canonical t)), (U.iv_type gb_t), (U.iv_type a_t) 
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
          let contains_expr = coll_expr U.bool_t Contains [result_var; key_var] in
          
          let assign_gbval_cmd =
            let branches = [assign_expr gb_val_id find_expr;
                            assign_expr gb_val_id init_expr]
            in cond_expr contains_expr branches 
          in
          
          let update_cmd e = expr_cmd (coll_expr U.unit_t Update [result_var; old_val; new_val e]) in
          let new_cmd e = expr_cmd (coll_expr U.unit_t Insert [result_var; new_val e]) in

          (* Declarations *)
          let decls = List.map (fun (id,t) -> 
              U.mk_decl (U.unit_t, mk_meta()) (U.mk_var_decl id t None)
            ) [gb_key_id, gb_key_t; gb_val_id, gb_val_t]
          in

          (* Bindings *)
          let gb_bindings, gb_bound_vars = mk_elem_bindings gb_fn_e loop_id loop_t in
          let bindings, agg_bound_vars =
            mk_agg_elem_bindings agg_fn_e gb_val_id gb_val_t loop_id (U.iv_type loop_t)
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
        in ret_with_proto (None, Some(cmds))

      | _ -> failwith "invalid reified group-by children"
    end

  | K3.AST.Sort -> ret_cmd_expr (coll_expr (U.i_type (type_of_expr e)) Sort)

  | K3.AST.Peek -> ret_expr e (Fn (Collection Peek))
  | K3.AST.Slice -> ret_expr e (Fn (Collection Slice))
  | K3.AST.Insert -> ret_cmd_expr (coll_expr U.unit_t Insert)
  | K3.AST.Delete -> ret_cmd_expr (coll_expr U.unit_t Delete)
  | K3.AST.Update -> ret_cmd_expr (coll_expr U.unit_t Update)

  | K3.AST.Send ->
    let send_target_e, send_addr_e, send_args_e = decompose_send e in
    let target_id, address =
      match tag_of_expr send_target_e, tag_of_expr send_addr_e with
      | K3.AST.Const(CTarget id), K3.AST.Const(CAddress addr) -> id, addr
      | _ -> failwith "invalid send target and address"
    in
    let target_type = type_of_expr send_target_e in
    let payload_type = 
      let send_arg_fail() = failwith "invalid value type in send argument" in
      let arg_types = List.map (fun e -> value_of (type_of_expr e) send_arg_fail) send_args_e
      in tuple_type_of_args arg_types
    in
    let type_tag = signature_of_type payload_type in
    let proto_spec =
      let x,y = children_protospecs
      in (x@[type_tag, target_id, target_type, payload_type, address]), y
    in
    ret_cmd_expr ~proto_spec:proto_spec (U.mk_fn (U.unit_t, mk_meta()) (Send type_tag))

  (* TODO: generates a side-effecting assignment command *) 
  | K3.AST.Assign -> failwith "imperative assign not yet implemented"

  (* TODO: primitive target language operation *)
  | Deref -> failwith "imperative deref not yet implemented"
  

let imperative_of_expr mk_meta fn_arg_env 
                       reify_cmds_and_ids ((id,t,decl,assign) as reified_sym) e
=
  let declare assign id t node_pair =
    let d_f da_opt = [U.mk_decl (U.unit_t, mk_meta()) (U.mk_var_decl id (U.i_type t) da_opt)] in
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
  let node_pair_with_proto =
    fold_tree
      (fun _ e2 -> (if e = e2 then reify_cmds_and_ids else []), reified_sym)
      (imperative_of_expr_node mk_meta fn_arg_env)
      (reify_cmds_and_ids, reified_sym) ((None,None), ([],[])) e
  in
  let node_pair = fst node_pair_with_proto in
  let result_pair =
    match decl, assign with
    | true, _ -> declare assign id t node_pair
    | false, true -> assign_if_expr id t node_pair
    | false, false -> ensure_cmd node_pair
  in result_pair, snd node_pair_with_proto


let imperative_of_reified_node mk_meta fn_arg_env _ sub_cmds_and_ids rt =
  let reified_sym, e = node_data rt in
  let (id,_,_,_) = reified_sym in
  let cmds, proto = imperative_of_expr mk_meta fn_arg_env sub_cmds_and_ids reified_sym e
  in (cmds, id), proto


let imperative_of_reified_expr mk_meta fn_arg_env rt =
  let cmds_and_ids, proto =
    fold_tree (fun _ _ -> None)
        (imperative_of_reified_node mk_meta fn_arg_env) None (([],""),([],[])) rt
  in (fst cmds_and_ids), proto


(* Flow program IAST generation
 *
 * Roles generate two forms of classes as part of their implementation:
 * -- a specialized class for each instruction in the role. This includes a
 *    custom step function to implement the FSM, and an initialization function
 *    that opens and closes resources. The initialization also instantiates any
 *    static metadata needed for the FSM, in our case these are resource id sets
 *    indicating the next set of valid resources to access following an 
 *    FSM transition.
 *
 * -- a specialized class for the role itself, that instantiates instruction
 *    implementations, and also sets up resource handles (the actual resource
 *    initialization, i.e. opening and closing, is done by the instruction itself) 
 *)

(* Generates a parser class with a single "parse" method for the given
 * desired output type.
 * This is used when generating a class representing each role to directly
 * create a parser for each unique event type encountered in the role.
 *)
let imperative_of_csv_parser mk_meta output_t =
  let meta t = U.ib_type t, mk_meta() in
  let unit_meta, int_meta = meta TUnit, meta TInt in
  let class_id = "parse_"^(signature_of_type output_t) in
  let input_id, input_var =
    let x = "input" in x, U.mk_var (meta TString) x
  in 
  let rec csv_constructor allow_tuples i var t =
    (* TODO: this should be a general n'th element method *)
    let index_fn = Member (Method "at") in 
    let error = "invalid value type in imperative_of_csv_parser" in
    let bt = base_of (value_of t (fun _ -> failwith error)) in
    let field_expr t = 
      i+1, 
      U.mk_fn (meta t) (Cast (U.ib_type t))
        [U.mk_fn (meta TString) index_fn [var; U.mk_const int_meta (CInt i)]]
    in
    match bt with
    | TTuple fields when allow_tuples ->
      let x,y = List.fold_left (fun (i,acc) t ->
          let j,e = csv_constructor false i var (TValue t) in j, acc@[e]
        ) (i,[]) fields
      in x, U.mk_tuple (meta bt) y

    | TBool   -> field_expr TBool
    | TByte   -> field_expr TByte
    | TInt    -> field_expr TInt
    | TFloat  -> field_expr TFloat
    | TString -> field_expr TString
    | _ -> failwith "unsupported type from csv input"
  in
  let tokenized_t, tok_meta = 
    let t = TCollection(TList, canonical TString) in U.ib_type t, meta t
  in
  let tokenized_id, tokenized_var =
    let id = "tokenized" in id, (U.mk_var tok_meta id)
  in
  let total, cstr_expr = csv_constructor true 0 tokenized_var output_t in
  let tokenized_var_decl =
    (* TODO: this should come from the channel_format_t parameter *)
    let delimiter = U.mk_const (meta TString) (CString ",") in
    let tokenize_fn =
      U.mk_fn tok_meta (Named "tokenize")
        [input_var; delimiter; U.mk_const int_meta (CInt total)]
    in U.mk_var_decl tokenized_id tokenized_t (Some(Init(tokenize_fn)))
  in
  let parse_method_decl =
    let body = [U.mk_decl unit_meta tokenized_var_decl;
                U.mk_return unit_meta cstr_expr]
    in DFn("parse", [input_id, U.ib_type TString], U.i_type output_t, body)
  in
    (TNamed(class_id)),
    DClass(class_id, Some(TNamed "parser"), [parse_method_decl, unit_meta])


(* Returns a parser declaration, the implementation type of the channel, and
 * a constructor for the implementation to be used to declare a variable
 * corresponding to the given resource. *)
let type_and_constructor_of_channel mk_meta id (t, ct, cf) =
  let meta t = t, mk_meta() in
  let ret rt cstr =
    let parser_t, parser_class_decl = imperative_of_csv_parser mk_meta t in
    let var_id = "parser_"^id in
    let parser_decl, parser_var =
      (U.mk_var_decl var_id parser_t None), (U.mk_var (meta parser_t) var_id) 
    in
      parser_class_decl, parser_decl,
         (TNamed(rt)), Some(Constructor(cstr@[parser_var])) 
  in 
  match ct,cf with
  | File(f), CSV -> 
    let meta = U.string_t, mk_meta() in
    ret "k3_file_source" [U.mk_const meta (CString f)]
  
  | Network(addr), CSV -> 
    let cstr_args = 
      let ip,port = addr in
      let ip_meta = U.string_t, mk_meta() in
      let port_meta = U.int_t, mk_meta() in
      [U.mk_const ip_meta (CString ip); U.mk_const port_meta (CInt port)]
    in
    ret "k3_network_source" cstr_args
  
  | _, _ -> failwith "unsupported stream channel"

let imperative_of_resource mk_meta (id, (source,resource)) =
  let parser_class_decl, parser_decl, t, da_opt = match resource with
    | Handle (t,ct,cf) -> type_and_constructor_of_channel mk_meta id (t,ct,cf) 
    | _ -> failwith "cannot generate code for a derived stream"
  in 
  let resource_decls =
    let d = DVar(id, t, da_opt) in
    let unit_meta = U.unit_t, mk_meta() in
    [U.mk_decl unit_meta parser_decl; U.mk_decl unit_meta d] 
  in parser_class_decl, resource_decls, t, (U.mk_var (t, mk_meta()) id)


(* Dispatcher step function branch generation. 
 * Given an expected resource id and a successful match state and mismatch state,
 * generates a conditional that tests whether the match succeeds and whether
 * the supplied data is valid, and then schedules the event.
 * This function is used to thread together a case statement in the main FSM
 * generation function as a sequences of if-then-elses
 *)
let imperative_of_dispatcher_state mk_meta role_id dispatch
                                   resource_id next_state_id fail_state_id
                                   ((event_var, event_meta), (source_var, source_meta)) =

  (* Type and meta constructor helpers *)
  let meta t = (t, mk_meta()) in
  let unit_meta, str_meta, fsm_meta, pred_meta =
    meta U.unit_t, meta U.string_t, meta U.int_t, meta U.bool_t
  in

  let return state_id = U.mk_return unit_meta (U.mk_const fsm_meta (CInt state_id)) in

  let schedule_exprs dispatch_ids =
    let expr trig_id = U.mk_expr unit_meta
      (U.mk_fn unit_meta (Named "schedule")
        [U.mk_const str_meta (CString trig_id); event_var])
    in List.map expr dispatch_ids 
  in
  
  let valid_pred = 
    let valid_event = U.mk_op pred_meta Neq [event_var; U.mk_const event_meta CNothing] in
    let resource_var = mk_resource_var mk_meta role_id resource_id in
    let match_source = U.mk_op pred_meta Eq [source_var; resource_var] in
    U.mk_op pred_meta And [match_source; valid_event]
  in
  let succeed_cmd = U.mk_block unit_meta ((schedule_exprs dispatch)@[return next_state_id])
  in [U.mk_ifelse pred_meta valid_pred [succeed_cmd; return fail_state_id]] 


(* Dispatcher step function declaration, to run an FSM.
 * The function accepts an argument of a state to run, and is implemented as a
 * sequenced conditional that testing whether to apply the actions of each state.
 * For actions, we check whether the operation succeeds and then return the
 * next state according to the FSM's transition table.
 *)  
let imperative_of_dispatcher mk_meta role_id resource_env prior_resources (id,dispatcher) =
 
  (* Type and meta constructor helpers *)
  let meta t = (t, mk_meta()) in
  let type_and_meta bt = let t = U.ib_type bt in t, meta t in

  let unit_meta, int_meta, pred_meta = meta U.unit_t, meta U.int_t, meta U.bool_t in
  let fsm_meta, event_meta, source_meta = int_meta, meta TTop, meta U.int_t in
  
  let int_vt, next_access_bt = 
    let x = canonical TInt in x, TCollection(TSet, x)
  in
  let next_access_t, next_access_meta = type_and_meta next_access_bt in
  let fsm_ret_t, fsm_ret_meta = type_and_meta TInt in
  
  let state_var_id, state_var = mk_state_var_pair fsm_meta id in
  let source_id, source_var = mk_source_var_pair source_meta id in
  let event_id, event_var = mk_event_var_pair event_meta id in 

  let error = U.mk_return unit_meta (U.mk_const fsm_meta (CInt (-1))) in
  
  let module A = ResourceActions in 
  let module F = ResourceFSM in
  let fsm_step_body =
    List.fold_left (fun else_branch (sid, ((rid, (ma,nid)), (fa,fid))) ->
      let fsm_pred = U.mk_op pred_meta Eq [state_var; U.mk_const fsm_meta (CInt sid)] in
      let action_cmd =
        let cmds = match ma with
          | F.Output (A.Source(A.Dispatch(dispatch),p)) ->
            
            (* Get resource-specific event type *)
            let event_t, event_meta =
              match handle_of_resource resource_env rid with
              | Some(source, event_t, ct, cf) -> 
                let t = U.i_type event_t in t, (t,mk_meta())
              | _ -> failwith ("invalid resource "^rid)
            in
            let typed_event_id, typed_event_var = mk_event_var_pair event_meta id in
            imperative_of_dispatcher_state mk_meta role_id dispatch rid nid fid
              ((typed_event_var, event_meta), (source_var, source_meta))
            
            (* TODO: dispatch for output patterns *)
          | F.Output (A.Sink(A.Dispatch(dispatch))) ->
            failwith "output channel dispatching not yet supported"
    
          | F.Terminate -> [error]
          | _ -> failwith "invalid match action"

        in U.mk_block unit_meta cmds
      in
      U.mk_ifelse unit_meta fsm_pred [action_cmd; else_branch]
    ) error dispatcher
  in
  let fsm_step_decl =
    let fsm_step_arg =
      [state_var_id, fst fsm_meta; source_id, fst source_meta; event_id, fst event_meta]
    in DFn("run_step", fsm_step_arg, fsm_ret_t, [fsm_step_body])
  in
  
  let renv_id, renv_var =
    let id, t = "resources", TNamed("resource_env") in id, U.mk_var (meta t) id
  in
  let init_body =
    let initialize_fsm = List.fold_left (fun body_acc (sid, ((_, (ma,_)), (fa,_))) ->
      match ma with 
      | F.Output (A.Source(A.Dispatch(dispatch),p)) ->
        let tmp_id, tmp_var =
          (* Temporary set to hold next access ids *)
          let x = "tmp_ids" in x, U.mk_var next_access_meta x
        in 
        let init_next_access_var_cmds =
          let insert_resource_id id =
            let resource_var = mk_resource_var mk_meta role_id id in 
            U.mk_expr unit_meta (
              U.mk_fn unit_meta (Collection Insert) [tmp_var; resource_var])
          in
          (* Declare temporary set, and add all next access ids to the set *)
          [U.mk_decl unit_meta (U.mk_var_decl tmp_id next_access_t None)]@
          (List.map insert_resource_id p)
        in 
        let add_next_access_cmds =
          (* Register next access ids for the given state *)
          [U.mk_expr unit_meta
            (U.mk_fn unit_meta (Named "add_next_access")
              [U.mk_const fsm_meta (CInt sid); tmp_var])]
        in
        let register_next_access_block =
          [U.mk_block unit_meta (init_next_access_var_cmds@add_next_access_cmds)]
        in
        body_acc@register_next_access_block
      | _ -> body_acc 
      ) [] dispatcher
    in
    (* Initialize resource delta. This leaves network sockets open from prior
     * instructions, but reloads files. *)
    let initialize_resources =
      let mk_renv_method method_id args = U.mk_expr unit_meta (
        U.mk_fn unit_meta (Member (Method method_id)) ([renv_var]@args)) 
      in
      let open_cmd id = mk_renv_method "open_resource" [mk_resource_var mk_meta role_id id] in
      let close_cmd id = mk_renv_method "close_resource" [mk_resource_var mk_meta role_id id] in
      let partition_net l = List.partition (is_net_handle resource_env) l in
      let opened_net, opened_files = partition_net prior_resources in
      let net_resources, file_resources = partition_net (resources_of_dispatcher dispatcher) in
      let pass_net, open_net, close_net =
        ListAsSet.inter opened_net net_resources,
        ListAsSet.diff net_resources opened_net,
        ListAsSet.diff opened_net net_resources 
      in
      let open_files, close_files = file_resources, opened_files in
      let open_cmds = List.map open_cmd (open_net@open_files) in
      let close_cmds = List.map close_cmd (close_net@close_files) in
      open_cmds@close_cmds
    in
    initialize_fsm@initialize_resources
  in
  let init_decl = DFn("init", [], U.unit_t, init_body) in

  let dispatcher_decls = [init_decl, unit_meta; fsm_step_decl, unit_meta]
  in
  [DClass("instruction_"^id, Some(TNamed "flow_instruction"), dispatcher_decls), unit_meta]
  


(* Generates code to register an instruction with a role class *)
let imperative_of_instruction mk_meta ds_env instr =
  let meta t = (t, mk_meta()) in
  match instr with
  | Consume id ->
    let unit_meta, int_meta, bool_meta = meta U.unit_t, meta U.int_t, meta U.bool_t in
    if List.mem_assoc id ds_env then
      let instr_id = "instr_"^id in 
      let decl, decl_meta = 
        let t = TNamed ("instruction_"^id) in
        U.mk_decl unit_meta (U.mk_var_decl instr_id t None), meta t
      in
      let add = U.mk_expr unit_meta (
        U.mk_fn unit_meta (Named "add_instruction") [U.mk_var decl_meta instr_id])
      in U.mk_block unit_meta [decl; add]
    else failwith ("could not find instruction named "^id)


(* Generates a role implementation class, which includes handles for resources
 * used in the role, as well as instruction objects for each specialized
 * instruction used by the role. *)
let imperative_of_event_loop mk_meta role_id (res_env, ds_env, instrs) =
  let meta t = t, mk_meta() in
  let unit_meta, int_meta = meta U.unit_t, meta U.int_t in

  let mk_decl_pair l = List.map (fun d -> (d, unit_meta)) l in
  
  let resource_id_decls =
    mk_decl_pair (List.map (fun (id, _) ->
      U.mk_var_decl (mk_resource_id id) U.int_t
       (Some(Init(U.mk_const int_meta (CInt (Hashtbl.hash id)))))
    ) res_env)
  in

  let source_ids_class_id = mk_source_ids_class_id role_id in
  let source_ids_var_id = mk_source_ids_var_id role_id in
  let source_ids_decls =
    if resource_id_decls = [] then []
    else 
      [DClass(source_ids_class_id, None, resource_id_decls), unit_meta;
       DVar(source_ids_var_id, TNamed source_ids_class_id, None), unit_meta]
  in
  let source_ids_var = U.mk_var (meta (TNamed source_ids_class_id)) source_ids_var_id in

  let fsm_decls = 
    let dispatcher_gen = imperative_of_dispatcher mk_meta role_id res_env in
    snd (List.fold_left (fun (prior_resources, decls) (id,d) ->
      let next_resources = resources_of_dispatcher d in
      next_resources, (decls@(dispatcher_gen prior_resources (id,d)))
      ) ([],[]) ds_env)
  in

  let register_resource_cmds, parser_decls = 
    let regs, parsers =
      List.split (List.map (fun ((id, (source, resource)) as r) ->
        let id_decl, id_var =
          let rid = mk_resource_id id in
          (U.mk_var_decl rid U.int_t
            (Some(Init(U.mk_const int_meta (CInt (Hashtbl.hash id)))))),
          (U.mk_fn int_meta (Member (Field rid)) [source_ids_var])
        in
        let parser_decl, resource_decl, resource_t, resource_var =
          imperative_of_resource mk_meta r
        in
        let mk_register_fn fn = 
          U.mk_fn unit_meta fn
            [U.mk_var (meta (TNamed "resource_env")) "resources"; id_var; resource_var]
        in
        let register_fn = match resource_t with 
          | TNamed("k3_file_source") -> mk_register_fn (Member (Method "add_file"))
          | TNamed("k3_network_source") -> mk_register_fn (Member (Method "add_network"))
          | _ -> failwith "invalid resource type"
        in
        let register_cmd =
          U.mk_block unit_meta (resource_decl@[U.mk_expr unit_meta register_fn])
        in
          register_cmd, parser_decl
      ) res_env)
    in
    regs, mk_decl_pair (ListAsSet.no_duplicates parsers) 
  in

  let flow_class_decl =
    let flow_class_name = mk_role_class_id role_id in
    let init_body =
      let register_loop_cmds = List.map (imperative_of_instruction mk_meta ds_env) instrs
      in register_resource_cmds@register_loop_cmds
    in
    if init_body = [] then [] 
    else
      let init_decl = DFn("init", [], U.unit_t, init_body), unit_meta in 
      let class_decls = parser_decls@[init_decl]
      in [DClass(flow_class_name, Some(TNamed "flow_role"), class_decls), unit_meta]
  in
  source_ids_decls@fsm_decls@flow_class_decl


let imperative_of_flow_statement mk_meta fn_arg_env (fs, a) =
  let cmds_of_reified_expr e = 
    imperative_of_reified_expr mk_meta fn_arg_env (reify_expr fn_arg_env e)
  in
  match fs with
    (* These are handled by event loop generation *)
  | Source (Resource _) -> [], [], [], ([],[])
  | Sink   (Resource _) -> [], [], [], ([],[])
  | Bind  _             -> [], [], [], ([],[])
  | Instruction i       -> [], [], [], ([],[])

  | Source (Code (id, arg, decls, body)) ->
    failwith "generator declaration not implemented"
  
  | Sink   (Code (id, arg, decls, body)) ->
    let local_decl_cmds = List.map (fun (id, t, ann) ->
        U.mk_decl (U.unit_t,ann) (DVar (id, U.iv_type t, None)) 
      ) decls
    in
    let cmds, protospec = cmds_of_reified_expr body in
    let trig_body = [U.mk_block (U.unit_t, mk_meta()) (local_decl_cmds@cmds)] in
    let trig_args =
      List.map (fun (id, vt) -> id, U.iv_type vt) (typed_vars_of_arg arg)
    in
    let arg_sig = signature_of_type (tuple_type_of_arg arg) in
    let new_spec = let x,y = protospec in x, y@[id, arg, arg_sig]
    in [DFn(id, trig_args, U.unit_t, trig_body), (U.unit_t,a)], [], [id,arg], new_spec 


let imperative_of_flow_program mk_meta fn_arg_env fp =
  let fs_fn = imperative_of_flow_statement mk_meta fn_arg_env in
  let event_loop = imperative_of_event_loop mk_meta "k3" (event_loop_of_flow fp) in
  List.fold_left (fun (decl_acc, init_acc, fn_acc, (psacc,ptacc)) (fs,a) -> 
      let decls, init, fn_arg_env, (ps,pt) = fs_fn (fs,a)
      in decl_acc@decls, init_acc@init, fn_acc@fn_arg_env, (psacc@ps, ptacc@pt)
    ) (event_loop, [], [], ([],[])) fp


(* K3 program IAST generation *)

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
        let iret_t = U.iv_type ret_t in
        begin match init with
          | Some(e) ->
            let arg = match arg_of_lambda e with
              | Some(a) -> a 
              | None -> failwith "invalid global function declaration" 
            in
            let cmds, protospec = cmds_of_reified_expr e in
            let fn_arg = List.map (fun (id,vt) -> id, U.iv_type vt) (typed_vars_of_arg arg) in
            let fn_body = [U.mk_block (iret_t, mk_meta()) cmds]
            in [DFn(id, fn_arg, iret_t, fn_body), (U.unit_t,m)], [], [id,arg], protospec
          | _ -> failwith "invalid function body"
        end
      else
        let cmds, protospec = match init with
          | Some(e) -> cmds_of_reified_expr e
          | _ -> [], ([],[]) 
        in [DVar(id, U.i_type t, None), (U.unit_t,m)], cmds, [], protospec
        
  | Foreign (id,t) ->
    (* TODO: special handling for fixed foreign functions *) 
    begin match id with
      | "sin" -> [], [], [], ([],[])
      | _ -> failwith ("unsupported foreign function: "^id)
    end

  | Flow fp -> imperative_of_flow_program mk_meta fn_arg_env fp

  (* Roles are handled separately and compiled to functions *)
  | Role (id, sp)   -> [], [], [], ([],[])
  | DefaultRole(id) -> [], [], [], ([],[])


(* Imperative AST generation for all roles in an imperative program.
 * This function returns the necessary declarations for a role, and a role
 * dispatch body for the query engine's entry point. *)
let imperative_of_roles mk_meta prog =
  let meta t = t, mk_meta() in
  let unit_meta, int_meta, bool_meta, str_meta =
    meta U.unit_t, meta U.int_t, meta U.bool_t, meta U.string_t
  in
  let roles, default_role = roles_of_program prog in
  let role_decls = List.fold_left (fun acc (id,evt_loop) ->
      let role_recvr_decl =
        let renv, _, _ = evt_loop in
        let d =
          MGen.generate_receiver
            mk_meta ("role_"^id^"_receiver") "k3_receiver" renv ([],[])
        in match d with None -> [] | Some(x) -> [x, unit_meta]
      in
      acc@role_recvr_decl@(imperative_of_event_loop mk_meta id evt_loop)
    ) [] roles
  in
  let dispatch_body =
    let mk_run_role id =
      let role_var, declare_role =
        let id, var, role_t =
          let x, t = "r"^id, TNamed(mk_role_class_id id)
          in x, (U.mk_var (t, mk_meta()) x), t
        in var, U.mk_decl unit_meta (U.mk_var_decl id role_t None) 
      in
      let mk_role_method_call method_id =
        U.mk_expr unit_meta (U.mk_fn unit_meta (Member (Method method_id)) [role_var])
      in 
      U.mk_block unit_meta 
        [declare_role; mk_role_method_call "init"; mk_role_method_call "run"]
    in
    let role_var =
      U.mk_fn str_meta (Member (Method "get_role")) [mk_options_var mk_meta]
    in 
    let valid_default, default =
      match default_role with 
        | None -> false, U.mk_return unit_meta (U.mk_const int_meta (CInt 1))
        | Some(id,_) ->  true, mk_run_role id
    in
    let match_role_cmds =
      List.fold_left (fun case_acc (id,_) ->
        let pred_e = U.mk_op bool_meta Eq [role_var; U.mk_const str_meta (CString id)]
        in U.mk_ifelse unit_meta pred_e [mk_run_role id; case_acc]
        ) default roles
    in 
    [match_role_cmds]@
    (if not valid_default then [] 
     else [U.mk_return unit_meta (U.mk_const int_meta (CInt 0))])
  in
    role_decls, dispatch_body


(* Runtime, and protocol generation *)
let imperative_of_protospec mk_meta spec =
  let unwrap_as_list opt = match opt with None -> [] | Some(x) -> [x] in

  (* Target table and runtime generation *)
  let target_class_decl = generate_targets mk_meta spec in
  let runtime_class_decl = generate_runtime mk_meta spec in

  (* Serializer generation *)
  let serializer_class_decl = SGen.generate_serializer mk_meta "k3_serializer" in

  (* Protocol generation *)
  let sender_decl = MGen.generate_sender mk_meta "k3_sender" "sender" spec in
  let recvr_decl = MGen.generate_receiver mk_meta "k3_receiver" "receiver" [] spec in
  let proto_decls = 
    List.flatten
      (List.map unwrap_as_list [serializer_class_decl; sender_decl; recvr_decl])
  in
  target_class_decl, runtime_class_decl,
    (proto_decls, serializer_class_decl, sender_decl, recvr_decl) 


(* Top-level code generation *)
let imperative_of_program mk_meta p =
  let meta t = t, mk_meta() in
  let unit_meta, int_meta, string_meta = meta U.unit_t, meta U.int_t, meta U.string_t in
  let main_body, decls, _, protospec =
    List.fold_left (fun (main_cmd_acc, decl_acc, fn_arg_env, (psacc,ptacc)) (d,m) ->
      let decls, init_cmds, fn_args, (ps,pt) =
        imperative_of_declaration mk_meta fn_arg_env (d,m)
      in main_cmd_acc@init_cmds, decl_acc@decls, fn_arg_env@fn_args, (psacc@ps, ptacc@pt)
    ) ([],[],[],([],[])) p
  in

  (* TODO: remove once we have a basic library implementation.
   * This is a hack to allow valid typing. *)
  let k3_decls =
    let flow_class_members =
      [DVar("resources", TNamed("resource_env"), None), unit_meta]
    in
    [DClass("trigger_dispatch", None, []), unit_meta;
     DClass("recv_dispatch", None, []), unit_meta;
     DClass("parser", None, []), unit_meta;
     DClass("sender", None, []), unit_meta;
     DClass("receiver", None, []), unit_meta;
     DClass("symbol_table", None, []), unit_meta;
     DClass("runtime", None, []), unit_meta;
     DClass("resource_env", None, []), unit_meta;
     DClass("flow_instruction", None, flow_class_members), unit_meta;
     DClass("flow_role", None, flow_class_members), unit_meta;
     DClass("trigger_msg", None, []), unit_meta;
     DClass(options_class_id, None, []), unit_meta;
     DVar("user_role", U.ib_type TString, None), unit_meta;
    ]
  in
  
  let target_class_decl, runtime_class_decl,
        (proto_decls, serializer_decl, sender_decl, recvr_decl)
     = imperative_of_protospec mk_meta protospec
  in

  let mk_component d = Component (List.map (fun d -> d, unit_meta) d) in
  let type_of_class_decl d = match d with
    | DClass(id,_,_) -> TNamed id
    | _ -> failwith "invalid class declaration"
  in
  
  let decls_types_and_vars =
    let skip_decl d id init = match d with 
      | None -> []
      | Some(x) -> [x, id, init]
    in
    let runtime_meta = meta (type_of_class_decl runtime_class_decl) in
    List.map (fun (d, id, args) ->
      let t = type_of_class_decl d
      in t, (U.mk_var_decl id t args, U.mk_var (meta t) id))
      ([target_class_decl, target_var_id, None;
       runtime_class_decl, runtime_var_id, None]
       @(skip_decl sender_decl "sender" None)
       @(skip_decl recvr_decl "receiver"
          (Some(Constructor[U.mk_var runtime_meta runtime_var_id])))
       @(skip_decl serializer_decl "serializer" None))
  in
  
  let dt i = fst (List.nth decls_types_and_vars i) in
  let dd i = fst (snd (List.nth decls_types_and_vars i)) in
  let dv i = snd (snd (List.nth decls_types_and_vars i)) in  

  let ensure_decl_and_type f i = if List.length decls_types_and_vars > i then [f i] else [] in
  let mk_runtime () = ensure_decl_and_type (fun i -> mk_component [dd i]) 1  in
  
  let mk_communicators () = 
    mk_component (List.concat (List.map (ensure_decl_and_type dd) [2; 3]))
  in

  let runtime_components =
    [Include (target_include, None, None, false); mk_component [runtime_class_decl]]
  in 
  let protocol_components =
    [Include (runtime_include, None, None, false)]@
    [Include(MGen.generate_serializer_include protospec, None, None, false);
     mk_component proto_decls]
  in
  let role_decls, dispatch_body = imperative_of_roles mk_meta p in
  let main_arg = ["argc", U.int_t; "argv", U.string_t] in
  let main_fn =
    let body =
      let opt_cstr = [dv 0; U.mk_var int_meta "argc"; U.mk_var string_meta "argv"] in
      let init_opts =
        let options_t = TNamed options_class_id in
        [U.mk_decl unit_meta
          (U.mk_var_decl (mk_options_id()) options_t (Some(Constructor opt_cstr)))]
      in init_opts@main_body@dispatch_body
    in 
    DFn("main", main_arg, U.int_t, body) 
  in 
      [Include(protocol_spec_file, None, Some(MGen.generate_serializer_specs protospec), false);
       Include (k3_include, Some([Component k3_decls]), None, true);
       Include(target_include, Some([mk_component [target_class_decl]]), None, false);
       Component ([dd 0, unit_meta]@decls);
       Include(runtime_include, Some(runtime_components), None, false)]
      @(mk_runtime ())@
      [Include(protocol_include, Some(protocol_components), None, false);
       mk_communicators ();
       Include("k3_roles.h", Some([Component role_decls]), None, false);
       Component [main_fn, unit_meta]]

end