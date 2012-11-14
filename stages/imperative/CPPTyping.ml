open Tree
open K3.AST
open K3.Annotation
open K3Util
open K3Typechecker

open CPP
open CPP.CPPTarget
open CPP.CPPTarget.ASTImport

module U = ImperativeUtil.Util(CPPTarget)

let iv_type t = TInternal (TValue t)
let unit_t = TInternal(TValue(canonical TUnit))

let filter_first_assoc k l = snd (List.fold_left (fun (found,acc) e ->
    if (not found) && k = (fst e) then (true, acc) else (found, acc@[e])
  ) (false,[]) l)

(* TODO *)
let typecheck_external_expr type_env tdecl_env children e =
  U.mk_iexpr (U.tag_of_expr e) (U.meta_of_expr e) children

(* TODO *)
let typecheck_external_cmd type_env tdecl_env meta children ext_cmd = 
  let nc = U.mk_cmd (CExt ext_cmd) meta children
  in (type_env, tdecl_env), nc

(* Returns a type environment and type declaration environment, extended
 * appropriately given the declaration *)
let rec bindings_of_decl ?(allow_function=true) type_env tdecl_env d = 
  let x, y = match d with
  | DType (id, tdecl) -> type_env, ((id, tdecl)::tdecl_env)

  | DVar (id, t, da_opt) -> ((id,t)::type_env), tdecl_env

  | DFn (id, arg_t, ret_t, body) when allow_function ->
    (id, TImpFunction(List.map snd arg_t, ret_t))::type_env, tdecl_env
   
  | DClass (id, parent_opt, members) ->
    (* Only the nested type env, and not the nested type decl env is exposed.
     * Thus nested class declarations are not visible. *) 
    let class_type_env = 
      let nested_type_env, _ = List.fold_left
        (fun (tenv, tdenv) (d,_) -> bindings_of_decl ~allow_function tenv tdenv d)
        ([],[]) members 
      in nested_type_env
    in type_env, (id, TComposite(class_type_env))::tdecl_env

  | _ -> failwith "invalid declaration for binding extraction"
  in ListAsSet.no_duplicates x, ListAsSet.no_duplicates y


(* Type deduction via alteration rather than full-retyping for CPP programs *)
let deduce_program_type program =

  let fail_if_undefined_type tdecl_env t = match t with
    | TNamed type_id -> 
      if not (List.mem_assoc type_id tdecl_env)
      then failwith ("undefined type "^type_id) else ()
    | _ -> ()
  in

  (* Returns a CPP expression *)
  let deduce_cpp_expr type_env tdecl_env e =
    let alter_expr_type _ ch_l e = 
      let ch = List.flatten ch_l in 
      match U.tag_of_expr e with
      | Var id -> 
        if List.mem_assoc id type_env then
          let var_meta = (List.assoc id type_env), snd(U.meta_of_expr e)
          in [U.mk_var var_meta id]
        else failwith ("undefined variable "^id)
      
      | _ ->
        let is_external t = match t with TInternal _ -> false | _ -> true in
          let child_types = List.map (fun c -> fst (U.meta_of_expr c)) ch in
          let externals = List.filter is_external child_types in
          if externals <> [] then [typecheck_external_expr type_env tdecl_env ch e]
          else [U.mk_iexpr (U.tag_of_expr e) (U.meta_of_expr e) ch]
    in
    List.hd (fold_tree (fun _ _ -> None) alter_expr_type None [] e)
  in
  
  (* Returns a pair of environments, and a CPP command *)
  let deduce_cpp_cmd type_env tdecl_env cmd =
    let fail_if_non_unit() = 
      if fst (U.meta_of_cmd cmd) = unit_t then ()
      else failwith "invalid cmd type, expected unit"
    in
    let bindings_of_cmd (preserved_envs, type_env, tdecl_env) cmd =
      let rtype_env, rtdecl_env = match U.tag_of_cmd cmd with
	      | Decl d ->
	        (* Disallow nested functions in commands *) 
	        bindings_of_decl ~allow_function:false type_env tdecl_env d
	      
	      | Foreach (id,t,e) -> 
	        let nt = try List.assoc id type_env with Not_found -> t
	        in ((id,nt)::type_env, tdecl_env)
	 
	      | _ -> (type_env, tdecl_env)
      in ((cmd, (type_env, tdecl_env))::preserved_envs), rtype_env, rtdecl_env
    in
    let alter_cmd_type ((preserved_envs, type_env, tdecl_env) as env, ch_l) cmd =
      fail_if_non_unit();
      let ch = List.flatten ch_l in
      match U.tag_of_cmd cmd with
      | Assign (id,e) -> 
        let nc = U.mk_assign (U.meta_of_cmd cmd) id (deduce_cpp_expr type_env tdecl_env e)
        in env, [nc]

      | Decl d -> env, [cmd] 

      | Expr e -> 
        let nc = U.mk_expr (U.meta_of_cmd cmd) (deduce_cpp_expr type_env tdecl_env e) 
        in env, [nc]

      | IfThenElse pred ->
        let npred = deduce_cpp_expr type_env tdecl_env pred 
        in env, [(U.mk_ifelse (U.meta_of_cmd cmd) npred ch)]

      | Block -> 
        let x,y = List.assoc cmd preserved_envs
        in (preserved_envs,x,y), [U.mk_block (U.meta_of_cmd cmd) ch]
      
      | Foreach (id,t,e) -> 
        let ne = deduce_cpp_expr type_env tdecl_env e in
        let nt = try List.assoc id type_env with Not_found -> t in
        let nenv =
          let x,y = List.assoc cmd preserved_envs in preserved_envs, x, y
        in nenv, [(U.mk_for (U.meta_of_cmd cmd) id nt ne ch)] 
      
      | While e ->
        let nc = U.mk_while (U.meta_of_cmd cmd) (deduce_cpp_expr type_env tdecl_env e) ch
        in env, [nc]

      | Return e ->
        let nc = U.mk_return (U.meta_of_cmd cmd) (deduce_cpp_expr type_env tdecl_env e)
        in env, [nc]
      
      | CExt ec ->
        let (x,y),z = typecheck_external_cmd type_env tdecl_env (U.meta_of_cmd cmd) ch ec
        in (preserved_envs, x, y), [z] 
    in
    let (_,x,y),z =
      fold_tree_thread bindings_of_cmd alter_cmd_type ([], type_env, tdecl_env) [] cmd
    in (x,y), List.hd z
  in

  (* Returns a pair of environments and a CPP command list *)
  let rec deduce_cpp_decl (type_env, tdecl_env, nprog) (d,(t,a)) =
    let rtype_env, rtdecl_env = bindings_of_decl type_env tdecl_env d in 
    match d with
	  | DVar (id, dt, da_opt) ->
      begin
      fail_if_undefined_type tdecl_env dt;
      let nda_opt = match da_opt with
        | Some(Constructor(exprs)) -> 
          let nexprs = List.map (deduce_cpp_expr type_env tdecl_env) exprs 
          in Some(Constructor(nexprs))
        | Some(Init(e)) -> Some(Init(deduce_cpp_expr type_env tdecl_env e))
        | None -> None
      in rtype_env, rtdecl_env, (nprog@[DVar(id, dt, nda_opt), (t,a)])
      end

	  | DFn (id, arg, ret_t, body) ->
      (* Type environments used inside the function body are extended with function
       * arguments, and threaded for local declarations. These are do not escape
       * the function definition. *)
      begin
      List.iter (fail_if_undefined_type tdecl_env) (List.map snd arg);
      fail_if_undefined_type tdecl_env ret_t; 
      let ntype_env = type_env@arg in
      let _, rbody = 
        List.fold_left (fun ((te,tde),acc) c ->
          let nenvs, nc = deduce_cpp_cmd te tde c in nenvs, (acc@[nc])
        ) ((ntype_env, tdecl_env), []) body 
      in
      rtype_env, rtdecl_env, (nprog@[DFn(id, arg, ret_t, rbody), (t,a)])
      end

    | DClass (id, parent_opt, members) ->
      (* Add bindings based on parent declarations *)
      let ptype_env, ptdecl_env = match parent_opt with
        | None -> type_env, tdecl_env
        | Some(pid) ->
          fail_if_undefined_type tdecl_env (TNamed pid);
          let ptype = List.assoc pid tdecl_env in
          begin match ptype with 
            | TComposite fields -> (type_env@fields), tdecl_env
            | _ -> type_env, tdecl_env
          end
      in
      let _,_,rmembers =
        List.fold_left deduce_cpp_decl (ptype_env,ptdecl_env,[]) members
      in rtype_env, rtdecl_env, (nprog@[DClass(id, parent_opt, rmembers), (t,a)])

    | _ -> rtype_env, rtdecl_env, (nprog@[(d,(t,a))])
  in 

  let rec deduce_cpp_program (type_env, tdecl_env) prog =
    let nd = ListAsSet.no_duplicates in
    List.fold_left 
      (fun (tacc, tdacc, comp_acc) c -> match c with
        | Include (name, None, _, _) -> (tacc, tdacc, comp_acc@[c])
        | Include (name, Some(p), code, expected) ->
          let x,y,z = deduce_cpp_program (tacc,tdacc) p in 
          let nc = Include (name, Some(z), code, expected)
          in (nd (tacc@x)), (nd (tdacc@y)), comp_acc@[nc]
        | Component decls ->
          let x,y,z = List.fold_left deduce_cpp_decl (tacc,tdacc,[]) decls
          in x,y,comp_acc@[Component(z)])
      (type_env,tdecl_env,[]) prog
  in
  
  let _, _, nprog = deduce_cpp_program ([],[]) program
  in nprog
