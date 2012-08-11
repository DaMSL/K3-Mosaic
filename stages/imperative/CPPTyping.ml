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
let bindings_of_decl ?(allow_function=true) type_env tdecl_env d = match d with
  | DType (id, tdecl) -> type_env, ((id, tdecl)::tdecl_env)

  | DVar (id, t, da_opt) -> ((id,t)::type_env), tdecl_env

  | DFn (id, arg, ret_t, body) when allow_function -> 
    begin match ret_t with
      | TInternal (TValue vt) -> 
        let t = TInternal(TFunction(deduce_arg_type arg, vt))
        in ((id,t)::type_env), tdecl_env
      | _ -> failwith "unsupported function type"
    end
   
  | _ -> failwith "invalid declaration for binding extraction"

(* Removes the first occurrence of any bindings in a type and type declaration
 * environment for the given declaration *)
let remove_bindings_of_decl type_env tdecl_env d =  match d with
  | DType (id, tdecl) -> type_env, (filter_first_assoc id tdecl_env)
  | DVar (id, t, da_opt) -> (filter_first_assoc id type_env), tdecl_env
  | DFn (id, arg, ret_t, body) -> (filter_first_assoc id type_env), tdecl_env

(* Type deduction via alteration rather than full-retyping for CPP programs *)
let deduce_program_type program =

  (* Returns a CPP expression *)
  let deduce_cpp_expr type_env tdecl_env e =
    let alter_expr_type _ ch e = match U.tag_of_expr e with
      | Var id -> 
        if List.mem_assoc id type_env then
          let var_meta = (List.assoc id type_env), snd(U.meta_of_expr e)
          in U.mk_var var_meta id
        else e
      | _ ->
        let is_external t = match t with TInternal _ -> false | _ -> true in
	      let child_types = List.map (fun c -> fst (U.meta_of_expr c)) ch in
	      let externals = List.filter is_external child_types in
	      if externals <> [] then typecheck_external_expr type_env tdecl_env ch e
	      else U.mk_iexpr (U.tag_of_expr e) (U.meta_of_expr e) ch
    in
    let dummy = U.mk_const (TInternal(TValue(canonical TUnknown)), []) CUnknown
    in fold_tree (fun _ _ -> None) alter_expr_type None dummy e
  in

  (* Returns a pair of environments, and a CPP command *)
  let deduce_cpp_cmd type_env tdecl_env cmd =
    let fail_if_non_unit() = 
      if fst (U.meta_of_cmd cmd) = unit_t then ()
      else failwith "invalid cmd type, expected unit"
    in
    let bindings_of_cmd (type_env, tdecl_env) cmd = match U.tag_of_cmd cmd with
      | Decl d ->
        (* Disallow nested functions in commands *) 
        bindings_of_decl ~allow_function:false type_env tdecl_env d
      
      | Foreach (id,t,e) -> 
        let nt = try List.assoc id type_env with Not_found -> t
        in ((id,nt)::type_env, tdecl_env)
 
      | _ -> (type_env, tdecl_env)
    in
    let alter_cmd_type ((type_env, tdecl_env) as env, ch) cmd =
      fail_if_non_unit();
      match U.tag_of_cmd cmd with
      | Assign (id,e) -> 
        let nc = U.mk_assign (U.meta_of_cmd cmd) id (deduce_cpp_expr type_env tdecl_env e)
        in env, nc

      | Decl d -> (remove_bindings_of_decl type_env tdecl_env d), cmd 

      | Expr e -> 
        let nc = U.mk_expr (U.meta_of_cmd cmd) (deduce_cpp_expr type_env tdecl_env e) 
        in env, nc

      | Block -> env, U.mk_block (U.meta_of_cmd cmd) ch
      
      | Foreach (id,t,e) -> 
        let ne = deduce_cpp_expr type_env tdecl_env e in
        let nt = try List.assoc id type_env with Not_found -> t in
        let nenv = (filter_first_assoc id type_env), tdecl_env
        in nenv, (U.mk_for (U.meta_of_cmd cmd) id nt ne ch) 
      
      | IfThenElse pred ->
        let npred = deduce_cpp_expr type_env tdecl_env pred 
        in env, (U.mk_ifelse (U.meta_of_cmd cmd) npred ch)

      | CExt ec -> typecheck_external_cmd type_env tdecl_env (U.meta_of_cmd cmd) ch ec 
    in
    let dummy = 
      let umeta = TInternal(TValue(canonical TUnknown)), []
      in U.mk_expr umeta (U.mk_const umeta CUnknown) 
    in
    fold_tree_thread bindings_of_cmd alter_cmd_type (type_env, tdecl_env) dummy cmd
  in

  (* Returns a pair of environments and a CPP command list *)
  let deduce_cpp_decl (type_env, tdecl_env, nprog) (d,(t,a)) =
    let rtype_env, rtdecl_env = bindings_of_decl type_env tdecl_env d in 
    match d with
	  | DVar (id, dt, da_opt) ->
      let nda_opt = match da_opt with
        | Some(Constructor(exprs)) -> 
          let nexprs = List.map (deduce_cpp_expr type_env tdecl_env) exprs 
          in Some(Constructor(nexprs))
        | Some(Init(e)) -> Some(Init(deduce_cpp_expr type_env tdecl_env e))
        | None -> None
      in rtype_env, rtdecl_env, (nprog@[DVar(id, dt, nda_opt), (t,a)])

	  | DFn (id, arg, ret_t, body) ->
      (* Type environments used inside the function body are extended with function
       * arguments, and threaded for local declarations. These are do not escape
       * the function definition. *) 
      let ntype_env = type_env@(List.map (fun (id,vt) -> id, iv_type vt) (typed_vars_of_arg arg)) in
      let _, rbody = 
        List.fold_left (fun ((te,tde),acc) c ->
          let nenvs, nc = deduce_cpp_cmd te tde c in nenvs, (acc@[nc])
        ) ((ntype_env, tdecl_env), []) body 
      in
      rtype_env, rtdecl_env, (nprog@[DFn(id, arg, ret_t, rbody), (t,a)])

    | _ -> rtype_env, rtdecl_env, (nprog@[(d,(t,a))])
  in 

  let _, _, nprog = List.fold_left deduce_cpp_decl ([],[],[]) program
  in nprog
