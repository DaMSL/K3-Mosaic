open Util
open Tree

open K3.AST
open K3.Annotation
open K3Typechecker

open CPP
open CPP.CPPTarget
open CPP.CPPTarget.ASTImport
open CPPTyping

module R = Runtime.Make(CPPTarget)
open R

module U = ImperativeUtil.Util(CPPTarget)
open U

let composite_field_prefix = "__a"

(* Creates Boost Multi-Index extractors for index positions *)
let bmi_indexes collection_t element_decl_t element_t (pidx_pos, sidx_pos) =
  let extractor_of_position element_decl_t i = 
    let id,t = match element_decl_t with
      | TComposite fields_t -> List.nth fields_t i 
      | _ -> failwith "invalid element type for Boost Multi Index"
    in BMIMember (t,id)
  in 
  let extractor_of_positions element_decl_t element_t pos = match pos with
    | [] -> BMIIdentity(element_t)
    | [i] -> extractor_of_position element_decl_t i
    | _ -> BMIComposite (List.map (extractor_of_position element_decl_t) pos)
  in
  let mk_bmi_index unique sorted pos = 
    let idx_id = 
      if pos = [] then "primary"
      else String.concat "" (List.map string_of_int pos)
    in
    idx_id, unique, sorted, extractor_of_positions element_decl_t element_t pos
  in
  
  (* TODO: uniqueness and ordering for secondaries *)
  let secondary = List.map (mk_bmi_index false false) sidx_pos in
  
  let primary =
    (* TODO: ordered primaries *)
    match pidx_pos with
    | Some(pos) -> [mk_bmi_index (collection_t = TSet) false pos]
    | None ->
      (* Bags need a non-unique primary index over the whole element *)
      if collection_t = TBag then [mk_bmi_index false false []] else [] 
  
  in primary@secondary

(* Retrieves a single primary, and possibly multiple secondary index positions
 * from a list of annotations *)
let indexes_of_annotations anns =
  let extract_index (pidx_pos, sidx_pos) a = match a with
    | Data(Constraint, FunDep(arg_pos, dep_pos)) ->
      begin match pidx_pos with
      | None -> Some(arg_pos), sidx_pos
      | _ -> failwith "found multiple primary indexes"
      end

    | Data(Hint, FunDep(arg_pos, dep_pos)) ->
      failwith "soft functional deps not supported"

    | Data(_, Index(k_pos)) -> pidx_pos, sidx_pos@[k_pos]
    | _ -> (pidx_pos, sidx_pos)
  in List.fold_left extract_index (None, []) anns

let element_of_collection t = match t with
  | TInternal(TValue(vt)) -> 
    begin match base_of vt with TCollection (ct,et) -> Some(ct, et) | _ -> None end
  | _ -> None

(* Creates a composite type declaration from a K3 base type, e.g. for element
 * types of Boost Multi-Indexes *)
let rec composite_decl_of_type id counter vt =
  let rcr_on_collection c vt =
    type_declarations_of_collection id c (TInternal (TValue vt))
  in
  let fields_of_list l = 
    let x, y, z = snd (List.fold_left (fun (i,(counter,acc,decl_acc)) vt ->
      let n_counter, nt, n_decls = rcr_on_collection i vt in
        i+1, (n_counter,
              acc@[composite_field_prefix^(string_of_int i), nt],
              (decl_acc@n_decls))
      ) (0,(counter,[],[])) l)
    in if y = [] then failwith "invalid field types for composite"
       else x, TComposite(y), z
  in
  match base_of vt with
  | TUnit | TUnknown | TAddress | TTarget _ ->
    failwith "invalid argument for composite type construction"

  (* Unwrap tuple collections *)
  | TTuple vtl -> fields_of_list vtl

  (* Create a direct composite for any other element type *)
  | _ -> fields_of_list [vt]

(* TODO: handle if the current collection does not have indexes, but some
 * collection child field does *)
and type_declarations_of_collection id counter t =
  let is_external_type t = match t with TInternal _ -> false | _ -> true in
  let unit_t = TInternal(TValue(canonical TUnit)) in 
  match element_of_collection t with
  | Some (collection_t, element_vt) ->
    let indexes = match t with
      | TInternal(TValue vt) -> indexes_of_annotations (annotation_of vt)
      | _ -> None, []
    in
    let nt_id, nt_elem_id = 
      let prefix = id^"__"^(string_of_int counter) in
      prefix^"_bmi", prefix^"_bmi_elem"
    in    
    let c, t_decl, decl_cmds = composite_decl_of_type id (counter+1) element_vt in
    begin match t_decl with
    | TComposite(tl) ->
      let convert =
        (match indexes with None, [] -> false | _,_ -> true)
        || List.exists is_external_type (List.map snd tl)
      in 
      if convert then
        let n_decl_cmds = (decl_cmds@[DType(nt_elem_id, t_decl), (unit_t,[])]) in
        let bmi_idx = bmi_indexes collection_t t_decl (TNamed nt_elem_id) indexes in
        let c_t_decl_cmd =
          [DType(nt_id, TExtDecl(TBoostMultiIndex(nt_elem_id, bmi_idx))), (unit_t, [])]
        in
          c, TNamed(nt_id), (n_decl_cmds@c_t_decl_cmd)
      else c, t, decl_cmds
  
    | _ -> c, t, decl_cmds
    end

  | _ -> counter, t, []
  
(* Declares a collection as a Boost Multi-Index.
 * We assume annotation checking has been performed. *)
let datastructure_of_collection id t da_opt (dt, da) =
  match type_declarations_of_collection id 0 t with
  | _, TNamed(nt_id), t_decls -> 
    let nc_decl = DVar(id, TNamed(nt_id), da_opt) in
    let nbmi_bindings = [id, TNamed(nt_id)] in
    (t_decls@[nc_decl, (dt,da)]), nbmi_bindings
  
  | _, _, t_decls ->  t_decls, []
  

(* Substitutes global variable definitions annotated with indexing
 * requirements to be Boost Multi-Indexes. 
 * Returns a new program, as well as bindings for substituted data structures *)

(* TODO:
 * i. local declarations
 *)
let optimize_datastructures program =
  let is_var_decl (d,_) = match d with DVar _ -> true | _ -> false in
  let split_decls decls = 
    let x,y = List.partition is_var_decl decls in
    (List.map (fun (d,(t,a)) -> mk_decl (t,a) d) x), y
  in
  let rec bmi_of_cmd (decls, cmds, bindings) cmd =
    let rebuild_cmd cmd_f cmd_l =
      let sub_decls, cmds_per_sub, nb =
        List.fold_left (fun (decl_acc, cmds_per_sub, b) sub_cmd ->
            let ndecls, nsub_cmds, nb = bmi_of_cmd ([], [], b) sub_cmd in
            let var_decls, rest_decls = split_decls ndecls in
            (decl_acc@rest_decls), cmds_per_sub@[var_decls@nsub_cmds], nb
          ) ([], [], bindings) cmd_l
      in (decls@sub_decls), (cmds@(cmd_f cmds_per_sub)), nb
    in
    match tag_of_cmd cmd with
    | Decl d -> 
      let ndecls, nb = bmi_of_decl ([], bindings) (d, meta_of_cmd cmd) in
      let var_decls, rest_decls = split_decls ndecls in
        (decls@rest_decls), (cmds@var_decls), nb
   
    | IfThenElse p ->
      let block_unless_single l =
        if List.length l = 1 then List.hd l
        else mk_block (meta_of_cmd (List.nth (sub_tree cmd) 1)) l
      in 
      rebuild_cmd (fun cmds_per_sub ->
          [mk_ifelse (meta_of_cmd cmd) p (List.map block_unless_single cmds_per_sub)]
        ) (sub_tree cmd)

    | Block ->
      rebuild_cmd (fun cmds_per_sub ->
          [mk_block (meta_of_cmd cmd) (List.flatten cmds_per_sub)]
        ) (sub_tree cmd)
 
    | Foreach (id,t,e) ->
      rebuild_cmd (fun cmds_per_sub ->
          [mk_for (meta_of_cmd cmd) id t e (List.flatten cmds_per_sub)]
        ) (sub_tree cmd)
        
    | While e ->
      rebuild_cmd (fun cmds_per_sub ->
          [mk_while (meta_of_cmd cmd) e (List.flatten cmds_per_sub)]
        ) (sub_tree cmd)

    | _ -> decls, (cmds@[cmd]), bindings

  and bmi_of_decl (nprog,bindings) (d,(dt,da)) = match d with
    | DVar (id,t,da_opt) ->
      begin match datastructure_of_collection id t da_opt (dt,da) with
        | [], [] -> (nprog@[d,(dt,da)],bindings)
        | x,y -> (nprog@x, bindings@y)
      end

    | DFn (id,arg,ret_t,body) -> 
      let ndecls, nbody, nbindings =
        List.fold_left bmi_of_cmd ([], [], bindings) body 
      in nprog@ndecls@[DFn (id,arg,ret_t,nbody), (dt,da)], nbindings

    | _ -> (nprog@[d,(dt,da)], bindings)
  in 
  let rec optimize_ds_program prog =
    List.fold_left (fun (cacc, bacc) c -> match c with
      | Include (name, None, _, _) -> cacc@[c], bacc
      | Include (name, Some(p), code, expected) ->
        let x,y = optimize_ds_program p in
        cacc@[Include (name, Some(x), code, expected)], bacc@y
      | Component decls -> 
        let x,y = List.fold_left bmi_of_decl ([], bacc) decls
        in cacc@[Component x], y
    ) ([],[]) prog
  in
  let nprog, bindings = optimize_ds_program program in
  (* Redo type inference on nprog *)
  (deduce_program_type nprog), bindings 

(* TODO:
 * CPP transforms:
 *
 * -- Collection operations:
 *   ++ Update(Map) => x[k] = v
 *
 * The following need additional constructs in our CPP AST:
 * -- Collection operations:
 *   ++ Foreach(Slice) => GetIndex;
 *                        IteratorPair = Index.EqualRange;
 *                        For ...
 *
 *    ** Note that slices are always reified for now, thus we may never have the
 *       above situation. This will be inefficient since the slice will explicitly
 *       materialize, and we should optimize.
 *
 *    ** This could be implemented by adding Slice detection as well as Range
 *       detection to the 'skipped' list in ReifiedK3.reify_from_parent
 *
 *   ++ Slice => Declare; GetIndex;
 *               IteratorPair = Index.EqualRange;
 *               Copy(IteratorPair, Back(Declared))
 *
 *     ** Since all Slice nodes are reified, this would cause a double-copy
 *        if we use a fresh declaration here. We want to obtain the target of
 *        the reification, and use the same approach without explicitly
 *        declaring at this point.
 *        TODO: how do we pass in the target for copying at this point of CG?
 *
 *
 * -- Late reification of collection operations. That is, these "builtins" are
 *    never provided as direct methods in CPP, and when requested as direct
 *    methods, must be reified here. Alternatively, we can throw an error or
 *    check if the implementation language supports these builtins prior to
 *    their construction upstream.
 *
 *   ++ Combine => Declare;
 *                 LeftIteratorPair; Copy(LeftIteratorPair, Back(Declared));
 *                 RightIteratorPair; Copy(RightIteratorPair, Back(Declared))
 *
 *   ++ Range   => Declare; For(Insert(Declared))
 *
 *   ++ Sort(Set|Bag) => Declare(List);
 *                       Copy(Begin, End, Back(Declared));
 *                       sort(DeclaredBegin, DeclaredEnd)
 *   ++ Sort(List)    => sort(Begin, End)
 *
 * -- Collection w/ single index annotation => TMap
 *   ++ Peek(Slice) => Find
 *   ++ Slice == Empty => Contains 
 *
 *) 

let rewrite_expr_node mk_meta td child_l e =
  let type_of_expr e = fst (meta_of_expr e) in
  let meta t = t, mk_meta() in
  let unit_meta, str_meta, int_meta, bool_meta, addr_meta = 
    meta unit_t, meta string_t, meta int_t, meta bool_t, meta addr_t
  in
  let e_meta, children = meta_of_expr e, List.flatten child_l in
  let ret e = [e] in
  match tag_of_expr e with
    | Const  (CTarget t)  -> ret @: mk_target_var mk_meta t

    | Const  (CAddress (host, port)) ->
      ret @: U.mk_fn addr_meta (Named "make_address")
        [U.mk_const str_meta (CString host); U.mk_const int_meta (CInt port)]

    (* TODO: this should only apply for sets, bags and lists *)
    | Fn (Collection Update) ->
      let c_expr = List.hd children in
      let old_expr, new_expr = List.nth children 1, List.nth children 2 in
      [U.mk_fn unit_meta (Collection Delete) [c_expr; old_expr];
       U.mk_fn unit_meta (Collection Insert) [c_expr; new_expr]]

    | Fn (Collection Find) -> 
      let c_expr = List.hd children in
      let it_meta = meta @: TExt (TIterator (type_of_expr c_expr))
      in ret @: (U.mk_fn it_meta (Collection Find) children)

    | Fn (Collection Contains) ->
      let c_expr = List.hd children in
      let it_meta = meta @: TExt (TIterator (type_of_expr c_expr)) in
      let new_children = [U.mk_fn it_meta (Collection Find) children;
                          U.mk_fn it_meta (Collection (CFExt EndIterator)) [c_expr]]
      in ret @: U.mk_op bool_meta Eq new_children
    
    | _ -> ret @: recompose_tree e children

let rewrite_expr mk_meta e =
  List.hd (fold_tree (fun _ _ -> None) (rewrite_expr_node mk_meta) None [] e)

let rec rewrite_cmd_node mk_meta td child_l c =
  let type_of_expr e = fst (meta_of_expr e) in
  let meta t = t, mk_meta() in
  let unit_meta, int_meta, bool_meta = meta unit_t, meta int_t, meta bool_t in
  let c_meta, children = meta_of_cmd c, List.flatten child_l in
  let ret c = [c] in
  match tag_of_cmd c with
  | Assign (id, e) -> ret @: U.mk_assign c_meta id (rewrite_expr mk_meta e)
  | Decl d -> ret @: U.mk_decl c_meta (rewrite_declaration mk_meta d)
  | Expr e -> ret @: U.mk_expr c_meta (rewrite_expr mk_meta e)
  | IfThenElse pred -> [U.mk_ifelse c_meta (rewrite_expr mk_meta pred) children]

  | Foreach (loop_var, loop_var_t, coll_expr) ->
    let decl_and_loop_cmds =
      let iterator_decls, iterator, end_iterator = match tag_of_expr coll_expr with
        | Var(id) ->
          let iterator_id = "it_"^id in
          let end_iterator_id = "end_"^id in
          let it_type, it_meta = 
            let t = TExt (TIterator (type_of_expr coll_expr)) in t, meta t
          in
          let it_init, end_init =
            Some(Init(U.mk_fn it_meta (Collection (CFExt BeginIterator)) [coll_expr])),
            Some(Init(U.mk_fn it_meta (Collection (CFExt EndIterator)) [coll_expr]))
          in
          let iterator_decl = U.mk_var_decl iterator_id it_type it_init in
          let end_iterator_decl = U.mk_var_decl end_iterator_id it_type end_init in
            [U.mk_decl unit_meta iterator_decl; 
             U.mk_decl unit_meta end_iterator_decl],
            U.mk_var it_meta iterator_id, U.mk_var it_meta end_iterator_id

        | _ -> failwith "non-reified loops are currently unsupported"
      in
      let test_expr = U.mk_op bool_meta Neq [iterator; end_iterator] in
      let advance_expr = U.mk_fn unit_meta (FExt IteratorIncrement) [iterator] in
      let for_tag = CExt (For (None, Some(test_expr), Some(advance_expr)))
      in iterator_decls@[U.mk_cmd for_tag unit_meta children]
    in
    ret @: U.mk_block unit_meta decl_and_loop_cmds

  | While cond -> ret @: U.mk_while c_meta (rewrite_expr mk_meta cond) children
  | Return retval -> ret @: U.mk_return c_meta (rewrite_expr mk_meta retval)

  | _ -> ret @: recompose_tree c children

and rewrite_cmd mk_meta c =
  List.hd (fold_tree (fun _ _ -> None) (rewrite_cmd_node mk_meta) None [] c)

and rewrite_declaration mk_meta d = match d with
  | DType (id, t_decl) -> d
  
  | DVar (id, t, da_opt) -> d
  
  | DFn (id, t_arg, t_ret, body) -> 
    let new_cmds = List.map (rewrite_cmd mk_meta) body
    in DFn(id, t_arg, t_ret, new_cmds)

  | DClass (id, parent_opt, class_decls) ->
    let new_decls = List.map (rewrite_declaration_w_meta mk_meta) class_decls
    in DClass(id, parent_opt, new_decls)

and rewrite_declaration_w_meta mk_meta (d,meta) = (rewrite_declaration mk_meta d, meta)

let rec rewrite_component mk_meta c = match c with
  | Include (id, Some(prog), None, false) ->  
    Include(id, Some(cpp_rewrite mk_meta prog), None, false)

  | Include (id, None, None, _)
  | Include (id, _, _, true)
  | Include (id, None, Some(_), false) -> c

  | Include (_, Some(_), Some(_), _) ->
    failwith "invalid module, modules cannot contain both programs and raw code"

  | Component decls_with_meta -> Component (List.map (rewrite_declaration_w_meta mk_meta) decls_with_meta)

and cpp_rewrite mk_meta program = List.map (rewrite_component mk_meta) program

let cpp_of_imperative mk_meta program = 
  cpp_rewrite mk_meta (fst (optimize_datastructures program))