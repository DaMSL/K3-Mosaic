open Util
open Tree

open K3.AST
open K3.Annotation
open K3Util
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
let bmi_tag_prefix = "idx"

let is_external_type t = match t with TInternal _ -> false | _ -> true

let positions_of_deps d = match d with Element -> [] | Positions p -> p

let tuple_type_fields vt = match base_of vt () with
  | TTuple fields -> fields
  | _ -> failwith "invalid tuple type"

let tuple_arity vt = List.length (tuple_type_fields vt)

let project_tuple_type_fields vt pos =
  List.map (List.nth (tuple_type_fields vt)) pos

let project_tuple_type vt pos =
  let t =  match project_tuple_type_fields vt pos with
    | []  -> failwith "invalid tuple projection"
    | [x] -> base_of x ()
    | x   -> TTuple x
  in ib_type t

let element_of_collection t = match t with
  | TInternal(TValue(vt)) ->
    begin match base_of vt () with TCollection (ct,et) -> Some(ct, et) | _ -> None end
  | _ -> None


(* Projecting function objects *)
let type_projection_id prefix vt pos =
  let element_sig = signature_of_type (TValue vt) in
  let pos_str = String.concat "" (List.map string_of_int pos)
  in prefix^"_"^element_sig^"_"^pos_str

let project_fn_id vt pos = type_projection_id "project" vt pos

let projection_of_positions vt pos =
  let mk_meta () = [] in
  let arg_id, arg_t, ret_t, arg_meta, ret_meta, unit_meta =
    let a_t, r_t = iv_type vt, project_tuple_type vt pos
    in "a", a_t, r_t, (a_t, mk_meta()), (r_t, mk_meta()), (unit_t, mk_meta())
  in
  let op_body =
    let pos_fields =
      let fields_t = project_tuple_type_fields vt pos in
      List.map2 (fun i t ->
          mk_fn (iv_type t, mk_meta()) (Member (Position i)) [mk_var arg_meta arg_id]
        ) pos fields_t
    in [mk_return unit_meta (mk_tuple ret_meta pos_fields)]
  in [DFn(project_fn_id vt pos, [arg_id, arg_t], ret_t, op_body), (unit_t, [])]

(* TODO: inherity CPP unary/binary function *)
let function_object_of_positions prefix mk_body vt pos =
  let mk_meta () = [] in
  let class_id, pfn_id = type_projection_id prefix vt pos, project_fn_id vt pos in
  let arg_t, pos_t, unit_meta = iv_type vt, project_tuple_type vt pos, (unit_t, mk_meta()) in
  let op_args, ret_t, op_body = mk_body pfn_id arg_t pos_t in
  let body = (projection_of_positions vt pos)@
             [DFn("operator()", op_args, ret_t, op_body), (unit_t, [])]
  in DClass(class_id, None, body), (unit_t, mk_meta())

let unary_function_object_of_positions prefix mk_body vt pos =
  let unary_body f_id arg_t pos_t =
    let arg_id, op_args = "a", ["a", arg_t] in
    let t, e = mk_body arg_id f_id arg_t pos_t
    in op_args, t, e
  in function_object_of_positions prefix unary_body vt pos

let binary_function_object_of_positions prefix mk_body vt pos =
  let binary_body f_id arg_t pos_t =
    let arg1_id, arg2_id, op_args = "a", "b", ["a", arg_t; "b", arg_t] in
    let t, e = mk_body arg1_id arg2_id f_id arg_t pos_t
    in op_args, t, e
  in function_object_of_positions prefix binary_body vt pos

let comparator_of_positions prefix cmp_op vt pos =
  let mk_meta () = [] in
  let cmp_fun arg1_id arg2_id proj_fn_id arg_t pos_t =
    let arg_meta, pos_meta, unit_meta, bool_meta =
      let f t = t, mk_meta() in f arg_t, f pos_t, f unit_t, f bool_t
    in
    let mk_project arg_id = mk_fn pos_meta (Named proj_fn_id) [mk_var arg_meta arg_id] in
    let cmp_expr = mk_op bool_meta cmp_op [mk_project arg1_id; mk_project arg2_id]
    in bool_t, [mk_return unit_meta cmp_expr]
  in binary_function_object_of_positions prefix cmp_fun vt pos

let inequality_of_positions vt pos = comparator_of_positions "less" Lt vt pos
let equality_of_positions vt pos = comparator_of_positions "equal" Eq vt pos

let hash_of_positions vt pos =
  let mk_meta () = [] in
  let hash_fn arg_id proj_fn_id arg_t key_t =
    let ret_t, ret_meta, arg_meta, pos_meta, unit_meta =
      let f t = t, mk_meta() in int_t, f int_t, f arg_t, f key_t, f unit_t
    in
    let mk_project arg_id = mk_fn pos_meta (Named proj_fn_id) [mk_var arg_meta arg_id] in
    let body = [mk_return unit_meta (mk_fn ret_meta (Named "boost::hash_value") [mk_project arg_id])]
    in ret_t, body
  in unary_function_object_of_positions "hash" hash_fn vt pos


(* Annotation-based collection specialization *)

(* Retrieves a single primary, and possibly multiple secondary index positions
 * from a list of annotations *)
let indexes_of_annotations anns =
  let extract_indexes acc a = match a with
    | Data(_, FunDep(_, Positions _))
    | Data(_, MVFunDep(_, Positions _)) ->
      failwith "all dependencies must currently range over the whole tuple"

    | Data(Constraint, FunDep(pos, dep)) -> acc@[true, false, pos, Some(dep)]
    | Data(Constraint, MVFunDep(pos, dep)) -> acc@[false, false, pos, Some(dep)]
    | Data(Constraint, Unique(pos)) -> acc@[true, false, pos, None]
    | Data(Constraint, Ordered(pos)) -> acc@[false, true, pos, None]

    | Data(Hint, FunDep(_, _))
    | Data(Hint, MVFunDep(_,_)) ->
      failwith "soft dependencies not supported"

    | _ -> acc
  in
  let indexes = List.fold_left extract_indexes [] anns in
  if indexes = [] then None, indexes
  else
    (* For now, pick the fun dep with the minimal head as the primary *)
    (* TODO: should only match with those indexes that cover the full element *)
    let primary = List.fold_left (fun ((cu,co,cp,cd) as current) ((u,o,p,d) as idx) ->
        match u, d with
        | true, Some _ -> if List.length p < List.length cp then idx else current
        | _ -> current
      ) (List.hd indexes) (List.tl indexes)
    in Some(primary), List.filter ((<>) primary) indexes


(* Creates Boost Multi-Index extractors for index positions *)
let bmi_indexes collection_t element_decl_t element_t (primary_opt, secondaries) =
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
  let mk_bmi_index (unique, ordered, pos, dep) =
    let idx_id = if pos = [] then "primary"
                 else bmi_tag_prefix^(String.concat "" (List.map string_of_int pos))
    in idx_id, unique, ordered, extractor_of_positions element_decl_t element_t pos
  in
  let p =
    match primary_opt with
    | Some(uniq, ord, pos, dep) -> [mk_bmi_index ((collection_t = TSet || uniq), ord, pos, dep)]
    | None ->
      (* BMIs default to an ordered set when no indexes are specified.
       * Thus bags explicitly need a non-unique primary index over the whole element. *)
      if collection_t = TBag then [mk_bmi_index (false, false, [], [])] else []
  in p@(List.map mk_bmi_index secondaries)


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
  match base_of vt () with
  | TUnit | TUnknown | TAddress | TTarget _ ->
    failwith "invalid argument for composite type construction"

  (* Unwrap tuple collections *)
  | TTuple vtl -> fields_of_list vtl

  (* Create a direct composite for any other element type *)
  | _ -> fields_of_list [vt]


(* CPP/STL collection type generation for basic annotations *)
and type_of_single_dependency_collection elem_vt uniq ord pos deps =
  let elem_t = iv_type elem_vt in
  let eq_class, eq_class_id = equality_of_positions elem_vt pos, type_projection_id "eq" elem_vt pos in
  let less_class, less_class_id = inequality_of_positions elem_vt pos, type_projection_id "less" elem_vt pos in
  let hash_class, hash_class_id = hash_of_positions elem_vt pos, type_projection_id "hash" elem_vt pos in
  let reto, retu = (fun ct -> ct, elem_t, [less_class]), (fun ct -> ct, elem_t, [eq_class; hash_class]) in
  match uniq, ord, deps with
    | true, true, None ->
      (* STL set *)
      reto @: TExt(TSTLCollection(TSTLSet elem_t, [TOrdered (Some less_class_id)]))

    | false, true, None ->
      (* STL multiset *)
      reto @: TExt(TSTLCollection(TSTLSet elem_t, [TOrdered (Some less_class_id); TMulti]))

    | true, false, None ->
      (* STL unordered_set *)
      retu @: TExt(TSTLCollection(TSTLSet elem_t, [TUnordered (Some(hash_class_id), Some(eq_class_id))]))

    | false, false, None ->
      (* STL unordered_multiset *)
      retu @: TExt(TSTLCollection(TSTLSet elem_t, [TUnordered (Some(hash_class_id), Some(eq_class_id)); TMulti]))

    | _, _, Some(d) ->
      let ret key_t value_t =
        (* The associative containers here do not need projection function objects
         * since they split the element type into key and value parts. *)
        let ct = match uniq, ord with
          | true, true -> TExt(TSTLCollection(TSTLMap (key_t, value_t), [TOrdered None]))
          | false, true -> TExt(TSTLCollection(TSTLMap (key_t, value_t), [TOrdered None; TMulti]))
          | true, false -> TExt(TSTLCollection(TSTLMap (key_t, value_t), [TUnordered (None, None)]))
          | false, false -> TExt(TSTLCollection(TSTLMap (key_t, value_t), [TUnordered (None, None); TMulti]))
        in ct, TExt(TPair (key_t, value_t)), []
      in
      let key_t = project_tuple_type elem_vt pos in
      let value_t = match d with
        | Element ->
          let rec diff_pos pos acc i =
            let nacc = if List.mem i pos then acc else i::acc
            in match i with 0 -> nacc | _ -> diff_pos pos nacc (i-1)
          in project_tuple_type elem_vt (diff_pos pos [] ((tuple_arity elem_vt)-1))
        | Positions(dep_pos) -> project_tuple_type elem_vt dep_pos
      in ret key_t value_t


and wrap_collection primary (orig_ct, orig_elem_t) (ct, elem_t, elem_t_decl_opt) (uniq, ord, pos, deps) =
  let mk_meta () = [] in
  let meta t = t, mk_meta() in
  let mk_itmv id t = id, t, meta t, mk_var (meta t) id in
  let unit_meta, bool_meta = let f t = t, mk_meta() in f unit_t, f bool_t in

  let mk_pos_str p = String.concat "" (List.map string_of_int p) in
  let pos_str = mk_pos_str pos in

  let element_error () = failwith "invalid element type and declaration" in
  let stl_collection_error () = failwith "invalid STL collection" in

  (* AST constructor helpers *)
  let mk_empty_decl id t = mk_decl unit_meta (mk_var_decl id t None) in
  let mk_init_decl id t init_expr = mk_decl unit_meta (mk_var_decl id t (Some(Init init_expr))) in
  let mk_build_decl id t c_args = mk_decl unit_meta (mk_var_decl id t (Some(Constructor c_args))) in
  let mk_effect_expr f args = mk_expr unit_meta (mk_fn unit_meta f args) in
  let mk_fn_call_expr m id args = mk_expr m (mk_fn m (Named id) args) in
  let mk_assign_from_fn m id fn args = mk_assign unit_meta id (mk_fn m fn args) in
  let mk_if pred then_branch = mk_ifelse unit_meta pred [then_branch; mk_block unit_meta []] in

  let mk_idx_fn m id args =
    let idx_tag, idx_t, idx_method =
      let x = bmi_tag_prefix^pos_str in x, TNamed ("index<"^x^">"), "get<"^x^">()."^id
    in mk_fn m (Named idx_method) args
  in
  let mk_idx_effect_expr m id args = mk_expr unit_meta (mk_idx_fn m id args) in

  let project_elem tuple_t pos = project_tuple_type (vi_type tuple_t) pos in
  let tuple_of_pos tuple_t tuple_expr pos =
    let fields_t = project_tuple_type_fields (vi_type tuple_t) pos
    in mk_tuple (meta (project_tuple_type (vi_type tuple_t) pos))
        (List.map2 (fun i t -> mk_fn (meta (iv_type t)) (Member (Position i)) [tuple_expr]) pos fields_t)
  in

  let mk_apply_first_match cit_t match_expr f =
    let it_id, _, cit_meta, it_var = mk_itmv "c_it" cit_t in
    let end_id, _, _, end_var = mk_itmv "c_end" cit_t in
    let valid_pred = mk_op bool_meta Neq [it_var; end_var] in
      [mk_init_decl it_id cit_t (mk_fn cit_meta (Named "find") [match_expr]);
       mk_init_decl end_id cit_t (mk_fn cit_meta (Named "end") []);
       mk_if valid_pred (mk_block unit_meta (f it_var))]
  in

  let mk_apply_match_range cit_t match_expr f =
    let it_id, _, cit_meta, it_var = mk_itmv "c_it" cit_t in
    let end_id, _, _, end_var = mk_itmv "c_end" cit_t in
    let range_id, range_t, range_meta, range_var = mk_itmv "key_range" (TExt (TPair (cit_t, cit_t))) in
    let pair_expr pair_op = mk_fn cit_meta (FExt pair_op) [range_var] in
    let range_pred = mk_op bool_meta Neq [pair_expr PairFirst; pair_expr PairSecond] in
    let range_body = mk_block unit_meta
      ([mk_init_decl it_id cit_t (pair_expr PairFirst);
        mk_init_decl end_id cit_t (pair_expr PairSecond)]
       @(f it_var end_var))
    in
    let match_fn = if primary then mk_fn range_meta (Named "equal_range") [match_expr]
                   else mk_idx_fn range_meta "equal_range" [match_expr]
    in
    [mk_init_decl range_id range_t match_fn;
     mk_if range_pred range_body]
  in

  let mk_apply_match_for_loop cit_t match_expr loop_cmds =
    let loop_gen it_var end_var =
      let test_expr = mk_op bool_meta Neq [it_var; end_var] in
      let adv_expr = mk_fn unit_meta (FExt IteratorIncrement) [it_var] in
      [mk_cmd (CExt (For (None, Some test_expr, Some adv_expr))) unit_meta (loop_cmds it_var)]
    in mk_apply_match_range cit_t match_expr loop_gen
  in

  let mk_apply_match_while_loop cit_t match_expr while_fn =
    let loop_gen it_var end_var =
      let init_cmds, test_expr, cmds = while_fn it_var end_var
      in init_cmds@[mk_cmd (CExt (For (None, Some test_expr, None))) unit_meta cmds]
    in mk_apply_match_range cit_t match_expr loop_gen
  in

  let mk_insert_fn mk_elem arg_id arg_t arg_var =
    let e, cmds = mk_elem "insert_elem" arg_var in
    let body = cmds@[mk_fn_call_expr unit_meta "insert" [e]]
    in [DFn("insert", [arg_id, arg_t], unit_t, body)]
  in

  let diff_pos n pos =
    let rec aux pos acc i =
      let nacc = if List.mem i pos then acc else i::acc
      in match i with 0 -> nacc | _ -> aux pos nacc (i-1)
    in aux pos [] n
  in

  let element_arity () = match bi_type orig_elem_t with
    | TTuple fields -> List.length fields
    | _ -> 1
  in

  let check_valid_element_positions pos =
    let all_greater = List.for_all ((>) (element_arity ())) pos in
    if pos = [] || not all_greater then element_error () else ()
  in

  let is_native_collection = match ct with
    | TInternal (TValue vt) ->
      begin try (let _ = collection_of (base_of vt ()) (fun () -> raise Not_found) in true)
            with Not_found -> false
      end
    | _ -> false
  in
  let is_stl_collection, is_stl_map =
    let x,y = match ct with
      | TExt (TSTLCollection (x, _)) -> true, (match x with TSTLMap _ -> true | _ -> false)
      | _ -> false, false
    in if x && not primary then stl_collection_error() else x,y
  in
  let is_boost_collection = not (is_native_collection || is_stl_collection) in

  let is_dependence_free = pos = [] || (match ct, deps with
      | TExt (TSTLCollection (TSTLMap _, _)), _ -> false
      | _, Some(Element) -> false
      | _, Some(Positions x) -> x = []
      | _, _ -> true)
  in

  (* Generate a dependence-free collection interface containing the following functions:
      -- insert(new)
      -- erase(existing)
      -- update(existing,new)
      -- slice(existing)
   *)
  let generate_dependence_free_methods () =
    let ce_t, cit_t, te_t = elem_t, TExt(TIterator ct), orig_elem_t in
    let ce_meta, cit_meta = meta ce_t, meta cit_t in

    let arg_id, arg_t, arg_meta, arg_var = mk_itmv "elem" orig_elem_t in
    let old_arg_id, _, _, old_arg_var = mk_itmv "existing" arg_t in

    (* Conversion expression construction *)
    let rec mk_elem id te_expr = match ce_t with
      | TNamed tid -> mk_var ce_meta id, [mk_build_decl id ce_t [te_expr]]
      | _ -> te_expr, []
    in

    let insert_fn = if ce_t = te_t then []
                    else mk_insert_fn mk_elem arg_id arg_t arg_var  in

    let delete_fn =
      if ce_t = te_t then [] else
      let existing_id, (existing_expr, existing_decls) = old_arg_id, mk_elem old_arg_id arg_var in
      let erase_cmds it_var end_var =
        let pred = mk_op bool_meta Neq [it_var; end_var]
        in [mk_if pred (mk_fn_call_expr unit_meta "erase" [it_var; end_var])]
      in
      let erase_body = existing_decls
                      @(mk_apply_match_range cit_t existing_expr erase_cmds)
      in [DFn("erase", [arg_id, arg_t], unit_t, erase_body)]
    in

    let update_fn =
      let existing_id, (existing_expr, existing_decls) = let x = "old_elem" in x, mk_elem x old_arg_var in
      let new_id, (new_expr, new_decls) = let x = "new_elem" in x, mk_elem x arg_var in
      let update_cmds it_var =
        if is_boost_collection then [mk_fn_call_expr unit_meta "replace" [it_var; new_expr]]
        else
          let cur_id, _, _, cur_var = mk_itmv "cur_it" cit_t in
          [mk_init_decl cur_id cit_t it_var;
           mk_effect_expr (FExt IteratorIncrement) [it_var];
           mk_fn_call_expr unit_meta "erase" [cur_var];
           mk_fn_call_expr unit_meta "insert" [it_var; new_expr]]
      in
      let update_body = existing_decls@new_decls
                       @(mk_apply_match_for_loop cit_t existing_expr update_cmds)
      in [DFn("update", [old_arg_id, arg_t; arg_id, arg_t], unit_t, update_body)]
    in

    let slice_fn =
      let existing_id, (existing_expr, existing_decls) = old_arg_id, mk_elem old_arg_id arg_var in
      let ret_id, ret_t, ret_meta, ret_var = mk_itmv "r" ct in
      let slice_cmds it_var =
        let elem_expr = mk_fn arg_meta (FExt IteratorElement) [it_var] in
        let pred = mk_op bool_meta Eq [existing_expr; elem_expr]
        in [mk_if pred (mk_effect_expr (Collection Insert) [ret_var; elem_expr])]
      in
      let body = existing_decls
                @[mk_empty_decl ret_id ret_t]
                @(mk_apply_match_for_loop cit_t existing_expr slice_cmds)
                @[mk_return unit_meta ret_var]
      in [DFn("slice", [arg_id, arg_t], ct, body)]
    in

    List.map (fun d -> d, unit_meta)
      (List.flatten [insert_fn; delete_fn; update_fn; slice_fn])
  in

  let generate_dependency_methods d =
    let dep_pos = match d with
      | Element -> diff_pos (element_arity()-1) pos
      | Positions(dep_pos) -> dep_pos
    in

    check_valid_element_positions pos;
    check_valid_element_positions dep_pos;

    (* Notes:
      Wrapping a collection provides transparency over the specialized container's
      actual element and key type.

      We expose a collection interface based on tuples, and the specialization
      module will let us interact with the specialized container type.

      Our code generator exposes the following methods as a wrapper:
      -- insert              : tuple element -> unit
      -- erase               : tuple element -> unit
      -- update              : tuple element -> tuple element -> unit
      -- slice               : tuple element -> collection
      -- erase_<key pos>     : tuple key -> unit
      -- update_<key pos>    : tuple key -> unit
      -- slice_<key pos>     : tuple key -> collection

      The container must provide iterators, and be aware of the following types:
      -- element type
      -- container iterator type, as a handle to full elements
      -- index key type (regardless of decomposition)
      -- index value type and whether this differs from elements (i.e. for
         elements decomposed into key/values)

      The container must provide the following methods:
      -- insert              : element -> unit
      -- insert_with_hint    : iterator -> element -> unit

      -- erase_iterator      : iterator -> unit

      -- find                : key -> iterator
      -- equal_range         : key -> iterator pair
      -- find                : iterator pair -> element -> iterator

      -- element_of_iterator : iterator -> element
      -- key_of_iterator     : iterator -> key
      -- value_of_iterator   : iterator -> value

      -- optional: replace   : iterator -> element -> unit

      A code generation API for specialized data structures must provide methods to
      produce expressions for all of the above.
      We can implement our tuple-based container wrapper with these methods and
      the following:
      -- tuple element -> element
      -- tuple element -> key
      -- tuple element -> index value
      -- tuple key     -> index key

      Note that these conversion methods are not needed if all of the container's
      methods can directly accept tuples.

      Boost multi_index_containers can achieve this through the appropriate
      specialization of extractors, e.g. by inheriting from the member extractor
      and overloading its operator() method to accept tuple-based key types

      Thus our CG method expects the following information:

      1. type information: (type_t, type_t, type_t, type_t option)
         -- element, iterator, key, value if decomposed.

      2. method information:
        (specialized generator functions * converters) option
        * (tuple generator functions) option

     *)

    let (ce_t, cit_t, ck_t, cv_t), (te_t, tk_t, tv_t) =
      let a,b, key_t, val_t = match elem_t with
        | TExt (TPair (k, v)) -> elem_t, v, k, v
        | _ -> elem_t, elem_t, project_elem orig_elem_t pos, project_elem orig_elem_t dep_pos
      in (a, TExt(TIterator ct), key_t, b), (orig_elem_t, key_t, val_t)
    in

    let ce_meta, cit_meta = meta ce_t, meta cit_t in

    let arg_id, arg_t, arg_meta, arg_var = mk_itmv "elem" orig_elem_t in
    let old_arg_id, _, _, old_arg_var = mk_itmv "existing" arg_t in
    let key_arg_id, _, key_meta, key_var = mk_itmv "key" tk_t in
    let val_id, _, val_meta, val_var = mk_itmv "val" tv_t in

    (* Conversion expression construction *)
    let rec mk_elem id te_expr = match ce_t with
      | TNamed tid -> mk_var ce_meta id, [mk_build_decl id ce_t [te_expr]]
      | TExt(TPair (k_t, v_t)) ->
        let (ke, kd), (ve, vd) = mk_key_of_tuple te_expr, mk_val_of_tuple te_expr
        in mk_fn ce_meta (FExt MakePair) [ke; ve], kd@vd
      | _ -> te_expr, []

    and mk_val_of_elem elem_expr = match ce_t with
      | TExt (TPair (k_t, v_t)) when v_t = cv_t ->
        mk_fn val_meta (FExt PairSecond) [elem_expr], []
      | _ -> elem_expr, []

    and mk_key_of_tuple te_expr = if ck_t = tk_t then tuple_of_pos te_t te_expr pos, []
                                  else mk_elem key_arg_id te_expr

    and mk_key_of_index_tuple tk_expr = tk_expr, []

    and mk_val_of_tuple te_expr = if cv_t = ce_t then mk_elem val_id te_expr
                                  else tuple_of_pos te_t te_expr dep_pos, []
    in

    let (key_expr, key_decls), (val_expr, val_decls) =
      mk_key_of_tuple arg_var, mk_val_of_tuple arg_var in

    (* Context generators for erase and update methods. *)
    let mk_remove_modifier_common use_new fn_id body_fn =
      let existing_id, (existing_expr, existing_decls) =
        let x = "old_elem" in x, mk_elem x (if use_new then old_arg_var else arg_var) in
      let new_id, (new_expr, new_decls) = let x = "new_elem" in x, mk_elem x arg_var in
      let args = (if use_new then [old_arg_id, arg_t] else [])@[arg_id, arg_t] in
      let body = body_fn (existing_id, (existing_expr, existing_decls)) (new_id, (new_expr, new_decls))
      in [DFn(fn_id, args, unit_t, body)]
    in

    let mk_remove_modifier use_new fn_id match_expr_f modifier_cmds =
      let body_f (existing_id, (existing_expr, existing_decls)) (new_id, (new_expr, new_decls)) =
        let id_of_var e = List.hd @: var_ids_of_expr e in
        let cur_id, _, _, cur_var = mk_itmv "cur_it" cit_t in
        let cmds_f it_var = modifier_cmds (id_of_var it_var) it_var cur_id cur_var
                              (if use_new then Some(new_expr) else None) in
        let match_expr, match_decls = match_expr_f ()
        in existing_decls@(if use_new then new_decls else [])@match_decls
           @(mk_apply_first_match cit_t match_expr cmds_f)
      in mk_remove_modifier_common use_new fn_id body_f
    in

    let mk_remove_many_modifier use_new fn_id modifier_cmds =
      let body_f (existing_id, (existing_expr, existing_decls)) (new_id, (new_expr, new_decls)) =
        let cmds it_var end_var =
          let id_of_var e = List.hd @: var_ids_of_expr e in
          let cur_id, _, _, cur_var = mk_itmv "cur_it" cit_t in
          let a = [mk_assign_from_fn cit_meta (id_of_var it_var) (Named "find") [it_var; end_var; existing_expr]] in
          let t = mk_op bool_meta Neq [it_var; end_var]
          in a, t, modifier_cmds a (id_of_var it_var) it_var cur_id cur_var
                    (if use_new then Some(new_expr) else None)
        in
        let key_expr, key_decls = if use_new then mk_key_of_tuple old_arg_var
                                  else key_expr, key_decls
        in existing_decls@(if use_new then new_decls else [])@key_decls
          @(mk_apply_match_while_loop cit_t key_expr cmds)
      in mk_remove_modifier_common use_new fn_id body_f
    in

    (* TODO: for fun deps (i.e. 1-1 deps):
     * -- skip bulk operations (i.e. "all" version of methods)
     * -- return singletons from slice method *)

    (* An insert function that accepts a full tuple, and inserts as a key-value pair *)
    let insert_fn = if ce_t = te_t || not primary then []
                    else mk_insert_fn mk_elem arg_id arg_t arg_var  in

    (* A delete function that accepts a full tuple and erases any complete matches,
     * that is, matches over both key attributes and non-key attributes.
     * For multi-containers (e.g., multisets or multimaps), this is a bulk operation.
     * Body implementation notes:
     * 1. first statement constructs an element type. Change to use tuple->element constructor.
     * 2. second statement constructs a matching loop. This makes a key, and iterates.
     *    The iterator element should be the element type of the specialization.
     * 3. Requires underlying collection to provide an erase method that returns an iterator
     *    to the next valid element after the deletion.
     *)
    (* TODO: check hint position of iterator *)
    let mk_delete_first_fn fn_id =
      let match_expr_f () = if primary then key_expr, key_decls else mk_key_of_index_tuple key_var in
      let erase_modifier_cmds _ it_var _ _ _ = [mk_fn_call_expr unit_meta "erase" [it_var]] in
         (mk_remove_modifier false fn_id match_expr_f erase_modifier_cmds)
    in
    let primary_delete_fn =
      if ce_t = te_t || not primary then [] else
      let erase_all_modifier_cmds step_cmd it_id it_var cur_id cur_var _ =
        let id_of_var e = List.hd @: var_ids_of_expr e in
        if is_boost_collection then
          [mk_assign_from_fn cit_meta (id_of_var it_var) (Named "erase") [it_var]]@step_cmd
        else
          [mk_init_decl cur_id cit_t it_var;
           mk_effect_expr (FExt IteratorIncrement) [it_var];
           mk_fn_call_expr unit_meta "erase" [cur_var]]@step_cmd
      in
         (mk_delete_first_fn "erase")
        @(mk_remove_many_modifier false "erase_all" erase_all_modifier_cmds)
    in

    (* A delete function that erases a key (i.e., a supertype of the value type)
     * For multi-containers (e.g., multisets or multimaps), this is a bulk operation. *)
    let index_delete_fn =
      if primary then [] else
      let key_expr, key_decls = mk_key_of_index_tuple key_var in
      let erase_body = key_decls@[mk_idx_effect_expr unit_meta "erase" [key_expr]] in
      let all_fn = [DFn("erase_"^pos_str^"_all", [key_arg_id, tk_t], unit_t, erase_body)]
      in (mk_delete_first_fn ("erase_"^pos_str))@all_fn
    in

    (* An update function that accepts a full new tuple, and updates the entry
     * matching the complete value, that is, matches over both key attributes and
     * non-key attributes, to the new value.
     * For multi-containers (e.g., multisets or multimaps), this is a bulk operation.
     * Body implementation notes.
     * Similar to the delete function, with additional requirements on
     * either a replace method, or a pair of insert and erase methods.
     * 1. The declarations below construct element types, one for the old element,
     *    and one for the new.
     * 2. The replace method takes an iterator indicating the element to replace
     *    and the new element var.
     * 3. The erase method takes an iterator indicating the element to remove, and the
     *    insert method takes an iterator as a position hint and the new element var.
     *)
    (* TODO: check hint position of iterator *)
    let update_modifier_common _ it_var cur_id cur_var new_expr_opt =
      let new_expr = match new_expr_opt with Some(e) -> e
        | None -> failwith "invalid new expression in update modifier"
      in
      if is_boost_collection then [mk_fn_call_expr unit_meta "replace" [it_var; new_expr]]
      else [mk_init_decl cur_id cit_t it_var;
            mk_effect_expr (FExt IteratorIncrement) [it_var];
            mk_fn_call_expr unit_meta "erase" [cur_var];
            mk_fn_call_expr unit_meta "insert" [it_var; new_expr]]
    in
    let primary_update_fn =
      if not primary then [] else
      let match_expr_f () = mk_key_of_tuple old_arg_var in
      let update_all_modifier_cmds step_cmd it_id it_var cur_id cur_var new_expr_opt =
        (update_modifier_common it_id it_var cur_id cur_var new_expr_opt)@step_cmd
      in
         (mk_remove_modifier true "update" match_expr_f update_modifier_common)
        @(mk_remove_many_modifier true "update_all" update_all_modifier_cmds)
    in

    (* An update function that accepts a full new tuple, and updates the entry
     * with the given key to the new value. This does not need the old value.
     * For multi-containers (e.g., multisets or multimaps), this is a bulk operation. *)
    let index_update_fn =
      let elem_expr, elem_decls = mk_elem "replace_elem" arg_var in
      let update_cmds it_var =
        let f = if primary then mk_fn_call_expr else mk_idx_effect_expr
        in [f unit_meta "replace" [it_var; elem_expr]]
      in
      let update_body_f cmds_f = elem_decls@key_decls@(cmds_f cit_t key_expr update_cmds) in
        [DFn("update_"^pos_str, [arg_id, arg_t], unit_t, (update_body_f mk_apply_first_match));
         DFn("update_"^pos_str^"_all", [arg_id, arg_t], unit_t, (update_body_f mk_apply_match_for_loop))]
    in

    (* A slice function that accepts a full or partial tuple and returns a collection. *)
    (* Body implementation notes:
     * 1. Declares an index value initialized from the argument tuple for comparisons.
     * 2. Declares an empty slice collection for the return value.
     * 3. Populates the return value by looping and matching.
     *)
    let slice_fn =
      let ret_id, ret_t, ret_meta, ret_var = mk_itmv "r" ct in
      if primary then
        let slice_cmds it_var =
          (* Element equality should compare value part first for short-circuting. *)
          let elem_expr = mk_fn arg_meta (FExt IteratorElement) [it_var] in
          let pred, pred_decls =
            let elem_val_expr, elem_val_decls = mk_val_of_elem elem_expr
            in mk_op bool_meta Eq [val_expr; elem_val_expr], elem_val_decls
          in
          pred_decls@[mk_if pred (mk_effect_expr (Collection Insert) [ret_var; elem_expr])]
        in
        let body = key_decls@val_decls
                  @[mk_empty_decl ret_id ret_t]
                  @(mk_apply_match_for_loop cit_t key_expr slice_cmds)
                  @[mk_return unit_meta ret_var]
        in [DFn("slice", [arg_id, arg_t], ct, body)]
      else
        let key_expr, key_decls = mk_key_of_index_tuple key_var in
        let slice_cmds it_var end_var =
          [mk_effect_expr (Named "copy")
            [it_var; end_var; U.mk_fn cit_meta (Collection (CFExt EndIterator)) [ret_var]]]
        in
        let body = key_decls
                  @[mk_empty_decl ret_id ret_t]
                  @(mk_apply_match_range cit_t key_expr slice_cmds)
                  @[mk_return unit_meta ret_var]
        in [DFn("slice_"^pos_str, [key_arg_id, tk_t], ct, body)]
    in
    List.map (fun d -> d, unit_meta)
      (List.flatten [insert_fn; primary_delete_fn; index_delete_fn;
                                primary_update_fn; index_update_fn; slice_fn])
  in

  match deps, is_dependence_free with
  | _, true -> generate_dependence_free_methods ()
  | Some(d), false -> generate_dependency_methods d
  | _, _ -> failwith "internal error on wrapping collection"


(* Generic collection type specialization.
 * Handles nested annotated collections by extracting annotations directly
 * associated with collection types *)
and type_declarations_of_collection id counter t =
  let unit_meta = unit_t, [] in
  match element_of_collection t with
  | Some (collection_t, element_vt) ->
    let indexes = match t with
      | TInternal(TValue vt) -> indexes_of_annotations (annotation_of vt)
      | _ -> None, []
    in
    (* Call to composite_decl_of_type can recursively enter this
     * function to deeply specialize a nested collection. *)
    let nested, new_counter, t_decl, aux_decls =
      match composite_decl_of_type id (counter+1) element_vt with
      | x, (TComposite(tl) as y), z ->
        List.exists is_external_type (List.map snd tl), x, y, z
      | x, y, z -> false, x, y, z
    in
    begin match nested, indexes with
    | false, (None, []) -> counter, t, []

    (* Directly specialize a single index request to the appropriate data structure *)
    | false, (Some(uniq, ord, pos, dep), []) ->
      let ct, et, decls = type_of_single_dependency_collection element_vt uniq ord pos dep in
      let rt, rdecls =
        let tid = "c_"^id in
        let members = wrap_collection true (t, iv_type element_vt) (ct, et, None) (uniq, ord, pos, dep) in
        TNamed tid, decls@[DClass(tid, Some(ct), members), unit_meta]
      in counter, rt, rdecls

    (* Data structures with multiple index requirements, or with nested specialized
     * collections are specialized as Boost multi_indexes *)
    | _, _ ->
      let ct_id, ct_elem_id, ct, c_elem_t, wct_id =
        let prefix = id^"__d"^(string_of_int counter) in
        let x,y = prefix^"_bmi", prefix^"_bmi_elem" in
        let nid = "c_"^id^(if counter > 0 then "_d"^(string_of_int counter) else "")
        in x, y, TNamed x, TNamed y, nid
      in
      begin match t_decl with
      | TComposite(tl) ->
        let coll_elem_and_decl = ct, c_elem_t, Some(t_decl) in
        let bmi_idx = bmi_indexes collection_t t_decl c_elem_t indexes in
        let ct_decl = TExtDecl(TBoostMultiIndex(c_elem_t, bmi_idx)) in
        let decls = aux_decls@[DType(ct_elem_id, t_decl), unit_meta; DType(ct_id, ct_decl), unit_meta] in
        let wrap_f primary = wrap_collection primary (t, iv_type element_vt) coll_elem_and_decl in
        let rt, rdecls =
          let p_members = match fst indexes with
            | None -> [] (* TODO: non-annotated collections with a nested and annotated collection
                            must still generate a wrapper interface *)
            | Some(uniq,ord,pos,dep) -> wrap_f true (uniq, ord, pos, dep)
          in
          let members = p_members@(List.flatten (List.map (wrap_f false) (snd indexes)))
          in TNamed wct_id, decls@[DClass(wct_id, Some(ct), members), unit_meta]
        in new_counter, rt, rdecls

      | _ -> new_counter, t, aux_decls
      end
    end

  | _ -> counter, t, []


(* Declares a collection as a Boost Multi-Index or an associative map.
 * We assume annotation checking has been performed. *)
(* TODO: we should also convert constructors or initalizers as appropriate here *)
let datastructure_of_collection id t da_opt (dt, da) =
  match type_declarations_of_collection id 0 t with
  | _, TNamed(nt_id), t_decls ->
    let nc_decl = DVar(id, TNamed(nt_id), da_opt) in
    let nbmi_bindings = [id, TNamed(nt_id)] in
    t_decls, [nc_decl, (dt,da)], nbmi_bindings

  | _, nt, t_decls when nt <> t ->
    t_decls, [DVar(id, nt, da_opt), (dt,da)], [id, nt]

  | _, _, t_decls ->  [], t_decls, []


(* Substitutes global variable definitions annotated with indexing
 * requirements to be Boost Multi-Indexes.
 * Returns a new program, as well as bindings for substituted data structures *)
let specialize_datastructures program =
  let is_var_decl (d,_) = match d with DVar _ -> true | _ -> false in
  let split_decls decls =
    let x,y = List.partition is_var_decl decls in
    (List.map (fun (d,(t,a)) -> mk_decl (t,a) d) x), y
  in
  let rec specialize_cmd (decls, cmds, bindings) cmd =
    let rebuild_cmd cmd_f cmd_l =
      let sub_decls, cmds_per_sub, nb =
        List.fold_left (fun (decl_acc, cmds_per_sub, b) sub_cmd ->
            let ndecls, nsub_cmds, nb = specialize_cmd ([], [], b) sub_cmd in
            let var_decls, rest_decls = split_decls ndecls in
            (decl_acc@rest_decls), cmds_per_sub@[var_decls@nsub_cmds], nb
          ) ([], [], bindings) cmd_l
      in (decls@sub_decls), (cmds@(cmd_f cmds_per_sub)), nb
    in
    match tag_of_cmd cmd with
    | Decl d ->
      let ntdecls, ndecls, nb = specialize_decl ([], [], bindings) (d, meta_of_cmd cmd) in
      let var_decls, rest_decls = split_decls ndecls in
        (decls@ntdecls@rest_decls), (cmds@var_decls), nb

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

  (* TODO: prune duplicate BMI type declarations *)
  and specialize_decl (tdecls,nprog,bindings) (d,(dt,da)) = match d with
    | DVar (id,t,da_opt) ->
      begin match datastructure_of_collection id t da_opt (dt,da) with
        | x, [], [] -> tdecls@x, nprog@[d,(dt,da)], bindings
        | x,y,z -> tdecls@x, nprog@y, bindings@z
      end

    (* TODO: specialize argument data structures based on their annotations *)
    | DFn (id,arg,ret_t,body) ->
      let ntdecls, nbody, nbindings = List.fold_left specialize_cmd ([], [], bindings) body
      in tdecls@ntdecls, nprog@[DFn (id,arg,ret_t,nbody), (dt,da)], nbindings

    | DClass (id, parent_opt, decls) ->
      let ntdecls, nbody, nbindings = List.fold_left specialize_decl ([], [], bindings) decls
      in tdecls@ntdecls, nprog@[DClass (id, parent_opt, nbody), (dt,da)], nbindings

    | _ -> tdecls, nprog@[d,(dt,da)], bindings
  in
  let rec specialize_program prog =
    let rcr = specialize_program in
    List.fold_left (fun (cacc, bacc) c -> match c with
      | Include (name, None, _, _) -> cacc@[c], bacc
      | Include (name, Some(p), code, expected) ->
        let x,y = rcr p in cacc@[Include (name, Some(x), code, expected)], bacc@y
      | Component decls ->
        let x,y,z = List.fold_left specialize_decl ([], [], bacc) decls in
        let decl_prog =
          if x = [] then []
          else [Include("k3_decls.hpp", Some([Component x]), None, false)]
        in cacc@decl_prog@[Component y], z
    ) ([],[]) prog
  in
  let nprog, bindings = specialize_program program in
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
      let mk_it_dv iterator_id end_iterator_id expr =
        let it_type, it_meta = let t = TExt (TIterator (type_of_expr expr)) in t, meta t in
        let it_init, end_init =
          Some(Init(U.mk_fn it_meta (Collection (CFExt BeginIterator)) [expr])),
          Some(Init(U.mk_fn it_meta (Collection (CFExt EndIterator)) [expr]))
        in
        let iterator_decl = U.mk_var_decl iterator_id it_type it_init in
        let end_iterator_decl = U.mk_var_decl end_iterator_id it_type end_init in
          [U.mk_decl unit_meta iterator_decl;
           U.mk_decl unit_meta end_iterator_decl],
          U.mk_var it_meta iterator_id, U.mk_var it_meta end_iterator_id
      in
      let iterator_decls, iterator, end_iterator = match tag_of_expr coll_expr with
        | Var(id) ->
          let iterator_id, end_iterator_id = "it_"^id, "end_"^id
          in mk_it_dv iterator_id end_iterator_id coll_expr

        (* TODO: this causes repeated evaluation of coll_expr while declaraing
         * iterators. This should be lifted and reified. *)
        | _ -> mk_it_dv "c_it" "c_end" coll_expr
      in
      let test_expr = U.mk_op bool_meta Neq [iterator; end_iterator] in
      let advance_expr = U.mk_fn unit_meta (FExt IteratorIncrement) [iterator] in
      let loop_body_decls =
        let lv_init_expr = mk_fn (meta loop_var_t) (FExt IteratorElement) [iterator]
        in [U.mk_decl unit_meta (U.mk_var_decl loop_var loop_var_t (Some (Init lv_init_expr)))]
      in
      let for_tag = CExt (For (None, Some(test_expr), Some(advance_expr)))
      in iterator_decls@[U.mk_cmd for_tag unit_meta (loop_body_decls@children)]
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
  cpp_rewrite mk_meta (fst (specialize_datastructures program))
