open K3
open K3Annotations
open K3Typechecker

open CPP
open CPP.CPPTarget
open CPP.CPPTarget.ASTImport

let composite_field_prefix = "__a"

(* Creates a composite type declaration from a K3 base type, e.g. for element
 * types of Boost Multi-Indexes *)
let composite_decl_of_type t =
  let fields_of_list l = snd (List.fold_left (fun (i,acc) t ->
    i+1, acc@[composite_field_prefix^(string_of_int i), TInternal(TValue(t))]) (0,[]) l)
  in
  match t with
  | TUnit | TUnknown | TAddress | TTarget _ ->
    failwith "invalid argument for composite type construction"

  | TCollection _ -> failwith "nested composite typed not supported"

  | TTuple vtl -> TComposite(fields_of_list vtl)
  | _ -> TComposite([composite_field_prefix^"0", TInternal(TValue(canonical t))])

(* Creates Boost Multi-Index extractors for index positions *)
let bmi_indexes collection_t element_decl_t (pidx_pos, sidx_pos) =
  let extractor_of_position element_decl_t i = 
    let id,t = match element_decl_t with
      | TComposite fields_t -> List.nth fields_t i 
      | _ -> failwith "invalid element type for Boost Multi Index"
    in BMIMember (t,id)
  in 
  let extractor_of_positions element_decl_t pos = match pos with
    | [] -> failwith "invalid empty index positions"
    | [i] -> extractor_of_position element_decl_t i
    | _ -> BMIComposite (List.map (extractor_of_position element_decl_t) pos)
  in
  let mk_bmi_index unique sorted pos = 
    let idx_id = String.concat "" (List.map string_of_int pos) in
    idx_id, unique, sorted, extractor_of_positions element_decl_t pos
  in
  
  (* TODO: uniqueness and ordering for secondaries *)
  let secondary = List.map (mk_bmi_index false false) sidx_pos in
  
  let primary =
    (* TODO: ordered primaries *)
    match pidx_pos with
    | Some(pos) -> [mk_bmi_index (collection_t = TSet) false pos] | None -> []
  
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

(* Declares a collection as a Boost Multi-Index.
 * We assume annotation checking has been performed. *)
let datastructure_of_collection (new_program, bmi_bindings) (t,anns) id da_opt collection_t element_bt =
  let unit_t = TInternal(TValue(canonical TUnit)) in 
  let indexes = indexes_of_annotations anns in
    match indexes with
    | None, [] -> new_program, bmi_bindings
    | _, _ ->
      let nt_id, nt_elem_id = id^"_bmi", id^"_bmi_elem" in
      let element_t_decl_cmd, element_t_decl =
        let x = composite_decl_of_type element_bt in
        DType(nt_elem_id, x), x
      in
      let bmi_idx = bmi_indexes collection_t element_t_decl indexes in
      let c_t_decl_cmd = DType(nt_id, TExtDecl(TBoostMultiIndex(nt_elem_id, bmi_idx))) in
      let nc_decl = DVar(id, TNamed(nt_id), da_opt) in
      let nbmi_bindings = bmi_bindings@[id, TNamed(nt_id)] in
      [element_t_decl_cmd, (unit_t,[]); c_t_decl_cmd, (unit_t,[]); nc_decl, (t,[])],
      nbmi_bindings

(* Substitutes global variable definitions annotated with indexing
 * requirements to be Boost Multi-Indexes. 
 * Returns a new program, as well as bindings for substituted data structures *)

(* TODO:
 * i. local variables
 * ii. nested collections with inner annotations
 *)
let optimize_datastructures program =
  let element_of_collection t = match t with
    | TInternal(TValue(vt)) -> 
      begin match base_of vt with TCollection (ct,et) -> Some(ct, base_of et) | _ -> None end
    | _ -> None
  in
  let bmi_of_decl (nprog,bindings) (d,(t,m)) = match d with
    | DVar (id,dt,da_opt) ->
      begin match element_of_collection dt with
        | Some (c_t, e_bt) ->
          datastructure_of_collection (nprog,bindings) (t,m) id da_opt c_t e_bt
        | _ -> (nprog@[d,(t,m)],bindings)
      end

    | _ -> (nprog@[d,(t,m)], bindings)
  in 
  let nprog, bindings = List.fold_left bmi_of_decl ([],[]) program in
  (* TODO: redo type inference on nprog *)
  nprog, bindings 

let substitute_loops program = program

let cpp_of_imperative program = 
  fst (optimize_datastructures program) 