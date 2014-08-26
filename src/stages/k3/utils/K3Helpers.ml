open Tree
open Util
open K3.AST
open K3.Annotation

module U = K3Util

(* Annotation manipulation *)
let mk_no_anno a = (a, [])

let mk_anno_sort (a,annos) xs = (a, annos@[Data(Constraint, Ordered xs)])

(* Type manipulation functions ------------- *)

(* convert an isolated value to a contained value all the way down *)
let iso_to_contained typ =
  let rec handle_base_type b = match b with
    | TMaybe(v) -> TMaybe(handle_value_type v)
    | TTuple(vl) -> TTuple(List.map (fun v -> handle_value_type v) vl)
    | TCollection(c, v) -> TCollection(c, handle_value_type v)
    | TTarget(b) -> TTarget(handle_base_type b)
    | x -> x
  and handle_mutable_type m = match m with
    | TMutable(b,a) -> TMutable(handle_base_type b, a)
    | TImmutable(b,a) -> TImmutable(handle_base_type b, a)
  and handle_value_type t = match t with
    | TIsolated(m) -> TContained(handle_mutable_type m)
    | TContained(m) -> TContained(handle_mutable_type m)
  in
  handle_value_type typ

(* the default type *)
let canonical typ = TIsolated(TImmutable(typ,[]))

(* A type for simple K3 types *)
let t_bool = canonical TBool
let t_bool_mut = TIsolated(TMutable(TBool,[]))
let t_int = canonical TInt
let t_int_mut = TIsolated(TMutable(TInt,[]))
let t_date = canonical TDate
let t_float = canonical TFloat
let t_float_mut = TIsolated(TMutable(TFloat,[]))
let t_byte = canonical TByte
let t_string = canonical TString
let t_unit = canonical TUnit
let t_unknown = canonical TUnknown

(* A type for addresses *)
let t_addr = canonical TAddress
let t_addr_mut = TIsolated(TMutable(TAddress,[]))

(* wrap a type in an immutable tuple *)
let wrap_ttuple typ = match typ with
  | [h]    -> h
  | h::t   -> canonical @: TTuple(typ)
  | _      -> invalid_arg "No tuple to wrap"

(* wrap a type in a mutable tuple *)
let wrap_ttuple_mut typ = match typ with
  | [h]    -> h
  | h::t   -> TIsolated(TMutable(TTuple(typ),[]))
  | _      -> invalid_arg "No mutable tuple to wrap"

(* wrap a type in a list *)
let wrap_tlist typ =
  let c = iso_to_contained typ in
  canonical @: TCollection(TList, c)

let wrap_tlist' tl = wrap_tlist @: wrap_ttuple tl

(* wrap a type in a mutable list *)
let wrap_tlist_mut typ =
  let c = iso_to_contained typ in
  TIsolated(TMutable(TCollection(TList, c),[]))

let wrap_tlist_mut' tl = wrap_tlist_mut @: wrap_ttuple tl

(* wrap a type in a set *)
let wrap_tset typ =
  let c = iso_to_contained typ in
  canonical @: TCollection(TSet, c)

let wrap_tset' tl = wrap_tset @: wrap_ttuple tl

(* wrap a type in a mutable set *)
let wrap_tset_mut typ =
  let c = iso_to_contained typ in
  TIsolated(TMutable(TCollection(TSet, c),[]))

let wrap_tset_mut' tl = wrap_tset_mut @: wrap_ttuple tl

(* wrap a type in a bag *)
let wrap_tbag typ =
  let c = iso_to_contained typ in
  canonical @: TCollection(TBag, c)

let wrap_tbag' tl = wrap_tbag @: wrap_ttuple tl

(* wrap a type in a mutable bag *)
let wrap_tbag_mut typ =
  let c = iso_to_contained typ in
  TIsolated(TMutable(TCollection(TBag, c),[]))

let wrap_tbag_mut' tl = wrap_tbag_mut @: wrap_ttuple tl

(* wrap a type in a mutable indirection *)
let wrap_tind t = TIsolated(TImmutable(TIndirect t, []))
let wrap_tind_mut t = TIsolated(TMutable(TIndirect t, []))

let wrap_tmaybe t = canonical @: TMaybe t
let wrap_tmaybes ts = List.map wrap_tmaybe ts

let wrap_tfunc tin tout = TFunction(tin, tout)

(* wrap a function argument *)
let wrap_args id_typ =
  let wrap_args_inner = function
    | ("_",_) -> AIgnored
    | (i,t) -> AVar(i,t)
  in match id_typ with
    | [x]   -> wrap_args_inner x
    | x::xs -> ATuple(List.map wrap_args_inner id_typ)
    | _     -> invalid_arg "No ids, types for wrap_args"

(* for deep arguments (using ATuple) *)
let wrap_args_deep id_arg =
  match id_arg with
  | []  -> invalid_arg "Nothing to wrap in wrap_args_deep"
  | [x] -> x
  | xs  -> ATuple xs

(* wrap function arguments, turning tmaybes to amaybes *)
let wrap_args_maybe id_typ =
  let wrap_args_inner = function
    | ("_",_) -> AIgnored
    | (i, TIsolated(TImmutable(TMaybe(t), _)))
    | (i, TIsolated(TMutable(TMaybe(t), _)))
    | (i, TContained(TImmutable(TMaybe(t), _)))
    | (i, TContained(TMutable(TMaybe(t), _))) -> AMaybe(AVar(i,t))
    | (i,t) -> AVar(i,t)
  in match id_typ with
    | [x]   -> wrap_args_inner x
    | x::xs -> ATuple(List.map wrap_args_inner id_typ)
    | _     -> invalid_arg "No ids, types for wrap_args_maybe"

(* Unwrap functions for types *)

(* returns mutability and type *)
let unwrap_vtype = function
  | TIsolated(TMutable(t,_))
  | TContained(TMutable(t,_))   -> true, t
  | TIsolated(TImmutable(t,_))
  | TContained(TImmutable(t,_)) -> false, t

(* unwrap a tuple type and return its list. If not a ttuple, return as singleton *)
let unwrap_ttuple vt = match snd @: unwrap_vtype vt with
  | TTuple vt_l -> vt_l
  | x           -> [vt]

(* Helper functions to create K3 AST nodes more easily *)

let meta = []   (* we fill meta with a default value *)

let class_id = "K3" (* used for symbol generation *)
let new_num () = Symbols.gen_int_sym class_id

(* function to make a simple tree with no meta or numbering *)
let mk_stree tag children = mk_tree @: (((new_num (), tag), meta), children)

(* Standard AST nodes *)
let mk_const constant = mk_stree (Const(constant)) []
let mk_cint i = mk_const @: CInt i
let mk_cfloat f = mk_const @: CFloat f
let mk_cstring s = mk_const @: CString s
let mk_cbool b = mk_const @: CBool b
let mk_ctarget t = mk_const @: CTarget t
let mk_cunknown = mk_const CUnknown
let mk_cunit = mk_const CUnit
let mk_caddress a = mk_const @: CAddress a

let mk_var id = mk_stree (Var(id)) []

let mk_tuple ?(force=false) items = match items with
  | [i] when not force -> i
  | i::is -> mk_stree Tuple items
  | _     -> invalid_arg "Nothing to use mk_tuple on"

let mk_just x = mk_stree Just [x]

let mk_nothing typ = mk_stree (Nothing typ) []

(* a nothing that wraps in a maybe type *)
let mk_nothing_m typ = mk_nothing @: wrap_tmaybe typ

let mk_empty val_type = mk_stree (Empty val_type) []

let mk_singleton val_type x = mk_stree (Singleton val_type) [x]

let mk_combine x y = mk_stree Combine [x;y]

let mk_range ctype start stride steps =
    mk_stree (Range(ctype)) [start; stride; steps]

let mk_add x y = mk_stree Add [x; y]

let mk_or = mk_add

let mk_mult x y = mk_stree Mult [x; y]

let mk_and = mk_mult

let mk_neg x = mk_stree Neg [x]

let mk_not = mk_neg

let mk_sub x y = mk_add x (mk_neg y)

let mk_eq x y = mk_stree Eq [x; y]

let mk_lt x y = mk_stree Lt [x; y]

let mk_neq left right = mk_stree Neq [left;right]

let mk_leq left right = mk_stree Leq [left;right]

let mk_geq left right = mk_not (mk_lt left right)

let mk_gt left right = mk_not (mk_leq left right)

let mk_lambda argt expr = mk_stree (Lambda(argt)) [expr]

let mk_lambda' argl expr = mk_lambda (wrap_args argl) expr

let mk_apply lambda input = mk_stree Apply [lambda; input]

let mk_block statements = mk_stree Block statements

let mk_iter iter_fun collection =
    mk_stree Iterate [iter_fun; collection]

let mk_if pred true_exp false_exp =
    mk_stree IfThenElse [pred; true_exp; false_exp]

let mk_map map_fun collection =
    mk_stree Map [map_fun; collection]

let mk_filter pred_fun collection =
    mk_stree Filter [pred_fun; collection]

let mk_flatten collection = mk_stree Flatten [collection]

let mk_agg agg_fun init collection =
    mk_stree Aggregate [agg_fun; init; collection]

let mk_gbagg group_fun agg_fun init collection =
    mk_stree GroupByAggregate [group_fun; agg_fun; init; collection]

let mk_sort collection compare_fun =
    mk_stree Sort [collection; compare_fun]

let mk_slice collection pattern =
  (* don't create a slice if we only have unknowns *)
  let pat_l = try U.decompose_tuple pattern
              with Failure _ -> [pattern]
  in
  let all_unknowns = List.for_all
    (fun x -> U.tag_of_expr x = Const(CUnknown)) pat_l
  in
  if all_unknowns then collection
  else mk_stree Slice [collection; pattern]

let mk_slice' collection pattern = mk_slice collection @: mk_tuple pattern

let mk_insert collection x = mk_stree Insert [collection; x]

let mk_delete collection x = mk_stree Delete [collection;x]

let mk_update collection old_val new_val =
    mk_stree Update [collection; old_val; new_val]

let mk_peek collection = mk_stree Peek [collection]

let mk_ind v = mk_stree Indirect [v]

(* left: TRef, right: T/TRef *)
let mk_assign left right = mk_stree Assign [left; right]

let mk_deref ref = mk_stree Deref [ref]

(* target:TTarget(T) address:TAdress args:T *)
let mk_send target address args = mk_stree Send [target; address; args]

(* Macros to make role related stuff *)
let mk_const_stream id typ l =
    (* copy from K3Util to prevent circularity *)
    let rec k3_container_of_list typ = function
    | [] -> mk_empty typ
    | [x] -> mk_singleton typ x
    | x::xs -> mk_combine (k3_container_of_list typ [x]) @:
        k3_container_of_list typ xs
    in
    mk_no_anno @:
    Source(Resource(id, Stream(TValue(typ),
        ConstStream(k3_container_of_list (wrap_tlist typ) l))))

let mk_random_stream id typ len = mk_no_anno @:
    Source(Resource(id, Stream(TValue(typ), RandomStream len)))

let mk_file_handle id typ path ?(is_json=false) is_sink =
    let t = Resource(id, Handle(TValue(typ), File path,
                if is_json then JSON else CSV))
    in if is_sink then mk_no_anno @: Sink t
       else mk_no_anno @: Source t

let mk_net_handle id typ addr ?(is_json=false) is_sink =
    let t = Resource(id, Handle(TValue(typ), Network addr, if is_json then JSON else CSV))
    in if is_sink then mk_no_anno @: Sink t
       else mk_no_anno @: Source t

let mk_bind id1 id2 = mk_no_anno @: Bind(id1, id2)

let mk_consume id = mk_no_anno @: Instruction(Consume id)

let mk_role id flowprog = mk_no_anno @: Role (id, flowprog)


(* Macros to do more complex tasks ---- *)

(* function to take a list of names and convert to K3 variables *)
(* "_" translates to CUnknown *)
let ids_to_vars = List.map (function
  | "_" -> mk_cunknown
  | x   -> mk_var x)

let vars_to_ids = List.map (fun x -> match U.tag_of_expr x with
  | Const(CUnknown) -> "_"
  | Var id          -> id
  | _               -> failwith "Not a var or unknown")

(* check if a collection is empty *)
let mk_is_empty collection typ =
  mk_eq collection @: mk_empty typ

(* checks if a member of a collection is present *)
let mk_has_member collection pattern typ =
  mk_neq
    (mk_slice collection pattern)
    (mk_empty typ)

let mk_has_member' col pat typ = mk_has_member col (mk_tuple pat) typ

let mk_code_sink name args locals code =
  mk_no_anno @: Sink(Code(name, args, locals, code))

let mk_code_sink' name args locals code = mk_code_sink name (wrap_args args) locals code

let mk_global_fn_raw name input_arg input_types output_types expr =
  mk_no_anno @:
    Global(name,
      wrap_tfunc input_types output_types,
      Some (mk_lambda (input_arg) expr)
    )

(* function to declare and define a global function. Assumes the global
 * construct allows for an expr_t as well.
 * The types are expected in list format (always!) *)
let mk_global_fn name input_names_and_types output_types expr =
  mk_global_fn_raw name
    (wrap_args input_names_and_types)
    (wrap_ttuple @: snd_many input_names_and_types)
    (wrap_ttuple output_types)
    expr

let mk_global_val name val_type =
  mk_no_anno @: Global(name, TValue(val_type), None)

let mk_global_val_init name val_type e =
  mk_no_anno @: Global(name, TValue(val_type), Some e)

let mk_foreign_fn name input_types output_types =
  mk_no_anno @: Foreign(name, wrap_tfunc input_types output_types)

let mk_flow stmt_list = mk_no_anno @: Flow(stmt_list)

(* a lambda with 2 arguments for things like aggregation functions *)
let mk_assoc_lambda arg1 arg2 expr = mk_lambda (ATuple[arg1; arg2]) expr

let mk_assoc_lambda' arg1 arg2 expr = mk_lambda (ATuple[wrap_args arg1; wrap_args arg2]) expr

(* A let that assigns multiple variables simultaneously.
 * For breaking up tuples and passing multiple values out of functions.
 * var_name_type_list must be a (string, type) list, and the var_values must
 * evaluate to the same types *)
let mk_let_many var_name_and_type_list var_values expr =
    mk_apply
        (mk_lambda (wrap_args var_name_and_type_list) expr)
        var_values

(* a classic let x = e1 in e2 construct *)
let mk_let var_name var_type var_value expr = mk_let_many [var_name, var_type] var_value expr

(* a let statement with deep argument matching *)
let mk_let_deep args var_values expr =
  mk_apply (mk_lambda args expr) var_values

let mk_let_deep' args var_values expr = mk_let_deep (wrap_args args) var_values expr

let project_from_tuple tuple_types tuple ~total ~choice =
  let l = create_range 1 total in
  let l = List.map (fun i -> "__"^soi i) l in
  let c = "__"^soi choice in
  mk_let_many (list_zip l tuple_types) tuple (mk_var c)

let mk_fst tuple_types tuple =
  project_from_tuple tuple_types tuple ~choice:1 ~total:2

let mk_snd tuple_types tuple =
  project_from_tuple tuple_types tuple ~choice:2 ~total:2

let project_from_col tuple_types col ~total ~choice =
  let l = create_range 1 total in
  let l = List.map (fun i -> "__"^soi i) l in
  let c = "__"^soi choice in
  let id_ts = list_zip l tuple_types in
  mk_map
    (mk_lambda' id_ts @: mk_var c) @:
    col

let mk_fst_many tuple_types collection =
  project_from_col tuple_types collection ~total:2 ~choice:1

let mk_snd_many tuple_types collection =
  project_from_col tuple_types collection ~total:2 ~choice:2


(* Functions to manipulate tuples in K3 code *)
let def_tup_prefix = "__temp_"

let mk_tuple_range ?(first=0) types = create_range first @: List.length types

(* convert a number to an id used for breaking apart tuples *)
let int_to_temp_id prefix i = prefix^string_of_int i

let types_to_ids_types ?first prefix types =
  let range = mk_tuple_range ?first types in
  let ids = List.map (int_to_temp_id prefix) range in
  list_zip ids types

(* break down a tuple into its components, creating ids with a certain prefix *)
let mk_destruct_tuple tup_name types prefix expr =
  let ids_types = types_to_ids_types prefix types in
  mk_let_many ids_types (mk_var tup_name) expr

(* rebuild a tuple based on the types of the tuple.
 * A lambda allows you to shuffle the ids/types as you wish
 * *)
let mk_rebuild_tuple ?(prefix=def_tup_prefix) tup_name types f =
  let ids_types = types_to_ids_types prefix types in
  mk_let_many ids_types (mk_var tup_name) @:
    mk_tuple @: ids_to_vars @: f @: fst_many ids_types

(* unwrap maybe values by creating an inner values with postfix "_unwrap" *)
let mk_unwrap_maybe var_names_and_types expr =
  let unwrap_n_t =
    List.map (fun (n,t) -> (n^"_unwrap",t)) var_names_and_types in
  let names = fst @: List.split var_names_and_types in
  let vars = ids_to_vars names in
  mk_apply
    (mk_lambda (wrap_args_maybe unwrap_n_t) expr) @:
    mk_tuple vars

(* K3 types for various elements of a k3 program *)
let t_trig_id = t_int (* In K3, triggers are always handled by numerical id *)
let t_stmt_id = t_int
let t_map_id = t_int

(* for vids *)
              (* epoch, count, switch hash *)
let vid_types = [t_int; t_int; t_int]
let vid_mut_types = [t_int_mut; t_int_mut; t_int_mut]
let t_vid = wrap_ttuple vid_types
let t_vid_mut = wrap_ttuple vid_mut_types

(* functions for comparing vids *)
(* vid format: (epoch, counter, switch hash) *)
type vid_op = VEq | VNeq | VGt | VLt | VGeq | VLeq
let mk_global_vid_op name tag =
  let lvid_id_t = types_to_ids_types "l" vid_types in
  let lvid_id = fst @: List.split lvid_id_t in
  let rvid_id_t = types_to_ids_types "r" vid_types in
  let rvid_id = fst @: List.split rvid_id_t in
  let arg_pair = wrap_args_deep [wrap_args lvid_id_t; wrap_args rvid_id_t] in
  let arg_types = wrap_ttuple [wrap_ttuple vid_types; wrap_ttuple vid_types] in
  let op f i =
    let nth_l = List.nth lvid_id in let nth_r = List.nth rvid_id in
    f (mk_var @: nth_l i) (mk_var @: nth_r i) in
  let mk_vid_eq = mk_and (op mk_eq 0) @: mk_and (op mk_eq 1) (op mk_eq 2) in
  let mk_vid_neq = mk_not mk_vid_eq in
  let mk_vid_lt = mk_or (op mk_lt 0) @:
                    mk_and (op mk_eq 0) @:
                      mk_or (op mk_lt 1) @:
                        mk_and (op mk_eq 1) (op mk_lt 2) in
  let mk_vid_geq = mk_not mk_vid_lt in
  let mk_vid_gt = mk_or (op mk_gt 0) @:
                    mk_and (op mk_eq 0) @:
                      mk_or (op mk_gt 1) @:
                        mk_and (op mk_eq 1) (op mk_gt 2) in
  let mk_vid_leq = mk_not mk_vid_gt in
  mk_global_fn_raw name arg_pair arg_types t_bool @:
    match tag with
    | VEq -> mk_vid_eq
    | VNeq -> mk_vid_neq
    | VGt -> mk_vid_gt
    | VGeq -> mk_vid_geq
    | VLt -> mk_vid_lt
    | VLeq -> mk_vid_leq

(* id function for maps *)
let mk_id tuple_types =
    let prefix = "__id_" in
    let r = mk_tuple_range @: tuple_types in
    let ids = List.map (int_to_temp_id prefix) r in
    let ids_types = list_zip ids tuple_types in
    mk_lambda' ids_types @:
      mk_tuple @: ids_to_vars @: fst_many @: ids_types

(* ----- Converting between ocaml lists and k3 containers ----- *)

let rec list_of_k3_container e =
  match U.tag_of_expr e with
  | Combine -> let l, r = U.decompose_combine e in
      list_of_k3_container l @ list_of_k3_container r
  | Empty _ -> []
  | Singleton _ -> [U.decompose_singleton e]
  | _ -> invalid_arg "not a k3 list"

let rec k3_container_of_list typ = function
  | [] -> mk_empty typ
  | [x] -> mk_singleton typ x
  | x::xs -> mk_combine (k3_container_of_list typ [x]) @:
    k3_container_of_list typ xs

(* convert an arg to a type *)
let rec value_type_of_arg = function
  | AIgnored     -> t_unknown (* who cares *)
  | AVar (_, vt) -> vt
  | AMaybe a     -> wrap_tmaybe @: value_type_of_arg a
  | ATuple xs    -> wrap_ttuple @: List.map value_type_of_arg xs

