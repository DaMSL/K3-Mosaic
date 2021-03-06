open Tree
open Util
open K3.AST
open K3.Annotation

module U = K3Util

(* Annotation manipulation *)
let mk_no_anno a = (a, [])

(* Type manipulation functions ------------- *)

(* the default type *)
let canonical typ = {typ; mut=false; anno=[]}

let preserve_mut t f = {t with typ=f t.typ}

(* change immutable type to mutable *)
let mut t = {t with mut=true}
let immut t = {t with mut=false}

(* A type for simple K3 types *)
let t_bool = canonical TBool
let t_bool_mut = mut t_bool
let t_int = canonical TInt
let t_int_mut = mut t_int
let t_date = canonical TDate
let t_float = canonical TFloat
let t_float_mut = mut t_int
let t_byte = canonical TByte
let t_string = canonical TString
let t_unit = canonical TUnit
let t_unknown = canonical TUnknown
let t_top = canonical TTop
let t_addr = canonical TAddress
let t_addr_mut = mut t_addr
let t_alias id = canonical (TAlias id)

let unit_arg = ["_u", t_unit]
let unknown_arg = ["_", t_unknown]

(* wrap a type in an immutable tuple *)
let wrap_ttuple typ = match typ with
  | []     -> canonical @@ TUnit
  | [h]    -> h
  | h::t   -> canonical @@ TTuple(typ)
let wrap_ttuple_mut typ = mut @@ wrap_ttuple typ

(* general collection wrap function *)
let wrap_tcol tcol typ =
  canonical @@ TCollection(tcol, typ)

(* wrap a type in a list *)
let wrap_tlist typ = wrap_tcol TList typ
let wrap_tlist' tl = wrap_tlist @@ wrap_ttuple tl

(* wrap a type in a set *)
let wrap_tset typ = wrap_tcol TSet typ
let wrap_tset' tl = wrap_tset @@ wrap_ttuple tl

(* wrap a type in a bag *)
let wrap_tbag typ = wrap_tcol TBag typ
let wrap_tbag' tl = wrap_tbag @@ wrap_ttuple tl

(* wrap a type in a map *)
let wrap_tmap typ = wrap_tcol TMap typ
let wrap_tmap' = function
  | [k; v] -> wrap_tmap @@ wrap_ttuple [k; v]
  | _      -> failwith "wrap_tmap': wrong number of arguments"

(* wrap a type in a sorted map *)
let wrap_tsortedmap typ = wrap_tcol TSortedMap typ
let wrap_tsortedmap' = function
  | [k; v] -> wrap_tsortedmap @@ wrap_ttuple [k; v]
  | _      -> failwith "wrap_tsortedmap': wrong number of arguments"

(* wrap a type in an sorted set *)
let wrap_tsortedset typ = wrap_tcol TSortedSet typ
let wrap_tsortedset' tl = wrap_tsortedset @@ wrap_ttuple tl

let wrap_tvector t = wrap_tcol TVector t
let wrap_tvector' t = wrap_tvector (wrap_ttuple t)

let t_bitset = wrap_tcol TBitSet t_int
let t_bool_vector = wrap_tvector t_bool
let t_int_vector = wrap_tvector t_int

(* wrap a type in a vmap *)
let wrap_tvmap ?idx typ = wrap_tcol (TVMap idx) typ
let wrap_tvmap' ?idx tl = wrap_tvmap ?idx @@ wrap_ttuple tl

(* create polyq *)
let wrap_tpolyq tags = wrap_tcol (TPolyQueue (false, tags)) t_unit
let wrap_tuniqpolyq tags = wrap_tcol (TPolyQueue (true, tags)) t_unit

(* what the generic type of data carried around is *)
let wrap_t_calc  = wrap_tvector
let wrap_t_calc' = wrap_tvector'

(* wrap a type in a mutable indirection *)
let wrap_tind t = canonical @@ TIndirect t
let wrap_tind_mut t = mut @@ wrap_tind t

let wrap_tmaybe t = canonical @@ TMaybe t
let wrap_tmaybes ts = List.map wrap_tmaybe ts

let wrap_tupmaybe t = wrap_ttuple [t_bool; t]
let wrap_tupmaybes ts = List.map wrap_tupmaybe ts

let wrap_tfunc tinl tout = canonical @@ TFunction(tinl, immut tout)

(* wrap a function argument *)
let wrap_args id_typ =
  let wrap_args_inner = function
    | ("_",_) -> AIgnored
    | (i,t) -> AVar(i,t)
  in match id_typ with
    | [x]   -> wrap_args_inner x
    | x::xs -> ATuple(List.map wrap_args_inner id_typ)
    | _     -> AVar("_u", t_unit)

(* for deep arguments (using ATuple) *)
let wrap_args_deep id_arg =
  match id_arg with
  | [] | [_] -> wrap_args id_arg
  | _  -> ATuple[wrap_args id_arg]

(* wrap function arguments, turning tmaybes to amaybes *)
let wrap_args_maybe id_typ =
  let wrap_args_inner = function
    | "_", _            -> AIgnored
    | i, {typ=TMaybe t} -> AMaybe(AVar(i,t))
    | i, t              -> AVar(i,t)
  in match id_typ with
    | [x]   -> wrap_args_inner x
    | x::xs -> ATuple(List.map wrap_args_inner id_typ)
    | _     -> invalid_arg "No ids, types for wrap_args_maybe"

(* Unwrap functions for types *)

(* returns mutability and type *)
let unwrap_col = function
  | TCollection(t_c, t_e) -> t_c, t_e
  | t -> failwith @@ "Not a collection: "^K3Printing.flat_string_of_base_type t

let unwrap_tcol t = unwrap_col t.typ

let unwrap_tfun t = match t.typ with
  | TFunction(it, ot) -> it, ot
  | _ -> failwith @@ "Not a function: "^K3Printing.flat_string_of_type t

let unwrap_t x = x.typ

(* unwrap a tuple type and return its list. If not a ttuple, return as singleton *)
let unwrap_ttuple vt = match vt.typ with
  | TTuple vt_l -> vt_l
  | x           -> [vt]

(* unwrap an indirection type *)
let unwrap_tind t = match t.typ with
  | TIndirect vt -> vt
  | _            -> t

let is_tind t = match t.typ with
  | TIndirect _ -> true
  | _           -> false

(* Helper functions to create K3 AST nodes more easily *)

let meta = []   (* we fill meta with a default value *)

let class_id = "K3" (* used for symbol generation *)
let new_num () = Symbols.gen_int_sym class_id

(* function to make a simple tree with no meta or numbering *)
let mk_stree tag children = mk_tree @@ (((new_num (), tag), meta), children)

(* Standard AST nodes *)
let mk_const constant = mk_stree (Const(constant)) []
let mk_cint i = mk_const @@ CInt i
let mk_cfloat f = mk_const @@ CFloat f
let mk_cstring s = mk_const @@ CString s
let mk_cbool b = mk_const @@ CBool b
let mk_ctrue = mk_cbool true
let mk_cfalse = mk_cbool false
let mk_ctarget t = mk_const @@ CTarget t
let mk_cunknown = mk_const CUnknown
let mk_cunit = mk_const CUnit
let mk_caddress a = mk_const @@ CAddress a

let mk_var id = mk_stree (Var(id)) []

let mk_ignore e = mk_stree Ignore [e]

let mk_tuple ?(force=false) items = match items with
  | []  when not force -> mk_cunit
  | [i] when not force -> i
  | _                  -> mk_stree Tuple items

let mk_just x = mk_stree Just [x]

let mk_nothing typ = mk_stree (Nothing typ) []

(* a nothing that wraps in a maybe type *)
let mk_nothing_m typ = mk_nothing @@ wrap_tmaybe typ

let mk_empty val_type = mk_stree (Empty val_type) []

let mk_singleton val_type x = mk_stree (Singleton val_type) [mk_tuple x]

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

(* deep matching on tuples *)
let mk_lambda' argl expr = mk_lambda (wrap_args_deep argl) expr

(* no deep matching. this is needed by higher order functions that don't
 * loop over a data structure directly, such as fold or groupby *)
let mk_lambda'' argl expr = mk_lambda (wrap_args argl) expr

let mk_apply lambda input =
  let input = if input = [] then [mk_cunit] else input in
  mk_stree Apply (lambda :: input)

let mk_apply' fn input = mk_apply (mk_var fn) input

let mk_block = function
  | []  -> mk_cunit
  | [x] -> x
  | xs  -> mk_stree Block xs

let mk_iter iter_fun collection =
    mk_stree Iterate [iter_fun; collection]

let mk_iter' f c = mk_iter f (mk_var c)

let mk_if pred true_exp false_exp =
    mk_stree IfThenElse [pred; true_exp; false_exp]

let mk_if_eq l r yes no = mk_if (mk_eq l r) yes no

let mk_case_sn pred id some none =
    mk_stree (CaseOf id) [pred; some; none]

(* reverse case for when that's easier to do *)
let mk_case_ns pred id none some =
    mk_stree (CaseOf id) [pred; some; none]

let mk_bind pred id expr = mk_stree (BindAs id) [pred; expr]
let mk_deref col = mk_bind col "__x" (mk_var "__x")

let mk_map map_fun collection =
    mk_stree Map [map_fun; collection]

let mk_filter pred_fun collection =
    mk_stree Filter [pred_fun; collection]

let mk_flatten collection = mk_stree Flatten [collection]

let mk_agg agg_fn init collection =
    mk_stree Aggregate [agg_fn; init; collection]

let mk_aggv agg_fn init collection =
    mk_stree AggregateV [agg_fn; init; collection]

let mk_gbagg group_fun agg_fun init collection =
    mk_stree GroupByAggregate [group_fun; agg_fun; init; collection]

let mk_sort compare_fun collection =
  mk_stree Sort [compare_fun; collection]

let mk_equijoin col1 col2 proj1 proj2 agg_fn zero =
  mk_stree Equijoin [col1; col2; proj1; proj2; agg_fn; zero]

let mk_size col = mk_stree Size [col]

let mk_subscript i tuple = mk_stree (Subscript i) [tuple]

let mk_peek col = mk_stree Peek [col]

let mk_peek' col = mk_peek (mk_var col)

let mk_peek_with_vid col lam_none lam_some = mk_stree PeekWithVid [col;lam_none;lam_some]

let mk_is_member col key = mk_stree IsMember [col; key]
let mk_is_member' col key = mk_is_member (mk_var col) key

(* generic version of slice used by multiple functions *)
let mk_slice_gen tag collection pattern =
  (* don't create a slice if we only have unknowns *)
  let pat_l = try U.decompose_tuple pattern
              with Failure _ -> [pattern]
  in
  let all_unknowns = List.for_all
    (fun x -> U.tag_of_expr x = Const(CUnknown)) pat_l
  in
  if all_unknowns then collection
  else
    mk_stree tag [collection; pattern]

let mk_slice collection pattern = mk_slice_gen Slice collection @@ mk_tuple pattern

let mk_slice' collection pattern = mk_slice (mk_var collection) pattern

(* first part of pat contains the vid *)
let mk_slice_lt col pat = mk_slice_gen (SliceOp OLt) col @@ mk_tuple pat
let mk_slice_lt' col pat = mk_slice_lt (mk_var col) pat

let mk_slice_leq col pat = mk_slice_gen (SliceOp OLeq) col @@ mk_tuple pat
let mk_slice_leq' col pat = mk_slice_leq (mk_var col) pat

let mk_slice_gt col pat = mk_slice_gen (SliceOp OGt) col @@ mk_tuple pat
let mk_slice_gt' col pat = mk_slice_gt (mk_var col) pat

let mk_slice_geq col pat = mk_slice_gen (SliceOp OGeq) col @@ mk_tuple pat
let mk_slice_geq' col pat = mk_slice_geq (mk_var col) pat

let mk_error' e = mk_apply' "error" [mk_apply' "print" [e]]
let mk_error s = mk_apply' "error" [mk_apply' "print" [mk_cstring s]]

let _err = mk_lambda' ["_", t_unknown] @@ mk_error "vector: out of bounds!"
let _err_e e =
  mk_lambda' ["_", t_unknown] @@
    mk_apply' "error" [
      mk_apply' "print" [
        mk_apply' "concat"
          [mk_apply' "string_of_int" [e]; mk_cstring " out of bounds!"]
      ]
    ]
let mk_at_with ?error col idx lam =
  let error = maybe (_err_e idx) id_fn error in
  mk_stree AtWith [col; idx; error; lam]

let mk_at_with' ?error col idx lam = mk_at_with ?error (mk_var col) idx lam

let mk_at col idx = mk_stree At [col; idx]
let mk_at' col idx = mk_at (mk_var col) idx

let mk_min_with col lam_none lam_some = mk_stree MinWith [col; lam_none; lam_some]

(* mk a var acces + subscripting *)
let mk_id_path id path =
  List.fold_left (fun acc i -> mk_subscript i acc) (mk_var id) path

let mk_insert ?(path=[]) id x = mk_stree Insert [mk_id_path id path; mk_tuple x]

let mk_insert_at ?(path=[]) id idx x = mk_stree InsertAt [mk_id_path id path; idx; mk_tuple x]

let mk_set_all ?(path=[]) id x = mk_stree SetAll [mk_id_path id path; mk_tuple x]
let mk_set_all_block ?path id x = mk_block [mk_set_all ?path id x; mk_var id]

let mk_extend ?(path=[]) id x = mk_stree Extend [mk_id_path id path; x]

(* key contains dummy value *)
let mk_upsert_with ?(path=[]) id key lam_empty lam_full =
  mk_stree UpsertWith [mk_id_path id path; mk_tuple key; lam_empty; lam_full]

let mk_upsert_with_before ?(path=[]) id key lam_empty lam_full =
  mk_stree UpsertWithBefore [mk_id_path id path; mk_tuple key; lam_empty; lam_full]

let mk_delete ?(path=[]) id x = mk_stree Delete [mk_id_path id path; mk_tuple x]

let mk_delete_block ?path id x = mk_block [mk_delete ?path id x; mk_var id]

let mk_delete_at ?(path=[]) id n = mk_stree DeleteAt [mk_id_path id path; n]

let mk_pop ?(path=[]) id = mk_stree Pop [mk_id_path id path]

(* first part of key contains the vid. Also contains dummy value *)
let mk_delete_prefix ?(path=[]) id x = mk_stree DeletePrefix [mk_id_path id path; mk_tuple x]

let mk_delete_all_prefix ?(path=[]) id x = mk_stree DeleteAllPrefix [mk_id_path id path; x]

(* remove an entry from a data structure (map) *)
let mk_delete_with ?(path=[]) id x lam_none lam_some =
  mk_stree DeleteWith [mk_id_path id path; mk_tuple x; lam_none; lam_some]

let mk_clear_all ?(path=[]) id = mk_stree ClearAll [mk_id_path id path]
let mk_clear_all_block ?path id = mk_block [mk_clear_all ?path id; mk_var id]

let mk_update ?(path=[]) id old_val new_val =
  mk_stree Update [mk_id_path id path; mk_tuple old_val; mk_tuple new_val]

(* first part of new val contains the vid *)
let mk_update_suffix ?(path=[]) id key lambda =
  mk_stree UpdateSuffix [mk_id_path id path; mk_tuple key; lambda]

(* version of upsert_with for vectors *)
let mk_update_at_with ?(path=[]) id key lambda =
  mk_stree UpdateAtWith [mk_id_path id path; key; lambda]

let mk_filter_gt collection filter_val =
  mk_stree (FilterOp OGt) [collection; mk_tuple filter_val]
let mk_filter_gt' col filter_val = mk_filter_gt (mk_var col) filter_val

let mk_filter_geq collection filter_val =
  mk_stree (FilterOp OGeq) [collection; mk_tuple filter_val]
let mk_filter_geq' col filter_val = mk_filter_geq (mk_var col) filter_val

let mk_filter_lt collection filter_val =
  mk_stree (FilterOp OLt) [collection; mk_tuple filter_val]
let mk_filter_lt' col filter_val = mk_filter_lt (mk_var col) filter_val

let mk_filter_leq collection filter_val =
  mk_stree (FilterOp OLeq) [collection; mk_tuple filter_val]
let mk_filter_leq' col filter_val = mk_filter_leq (mk_var col) filter_val

(* handle the common case of updating a peek on a slice *)
let mk_update_slice col slice new_val =
  mk_case_ns
    (mk_peek @@ mk_slice' col slice)
    "__slice"
    mk_cunit @@
    mk_update col [mk_var "__slice"] [new_val]

let mk_ind v = mk_stree Indirect [v]

(* left: TRef, right: T/TRef *)
let mk_assign ?(path=[]) id x = mk_stree Assign [mk_id_path id path; x]

(* target:TTarget(T) address:TAdress args:T *)
let mk_send target address args = mk_stree Send [mk_ctarget target; address; mk_tuple args]

let mk_send_raw target addr args = mk_stree Send [target; addr; args]

(* A let that assigns multiple variables simultaneously.
 * For breaking up tuples and passing multiple values out of functions.
 * var_name_type_list must be a (string, type) list, and the var_values must
 * evaluate to the same types *)
let mk_let var_ids tuple_val expr =
  mk_stree (Let(var_ids)) [tuple_val; expr]

let mk_let_block vars v es = mk_let vars v @@ mk_block es

(* Polyqueue functions *)

(* lambda for these functions takes tag, tuple_idx, tuple_offset *)
(* return the same idx,offset to increase 1, or another to skip *)
let mk_poly_iter idx offset lam col = mk_stree PolyIter [idx; offset; lam; col]
let mk_poly_iter' ?(unique=false) lam =
  let idx, offset, pq = if unique then "uidx", "uoffset", "upoly_queue" else "idx", "offset", "poly_queue" in
  mk_poly_iter (mk_var idx) (mk_var offset) lam (mk_var pq)
let mk_poly_fold lam zero col = mk_stree PolyFold [lam; zero; col]

(* lambda for these functions takes tuple_idx, tuple_offset, elem *)
let mk_poly_iter_tag tag idx offset lam col = mk_stree (PolyIterTag tag) [idx; offset; lam; col]
let mk_poly_iter_tag' ?(unique=false) tag lam =
  let idx, offset, pq = if unique then "uidx", "uoffset", "upoly_queue" else "idx", "offset", "poly_queue" in
  mk_poly_iter_tag tag (mk_var idx) (mk_var offset) lam (mk_var pq)

let mk_poly_fold_tag tag idx offset lam zero col = mk_stree (PolyFoldTag tag) [idx; offset; lam; zero; col]
let mk_poly_fold_tag' ?(unique=false) tag lam zero =
  let idx, offset, pq = if unique then "uidx", "uoffset", "upoly_queue" else "idx", "offset", "poly_queue" in
  mk_poly_fold_tag tag (mk_var idx) (mk_var offset) lam zero (mk_var pq)

let mk_poly_at tag col idx offset = mk_stree (PolyAt tag) [col; idx; offset]
let mk_poly_at' ?(unique=false) tag =
  let idx, offset, pq = if unique then "uidx", "uoffset", "upoly_queue" else "idx", "offset", "poly_queue" in
  mk_stree (PolyAt tag) [mk_var pq; mk_var idx; mk_var offset]

let mk_poly_at_with tag col idx offset lam_none lam = mk_stree (PolyAtWith tag) [col; idx; offset; lam_none; lam]
let mk_poly_at_with' ?(unique=false) tag lam =
  let idx, offset, pq = if unique then "uidx", "uoffset", "upoly_queue" else "idx", "offset", "poly_queue" in
  let lam_none = mk_lambda' unit_arg @@ mk_error "oops" in
  mk_poly_at_with tag (mk_var pq) (mk_var idx) (mk_var offset) lam_none lam

let mk_poly_insert ?(path=[]) tag col elem = mk_stree (PolyInsert tag) [mk_id_path col path; mk_tuple elem]
let mk_poly_insert_block ?path tag col elem = mk_block [ mk_poly_insert ?path tag col elem; mk_var col]

let mk_poly_tag_at col idx = mk_stree PolyTagAt [col; idx]

(* skip to the next entry. Returns a tuple of idx, off *)
let mk_poly_skip tag col idx off = mk_stree (PolySkip(false, tag)) [col; idx; off]
let mk_poly_skip' ?(unique=false) tag =
  let idx, offset, pq = if unique then "uidx", "uoffset", "upoly_queue" else "idx", "offset", "poly_queue" in
  mk_poly_skip tag (mk_var pq) (mk_var idx) (mk_var offset)

(* do the poly skip and let bind the new values *)
let mk_poly_skip_block ?(unique=false) tag es =
  let idx, offset = if unique then "uidx", "uoffset" else "idx", "offset" in
  mk_let [idx; offset] (mk_poly_skip' ~unique tag) @@ mk_block es

(* skip all the tags of this kind *)
let mk_poly_skip_all tag col idx off = mk_stree (PolySkip(true, tag)) [col; idx; off]
let mk_poly_skip_all' ?(unique=false) tag =
  let idx, offset, pq = if unique then "uidx", "uoffset", "upoly_queue" else "idx", "offset", "poly_queue" in
  mk_poly_skip_all tag (mk_var pq) (mk_var idx) (mk_var offset)

let mk_poly_skip_all_block ?(unique=false) tag es =
  let idx, offset = if unique then "uidx", "uoffset" else "idx", "offset" in
  mk_let [idx; offset] (mk_poly_skip_all' ~unique tag) @@ mk_block es

let mk_poly_unpack col = mk_stree PolyUnpack [col]

let mk_poly_reserve ?(path=[]) col num sz_fixed sz_var =
  mk_stree PolyReserve [mk_id_path col path; num; sz_fixed; sz_var]

(* ----- Converting between ocaml lists and k3 containers ----- *)

let rec list_of_k3_container e =
  match U.tag_of_expr e with
  | Combine -> let l, r = U.decompose_combine e in
      list_of_k3_container l @ list_of_k3_container r
  | Empty _ -> []
  | Singleton _ -> [U.decompose_singleton e]
  | _ -> invalid_arg "not a k3 list"

let rec k3_container_of_list typ = function
  | []    -> mk_empty typ
  | [x]   -> mk_singleton typ [x]
  | x::xs -> mk_combine (k3_container_of_list typ [x]) @@
    k3_container_of_list typ xs

(* convenience function to aggregate starting with the first item *)
(* NOTE: will run the first item twice *)
let mk_agg_fst agg_fn col =
  mk_agg agg_fn
    (mk_case_sn (mk_peek col) "__case"
      (mk_var "__case") @@
      mk_apply' "error" [mk_cstring "error with mk_agg_fst"]) col

(* Macros to make role related stuff *)
let mk_const_stream id typ (l:expr_t list) =
    (* copy from K3Util to prevent circularity *)
    mk_no_anno @@
    Source(Resource(id, Stream(typ,
        ConstStream(k3_container_of_list (wrap_tlist typ) l))))

let mk_random_stream id typ len = mk_no_anno @@
    Source(Resource(id, Stream(typ, RandomStream len)))

let mk_file_handle id typ path ?(is_json=false) is_sink =
    let t = Resource(id, Handle(typ, File path,
                if is_json then JSON else CSV))
    in if is_sink then mk_no_anno @@ Sink t
       else mk_no_anno @@ Source t

let mk_net_handle id typ addr ?(is_json=false) is_sink =
    let t = Resource(id, Handle(typ, Network addr, if is_json then JSON else CSV))
    in if is_sink then mk_no_anno @@ Sink t
       else mk_no_anno @@ Source t

let mk_bind_role id1 id2 = mk_no_anno @@ BindFlow(id1, id2)

let mk_consume id = mk_no_anno @@ Instruction(Consume id)

let mk_role id flowprog = mk_no_anno @@ Role(id, flowprog)

let mk_typedef id t = mk_no_anno @@ Typedef(id, t)


(* Macros to do more complex tasks ---- *)

(* function to take a list of names and convert to K3 variables *)
(* "_" translates to CUnknown *)
let ids_to_vars = List.map (function
  | "_" -> mk_cunknown
  | x   -> mk_var x)

let ids_to_vars' l = ids_to_vars @@ fst_many l

let vars_to_ids = List.map (fun x -> match U.tag_of_expr x with
  | Const(CUnknown) -> "_"
  | Var id          -> id
  | _               -> failwith "Not a var or unknown")

(* check if a collection is empty *)
let mk_is_empty collection ~y ~n =
  mk_if (mk_eq (mk_cint 0) (mk_size collection)) y n

(* checks if a member of a collection is present *)
let mk_has_member collection pattern typ =
  mk_neq
    (mk_slice collection pattern)
    (mk_empty typ)

let mk_code_sink name args locals code =
  mk_no_anno @@ Sink(Code(name, args, locals, code))

let mk_code_sink' name args locals code = mk_code_sink name (wrap_args_deep args) locals code

let mk_global_fn_raw name input_arg input_types output_types expr =
  mk_no_anno @@
    Global(name,
      wrap_tfunc input_types output_types,
      Some (mk_lambda (input_arg) expr)
    )

(* function to declare and define a global function. Assumes the global
 * construct allows for an expr_t as well.
 * The types are expected in list format (always!)
 * @wr_arg: list of arguments that we write to
*)
let mk_global_fn ?(wr_all=false) ?(wr_arg=[]) name i_ts o_ts expr =
  let o_ts = if null o_ts then [t_unit] else o_ts in
  let anno_t n (i, t) =
    (* check if we specified it as a write arg *)
    if List.mem n wr_arg || wr_all then i, t
    else match t.typ with
      (* don't cause pass by ref on simple types *)
      | TUnit | TInt | TDate | TBool | TFloat | TIndirect _-> i, t
      | _ -> i, {t with anno = Property(false,"CRef")::t.anno}
    in
  let i_ts = if i_ts = [] then ["_u", t_unit] else List.mapi anno_t i_ts in
  mk_global_fn_raw name
    (wrap_args i_ts)
    (snd_many i_ts)
    (wrap_ttuple o_ts)
    expr

let mk_global_val name val_type =
  mk_no_anno @@ Global(name, val_type, None)

let mk_global_val_init name val_type e =
  mk_no_anno @@ Global(name, val_type, Some e)

let mk_foreign_short name t = mk_no_anno @@ Foreign(name, t)

let mk_foreign_fn name input_types output_types =
  mk_foreign_short name @@ wrap_tfunc input_types output_types

let mk_flow stmt_list = mk_no_anno @@ Flow(stmt_list)

(* a lambda with 2 arguments for things like aggregation functions *)
let mk_assoc_lambda arg1 arg2 expr = mk_lambda (ATuple[arg1; arg2]) expr

let mk_assoc_lambda' arg1 arg2 expr = mk_lambda (ATuple[wrap_args arg1; wrap_args arg2]) expr

let mk_lambda2 arg1 arg2 expr = mk_lambda (ATuple[arg1; arg2]) expr
let mk_lambda2' a1 a2 expr = mk_lambda2 (wrap_args a1) (wrap_args a2) expr
let mk_lambda2'' a1 a2 expr = mk_lambda (ATuple[ATuple[wrap_args a1;wrap_args a2]]) expr

let wrap_atuple = function
  | []  -> failwith "nothing to wrap"
  | [x] -> x
  | xs  -> ATuple xs

let mk_lambda2d' a1 a2 expr =
  mk_lambda2 (wrap_atuple @@ List.map wrap_args a1) (wrap_atuple @@ List.map wrap_args a2) expr

let mk_lambda3 arg1 arg2 arg3 expr = mk_lambda (ATuple[arg1; arg2; arg3]) expr
let mk_lambda3' a1 a2 a3 expr = mk_lambda3 (wrap_args a1) (wrap_args a2) (wrap_args a3) expr

let mk_lambda4 a1 a2 a3 a4 expr = mk_lambda (ATuple[a1; a2; a3; a4]) expr
let mk_lambda4' a1 a2 a3 a4 expr = mk_lambda4 (wrap_args a1) (wrap_args a2) (wrap_args a3) (wrap_args a4) expr

let mk_fst tuple = mk_subscript 1 tuple
let mk_fst' tuple = mk_subscript 1 (mk_var tuple)

let mk_snd tuple = mk_subscript 2 tuple
let mk_snd' tuple = mk_subscript 2 (mk_var tuple)

let mk_thd tuple = mk_subscript 3 tuple
let mk_thd' tuple = mk_subscript 3 (mk_var tuple)

(* insert and var block, for usual insertion in a lambda *)
let mk_insert_block ?path id x =
  mk_block [mk_insert ?path id x; mk_var id]

let mk_insert_at_block ?path id idx x =
  mk_block [mk_insert_at ?path id idx x; mk_var id]

let mk_extend_block ?path id col =
  mk_block [mk_extend ?path id col; mk_var id]

let mk_upsert_with_block ?path id key lam_empty lam_full =
  mk_block [mk_upsert_with ?path id key lam_empty lam_full; mk_var id]

let mk_update_at_with_block ?path id key lambda =
  mk_block [mk_update_at_with ?path id key lambda; mk_var id]

let project_from_col tuple_types col ~choice =
  let t_col = wrap_t_calc' [hd tuple_types] in
  mk_agg
    (mk_lambda2' ["acc", t_col] ["x", wrap_ttuple tuple_types] @@
      mk_insert_block "acc" [mk_subscript choice @@ mk_var "x"])
    (mk_empty t_col) @@
    col

let mk_fst_many t col = project_from_col t col ~choice:1

let mk_snd_many t col = project_from_col t col ~choice:2


(* Functions to manipulate tuples in K3 code *)
let def_tup_prefix = "__temp_"

(* convert a number to an id used for breaking apart tuples *)
let int_to_temp_id prefix i = prefix^string_of_int i

let types_to_ids_types ?first prefix types =
  let range = create_corr_range ?first types in
  let ids = List.map (int_to_temp_id prefix) range in
  list_zip ids types

(* break down a tuple into its components, creating ids with a certain prefix *)
let mk_destruct_tuple tup_name types prefix expr =
  let ids_types = types_to_ids_types prefix types in
  mk_let (fst_many ids_types) (mk_var tup_name) expr

(* rebuild a tuple based on the types of the tuple.
 * A lambda allows you to shuffle the ids/types as you wish
 * *)
let mk_rebuild_tuple ?(prefix=def_tup_prefix) tup_name types f =
  let ids_types = types_to_ids_types prefix types in
  mk_let (fst_many ids_types) (mk_var tup_name) @@
    mk_tuple @@ ids_to_vars @@ f @@ fst_many ids_types

(* K3 types for various elements of a k3 program *)
let t_trig_id = t_int (* In K3, triggers are always handled by numerical id *)
let t_stmt_id = t_int
let t_stmt_map_id = t_int
let t_map_id = t_int

(* --- vids --- *)
              (* epoch, count, switch hash *)
let vid_types = [t_int]
let vid_id_t = ["vid", t_int]
let t_vid = wrap_ttuple vid_types

(* increment a vid. assume "vid" *)
let vid_increment ?(vid_expr=mk_var "vid") () =
  mk_tuple [mk_add vid_expr (mk_cint 1)]

let min_vid_k3 = mk_tuple [mk_cint 0]
let sys_init_vid_k3 = mk_tuple [mk_cint 2]
let start_vid_k3 = mk_tuple [mk_cint 4]

(* id function for maps *)
let mk_id tuple_types =
    let prefix = "__id_" in
    let r = create_corr_range @@ tuple_types in
    let ids = List.map (int_to_temp_id prefix) r in
    let ids_types = list_zip ids tuple_types in
    mk_lambda' ids_types @@
      mk_tuple @@ ids_to_vars @@ fst_many @@ ids_types

(* convert an arg to a type *)
let type_of_arg a =
  let rec loop = function
    | AIgnored     -> t_unknown (* who cares *)
    | AVar (_, vt) -> vt
    | AMaybe a     -> wrap_tmaybe @@ loop a
    | ATuple xs    -> wrap_ttuple @@ List.map loop xs
  in match a with
  (* first level is unwrapped *)
  | AIgnored     -> [t_unknown]
  | AVar (_, vt) -> [vt]
  | AMaybe a     -> [wrap_tmaybe @@ loop a]
  | ATuple xs    -> List.map loop xs

let mk_convert_col src_t dest_t col =
  if src_t = dest_t then col else
  let t_c, t_elem = unwrap_tcol src_t in
  mk_agg
    (mk_lambda''
      ["acc_conv", dest_t; "x", t_elem] @@
      mk_insert_block "acc_conv" [mk_var "x"])
    (mk_empty dest_t)
    col

(* supply only the col part of the dst type *)
let mk_convert_col_dst dst_t src_col_t col =
  let t_c, t_elem = unwrap_tcol dst_t in
  mk_convert_col {dst_t with typ=TCollection(src_col_t, t_elem)} dst_t col

let mk_convert_col' src_t dest_col_t col =
  let t_c, t_elem = unwrap_tcol src_t in
  mk_convert_col src_t {src_t with typ=TCollection(dest_col_t, t_elem)} col

let mk_peek_or_zero ?(zero=(mk_cint 0)) e =
  mk_case_ns (mk_peek e) "x"
    zero (mk_var "x")

let mk_peek_or_error s e = mk_case_ns (mk_peek e) "x"
  (mk_error s) @@
  mk_var "x"

let mk_lookup col pat = mk_peek @@ mk_slice col pat
let mk_lookup' col pat = mk_peek @@ mk_slice' col pat

let rec default_value_of_t t = match t.typ with
  | TUnit         -> mk_cunit
  | TBool         -> mk_cfalse
  | TInt          -> mk_cint 0
  | TDate         -> mk_cint 0
  | TFloat        -> mk_cfloat 0.
  | TString       -> mk_cstring ""
  | TMaybe t      -> mk_nothing t
  | TTuple l      -> mk_tuple @@ List.map default_value_of_t l
  | TCollection _ -> mk_empty t
  | TAddress      -> mk_caddress ("0.0.0.0", 1)
  | TIndirect t   -> mk_ind @@ default_value_of_t t
  | _ -> failwith "no default value for type"

(* data structure record to standardize manipulation *)
type data_struct = { id: string;
                     e: (string * type_t) list;
                     ee: (string * type_t) list list;
                     t: type_t;
                     init: expr_t option;
                     (* init that isn't used right away *)
                     d_init:expr_t option;
                     map_id: int option;
                     global: bool; (* real global ds *)
                     vid: bool; (* contains vids *)
                   }


(* also add default values if missing *)
let create_ds ?(e=[]) ?(ee=[]) ?init ?d_init ?map_id ?(global=false) ?(vid=false) id t =
  (* add default values so we don't have to initialize manually *)
  let init = match init with
    | Some _ as x -> x
    | None -> try some @@ default_value_of_t t
              with Failure _ -> None
  in
  {id; t; e; ee; init; d_init; map_id; global; vid}

(* utility functions *)
let decl_global x =
  let wrap = if x.global then wrap_tind else id_fn in
  match x.init with
  | Some init -> mk_global_val_init x.id (wrap x.t) init
  | None      -> mk_global_val x.id (wrap x.t)

let delayed_init x = match x.d_init with
  | Some init -> mk_assign x.id init
  | None      -> failwith "no delayed init"

(* convenience function to add to ids in names *)
let id_t_add x id_t = List.map (fun (id,t) -> id^x, t) id_t

(* modify e values of id_t format and convert any remaining values to vars *)
let modify_e id_t val_l =
  let map = strmap_of_list val_l in
  (* check for unused *)
  let set = StrSet.of_list @@ fst_many val_l in
  let set, res =
    List.fold_left (fun (acc_set, acc) (s, _) ->
      try
        let acc_set' = StrSet.remove s acc_set in
        acc_set', StrMap.find s map::acc
      with Not_found -> acc_set, mk_var s::acc)
    (set, [])
    id_t
  in
  if StrSet.cardinal set <> 0 then failwith "modify_e: not all changes used"
  else List.rev res

let index_e id_t s =
  List.assoc s @@ insert_index_snd ~first:1 @@ fst_many id_t

(* code to count the size of a collection *)
let mk_size_slow col = mk_size (mk_var col.id)

let mk_min_max v v' v_t comp_fn zero col_e col = mk_agg
  (mk_assoc_lambda' [v, v_t] col_e @@
    mk_if (comp_fn (mk_var v) v')
      (mk_var v) @@
      v')
  zero @@
  mk_var col

(* pop off the front of a list *)
let mk_pop_sim ?cond col_nm bind_nm fail success =
  let action =
    mk_block [
      success;
      (* delete should be last so we can bind by reference *)
      mk_pop col_nm
    ]
  in
  mk_case_ns (mk_peek' col_nm) bind_nm
    fail @@
    match cond with
    | None -> action
    | Some cond -> mk_if cond action mk_cunit

(* increment a stateful variable *)
let mk_incr ?(n=mk_cint 1) nm = mk_assign nm @@ mk_add (mk_var nm) n
let mk_decr ?(n=mk_cint 1) nm = mk_assign nm @@ mk_sub (mk_var nm) n

(* delete one entry in a data structure *)
let mk_delete_one ds slice =
  mk_case_ns (mk_peek @@ mk_slice' ds.id slice) "lookup_data"
    mk_cunit @@
    mk_delete ds.id [mk_var "lookup_data"]

(* for maps *)
(* add a default entry or perform an operation (v) on the slice (nm) *)
let mk_upsert_with_sim ds nm ~k ~default ~v  =
  (* to allow error by default, we special case *)
  let is_error e = try
    "error" = U.decompose_var @@ fst @@ U.decompose_apply e
    with Failure _ -> false
  in
  let check e g = if is_error e then e else g in
  let slice = [mk_tuple k; mk_cunknown] in
  mk_case_ns (mk_peek @@ mk_slice' ds.id slice) nm
    (check default @@ mk_insert ds.id [mk_tuple k; default]) @@
    check v @@ mk_update ds.id [mk_var nm] [mk_tuple k; v]

(* modify (v) the slice (nm) or delete it (delcond) *)
let mk_delete_with_cond ds nm ~k ~delcond ~v  =
  let slice = [mk_tuple k; mk_cunknown] in
  mk_case_sn (mk_peek @@ mk_slice' ds.id slice) nm
    (mk_if delcond
      (mk_delete ds.id [mk_var nm]) @@
      mk_update ds.id [mk_var nm] [mk_tuple k; v])
    mk_cunit

let mk_counter nm = create_ds nm (mut t_int) ~init:(mk_cint 0)

let mk_bool_ds ?(init=mk_cfalse) nm = create_ds nm (mut t_bool) ~init

let mk_barrier ?(args=[]) ?(pre=[]) ?(reusable=false) nm ~ctr ~total ~after =
  mk_code_sink' nm args [] @@
    mk_block @@ pre @ [
      mk_incr ctr;
      mk_if (mk_eq (mk_var ctr) total)
        (* continue with switch init *)
        (mk_block @@
          (if reusable then [mk_assign ctr @@ mk_cint 0] else []) @
          [after])
        mk_cunit
    ]

(* create an id function *)
let mk_id_fn ds = mk_lambda' ["x", wrap_ttuple @@ snd_many ds.e] @@ mk_var "x"

let mk_id_fn' ts = mk_lambda' ["x", wrap_ttuple ts] @@ mk_var "x"

(* case-like structure for tuple 'options' *)
let mk_case_tup pred id ~none ~some =
  mk_if (mk_fst pred)
    (mk_let [id] (mk_snd pred) some)
    none

let mk_case_tup_sn pred id some none = mk_case_tup pred id ~none ~some
let mk_case_tup_ns pred id none some = mk_case_tup pred id ~none ~some

let mk_tup_just x = mk_tuple [mk_ctrue; x]

let mk_tup_nothing typ = mk_tuple [mk_cfalse; default_value_of_t typ]

let mk_is_tup_nothing x = (mk_not @@ mk_fst x)

let mk_filter_cnt cond ds =
  let t = wrap_tbag t_int in
  mk_fst @@
  mk_agg
    (mk_lambda2' ["_acc", t; "_cnt", t_int] ds.e @@
      mk_block [
        mk_if cond (mk_insert "_acc" [mk_var "_cnt"]) mk_cunit;
        mk_tuple [mk_var "_acc"; mk_add (mk_var "_cnt") @@ mk_cint 1]
      ])
    (mk_tuple [mk_empty t; mk_cint 0]) @@
    mk_var ds.id

(* loop over bitmaps as in route and shuffle *)
let mk_iter_bitmap ?(idx="ip") e bitmap =
    mk_iter (mk_lambda' [idx, t_int] e) bitmap

let mk_iter_bitmap' ?idx e bitmap = mk_iter_bitmap ?idx e (mk_var bitmap)

let mk_agg_bitmap ?(idx="ip") ?(move=false) args e zero bitmap =
    (if move then U.add_property "Move" else id_fn) @@
      mk_agg (mk_lambda2' args [idx, t_int] e) zero bitmap

let mk_agg_bitmap' ?idx ?move args e zero bitmap =
  mk_agg_bitmap ?idx ?move args e zero (mk_var bitmap)

(* check for tag validity *)
let mk_check_tag tag col idx offset e =
  mk_if (mk_eq (mk_poly_tag_at col idx) @@ mk_cint tag)
    e
    (mk_error "wrong tag")

let mk_if_tag tag col idx offset e1 e2 =
  mk_if (mk_eq (mk_poly_tag_at col idx) @@ mk_cint tag) e1 e2

let mk_check_tag' ?(unique=false) tag e =
  let idx, offset, pq = if unique then "uidx", "uoffset", "upoly_queue" else "idx", "offset", "poly_queue" in
  mk_check_tag tag (mk_var pq) (mk_var idx) (mk_var offset) e

let mk_if_tag' ?(unique=false) tag e e2 =
  let idx, offset, pq = if unique then "uidx", "uoffset", "upoly_queue" else "idx", "offset", "poly_queue" in
  mk_if_tag tag (mk_var pq) (mk_var idx) (mk_var offset) e e2

(* check if we've reached the end of the poly buffer *)
let mk_if_poly_end_ny ?(unique=false) smaller larger =
  let idx, pq = if unique then "uidx", "upoly_queue" else "idx", "poly_queue" in
  mk_if (mk_lt (mk_var idx) @@ mk_size @@ mk_var pq) smaller larger

let mk_concat e e' = mk_apply' "concat" [e;e']
let mk_print e = mk_apply' "print" [e]
let mk_mod e e' = mk_apply' "mod" [e; e']
let mk_divi e e' = mk_apply' "divi"  [e; e']
let mk_divf e e' = mk_apply' "divf"  [e; e']
let mk_soi e = mk_apply' "string_of_int" [e]
