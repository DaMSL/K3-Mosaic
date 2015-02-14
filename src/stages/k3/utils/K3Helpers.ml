open Tree
open Util
open K3.AST
open K3.Annotation

module U = K3Util

(* Annotation manipulation *)
let mk_no_anno a = (a, [])

let mk_anno_sort (a,annos) xs = (a, annos@[Data(Constraint, Ordered xs)])

(* Type manipulation functions ------------- *)

(* the default type *)
let canonical typ = {typ; mut=false; anno=[]}

let preserve_mut t f = {t with typ=f t.typ}

(* change immutable type to mutable *)
let mut t = {t with mut=true}

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

(* wrap a type in an immutable tuple *)
let wrap_ttuple typ = match typ with
  | [h]    -> h
  | h::t   -> canonical @@ TTuple(typ)
  | _      -> invalid_arg "No tuple to wrap"
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

(* wrap a type in a multimap *)
let wrap_tmmap idxs typ = wrap_tcol (TMultimap idxs) typ
let wrap_tmmap' idxs tl = wrap_tmmap idxs @@ wrap_ttuple tl

(* wrap a type in a mutable indirection *)
let wrap_tind t = canonical @@ TIndirect t
let wrap_tind_mut t = mut @@ wrap_tind t

let wrap_tmaybe t = canonical @@ TMaybe t
let wrap_tmaybes ts = List.map wrap_tmaybe ts

let wrap_tfunc tin tout = canonical @@ TFunction(tin, tout)

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

let mk_tuple ?(force=false) items = match items with
  | [i] when not force -> i
  | i::is -> mk_stree Tuple items
  | _     -> invalid_arg "Nothing to use mk_tuple on"

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

let mk_lambda' argl expr = mk_lambda (wrap_args argl) expr

let mk_apply lambda input = mk_stree Apply [lambda; input]

let mk_apply' fn input = mk_apply (mk_var fn) input

let mk_block statements = mk_stree Block statements

let mk_iter iter_fun collection =
    mk_stree Iterate [iter_fun; collection]

let mk_if pred true_exp false_exp =
    mk_stree IfThenElse [pred; true_exp; false_exp]

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

let mk_gbagg group_fun agg_fun init collection =
    mk_stree GroupByAggregate [group_fun; agg_fun; init; collection]

let mk_sort compare_fun collection =
    mk_stree Sort [compare_fun; collection]

let mk_subscript i tuple = mk_stree (Subscript i) [tuple]

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

(* l_idx is the list of indices to use, made of ocaml ints *)
(* l_comp is the pattern of gt, le, eq expressed as GTA, GT, EQ, LT, LTA *)
let mk_slice_idx ~idx ~comp col pat =
  mk_slice_gen (SliceIdx(idx, comp)) col pat

let mk_slice_idx' ~idx ~comp col pat =
  mk_slice_idx ~idx ~comp col @@ mk_tuple pat

let mk_insert col x = mk_stree (Insert col) [mk_tuple x]

let mk_delete col x = mk_stree (Delete col) [mk_tuple x]

let mk_update col old_val new_val =
    mk_stree (Update col) [mk_tuple old_val; mk_tuple new_val]

let mk_peek col = mk_stree Peek [col]

let mk_peek' col = mk_peek (mk_var col)

(* handle the common case of updating a peek on a slice *)
let mk_update_slice col slice new_val =
  mk_case_ns
    (mk_peek @@ mk_slice' col slice)
    "__slice"
    mk_cunit @@
    mk_update col [mk_var "__slice"] [new_val]

let mk_ind v = mk_stree Indirect [v]

(* left: TRef, right: T/TRef *)
let mk_assign left right = mk_stree (Assign left) [right]

(* target:TTarget(T) address:TAdress args:T *)
let mk_send target address args = mk_stree Send [mk_ctarget target; address; mk_tuple args]

let mk_send_raw target addr args = mk_stree Send [target; addr; args]

(* A let that assigns multiple variables simultaneously.
 * For breaking up tuples and passing multiple values out of functions.
 * var_name_type_list must be a (string, type) list, and the var_values must
 * evaluate to the same types *)
let mk_let var_ids tuple_val expr =
  mk_stree (Let(var_ids)) [tuple_val; expr]

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
      mk_apply' "error" @@ mk_cstring "error with mk_agg_fst") col

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

let mk_role id flowprog = mk_no_anno @@ Role (id, flowprog)


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
  mk_eq collection @@ mk_empty typ

(* checks if a member of a collection is present *)
let mk_has_member collection pattern typ =
  mk_neq
    (mk_slice collection pattern)
    (mk_empty typ)

let mk_code_sink name args locals code =
  mk_no_anno @@ Sink(Code(name, args, locals, code))

let mk_code_sink' name args locals code = mk_code_sink name (wrap_args args) locals code

let mk_global_fn_raw name input_arg input_types output_types expr =
  mk_no_anno @@
    Global(name,
      wrap_tfunc input_types output_types,
      Some (mk_lambda (input_arg) expr)
    )

(* function to declare and define a global function. Assumes the global
 * construct allows for an expr_t as well.
 * The types are expected in list format (always!) *)
let mk_global_fn name input_names_and_types output_types expr =
  let output_types = if null output_types then [t_unit] else output_types in
  mk_global_fn_raw name
    (wrap_args input_names_and_types)
    (wrap_ttuple @@ snd_many input_names_and_types)
    (wrap_ttuple output_types)
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

let mk_fst tuple = mk_subscript 1 tuple
let mk_fst' tuple = mk_subscript 1 (mk_var tuple)

let mk_snd tuple = mk_subscript 2 tuple
let mk_snd' tuple = mk_subscript 2 (mk_var tuple)

let project_from_col tuple_types col ~choice =
  mk_map
    (mk_lambda' ["x", wrap_ttuple tuple_types] @@ mk_subscript choice @@ mk_var "x") @@
    col

let mk_fst_many t col = project_from_col t col ~choice:1

let mk_snd_many t col = project_from_col t col ~choice:2


(* Functions to manipulate tuples in K3 code *)
let def_tup_prefix = "__temp_"

let mk_tuple_range ?(first=0) types = create_range first @@ List.length types

(* convert a number to an id used for breaking apart tuples *)
let int_to_temp_id prefix i = prefix^string_of_int i

let types_to_ids_types ?first prefix types =
  let range = mk_tuple_range ?first types in
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
let t_map_id = t_int

(* --- vids --- *)
              (* epoch, count, switch hash *)
let vid_types = [t_int; t_int]
let vid_id_t = ["epoch", t_int; "count", t_int]
let t_vid = wrap_ttuple vid_types

(* increment a vid. assume "vid" *)
let vid_increment ?(vid_expr=mk_var "vid") () =
  mk_tuple [mk_subscript 1 vid_expr; mk_add (mk_subscript 2 vid_expr) (mk_cint 1)]

let min_vid_k3 = mk_tuple [mk_cint 0; mk_cint 0]

(* id function for maps *)
let mk_id tuple_types =
    let prefix = "__id_" in
    let r = mk_tuple_range @@ tuple_types in
    let ids = List.map (int_to_temp_id prefix) r in
    let ids_types = list_zip ids tuple_types in
    mk_lambda' ids_types @@
      mk_tuple @@ ids_to_vars @@ fst_many @@ ids_types

(* convert an arg to a type *)
let rec type_of_arg = function
  | AIgnored     -> t_unknown (* who cares *)
  | AVar (_, vt) -> vt
  | AMaybe a     -> wrap_tmaybe @@ type_of_arg a
  | ATuple xs    -> wrap_ttuple @@ List.map type_of_arg xs

let mk_convert_col src_t dest_t col =
  let t_c, t_elem = unwrap_tcol src_t in
  mk_agg
    (mk_lambda
      (wrap_args ["acc_conv", dest_t; "x", t_elem]) @@
      mk_combine
          (mk_singleton dest_t [mk_var "x"]) @@
          mk_var "acc_conv")
    (mk_empty dest_t)
    col

(* supply only the col part of the dst type *)
let mk_convert_col' src_t dest_col_t col =
  let t_c, t_elem = unwrap_tcol src_t in
  mk_convert_col src_t {src_t with typ=TCollection(dest_col_t, t_elem)} col

let mk_peek_or_zero e = mk_case_ns (mk_peek e) "x"
  (mk_cint 0) (mk_var "x")

let mk_error s = mk_apply (mk_var "error") @@ mk_cstring s

let mk_peek_or_error s e = mk_case_ns (mk_peek e) "x"
  (mk_error s) @@
  mk_var "x"

(* data structure record to standardize manipulation *)
type data_struct = { id: string;
                     e: (string * type_t) list;
                     t: type_t;
                     init: expr_t option;
                     (* init that isn't used right away *)
                     d_init:expr_t option;
                     map_id: int option;
                   }

let create_ds ?e ?init ?d_init ?map_id id t =
  let e = unwrap_option [] e in
  {id; t; e; init; d_init; map_id}

(* utility functions *)
let decl_global x = match x.init with
  | Some init -> mk_global_val_init x.id x.t init
  | None      -> mk_global_val x.id x.t

let delayed_init x = match x.d_init with
  | Some init -> mk_assign x.id init
  | None      -> failwith "no delayed init"

(* convenience function to add to ids in names *)
let id_t_add x id_t = List.map (fun (id,t) -> id^x, t) id_t

(* modify e values of id_t format and convert any remaining values to vars *)
let modify_e id_t val_l =
  let m = strmap_of_list val_l in
  List.map (fun (s, _) ->
    try StrMap.find s m
    with Not_found -> mk_var s)
  id_t

let index_e id_t s =
  List.assoc s @@ insert_index_snd @@ fst_many id_t

let unit_arg = ["_", t_unit]

(* code to count the size of a collection *)
let mk_size_slow col = mk_agg
  (mk_assoc_lambda' ["count", t_int] col.e @@ mk_add (mk_var "count") @@ mk_cint 1)
  (mk_cint 0) @@
  mk_var col.id

let mk_min_max v v' v_t comp_fn zero col = mk_agg
  (mk_assoc_lambda' [v, v_t] col.e @@
    mk_if (comp_fn (mk_var v) @@ mk_var v')
      (mk_var v) @@
      mk_var v')
  zero @@
  mk_var col.id

(* pop off the front of a list *)
let mk_pop col_nm bind_nm fail success =
  mk_case_ns (mk_peek' col_nm) bind_nm
    fail @@
    mk_block [
      mk_delete col_nm [mk_var bind_nm];
      success
    ]

(* increment a stateful variable *)
let mk_incr nm = mk_assign nm @@ mk_add (mk_var nm) @@ mk_cint 1
let mk_decr nm = mk_assign nm @@ mk_sub (mk_var nm) @@ mk_cint 1

