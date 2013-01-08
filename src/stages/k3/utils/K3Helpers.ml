open Tree
open Util
open K3.AST
open K3.Annotation

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
let t_int = canonical TInt
let t_int_mut = TIsolated(TMutable(TInt,[]))
let t_float = canonical TFloat
let t_float_mut = TIsolated(TMutable(TFloat,[]))
let t_string = canonical TString
let t_unit = canonical TUnit

(* A type for addresses *)
let t_addr = canonical TAddress

(* wrap a type in a list *)
let wrap_tlist typ = 
  let c = iso_to_contained typ in
  canonical @: TCollection(TList, c)

(* wrap a type in a mutable list *)
let wrap_tlist_mut typ = 
  let c = iso_to_contained typ in
  TIsolated(TMutable(TCollection(TList, c),[]))

(* wrap a type in a set *)
let wrap_tset typ = 
  let c = iso_to_contained typ in
  canonical @: TCollection(TSet, c)

(* wrap a type in a mutable set *)
let wrap_tset_mut typ = 
  let c = iso_to_contained typ in
  TIsolated(TMutable(TCollection(TSet, c),[]))

(* wrap a type in a bag *)
let wrap_tbag typ = 
  let c = iso_to_contained typ in
  canonical @: TCollection(TBag, c)

(* wrap a type in a mutable bag *)
let wrap_tbag_mut typ = 
  let c = iso_to_contained typ in
  TIsolated(TMutable(TCollection(TBag, c),[]))

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

let wrap_tmaybe t = canonical @: TMaybe t
let wrap_tmaybes ts = List.map wrap_tmaybe ts

(* wrap a function argument *)
let wrap_args id_typ = 
  let wrap_args_inner = function
    | ("_",_) -> AIgnored
    | (i,t) -> AVar(i,t)
  in match id_typ with
    | [x]   -> wrap_args_inner x
    | x::xs -> ATuple(List.map wrap_args_inner id_typ)
    | _     -> invalid_arg "No ids, types for wrap_args"

(* wrap function arguments, turning tmaybes to amaybes *)
let wrap_args_maybe id_typ = 
  let wrap_args_inner = function
    | ("_",_) -> AIgnored
    | (i, TIsolated(TImmutable(TMaybe(t), _))) -> AMaybe(AVar(i,t))
    | (i, TIsolated(TMutable(TMaybe(t), _))) -> AMaybe(AVar(i,t))
    | (i, TContained(TImmutable(TMaybe(t), _))) -> AMaybe(AVar(i,t))
    | (i, TContained(TMutable(TMaybe(t), _))) -> AMaybe(AVar(i,t))
    | (i,t) -> AVar(i,t)
  in match id_typ with
    | [x]   -> wrap_args_inner x
    | x::xs -> ATuple(List.map wrap_args_inner id_typ)
    | _     -> invalid_arg "No ids, types for wrap_args_maybe"

(* Helper functions to create K3 AST nodes more easily *)

let meta = []   (* we fill meta with a default value *)

let class_id = "K3" (* used for symbol generation *)
let new_num () = Symbols.gen_int_sym class_id 

(* function to make a simple tree with no meta or numbering *)
let mk_stree tag children = mk_tree @: (((new_num (), tag), meta), children)

(* Standard AST nodes *)
let mk_const constant = mk_stree (Const(constant)) []

let mk_var id = mk_stree (Var(id)) []

let mk_tuple items = match items with
  | [i]   -> i
  | i::is -> mk_stree Tuple items
  | _     -> invalid_arg "Nothing to use mk_tuple on"

let mk_just x = mk_stree Just [x]

let mk_empty val_type = mk_stree (Empty(val_type)) []

let mk_singleton val_type x = mk_stree (Singleton(val_type)) [x]

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

let mk_apply lambda input = mk_stree Apply [lambda; input]

let mk_block statements = mk_stree Block statements

let mk_iter iter_fun collection = 
    mk_stree Iterate [iter_fun; collection]

let mk_if pred true_exp false_exp =
    mk_stree IfThenElse [pred; true_exp; false_exp]

let mk_map map_fun collection =
    mk_stree Map [map_fun; collection]

let mk_filtermap pred_fun map_fun collection = 
    mk_stree FilterMap [pred_fun; map_fun; collection]

let mk_flatten collection = mk_stree Flatten [collection]

let mk_agg agg_fun init collection =
    mk_stree Aggregate [agg_fun; init; collection]

let mk_gbagg group_fun agg_fun init collection =
    mk_stree GroupByAggregate [group_fun; agg_fun; init; collection]

let mk_sort collection compare_fun =
    mk_stree Sort [collection; compare_fun]

let mk_slice collection pattern =
    mk_stree Slice [collection; pattern]

let mk_insert collection x = mk_stree Insert [collection; x]

let mk_delete collection x = mk_stree Delete [collection;x]

let mk_update collection old_val new_val =
    mk_stree Update [collection; old_val; new_val]

let mk_peek collection = mk_stree Peek [collection]

(* left: TRef, right: T/TRef *)
let mk_assign left right =
    mk_stree Assign [left; right]
;;

let mk_deref ref =
    mk_stree Deref [ref]
;;

(* target:TTarget(T) address:TAdress args:T *)
let mk_send target address args =
    mk_stree Send [target; address; args]
;;

(* Macros to do more complex tasks ---- *)

(* functions to extract names/types from argument lists (id, type) *)
let extract_arg_types l = List.map (fun (_,typ) -> typ) l

let extract_arg_names l = List.map (fun (nam,_) -> nam) l

(* function to take a list of names and convert to K3 variables *)
(* "_" translates to CUnknown *)
let ids_to_vars = List.map (fun x -> match x with 
  | "_" -> mk_const @: CUnknown 
  | x   -> mk_var x)

(* check if a collection is empty *)
let mk_is_empty collection typ =
  mk_eq collection @: mk_empty typ

(* checks if a member of a collection is present *)
let mk_has_member collection pattern member_type = 
    mk_neg @: mk_eq (mk_slice collection pattern) 
      (mk_empty @: wrap_tlist member_type)

let mk_code_sink name args locals code =
  mk_no_anno @: Sink(Code(name, args, locals, code))

(* function to declare and define a global function. Assumes the global
 * construct allows for an expr_t as well.
 * The types are expected in list format (always!) *)
let mk_global_fn name input_names_and_types output_types expr =
  mk_no_anno @:
    Global(name, 
      TFunction(wrap_ttuple @: extract_arg_types input_names_and_types,
          wrap_ttuple output_types),
      Some (mk_lambda (wrap_args input_names_and_types) expr)
    )
;;

let mk_global_val name val_type = 
  mk_no_anno @: Global(name, TValue(val_type), None)

let mk_foreign_fn name input_types output_types =
  mk_no_anno @: Foreign(name, TFunction(input_types, output_types))

let mk_flow stmt_list = mk_no_anno @: Flow(stmt_list)

(* a lambda with 2 arguments for things like aggregation functions *)
let mk_assoc_lambda arg1 arg2 expr = mk_lambda (ATuple[arg1;arg2]) expr

(* a classic let x = e1 in e2 construct *)
let mk_let var_name var_type var_value expr =
    mk_apply
        (mk_lambda (wrap_args [var_name, var_type])
            (expr)
        )
        (var_value)

(* A let that assigns multiple variables simultaneously. 
 * For breaking up tuples and passing multiple values out of functions.
 * var_name_type_list must be a (string, type) list, and the var_values must
 * evaluate to the same types *)
let mk_let_many var_name_and_type_list var_values expr =
    mk_apply
        (mk_lambda (wrap_args var_name_and_type_list) expr)
        var_values

let mk_fst tuple_types tuple =
    mk_let_many (list_zip ["__fst";"__snd"] tuple_types) tuple (mk_var "__fst")

let mk_snd tuple_types tuple =
    mk_let_many (list_zip ["__fst";"__snd"] tuple_types) tuple (mk_var "__snd")


(* Functions to manipulate tuples in K3 code *)
type tuple_pat = Position of int | ExternVar of id_t | Unknown

let def_tup_prefix = "__temp_"

let mk_tuple_range types = create_range 0 @: List.length types

let tuple_make_pattern (types:value_type_t list) = 
    List.map (fun x -> Position x) (mk_tuple_range types)

(* functions to take and drop from the pattern, filling in unknowns for the
 * values you drop. list_drop and list_take can be used for non-slice operations
 *)
let slice_pat_take num pat =
    let range = create_range 1 (List.length pat - num) in
    let unknowns = List.map (fun _ -> Unknown) range in
    list_take num pat @unknowns

let slice_pat_drop num pat =
    let range = create_range 1 (List.length pat - num) in
    let unknowns = List.map (fun _ -> Unknown) range in
    unknowns@list_drop num pat

(* convert a number to an id used for breaking apart tuples *)
let int_to_temp_id prefix i = prefix^string_of_int i

let tuple_pat_to_ids pat =
    List.map 
    (fun x -> match x with 
      | Position y -> int_to_temp_id def_tup_prefix y
      | ExternVar y -> y
      | Unknown -> "_")
    pat

let types_to_ids_types prefix types =
  let range = mk_tuple_range types in
  let ids = List.map (int_to_temp_id prefix) range in
  list_zip ids types

(* break down a tuple into its components, creating ids with a certain prefix *)
let mk_destruct_tuple tup_name types prefix expr =
  let ids_types = types_to_ids_types prefix types in
  mk_let_many ids_types (mk_var tup_name) expr

(* rebuild a tuple based on the types of the tuple and a pattern of temporaries
 * or external variables
 *)
let mk_rebuild_tuple tup_name types pattern =
  mk_destruct_tuple tup_name types def_tup_prefix
    (mk_tuple @: ids_to_vars @: tuple_pat_to_ids pattern)

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
let t_vid = wrap_ttuple @: [t_int; t_int] (* so we can distinguish *)
let t_vid_mut = wrap_ttuple @: [t_int_mut; t_int_mut]
let t_trig_id = t_int (* In K3, triggers are always handled by numerical id *)
let t_stmt_id = t_int
let t_map_id = t_int

(* id function for maps *)
let mk_id tuple_types = 
    let prefix = "__id_" in
    let r = mk_tuple_range @: tuple_types in
    let ids = List.map (int_to_temp_id prefix) r in
    let ids_types = list_zip ids tuple_types in
    mk_lambda (wrap_args ids_types) @:
      mk_tuple @: ids_to_vars @: extract_arg_names @: ids_types

