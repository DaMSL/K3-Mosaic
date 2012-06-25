
open K3
open Tree
open Util

(* Type manipulation functions ------------- *)

(* A type for simple immutable integers *)
let t_int = TIsolated(TImmutable(TInt))

(* wrap a type in an immutable list *)
let wrap_tlist typ = 
    let i = match typ with
    | TIsolated(x)  -> TContained(x)
    | TContained(_) -> typ
    in TIsolated(TImmutable(TCollection(TList, i)))

(* wrap a type in an immutable tuple *)
let wrap_ttuple typ = TIsolated(TImmutable(TTuple(typ)))

(* Helper functions to create K3 AST nodes more easily *)

let meta = 0    (* we fill meta with a default value *)
let num = 0

(* function to make a simple tree with no meta or numbering *)
let mk_stree tag children = mk_tree (((num, tag), meta), children)

(* Standard AST nodes *)
let mk_const constant = mk_stree (Const(constant)) []

let mk_var id = mk_stree (Var(id)) []

let mk_tuple items = mk_stree Tuple items

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

let mk_iter collection iter_fun = 
    mk_stree Iterate [collection; iter_fun]

let mk_if pred true_exp false_exp =
    mk_stree IfThenElse [pred; true_exp; false_exp]

let mk_map map_fun collection =
    mk_stree Map [map_fun; collection]

let mk_filtermap pred_fun map_fun collection = 
    mk_stree FilterMap [pred_fun; map_fun; collection]

let mk_flatten collection = mk_stree Flatten [collection]

let mk_agg agg_fun init collection =
    mk_stree Aggregate [agg_fun; init; collection]

let mk_gbagg agg_fun group_fun init collection =
    mk_stree GroupByAggregate [agg_fun; group_fun; init; collection]

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

(* target:TTarget(N,T) args:T *)
let mk_send target args =
    mk_stree Send [target; args]
;;

(* Macros to do more complex tasks ---- *)

(* functions to extract names/types from argument lists (id, type) *)
let extract_arg_types l = List.map (fun (_,typ) -> typ) l

let extract_arg_names l = List.map (fun (nam,_) -> nam) l

(* function to take a list of names and convert to K3 variables *)
let convert_names_to_vars = 
  List.map (fun x -> if x = "_" then mk_const CUnknown else mk_var x)

(* strip the AVar or ATuple from a list of arguments *)
let strip_args arg = match arg with
    | ATuple(list_x) -> list_x
    | AVar(id, typ) -> [(id, typ)]

(* checks if a member of a collection is present *)
let mk_has_member collection pattern member_type = 
    mk_neg (mk_eq (mk_slice collection pattern) (mk_empty (member_type)))

(* function to declare and define a global function. Assumes the global
 * construct allows for an expr_t as well.
 * The types are expected in list format (always!) *)
(*
let mk_global_fn name input_names_and_types output_types expr =
    let wrap_args args = match args with
      | [id, typ]  -> AVar(id, typ)
      | []      -> invalid_arg "Can't have 0 length args"
      | _       -> ATuple(args)
    in
    Global(name, 
      TFunction(wrap_ttuple (extract_arg_types input_names_and_types)),
          wrap_ttuple (output_types))),
      mk_lambda (wrap_args input_names_and_types) expr
    )
;;
*)

(* a lambda with 2 arguments for things like aggregation functions *)
let mk_assoc_lambda arg1 arg2 expr =
    let is_a_tuple arg = match arg with
        | ATuple(_) -> true
        | AVar(_, _) -> false
    in
    let destruct_tuple_if_needed tuple_name args expr = 
        if is_a_tuple args then
            (mk_apply
                (mk_lambda
                    (ATuple(strip_args args))
                    (expr)
                )
                (mk_var tuple_name)
            )
        else expr
    in
    let subst_args args name =
      if is_a_tuple args then 
        [(name, (wrap_ttuple (extract_arg_types (strip_args args))))]
      else strip_args args
    in
    mk_lambda
      (ATuple(
            (subst_args arg1 "__temp1")@ 
            (subst_args arg2 "__temp2"))
      )
      (destruct_tuple_if_needed "__temp1" arg1 
            (destruct_tuple_if_needed "__temp2" arg2 expr)
      )
      

(* a classic let x = e1 in e2 construct *)
let mk_let var_name var_type var_value expr =
    mk_apply
        (mk_lambda (AVar(var_name, var_type))
            (expr)
        )
        (var_value)

(* A let that assigns multiple variables simultaneously. 
 * For breaking up tuples and passing multiple values out of functions.
 * var_name_type_list must be a (string, type) list, and the var_values must
 * evaluate to the same types *)
let mk_let_many var_name_and_type_list var_values expr =
    mk_apply
        (mk_lambda (ATuple(var_name_and_type_list))
            (expr)
        )
        (var_values)

(* returns code for a tuple where only the first new_size entries are taken *)
(* allows adding to start_ids_types which contains ids and types *)
let mk_reduced_tuple tup_name types new_size start_ids_types =
    let start_vars = convert_names_to_vars (extract_arg_names start_ids_types)
    in
    let list_size = List.length types in
    let size = if new_size > list_size then list_size else new_size in
    let var_range = create_range 1 size in
    let reduced_types = list_take types size in
    let ids = List.map (fun num -> "temp_"^string_of_int num) var_range in
    let ids_and_types = list_zip ids reduced_types in
    let filler_range = create_range 1 (list_size - size) in
    let filler = List.map (fun x -> mk_const CUnknown) filler_range in
    mk_apply
      (mk_lambda 
        (ATuple(start_ids_types@ids_and_types))    
        (mk_tuple
          (start_vars@(convert_names_to_vars ids)@filler)
        )
      )
      (mk_var tup_name)





