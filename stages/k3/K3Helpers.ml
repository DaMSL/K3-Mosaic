
open K3
open Tree

(* Helper functions to create K3 AST nodes more easily *)

let meta = 0    (* we fill meta with a default value *)

(* Standard AST nodes *)
let mk_const constant = recompose_tree ((meta, Const(constant)), [])

let mk_var id = recompose_tree ((meta, Var(id)), [])

let mk_tuple items = recompose_tree ((meta, Tuple), items)

let mk_just x = recompose_tree ((meta, Just), [x])

let mk_empty val_type = recompose_tree ((meta, Empty(val_type)), [])

let mk_singleton val_type x = recompose_tree ((meta, Singleton(val_type)), [x])

let mk_combine x y = recompose_tree ((meta, Combine), [x;y])

let mk_range ctype start stride steps =
    recompose_tree ((meta, Range(ctype)), [start; stride; steps])

let mk_add x y = recompose_tree ((meta, Add), [x; y])

let mk_or = mk_add

let mk_mult x y = recompose_tree ((meta, Mult), [x; y])

let mk_and = mk_mult

let mk_neg x = recompose_tree ((meta, Neg), [x])

let mk_not = mk_neg

let mk_eq x y = recompose_tree ((meta, Eq), [x; y])

let mk_lt x y = recompose_tree ((meta, Lt), [x; y])

let mk_neq left right = recompose_tree ((meta, Neq), [left;right])

let mk_leq left right = recompose_tree ((meta, Leq), [left;right])

let mk_geq left right = mk_not (mk_lt left right)

let mk_gt left right = mk_not (mk_leq left right)

let mk_lambda argt expr = recompose_tree ((meta, Lambda(argt)), [expr])

let mk_apply lambda input = recompose_tree ((meta, Apply), [lambda; input])

let mk_block statements = recompose_tree ((meta, Block), statements)

let mk_iter collection iter_fun = 
    recompose_tree ((meta, Iterate), [collection; iter_fun])

let mk_ifthenelse pred true_exp false_exp =
    recompose_tree ((meta, IfThenElse), [pred; true_exp; false_exp])

let mk_map map_fun collection =
    recompose_tree ((meta, Map), [map_fun; collection])

let mk_filtermap pred_fun map_fun collection = 
    recompose_tree ((meta, FilterMap), [pred_fun; map_fun; collection])

let mk_flatten collection = recompose_tree ((meta, Flatten), [collection])

let mk_aggregate agg_fun init collection =
    recompose_tree ((meta, Aggregate), [agg_fun; init; collection])

let mk_groupbyaggregate agg_fun group_fun init collection =
    recompose_tree ((meta, GroupByAggregate), [agg_fun; group_fun; init; collection])

let mk_sort collection compare_fun =
    recompose_tree ((meta, Sort), [collection; compare_fun])

let mk_slice collection pattern =
    recompose_tree ((meta, Slice), [collection; pattern])

let mk_insert collection x = recompose_tree ((meta, Insert), [collection; x])

let mk_delete collection x = recompose_tree ((meta, Delete), [collection;x])

let mk_update collection old_val new_val =
    recompose_tree ((meta, Update), [collection; old_val; new_val])

let mk_peek collection = recompose_tree ((meta, Peek), [collection])

(* left: TRef, right: T/TRef *)
let mk_assigntoref left right =
    recompose_tree ((meta, AssignToRef), [left; right])
;;

(* target:TTarget(N,T) args:T *)
let mk_send target args =
    recompose_tree ((meta, Send), [target; args])
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
      TFunction(BaseT(TTuple(extract_arg_types input_names_and_types)),
          BaseT(TTuple(output_types))),
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
        [(name, (BaseT(TTuple(extract_arg_types (strip_args args)))))]
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

(* Type manipulation functions ------------- *)

(* Add a generic list wrapper around a type *)
let wrap_tlist typ = BaseT(TCollection(TList, typ))

let wrap_ttuple typ = BaseT(TTuple(typ))

let t_int = BaseT(TInt)

