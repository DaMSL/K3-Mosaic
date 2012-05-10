
open K3
open Tree

(* Helper functions to create K3 AST nodes more easily *)

let meta = 0    (* we fill meta with a default value *)

let mk_const constant =
    recompose_tree ((meta, Const(constant)), [])
;;

let mk_var id =
    recompose_tree ((meta, Var(id)), [])
;;

let mk_tuple items =
    recompose_tree ((meta, Tuple), items)
;;

let mk_just x =
    recompose_tree ((meta, Just), [x])
;;

let mk_empty val_type =
    recompose_tree ((meta, Empty(val_type)), [])
;;

let mk_singleton val_type x =
    recompose_tree ((meta, Singleton(val_type)), [x])
;;

let mk_combine x y =
    recompose_tree ((meta, Combine), [x;y])
;;

let mk_range ctype start stride steps =
    recompose_tree ((meta, Range(ctype)), [start; stride; steps])
;;

let mk_add x y =
    recompose_tree ((meta, Add), [x; y])
;;

let mk_mult x y =
    recompose_tree ((meta, Mult), [x; y])
;;

let mk_neg x =
    recompose_tree ((meta, Neg), [x])
;;

let mk_eq x y =
    recompose_tree ((meta, Eq), [x; y])
;;

let mk_lt x y =
    recompose_tree ((meta, Lt), [x; y])
;;

let mk_neq left right =
    recompose_tree ((meta, Neq), [left;right])
;;

let mk_leq left right =
    recompose_tree ((meta, Leq), [left;right])
;;

let mk_lambda argt expr =
    recompose_tree ((meta, Lambda(argt)), [expr])
;;

let mk_apply lambda input =
    recompose_tree ((meta, Apply), [lambda; input])
;;

let mk_block statements =
    recompose_tree ((meta, Block), statements)
;;

let mk_iterate collection iter_fun =
    recompose_tree ((meta, Iterate), [collection; iter_fun])
;;

let mk_ifthenelse pred true_exp false_exp =
    recompose_tree ((meta, IfThenElse), [pred; true_exp; false_exp])
;;

let mk_map map_fun collection =
    recompose_tree ((meta, Map), [map_fun; collection])
;;

let mk_filtermap pred_fun map_fun collection = 
    recompose_tree ((meta, FilterMap), [pred_fun; map_fun; collection])
;;

let mk_flatten collection = 
    recompose_tree ((meta, Flatten), [collection])
;;

let mk_aggregate agg_fun init collection =
    recompose_tree ((meta, Aggregate), [agg_fun; init; collection])
;;

let mk_groupbyaggregate agg_fun group_fun init collection =
    recompose_tree ((meta, GroupByAggregate), [agg_fun; group_fun; init; collection])
;;

let mk_sort collection compare_fun =
    recompose_tree ((meta, Sort), [collection; compare_fun])
;;

let mk_slice collection pattern =
    recompose_tree ((meta, Slice), [collection; pattern])
;;

let mk_insert collection x =
    recompose_tree ((meta, Insert), [collection; x])
;;

let mk_delete collection x =
    recompose_tree ((meta, Delete), [collection;x])
;;

let mk_update collection old_val new_val =
    recompose_tree ((meta, Update), [collection; old_val; new_val])
;;

let mk_peek collection =
    recompose_tree ((meta, Peek), [collection])
;;

(* left: TRef, right: T/TRef *)
let mk_assigntoref left right =
    recompose_tree ((meta, AssignToRef), [left; right])
;;

(* target:TTarget(N,T) args:T *)
let mk_send target args =
    recompose_tree ((meta, Send), [target; args])
;;


