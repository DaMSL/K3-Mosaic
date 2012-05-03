
open K3
open Tree

(* Helper functions to create K3 AST nodes more easily *)

let mk_const meta constant =
    recompose_tree ((meta, Const(constant)), [])
;;

let mk_var meta id =
    recompose_tree ((meta, Var(id)), [])
;;

let mk_tuple meta items =
    recompose_tree ((meta, Tuple), items)
;;

let mk_just meta x =
    recompose_tree ((meta, Just), [x])
;;

let mk_empty meta val_type =
    recompose_tree ((meta, Empty(val_type)), [])
;;

let mk_singleton meta val_type x =
    recompose_tree ((meta, Singleton(val_type)), [x])
;;

let mk_combine meta x y =
    recompose_tree ((meta, Combine), [x;y])
;;

let mk_range meta ctype start stride steps =
    recompose_tree ((meta, Range(ctype)), [start; stride; steps])
;;

let mk_add meta x y =
    recompose_tree ((meta, Add), [x; y])
;;

let mk_mult meta x y =
    recompose_tree ((meta, Mult), [x; y])
;;

let mk_neg meta x =
    recompose_tree ((meta, Neg), [x])
;;

let mk_eq meta x y =
    recompose_tree ((meta, Eq), [x; y])
;;

let mk_lt meta x y =
    recompose_tree ((meta, Lt), [x; y])
;;

let mk_neq meta left right =
    recompose_tree ((meta, Neq), [left;right])
;;

let mk_leq meta left right =
    recompose_tree ((meta, Leq), [left;right])
;;

let mk_lambda meta argt expr =
    recompose_tree ((meta, Lambda(argt)), [expr])
;;

let mk_apply meta lambda input =
    recompose_tree ((meta, Apply), [lambda; input])
;;

let mk_block meta statements =
    recompose_tree ((meta, Block), statements)
;;

let mk_iterate meta collection iter_fun =
    recompose_tree ((meta, Iterate), [collection; iter_fun])
;;

let mk_ifthenelse meta pred true_exp false_exp =
    recompose_tree ((meta, IfThenElse), [pred; true_exp; false_exp])
;;

let mk_map meta map_fun collection =
    recompose_tree ((meta, Map), [map_fun; collection])
;;

let mk_filtermap meta pred_fun map_fun collection = 
    recompose_tree ((meta, FilterMap), [pred_fun; map_fun; collection])
;;

let mk_flatten meta collection = 
    recompose_tree ((meta, Flatten), [collection])
;;

let mk_aggregate meta agg_fun init collection =
    recompose_tree ((meta, Aggregate), [agg_fun; init; collection])
;;

let mk_groupbyaggregate meta agg_fun group_fun init collection =
    recompose_tree ((meta, GroupByAggregate), [agg_fun; group_fun; init; collection])
;;

let mk_sort meta collection compare_fun =
    recompose_tree ((meta, Sort), [collection; compare_fun])
;;

let mk_slice meta collection pattern =
    recompose_tree ((meta, Slice), [collection; pattern])
;;

let mk_insert meta collection x =
    recompose_tree ((meta, Insert), [collection; x])
;;

let mk_delete meta collection x =
    recompose_tree ((meta, Delete), [collection;x])
;;

let mk_update meta collection old_val new_val =
    recompose_tree ((meta, Update), [collection; old_val; new_val])
;;

let mk_peek meta collection =
    recompose_tree ((meta, Peek), [collection])
;;

(* left: TRef, right: T/TRef *)
let mk_assigntoref meta left right =
    recompose_tree ((meta, AssignToRef), [left; right])
;;

(* target:TTarget(N,T) args:T *)
let mk_send meta target args =
    recompose_tree ((meta, Send), [target; args])
;;


