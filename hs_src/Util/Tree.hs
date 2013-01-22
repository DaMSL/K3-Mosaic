module Util.Tree where

{-open Symbols-}
{-open Printing-}
{-open Util-}

import Data.List (foldl')

data Tree a = Leaf a
            | Node a [Tree a]
            deriving Show

-- Tree constructors, destructors

mk_tree (d, []) = Leaf d
mk_tree (d, children)  = Node d children

decompose_tree (Leaf d) = (d, [])
decompose_tree (Node d children) = (d, children)

recompose_tree t new_children =
  let (d, children) = decompose_tree t
  in case children of
    [] -> t
    _  -> Node d new_children

-- Tree accessors

node_data t = fst $ decompose_tree t

tree_data t = node_data t : (concat $ map tree_data $ snd $ decompose_tree t)
  
sub_tree t = snd $ decompose_tree t

-- Tree traversals

fold_tree td_f bu_f td_init bu_init tree = ft_aux td_init tree
  where ft_aux td t = let n_td = td_f td t
          in case sub_tree t of
            [] -> bu_f n_td [bu_init] t
            c  -> bu_f n_td (map (ft_aux n_td) c) t

-- for tree modifications, bottom-up direction starts with no init
fold_tree1 td_f bu_f td_init tree = ft_aux td_init tree
  where ft_aux td t = let n_td = td_f td t
          in case sub_tree t of
            [] -> bu_f n_td [] t
            c  -> bu_f n_td (map (ft_aux n_td) c) t
    
fold_tree_thread td_f bu_f td_init bu_init tree = ft_aux td_init tree
  where ft_aux td t = let n_td = td_f td t
          in case snd $ decompose_tree t of
            [] -> bu_f (n_td, [bu_init]) t
            c  -> bu_f (foldl' recur (n_td, []) c) t
              where recur acc ch = 
                      let r = ft_aux (fst acc) ch 
                      in (fst r, snd acc++[snd r])

{-fold_tree_lazy td_f bu_f td_init bu_init tree =-}
  {-let rec ft_aux td t =-}
    {-let n_td = lazy (td_f td t)-}
    {-in begin match snd (decompose_tree t) with -}
        {-| [] -> bu_f n_td [lazy bu_init] t-}
        {-| c -> bu_f n_td (map (\c_t -> lazy (ft_aux n_td c_t)) c) t-}
      {-end-}
  {-in ft_aux (lazy td_init) t-}

-- Modify a tree by running a function over it, bottom up
modify_tree_bu e fn = fold_tree1 (\ _ _ -> Nothing) rebuild_tree Nothing e
  where rebuild_tree _ acc_children tree =
          let d = node_data tree
              t = mk_tree (d, acc_children)
          in fn t

-- Modify a tree by running a function over it, bottom up
modify_tree_bu_with_path e fn = 
  fold_tree1 (\n_td t -> t:n_td) rebuild_tree [] e
  where rebuild_tree path acc_children t =
          let d = node_data t
              tree = mk_tree (d, acc_children)
          in fn tree (drop 1 path)

-- Trees with tuple metadata

decorate_tree f t = mk_tree (f d, map (decorate_tree f) $ sub_tree t)
  where d = node_data t

prepend_tree f t = decorate_tree (\d -> (f d, d)) t
append_tree f t =  decorate_tree (\d -> (d, f d)) t

project_tree t = mk_tree (d, map project_tree $ sub_tree t)
  where d = snd $ node_data t

fst_data t = fst $ node_data t
snd_data t = snd $ node_data t

-- Generic tree labelling

{-label_tree t = prepend_tree (\_ -> gen_int_sym default_class) t-} -- Need default_class
unlabel_tree t = project_tree t

label_of_node t = fst_data t
label_of_tree t = map fst $ tree_data t

-- Generic tree pretty printing

{-let rec flat_string_of_tree string_of_data t =-}
  {-let rcr = flat_string_of_tree string_of_data in-}
  {-match t with-}
    {-| Node(data, children) -> string_of_data data (List.map rcr children)-}
    {-| Leaf(data) -> string_of_data data []-}

{-let print_tree print_data t =-}
  {-fold_tree_lazy (\_ _ -> ()) (\_ x y -> print_data x y) () () t-}
  
{-let string_of_tree print_data t =-}
  {-wrap_formatter (\() -> print_tree print_data t)-}
