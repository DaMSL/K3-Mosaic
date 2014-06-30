open Symbols
open Printing
open Util

type 'a tree_t
    = Leaf of 'a
    | Node of 'a * 'a tree_t list

(* Tree constructors, destructors *)

let mk_tree (data, children) =
    match children with
    | [] -> Leaf(data)
    | _ -> Node(data, children)

let decompose_tree t =
    match t with
    | Leaf(data) -> (data, [])
    | Node(data, children) -> (data, children)

let recompose_tree t new_children =
  let data, children = decompose_tree t in
  begin match children with
    | []      -> t
    | _       -> Node(data, new_children)
  end

(* Tree accessors *)

let node_data t = fst (decompose_tree t)

let rec tree_data t =
  (node_data t)::(List.flatten (List.map tree_data (snd (decompose_tree t))))

let sub_tree t = snd (decompose_tree t)

(* Tree traversals *)

let fold_tree td_f bu_f td_init bu_init t =
  let rec ft_aux td t =
    let n_td = td_f td t
    in begin match sub_tree t with
        | [] -> bu_f n_td [bu_init] t
        | c -> bu_f n_td (List.map (ft_aux n_td) c) t
      end
  in ft_aux td_init t

(* for tree modifications, bottom-up direction starts with no init *)
let fold_tree1 td_f bu_f td_init t =
  let rec ft_aux td t =
    let n_td = td_f td t
    in begin match sub_tree t with
        | [] -> bu_f n_td [] t
        | c -> bu_f n_td (List.map (ft_aux n_td) c) t
      end
  in ft_aux td_init t

let fold_tree_thread td_f bu_f td_init bu_init t =
  let rec ft_aux td t =
    let n_td = td_f td t in
    let recur acc ch =
      let r = ft_aux (fst acc) ch in (fst r, (snd acc)@[snd r])
    in
      begin match snd (decompose_tree t) with
        | [] -> bu_f (n_td, [bu_init]) t
        | c -> bu_f (List.fold_left recur (n_td, []) c) t
      end
  in ft_aux td_init t

(* Lazy tree folding *)
let fold_tree_lazy td_f bu_f td_init bu_init t =
  let rec ft_aux td t =
    let n_td = lazy (td_f td t)
    in begin match snd (decompose_tree t) with
        | [] -> bu_f n_td [lazy bu_init] t
        | c -> bu_f n_td (List.map (fun c_t -> lazy (ft_aux n_td c_t)) c) t
      end
  in ft_aux (lazy td_init) t

(* Modify a tree by running a function over it, bottom up *)
let modify_tree_bu e fn =
  (* first, make the node and attach its children *)
  let rebuild_tree _ acc_children t =
     let data = node_data t in
     let tree = mk_tree (data, acc_children) in
     fn tree in  (* run the function only on the created tree *)
  fold_tree1 (fun _ _ -> None) rebuild_tree None e

(* Modify a tree by running a function over it, bottom up *)
let modify_tree_bu_with_path e fn =
  (* first, make the node and attach its children *)
  let rebuild_tree path acc_children t =
     let data = node_data t in
     let tree = mk_tree (data, acc_children) in
     fn tree (list_drop 1 path) in
  fold_tree1 (fun n_td t -> t::n_td) rebuild_tree [] e

(* Modify a tree by running a function over it, bottom up *)
let modify_tree_bu_with_path_and_msgs e fn =
  (* first, make the node and attach its children *)
  let rebuild_tree path msg_children t =
     let data = node_data t in
     let msg_children = List.split msg_children in
     let tree = mk_tree (data, snd msg_children) in
     fn tree (fst msg_children) (list_drop 1 path)  in
  snd @: fold_tree1 (fun n_td t -> t::n_td) rebuild_tree [] e

(* Trees with tuple metadata *)

let rec decorate_tree f t =
  let d = node_data t
  in mk_tree (f d, (List.map (decorate_tree f) (sub_tree t)))

let rec prepend_tree f t = decorate_tree (fun d -> (f d, d)) t
let rec append_tree f t = decorate_tree (fun d -> (d, f d)) t

let rec project_tree t =
  let d = snd (node_data t)
  in mk_tree (d, List.map project_tree (sub_tree t))

let fst_data t = fst (node_data t)
let snd_data t = snd (node_data t)

(* Generic tree labelling *)

let label_tree t = prepend_tree (fun _ -> gen_int_sym default_class) t
let unlabel_tree t = project_tree t

let label_of_node t = fst_data t
let label_of_tree t = List.map fst (tree_data t)

(* Generic tree pretty printing *)

let rec flat_string_of_tree string_of_data t =
  let rcr = flat_string_of_tree string_of_data in
  match t with
    | Node(data, children) -> string_of_data data (List.map rcr children)
    | Leaf(data) -> string_of_data data []

let print_tree print_data t =
  fold_tree_lazy (fun _ _ -> ()) (fun _ x y -> print_data x y) () () t

let string_of_tree print_data t =
  wrap_formatter (fun () -> print_tree print_data t)
