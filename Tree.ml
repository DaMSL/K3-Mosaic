(* A generic tree *)
type ('a, 'tag) tree_t
    = Leaf of 'a * 'tag
    | Node of 'a * 'tag * ('a, 'tag) tree_t list

(* Tree decomposition *)
let decompose_tree t = match t with
    | Leaf(meta, tag) -> (meta, tag), []
    | Node(meta, tag, children) -> (meta, tag), children

let recompose_tree ((meta, tag), children) = match children with
    | [] -> Leaf(meta, tag)
    | _ -> Node(meta, tag, children)

let data_of_tree e = fst (decompose_tree e)
let sub_of_tree e = snd (decompose_tree e)

let sub_tree = sub_of_tree

let rebuild_tree parts t =
  let (data,tag), c = decompose_tree t in 
  begin match c with
    | []      -> t
    | _       -> Node(data, tag, parts)
  end

let rec fold_tree td_f bu_f td_acc bu_acc t =
  let nacc = td_f td_acc t in
  let app_f = bu_f nacc in
  let recur = fold_tree td_f bu_f nacc bu_acc in
  let _, c = decompose_tree t in
    begin match c with 
      | [] -> app_f [bu_acc] t
      | _ -> app_f (List.map recur c) t
    end
    
let rec string_of_tree string_of_a string_of_tag t =
  let rcr = string_of_tree string_of_a string_of_tag in
  match t with
    | Node (a, tag, children) ->
      string_of_tag tag (List.map rcr children) 
    | Leaf (a, tag) -> string_of_tag tag []
