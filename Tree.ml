type 'a tree_t
    = Leaf of 'a
    | Node of 'a * 'a tree_t list

let decompose_tree t =
    match t with
    | Leaf(data) -> (data, [])
    | Node(data, children) -> (data, children)

let recompose_tree (data, children) =
    match children with
    | [] -> Leaf(data)
    | _ -> Node(data, children)

let data_of_tree e = fst (decompose_tree e)
let sub_of_tree e = snd (decompose_tree e)

let sub_tree = sub_of_tree

let rebuild_tree parts t =
  let data, children = decompose_tree t in
  begin match children with
    | []      -> t
    | _       -> Node(data, parts)
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
    
let rec string_of_tree string_of_data t =
  match t with
    | Node(data, children) -> string_of_data data (List.map (string_of_tree string_of_data) children)
    | Leaf(data) -> string_of_data data []
