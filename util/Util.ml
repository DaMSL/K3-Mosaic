(* Utilities that are useful *)

(* make a range from first to last. Tail recursive *)
let create_range first length =
    let rec range_inner index acc =
        if index >= first+length then acc
        else range_inner (index+1) (index::acc)
    in
    List.rev(range_inner first [])

(* take the first x elements of a list *)
let list_take li len =
  let rec take li2 len2 acc_list =
    if len2 >= len then acc_list
    else match li2 with 
    | [] -> acc_list
    | head::tail -> take tail (len2+1) (head::acc_list)
  in
  List.rev (take li 0 [])

let list_zip list1 list2 = List.map2 (fun i j -> (i,j)) list1 list2

let compose_fn f g x = f(g x)
