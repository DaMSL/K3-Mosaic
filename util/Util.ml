(* Utilities that are useful *)

(* low precedence function application allows us to remove 
 * many () from our code *)
let (@:) f x = f x;;

(* make a range from first to last. Tail recursive *)
let create_range first length =
    let rec range_inner index acc =
        if index >= first+length then acc
        else range_inner (index+1) (index::acc)
    in
    List.rev(range_inner first [])

(* take the first x elements of a list *)
let list_take len li =
  let rec take len2 li2 acc_list =
    if len2 >= len then acc_list
    else match li2 with 
    | [] -> acc_list
    | head::tail -> take (len2+1) tail (head::acc_list)
  in
  List.rev (take 0 li [])

(* drop the first x elements of a list *)
let rec list_drop len li = match li with
  | [] -> []
  | x::xs when len = 0 -> li
  | x::xs -> list_drop (len-1) xs

let list_zip list1 list2 = List.map2 (fun i j -> (i,j)) list1 list2

let compose_fn f g x = f(g x)
