open Util
open K3Values

(* ----- Bag functions ----- *)

type 'a t = 'a list

let of_list l = l

let to_list b = b

let singleton x = [x]

let empty = []

let is_empty b = b = []

let insert x bag = x::bag

let delete x bag = list_remove x bag

let fold = List.fold_left

let map = List.map

let filter = List.filter

let iter = List.iter

let combine x y = x @ y

let peek b = match b with
  | h::_ -> Some h
  | _    -> None

let update v v' b = list_replace v v' b

let equals x y =
  let sort = List.sort compare in
  sort x = sort y

let iter2 f x y = 
  let sort = List.sort compare in
  List.iter2 f (sort x) (sort y)