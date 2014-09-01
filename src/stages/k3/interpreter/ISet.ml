open Util
open K3Values

(* ----- Set functions ----- *)

type 'a t = 'a list

let of_list l = nub @: l

let to_list s = s

let singleton x = [x]

let empty = []

let is_empty b = b = []

let insert x set = nub @: x::set

let delete x set = list_remove x set

let fold = List.fold_left

let map = List.map

let filter = List.filter

let iter = List.iter

let iter2 = List.iter2

let combine x y = nub @: x @ y

let peek b = match b with
  | h::_ -> Some h
  | _    -> None

let update v v' b = nub @: list_replace v v' b

let equals x y =
  let sort = List.sort compare in
  sort x = sort y

let iter2 f x y = 
  let sort = List.sort compare in
  List.iter2 f (sort x) (sort y)
