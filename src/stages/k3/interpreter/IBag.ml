open Util
open K3Values

(* ----- Bag functions ----- *)

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

let iterate = List.iterate

let combine x y = x@y

let peek b = match b with
  | h::_ -> VOption (Some h)
  | _    -> VOption None

let update v v' b = list_replace v v' b

let equals x y =
  let sort = List.sort compare in
  sort x = sort y

