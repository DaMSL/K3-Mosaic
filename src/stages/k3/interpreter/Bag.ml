open Util
open K3Values

(* ----- Bag functions ----- *)
let insert x bag = x::bag

let delete x bag = list_remove x bag

let fold = List.fold_left

let map = List.map

let filter = List.filter

let iterate = List.iterate

let singleton x = [x]

let empty = []

let is_empty b = b = []

let merge x y = x@y

let peek b = match b with
  | h::_ -> VOption (Some h)
  | _    -> VOption None

let update v v' b = list_replace v v' b

