open Util
open K3Values

(* ----- List functions ----- *)
let singleton x = [x]

let empty = []

let is_empty = function [] -> true | _ -> false

let insert x l = x::l

let delete x l = list_remove x l

let fold = List.fold_left

let map = List.map

let filter = List.filter

let iterate = List.iterate

let combine x y = x@y

let peek b = match b with
  | h::_ -> VOption (Some h)
  | _    -> VOption None

let update v v' b = list_replace v v' b
