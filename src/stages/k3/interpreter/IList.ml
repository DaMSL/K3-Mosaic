open Util

(* ----- List functions ----- *)

type 'a t = 'a list

let singleton x = [x]

let empty = []

let is_empty = function [] -> true | _ -> false

let insert x l = x::l

let delete x l = list_remove x l

let fold = List.fold_left

let map = List.map

let filter = List.filter

let iter = List.iter

let combine x y = x @ y

let peek b = match b with
  | h::_ -> Some h
  | _    -> None

let sort = List.sort

let update v v' b = list_replace v v' b

let iter2 = List.iter2
