open Util

(* ----- List functions ----- *)

type 'a t = 'a list

let of_list l = l
let to_list m = m

let singleton x = [x]

let empty = []

let is_empty = function [] -> true | _ -> false

let insert x l = l@[x]

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

let at = List.nth

let tail = tl

exception Mismatch of int

let compare f x y =
  let lx, ly = List.length x, List.length y in
  if lx <> ly then lx - ly
  else
  try
     List.iter2 (fun x y -> let v = f x y in
                  if v <> 0 then raise (Mismatch v)) x y;
     0
  with Mismatch v -> v

let size l = List.length l
