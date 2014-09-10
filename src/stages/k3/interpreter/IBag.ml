open Util

(* ----- Bag functions ----- *)

module type S = sig
  type elt
  type t
  val empty : t
  val singleton : elt -> t
  val is_empty : t -> bool
  val insert : elt -> t -> t
  val delete : elt -> t -> t
  val combine : t -> t -> t
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val map : (elt -> elt) -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val filter : (elt -> bool) -> t -> t
  val update : elt -> elt -> t -> t
  val peek : t -> elt option
  val count : elt -> t -> int
  val to_list : t -> elt list
  val of_list : elt list -> t
  val compare : t -> t -> int
end

module Make(Ord : ICommon.OrderedKeyType) = struct

  module HMap = HashMap.Make(Ord)

  type elt = Ord.t
  type t = int HMap.t

  let insert x bag = try
      let i = HMap.find x bag in
      HMap.add x (i+1) bag
    with Not_found -> HMap.add x 1 bag

  let delete x bag = try
      match HMap.find x bag with
      | 1 | 0 -> HMap.remove x bag
      | i -> HMap.add x (i-1) bag
    with Not_found -> bag

  let empty = HMap.empty

  let is_empty = HMap.is_empty

  let singleton x = HMap.singleton x 1

  let fold f (zero:'a) (bag:t) : 'a = HMap.fold (fun k v acc ->
      iterate (fun acc' -> f acc' k) (acc: 'a) (v:int)) bag (zero:'a)

  let map f bag = fold (fun acc x -> insert (f x) acc) empty bag

  let filter f bag = fold (fun acc x -> if f x then insert x acc else acc) empty bag

  let iter f bag = HMap.iter (fun k v -> for i = 1 to v do f k done) bag

  let combine x y = HMap.merge (fun k a b -> match a, b with
      | Some a, Some b -> Some(a+b)
      | Some a, _
      | _, Some a      -> Some a
      | _              -> None) x y

  let peek b = try Some(fst @: HMap.choose b) with Not_found -> None

  let update k k' b =
    let m = delete k b in
    insert k' m

  let count e m = HMap.find e m

  let of_list l = List.fold_left (fun acc x -> insert x acc) empty l

  let to_list (b:t) : elt list = fold (fun (acc:elt list) (x:elt) -> x::acc) ([]:elt list) (b:t)

  let compare x y = HMap.compare (-) x y
end
