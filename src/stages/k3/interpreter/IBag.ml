open Util

(* ----- Bag functions ----- *)

module type S = sig
  type elt
  module InnerMap : NearMap.S
  type t = int InnerMap.t
  val empty : t
  val singleton : elt -> t
  val is_empty : t -> bool
  val insert : elt -> t -> t
  val delete : elt -> t -> t
  val combine : t -> t -> t
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val map : (elt -> elt) -> t -> t
  val iter : (elt -> unit) -> t -> unit
  val iter2 : (elt -> elt -> unit) -> t -> t -> unit
  val filter : (elt -> bool) -> t -> t
  val update : elt -> elt -> t -> t
  val peek : t -> elt option
  val to_list : t -> elt list
  val from_list : elt list -> t
end

module Make(Ord : ICommon.OrderedKeyType) = struct

  module InnerMap = NearMap.Make(Ord)

  type elt = Ord.t
  type t = int InnerMap.t

  let insert x bag = try
      let i = InnerMap.find x bag in
      InnerMap.add x (i+1) bag
    with Not_found -> InnerMap.add x 1 bag

  let delete x bag = try
      match InnerMap.find x bag with
      | 1 | 0 -> InnerMap.remove x bag
      | i -> InnerMap.add x (i-1) bag
    with Not_found -> bag

  let empty = InnerMap.empty

  let is_empty = InnerMap.is_empty

  let singleton x = InnerMap.singleton x 1

  let fold f zero bag = InnerMap.fold (fun k v acc ->
      iterate (flip f k) acc v) bag zero

  let map f bag = fold (fun acc x -> insert (f x) acc) empty bag

  let filter f bag = fold (fun acc x -> if f x then insert x acc else acc) empty bag

  let iter f bag = InnerMap.iter (fun k v -> for i = 1 to v do f k done) bag

  let combine x y = InnerMap.merge (fun k a b -> match a, b with
      | Some a, Some b -> Some(a+b)
      | Some a, _
      | _, Some a      -> Some a
      | _              -> None) x y

  let peek b = try Some(fst @: InnerMap.choose b) with Not_found -> None

  let update k k' b =
    let m = delete k b in
    insert k' m

  let iter2 f x y =
    InnerMap.iter2 (fun k v v' ->
        if v != v' then invalid_arg "unequal counts"
        else for i=1 to v do f k done)
      x y

  let from_list l = List.fold_left (fun acc x -> insert x acc) empty l

  let to_list b = fold (fun acc x -> x::acc) [] b
end




