open Util

(* ----- Set functions ----- *)

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
  val size : t -> int
  val intersect : t -> t -> t
  val union : t -> t -> t
  val to_list : t -> elt list
  val of_list : elt list -> t
  val compare : t -> t -> int
  val to_string : t -> string
end

module Make(Ord : ICommon.OrderedKeyType) = struct

  module HMap = HashMap.Make(Ord)

  type elt = Ord.t
  type t = unit HMap.t

  let insert x bag = try
      HMap.find x bag; bag
    with Not_found -> HMap.add x () bag

  let delete x bag = HMap.remove x bag

  let empty = HMap.empty

  let is_empty = HMap.is_empty

  let singleton x = HMap.singleton x ()

  let fold f (zero:'a) (bag:t) : 'a = HMap.fold (fun k _ acc -> f acc k) bag (zero:'a)

  let map f bag = fold (fun acc x -> insert (f x) acc) empty bag

  let filter f bag = fold (fun acc x -> if f x then insert x acc else acc) empty bag

  let iter f bag = HMap.iter (fun k _ -> f k) bag

  let combine x y = HMap.merge (fun k a b -> match a, b with
      | Some _, _
      | _, Some _      -> Some ()
      | _              -> None) x y

  let peek b = try Some(fst @@ HMap.choose b) with Not_found -> None

  let update k k' b =
    let m = delete k b in
    insert k' m

  let intersect l r = HMap.merge (fun k ml mr -> match ml, mr with
    | Some _, Some _ -> Some ()
    | _              -> None
  ) l r

  let union l r = HMap.merge (fun _ ml mr -> match ml, mr with
    | Some _, _
    | _, Some _      -> Some ()
    | _              -> None
  ) l r

  let size h = HMap.fold (fun _ _ acc -> acc + 1) h 0

  let of_list l = List.fold_left (fun acc x -> insert x acc) empty l

  let to_list (b:t) : elt list =
    fold (fun (acc:elt list) (x:elt) -> x::acc) ([]:elt list) (b:t)

  let compare x y = HMap.compare (fun _ _ -> 0) x y

  let to_string (b:t) = Printf.sprintf "[%s]" @@ String.concat "; " @@ List.map Ord.to_string @@ to_list b
end
