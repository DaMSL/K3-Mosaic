open Util

(* ----- HashMap: performant map ----- *)

module type S = sig
    type key
    type 'a t
    val empty : 'a t
    val singleton : key -> 'a -> 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update_with : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val remove : key -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val combine : 'a t -> 'a t -> 'a t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val map : (key -> 'a -> 'b) -> 'a t -> 'b t
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val update : key -> 'a -> key -> 'a -> 'a t -> 'a t
    val peek : 'a t -> (key * 'a) option
    val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val choose : 'a t -> (key * 'a)
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
end

module type OrderedType = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
end

module Make(Ord : OrderedType) = struct

  module IntMap : (Map.S with type key = int) = Map.Make(struct
    type t = int let compare = (-) end)

  module WrapMap : (Map.S with type key = Ord.t) = Map.Make(Ord)

  type key = Ord.t
  type 'a t = ('a WrapMap.t) IntMap.t

  let empty = IntMap.empty

  let singleton k v = IntMap.singleton (Ord.hash k) @@
                        WrapMap.singleton k v

  let is_empty x = x = empty

  let add k v m =
    let h = Ord.hash k in
    let innermap  = try IntMap.find h m with Not_found -> WrapMap.empty in
    let innermap' = WrapMap.add k v innermap in
    IntMap.add h innermap' m

  let update_with k f m =
    let h = Ord.hash k in
    let innermap  = try IntMap.find h m with Not_found -> WrapMap.empty in
    let oldv      = try some @@ WrapMap.find k innermap with Not_found -> None in
    match f oldv with
    | None ->
        let innermap' = WrapMap.remove k innermap in
        if WrapMap.is_empty innermap' then IntMap.remove h m
        else IntMap.add h innermap' m
    | Some v ->
        let innermap' = WrapMap.add k v innermap in
        IntMap.add h innermap' m

  let remove k m =
    let h = Ord.hash k in
    try
      let im  = IntMap.find h m in
      let im' = WrapMap.remove k im in
      if WrapMap.is_empty im' then
        IntMap.remove h m
      else IntMap.add h im' m
    with Not_found -> m

  let find k m =
    let h = Ord.hash k in
    let im = IntMap.find h m in
    WrapMap.find k im

  let combine x y =
    let innerMerge k mx my = match mx, my with
      | _, Some y -> Some y
      | Some x, _ -> Some x
      | _         -> None
    in
    let outerMerge k mx my = match mx, my with
      | Some x, Some y -> Some(WrapMap.merge innerMerge x y)
      | Some x, _
      | _, Some x      -> Some x
      | _              -> None
    in IntMap.merge outerMerge x y

  let fold (f:key -> 'a -> 'b -> 'b) (m: 'a t) (zero: 'b) =
    IntMap.fold (fun _ im acc -> WrapMap.fold f im acc)
    m zero

  let merge f m m' =
    let outerMerge k mx my = match mx, my with
      | Some x, Some y -> Some(WrapMap.merge f x y)
      | Some x, None   -> Some(WrapMap.merge f x WrapMap.empty)
      | None, Some x   -> Some(WrapMap.merge f WrapMap.empty x)
      | _              -> None
    in IntMap.merge outerMerge m m'

  let map f m =
    fold (fun k v acc -> add k (f k v) acc) m empty

  let iter f m = IntMap.iter (fun _ im -> WrapMap.iter f im) m

  let filter f m =
    fold (fun k v acc -> if f k v then add k v acc else acc)
    IntMap.empty m

  let update k v k' v' m = add k' v' @@ remove k m

  let peek m = try
	Some(WrapMap.choose @@ snd @@ IntMap.choose m)
    with Not_found -> None

  let choose m = WrapMap.choose @@ snd @@ IntMap.choose m

  let to_list m = fold (fun k v acc -> (k,v)::acc) m []

  let of_list l = List.fold_left (fun acc (k, v) ->
    add k v acc) empty l

  let compare (f:'a -> 'a -> int) (m:'a t) (m': 'a t) : int = IntMap.compare (fun a b -> WrapMap.compare f a b) m m'

end

