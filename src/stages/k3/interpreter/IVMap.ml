open Util
open K3.AST
module KP = K3Printing

(* ------ Multimap functions ------ *)
(* NOTE: remove_prefix: we delete up to and including vid, and keep a frontier at vid
 *       update_suffix: updates the > vid and all following vids
 *       frontier: gets values < the vid only
 * Check if these are correct!
 *)


module type S = sig
  type 'a t
  type vid
  type key
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : vid -> key -> 'a -> 'a t -> 'a t
  val singleton : vid -> key -> 'a -> 'a t
  val remove : vid -> key -> 'a t -> 'a t
  val remove_prefix : vid -> 'a t -> 'a t
  val combine : 'a t -> 'a t -> 'a t
  val fold : (vid -> key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map : (vid -> key -> 'a -> 'b) -> 'a t -> 'b t
  val iter : (vid -> key -> 'a -> unit) -> 'a t -> unit
  val filter : (vid -> key -> 'a -> bool) -> 'a t -> 'a t
  val update : vid -> key -> 'a -> vid -> key -> 'a -> 'a t -> 'a t
  val update_with : vid -> key -> ('a option -> 'a option) -> 'a t -> 'a t
  val update_suffix : vid -> key -> ('a -> 'a ) -> 'a t -> 'a t
  val peek : 'a t -> (vid * key * 'a) option
  val to_list : 'a t -> (vid * key * 'a) list
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val size : 'a t -> int
  val frontier_point: vid -> key -> 'a t -> 'a t
  val frontier_slice: vid -> 'a t -> 'a t
end

module Make(OrdVid: ICommon.OrderedKeyType)(OrdKey: ICommon.OrderedKeyType) = struct

  module VIDMap : (NearMap.S with type key = OrdVid.t) = NearMap.Make(OrdVid)
  module HMap   : (HashMap.S with type key = OrdKey.t) = HashMap.Make(OrdKey)

  type 'a t = 'a VIDMap.t HMap.t
  type vid = OrdVid.t
  type key = OrdKey.t

  let empty : 'a VIDMap.t HMap.t = HMap.empty

  let is_empty mm = mm = empty

  let rec add vid k v m =
    HMap.update_with k (function
      | None     -> some @@ VIDMap.singleton vid v
      | Some old -> some @@ VIDMap.add vid v old) m

  let update_with vid k f m =
    HMap.update_with k (function
      | None -> begin match f None with
          | None   -> None
          | Some v -> some @@ VIDMap.singleton vid v
          end
      | Some vidmap ->
          let vidmap' = VIDMap.update_with vid f vidmap in
          if VIDMap.is_empty vidmap' then None
          else Some vidmap'
    ) m

  (* get the frontier for a slice (must read entire domain) *)
  let frontier_slice vid m =
    HMap.fold (fun k vidmap acc ->
      try
        add vid k (VIDMap.find_lt vid vidmap) acc
      with Not_found -> acc
    ) m empty

  let singleton vid k v = HMap.singleton k (VIDMap.singleton vid v)

  (* get the frontier at a specific key *)
  let frontier_point vid k m =
    let vidmap = HMap.find k m in
    singleton vid k @@ VIDMap.find_lt vid vidmap

  let remove vid k m =
    HMap.update_with k (function
      | None     -> None
      | Some oldm ->
          let oldm' = VIDMap.remove vid oldm in
          if VIDMap.is_empty oldm' then None
          else Some oldm') m

  let merge f m m' =
    HMap.merge (fun k v v' ->
      let lvidm, rvidm = match v, v' with
        | Some x, Some y -> x, y
        | Some x, _      -> x, VIDMap.empty
        | _, Some y      -> VIDMap.empty, y
        | _              -> VIDMap.empty, VIDMap.empty
      in
      let vidm' = VIDMap.merge (fun vid v v' -> f vid k v v') lvidm rvidm in
      if VIDMap.is_empty vidm' then None else Some vidm'
    ) m m'

  let combine m m' = merge (fun _ _ v v' -> match v, v' with
    | _, Some x
    | Some x, _ -> Some x
    | _         -> None) m m'

  (* for GC: save a frontier and delete all before *)
  let remove_prefix vid (m: 'a t) =
    let slice = frontier_slice vid m in
    let m' = HMap.map (fun k vidmap ->
      VIDMap.filter (fun vid' v -> OrdVid.compare vid' vid > 0) vidmap
    ) m in
    combine m' slice

  let fold f m zero =
    HMap.fold (fun k vidm acc ->
      VIDMap.fold (fun vid v acc' -> f vid k v acc') vidm acc
    ) m zero

  let map f m =
    fold (fun vid k v acc -> add vid k (f vid k v) acc) m empty

  (* update from a certain vid onwards *)
  let update_suffix (vid:OrdVid.t) (k:OrdKey.t) (f:'a -> 'a) (m:'a t) : 'a t =
    map (fun vid' k' v ->
      if OrdKey.compare k' k > 0 && OrdVid.compare vid' vid >=0 then f v
      else v) m

  let iter f m = fold (fun vid k v _ -> f vid k v) m ()

  let filter f m =
    fold (fun vid k v acc ->
      if f vid k v then add vid k v acc else acc
    ) m empty

  let update t k v t' k' v' m =
    let m' = remove t k m in
    add t' k' v' m'

  let peek m = match HMap.peek m with
    | None -> None
    | Some (k, vidm) -> match VIDMap.peek vidm with
      | None -> None
      | Some (vid, v) -> Some (vid, k, v)

  let size m = fold (fun _ _ _ acc -> acc + 1) m 0

  let to_list mm = fold (fun vid k v acc -> (vid, k, v)::acc) mm []

  let compare f m m' =
    HMap.compare (fun a b -> VIDMap.compare f a b) m m'

end
