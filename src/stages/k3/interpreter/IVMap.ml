open Util
open K3.AST
module KP = K3Printing

(* ------ VMap functions ------ *)
(* NOTE: remove_prefix: we delete up to and including vid, and keep a frontier at vid
 *       update_suffix: updates the > vid and all following vids
 *       frontier: gets values < the vid only
 *         this is critical to prevent fetch/push from reading data that was updated in the
 *         same step, but isn't supposed to be available yet.
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
  val remove_prefix : vid -> key -> 'a t -> 'a t
  val combine : 'a t -> 'a t -> 'a t
  val fold : (vid -> key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map : (vid -> key -> 'a -> 'b) -> 'a t -> 'b t
  val iter : (vid -> key -> 'a -> unit) -> 'a t -> unit
  val filter : (vid -> key -> 'a -> bool) -> 'a t -> 'a t
  val update : vid -> key -> 'a -> key -> 'a -> 'a t -> 'a t
  val update_with : ?frontier:bool -> vid -> key -> ('a option -> 'a option) -> 'a t -> 'a t
  val update_suffix : vid -> key -> ('a -> 'a ) -> 'a t -> 'a t
  val peek : 'a t -> (vid * key * 'a) option
  val to_list : 'a t -> (vid * key * 'a) list
  val of_list : (vid * key * 'a) list -> 'a t
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

  let update_with ?(frontier=false) (vid:vid) (k:key) (f:'a option -> 'a option) (m:'a t) : 'a t=
    HMap.update_with k (function
      | None -> begin match f None with
          | None   -> None
          | Some v -> some @@ VIDMap.singleton vid v
          end
      | Some vidmap ->
          let find_lt key m = snd @@ VIDMap.find_lt key m in
          let fn =
            if frontier then find_lt
            else VIDMap.find in
          let vidmap' = VIDMap.update_with ~fn vid f vidmap in
          if VIDMap.is_empty vidmap' then None
          else Some vidmap'
    ) m

  (* get the frontier for a slice (must read entire domain) *)
  let frontier_slice vid m =
    HMap.fold (fun k vidmap acc ->
      try
        let vid', v = VIDMap.find_lteq vid vidmap in
        add vid' k v acc
      with Not_found -> acc
    ) m empty

  let singleton vid k v = HMap.singleton k (VIDMap.singleton vid v)

  (* get the frontier at a specific key *)
  let frontier_point vid k m =
    let vidmap = HMap.find k m in
    let vid', v = VIDMap.find_lt vid vidmap in
    singleton vid' k v

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
  let remove_prefix vid key (m: 'a t) =
    let vidmap = HMap.find key m in
    let keep = ref None in
    let vidmap = VIDMap.filter (fun t v ->
      if OrdVid.compare t vid <= 0 then begin
        (match !keep with
        | None    -> keep := Some(t,v)
        | Some (t',v') -> if OrdVid.compare t t' > 0 then keep := Some(t, v));
        false
      end else true) vidmap
    in
    (* add back frontier value *)
    let vidmap = match !keep with
    | None      -> vidmap
    | Some(t,v) -> VIDMap.add t v vidmap
    in
    HMap.add key vidmap m

  let fold f m zero =
    HMap.fold (fun k vidm acc ->
      VIDMap.fold (fun vid v acc' -> f vid k v acc') vidm acc
    ) m zero

  let map f m =
    fold (fun vid k v acc -> add vid k (f vid k v) acc) m empty

  (* update from a certain vid onwards *)
  let update_suffix (vid:OrdVid.t) (k:OrdKey.t) (f:'a -> 'a) (m:'a t) : 'a t =
    map (fun vid' k' v ->
      if OrdKey.compare k' k = 0 && OrdVid.compare vid' vid > 0 then f v
      else v) m

  let iter f m = fold (fun vid k v _ -> f vid k v) m ()

  let filter f m =
    fold (fun vid k v acc ->
      if f vid k v then add vid k v acc else acc
    ) m empty

  let update t k v k' v' m =
    let m' = remove t k m in
    add t k' v' m'

  let peek m = match HMap.peek m with
    | None -> None
    | Some (k, vidm) -> match VIDMap.peek vidm with
      | None -> None
      | Some (vid, v) -> Some (vid, k, v)

  let size m = fold (fun _ _ _ acc -> acc + 1) m 0

  let to_list mm = fold (fun vid k v acc -> (vid, k, v)::acc) mm []

  let of_list l =
    List.fold_left (fun acc (vid, k, v) -> add vid k v acc) empty l

  let compare f m m' =
    HMap.compare (fun a b -> VIDMap.compare f a b) m m'

end

(*** testing ***)

module Int =
  struct type t = int let compare = (-) let hash = Hashtbl.hash let to_string = soi end

module VM = Make(Int)(Int)

let test =
  let (===) x y = VM.compare (String.compare) x y = 0 in

  let m = VM.add 1 1 "1-1" @@ VM.empty in
  let n = VM.singleton 1 1 "1-1" in
  assert (n === m);

  let m =
    VM.of_list [2,1,"2-1"; 1,10,"1-10"; 3,20,"3-20"; 1,20,"1-20"; 4,1,"4-1";
                15,1,"15-1"]
  in
  let n = VM.frontier_point 10 20 m in
  let o = VM.singleton 3 20 "3-20" in
  assert (n === o);

  let n = VM.frontier_slice 2 m in
  let o = VM.of_list [1, 20, "1-20"; 1, 10, "1-10"; 2, 1, "2-1"] in
  assert (n === o);

  let n = VM.update_suffix 2 1 (fun s -> s^"boo") m in
  let o = VM.add 4 1 "4-1boo" @@ VM.add 15 1 "15-1boo" @@ VM.remove 4 1 @@ VM.remove 15 1 m in
  assert (n === o);

  let n = VM.remove_prefix 5 1 m in
  let o = VM.remove 2 1 m in
  assert (n === o)






