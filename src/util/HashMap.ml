open Util
       
(* ----- HashMap: performant map ----- *)

module type S = sig
    type key
    type 'a t
    val empty : 'a t
    val singleton : key -> 'a -> 'a t
    val is_empty : 'a t -> bool
    val insert : key -> 'a -> 'a t -> 'a t
    val delete : key -> 'a t -> 'a t
    val find : key -> 'a t -> 'a value
    val combine : 'a t -> 'a t -> 'a t
    val fold : (key -> 'a -> 'b) -> 'a t -> 'b -> 'b
    val map : (key -> 'a -> 'b) -> 'a t -> 'b t
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val iter2 : (key -> 'a option -> 'b option -> unit) -> 'a t -> 'b t -> unit
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val update : key -> 'a -> key -> 'a -> 'a t -> 'a t
    val peek : 'a t -> (key * 'a) option
    val to_list : 'a t -> (key * 'a) list
    val from_list : (key * 'a) list -> 'a t
end
		  
module Make(Ord : ICommon.OrderedKeyType) = struct
  
  module IntMap = Map.Make(struct
    type t = int let compare x y = x - y
  end
			     
  module HashMap = Map.Make(Ord)
			   
  type key = Ord.t
  type 'a t = 'a InnerMap.t IntMap.t
  
  let empty = IntMap.empty
		
  let singleton k v = IntMap.singleton (hash k) @@
                        InnerMap.singleton k v
					   
  let is_empty x = x = empty
			 
  let insert k v m = 
    let h = hash k in
    try
      let im = IntMap.find h m in
      IntMap.insert h (InnerMap.insert k v im) m
    with Not_found ->
      IntMap.insert h (InnerMap.singleton k v) m
		    
  let delete k m =
    let h = hash k in
    try
      let im  = IntMap.find h m in
      let im' = InnerMap.delete k im in
      if InnerMap.is_empty im' then
        IntMap.delete h m
      else IntMap.insert h im' m
    with Not_found -> m
			
  let find k m =
    let h = hash k in
    let im = IntMap.find h m in
    InnerMap.find k im
       
  let combine x y =
    let innerMerge k mx my = match mx, my with
      | _, Some y -> Some y
      | Some x, _ -> Some x
      | _         -> None
    in
    let outerMerge k mx my = match mx, my with
      | Some x, Some y -> Some(InnerMap.merge innerMerge x y)
      | Some x, _      -> Some x
      | _, Some y      -> Some y
      | _              -> None
    in IntMap.merge outerMerge x y
		    
  let fold f zero m =
    IntMap.fold (fun _ im acc ->
      InnerMap.fold f im acc)
      m zero
      
  let map f m = fold insert m
		     
  let iter f m = IntMap.iter (fun _ im -> InnerMap.iter f im) m
			     
  let iter2 f m =
    let innerMerge k mx my = match mx, my with
      | None, None -> None
      | _          -> f k mx my; None
    in
    let outerMerge k mx my = match mx, my with
      | Some x, Some y -> InnerMap.merge innerMerge x y; None
      | Some x, None   ->
          InnerMap.iter (fun k v -> f k (Some v) None) x; None
      | None, Some y   ->
          InnerMap.iter (fun k v -> f k None (Some v)) y; None
      | _              -> None
    in IntMap.merge outerMerge x y
		    
  let filter f m =
    fold (fun k v acc -> if f k v then insert k v acc else acc)
    IntMap.empty m
    
  let update k v k' v' m = insert k' v' @@ delete k v m
						  
  let peek m = choose @@ choose m
				
  let to_list m = fold (fun k v acc -> (k,v)::acc) [] m
		       
  let of_list l = List.fold_left (fun acc (k, v) ->
		    insert k v acc) empty l
end

