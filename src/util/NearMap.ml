(* ---- NearMap (a Map with nearest lookup) ---- *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val mem:  key -> 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val update_with: ?fn:(key -> 'a t -> 'a) -> key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton: key -> 'a -> 'a t
    val remove: key -> 'a t -> 'a t
    val merge:
          (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    val exists: (key -> 'a -> bool) -> 'a t -> bool
    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal: 'a t -> int
    val bindings: 'a t -> (key * 'a) list
    val min_binding: 'a t -> (key * 'a)
    val max_binding: 'a t -> (key * 'a)
    val choose: 'a t -> (key * 'a)
    val split: key -> 'a t -> 'a t * 'a option * 'a t
    val find: key -> 'a t -> 'a
    val find_gt: key -> 'a t -> key * 'a
    val find_geq: key -> 'a t -> key * 'a
    val find_lt: key -> 'a t -> key * 'a
    val find_leq: key -> 'a t -> key * 'a
    val filter_gt: key -> 'a t -> 'a t
    val filter_geq: key -> 'a t -> 'a t
    val filter_lt: key -> 'a t -> 'a t
    val filter_leq: key -> 'a t -> 'a t
    val find_range : key -> key -> 'a t -> 'a list
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
    val peek : 'a t -> (key * 'a) option
    val combine : 'a t -> 'a t -> 'a t
    val update : key -> 'a -> key -> 'a -> 'a t -> 'a t
  end

module Make(Ord: OrderedType) = struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of 'a t * key * 'a * 'a t * int

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let singleton x d = Node(Empty, x, d, Empty, 1)

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec add x data = function
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            Node(l, x, data, r, h)
          else if c < 0 then
            bal (add x data l) v d r
          else
            bal l v d (add x data r)

    let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    type comp = GT | GTEQ | LT | LTEQ

    (* find the one value just greater than/less than x *)
    let find_comp comp x m =
      let rec loop last m =
        match m, last with
        | Empty, None   -> raise Not_found
        | Empty, Some x -> x
        | Node(l, v, d, r, _), _ ->
            let op, s, t = match comp with
              | GT   -> ((>):int -> int -> bool),  l, r
              | GTEQ -> (>=), l, r
              | LT   -> (<),  r, l
              | LTEQ -> (<=), r, l
            in
            let c = Ord.compare v x in
            if op c 0 then loop (Some(v,d)) s
            else loop last t
      in loop None m

    let find_gt x m = find_comp GT x m
    let find_lt x m = find_comp LT x m
    let find_geq x m = find_comp GTEQ x m
    let find_leq x m = find_comp LTEQ x m

    (* find all values between x and y, exclusive *)
    let find_range x y m =
      let rec loop = function
        | Empty -> []
        | Node(l, v, d, r, _) ->
            let comp f a b = f (Ord.compare a b) 0 in
            let (<<) = comp (<) in
            let d' = if x << v && v << y then [d] else [] in
            let lacc = if x << v then loop l else [] in
            let racc = if v << y then loop r else [] in
            d' @ lacc @ racc
      in loop m

    let rec mem x = function
        Empty -> false
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, r, _) -> (x, d)
      | Node(l, x, d, r, _) -> min_binding l

    let rec max_binding = function
        Empty -> raise Not_found
      | Node(l, x, d, Empty, _) -> (x, d)
      | Node(l, x, d, r, _) -> max_binding r

    let rec remove_min_binding = function
        Empty -> invalid_arg "Map.remove_min_elt"
      | Node(Empty, x, d, r, _) -> r
      | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove x l) v d r
          else
            bal l v d (remove x r)

    let update_with ?(fn=find) key f m =
      let v = try Some(fn key m)
              with Not_found -> None in
      match f v with
      | None   -> remove key m
      | Some v -> add key v m

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter f l; f v d; iter f r

    let rec map f = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let l' = map f l in
          let d' = f d in
          let r' = map f r in
          Node(l', v, d', r', h)

    let rec mapi f = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let l' = mapi f l in
          let d' = f v d in
          let r' = mapi f r in
          Node(l', v, d', r', h)

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold f r (f v d (fold f l accu))

    let rec for_all p = function
        Empty -> true
      | Node(l, v, d, r, _) -> p v d && for_all p l && for_all p r

    let rec exists p = function
        Empty -> false
      | Node(l, v, d, r, _) -> p v d || exists p l || exists p r

    (* Beware: those two functions assume that the added k is *strictly*
       smaller (or bigger) than all the present keys in the tree; it
       does not test for equality with the current min (or max) key.

       Indeed, they are only used during the "join" operation which
       respects this precondition.
    *)

    let rec add_min_binding k v = function
      | Empty -> singleton k v
      | Node (l, x, d, r, h) ->
        bal (add_min_binding k v l) x d r

    let rec add_max_binding k v = function
      | Empty -> singleton k v
      | Node (l, x, d, r, h) ->
        bal l x d (add_max_binding k v r)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v d r =
      match (l, r) with
        (Empty, _) -> add_min_binding v d r
      | (_, Empty) -> add_max_binding v d l
      | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
          if lh > rh + 2 then bal ll lv ld (join lr v d r) else
          if rh > lh + 2 then bal (join l v d rl) rv rd rr else
          create l v d r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          join t1 x d (remove_min_binding t2)

    let concat_or_join t1 v d t2 =
      match d with
      | Some d -> join t1 v d t2
      | None -> concat t1 t2

    let rec split x = function
        Empty ->
          (Empty, None, Empty)
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then (l, Some d, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
          else
            let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)

    let rec merge f s1 s2 =
      match (s1, s2) with
        (Empty, Empty) -> Empty
      | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
          let (l2, d2, r2) = split v1 s2 in
          concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
      | (_, Node (l2, v2, d2, r2, h2)) ->
          let (l1, d1, r1) = split v2 s1 in
          concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
      | _ ->
          assert false

    let rec filter p = function
        Empty -> Empty
      | Node(l, v, d, r, _) ->
          (* call [p] in the expected left-to-right order *)
          let l' = filter p l in
          let pvd = p v d in
          let r' = filter p r in
          if pvd then join l' v d r' else concat l' r'

    let filter_comp comp x m =
      let rec loop = function
        | Empty -> Empty
        | Node(l, v, d, r, _) ->
            let op, do_l, do_r, other =  match comp with
              | GT   -> ((>):int -> int -> bool),  true, false, r
              | GTEQ -> (>=), true, false, r
              | LT   -> (<),  false, true, l
              | LTEQ -> (<=), false, true, l
            in
            let c = Ord.compare v x in
            if op c 0 then
              join (if do_l then loop l else l) v d (if do_r then loop r else r)
            else
              loop other
      in loop m

    let filter_lt x m = filter_comp LT x m
    let filter_gt x m = filter_comp GT x m
    let filter_geq x m = filter_comp GTEQ x m
    let filter_leq x m = filter_comp LTEQ x m

    let rec partition p = function
        Empty -> (Empty, Empty)
      | Node(l, v, d, r, _) ->
          (* call [p] in the expected left-to-right order *)
          let (lt, lf) = partition p l in
          let pvd = p v d in
          let (rt, rf) = partition p r in
          if pvd
          then (join lt v d rt, concat lf rf)
          else (concat lt rt, join lf v d rf)

    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = Ord.compare v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)

    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            Ord.compare v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)

    let rec cardinal = function
        Empty -> 0
      | Node(l, _, _, r, _) -> cardinal l + 1 + cardinal r

    let rec bindings_aux accu = function
        Empty -> accu
      | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

    let bindings s =
      bindings_aux [] s

    let choose = min_binding

    let to_list m = List.rev @@ fold (fun k v acc -> (k,v)::acc) m []
    let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l

    let peek m = try Some(choose m) with Not_found -> None

    let combine m m' = merge (fun _ mx my -> match mx, my with
      | _, Some _ -> my
      | Some _, _ -> mx
      | _         -> None) m m'

    let update oldk oldv k v m = add k v @@ remove oldk m

end

module Int = struct type t = int let compare = (-) end

module VM = Make(Int)

let test =
  let (===) x y = VM.compare (String.compare) x y = 0 in

  let m = VM.of_list
    [2, "2"; 3, "3"; 4, "4"; 5, "5"; 10, "10"; 20, "20"; 30, "30"] in

  let n = VM.filter_lt 4 m in
  let o = VM.of_list [2, "2"; 3, "3"] in
  assert (n === o);

  let n = VM.filter_leq 3 m in
  let o = VM.of_list [2, "2"; 3, "3"] in
  assert (n === o);

  let n = VM.filter_gt 10 m in
  let o = VM.of_list [20, "20"; 30, "30"] in
  assert (n === o);

  let n = VM.filter_geq 10 m in
  let o = VM.of_list [10, "10"; 20, "20"; 30, "30"] in
  assert (n === o)




