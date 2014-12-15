(* Utilities that are useful *)

let _ = Random.self_init ()

module IntSet = Set.Make(struct type t = int let compare = (-) end)
module IntMap = Map.Make(struct type t = int let compare = (-) end)
module StrMap = Map.Make(struct type t = string let compare = String.compare end)

(* abbreviations for annoyingly long functions *)
let foi = float_of_int
let iof = int_of_float
let soi = string_of_int
let sof = string_of_float
let ios = int_of_string
let fos = float_of_string
let bos = bool_of_string
let sob = string_of_bool

let const a _ = a

(* Either type *)
type ('a, 'b) either_t = Left of 'a | Right of 'b

(* low precedence function application allows us to remove
 * many () from our code *)
let (@:) f x = f x;;

let compose f g = fun x -> f (g x)

let flip f a b = f b a

let id_fn a = a

let (|-) = compose

let pair x y = (x,y)

let null l = match l with [] -> true | _ -> false

let at l i = List.nth l i

let fst_many l = fst @: List.split l

let snd_many l = snd @: List.split l

(* apply to one part of the tuple, and send the other part through *)
let first  f (x,y) = f x, y
let second f (x,y) = x, f y

let first3  f (x,y,z) = f x, y, z
let second3 f (x,y,z) = x, f y, z
let third3  f (x,y,z) = x, y, f z

let singleton x = [x]

(* take the first x elements of a list *)
let list_take len li =
  let rec take len2 li2 acc_list =
    if len2 >= len then acc_list
    else match li2 with
    | [] -> acc_list
    | head::tail -> take (len2+1) tail (head::acc_list)
  in
  List.rev (take 0 li [])

(* drop the first x elements of a list *)
let rec list_drop len li = match li with
  | [] -> []
  | x::xs when len = 0 -> li
  | x::xs -> list_drop (len-1) xs

(* split list into before and after *)
let list_split len l =
  let rec split len acc = function
    | [] -> acc, []
    | x::xs when len = 0 -> List.rev (x::acc), xs
    | x::xs -> split (len-1) (x::acc) xs
  in split len [] l

(* take the last x elements of a list *)
let list_take_end len li = list_drop (List.length li - len) li

(* drop from the end of a list *)
let list_drop_end len li = list_take (List.length li - len) li

(* tail-recursive and tolerates different lengths *)
let list_zip l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | _, [] | [], _   -> List.rev acc
    | x::xs, y::ys    -> loop ((x,y)::acc) xs ys
  in loop [] l1 l2

let list_unzip l =
  let rec loop acc acc' = function
    | []         -> (List.rev acc, List.rev acc')
    | (x,x')::xs -> loop (x::acc) (x'::acc') xs
  in loop [] [] l

let list_fold2 f zero l1 l2 =
  let rec loop acc l1 l2 = match l1, l2 with
    | [], []       -> acc
    | x::xs, y::ys -> loop (f acc x y) xs ys
    | _ -> invalid_arg "mismatching lists"
  in loop zero l1 l2

let hd l = match l with
  | x::_ -> x
  | _ -> invalid_arg "empty list"

let hd' = function
  | x::_ -> Some x
  | _    -> None

let tl l = match l with
  | _::x -> x
  | _ -> invalid_arg "empty list or singleton"

let rec list_last xs = match xs with
  | []    -> failwith "empty list"
  | [x]   -> x
  | _::xs -> list_last xs

let replicate n x =
  let rec loop n acc = match n with
    | 0 -> acc
    | _ -> loop (n-1) (x::acc)
  in loop n []

(* will only remove one instance of x in xs (as opposed to filter) *)
let list_remove r l =
  let rec loop acc = function
    | x::xs when x = r -> List.rev_append acc xs
    | x::xs -> loop (x::acc) xs
    | []    -> List.rev acc
  in loop [] l

let list_replace r r' l =
  let rec loop acc = function
    | x::xs when x = r -> List.rev_append acc (r'::xs)
    | x::xs -> loop (x::acc) xs
    | []    -> List.rev acc
  in loop [] l

let list_modify n f l =
  let rec loop i acc = function
    | x::xs when i = 0 -> List.rev_append acc @@ (f x)::xs
    | x::xs -> loop (i-1) (x::acc) xs
    | []    -> List.rev acc
  in loop n [] l

let compose_fn f g x = f(g x)

(* function that folds until a predicate is true *)
let rec foldl_until f acc = function
    | x::xs ->
        begin match f acc x with
        | Left a -> a
        | Right acc' -> foldl_until f acc' xs
        end
    | [] -> acc

(* filter list by a set of indices (starting at 0) *)
(* assume a small list *)
let list_filter_idxs_by_set idxs l =
  snd @:
    List.fold_right (fun x (i, acc) ->
      if IntSet.mem i idxs then (i+1, x::acc)
      else (i+1, acc)
    ) l (0, [])

(* this version preserves index ordering *)
let list_filter_idxs_by_list idxs l =
  let a = Array.of_list l in
  List.map (fun i -> a.(i)) idxs

(* I/O helpers *)
(* read a file and convert it into lines *)
let read_file_lines file =
  let in_chan = open_in file in
  let rec read_lines acc =
    let input = try Some(input_line in_chan) with End_of_file -> None in
    match input with
    | None   -> acc
    | Some x -> read_lines (x::acc)
  in
  let ls = List.rev @: read_lines [] in
  close_in in_chan; ls

let read_file file = String.concat "\n" @: read_file_lines file

let write_file file s =
  let out_chan = open_out file in
  output_string out_chan s;
  close_out out_chan

let write_file_lines file l =
  let out_chan = open_out file in
  List.iter (output_string out_chan) l;
  close_out out_chan

(* make a range from first to last. Tail recursive *)
let create_range ?(step=1) first length =
    let last = first + ((length-1) * step) in
    let rec range_inner index acc =
        if index > last then acc
        else range_inner (index+step) (index::acc)
    in
    List.rev(range_inner first [])

(* make a range that corresponds to a given list *)
let create_corr_range first xs = create_range first @: List.length xs

let insert_index_fst ?(first=0) xs =
    let is = create_corr_range first xs in
    list_zip is xs

let insert_index_snd ?(first=0) xs =
    let is = create_corr_range first xs in
    list_zip xs is

(* tail recursive, so more efficient than mapping alone *)
let list_map f l = List.rev @: List.rev_map f l

(* get an index with every item in a map *)
let list_mapi f l = List.rev @: snd @: List.fold_left
  (fun (i,acc) x -> i+1, (f (i,x))::acc) (0,[]) l

(* a cross between a map and a fold. Can only map the current list, but also
 * gets another value to play with, and no need to project out the temporary
 * value *)
let mapfold fn init l =
  let a, b =
    List.fold_left (fun (v, acc) x ->
      let v', x' = fn v x in
      v', x'::acc
    ) (init, []) l
  in a, List.rev b

let filter_map fn l =
  List.rev @:
    List.fold_left (fun acc x ->
      match fn x with
      | None   -> acc
      | Some y -> y::acc
    ) [] l

(* calls f on its output over and over, num times *)
let iterate f init num =
  let rec loop acc = function
    | i when i <= 0 -> acc
    | i -> loop (f acc) (i-1)
  in loop init num

(* calls f on its output over and over again until p is true *)
let iterate_until f init =
  let rec loop acc =
    match f acc with
    | Left  a -> a
    | Right b -> loop b
  in loop init

(* repeat a function many times, building a list from indices *)
(* do this without instantiating the index list *)
let list_populate f init num =
  List.rev @: snd @: iterate
    (fun (i, acc) -> i+1, (f i)::acc)
    (init, [])
    num

(* transform a list into a list of lists of i elements *)
(* if there aren't enough elements to fill the last list, it's filled as much as
 * possible *)
let list_bunch i l =
  let rec loop acc l =
    match l with
    | [] -> acc
    | _  -> let taken = list_take i l in
      match list_drop i l with
      | []    -> taken::acc
      | xs    -> loop (taken::acc) xs
  in List.rev @: loop [] l

(* intersperse 2 lists together. When one runs out, continue with the other *)
let list_intersperse la lb =
  let rec loop acc l1 l2 = match l1, l2 with
    | x::xs, y::ys -> loop (y::x::acc) xs ys
    | x::xs, []    -> loop (x::acc) xs []
    | [],    y::ys -> loop (y::acc) [] ys
    | [], []       -> acc
  in List.rev @: loop [] la lb

let list_intercalate v = function
  | [] -> []
  | l  -> let rec loop acc = function
            | x::xs -> loop (x::v::acc) xs
            | []    -> acc
          in List.rev @: loop [hd l] (tl l)

(* functions without exceptions *)
let list_find f l = try Some(List.find f l) with Not_found -> None
let find fn k m = try Some(fn k m) with Not_found -> None

(* find the maximum element of a list according to a transformation function *)
let list_minmax op f l = match l with
  | [x]   -> (x, f x)
  | x::xs ->
    List.fold_left
      (fun acc m -> let n = f m in
                    if op n (snd acc) then (m,n) else acc)
      (x, f x) xs
  | _     -> invalid_arg "Empty list"

let list_max_op f l = list_minmax (>) f l
let list_min_op f l = list_minmax (<) f l

let list_min l = List.fold_left (fun acc x ->
  if x < acc then x else acc)
  (hd l)
  (tl l)

let list_max l = List.fold_left (fun acc x ->
  if x > acc then x else acc)
  (hd l)
  (tl l)

(* modify/add to/remove_from an association list generically *)
let assoc_modify f item l =
  let mod_or_delete m_value reduced_l =
    match f m_value with
    | None   -> reduced_l
    | Some x -> (item, x)::reduced_l
  in
  let found, l =
    List.fold_left (fun (found, acc) ((k,v) as x) ->
      if k = item then true, mod_or_delete (Some v) acc
      else found, x::acc
    ) (false, []) l
  in
  let l = if found then l else mod_or_delete None l in
  List.rev l

(* perform a join on 2 association lists. Assumes uniqueness in the keys *)
let assoc_join l1 l2 =
  let hash = Hashtbl.create (List.length l2) in
  List.iter (fun (k, v) -> Hashtbl.replace hash k v) l2;
  filter_map (fun (k, v) ->
    try
      Some (k, (v, Hashtbl.find hash k))
    with Not_found -> None
  ) l1

(* perform a cross-product on 2 lists. Doesn't preserve order *)
let cartesian_product l1 l2 =
  List.fold_left (fun acc x ->
      List.fold_left (fun acc' y -> (x,y)::acc')
        acc
        l2
    )
    []
    l1

(* efficient function to get unique entities *)
let nub xs =
    let h = Hashtbl.create 50 in
    List.rev @:
      List.fold_left (fun acc x ->
        if Hashtbl.mem h x then acc
        else (Hashtbl.replace h x (); x::acc)
      ) [] xs

(* --- Array function --- *)

let array_find pred arr =
    let l = Array.length arr and index = ref 0 and found = ref false in
    while not !found && !index < l do
        found := pred arr.(!index);
        index := !index + 1;
    done;
    index := !index - 1;
    if not !found then raise Not_found
    else !index, arr.(!index)

(* map an array to a list *)
let array_map f arr =
  List.rev @: Array.fold_left (fun acc x -> (f x)::acc) [] arr

(* wrap with some *)
let some x = Some(x)

let is_some = function None -> false | Some _ -> true

(* unwrap a some. Fail if not a Some *)
let unwrap_some = function None -> failwith "Not a Some" | Some x -> x

let maybe def f = function None -> def | Some x -> f x

let maybe_f def f = function None -> def () | Some x -> f x

(* flatten a list of maybes into a list *)
let flatten_some l = List.rev @:
  List.fold_left (fun acc -> function
    | Some x -> x::acc
    | None   -> acc
  ) [] l

(* --- String functions --- *)
(* split a string into lines *)
let lines s = Str.split (Str.regexp "\\(\n\\|\n\r\\)+") s

let unlines l = String.concat "\n" l

let words s = Str.split (Str.regexp "[\t ]+") s

let unwords l = String.concat " " l

let str_take i s = let l = String.length s in
  let i' = if i > l then l else i in
  Str.first_chars s i'

let str_drop i s = let l = String.length s in
  let i' = if i > l then l else i in
  Str.string_after s i'

let str_drop_end i s = let l = String.length s in
  let i' = if i > l then 0 else (l-i) in
  Str.first_chars s i'

let str_take_end i s = let l = String.length s in
  let i' = if i > l then l else i in
  Str.last_chars s i'

(* --- regexp helpers --- *)

(* returns a list of groups of a regexp *)
let r_groups str ~r ~n =
  if Str.string_match r str 0 then
    list_map (fun i ->
      try
        begin match Str.matched_group i str with
        | "" -> None
        | x  -> Some x
        end
      with
      | Not_found -> None
      | Invalid_argument _ -> invalid_arg @: Printf.sprintf "Bad group %d for string %s" i str
    ) @: create_range 1 n
  else []

let r_match r str = Str.string_match r str 0

(* --- other stuff ---- *)

(* make a list contain given number of given element *)
let  make_lst element num =
  let rec helper lst num =
    if num = 0 then lst
    else helper (element::lst) (num-1)
  in
  helper [] num

(* transpose a list of lists *)
let transpose l =
  List.rev @: fst @: List.fold_right (fun _ (acc,rem) ->
    let newl, rem =
      List.fold_right (fun x (acc2,rem2) ->
        match x with
        | []    -> failwith "List is too short"
        | x::xs -> x::acc2, xs::rem2
      )
      rem
      ([],[])
    in
    newl::acc, rem
  )
  (List.hd l)
  ([], l)

(* convert date string to integer *)
let r_date = Str.regexp "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)"
let int_of_sql_date s = match r_groups s ~n:3 ~r:r_date with
  | [Some y; Some m; Some d] ->
      (ios y)*10000 + (ios m)*100 + (ios d)
  | l -> invalid_arg @: Printf.sprintf "int_of_sql_date for string %s. Found only %i members" s (List.length l)

  (* extract a part of the date, as represented by an integer *)
let date_part p i = match p with
  | "day"   -> i mod 100
  | "month" -> (i mod 10000) / 100
  | "year"  -> i / 10000
  | _       -> failwith @@ "date_part: unknown param "^p

(* replace a hash value by looking at the old value, if any *)
let hashtbl_replace hash key replace_fn =
  let old = try Some(Hashtbl.find hash key)
            with Not_found -> None
  in
  Hashtbl.replace hash key (replace_fn old)

let hashtbl_combine h h' combine_fn =
  Hashtbl.iter (fun k v' ->
    try
      let v = Hashtbl.find h k in
      Hashtbl.replace h k @@ combine_fn v v'
    with Not_found ->
      Hashtbl.add h k v'
  ) h'


let intset_of_list l =
  List.fold_left (fun acc x -> IntSet.add x acc) IntSet.empty l

let string_of_int_list l = String.concat ", " @@ List.map soi l
let string_of_int_set  s = String.concat ", " @@ List.map soi @@ IntSet.elements s

