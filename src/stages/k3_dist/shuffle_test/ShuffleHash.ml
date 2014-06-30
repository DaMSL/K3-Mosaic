(* Shuffle with Consistent Hashing *)

open Util

(*
 * m_to_n_pat = positions of m's keys in n's schema (pregenerated for each mn)
 * node_to_ip: translate node number to IP
 * bmod = (int * int) list = (index * bucket modulo factor) list
 * (pre-generated per map), order determines order of bucketing
*)

(* for each binding pattern, keep a subspace of buckets corresponding to
 * the unbound dimensions of the binding pattern.
 * space : (index * value) list list
 *
 * unbound_pat_space = ((int list) * (int * value) list list) list
 *)

(*
 * Note: this is a brute force approach to finding the possible destinations. An
 * optimized method would use i:j:k:l information to reduce the search space
 *)

(* utility function to make a list of a range over integers *)
let range first last =
    let rec range_inner index acc =
        if index = last then acc@[index]
        else range_inner (index+1) (acc@[index])
    in
		if first > last then invalid_arg "Range has first > last"
    else range_inner first []
;;

(* utility implementation of group_by_aggregate. Could be more efficient with
 * hashmaps *)
let group_by_aggregate (agg_fn:'a->'b->'a) (init:'a) (group_fn:'b->'c)
(collection:'b list) : ('c * 'a) list =
    List.fold_left
        (fun acc_groups i ->
            let (match_group, mismatch_groups) =
                List.partition
                    (fun (tag, group) -> (group_fn i)=tag)
                    acc_groups
            in
            match match_group with
            | [] -> (group_fn i, agg_fn init i) :: mismatch_groups
            | [(tag, group)] -> (tag, agg_fn group i) :: mismatch_groups
            | head::tail -> raise (Invalid_argument "too many matches in
                aggregate function")
        )
        []
        collection
;;

(* Example node_to_ip. Would normally look up in a table *)
(* For now, we just use an identity function *)
let node_to_ip node = node

(*
 * Pre-calculate the contribution of every dimension to the total linear
 * function. Each dimension contributes the product of all previous
 * dimension buckets. This calculation does not depend on anything but bmod
 * and can be taken outside of the function.
 * Like bmod, dims are ordered by their position in the list
 *)
let calc_dim_bounds bmod =
  fst @: List.fold_left
    (fun (xs,acc) (pos, bin_size) -> (xs@[pos, acc], bin_size * acc))
    ([],1)
    bmod

  (* run over the m_key input and calculate the value that the bound variables contribute
  * the bound values serve as our constant. We can pre-calculate their contribution
  * to the node location and use that for every possible node that results from
  * unbound values, so this calculation should only be done once.
  * For each bound value, we multiply it by the bucket of every previous dimension
  * obtained in dim_boundaries *)
let calc_bound_bucket bmod dim_bounds key =
  fst @: List.fold_left
    (fun (acc,index) v -> match v with
      | Some x -> (try
            let value = (Hashtbl.hash x) mod (List.assoc index bmod) in
            let bucket_size = List.assoc index dim_bounds in
            (acc + value * bucket_size, index + 1)
          with Not_found -> (acc, index+1)) (* unpartitioned *)
      | None   -> (acc, index+1) (* no key value ie. unbound *)
    )
    (0,0)
    key

(* make a list of lists of ranges 0...b_i for each unbound_dim.
* This is every possible value in each unbound dimension
*)
let calc_unbound_domains bmod key : (int * int list) list =
  (* find which dimensions and buckets are not specified in the pattern
   * (unbound) but are used for partitioning *)
  let unbound_dims bmod key =
    try
      List.filter
        (fun (i,_) -> match List.nth key i with None -> true | _ -> false) bmod
    with Failure(_) -> invalid_arg "key too short"
  in
  List.map (fun (i, b_i) -> (i, range 0 @: b_i-1)) (unbound_dims bmod key)

(* This is a cartesian product over every one of the unbound dimensions
 * The result is a list of lists of [dim, val, dim, val...]
 * We're enumerating all possible combinations of values for all unbound
 * dimensions, which will gives us the resulting slice.
 * The way to do this is to accumulate a cartesian product with every step
 * of the fold_left loop.
 *
 * This should be precomputed once per pattern, rather than repeated
 * per invocation of route.
 *)
let calc_unbound_cart_prod (unbound_domains:(int * int list) list) : (int * int) list list =
  List.fold_left
    (fun prev_cart_prod (i, (domain:int list)) -> List.flatten
      (List.map
        (* for every domain element in the domain *)
        (fun (domain_element:int) -> match prev_cart_prod with
          | [] -> [[(i,domain_element)]]
          | _  -> List.map
                   (* add current element to every previous sublist *)
                   (fun rest_tup -> rest_tup@[i,domain_element])
                   prev_cart_prod
        )
        domain
      )
    )
    []
    unbound_domains

(* function to calculate the full value of a possible bucket *)
let full_bucket_calc dim_bounds unbound_bucket bound_bucket : int =
  List.fold_left
    (fun acc (i,v) -> acc + v * List.assoc i dim_bounds)
    bound_bucket (* start with this const *)
    unbound_bucket

(* We now add in the value of the bound variables as a constant
 * and calculate the result for every possibility
 *)
let calc_unbound_ip_list bound_bucket unbound_cart_prod dim_bounds =
  let ip_list =
    group_by_aggregate (fun acc ip -> ip::acc) [] (fun ip -> ip) @:
      List.map
        (fun (unbound_bucket:(int * int) list) ->
          Ring.get_node_for
            (full_bucket_calc dim_bounds unbound_bucket bound_bucket)
        )
        (unbound_cart_prod:((int * int) list list))
  in
  match ip_list with
  | [] -> [node_to_ip (Ring.get_node_for bound_bucket)]
  | _ -> List.map fst ip_list (* we only want ips ie the group tag *)

(* Returns a list of ips *)
(* Route is specialized for a specific bmod *)
(* m_key = m's parameters
 *     e.g. [Some 10; None] (in reality, this has to be a tuple since it
 *     supports different types
 *
 * bmod = (int * int) list = (dim_index * bucket_size) list
 *   each location in the list represents an order of partitioning
 *   (pre-generated per map)
 *)
let route (bmod:(int * int) list) (num_of_nodes:int) (key:int option list) =
  let dim_bounds = calc_dim_bounds bmod in
  let bound_bucket = calc_bound_bucket bmod dim_bounds key in
  let unbound_domains = calc_unbound_domains bmod key in
  let unbound_cart_prod = calc_unbound_cart_prod unbound_domains in
  calc_unbound_ip_list bound_bucket unbound_cart_prod dim_bounds

(* Returns a list of ip, tuple pairs.
 *
 * To handle empty messages, we create all possible empty messages up front
 * and fill in with the tuples given. *)

(* shuffle_on_empty = boolean flag to indicate whether a shuffle
 *   should be sent out to empty tuple destinations
 *
 * m = m1, n = m2,
 *
 * n_key = bound variables used to access n
 *         e.g. [10; 5; 3.4]
 * n_pat = positions of bound vars (i.e., b_n) in n's schema.
 *         (pregenerated per statement)
 *         e.g. [1; 0; 2] referring back to the example in n_key and their
 *         positions within n's schema
 *
 * m_to_n_pat = positions of m's keys in n's schema except
 *              for common bound variables. that is m's bound variables
 *              does not override n's (they would be the same).
 *              pregenerated at each call site for shuffle,
 *              i.e. per statement.
 *              e.g. [(0,3);(1,5);(2,1)...] for each index of m, we get the
 *              index of n.
 *
 * tuples = tuples of m. Consists of a list of keys, and a final value.
 *         e.g. [10; 5; 3.4; 200], where the final 200 is the value.
 *)

(* start with n_key and build up an n_key that can be used to
 * route with *)
(* In K3, this function will have to be part of the code per binding *)
let full_n_key (n_key:int option list) n_to_m_pat m_tuple : int option list =
  snd @: List.fold_left
    (fun (index,acc_tup) k ->
      if List.exists (fun (x,_) -> x = index) n_to_m_pat then
        let m_place = List.assoc index n_to_m_pat in
        let m_val = try List.nth m_tuple m_place with
        | Invalid_argument "List.nth" -> invalid_arg "Bad n_to_m_pat or tuple"
        in
        (index+1, acc_tup@ [Some m_val]) (* take from the tuple *)
      else
        (index+1, acc_tup@ [k])
    )
    (0,[])
    n_key

let get_all_targets shuffle_on_empty route_to_n n_key =
  (* in shuffle on empty case, we prepare all the routing that must
   * be done for empty packets *)
  if shuffle_on_empty then
    List.map
      (fun ip -> (ip,[]))
        (route_to_n n_key)
  else []               (* just an empty set to begin with *)

let shuffle_m_to_n n_bmod num_of_nodes n_to_m_pat n_key tuples shuffle_on_empty =
    let route_to_n = route n_bmod num_of_nodes in
    let all_targets = get_all_targets shuffle_on_empty route_to_n n_key in
    group_by_aggregate (* sort by IPs *)
      (fun acc (ip, tuple) -> acc@ [tuple])
      []
      (fun (ip, tuple) -> ip)
      (all_targets@ List.flatten @:
        List.map (fun m_tuple ->
          List.map
            (fun ip -> (ip, m_tuple))
            (route_to_n @: full_n_key n_key n_to_m_pat m_tuple)
          )
          tuples
        )

