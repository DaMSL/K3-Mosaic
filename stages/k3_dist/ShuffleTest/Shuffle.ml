(*
 * m_to_n_pat = positions of m's keys in n's schema (pregenerated for each mn)
 * node_to_ip: translate node number to IP
 * bmod = (int * int) list = (index * bucket modulo factor) list (pre-generated per
 * map)
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
;;

(* Returns a list of ips *)                        
(* Route is specialized for a specific bmod *)
let route bmod num_of_nodes m_key m_pat   =
    (* m_key = m's parameters
     *     e.g. [10; 15; 3.4]
     * m_pat = position of each parameter in m_s schema
     *     e.g. [0; 2; 1] refers to the m_key values' position in m's pattern.
     *     NOTE: should -1 be no position? ***
     *
     * bmod = (int * int) list
     *   = (index * bucket modulo factor) list
     *   (pre-generated per map)
     *   TODO: lookup must be by reading the first number
     *)
		if List.length m_key <> List.length m_pat then invalid_arg "m_pat doesn't match m_key"
		else
    
    (*
     * Combine the pattern and key lists into one list
     * this isn't the cleanest/fastest way of doing it, but it works
     * and this duplicates the way we do it in K3 which doesn't have map2
     *)
    let bound_info = 
        fst(List.fold_left
            (fun (acclist, accnum) x -> 
                (acclist @ [(x, List.nth m_pat accnum)], accnum+1))
            ([],0)
            m_key
        )
    in
    
    (*
     * Pre-calculate the contribution of every dimension to the total linear
     * function. Each dimension contributes the product of all previous
     * dimension buckets. This calculation does not depend on anything but bmod
		 * and can be taken outside of the function.
     *)
    let dim_boundaries =
			(0,1)::(                 (* amend the first dimension (always 1) to the list *)
	      List.map 
	        (fun i -> 
						(i, 
	            List.fold_left
	                (fun acc x -> acc * (List.assoc x bmod)) 
	                1 
	                (range 0 (i-1))
					  )
	        )
	        (range 1 ((List.length bmod)-1))  (* start from 1 because the first dimension has no contribution *)
			)
    in

    (* run over the m_key input and calculate the value that the bound variables contribute
     * the bound values serve as our constant. We can pre-calculate their contribution 
     * to the node location and use that for every possible node that results from 
     * unbound values, so this calculation should only be done once.
     * For each value, we multiply it by the bucket of every previous dimension
     * obtained in dim_boundaries
     *)
    let bound_bucket = 
        List.fold_left 
            (fun acc (x, pat) -> 
                acc + ((x mod (List.assoc pat bmod)) * List.assoc pat dim_boundaries))
            0
            bound_info
    in
    
    (* find which dimensions and buckets are not specified in the pattern (unbound) *)
    let unbound_dims = 
        List.filter
            (fun (i, b_i) -> not(List.mem i m_pat))
            bmod
    in
    
    (* make a list of lists of ranges 0...b_i for each unbound_dim.
     * This is every possible value in each unbound dimension
     *)
    let unbound_domains = 
        List.map
            (fun (i, b_i) -> (i, range 0 (b_i-1)))
            unbound_dims
    in

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
    let unbound_cart_prod = 
        (* prev_cart_prod : (index * value) list list *)
        List.fold_left
            (fun prev_cart_prod (i, domain) ->
                List.flatten
                    (List.map 
                        (* for every domain element in the domain *)
                        (fun domain_element -> 
														if prev_cart_prod = [] then [[(i,domain_element)]]
														else List.map 
		                               (* slap on the list of the previous stuff *)
		                               (fun rest_tup -> rest_tup@[(i,domain_element)])
		                               prev_cart_prod
                        )
                        domain
                    )
            )
            [] 
            unbound_domains
    in
		
    
    (* We now add in the value of the bound variables as a constant
     * and calculate the result for every possibility
     *)
		let unbound_ip_list = 
			List.map  (* clean up the list of tuples. we just want ips *)
				(fun (ip1, ip2) -> ip1)
		    (group_by_aggregate (* use just to group ips *)
		        (fun acc ip -> ip::acc)
		        []
		        (fun ip -> ip)
			        (List.map
			            (fun unbound_bucket -> 
			                node_to_ip
			                    ((List.fold_left
			                        (fun acc (i, v) -> 
			                            acc + v * List.assoc i dim_boundaries
			                        )
			                        bound_bucket  (* start with this const *)
			                        unbound_bucket) 
			                        mod num_of_nodes
			                    )
			            )
			            unbound_cart_prod
							)
			  )
		in
		(* handle the case of no unbound stuff *)
		if unbound_ip_list <> [] then unbound_ip_list
		else [node_to_ip (bound_bucket mod num_of_nodes)]
;;

	let num_of_nodes = 16
	let n_bmod = [(0,2);(1,2)]
	let m_to_n_pat = [(0,-1);(1,1);(2,-1);(3,0);(4,-1)]
	let shuffle_on_empty = false
	let n_pat = []
	let n_key = [] 
	let tuples = [[101;203;305;404;501;2];[450;383;214;563;321;5]]
	
(* note: always need to add the data to the end !!! *)
	
(* Returns a list of ip, tuple pairs.
 *
 * To handle empty messages, we create all possible empty messages up front
 * and fill in with the tuples given. *)

let shuffle_m_to_n n_bmod num_of_nodes m_to_n_pat n_key n_pat tuples shuffle_on_empty =

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

    let route_to_n = route n_bmod num_of_nodes 
		in

    let all_ips =
      (* in shuffle on empty case, we prepare all the routing that must
       * be done for empty packets
       *)
    	if shuffle_on_empty then
      	List.map
       	  	(fun ip -> (ip,[]))
        		(route_to_n n_key n_pat)
    	else []               (* just an empty set to begin with *)
    in
    let final_n_pat = 
      List.fold_left
          (fun acc_pat (m_idx, n_idx) -> 
              if m_idx >= 0 && n_idx >= 0 && 
							not (List.exists (fun x -> x = n_idx) n_pat) then 
                  acc_pat @ [n_idx]
              else acc_pat
          )
          n_pat
          m_to_n_pat
    in
    group_by_aggregate
    	(fun acc (ip, tuple) -> tuple::acc)
      [] 
      (fun (ip, tuple) -> ip)
      (all_ips@
          (List.flatten
              (List.map
                  (fun t_m ->
                      (* t_m are tuples in map m 
                       * t1i is a tuple in map m
                       * t2i and pat2 is the partial tuple and pattern of map n,
                       * whose fields are present in m.
                       *
                       * we need m_to_n_pat here to detangle which variables in
                       * t1i (from the read map) already used bound vars
                       *)
                      let final_t_n = 
                          List.fold_left
                              (fun acc_tup (m_idx, n_idx) -> 
                                  if m_idx >= 0 && n_idx >= 0 && 
																		not (List.exists (fun x -> x = n_idx) n_pat) then 
                                      acc_tup @ [List.nth t_m m_idx]
                                  else acc_tup
                              )
                              n_key
                              m_to_n_pat
                      in
                      List.map
                          (fun ip -> (ip, final_t_n))
                          (route_to_n final_t_n final_n_pat)
                  )
                  tuples
              )
          )
      )
;;
    
