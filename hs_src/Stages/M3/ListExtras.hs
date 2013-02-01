{-
   Additional functionality for manipulating and working with lists, meant to 
   supplement the core functionality in Ocaml's List module.
-}

{- 
   Scan through elements of a list.  Similar to List.iter, but the elements
   both before and after the element currently being processed are passed to 
   the lambda function.
    
   e.g., On the list {[ [1;2;3;4] ]} the lambda function would be invoked as...
   
   {[ f [] 1 [2;3;4] ]}
   
   {[ f [1] 2 [3;4] ]}
   
   {[ f [1;2] 3 [4] ]}
   
   {[ f [1;2;3] 4 [] ]}
    
   ++param f    The scan function.  On any given invocation of f, l may be 
               reconstructed by concatenating f's three inputs in order.
   ++param l    The list to scan over
-}
scan :: ([a] -> a -> [a] -> ()) -> [a] -> ()
scan f l =
    let iterate prev curr_next =
        case curr_next of
            []        -> ()
            curr:next -> (f prev curr next); (iterate (prev++[curr]) next)
    in iterate [] l
   
{- 
   Map the elements of a list in the same manner as scan.  Like 
   ListExtras.scan, but based on List.map instead of List.iter. 
   
   ++param f    The map function.  On any given invocation of f, l may be 
               reconstructed by concatenating f's three inputs in order.
   ++param l    The list to map over
   ++return     The re-mapped list
-}
scan_map :: ([a] -> a -> [a] -> [b]) -> [a] -> [b] 
scan_map f l =
   let iterate prev curr_next =
      case curr_next of
          []        -> []
          curr:next -> (f prev curr next) : (iterate (prev++[curr]) next)
   in iterate [] l

{- 
   Fold the elements of a list in the sam manner as scan.  Like ListExtras.scan
   but based on List.fold_left instead of List.iter  
   
   ++param f    The fold function.  On any given invocation of f, l may be 
               reconstructed by concatenating f's first three inputs in order.  
               The fourth parameter is the most recent fold value.
   ++param init The initial fold value
   ++param l    The list to fold over
   ++return     The final fold value
-}
scan_fold :: (b -> [a] -> a -> [a] -> b) -> b -> [a] -> b
scan_fold f init l =
   let iterate curr_val prev curr_next =
      case curr_next of
         []        -> curr_val
         curr:next -> iterate (f curr_val prev curr next) (prev++[curr]) next
   in iterate init [] l

-- test if an item is member of an assoc list
elem_assoc a l = maybe False (\_-> True) $ lookup a l

-- remove member from assoc list
remove_assoc x l = concat $ map remove l
  where remove (u,_) | u==x = []
        remove a            = [a]

{- 
   A standard reduce (i.e., group-by) implementation on lists.

   Given a list of 2-tuples {b (a,b)}, produce a list of 2-tuples 
   {[ (a,b list) ]} such that the returned list has only one element for every 
   distinct value of [a], and every value of [b] appears paired with the 
   same value of [a] that it appeared with in the input list.
   
   ++param l  The list of [(a,b)] elements to reduce
   ++return   Tuples in [l] grouped by [a]
-}
reduce_assoc :: [(a,b)] -> [(a, [b])]
reduce_assoc l =
   foldr (\(a,b) ret ->
      if a `elem_assoc` ret
      then ((a, b) : (lookup a ret)) : (remove_assoc a ret)
      else (a, [b]) : ret
   ) [] l

{- 
   Flatten on a list of 2-tuples of lists.  Shorthand for List.split, and 
   flattening each list individually 
   
   ++param l  A list of list pairs
   ++return   A pair of lists
-}
flatten_list_pair l =
   let (a, b) = unzip l in (concat a, concat b)

{-
   Compute the full associative outer join of two lists of 2-tuples using the 
   first tuple element as a join column.
   
   ++param a  A list of 2-tuples [(c,a)]
   ++param b  A list of 2-tuples [(c,b)]
   ++return   A list of tuples [(c,(a option,b option))] where c comes from the 
             union of the first tuple elements of [a] and [b], and the 
             remaining fields are populated by the result of {[List.assoc a c]}
             and {[List.assoc b c]}.
-}
outer_join_assoc :: [(c,a)] -> [(c,b)] -> [(c, (Maybe a, Maybe b))] 
outer_join_assoc a b =
   let (outer_b, join) = 
      foldl' (\(b, join) (c, a_v) ->
                (remove_assoc c b, join ++ [(c, (Just a_v, lookup c b))])) 
        (b, []) 
        a
   in join ++ map (\(c, b_v) -> (c, (Nothing, Just b_v))) outer_b

{-
   Translate a list into a string.
   
   ++param sep            (optional) Inter-element separator string
   ++param string_of_elem Stringifier for individual list elements
   ++param l              The list to stringify
   ++return               The stringified form of the list
-}
string_of_list = string_of_list_sep "; "

string_of_list_sep :: String -> (a -> String) -> [a] -> String
string_of_list_sep sep string_of_elem l =
  intersperse sep $ map string_of_elem l

{-
   Translate a list into an ocaml-style string list
   
   ++param string_of_elem Stringifier for individual list elements
   ++param l              The list to stringify
   ++return               The stringified form of the list
-}
string_of_list :: String -> (a -> String) -> [a] -> String
ocaml_of_list string_of_elem l =
   "[" ++ (string_of_list string_of_elem l) ++ "]"

{-
   Find and return the index of the indicated element (testing for equality 
   using =).  The returned index is the parameter that would be passed to 
   List.nth to get the indicated element.
   
   ++param a   The element to look for
   ++param l   The list to look for the element in
   ++return    The index of the element in the list 
-}
index_of :: a -> [a] -> Int
index_of a l = a `elemIndex` l

{-
   Return the sublist of the passed list starting at the indicated index and 
   containing the indicated number of elements.
   
   ++param start  The index of the first element to be included in the sublist
   ++param cnt    The number of consecutive elements to return in the sublist
   ++param l      The list to compute a sublist of
   ++return       The sublist of [l] of size [cnt] starting at index [start].
-}
sublist start cnt l =
   snd (foldl' (\(i, r) e ->
      if i <= 0 then
         if (length r < cnt) or (cnt < 0) then (i, r ++ [e])
                                               else (i, r)
      else (i-1,r)
   ) (start,[]) l)

{-
   Partition a list, splitting at the position of a given pivot element.  
   This does not attempt to compare elements to the pivot element, but rather 
   returns splits based on position in the input list.
   
   ++param a  The pivot element
   ++param l  The input list
   ++return   The 2-tuple consisting of all elements that occur (positionally) in
             [l] before the first occurrence of [a], and all the elements that 
             occur after.
-}
split_at_pivot a l =
   let idx = a `elemIndex` l in
      (sublist 0 (idx) l, sublist (idx+1) (-1) l)

{-
   Partition a list, splitting at the given position.
   
   ++param a  The split position
   ++param l  The input list
   ++return   The 3-tuple consisting of all elements that occur (positionally) in
             [l] before the split position, the element at the split position, 
             and all the elements that occur after.
-}
split_at_position pos l =
   (sublist 0 (pos) l, sublist (pos) 1 l, sublist (pos+1) (-1) l)

{-
   Partition a list, splitting at the last position.
   
   ++param l  The input list
   ++return   The 3-tuple consisting of all elements that occur (positionally) in
             [l] before the split position, the element at the split position, 
             and all the elements that occur after.
-}
split_at_last l =
   let (x, y, _) = split_at_position ((length l)-1) l
   in (x, y)
   

{-
   Helper function for performing folds over associative lists.  Like 
   [List.fold_left], but the function takes 3 parameters rather than 2.
   
   ++param fn    The fold function (with 3 parameters, rather than 2)
   ++param init  The initial fold value
   ++param l     The list to fold over
   ++return      The final fold value
-}
assoc_fold :: (a -> b -> c -> c) -> c -> [(a, b)] -> c =
assoc_fold fn init l = foldl' (\c (a, b) -> fn a b c) init l

{-
   Compute the maximal element of a list (according to [Pervasives.max]).
   
   ++param l   The list
   ++return    The maximal element of [l]
-}
max  = maximum

{-
   Compute the minimal element of a list (according to [Pervasives.min]).
   
   ++param l   The list
   ++return    The minimal element of [l]
-}
min = minimum

{-
   Convert a 2-element list into a 2-tuple
   
   ++param l   A 2 element list
   ++return    A 2-tuple containing the list
-}
list_to_pair l = (head l, head $ tail l) 

{-
   Creates all k-tuples of elements of the list src.
   
   Example::
   
   {[ k_tuples 3 [1;2] =
            [[1; 1; 1]; [2; 1; 1]; [1; 2; 1]; [2; 2; 1]; [1; 1; 2];
             [2; 1; 2]; [1; 2; 2]; [2; 2; 2]]
   ]}
   
   ++param k    The size of each returned list
   ++param src  The set of possible elements in each returned list
   ++return     A list of the returned lists
-}
k_tuple :: Int -> [a] -> [[a]]
k_tuples k src =
   if (k <= 0) then [[]]
   else concat $ map (\t -> map (\x -> x:t) src) $ k_tuples (k-1) src


{-exception CycleFound-}

{- 
   Perform topological sort on a given graph starting from a given node.
    
   Example:: 
        
   {[ toposort_from_node [ (1, [2;3]); (2, [4;5]); (4, [6]) ] [] 1 = 
                         [ 1; 3; 2; 5; 4; 6]
    ]}
        
   ++param graph      A graph representing a partial order between elements
   ++param visited    A list of already visited nodes (useful in multiple
                     invocations, see [toposort])
   ++param start_node The root node of the graph
   ++return           A list of topologically sorted elements 
-}
toposort_from_node graph visited start_node =
   let explore path node visited =
      if node `elem` path then Nothing 
      else if node `elem` visited then visited 
      else                    
         let new_path = node:path in
             child_nodes = case lookup node graph of 
                 Nothing -> []
                 Just x  -> x
             visited = foldr (explore new_path) child_nodes visited 
         in node : visited
   in explore [] start_node visited

{- 
   Perform topological sort on a given graph which may contain 
   multiple root elements.
   Example:: 
        
   {[ toposort [ (0, [2; 3]); (1, [2]) ] = 
               [ 0; 3; 1; 2]
    ]}
   {[ toposort [ (1, [2]); (5, [6; 7]); (3, [2]); (6, [3; 7]); (8, [7]); 
                 (4, [3; 1]) ] =
               [ 5; 6; 8; 7; 4; 3; 1; 2]
    ]}
        
   ++param graph      A graph representing a partial order between elements
   ++return           A list of topologically sorted elements 
-}
toposort graph =
   foldr (\(node, _) visited -> toposort_from_node graph visited node) graph []
                                                            
