module Util.ListAsSet where

import Data.List (foldl')

--   Utility functionality for using Lists as lightweight sets.  Item equality is
--   tested for using deep equality: = and <>.

{- 
   Computes the difference between two sets
   @param l1 The first list set
   @param l2 The second list set
   @return   The difference l1 - l2
-}
diff l1 l2 = concat $ map f l1
  where f x = if x `elem` l2 then [] else [x]

{-
   Determines if one list is a subset of the other.
   @param l1 The first list set
   @param l2 The second list set
   @return   true if l1 is a subset of l2
-}
subset l1 l2 = diff l1 l2 == []

{-
   Compare two sets for equality
   @param l1 The first list set
   @param l2 The second list set
   @return   true if l1 and l2 describe the same set
-}
seteq l1 l2 = subset l1 l2 && subset l2 l1

{-
   Compute the intersection of two sets
   @param l1 The first list set
   @param l2 The second list set
   @return   The set intersection of l1 and l2 
-}
inter l1 l2 = diff l1 $ diff l1 l2

{-
   Compute the union of two sets
   @param l1 The first list set
   @param l2 The second list set
   @return   The union of l1 and l2
-}
union l1 l2 = l1 ++ diff l2 l1

{-
   Compute the union of multiple sets
   @param l  A list of list sets
   @return   The union of all sets in l
-}
multiunion l = foldl' union [] l

{-
   Compute the intersection of multiple sets
   @param l  A list of list sets
   @return   The intersection of all lists in l
-}
multiinter l = foldl' inter (head l) (tail l)

{-
   Eliminate all duplicates in a list
   @param l  A list
   @return   The set of all distinct elements in l
-}
no_duplicates l = multiunion $ map (\x -> [x]) l

{-
   Eliminate all duplicates in a list (identical to no_duplicates)
   @param l  A list
   @return   The set of all distinct elements in l
-}
uniq l = no_duplicates l

{-
   Computes all subsets of r that have exactly k elements.
   does not produce duplicate sets if r does not have duplicates.
   sorting -- e.g. (List.sort Pervasives.compare l) --
   improves readability of the result. 
   @param k  The size of each returned list set
   @param r  The superset of all of the returned subsets 
-}
subsets_of_size k r = add_k_elements_to_from k [] r
  where add_k_elements_to_from k l r =
          -- select_gt: selects those elements of r that are greater than x
          let select_gt x r = filter (> x) r
              f x = add_k_elements_to_from (k-1) (l++[x]) (select_gt x r)
          in if k <= 0 then [l]
          else concat $ map f r


{- Distributes a list of lists.
   e.g., 
   distribute [[[1;2]; [2;3]]; [[3;4]; [4;5]]; [[5;6]]];;
   results in
   [[[1; 2]; [3; 4]; [5; 6]]; [[2; 3]; [3; 4]; [5; 6]];
    [[1; 2]; [4; 5]; [5; 6]]; [[2; 3]; [4; 5]; [5; 6]]]
    
   @param l  A list of lists
   @return   The n-way cross product of all n elements in l
-}
distribute :: Eq a => [[a]] -> [[a]]
distribute l =
   if l == [] then [[]]
   else concat $ map f $ distribute $ tail l
        where f t = let g x = [x] ++ t
                in map g $ head l

{- Permutes a list of elements.
   e.g., 
   permute [1;2;3];
   results in
   [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]]
    
   @param l  A list of lists
   @return   The permutation of all n elements in l
-}
permute :: Eq a => [a] -> [[a]]
permute l =
	if l == [] then [[]]
	else concat $ map (insert $ head l) $ permute $ tail l
       where insert e []             = [[e]]
             insert e sublist@(h:hs) = (e:sublist) : (map (\x -> h:x) $ insert e hs)
		
			
