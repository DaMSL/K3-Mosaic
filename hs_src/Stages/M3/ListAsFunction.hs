module Stages.M3.ListAsFunction where
-- Typechecks
{-
   Utilities for performing list-based translation.  The basic type [table_fn_t]
   describes a functional mapping as a list of 2-tuples, with the first elements
   representing the domain, and the second representing the range.  
-}

import Data.List (foldl', find)
import qualified Stages.M3.ListExtras as LE
import Stages.M3.ListExtras (elem_assoc)
import qualified Util.ListAsSet as LAS

{-
   A list, or table of mappings, represented as 2-tuples.  Each element of the 
   table represents a mapping from the first element of the 2-tuple to the 
   second.
-}
type TableFn a b = [(a,b)]

{-
   Raised if elements of the domain are duplicated
-}
{-exception NonFunctionalMappingException-}

{-
   Apply the functional mapping to an element.
   ++param theta   The functional mapping table
   ++param default The output value to be used if the [x] is not present in the 
                  domain of [theta]
   ++param x       The value to be mapped
   ++return        The mapping of [x] in [theta], or [default] if [x] is not 
                  present in the domain of [theta]
   ++raise NonFunctionalMappingException If [theta]'s domain has duplicates
-}
apply :: Eq a => TableFn a b -> b -> a -> b
apply theta def x =
   let g (y, z) = if x == y then [z] else []
       x2 = concat $ map g theta
   in
   if null x2 then def
   else if length x2 == 1 then head x2
   else error "apply: duplicates in theta's domain"

{-
   Apply the functional mapping to an element or leave it untouched if the
   element is not present in the functional mapping table.
   ++param theta The functional mapping table
   ++param x     The value to be mapped
   ++return      The mapping of [x] in [theta], or [x] if it is not present in 
                the domain of [theta]
   ++raise NonFunctionalMappingException If [theta]'s domain has duplicates
-}
apply_if_present :: Eq a => TableFn a a -> a -> a
apply_if_present theta x = apply theta x x

{-
   Stringify the provided functional mapping table.
   ++param theta      The functional mapping table
   ++param left_to_s  A stringifier for elements of the mapping's domain
   ++param right_to_s A stringifier for elements of the mapping's range
   ++return           The stringified functional mapping
-}
string_of_table_fn :: TableFn a b -> (a -> String) -> (b -> String) -> String
string_of_table_fn theta left_to_s right_to_s =
  let val = foldl' 
        (\accum (x,y) -> 
          let prefix = case accum of 
                        Just a  -> a ++ ", " 
                        Nothing -> ""
          in Just $ prefix ++ "{ "++(left_to_s x)++ " => "++(right_to_s y)++ " }"
        ) Nothing theta
  in case val of
   Just a  -> "[ "++a++" ]"
   Nothing -> "[]"

{-
   Stringify the provided string -> string functional mapping table
   ++param theta The functional mapping table
   ++return      The stringified functional mapping
-}
string_of_string_fn theta =
   "{" ++(LE.string_of_list_sep ", " (\(x,y) -> x++"->"++y) theta)++"}"

{-
   Apply the functional mapping to an element, raising an exception if it is
   not present in the functional mapping table.
   ++param theta The functional mapping table
   ++param x     The value to be mapped
   ++return      The mapping of [x] in [theta]
   ++raise NonFunctionalMappingException If [theta]'s domain has duplicates or if
                                        [x] is not present in [theta]'s domain
-}
apply_strict :: Eq a => TableFn a b -> a -> b
apply_strict theta x =
   let g (y, z) = if (x == y) then [z] else []
       x2 = concat $ map g theta
   in
   if length x2 == 1 then head x2
   else error "apply_strict: functional mapping exception"

{-
   Compute the domain of a functional mapping
   ++param theta The functional mapping table
   ++return      The domain of the functional mapping table
-}
dom :: TableFn a b -> [a]
dom theta = fst $ unzip theta

{-
   Compute the range/image of a functional mapping
   ++param theta The functional mapping table
   ++return      The range/image of the functional mapping table
-}
img :: TableFn a b -> [b] 
img theta = snd $ unzip theta

{-
   Determine whether an element is in the domain of a functional mapping
   ++param theta The functional mapping table
   ++param x     An element of the type of the domain of the mapping
   ++return      True if [x] is in the domain of [theta]
-}
in_dom :: Eq a => TableFn a b -> a -> Bool
in_dom theta a = a `elem_assoc` theta

{-
   Determine whether an element is in the range/image of a functional mapping
   ++param theta The functional mapping table
   ++param x     An element of the type of the range/image of the mapping
   ++return      True if [x] is in the range/image of [theta]
-}
in_img :: (Eq a, Eq b) => TableFn a b -> b -> Bool
in_img theta b = maybe False (\_ -> True) $ find (\(_, cmp_b) -> cmp_b == b) theta

{-
   Determine whether a mapping is functional (has no duplicates in its domain)
   ++param theta The functional mapping table
   ++return      True if the domain of [theta] contains no duplicates
-}
functional :: Eq a => TableFn a b -> Bool
functional theta = let d = dom theta in LAS.no_duplicates d == d

{-
   Determine whether a mapping is onto (has no duplicates in its range/image)
   ++param theta The functional mapping table
   ++return      True if the range/image of [theta] contains no duplicates
-}
onto :: (Eq a, Eq b) => TableFn a b -> Bool
onto theta = let i = img theta in LAS.no_duplicates i == i

{-
   Attempt to merge two functional mappings
   ++param theta1  The first input functional mapping table
   ++param theta2  The second input functional mapping table
   ++return        A Just() wrapped mapping if the two functional mappings can be
                  merged into a functional and onto mapping, or Nothing otherwise.
-}
merge :: (Eq a, Eq b) => TableFn a b -> TableFn a b -> Maybe (TableFn a b)
merge theta1 theta2 =
   if any (\a -> (lookup a theta1) /= (lookup a theta2))
                  (LAS.inter (dom theta1) (dom theta2))
   then Nothing 
   else let merged = LAS.union theta1 theta2
        in if onto merged then Just merged else Nothing

{-
   Attempt to murge multiple functional mappings.  Identical to folding 
   Function.merge over a list of mapping tables.
   ++param thetas  The list of functional mapping tables.
   ++return        A Just() wrapped mapping if the functional mapping tables
                  can be successfullly merged (as in merge), or Nothing if not.
-}
multimerge :: (Eq a, Eq b) => [TableFn a b] -> Maybe (TableFn a b)
multimerge thetas =
   foldl' (\a b -> case a of 
                        Just sa -> merge sa b 
                        Nothing -> Nothing)
          (Just []) thetas

{-
   Find identities in a functional mapping table. (mappings that map to 
   themselves.
   ++param theta The functional mapping table
   ++return      A list of all elements that would be mapped to themselves
-}
identities :: Eq a => TableFn a a -> [a]
identities theta =
   let f (x,y) = if (x==y) then [x] else []
   in concat $ map f theta

{-
   Determine if a functional mapping consists entirely of identities.
   ++param theta The functional mapping table
   ++return      True if [theta] is the identity function for its domain.
-}
is_identity :: Eq a => TableFn a a -> Bool
is_identity theta = (length $ identities theta) == length theta

{-
   Determine if the provided functional mapping has a transitive closure (i.e., 
   if applying the mapping a second time could produce a different mapping).  
   ++param theta The functional mapping table
   ++return      True if the mapping is its own transitive closure
-}
intransitive :: Eq a => TableFn a a -> Bool
intransitive theta = null $ 
  LAS.diff (LAS.inter (dom theta) $ img theta) $ identities theta
