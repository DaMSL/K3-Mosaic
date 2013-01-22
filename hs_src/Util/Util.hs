-- Utilities that are useful
module Util.Util where

import qualified Data.Ix as Ix

-- take the last x elements of a list *)
take_end len l = drop (length l - len) l

-- drop from end of list
drop_end len l = take (length l - len) l

-- function that folds until a predicate is true *)
foldlUntil _ pred acc (x:xs) | pred acc x = acc
foldlUntil f pred acc (x:xs) = foldlUntil f pred (f acc x) xs 
foldlUntil _ _ acc [] = acc

-- I/O helpers
{-let read_file f = -}
  {-let lines, in_chan = ref [], (open_in f) in-}
  {-let all_lines =-}
    {-try while true do-}
          {-lines := (!lines @ [input_line in_chan])-}
        {-done;-}
        {-[]-}
    {-with End_of_file -> !lines-}
  {-in close_in in_chan; String.concat "\n" all_lines -}

-- make a range. different from regular range function
create_range first length = Ix.range (first, (length + first - 1))

-- make a range that corresponds to a given list *)
create_corr_range first xs = create_range 0 $ length xs

-- insert a numerical index as the first of a pair
insert_index_fst first xs = zip is xs
  where is = create_corr_range first xs

-- insert a numerical index as the snd of a pair
insert_index_snd first xs = zip xs is
  where is = create_corr_range first xs

