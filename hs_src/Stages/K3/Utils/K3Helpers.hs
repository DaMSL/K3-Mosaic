module K3.Utils.K3Helpers where

import Util.Util
import Util.Tree
{-import Util.-}
import Stages.K3.Core.K3AST hiding (Just) 
import qualified Stages.K3.Core.K3AST as K3AST
import Stages.K3.Core.K3Annotations hiding (Id)

-- Annotation manipulation
mk_no_anno a = (a, [])

mk_anno_sort (a, annos) xs = (a, annos++[Data Constraint $ Ordered xs])

-- Type manipulation functions -------------

-- convert an isolated value to a contained value all the way down
iso_to_contained typ = handle_value_type typ
  where handle_value_type t = case t of
          TIsolated m  -> TContained $ handle_mut_type m
          TContained m -> TContained $ handle_mut_type m
        handle_mut_type m = case m of
          TMutable b a   -> TMutable (handle_base_type b) a
          TImmutable b a -> TImmutable (handle_base_type b) a
        handle_base_type b = case b of
          TMaybe v  -> TMaybe $ handle_value_type v
          TTuple vl -> TTuple $ map handle_value_type vl
          TCollection c v -> TCollection c $ handle_value_type v
          TTarget b -> TTarget $ handle_base_type b
          x -> x
    
-- the default type
canonical typ = TIsolated $ TImmutable typ []

-- A type for simple K3 types
t_bool = canonical TBool
t_int = canonical TInt
t_int_mut = TIsolated $ TMutable TInt []
t_float = canonical TFloat
t_float_mut = TIsolated $ TMutable TFloat []
t_string = canonical TString
t_unit = canonical TUnit

-- A type for addresses
t_addr = canonical TAddress

-- wrap a type in a list
wrap_tlist typ = canonical $ TCollection TList c
  where c = iso_to_contained typ
  
-- wrap a type in a mutable list
wrap_tlist_mut typ = TIsolated $ TMutable (TCollection TList c) []
  where c = iso_to_contained typ

-- wrap a type in a set
wrap_tset typ = canonical $ TCollection TSet c
  where c = iso_to_contained typ

-- wrap a type in a mutable set
wrap_tset_mut typ = TIsolated $ TMutable (TCollection TSet c) []
  where c = iso_to_contained typ

-- wrap a type in a bag
wrap_tbag typ = canonical $ TCollection TBag c
  where c = iso_to_contained typ

-- wrap a type in a mutable bag
wrap_tbag_mut typ = TIsolated $ TMutable (TCollection TBag c) []
  where c = iso_to_contained typ

-- wrap a type in an immutable tuple
wrap_ttuple [x]        = x
wrap_ttuple typ@(x:xs) = canonical $ TTuple typ
wrap_ttuple _          = error "No tuple to wrap"

-- wrap a type in a mutable tuple
wrap_ttuple_mut [x]        = x
wrap_ttuple_mut typ@(x:xs) = TIsolated $ TMutable (TTuple typ) []
wrap_ttuple_mut _          = error "No mutable tuple to wrap"

wrap_tmaybe t   = canonical $ TMaybe t
wrap_tmaybes ts = map wrap_tmaybe ts

-- wrap a function argument (ids, types)
wrap_args id_type = 
  let inner ("_", _) = AIgnored
      inner (i, t)   = AVar i t
  in case id_type of
    [x]  -> inner x 
    x:xs -> ATuple $ map inner id_type
    _    -> error "No ids, types for wrap_args"

-- for deep arguments (using ATuple)
wrap_a2 []  = error "Nothing to wrap in wrap_a2"
wrap_a2 [x] = x
wrap_a2 xs  = ATuple xs

-- wrap function arguments (id,typ), turning tmaybes to amaybes
wrap_args_maybe id_type = 
  let inner p = case p of
        ("_",_) -> AIgnored
        (i, TIsolated (TImmutable (TMaybe t) _))  -> AMaybe $ AVar i t
        (i, TIsolated (TMutable (TMaybe t) _))    -> AMaybe $ AVar i t
        (i, TContained (TImmutable (TMaybe t) _)) -> AMaybe $ AVar i t
        (i, TContained (TMutable(TMaybe t) _))    -> AMaybe $ AVar i t
        (i,t) -> AVar i t
  in case id_type of 
    [x]  -> inner x
    x:xs -> ATuple $ map inner id_type
    _    -> error "No ids, types for wrap_args_maybe"

-- function to make a simple tree with no meta or numbering
mk_stree tag children = mk_tree $ (mk_no_anno (0, tag), children)

-- Standard AST nodes
mk_const c = mk_stree (Const c) []

mk_var id = mk_stree (Var id) []

mk_tuple [i] = i
mk_tuple items@(i:is) = mk_stree Tuple items
mk_tuple _ = error "Nothing to use mk_tuple on"

mk_just x = mk_stree K3AST.Just [x]

mk_empty val_type = mk_stree (Empty val_type) []

mk_singleton val_type x = mk_stree (Singleton val_type) [x]

mk_combine x y = mk_stree Combine [x,y]

mk_range ctype start stride steps =
    mk_stree (Range ctype) [start, stride, steps]

mk_add x y = mk_stree Add [x, y]

mk_or = mk_add

mk_mult x y = mk_stree Mult [x, y]

mk_and = mk_mult

mk_neg x = mk_stree Neg [x]

mk_not = mk_neg

mk_sub x y = mk_add x $ mk_neg y

mk_eq x y = mk_stree Eq [x, y]

mk_lt x y = mk_stree Lt [x, y]

mk_neq left right = mk_stree Neq [left,right]

mk_leq left right = mk_stree Leq [left,right]

mk_geq left right = mk_not $ mk_lt left right

mk_gt left right = mk_not $ mk_leq left right

mk_lambda argt expr = mk_stree (Lambda argt) [expr]

mk_apply lambda input = mk_stree Apply [lambda, input]

mk_block statements = mk_stree Block statements

mk_iter iter_fun collection = mk_stree Iterate [iter_fun, collection]

mk_if pred true_exp false_exp = mk_stree IfThenElse [pred, true_exp, false_exp]

mk_map map_fun collection = mk_stree Map [map_fun, collection]

mk_filtermap pred_fun map_fun collection = 
    mk_stree FilterMap [pred_fun, map_fun, collection]

mk_flatten collection = mk_stree Flatten [collection]

mk_agg agg_fun init collection = mk_stree Aggregate [agg_fun, init, collection]

mk_gbagg group_fun agg_fun init collection =
    mk_stree GroupByAggregate [group_fun, agg_fun, init, collection]

mk_sort collection compare_fun = mk_stree Sort [collection, compare_fun]

mk_slice collection pattern = mk_stree Slice [collection, pattern]

mk_insert collection x = mk_stree Insert [collection, x]

mk_delete collection x = mk_stree Delete [collection,x]

mk_update collection old_val new_val =
    mk_stree Update [collection, old_val, new_val]

mk_peek collection = mk_stree Peek [collection]

-- left: TRef, right: T/TRef
mk_assign left right = mk_stree Assign [left, right]

mk_deref ref = mk_stree Deref [ref]

-- target:TTarget(T) address:TAdress args:T
mk_send target address args = mk_stree Send [target, address, args]


-- Macros to do more complex tasks ----------- 

-- functions to extract names/types from argument lists (id, type)
extract_arg_types l = map snd l

extract_arg_names l = map fst l

-- function to take a list of names and convert to K3 variables
-- "_" translates to CUnknown
ids_to_vars = map f
  where f "_" = mk_const CUnknown 
        f x   = mk_var x

-- check if a collection is empty
mk_is_empty collection typ = mk_eq collection $ mk_empty typ

-- checks if a member of a collection is present
mk_has_member collection pattern member_type = 
  mk_not $ mk_eq (mk_slice collection pattern) $
    mk_empty $ wrap_tlist member_type

mk_code_sink name args locals code =
  mk_no_anno $ Sink $ Code name args locals code

mk_global_fn_raw name input_arg input_types output_types expr =
  mk_no_anno $ 
    Global name
      (TFunction input_types output_types) $
      Just $ mk_lambda input_arg expr

-- function to declare and define a global function. Assumes the global
-- construct allows for an expr_t as well.
-- The types are expected in list format (always!)
mk_global_fn name input_names_and_types output_types expr =
  mk_global_fn_raw name 
    (wrap_args input_names_and_types)
    (wrap_ttuple $ extract_arg_types input_names_and_types)
    (wrap_ttuple output_types) 
    expr

mk_global_val name val_type = 
  mk_no_anno $ Global name (TValue val_type) Nothing

mk_global_val_init name val_type e = 
  mk_no_anno $ Global name (TValue val_type) $ Just e

mk_foreign_fn name input_types output_types =
  mk_no_anno $ Foreign name (TFunction input_types output_types)

mk_flow stmt_list = mk_no_anno $ Flow stmt_list

-- a lambda with 2 arguments for things like aggregation functions
mk_assoc_lambda arg1 arg2 expr = mk_lambda (ATuple [arg1,arg2]) expr

-- a classic let x = e1 in e2 construct
mk_let var_name var_type var_value expr =
    mk_apply
        (mk_lambda (wrap_args [var_name, var_type])
            expr
        ) 
        var_value

-- A let that assigns multiple variables simultaneously. 
-- For breaking up tuples and passing multiple values out of functions.
-- var_name_type_list must be a (string, type) list, and the var_values must
-- evaluate to the same types
mk_let_many var_name_and_type_list var_values expr =
    mk_apply
        (mk_lambda (wrap_args var_name_and_type_list) expr)
        var_values

mk_fst tuple_types tuple =
    mk_let_many (zip ["__fst","__snd"] tuple_types) tuple $ mk_var "__fst"

mk_snd tuple_types tuple =
    mk_let_many (zip ["__fst","__snd"] tuple_types) tuple $ mk_var "__snd"


-- Functions to manipulate tuples in K3 code
data TuplePat = Position Int | ExternVar Id | Unknown

def_tup_prefix = "__temp_"

mk_tuple_range types = create_range 0 $ length types

tuple_make_pattern :: [ValueType] -> [TuplePat]
tuple_make_pattern types = 
    map Position $ mk_tuple_range types

-- functions to take and drop from the pattern, filling in unknowns for the
-- values you drop. list_drop and list_take can be used for non-slice operations
slice_pat_take num pat = take num pat++unknowns
  where unknowns = map (\_ -> Unknown) range
        range = create_range 1 $ length pat - num

slice_pat_drop num pat = unknowns++drop num pat
  where unknowns = map (\_ -> Unknown) range
        range = create_range 1 (length pat - num)

-- convert a number to an id used for breaking apart tuples
int_to_temp_id :: String -> Int -> String
int_to_temp_id prefix i = prefix++show i

tuple_pat_to_ids pat = map f pat
  where f (Position y) = int_to_temp_id def_tup_prefix y
        f (ExternVar y) = y
        f Unknown = "_"

types_to_ids_types prefix types = zip ids types
  where ids = map (int_to_temp_id prefix) range
        range = mk_tuple_range types

-- break down a tuple into its components, creating ids with a certain prefix *)
mk_destruct_tuple tup_name types prefix expr = 
  mk_let_many ids_types (mk_var tup_name) expr
  where ids_types = types_to_ids_types prefix types
  
-- rebuild a tuple based on the types of the tuple and a pattern of temporaries
-- or external variables
--
mk_rebuild_tuple tup_name types pattern =
  mk_destruct_tuple tup_name types def_tup_prefix
    (mk_tuple $ ids_to_vars $ tuple_pat_to_ids pattern)

-- unwrap maybe values by creating an inner values with postfix "_unwrap"
mk_unwrap_maybe var_names_and_types expr =
  mk_apply
    (mk_lambda (wrap_args_maybe unwrap_n_t) expr) $ 
    mk_tuple vars
  where vars = ids_to_vars names
        unwrap_n_t = 
          map (\(n,t) -> (n++"_unwrap",t)) var_names_and_types
        names = fst $ unzip var_names_and_types

-- K3 types for various elements of a k3 program
t_trig_id = t_int -- In K3, triggers are always handled by numerical id
t_stmt_id = t_int
t_map_id = t_int

-- for vids
vid_types = [t_int, t_int, t_int]
vid_mut_types = [t_int_mut, t_int_mut, t_int_mut]
t_vid = wrap_ttuple vid_types
t_vid_mut = wrap_ttuple vid_mut_types

-- functions for comparing vids
data VidOp = VEq | VNeq | VGt | VLt | VGeq | VLeq
mk_global_vid_op name tag = 
  let lvid_id_t = types_to_ids_types "l" vid_types
      lvid_id = fst $ unzip lvid_id_t
      rvid_id_t = types_to_ids_types "r" vid_types
      rvid_id = fst $ unzip rvid_id_t
      arg_pair = wrap_a2 [wrap_args lvid_id_t, wrap_args rvid_id_t]
      arg_types = wrap_ttuple [wrap_ttuple vid_types, wrap_ttuple vid_types]
      op f i = f (mk_var $ nth_l i) (mk_var $ nth_r i)
        where nth_l = (lvid_id !!) 
              nth_r = (rvid_id !!) 
      mk_vid_eq = mk_and (op mk_eq 0) $ mk_and (op mk_eq 1) (op mk_eq 2)
      mk_vid_neq = mk_not mk_vid_eq
      mk_vid_lt = mk_and (op mk_lt 0) $ mk_and (op mk_lt 1) (op mk_lt 2)
      mk_vid_geq = mk_not mk_vid_lt
      mk_vid_gt = mk_and (op mk_gt 0) $ mk_and (op mk_gt 1) (op mk_gt 2)
      mk_vid_leq = mk_not mk_vid_gt
  in mk_global_fn_raw name arg_pair arg_types t_bool $
    case tag of
      VEq  -> mk_vid_eq
      VNeq -> mk_vid_neq
      VGt  -> mk_vid_gt
      VGeq -> mk_vid_geq
      VLt  -> mk_vid_lt
      VLeq -> mk_vid_leq

-- id function in k3
mk_id tuple_types = 
  mk_lambda (wrap_args ids_types) $
    mk_tuple $ ids_to_vars $ extract_arg_names $ ids_types
  where ids_types = zip ids tuple_types
        ids = map (int_to_temp_id prefix) r
        r = mk_tuple_range $ tuple_types
        prefix = "__id_"

