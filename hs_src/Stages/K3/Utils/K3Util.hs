-- Various utilities for dealing with K3 files
import Util.Util
import Util.ListAsSet
import Util.Tree

import Stages.K3 hiding (Just)
import qualified Stages.K3 as K3
import Stages.K3.Utils.K3Builders

import Control.Monad.State
import Control.Applicative hiding (Const)
import Data.Char (isNumber)
import Data.Foldable (foldl', foldlM)

-- AST accessors
id_of_expr e = fst $ fst_data e 
tag_of_expr e = snd $ fst_data e
meta_of_expr e = snd_data e

-- Variable id extraction
vars_of_arg a = case a of
  AIgnored -> []
  AVar v _ -> [v]
  AMaybe a' -> vars_of_arg a'
  ATuple vt_l -> concat $ map vars_of_arg vt_l
  
typed_vars_of_arg arg = case arg of
  AIgnored -> []
  AVar v t -> [(v,t)]
  AMaybe a -> typed_vars_of_arg a
  ATuple vt_l -> concat $ map typed_vars_of_arg vt_l

id_of_var e = case tag_of_expr e of
  Var id -> id 
  _ -> error "invalid variable"

tuple_type_of_args args_t =
	if length args_t == 1 then TValue (head args_t) 
	else TValue $ TIsolated $ TImmutable (TTuple args_t) []

tuple_type_of_arg arg =
  tuple_type_of_args $ map snd $ typed_vars_of_arg arg 

-- Predicates
is_const e = case tag_of_expr e of 
  Const _ -> True
  _ -> False

is_var e = case tag_of_expr e of 
  Var _ -> True
  _ -> False

is_var_match id e = is_var e && id_of_var e == id

-- Bindings
lambda_bindings f (Lambda x) = f x 
lambda_bindings f _ = []

arg_of_lambda e = case lambda_bindings (\x -> [x]) $ tag_of_expr e of
  []  -> Nothing
  [x] -> Just x
  _ -> error "invalid lambda arg"

vars_of_lambda e = lambda_bindings vars_of_arg $ tag_of_expr e

typed_vars_of_lambda e =
  lambda_bindings typed_vars_of_arg $ tag_of_expr e


-- Type tags -----------------------

-- Compute a readable type signature for a K3 type.
-- Type signatures do not preserve annotations.
signature_of_type t = sig_t 0 t
  where sig_t d (TFunction arg_t ret_t) = 
          tag d "F" [sig_vt (d+1) arg_t, sig_vt (d+1) ret_t] 
        sig_t d (TValue vt) = tag d "V" [sig_vt (d+1) vt]
        sig_vt d (TIsolated mt) = tag d "I" [sig_mt (d+1) mt]
        sig_vt d (TContained mt) = tag d "C" [sig_mt (d+1) mt]
        sig_mt d (TMutable bt _) = tag d "M" [sig_bt (d+1) bt]
        sig_mt d (TImmutable bt _) = tag d "U" [sig_bt (d+1) bt]
        sig_bt d bt = case bt of
          TUnknown          -> tag   d "k" []
          TUnit             -> tag   d "n" []
          TBool             -> tag   d "b" []
          TByte             -> tag   d "y" []
          TInt              -> tag   d "i" []
          TFloat            -> tag   d "d" []
          TString           -> tag   d "s" []
          TMaybe vt         -> tag   d "o" [sig_vt (d+1) vt]
          TTuple vtl        -> tag_t d "t" $ map (sig_vt $ d+1) vtl
          TCollection ct et -> tag   d "c" [sig_ct (d+1) ct, sig_vt (d+1) et]
          TAddress          -> tag   d "a" []
          TTarget arg_t     -> tag   d "h" [sig_bt (d+1) arg_t]
        sig_ct d TSet  = tag d "S" []
        sig_ct d TBag  = tag d "B" []
        sig_ct d TList = tag d "L" []
        tag_t d s l = tag d s $ (show $ length l):l
        tag d s l = {-(show d)++-} s++concat l

-- Reconstruct a type from a signature, with empty annotations.
type_of_signature :: String -> Type [a]
type_of_signature s = evalState t_sig s
  where 
    inc = get >>= put . tail
    getChar :: State String Char
    getChar = get >>= return . head
    getCharInc = do c <- getChar
                    inc
                    return c
    getNumStr = do c <- getCharInc
                   cNext <- getChar
                   case (isNumber c, isNumber cNext) of
                     (True, True) -> getNumStr >>= return . (c:)
                     (True, _   ) -> return $ [c]
                     (False, _  ) -> return $ ""
    getNum :: State String Int
    getNum = getNumStr >>= return . read
    t_sig = do 
      c <- getCharInc
      case c of
        'F' -> do a <- vt_sig
                  r <- vt_sig
                  return $ TFunction a r
        'V' -> vt_sig >>= return . TValue
        _   -> error "invalid tag for type"
    vt_sig = do 
      c <- getCharInc
      case c of
        'I' -> mt_sig >>= return . TIsolated
        'C' -> mt_sig >>= return . TContained
        _   -> error "invalid tag for value type"
    mt_sig = do 
      c <- getCharInc 
      case c of
        'M' -> do {b <- bt_sig; return $ TMutable b []}
        'U' -> do {b <- bt_sig; return $ TImmutable b []}
        _   -> error "invalid tag for mutable type"
    bt_sig = do 
      c <- getCharInc
      case c of
        'k' -> return TUnknown
        'n' -> return TUnit
        'b' -> return TBool
        'y' -> return TByte
        'i' -> return TInt
        'd' -> return TFloat
        's' -> return TString
        'o' -> vt_sig >>= return . TMaybe
        't' -> do num <- getNum
                  vt_sig_l num >>= return . TTuple
        'c' -> do c <- ct_sig 
                  e <- vt_sig
                  return $ TCollection c e
        'a' -> return TAddress
        'h' -> bt_sig >>= return . TTarget
        _ -> error "invalid tag for base type"
    ct_sig = do 
      c <- getCharInc
      case c of
        'S' -> return TSet
        'B' -> return TBag
        'L' -> return TList
        _   -> error "invalid tag for collection type"
    vt_sig_l n = 
      if n == 0 then return []
      else do 
        v <- vt_sig 
        vs <- vt_sig_l $ n-1
        return $ v:vs

-- AST testers
is_peek e = case tag_of_expr e of 
  Peek -> True 
  _ -> False

-- AST destructors
nth e i = (sub_tree e) !! i

decompose_add e = case tag_of_expr e of 
  Add -> (nth e 0, nth e 1) 
  _ -> error "not Add"
decompose_aggregate e = case tag_of_expr e of 
  Aggregate -> (nth e 0, nth e 1, nth e 2) 
  _ -> error "not Aggregate"
decompose_apply e = case tag_of_expr e of
  Apply -> (nth e 0, nth e 1) 
  _ -> error "not Apply"
decompose_assign e = case tag_of_expr e of 
  Assign -> (nth e 0, nth e 1) 
  _ -> error "not Assign"
decompose_block e = case tag_of_expr e of 
  Block -> sub_tree e 
  _ -> error "not a Block"
decompose_combine e = case tag_of_expr e of 
  Combine -> (nth e 0, nth e 1) 
  _ -> error "not a Combine"
decompose_delete e = case tag_of_expr e of 
  Delete -> (nth e 0, nth e 1) 
  _ -> error "not a Delete"
decompose_deref e = case tag_of_expr e of 
  Deref -> (nth e 0) 
  _ -> error "not a Deref"
decompose_eq e = case tag_of_expr e of 
  Eq -> (nth e 0, nth e 1) 
  _ -> error "not an Equals"
decompose_filter_map e = case tag_of_expr e of 
  FilterMap -> (nth e 0, nth e 1, nth e 2) 
  _ -> error "not a FilterMap"
decompose_flatten e = case tag_of_expr e of 
  Flatten -> nth e 0 
  _ -> error "not a Flatten"
decompose_gbagg e = case tag_of_expr e of 
  GroupByAggregate -> (nth e 0, nth e 1, nth e 2, nth e 3) 
  _ -> error "not a GroupByAggregte"
decompose_ifthenelse e = case tag_of_expr e of 
  IfThenElse -> (nth e 0, nth e 1, nth e 2) 
  _ -> error "not a IfThenElse"
decompose_insert e = case tag_of_expr e of 
  Insert -> (nth e 0, nth e 1) 
  _ -> error "not a Insert"
decompose_iterate e = case tag_of_expr e of 
  Iterate -> (nth e 0, nth e 1) 
  _ -> error "not a Iterate"
decompose_just e = case tag_of_expr e of
  K3.Just -> nth e 0 
  _ -> error "not a Just"
decompose_lambda e = case tag_of_expr e of 
  Lambda _ -> nth e 0 
  _ -> error "not a Lambda"
decompose_leq e = case tag_of_expr e of 
  Leq -> (nth e 0, nth e 1) 
  _ -> error "not a Leq"
decompose_lt e = case tag_of_expr e of 
  Lt -> (nth e 0, nth e 1) 
  _ -> error "not a Lt"
decompose_map e = case tag_of_expr e of 
  Map -> (nth e 0, nth e 1) 
  _ -> error "not a Map"
decompose_mult e = case tag_of_expr e of 
  Mult -> (nth e 0, nth e 1) 
  _ -> error "not a Mult"
decompose_neg e = case tag_of_expr e of 
  Neg -> nth e 0 
  _ -> error "not a Neg"
decompose_neq e = case tag_of_expr e of 
  Neq -> (nth e 0, nth e 1) 
  _ -> error "not a Neq"
decompose_peek e = case tag_of_expr e of 
  Peek -> nth e 0 
  _ -> error "not a Peek"
decompose_range e = case tag_of_expr e of
  Range _ -> (nth e 0, nth e 1, nth e 2) 
  _ -> error "not a Range"
decompose_send e = 
  let rest i acc = if i == 1 then acc else rest (i-1) (nth e i:acc)
  in case tag_of_expr e of 
    Send -> (nth e 0, nth e 1, rest ((length $ sub_tree e)-1) []) 
    _ -> error "not a Send"
decompose_singleton e = case tag_of_expr e of
  Singleton vt -> nth e 0 
  _ -> error "not a Singleton"
decompose_slice e = case tag_of_expr e of 
  Slice -> (nth e 0, nth e 1) 
  _ -> error "not a Slice"
decompose_sort e = case tag_of_expr e of 
  Sort -> (nth e 0, nth e 1) 
  _ -> error "not a Sort"
decompose_tuple e = case tag_of_expr e of 
  Tuple -> sub_tree e  
  _ -> error "not a Tuple"
decompose_update e = case tag_of_expr e of 
  Update -> (nth e 0, nth e 1, nth e 2) 
  _ -> error "not an Update"

case_declaration id case_f l =
  case filter case_f l of
    []  -> error $ "No case found for "++id
    [x] -> x
    _   -> error $ "Multiple cases found for "++id

-- Declaration accessors
is_global (Global _ _ _,_)    = True
is_global (_,_)               = False
is_foreign (Foreign _ _,_)    = True
is_foreign (_,_)              = False
is_flow (Flow _,_)            = True
is_flow (_,_)                 = False
is_role (Role _ _,_)           = True
is_role (_,_)                 = False
is_def_role (DefaultRole _,_) = True
is_def_role (_,_)             = False

globals_of_program = filter is_global
flows_of_program   = filter is_flow

global_of_program id p = 
  case_declaration id (\(d,_) -> case d of
      Global n _ _ | n == id -> True
      _ -> False 
    ) $ globals_of_program p

-- Flow program accesors
is_source (Source _,_)                = True
is_source (_,_)                       = False
is_sink (Sink _,_)                    = True
is_sink (_,_)                         = False
is_generator (Source(Code _ _ _ _),_) = True
is_generator (_,_)                    = False
is_trigger (Sink(Code _ _ _ _),_)     = True
is_trigger (_,_)                      = False

endpoints_of_flow case_f fp = concat $ map unwrap endpoints 
  where unwrap (Source ep,_) = [ep] 
        unwrap (Sink ep,_)   = [ep] 
        unwrap (_,_)         = []
        endpoints = filter case_f fp

endpoints_of_program case_f p = endpoints_of_flow case_f flow_statements
  where flow_statements = concat $ map unwrap $ flows_of_program p
        unwrap (Flow p,_) = p
        unwrap (_,_) = []
  
sources_of_flow p    = endpoints_of_flow is_source p
sinks_of_flow p      = endpoints_of_flow is_sink p 
generators_of_flow p = endpoints_of_flow is_generator p
triggers_of_flow p   = endpoints_of_flow is_trigger p 

sources_of_program p    = endpoints_of_program is_source p
sinks_of_program p      = endpoints_of_program is_sink p 
generators_of_program p = endpoints_of_program is_generator p
triggers_of_program p   = endpoints_of_program is_trigger p 

trigger_of_program id p =
  case_declaration id (\fs -> case fs of
      Code n _ _ _ | n == id -> True
      _ -> False 
    ) $ triggers_of_program p

id_of_code (Code id _ _ _) = id
id_of_code _ = error "id_of_trig: not a Code"

expr_of_code (Code _ _ _ e) = e
expr_of_code _ = error "expr_of_code: not a Code"

args_of_code (Code _ a _ _) = a
args_of_code _ = error "args_of_code: not a Code"

-- Expression extraction

-- Returns all subexpressions caseing a given predicate
filter_expr f e =
  fold_tree
    (\x _ -> x)
    (\_ acc e -> (concat acc)++(if f e then [e] else []))
    Nothing [] e

-- Returns all variables in an expression
vars_of_expr e = uniq $ filter_expr is_var e

-- Returns the free variables in an expression
free_vars_of_expr e = uniq $ fold_tree add_bindings not_bound_var [] [] e
  where
    add_bindings env e = vars_of_lambda e++env
    not_bound_var env acc e =
      concat acc ++
        if is_var e && id_of_var e `notElem` env then [e] else [] 

-- Returns whether e2 is directly contained in e1
contains_expr e1 e2 = fold_tree (\x _ -> x) contains_aux Nothing False e1
  where 
    contains_aux _ contained e = or contained || e == e2

-- Substitutes any occurrences of the given bindings in an expression,
-- in a bottom-up, capture-avoiding fashion.
-- Assumes substitution function domain and range are (subtree) disjoint.
substitute_expr subs e = fold_tree remove_var sub_aux subs (e, []) e
  where 
    remove_var subs e =
      let vars = vars_of_lambda e in
      if vars == [] then subs
      else
        foldl' (\acc (src, dest) -> 
            if is_var src && id_of_var src `elem` vars then acc
            else acc++[(src,dest)]) 
          [] subs
    sub_aux subs parts_w_sub_ids e =
      let (parts, sub_ids) =
            let (x,y) = unzip parts_w_sub_ids 
            in (x, concat y)
          new_e = recompose_tree e parts in 
      case new_e `lookup` subs of
        Just sub_e -> 
           (sub_e, sub_ids++[(id_of_expr new_e, id_of_expr sub_e)])
        Nothing -> (new_e, sub_ids)
  
-- Linearizes (i.e. flattens) an expression tree to its constituent
-- subexpressions, in an order given by its first argument.
-- The first argument linearizes a single node and is of the form:
--   child linearizations -> node -> linearization  *)
linearize_expr f e = fold_tree (\x _ -> x) (\_ -> f) Nothing [] e

-- Common linearizations
pre_order_linearization children node = node:concat children
post_order_linearization children node = concat children++[node]

-- renumber ids for a whole program *)
renumber_program_ids prog = evalState get_prog 0
  where 
    get_prog = reverse <$> foldlM handle_dec [] prog
    handle_dec acc d = case d of
      (Global x y (Just e),a) -> do
        ast <- renumber_ast_ids e
        return $ (Global x y (Just ast), a):acc
      (Flow fs, a) -> do
        flows <- foldlM handle_flow [] fs
        return $ (Flow $ reverse flows, a):acc
      (Role id fs,a) -> do
        flows <- foldlM handle_flow [] fs
        return $ (Role id (reverse flows), a):acc
      x -> return $ x:acc
    handle_flow acc f = case f of
      (Source(Code x y z e),a) -> do
        c <- handle_code x y z e
        return $ (Source c, a):acc
      (Sink(Code x y z e),a) -> do 
        c <- handle_code x y z e
        return $ (Sink c, a):acc
      x -> return $ x:acc
    handle_code x y z e = renumber_ast_ids e >>= return . Code x y z

-- Produce the same tree of new ids top down
renumber_ast_ids :: Expr a -> State Int (Expr a)
renumber_ast_ids exp =
  foldTree1M getNext modify 0 exp
  where 
    modify i acc_children t =
      return $ mk_tree $ (((i, tag_of_expr t), meta_of_expr t), acc_children)
    getNext _ _ = do {i <- get; put $ i+1; return i}

list_of_k3_container e = 
  case tag_of_expr e of
    Combine -> let (l, r) = decompose_combine e in
      list_of_k3_container l ++ list_of_k3_container r
    Empty _ -> []
    Singleton _ -> [decompose_singleton e]
    _ -> error "not a k3 list"

k3_container_of_list typ l = case l of
    []   -> mk_empty typ
    [x]  -> mk_singleton typ x
    x:xs -> mk_combine (k3_container_of_list typ [x]) $
      k3_container_of_list typ xs


