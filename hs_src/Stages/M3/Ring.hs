module Stages.M3.Ring where
-- Typechecks

import qualified Util.ListAsSet as LAS
import Data.List (foldl')
import Data.Foldable (foldlM)

{-
   A module for representing expressions over a ring functorized over a based 
   type.  The module also includes tools for transforming arbitrary expressions
   into polynomials, and for computing deltas of expressions.
-}

{- The base type t over which we define the ring, plus its zero and one
   elements.
-}
{-module type Base =-}
{-sig-}
  {-type t-}
  {-val  zero: t-}
  {-val  one:  t-}
{-end-}
{-;;-}

class Eq l => Ringable l where
  ring_zero :: l
  ring_one :: l
  zero :: Expr l
  zero = Val ring_zero
  one :: Expr l
  one = Val ring_one


{- We build rings using the Make functor over the Base type.
   Examples are in unit/test/ring.ml.
-}
   {- the type of base values. -}
   {-data Leaf-}

   {- expression type. The implementation is hidden (abstract), so it
      can only be accessed through special access functions.

      Making expr_t abstract:
      Since the nesting structure of an expression in general matters to us,
      being flexible in the implementation may not be so necessary. However,
      we want the user to use mk_sum and m_prod to construct sums and
      products, rather than the Sum and Prod constructors from expr_t, because
      the mk-functions automatically eliminate certain redundancies on the
      fly that the implementations of other functions of Ring may
      depend upon. 
   -}
data Expr l = Val l
    | Sum  [Expr l]
    | Prod [Expr l]
    | Neg  (Expr l)
    deriving (Show, Eq)


-- constant multiplicity represented by int; could be an arbitrary
-- constant ring element.
type Mono l = (Int, [l])

   -- Comparison options to allow for order-independance in Sum/Prod terms
data CmpOpt = OptSumOrderIndependent
      | OptProdOrderIndependent
      deriving (Show, Eq)

{- For using Ring, there should be no need to read on below.
   The following code implements a ring with some standard operations
   on expressions; there is nothing specific to DBToaster in here.


   Representation invariant:
   * If the value of an expression is zero, it is syntactically represented
   as zero. (We automatically rewrite 0*x=x*0 to 0 and 0+x=x+0 to x.)

   We also always represent 1*x=x*1 as x.

   But we do not internally represent expressions as polynomials, because
   this transformation may produce exponential blow-up. So the function
   for conversion to a polynomial is not a dummy.
-}

{-zero = Val(T.zero)  (** Sum [] *)-}
{-one  = Val(T.one)   (** Prod [] *)-}

-- Default comparison options. See cmp_opt_t
default_cmp_opts = [OptSumOrderIndependent, OptProdOrderIndependent]

{- returns a list l of expressions such that (mk_sum l) resp. (mk_prod l)
  are equivalent to the input.

  Applied to a polynomial, sum_list returns the list of monomials.
  Applied to a monomial, prod_list returns the list of the constituent
  base values. But both functions can be applied to any expression. 
-}
sum_list (Sum l) = l 
sum_list e = [e]
prod_list (Prod l) = l
prod_list e = [e]

-- turns a value of type T.t into an expression (consisting just of
--      that value)
mk_val a = Val a

-- turns a list l of expressions into \bigsum l and \bigprod l, resp.
mk_sum :: Ringable l => [Expr l] -> Expr l
mk_sum l =
  let l2 = filter (\x -> x /= zero) l in
  if null l2 then zero
  else if null $ tail l2 then head l2
       else Sum $ concat $ map sum_list l2

mk_prod :: Ringable l => [Expr l] -> Expr l
mk_prod l =
  let zeroes = filter (\x -> x == zero) l in
  if not $ null zeroes then zero
  else
      let l2 = filter (\x -> x /= one) l in
      if null l2 then one
      else if null $ tail l2 then head l2
           else Prod $ concat $ map prod_list l2

mk_neg :: Expr l -> Expr l
mk_neg (Neg e) = e
mk_neg e = Neg e

{-exception NotAValException of expr_t-}
get_val (Val x) = x
get_val e = error $ show e ++ " is not a val"

{- (fold sum_f prod_f leaf_f e) folds the expression by proceeding
      bottom-up, applying
      sum_f  to the fold result of the constituents of sums;
      prod_f to the fold result of the constituents of products;
      neg_f to the fold result of e in an expression -e; and
      leaf_f to leaves.

      For example, (apply_to_leaves f e) below can be implemented as

      (fold (fun x -> mk_sum x) (fun x -> mk_prod x)
            (fun x -> mk_neg x) (fun x -> (f x)) e)
-}
fold :: ([b] -> b) -> ([b] -> b) -> (b -> b) -> (l -> b) -> Expr l -> b
fold sum_f prod_f neg_f leaf_f e =
  case e of
    Sum l  -> sum_f $ map (fold sum_f prod_f neg_f leaf_f) l
    Prod l -> prod_f $ map (fold sum_f prod_f neg_f leaf_f) l
    Neg x  -> neg_f $ fold sum_f prod_f neg_f leaf_f x
    Val x  -> leaf_f x

-- (apply_to_leaves f e)  applies function f to each base value leaf of
--    expression e, i.e., replaces the base value v by expression (f v)
apply_to_leaves :: Ringable l => (l -> Expr l) -> Expr l -> Expr l
apply_to_leaves f e = fold mk_sum mk_prod mk_neg f e

-- returns the list of leaves of an expression in top-down left to right
--      traversal order.
leaves :: Expr l -> [l]
leaves e = fold concat concat id (\x -> [x]) e

-- (substitute f e1 e2 e3) replaces each occurrence of e1 in e3 by e2.
substitute :: Ringable l => Expr l -> Expr l -> Expr l -> Expr l
substitute replace_this by_that in_expr =
  case in_expr of
    x | x == replace_this -> by_that
    Sum l  -> mk_sum $ map (substitute replace_this by_that) l
    Prod l -> mk_prod $ map (substitute replace_this by_that) l
    Neg e  -> mk_neg $ substitute replace_this by_that e
    x@(Val _)  -> x

{- make a set of substitutions given by a list of
  (replace_this by_that) pairs. The order in which the replacements are
  made is arbitrary, so you must not have dependent chain of replacements
  such as in (substitute_many [(a,b), (b,c)] e); you must not assume that
  this will replace a by c in e. There should not be any overlap between
  the domain and the image of the mapping specified by l.
-}
substitute_many l in_expr = foldr (\(x,y) -> substitute x y) l in_expr

{- (extract sum_combinator_f prod_combinator_f neg_f leaf_f expr);

  folds an expression into a pair of values, where the first is

  (fold sum_combinator_f prod_combinator_f neg_f
        (fun lf -> let (x, y) = leaf_f lf in x) expr)

  and the second is

  (apply_to_leaves (fun lf -> let (x, y) = leaf_f lf in y) expr)
-}
extract :: Ringable l => ([a] -> a) -> ([a] -> a) -> (a -> a) -> 
           (l -> (a, Expr l)) -> Expr l -> (a, Expr l)
extract sum_combinator prod_combinator neg_f leaf_f expr =
    let f comb_f1 comb_f2 l =
          let (a, b) = unzip l
          in (comb_f1 a, comb_f2 b)
    in
    fold (f sum_combinator mk_sum) (f prod_combinator mk_prod)
        (\(x,y) -> (neg_f x, mk_neg y))
        leaf_f expr

{- (delta f e) returns an expression such that

  mk_sum [e; (delta f e)]

  is equivalent to, but simpler than,

  (apply_to_leaves (fun x -> mk_sum [(mk_val x); f x]) e)

  by exploiting linearity and something related to
  the product rule of differentiation in calculus.

  That is, if (f x) expresses change to a leaf base value x of the
  expression e, then (delta f e) expresses the overall change to e.
-}
delta :: Ringable l => (l -> Expr l) -> Expr l -> Expr l
delta lf_delta e =
    case e of
      Sum l     -> mk_sum $ map (delta lf_delta) l
      Prod []   -> zero
      Prod(x:l) ->
        mk_sum $ [mk_prod $ (delta lf_delta x):l,
          mk_prod $ [mk_sum [x, delta lf_delta x],
            delta lf_delta $ mk_prod l]]
        -- this nesting makes sense because it renders the delta
        --  computation cheaper: delta for l is only computed once here;
        --  we can still distribute later if we need it.
      Neg x -> Neg $ delta lf_delta x
      Val x -> lf_delta x

{- a polynomial is a sum of monomials.
  A monomial is a (possibly negated) product of base values.
  turns an arbitrary expression into a polynomial represents as a list
  of monomials represented as mono_t values.
-}
polynomial :: Eq l => Expr l -> [Mono l]
polynomial e = case e of
    Val x  -> [(1, [x])]
    Neg x  -> let l = (polynomial x)
              in map (\(c, v) -> (-c, v)) l
    Sum l  -> concat $ map polynomial l
    Prod l -> let mono_prod ml =
                    let (a,b) = unzip ml
                        lmult l = foldl' (*) 1 l
                    in (lmult a, concat b)
              in (map mono_prod $ LAS.distribute $ map polynomial l)

-- turns a monomial into a product expression
monomial_to_expr :: Ringable l => Mono l -> Expr l
monomial_to_expr (m, l) =
  let list_to_prod l = mk_prod $ map mk_val l in
  case m of
    1  -> list_to_prod l
    -1 -> mk_neg $ list_to_prod l
    _  -> error "Ring.monomial_to_expr: general multiplicities not implemented"

-- turns the input expression into a sum of products.
polynomial_expr :: Ringable l => Expr l -> Expr l
polynomial_expr e = mk_sum $ map monomial_to_expr $ polynomial e

{-exception CannotCastToMonomialException of expr_t-}

-- casts an expression to a monomial. If that is not possible
--  (because of the presence of sums or zeros, an exception is thrown.
cast_to_monomial e = maybe (error "Can't cast " ++ show e ++ " to monomial") 
  id $ cast_to_monomial_inner e

-- check if an expression is a monomial
cast_to_monomial_inner :: Eq l => Expr l -> Maybe [l]
cast_to_monomial_inner e =
  let ms = polynomial e in case ms of
    (1, m):[]  -> Just m
    (-1, m):[] -> error "Ring.cast_to_monomial TODO"
    _          -> Nothing

try_cast_to_monomial :: Eq l => Expr l -> Bool
try_cast_to_monomial e = case cast_to_monomial_inner e of
    Just _  -> True
    Nothing -> False


{- simplifies expressions by unnesting sums of sums and products
  of products, and simplifies using ones and zeros.
  The only real difference to the function polynomial is that
  products of sums are not turned into sums of products using
  distributivity. Thus, while polynomial may create exponential-factor
  blow-up in the worst case, simplify is always polynomial-time.
-}
simplify :: Ringable l => Expr l -> Expr l
simplify e = apply_to_leaves mk_val e

{- cmp_exprs sum_f prod_f leaf_f a b -> 
      
      Helper function for comparing expressions.  
      
      Performs a DFS in parallel over two expressions.  If any differences are
      encountered between the two expressions, cmp_exprs will immediately return
      None.  
      
      
      cmp_leaf will be invoked to determine whether two leaves are equivalent
      and should return None if they are not.  Metadata about the comparison
      may be returned if they are equivalent in isolation.
      
      sum_f and prod_f are invoked on values returned by cmp_leaf and should be
      used to ensure that the returned metadata is consistent.  If so, metadata
      regarding the entire sum or product should be returned.      
      
      A positive result (Some(x)) is guaranteed to be an equivalent expr, while
      a negative result (None) only indicates that we were not able to establish
      equivalence.  Among other things, form normalization and double-negation  
      are not handled properly (yet).
-}

cmp_exprs :: Ringable l => ([a] -> Maybe a) -> 
    ([a] -> Maybe a) -> (l -> l -> Maybe a) -> Expr l -> Expr l -> Maybe a
cmp_exprs = cmp_exprs_opts default_cmp_opts

cmp_exprs_opts :: Ringable l => [CmpOpt] -> ([a] -> Maybe a) -> 
    ([a] -> Maybe a) -> (l -> l -> Maybe a) -> Expr l -> Expr l -> Maybe a
cmp_exprs_opts cmp_opts sum_f prod_f leaf_f a b =
  let sum_order_indep = OptSumOrderIndependent `elem` cmp_opts
      prod_order_indep = OptProdOrderIndependent `elem` cmp_opts
      rcr a b = cmp_exprs_opts cmp_opts sum_f prod_f leaf_f a b
      rcr_all order_indep merge_fn al bl = 
        if length al /= length bl then Nothing
        else 
          if order_indep then do 
            -- For each term of the first expression find the equivalent
            -- yet unmatched term from the second expression, if exists
            val <- 
              foldlM (\(mappings, bl_matched) a -> do
                let bl_unmatched = LAS.diff bl bl_matched
                    (found_b, mapping_if_found) = find_expr rcr a bl_unmatched
                new_mapping <- mapping_if_found 
                return $ (new_mapping : mappings, found_b : bl_matched)
              ) ([],[]) al 
            merge_fn $ fst val
          else do
            vals <- mapM (\(a, b) -> rcr a b) $ zip al bl
            merge_fn vals
  in
  case (a,b) of
  (Val  xa, Val  xb) -> leaf_f xa xb
  (Neg  ae, Neg  be) -> rcr ae be 
  (Sum  ae, Sum  be) -> rcr_all sum_order_indep  sum_f  ae be 
  (Prod ae, Prod be) -> rcr_all prod_order_indep prod_f ae be 
  _ -> Nothing
    
find_expr :: Ringable l => (Expr l -> Expr l -> Maybe a) -> Expr l -> 
  [Expr l] -> (Expr l, Maybe a)
find_expr cmp_f a bl =
    foldl' (\x@(e, mapping) b ->
      case mapping of
        Nothing -> x
        _ -> (b, cmp_f a b)
    ) (zero, Nothing) bl 

{- multiply_out lhs sum rhs 
  
  Shorthand operation that multiplies every sum term in sum by lhs and rhs
  and returns the resultant list of expressions
-}
multiply_out :: Ringable l => [Expr l] -> Expr l -> [Expr l] -> [Expr l]
multiply_out lhs sum rhs = map (\ x -> mk_prod $ lhs++[x]++rhs) $ sum_list sum



