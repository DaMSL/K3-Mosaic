module Make where

import qualified Util.ListAsSet as LAS

{-zero = Val(T.zero)  (** Sum [] *)-}
{-one  = Val(T.one)   (** Prod [] *)-}

-- Default comparison options. See cmp_opt_t
default_cmp_opts = [OptSumOrderIndependent; OptProdOrderIndependent]

sum_list Sum l = l 
sum_list _ = [e]
prod_list Prod l = l
prod_list _ = [e]

mk_val a = Val a

-- any construction of complex expressions is done with mk_sum and mk_prod,
-- which enforce the representation invariant.
--
mk_sum :: Ringable a => [a] -> Expr a
mk_sum l =
  let l2 = filter (\x -> x /= zero) l in
  if null l2 then zero
  else if null $ tail l2 then head l2
       else Sum $ concat $ map sum_list l2

mk_prod :: Ringable a => [a] -> Expr a
mk_prod l =
  let zeroes = filter (\x -> x == zero) l in
  if not $ null zeroes then zero
  else
      let l2 = filter (\x -> x /= one) l in
      if null l2 then one
      else if null $ tail l2 then head l2
           else Prod $ concat $ map prod_list l2

mk_neg (Neg e) = e
mk_neg e = e

{-exception NotAValException of expr_t-}
get_val (Val x) = x
get_val _ = error $ show e ++ " is not a val"

fold :: ([b] -> b) -> ([b] -> b) -> (b -> b) -> (l -> b) -> Expr
fold sum_f prod_f neg_f leaf_f e =
  case e of
    Sum l  -> sum_f $ map (fold sum_f prod_f neg_f leaf_f) l
    Prod l -> prod_f $ map (fold sum_f prod_f neg_f leaf_f) l)
    Neg x  -> neg_f $ fold sum_f prod_f neg_f leaf_f x
    Val x  -> leaf_f x

apply_to_leaves :: (l -> Expr) -> Expr
apply_to_leaves f e = fold mk_sum mk_prod mk_neg f e

leaves :: Expr -> [l]
leaves e = fold concat concat id (\x -> [x]) e

substitute replace_this by_that in_expr =
  case in_expr of
    x | x == replace_this -> by_that
    Sum l  -> mk_sum $ map (substitute replace_this by_that) l
    Prod l -> mk_prod $ map (substitute replace_this by_that) l
    Neg e  -> mk_neg $ substitute replace_this by_that e
    x@(Val _)  -> x

substitute_many l in_expr = foldr (\(x,y) -> substitute x y) l in_expr

extract :: ([a] -> a) -> ([a] -> a) -> (a -> a) -> (l -> (a, Expr)) -> 
    Expr -> (a, Expr)
extract sum_combinator prod_combinator neg_f leaf_f expr =
    let f comb_f1 comb_f2 l =
        let (a, b) = unzip l in
        (comb_f1 a, comb_f2 b)
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
      Sum l     -> mk_sum $ map (delta . lf_delta) l
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

polynomial :: Expr l -> [Mono l]
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

monomial_to_expr :: Mono l -> Expr l
monomial_to_expr (m, l) =
  let list_to_prod l = mk_prod $ map mk_val l in
  case m of
    1  -> list_to_prod l
    -1 -> mk_neg $ list_to_prod l
    _  -> error "Ring.monomial_to_expr: general multiplicities not implemented"

polynomial_expr :: Expr l -> Expr l
polynomial_expr e = mk_sum $ map monomial_to_expr $ polynomial e

{-exception CannotCastToMonomialException of expr_t-}

cast_to_monomial e = case cast_to_monomial_inner e of
  Just x  -> x
  Nothing -> error "Can't cast " ++ e ++ " to monomial"

cast_to_monomial_inner :: Expr l -> Maybe [l]
cast_to_monomial_inner e =
  let ms = polynomial e in case ms of
    (1, m):[]  -> Just m
    (-1, m):[] -> error "Ring.cast_to_monomial TODO"
     _ -> Nothing

try_cast_to_monomial Expr l -> Bool =
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
simplify :: Expr l -> Expr l
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
cmp_exprs = cmp_exprs_opts default_cmp_opts

cmp_exprs_opts :: [CmpOpt] -> ([a] -> Maybe a) -> ([a] -> Maybe a) -> 
    (l -> l -> Maybe a) -> Expr l -> Expr l -> Maybe a
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
            val <- foldlM (\(mappings, bl_matched) a -> do
              let bl_unmatched = LAS.diff bl bl_matched
                  (found_b, mapping_if_found) = find_expr rcr a bl_unmatched
              new_mapping <- return mapping_if_found 
              return $ new_mapping : mappings, found_b : bl_matched
            ) (Just ([],[])) al 
            return $ merge_fn $ fst val
          else do
            vals <- mapM (\(a, b) -> return $ rcr a b) $ zip al bl
            merge_fn vals
  in
  case (a,b) of
  (Val  xa, Val  xb) -> leaf_f  xa xb
  (Neg  ae, Neg  be) -> rcr ae be 
  (Sum  ae, Sum  be) -> rcr_all sum_order_indep  sum_f  ae be 
  (Prod ae, Prod be) -> rcr_all prod_order_indep prod_f ae be 
  _ -> Nothing
    
find_expr :: (Expr l -> Expr l -> Maybe a) -> Expr l -> 
  [Expr l] -> (Expr, Maybe a)
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
multiply_out :: [Expr l] -> Expr l -> [Expr l] -> [Expr l]
multiply_out lhs sum rhs = map (\ x -> mk_prod $ lhs++[x]++rhs) $ sum_list sum



