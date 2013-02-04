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

class Ringable a where
  zero :: Ringable a
  one :: Ringable a


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
        Sum  [Expr l]
        Prod [Expr l]
        Neg  [Expr l]


   -- constant multiplicity represented by int; could be an arbitrary
   -- constant ring element.
   type Mono l = (Int, [l])

   -- Comparison options to allow for order-independance in Sum/Prod terms
   data CmpOpt = 
      | OptSumOrderIndependent
      | OptProdOrderIndependent

   -- the zero- and one-elements of the ring
   val zero: expr_t
   val one:  expr_t
    
   val default_cmp_opts : cmp_opt_t list

   (** constructing a (nested) expression using base values,
      sums, and products of expressions *)

   (** turns a value of type T.t into an expression (consisting just of
      that value) *)
   val mk_val: leaf_t -> expr_t

   (** turns a list l of expressions into \bigsum l and \bigprod l, resp. *)
   val mk_sum:  (expr_t list) -> expr_t
   val mk_prod: (expr_t list) -> expr_t
   val mk_neg:  expr_t -> expr_t

   (** accessing the contents of an expression *)
   exception NotAValException of expr_t
   val get_val: expr_t -> leaf_t

   (** returns a list l of expressions such that (mk_sum l) resp. (mk_prod l)
      are equivalent to the input.

      Applied to a polynomial, sum_list returns the list of monomials.
      Applied to a monomial, prod_list returns the list of the constituent
      base values. But both functions can be applied to any expression. *)
   val  sum_list: expr_t -> (expr_t list)
   val prod_list: expr_t -> (expr_t list)


   (** some functions for constructing modified expressions *)

   (** (fold sum_f prod_f leaf_f e) folds the expression by proceeding
      bottom-up, applying
      sum_f  to the fold result of the constituents of sums;
      prod_f to the fold result of the constituents of products;
      neg_f to the fold result of e in an expression -e; and
      leaf_f to leaves.

      For example, (apply_to_leaves f e) below can be implemented as

      (fold (fun x -> mk_sum x) (fun x -> mk_prod x)
            (fun x -> mk_neg x) (fun x -> (f x)) e)
   *)
   val fold: ('b list -> 'b) -> ('b list -> 'b) -> ('b -> 'b) ->
             (leaf_t -> 'b) -> expr_t -> 'b


   (** (apply_to_leaves f e)  applies function f to each base value leaf of
      expression e, i.e., replaces the base value v by expression (f v) *)
   val apply_to_leaves: (leaf_t -> expr_t) -> expr_t -> expr_t

   (** returns the list of leaves of an expression in top-down left to right
      traversal order. *)
   val leaves: expr_t -> (leaf_t list)

   (** (substitute f e1 e2 e3) replaces each occurrence of e1 in e3 by e2. *)
   val substitute: expr_t -> (** replace_this *)
                   expr_t -> (** by_that *)
                   expr_t -> (** in_expr *)
                   expr_t    (** returns expr with substitutions applied *)

   (** make a set of substitutions given by a list of
     (replace_this by_that) pairs. The order in which the replacements are
     made is arbitrary, so you must not have dependent chain of replacements
     such as in (substitute_many [(a,b), (b,c)] e); you must not assume that
     this will replace a by c in e. There should not be any overlap between
     the domain and the image of the mapping specified by l.
   *)
   val substitute_many: ((expr_t * expr_t) list) -> expr_t -> expr_t

   (** (extract sum_combinator_f prod_combinator_f neg_f leaf_f expr);

      folds an expression into a pair of values, where the first is

      (fold sum_combinator_f prod_combinator_f neg_f
            (fun lf -> let (x, y) = leaf_f lf in x) expr)

      and the second is

      (apply_to_leaves (fun lf -> let (x, y) = leaf_f lf in y) expr)
   *)
   val extract: ('a list -> 'a) -> ('a list -> 'a) -> ('a -> 'a) ->
                (leaf_t -> ('a * expr_t)) -> expr_t -> ('a * expr_t)

   val delta: (leaf_t -> expr_t) -> expr_t -> expr_t

   (** a polynomial is a sum of monomials.
      A monomial is a (possibly negated) product of base values.
      turns an arbitrary expression into a polynomial represents as a list
      of monomials represented as mono_t values.
   *)
   val polynomial: expr_t -> (mono_t list)
   (** val polynomial: expr_t -> expr_t *)

   (** turns a monomial into a product expression *)
   val monomial_to_expr: mono_t -> expr_t

   (** turns the input expression into a sum of products. *)
   val polynomial_expr:  expr_t -> expr_t

   (** casts an expression to a monomial. If that is not possible
      (because of the presence of sums or zeros, an exception is thrown. *)
   val cast_to_monomial: expr_t -> (leaf_t list)
   exception CannotCastToMonomialException of expr_t

   (** check if an expression is a monomial *)
   val try_cast_to_monomial: expr_t -> bool

   (** simplifies expressions by unnesting sums of sums and products
      of products, and simplifies using ones and zeros.
      The only real difference to the function polynomial is that
      products of sums are not turned into sums of products using
      distributivity. Thus, while polynomial may create exponential-factor
      blow-up in the worst case, simplify is always polynomial-time.
   *)
   val simplify: expr_t -> expr_t
   
   (** cmp_exprs sum_f prod_f leaf_f a b -> 
      
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
   *)
   val cmp_exprs: ?cmp_opts:cmp_opt_t list -> 
                  ('a list -> 'a option) ->
                  ('a list -> 'a option) ->
                  (leaf_t -> leaf_t  -> 'a option) ->
                  expr_t -> expr_t -> 'a option
   
   (** multiply_out lhs sum rhs 
      
      Shorthand operation that multiplies every sum term in sum by lhs and rhs
      and returns the resultant list of expressions
   *)
   val multiply_out: expr_t list -> expr_t -> expr_t list -> expr_t list
end




(** For using Ring, there should be no need to read on below.
   The following code implements a ring with some standard operations
   on expressions; there is nothing specific to DBToaster in here.


   Representation invariant:
   * If the value of an expression is zero, it is syntactically represented
   as zero. (We automatically rewrite 0*x=x*0 to 0 and 0+x=x+0 to x.)

   We also always represent 1*x=x*1 as x.

   But we do not internally represent expressions as polynomials, because
   this transformation may produce exponential blow-up. So the function
   for conversion to a polynomial is not a dummy.
*)




