(* The K3 Programming Language *)

open K3AST
open K3Annotations

(* Functorized AST construction *)
module GenericAST :
  functor (Annotation : ASTAnnotationType) ->
    ( ASTType with type annotation_t = Annotation.annotation_t)

(* Default, untyped AST construction *)
module rec AST : ( ASTType with type annotation_t = Annotation.annotation_t )
and Annotation : ( AnnotationType with type type_t = AST.type_t
                                   and type expr_t = AST.expr_t )