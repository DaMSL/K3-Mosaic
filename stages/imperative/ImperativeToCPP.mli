open K3.Annotation
open CPP
open CPP.CPPImpl.AST
open CPP.CPPTarget

val cpp_of_imperative :
  (unit -> annotation_t) -> (type_t * annotation_t) program_t
  	-> (type_t * annotation_t) program_t