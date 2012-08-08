open K3Annotations
open CPP
open CPP.CPPImpl.AST
open CPP.CPPTarget

val cpp_of_imperative :
  (type_t * annotations_t) program_t -> (type_t * annotations_t) program_t