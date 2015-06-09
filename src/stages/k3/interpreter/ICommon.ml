open Util
open K3.AST

module type OrderedKeyType = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
end
