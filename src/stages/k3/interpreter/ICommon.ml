open Util
open K3.AST

module type OrderedKeyType = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val filter_idxs : index_t -> t -> t
end
