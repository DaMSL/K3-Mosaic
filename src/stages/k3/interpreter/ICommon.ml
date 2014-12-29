open Util
open K3.AST

module type OrderedKeyType = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val filter_idxs : index_t -> t -> t
  (* set values *other* than this set to min/max *)
  val filter_with_minmax : [`Min | `Max] -> index_t -> IntSet.t -> t -> t
  val to_string : t -> string
end
