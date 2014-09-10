open Util

module type OrderedKeyType = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val filter_idxs : IntSet.t -> t -> t
end
