module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module type H =
  sig
    type elt
    type t
    val is_empty : t -> bool
    val singleton : elt -> t
    val size : t -> int
    val insert : t -> elt -> unit
    val peek : t -> elt
    val remove : t -> elt
  end

module Make(Ord : OrderedType) : H with type elt = Ord.t
