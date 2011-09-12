open Util

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

module Make(Ord : OrderedType) =
struct
  module A = Array2
  type elt = Ord.t
  type t = int ref * elt A.t

  let is_empty (l, _) = !l = 0

  let singleton x = ref 1, A.make 1 x

  let size (l, a) = !l

  let parent n = (n - 1) / 2

  let rec up a n =
    if n = 0 then ()
    else
      let x = A.get a n and y = A.get a (parent n) in
      if Ord.compare x y < 0 then A.swap n (parent n) a; up a (parent n)

  let insert (l, a) x =
        A.set a !l x;
        up a !l;
        incr l

  let peek (_, a) = A.get a 0

  let rec down (l, a) n =
    let cl, cr = 2 * n + 1, 2 * n + 2 in
    if cl >= !l then (* no child *)
    ()
    else if cr = !l then (* one child *)
      let me, clx = A.get a n, A.get a cl in
      if Ord.compare me clx > 0 then A.swap n cl a;
    else (* two children *)
      let me, clx, crx = A.get a n, A.get a cl, A.get a cr in
      if Ord.compare me clx > 0 || Ord.compare me crx > 0 then
        if Ord.compare clx crx > 0 then
          (A.swap n cr a; down (l, a) cr)
        else
          (A.swap n cl a; down (l, a) cl)

  let remove (l, a) =
    if !l = 0 then invalid_arg "remove";
    decr l;
    let x = A.get a 0 in
    if !l = 0 then A.clean a
    else (A.swap 0 !l a; down (l, a) 0);
    x

end
