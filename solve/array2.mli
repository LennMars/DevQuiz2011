type 'a t = int ref * 'a * 'a array array
val length : 'a t -> int
val make : int -> 'a -> 'a t
val clean : 'a t -> unit
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val swap : int -> int -> 'a t -> unit

