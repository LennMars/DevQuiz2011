type state
val debug : bool ref
val closed_max_length : int
type dir = L | R | U | D
exception Invalid_flip
val input : string -> state
val mask : state -> int list -> state
val fix : state -> int list -> state
val get_size : state -> int * int
val opposite : dir -> dir
val dir_to_string : dir -> string
val char_to_dir : char -> dir
val apply : state -> dir list -> state
val make_masks : state -> int list list
val p : state -> unit
val perr : state -> unit
val answer : state -> state
val solve : state -> dir list option
