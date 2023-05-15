type t

val make_map : string -> t
val get_tile : t -> int -> int -> int
val get_spawn : t -> int * int
val get_exit : t -> (int * int) list
val get_next : t -> int
val get_coords : t -> int array array
val in_solid : t -> int -> int -> bool
val in_spike : t -> int -> int -> bool * int
