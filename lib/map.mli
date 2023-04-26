type t

val make_map: string -> t
val get_tile: t -> int -> int -> int
val get_spawn: t -> int * int
val get_exit: t -> int * int
val check_collision: t -> int -> int -> bool