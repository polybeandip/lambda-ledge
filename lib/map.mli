type t
(** [t] is the type of a map *)

val make_map : string -> t
(** [make_map path] is a [Map.t] representing the map data in the text file at
    path [path] *)

val get_tile : t -> int -> int -> int
(** [get_tile map x y] is the tile at coordinates (x,y) in [map] *)

val get_spawn : t -> int * int
(** [get_spawn map] is the spawn location of the player in [map] *)

val get_exit : t -> (int * int) list
(** [get_spawn map] is the list of coordinates of all exits the player can use
    to leave [map] *)

val get_next : t -> int
(** [get_next map] is the map the player moves to after passing through an exit *)

val get_coords : t -> int array array
(** [get_coords map] is an integer matrix where entry (x,y) is the tile at
    coordinates (x,y) *)

val in_solid : t -> int -> int -> bool
(** [in_solid map x,y] is true if the tile at (x,y) is solid and false otherwise *)

val in_spike : t -> int -> int -> bool * int
(** [in_spike map x,y] is true if the tile at (x,y) is a spike and false
    otherwise *)

val in_battery : t -> int -> int -> bool
(** [in_battery map x,y] is true if the tile at (x,y) is a battery and false
    otherwise *)

val add_used : t -> int -> int -> unit
(** [add_used map x,y] sets the battery at tile (x,y) to used, causing it to no
    longer be drawn on screen *)

val clear_used : t -> unit
(** [clear_used map] sets the list of used batteries to [\[\]] *)
