type t
(** [t] is the type of the player character *)

type key_pressed = {
  a : bool;
  d : bool;
  space : bool
}
(** [key_presssed] represents which keys are being presssed *)

val init : int -> int -> t
(** [init x y v] spawns a player charater at (x,y) with speed v*)

val get_x : t -> int
(** [get_x p] is the current the x coordinate of player [p]*)

val get_y : t -> int
(** [get_y p] is the current the y coordinate of player [p]*)

val move : t -> key_pressed -> t
(** [move p] moves the player according to which keys are being pressed *)

val sprite_set : string list
(** [sprite_set] is a list of paths to player sprites *)

val sprite : t -> int -> int
(** [sprite p] is the index in [sprite_set] to render of player [p]*)

val fall : t -> t

val is_falling : t -> bool

val set_falling : t -> bool -> t
