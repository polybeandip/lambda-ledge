type t
(** [t] is the type of the player character *)

type key_pressed = {
  l : int;
  r : int;
  u : int;
  d : int;
  c : int
}
(** [key_presssed] represents which keys are being presssed *)

val init : int -> int -> t
(** [init x y] spawns a player charater at (x,y) *)

val get_x : t -> int
(** [get_x p] is the current the x coordinate of player [p]*)

val get_y : t -> int
(** [get_y p] is the current the y coordinate of player [p]*)

val update : t -> key_pressed -> Map.t -> t
(** [update p] updates the player's state according to which keys are pressed *)

val sprite : t -> int -> int
(** [sprite p] is the index in [sprite_set] to render of player [p]*)