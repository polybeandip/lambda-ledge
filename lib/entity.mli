(** [t] is the type of the player character *)
type t

(** [key_presssed] represents which keys are being presssed *)
type key_pressed = {
  w : bool;
  a : bool;
  s : bool;
  d : bool;
}

(** [init x y v] spawns a player charater at (x,y) with speed v*)
val init : int -> int -> int -> t

(** [get_x p] is the current the x coordinate of player [p]*)
val get_x : t -> int

(** [get_y p] is the current the y coordinate of player [p]*)
val get_y : t -> int

(** [move p] moves the player according to which keys are being pressed *)
val move : t -> key_pressed -> t

(** [sprite_set] is a list of paths to player sprites *)
val sprite_set : string list

(** [sprite p] is the index in [sprite_set] to render of player [p]*)
val sprite : t -> int -> int
