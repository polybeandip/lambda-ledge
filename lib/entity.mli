type dir

type t = {
  x : int;
  y : int;
  speed : int;
  dir : dir;
}

type key_pressed = {
  w : bool;
  a : bool;
  s : bool;
  d : bool;
}

val init : int -> int -> int -> t
val move : t -> key_pressed -> t
val sprite : t -> int -> int
val sprite_set : string list
