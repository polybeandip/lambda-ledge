type t = 
  {
    x: int; 
    y: int;
    vel_x: int;
    vel_y: int;
    acc: int
  }

val init: int -> int -> int -> t
val move: t -> t
val stop: t -> t
val accelerate: t -> char -> t