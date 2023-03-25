type t = 
  {
    x: int; 
    y: int;
    vel_x: int;
    vel_y: int;
    acc: int
  }

type key_pressed = {w:bool;a:bool;s:bool;d:bool}

val init: int -> int -> int -> t
val move: t -> t
val stop: t -> t
val accelerate: t -> key_pressed -> t