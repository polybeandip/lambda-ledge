type dir = W | A | S | D 
type t = 
  {
    x: int; 
    y: int;
    speed: int;
    look: dir
  }

val init: int -> int -> int -> t
val getx: t -> int
val gety: t -> int
val move: t -> dir -> t
val sprite: t -> unit