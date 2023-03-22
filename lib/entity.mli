type t 
type dir = 
 | W of int
 | A of int
 | S of int
 | D of int

val init: int -> int -> t
val getx: t -> int
val gety: t -> int
val move: t -> dir -> t