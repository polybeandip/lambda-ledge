type dir = U | D | L | R

type t = 
  {
    x: int; 
    y: int;
    speed: int;
    dir: dir
  }

type key_pressed = {w:bool;a:bool;s:bool;d:bool}

let init x y v = {x = x; y = y; speed = v; dir = R}
let move curr kp= 
  let x = ref curr.x in
  let y = ref curr.y in
  match kp with {w; a; s; d} ->
    if w then y := !y - curr.speed else ();
    if a then x := !x - curr.speed else ();
    if d then x := !x + curr.speed else ();
    if s then y := !y + curr.speed else ();
    let dir = match !x - curr.x, !y - curr.y with
      | 0,0 -> curr.dir
      | x, 0 -> if x < 0 then L else R
      | _, y -> if y < 0 then U else D
    in {curr with y = !y; x = !x; dir = dir}

(**returns the sprite to render based on the direction the entity is facing*)
let sprite curr num = 
  let prefix = match curr.dir with
  | U -> "sprites/up" 
  | D -> "sprites/down" 
  | L -> "sprites/left" 
  | R -> "sprites/right" 
  in prefix ^ string_of_int num ^ ".bmp"