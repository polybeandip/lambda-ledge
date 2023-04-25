type dir =
  | U
  | D
  | L
  | R
  | UR
  | UL
  | DR
  | DL

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

let init x y v = { x; y; speed = v; dir = R }

let move curr kp =
  let x = ref curr.x in
  let y = ref curr.y in
  match kp with
  | { w; a; s; d } ->
      if w then y := !y - curr.speed else ();
      if a then x := !x - curr.speed else ();
      if d then x := !x + curr.speed else ();
      if s then y := !y + curr.speed else ();
      let dir =
        match (!x - curr.x, !y - curr.y) with
        | 0, 0 -> curr.dir
        | x, 0 -> if x < 0 then L else R
        | 0, y -> if y < 0 then U else D
        | x, y ->
            if x < 0 && y > 0 then UL
            else if x > 0 && y > 0 then UR
            else if x < 0 && y < 0 then DL
            else DR
      in
      { curr with y = !y; x = !x; dir }

(**returns the sprite to render based on the direction the entity is facing*)
let sprite curr num =
  let index = 
  match curr.dir with
  | U -> 0
  | D -> 1
  | L -> 2
  | R -> 3
  | UR -> 4
  | UL -> 5
  | DR -> 6
  | DL -> 7
  in
  if num = 0 then index else index + 8

let sprite_set = List.map (fun x ->  x ^ ".bmp") ["right-up"; "right"; "left";"right"; "right"; "left"; "right-up"; "left-up"]