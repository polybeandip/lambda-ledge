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
  falling : bool;
  dir : dir;
}

type key_pressed = {
  a : bool;
  d : bool;
  space : bool;
}

let speed = 10
let vert = 48*3
let gravity = 1

let get_x curr = curr.x
let get_y curr = curr.y
let is_falling curr = curr.falling
let set_falling curr b = {curr with falling = b}
let init x y = { x; y; dir = R; falling = false}
let fall curr = {curr with y = curr.y + gravity}

let move curr kp =
  let x = ref curr.x in
  let y = ref curr.y in
  let og = ref curr.falling in
  match kp with
  | { a; d; space} ->
      if space && (not !og) then (y := !y - vert; og := not !og) else ();
      if a then x := !x - speed else ();
      if d then x := !x + speed else ();
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
      { y = !y; x = !x; dir; falling = !og}

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

let sprite_set =
  List.map
    (fun x -> x ^ ".bmp")
    [
      "right-up";
      "right";
      "left";
      "right";
      "right";
      "left";
      "right-up";
      "left-up";
    ]
