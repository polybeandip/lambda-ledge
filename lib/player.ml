type t = {
  x : int;
  y : int;
  v_y : int;
  on_ground : bool;
  idle : bool;
  dir : dir;
}

and dir =
  | RU
  | LU
  | LD
  | RD
  | L
  | R

type key_pressed = {
  l : int;
  r : int;
  u : int;
  d : int;
  c : int;
}

let v_x = 6
let gravity = 2
let max_fall = 10
let jump_force = 3
let dash = 20
let cap_x = Gamedata.(screen_w - tile_size)
let cap_y = Gamedata.(screen_h - tile_size)
let jump_hold_time = 3
let local_hold_time = ref 0
let float = ref false
let get_x curr = curr.x
let get_y curr = curr.y
let init x y = { x; y; dir = R; v_y = 0; idle = true; on_ground = false }

let sprite p num =
  match p.dir, p.idle with
  | RU, _ -> 0
  | LU, _ -> 1
  | RD, _ -> 2
  | LD, _ -> 3
  | L, true-> 4
  | L, false-> 5
  | R, true -> 6
  | R, false -> 7

let in_solid map x y =
  Map.in_solid map (x / Gamedata.tile_size) (y / Gamedata.tile_size)

let check_collision x y map =
  let x1 = x + 10 in
  let y1 = y in
  let x2 = x + Gamedata.tile_size - 10 in
  let y2 = y + Gamedata.tile_size - 10 in
  let f = in_solid map in
  f x1 y1 || f x1 y2 || f x2 y1 || f x2 y2

let on_ground player map =
  let x1 = player.x + 10 in
  let x2 = player.x + Gamedata.tile_size - 10 in
  let y = player.y + Gamedata.tile_size + 6 in
  let f = in_solid map in
  if player.y >= cap_y - 6 then player.y >= cap_y else f x1 y || f x2 y

let set_pos p delta_x delta_y map =
  if delta_y <= 0 && delta_y > -10 then float := true else ();
  let pos_x =
    if p.x + delta_x < 0 then 0
    else if p.x + delta_x >= cap_x then cap_x
    else p.x + delta_x
  in
  let pos_y =
    if p.y + delta_y < 0 then 0
    else if p.y + delta_y >= cap_y then cap_y
    else p.y + delta_y
  in
  if check_collision pos_x pos_y map = false then
    { p with x = pos_x; y = pos_y }
  else if check_collision pos_x p.y map = false then
    { p with x = pos_x; y = p.y }
  else if check_collision p.x pos_y map = false then
    { p with x = p.x; y = pos_y }
  else p

let dir p kp =
  match (kp.r - kp.l, kp.u - kp.d) with
  | 0, 0 -> 
    if p.dir = RD then R
    else if p.dir = LD then L
    else p.dir
  | 0, 1 ->
      if p.dir = RU || p.dir = LU then p.dir
      else if p.dir = R || p.dir = RD then RU
      else if p.dir = L || p.dir = LD then LU
      else RU
  | 0, -1 -> 
      if p.dir = RD || p.dir = LD then p.dir
      else if p.dir = R || p.dir = RU then RD
      else if p.dir = L || p.dir = LU then LD
      else RD
  | 1, _ -> R
  | -1, _ -> L
  | _ -> failwith "Not possible"

let idle kp = kp.l = 0 && kp.r = 0 && kp.c = 0
let vel_x kp = (kp.r - kp.l) * v_x

let vel_y (p : t) (kp : key_pressed) =
  if p.on_ground then
    let v_y =
      if kp.c = 1 then (
        local_hold_time := jump_hold_time;
        -jump_force)
      else 0
    in
    v_y
  else if !local_hold_time > 0 && kp.c = 1 then (
    local_hold_time := !local_hold_time - 1;
    p.v_y - jump_force)
  else if !local_hold_time > 0 && kp.c = 0 then (
    local_hold_time := 0;
    p.v_y + gravity)
  else
    p.v_y
    +
    if !float then (
      float := false;
      gravity / 2)
    else gravity

let update (p : t) (kp : key_pressed) (map : Map.t) : t =
  let p =
    { p with on_ground = on_ground p map; idle = idle kp; dir = dir p kp }
  in
  let p = { p with v_y = vel_y p kp |> Util.cap max_fall } in
  set_pos p (vel_x kp) p.v_y map
