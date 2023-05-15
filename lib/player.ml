type t = {
  x : int;
  y : int;
  v_y : int;
  v_x : int;
  on_ground : bool;
  idle : bool;
  can_dash : bool;
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
  x : int;
  c : int;
}

let string_of_dir = function
  | RU -> "RU"
  | LU -> "LU"
  | LD -> "LD"
  | RD -> "RD"
  | R -> "R"
  | L -> "L"

let string_of_player p =
  match p with
  | { x; y; v_x; v_y; on_ground; idle; can_dash; dir } ->
      "{" ^ "x:" ^ string_of_int x ^ ", " ^ "y:" ^ string_of_int y ^ ", "
      ^ "v_x: " ^ string_of_int v_x ^ ", " ^ "v_y:" ^ string_of_int v_y ^ ", "
      ^ "on_ground:" ^ string_of_bool on_ground ^ ", " ^ "idle:"
      ^ string_of_bool idle ^ ", " ^ "can_dash: " ^ string_of_bool can_dash
      ^ ", " ^ "dir: " ^ string_of_dir dir ^ "}"

let debug = false
let v_x = 6
let gravity = 2
let max_fall = 10
let jump_force = 3
let wall_x = Gamedata.(screen_w - tile_size)
let wall_y = Gamedata.(screen_h - tile_size)
let jump_hold = 3
let local_jump = ref 0
let float = ref false
let dash = 20
let cooldown = 30
let local_cool = ref 0
let dash_hold = 5
let local_dash = ref 0
let get_x (p : t) = p.x
let get_y (p : t) = p.y

let init x y =
  {
    x;
    y;
    dir = R;
    v_x = 0;
    v_y = 0;
    idle = true;
    on_ground = false;
    can_dash = true;
  }

let sprite p num =
  match (p.dir, p.idle, not p.can_dash) with
  | RU, _, false -> 0
  | LU, _, false -> 1
  | RD, _, false -> 2
  | LD, _, false -> 3
  | L, true, false -> 4
  | L, false, false -> 5
  | R, true, false -> 6
  | R, false, false -> 7
  | R, _, true | RU, _, true | RD, _, true -> 8
  | L, _, true | LU, _, true | LD, _, true -> 9

let in_solid map x y =
  Map.in_solid map (x / Gamedata.tile_size) (y / Gamedata.tile_size)

let in_spike map x y =
  match Map.in_spike map (x / Gamedata.tile_size) (y / Gamedata.tile_size) with
  | false, _ -> false
  | true, 7 -> y mod Gamedata.tile_size > 24
  | true, 8 -> x mod Gamedata.tile_size > 24
  | true, 9 -> y mod Gamedata.tile_size < 24
  | true, 10 -> x mod Gamedata.tile_size < 24
  | _ -> failwith "Not possible"

let hitbox x y =
  let x1 = x + 10 in
  let y1 = y in
  let x2 = x + Gamedata.tile_size - 10 in
  let y2 = y + Gamedata.tile_size - 10 in
  (x1, y1, x2, y2)

let check_collision x y map =
  let x1, y1, x2, y2 = hitbox x y in
  let f = in_solid map in
  f x1 y1 || f x1 y2 || f x2 y1 || f x2 y2

let on_ground (player : t) map =
  let x1 = player.x + 10 in
  let x2 = player.x + Gamedata.tile_size - 10 in
  let y = player.y + Gamedata.tile_size + 6 in
  let f = in_solid map in
  if player.y >= wall_y - 6 then player.y >= wall_y else f x1 y || f x2 y

let is_dead (p : t) map =
  let x1, y1, x2, y2 = hitbox p.x p.y in
  let f = in_spike map in
  f x1 y1 || f x1 y2 || f x2 y1 || f x2 y2

let is_finished (p : t) map =
  let x1, y1, x2, y2 = hitbox p.x p.y in
  let f a b = 
    let rec f_aux = function
      | h :: t -> (a / Gamedata.tile_size, b / Gamedata.tile_size) = h
      | [] -> false
    in 
    f_aux (Map.get_exit map)
  in
  f x1 y1 || f x1 y2 || f x2 y1 || f x2 y2

let set_pos p map =
  if p.v_y <= 0 && p.v_y > -10 then float := true else ();
  let pos_x =
    if p.x + p.v_x < 0 then 0
    else if p.x + p.v_x >= wall_x then wall_x
    else p.x + p.v_x
  in
  let pos_y =
    if p.y + p.v_y < 0 then 0
    else if p.y + p.v_y >= wall_y then wall_y
    else p.y + p.v_y
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
  | 0, 0 -> if p.dir = RD then R else if p.dir = LD then L else p.dir
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
        local_jump := jump_hold;
        -jump_force)
      else 0
    in
    v_y
  else if !local_jump > 0 && kp.c = 1 then (
    local_jump := !local_jump - 1;
    p.v_y - jump_force)
  else if !local_jump > 0 && kp.c = 0 then (
    local_jump := 0;
    p.v_y + gravity)
  else
    p.v_y
    +
    if !float then (
      float := false;
      gravity / 2)
    else gravity

let dash p kp =
  let split = 700 * dash / 1000 in
  match (kp.l, kp.r, kp.u, kp.d) with
  | 1, 0, 0, 0 -> (-dash, 0)
  | 0, 1, 0, 0 -> (dash, 0)
  | 0, 0, 1, 0 -> (0, -dash)
  | 0, 0, 0, 1 -> (0, dash)
  | 1, 0, 1, 0 -> (-split, -split)
  | 1, 0, 0, 1 -> (-split, split)
  | 0, 1, 1, 0 -> (split, -split)
  | 0, 1, 0, 1 -> (split, split)
  | _ ->
      if p.dir = L then (-dash, 0)
      else if p.dir = R then (dash, 0)
      else if p.dir = RU || p.dir = LU then (0, -dash)
      else (0, dash)

let update (p : t) (kp : key_pressed) (map : Map.t) : t =
  let p =
    {
      p with
      on_ground = on_ground p map;
      idle = idle kp;
      dir = dir p kp;
      can_dash = p.can_dash || (on_ground p map && !local_cool = 0);
    }
  in
  match is_dead p map with
  | true ->
      let s_x, s_y =
        Map.get_spawn map |> fun (x, y) ->
        (x * Gamedata.tile_size, y * Gamedata.tile_size)
      in
      let p = init s_x s_y in
      set_pos p map
  | false ->
      if debug then print_endline (string_of_player p) else ();
      if !local_cool > 0 then local_cool := !local_cool - 1 else ();
      if !local_dash > 0 then (
        local_dash := !local_dash - 1;
        set_pos p map)
      else if kp.x = 1 && p.can_dash then (
        local_cool := cooldown;
        local_dash := dash_hold;
        let v_x, v_y = dash p kp in
        let p = { p with v_x; v_y; can_dash = false } in
        set_pos p map)
      else
        let p =
          { p with v_x = vel_x kp; v_y = vel_y p kp |> Util.cap max_fall }
        in
        set_pos p map
