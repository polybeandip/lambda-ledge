type t = 
  {
    x: int; 
    y: int;
    vel_x: int;
    vel_y: int;
    acc: int
  }

let init x y acc = {x = x; y = y; vel_x = 0; vel_y = 0; acc=acc}
let set_vel curr vel_x vel_y = {curr with vel_x = vel_x; vel_y = vel_y}
let move curr = {curr with y = curr.y + curr.vel_y; x = curr.x + curr.vel_x}
let stop curr = set_vel curr 0 0
let accelerate curr c = 
  let set_vel = set_vel curr in
  match c with
  | 'w' -> set_vel curr.vel_x (curr.vel_y + curr.acc)
  | 'a' -> set_vel (curr.vel_x - curr.acc) curr.vel_y
  | 's' -> set_vel curr.vel_x (curr.vel_y - curr.acc)
  | 'd' -> set_vel (curr.vel_x + curr.acc)  curr.vel_y
  | _ -> stop curr

(**returns the sprite to render based on the direction the entity is facing*)
let _sprite _ = raise (Failure "not implemented")