type t = 
  {
    x: int; 
    y: int;
    vel_x: int;
    vel_y: int;
    acc: int
  }

type key_pressed = {w:bool;a:bool;s:bool;d:bool}

let init x y acc = {x = x; y = y; vel_x = 0; vel_y = 0; acc=acc}
let set_vel vel_x vel_y curr = {curr with vel_x = vel_x; vel_y = vel_y}
let move curr = {curr with y = curr.y + curr.vel_y; x = curr.x + curr.vel_x}
let stop curr = set_vel 0 0 curr
let accelerate curr kp = 
  curr 
  |> (if kp.w then set_vel curr.vel_x (curr.vel_y - curr.acc) else Fun.id)
  |> (if kp.a then set_vel (curr.vel_x - curr.acc) curr.vel_y else Fun.id)
  |> (if kp.s then set_vel curr.vel_x (curr.vel_y + curr.acc) else Fun.id)
  |> (if kp.d then set_vel (curr.vel_x + curr.acc)  curr.vel_y else Fun.id)

(**returns the sprite to render based on the direction the entity is facing*)
let _sprite _ = raise (Failure "not implemented")