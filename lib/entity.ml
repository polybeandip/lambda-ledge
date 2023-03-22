type dir = W | A | S | D 

type t = 
  {
    x: int; 
    y: int;
    speed: int;
    look: dir
  }

let init x y speed = {x = x; y = y; speed=speed; look=D}
let getx t = t.x
let gety t = t.y
let move t = function
  | W ->  {t with y = t.y + t.speed; look = W}
  | A ->  {t with x = t.x - t.speed; look = A}
  | S ->  {t with y = t.y - t.speed; look = S}
  | D ->  {t with x = t.x + t.speed; look = D}

(**returns the sprite to render based on the direction the entity is facing*)
let sprite t = raise (Failure "not implemented")