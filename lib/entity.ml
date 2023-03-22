type t = {x: int; y: int}
type dir = 
 | W of int
 | A of int
 | S of int
 | D of int

let init x y = {x = x; y = y}
let getx t = t.x
let gety t = t.y
let move t = function
  | W dy ->  {t with y = t.y + dy}
  | A dx ->  {t with x = t.x - dx}
  | S dy ->  {t with y = t.y - dy}
  | D dx ->  {t with x = t.x + dx}