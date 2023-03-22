open Graphics
open Bullet.Entity

let xdim = 100
let ydim = 100
let v = 5

let rec move_sprite (curr: t) = 
  clear_graph ();
  fill_circle curr.x  curr.y 10;
  let new_state = if not (key_pressed ()) then curr
  else
    match read_key () with
    | '0' -> exit 0
    | 'w' -> move curr W 
    | 'a' -> move curr A 
    | 's' -> move curr S 
    | 'd' -> move curr D 
    | _ -> curr
  in move_sprite new_state

let main () = 
  open_graph ""; 
  set_window_title "blob move";
  set_color red;
  resize_window xdim ydim;
  move_sprite (init 50 50 v)  

let () = main (); 