open Graphics
open Bullet.Entity

(*flushes key presses from queue*)
let flush_kp () = while key_pressed () do
  let _ = read_key ()
  in ()
done

let rec move_sprite (curr: t) = 
  clear_graph ();
  fill_circle curr.x curr.y 10; (*change once we have a draw sprite method*)
  let new_state = if not (key_pressed ()) then stop curr 
  else 
    let key = read_key() in 
    flush_kp ();
    if key = '0' then exit 0 else accelerate curr key
  in new_state |> move |> move_sprite

let main xdim ydim = 
  open_graph ""; 
  set_window_title "blob move";
  set_color red;
  resize_window xdim ydim;
  init (xdim/2) (ydim/2) 5 |> move_sprite

let () = main 600 450 ; 