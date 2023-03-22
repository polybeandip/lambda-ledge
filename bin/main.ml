open Graphics
open Bullet.Entity

let xdim = 100
let ydim = 100
let v = 5

let rec draw_sprite (pl: t) = 
  clear_graph ();
  fill_circle (getx pl) (gety pl) 10;
  let shift = move pl in
  match read_key () with
  | '0' -> ()
  | 'w' -> shift (W v) |> draw_sprite
  | 'a' -> shift (A v) |> draw_sprite
  | 's' -> shift (S v) |> draw_sprite
  | 'd' -> shift (D v) |> draw_sprite
  | _ -> draw_sprite pl



let main () = 
  open_graph ""; 
  set_window_title "bullet physics";
  set_color red;
  match read_key with 
  _ -> ();
  resize_window xdim ydim;
  draw_sprite (init 50 50)

let () = main ()