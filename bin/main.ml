open Tsdl
open Bullet.Entity

let draw_interval = 0.016666

let move_sprite (curr: t) = 
  Sdl.pump_events ();
  let keys = Sdl.get_keyboard_state () in
  let pressed = 
    {
      w = keys.{Sdl.Scancode.w} = 1; 
      a = keys.{Sdl.Scancode.a} = 1; 
      s = keys.{Sdl.Scancode.s} = 1; 
      d = keys.{Sdl.Scancode.d} = 1
    } 
  in
  if pressed.w = false && pressed.a = false && pressed.s = false && pressed.d = false 
    then stop curr
    else accelerate curr pressed
  |> move 

let rec loop render curr  =
  let next_draw = Unix.gettimeofday () +. draw_interval in
  match Sdl.render_clear render with
    | Error (`Msg e) -> Sdl.log "Render clear error: %s" e; exit 1
    | Ok () -> ();
  match Sdl.set_render_draw_color render 255 0 0 255 with 
    | Error (`Msg e) -> Sdl.log "Color set render error: %s" e; exit 1
    | Ok _ -> ();
  let surface = 
  match Sdl.load_bmp "sprites/hot.bmp" with
    | Error (`Msg e) -> Sdl.log "Load sprite error: %s" e; exit 1
    | Ok x -> x 
  in
  let texture =
  match Sdl.create_texture_from_surface render surface with
    | Error (`Msg e) -> Sdl.log "Create texture error: %s" e; exit 1
    | Ok x -> x
  in
  match Sdl.render_copy ~dst:(Sdl.Rect.create ~w:30 ~h:30 ~x:curr.x ~y:curr.y) render texture  with
    | Error (`Msg e) -> Sdl.log "Draw sprite error: %s" e; exit 1
    | Ok () -> ();
  match Sdl.render_draw_point render curr.x curr.y with
    | Error (`Msg e) -> Sdl.log "Render point error: %s" e; exit 1
    | Ok () -> ();
  match Sdl.set_render_draw_color render 255 255 255 255 with 
    | Error (`Msg e) -> Sdl.log "Color set render error: %s" e; exit 1
    | Ok _ -> ();
  Sdl.render_present render;
  if next_draw > Unix.gettimeofday () then Unix.sleepf(next_draw -. Unix.gettimeofday ()) else ();
  Sdl.pump_events ();
  if (Sdl.get_keyboard_state ()).{Sdl.Scancode.q} = 1 then () 
  else move_sprite curr |> loop render 

let main () = match Sdl.init Sdl.Init.(video + events) with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
| Ok () ->
    match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
        let render = match Sdl.create_renderer w with 
          | Error (`Msg e) -> Sdl.log "Create render error: %s" e; exit 1
          | Ok r -> r
        in 
        match Sdl.set_render_draw_color render 255 255 255 255 with 
          | Error (`Msg e) -> Sdl.log "Color set render error: %s" e; exit 1
          | Ok _ -> ();
        match Sdl.render_clear render with
          | Error (`Msg e) -> Sdl.log "Render clear error: %s" e; exit 1
          | Ok _ -> ();
        Sdl.render_present render;
        loop render (init 320 240 1);
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0

let () = main ()