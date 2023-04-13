open Tsdl
open Bullet.Entity

type game_state = 
  {
    player_pos: Bullet.Entity.t;
    flip: int
  }

(* Screen Settings *)
let original_tile_size = 16 (* 16x16 tile *)
let scale = 3
let tile_size = scale * original_tile_size (* 48x48 tile *)
let tile_screen_col = 16
let tile_screen_row = 12
let screen_w = tile_screen_col * tile_size (* 768 pixels *)
let screen_h = tile_screen_row * tile_size (* 576 pixels *)

(* UPDATE: functions for updating game_state *)
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
  move curr pressed

let update game_state =  let curr = game_state.player_pos in 
  {player_pos = move_sprite curr; flip = 1 - game_state.flip}

(* DRAW: functions for drawing data onto screen *)
let repaint render curr = 
  match Sdl.render_clear render with
    | Error (`Msg e) -> Sdl.log "Render clear error: %s" e; exit 1
    | Ok () -> ();
  let surface = 
  match Sdl.load_bmp (sprite curr 0) with
    | Error (`Msg e) -> Sdl.log "Load sprite error: %s" e; exit 1
    | Ok x -> x 
  in
  let texture =
  match Sdl.create_texture_from_surface render surface with
    | Error (`Msg e) -> Sdl.log "Create texture error: %s" e; exit 1
    | Ok x -> x
  in
  match Sdl.render_copy ~dst:(Sdl.Rect.create ~w:tile_size ~h:tile_size ~x:curr.x ~y:curr.y) render texture with
    | Error (`Msg e) -> Sdl.log "Draw sprite error: %s" e; exit 1
    | Ok () -> ();
  match Sdl.render_draw_point render curr.x curr.y with
    | Error (`Msg e) -> Sdl.log "Render point error: %s" e; exit 1
    | Ok () -> ();
  match Sdl.set_render_draw_color render 255 255 255 255 with 
    | Error (`Msg e) -> Sdl.log "Color set render error: %s" e; exit 1
    | Ok _ -> ();
  Sdl.render_present render;
  Sdl.pump_events ()

(* Game loop *)
let rec loop render gs =
  let draw_interval = 0.016666 in
  let next_draw = Unix.gettimeofday () +. draw_interval in
  let gs = update gs in repaint render (gs.player_pos);
  if next_draw > Unix.gettimeofday () then Unix.sleepf(next_draw -. Unix.gettimeofday ()) else ();
  if (Sdl.get_keyboard_state ()).{Sdl.Scancode.q} = 1 then () else loop render gs

(* Init *)
let main () = match Sdl.init Sdl.Init.(video + events) with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
| Ok () ->
    match Sdl.create_window ~w:screen_w ~h:screen_h "SDL OpenGL" Sdl.Window.opengl with
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
        loop render ({player_pos = init (screen_w/2) (screen_h/2) 10; flip = 0});
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0

let () = main ()