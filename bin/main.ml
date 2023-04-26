open Tsdl
open Bullet

(** [game_state] represents the current state of the game*)
type game_state = {
  map : Map.t;
  player : Entity.t;
  flip : int;
}

(*****************************************************************)
(* UPDATE: functions for updating game_state *)
(*****************************************************************)
let move_sprite (curr : Entity.t) =
  Sdl.pump_events ();
  let keys = Sdl.get_keyboard_state () in
  let pressed : Entity.key_pressed =
    {
      w = keys.{Sdl.Scancode.w} = 1;
      a = keys.{Sdl.Scancode.a} = 1;
      s = keys.{Sdl.Scancode.s} = 1;
      d = keys.{Sdl.Scancode.d} = 1;
    }
  in
  Entity.move curr pressed

let check_collision shift map =
  let x = Entity.get_x shift in
  let y = Entity.get_y shift in
  let open Gamedata in
  let x1 = x + 4 in
  let y1 = y in
  let x2 = x + tile_size -6 in
  let y2 = y + tile_size - 6 in
  let f a b = Map.check_collision map (a/tile_size) (b/tile_size) in
  (f x1 y1) || (f x1 y2) || (f x2 y1) || (f x2 y2)

let update game_state =
  let shift = game_state.player |> move_sprite in
  if check_collision shift game_state.map = false then
  { game_state with player = shift; flip = 1 - game_state.flip }
  else
  { game_state with flip = 1 - game_state.flip }

(*****************************************************************)
(* DRAW: functions for drawing data onto screen *)
(*****************************************************************)
let draw_texture render texture x y w h =
  match
    Sdl.render_copy
      ~dst:(Sdl.Rect.create ~w:w ~h:h ~x ~y)
      render texture
  with
  | Error (`Msg e) ->
      Sdl.log "Draw sprite error: %s" e;
      exit 1
  | Ok () -> ()

let draw_map render game_state =
  let open Gamedata in
  let bg = match !background with Some x -> x | None -> raise (Failure "fuck") in
  let m = game_state.map in
  let tiles = !tiles in
  let draw_tile n tile_x tile_y =
    draw_texture render
      (Util.weird_get tiles n )
      (tile_size * tile_x) (tile_size * tile_y) tile_size tile_size
  in
  draw_texture render bg 0 0 screen_w screen_h;
  for i = 0 to tile_screen_col -1 do
    for j = 0 to tile_screen_row -1 do
      let n = Map.get_tile m i j in
      draw_tile n i j
    done
  done

let repaint render game_state =
  let open Gamedata in
  let curr = game_state.player in
  let player_sprites = !player_sprites in
  match Sdl.render_clear render with
  | Error (`Msg e) ->
      Sdl.log "Render clear error: %s" e;
      exit 1
  | Ok () ->
      draw_map render game_state;
      let x = Entity.get_x curr in
      let y = Entity.get_y curr in
      draw_texture render 
        (Util.weird_get player_sprites (Entity.sprite curr 0)) x y tile_size tile_size;
      Sdl.render_present render;
      Sdl.pump_events ()

(*****************************************************************)
(* Init + Game Loop: the entry point into the game and game loop *)
(*****************************************************************)
let rec loop render gs =
  let draw_interval = 0.016666 in
  let next_draw = Unix.gettimeofday () +. draw_interval in
  let gs = update gs in
  repaint render gs;
  if next_draw > Unix.gettimeofday () then
    Unix.sleepf (next_draw -. Unix.gettimeofday ())
  else ();
  if (Sdl.get_keyboard_state ()).{Sdl.Scancode.q} = 1 then ()
  else loop render gs

let main () = match Sdl.init Sdl.Init.(video + events) with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
| Ok () ->
    let open Gamedata in
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
    load_sprites render;
    let m = Map.make_map "maps/map.txt" in
    let gs = match Map.get_spawn m with (a,b) ->
      {
        map = m;
        player = Entity.init (a*tile_size) (b*tile_size) 8; 
        flip = 0; 
      } 
    in
    loop render gs;
    Sdl.destroy_window w;
    Sdl.quit ();
    exit 0

let () = main ()