open Tsdl
open Bullet

let name = "game name" (* fix this later*)

type game_state = {
  map : Map.t;
  player : Player.t;
  flip : int;
}
(** [game_state] represents the current state of the game*)

(*****************************************************************)
(* UPDATE: functions for updating game_state *)
(*****************************************************************)
let check_collision p map =
  let x,y = Player.get_x p, Player.get_y p in
  let open Gamedata in
  let x1 = x + 4 in
  let y1 = y in
  let x2 = x + tile_size - 6 in
  let y2 = y + tile_size - 6 in
  let f a b = Map.in_solid map (a / tile_size) (b / tile_size) in
  f x1 y1 || f x1 y2 || f x2 y1 || f x2 y2

let move_sprite gs =
  let p = gs.player in
  let map = gs.map in
  Sdl.pump_events ();
  let keys = Sdl.get_keyboard_state () in
  let pressed : Player.key_pressed =
    {
      a = keys.{Sdl.Scancode.a} = 1;
      d = keys.{Sdl.Scancode.d} = 1;
      space = keys.{Sdl.Scancode.space} = 1;
    }
  in
  let shift = Player.move p pressed in
  let fall = Player.fall p in
  let fall_shift = Player.fall shift in
  if Player.is_falling p then
    if check_collision fall_shift map = false then fall_shift
    else if check_collision fall map = false then fall
    else if check_collision shift map = false then
      Player.set_falling shift false
    else Player.set_falling p false
  else 
    if check_collision fall_shift map = false then Player.set_falling fall_shift true
    else if check_collision shift map = false then shift 
    else p

let update game_state =
  {
    game_state with
    player = move_sprite game_state;
    flip = 1 - game_state.flip;
  }

(*****************************************************************)
(* DRAW: functions for drawing data onto screen *)
(*****************************************************************)
let draw_texture render texture x y w h =
  match Sdl.render_copy ~dst:(Sdl.Rect.create ~w ~h ~x ~y) render texture with
  | Error (`Msg e) ->
      Sdl.log "Draw sprite error: %s" e;
      exit 1
  | Ok () -> ()

let draw_map render game_state =
  let open Gamedata in
  let bg =
    match !background with
    | Some x -> x
    | None -> raise (Failure "fuck")
  in
  let m = game_state.map in
  let tiles = !tiles in
  let draw_tile n tile_x tile_y =
    draw_texture render (Util.weird_get tiles n) (tile_size * tile_x)
      (tile_size * tile_y) tile_size tile_size
  in
  draw_texture render bg 0 0 screen_w screen_h;
  for i = 0 to tile_screen_col - 1 do
    for j = 0 to tile_screen_row - 1 do
      let n = Map.get_tile m i j in
      draw_tile n i j
    done
  done

let draw_player render game_state =
  let open Gamedata in
  let p = game_state.player in
  let x,y = Player.get_x p, Player.get_y p in
  draw_texture render
    (Util.weird_get !player_sprites (Player.sprite p 0))
    x y tile_size tile_size

let repaint render game_state =
  match Sdl.render_clear render with
  | Error (`Msg e) ->
      Sdl.log "Render clear error: %s" e;
      exit 1
  | Ok () ->
      draw_map render game_state;
      draw_player render game_state;
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

let main () = 
  Sdl.init Sdl.Init.(video + events) |> ignore;
  let open Gamedata in
  match Sdl.create_window ~w:screen_w ~h:screen_h name Sdl.Window.opengl with
  | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
  | Ok w ->
  let render = match Sdl.create_renderer w with 
  | Error (`Msg e) -> Sdl.log "Create render error: %s" e; exit 1
  | Ok r -> r
  in 
  match Sdl.render_clear render with
  | Error (`Msg e) -> Sdl.log "Render clear error: %s" e; exit 1
  | Ok _ -> ();
  Sdl.render_present render;
  load_sprites render;
  let m = Map.make_map "maps/map.txt" in
  let gs = match Map.get_spawn m with (a,b) ->
    {
      map = m;
      player = Player.init (a*tile_size) (b*tile_size); 
      flip = 0; 
    } 
  in
  loop render gs;
  Sdl.destroy_window w;
  Sdl.quit ();
  exit 0

let () = main ()