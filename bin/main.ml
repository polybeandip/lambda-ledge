open Tsdl
open Bullet
open Bullet.Entity

type game_state = {
  player : Bullet.Entity.t;
  player_sprites : Sdl.texture Array.t;
  flip : int;
  tiles : Sdl.texture Array.t;
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
let move_sprite (curr : t) =
  Sdl.pump_events ();
  let keys = Sdl.get_keyboard_state () in
  let pressed =
    {
      w = keys.{Sdl.Scancode.w} = 1;
      a = keys.{Sdl.Scancode.a} = 1;
      s = keys.{Sdl.Scancode.s} = 1;
      d = keys.{Sdl.Scancode.d} = 1;
    }
  in
  move curr pressed

let update game_state =
  let curr = game_state.player in
  { game_state with player = move_sprite curr; flip = 1 - game_state.flip }

(* DRAW: functions for drawing data onto screen *)
let draw_texture render texture x y =
  match
    Sdl.render_copy
      ~dst:(Sdl.Rect.create ~w:tile_size ~h:tile_size ~x ~y)
      render texture
  with
  | Error (`Msg e) ->
      Sdl.log "Draw sprite error: %s" e;
      exit 1
  | Ok () -> ()

let draw_background render data game_state =
  let ic = open_in data in
  let draw_tile n tile_x tile_y =
    draw_texture render
      (Array.get game_state.tiles (n - 1))
      (tile_size * tile_x) (tile_size * tile_y)
  in
  let rec read_line ch ln =
    let split = try input_line ch with End_of_file -> "EOF" in
    if split = "EOF" then ()
    else
      let lst = List.map int_of_string (String.split_on_char ' ' split) in
      let index = ref ~-1 in
      List.iter
        (fun x ->
          index := !index + 1;
          draw_tile x !index ln)
        lst;
      read_line ch (ln + 1)
  in
  read_line ic 0;
  close_in ic

let repaint render game_state =
  let curr = game_state.player in
  let player_sprites = game_state.player_sprites in
  match Sdl.render_clear render with
  | Error (`Msg e) ->
      Sdl.log "Render clear error: %s" e;
      exit 1
  | Ok () ->
      ();
      draw_background render "maps/map.txt" game_state;
      draw_texture render
        (Array.get player_sprites (sprite curr 0))
        curr.x curr.y;
      Sdl.render_present render;
      Sdl.pump_events ()

(* Game loop *)
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

(* Init *)
let make_texture render path =
  let path = "sprites/" ^ path in
  let surface =
    match Sdl.load_bmp path with
    | Error (`Msg e) ->
        Sdl.log "Load sprite error: %s" e;
        exit 1
    | Ok x -> x
  in
  match Sdl.create_texture_from_surface render surface with
  | Error (`Msg e) ->
      Sdl.log "Create texture error: %s" e;
      exit 1
  | Ok x -> x

let load_sprites render sprites =
  List.map (make_texture render) sprites |> Array.of_list

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
    let gs = 
      {
        player = init (screen_w/2) (screen_h/2) 10; 
        flip = 0; 
        player_sprites = load_sprites render Entity.sprite_set;
        tiles = load_sprites render Tile.tile_set
      } 
    in
    loop render gs;
    Sdl.destroy_window w;
    Sdl.quit ();
    exit 0

let () = main ()