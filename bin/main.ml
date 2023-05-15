open Tsdl
open Bullet
open Gamedata

let name = "game name" (* fix this later*)

type rain = {
  one : int;
  two : int;
  three : int;
  four : int;
  five : int;
  six : int;
  seven : int;
  eight : int;
}

type game_state = {
  map : Map.t;
  player : Player.t;
  flip : int;
  rain : rain;
}
(** [game_state] represents the current state of the game*)

(*****************************************************************)
(* UPDATE: functions for updating game_state *)
(*****************************************************************)

let rain_fall = function
  | { one; two; three; four; five; six; seven; eight } ->
      {
        one = (if one > 750 then -100 else one + 11);
        two = (if two > 700 then -50 else two + 9);
        three = (if three > 700 then -250 else three + 11);
        four = (if four > 700 then -200 else four + 13);
        five = (if five > 700 then -320 else five + 10);
        six = (if six > 700 then -275 else six + 14);
        seven = (if seven > 700 then -175 else seven + 11);
        eight = (if eight > 700 then -75 else eight + 9);
      }

let update game_state =
  let keys = Sdl.get_keyboard_state () in
  let pressed : Player.key_pressed =
    {
      l = keys.{Sdl.Scancode.left};
      r = keys.{Sdl.Scancode.right};
      u = keys.{Sdl.Scancode.up};
      d = keys.{Sdl.Scancode.down};
      x = keys.{Sdl.Scancode.x};
      c = keys.{Sdl.Scancode.c};
    }
  in
  let player = Player.update game_state.player pressed game_state.map in
  match Player.is_finished player game_state.map with
  | false ->
      {
        game_state with
        player;
        flip = 1 - game_state.flip;
        rain = rain_fall game_state.rain;
      }
  | true -> (
      let map =
        Map.make_map
          ("maps/map" ^ (Map.get_next game_state.map |> string_of_int) ^ ".txt")
      in
      match Map.get_spawn map with
      | a, b ->
          {
            map;
            player = Player.init (a * tile_size) (b * tile_size);
            flip = 1 - game_state.flip;
            rain = rain_fall game_state.rain;
          })

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
  let bg =
    match !background with
    | Some x -> x
    | None -> raise (Failure "Make background texture error")
  in
  let m = game_state.map in
  let tiles = !tiles in
  let draw_tile n tile_x tile_y =
    draw_texture render (Util.unpack_get tiles n) (tile_size * tile_x)
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
  let p = game_state.player in
  let x, y = (Player.get_x p, Player.get_y p) in
  draw_texture render
    (Util.unpack_get !player_sprites (Player.sprite p 0))
    x y tile_size tile_size

let draw_rain1 render x y w h =
  match Sdl.set_render_draw_color render 65 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () -> (
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ())

let draw_rain2 render x y w h =
  match Sdl.set_render_draw_color render 55 65 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () -> (
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ())

let draw_rain3 render x y w h =
  match Sdl.set_render_draw_color render 45 55 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () -> (
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ())

let draw_rain4 render x y w h =
  match Sdl.set_render_draw_color render 66 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () -> (
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ())

let draw_rain5 render x y w h =
  match Sdl.set_render_draw_color render 66 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () -> (
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ())

let draw_rain6 render x y w h =
  match Sdl.set_render_draw_color render 66 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () -> (
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ())

let draw_rain7 render x y w h =
  match Sdl.set_render_draw_color render 66 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () -> (
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ())

let draw_rain8 render x y w h =
  match Sdl.set_render_draw_color render 66 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () -> (
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ())

let draw_rain render game_state =
  draw_rain1 render 280 game_state.rain.one 6 50;
  draw_rain2 render 150 game_state.rain.two 6 40;
  draw_rain3 render 410 game_state.rain.three 6 55;
  draw_rain4 render 540 game_state.rain.four 6 55;
  draw_rain5 render 30 game_state.rain.four 6 55;
  draw_rain6 render 670 game_state.rain.six 6 55;
  draw_rain7 render 800 game_state.rain.seven 6 55;
  draw_rain8 render 925 game_state.rain.eight 6 55

let repaint render game_state =
  Sdl.set_render_draw_blend_mode render Sdl.Blend.mode_blend |> ignore;
  match Sdl.render_clear render with
  | Error (`Msg e) ->
      Sdl.log "Render clear error: %s" e;
      exit 1
  | Ok () ->
      draw_map render game_state;
      (* Draw the map first *)
      draw_player render game_state;
      (* Draw the player on top of the map *)
      draw_rain render game_state;
      Sdl.render_present render;
      Sdl.pump_events ()

(*****************************************************************)
(* Init + Game Loop: the entry point into the game and game loop *)
(*****************************************************************)
let draw_interval = 0.016666

let rec loop render gs =
  let next_draw = Unix.gettimeofday () +. draw_interval in
  let gs_updated = update gs in
  repaint render gs_updated;
  if next_draw > Unix.gettimeofday () then
    Unix.sleepf (next_draw -. Unix.gettimeofday ())
  else ();
  if (Sdl.get_keyboard_state ()).{Sdl.Scancode.q} = 1 then ()
  else loop render gs_updated

let main () =
  Sdl.init Sdl.Init.(video + events) |> ignore;
  match Sdl.create_window ~w:screen_w ~h:screen_h name Sdl.Window.opengl with
  | Error (`Msg e) ->
      Sdl.log "Create window error: %s" e;
      exit 1
  | Ok w -> (
      let render =
        match Sdl.create_renderer w with
        | Error (`Msg e) ->
            Sdl.log "Create render error: %s" e;
            exit 1
        | Ok r -> r
      in
      match Sdl.render_clear render with
      | Error (`Msg e) ->
          Sdl.log "Render clear error: %s" e;
          exit 1
      | Ok _ ->
          ();
          Sdl.render_present render;
          load_sprites render;
          let m = Map.make_map "maps/map1.txt" in
          let rain =
            {
              one = 0;
              two = -100;
              three = -55;
              four = -20;
              five = -150;
              six = -250;
              seven = -66;
              eight = -96;
            }
          in
          let gs =
            match Map.get_spawn m with
            | a, b ->
                {
                  map = m;
                  player = Player.init (a * tile_size) (b * tile_size);
                  flip = 0;
                  rain;
                }
          in
          loop render gs;
          Sdl.destroy_window w;
          Sdl.quit ();
          exit 0)

let () = main ()
