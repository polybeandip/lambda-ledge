open Tsdl
open Bullet
open Gamedata

let name = "game name" (* fix this later*)

type game_state = {
  map : Map.t;
  player : Player.t;
  flip : int;
  rain1 : int;
  rain2 : int;
  rain3 : int;
  rain4 : int;
  rain5 : int;
  rain6 : int;
  rain7 : int;
  rain8 : int;
}
(** [game_state] represents the current state of the game*)

(*****************************************************************)
(* UPDATE: functions for updating game_state *)
(*****************************************************************)
let update game_state =
  let keys = Sdl.get_keyboard_state () in
  let pressed : Player.key_pressed =
    {
      l = keys.{Sdl.Scancode.left};
      r = keys.{Sdl.Scancode.right};
      u = keys.{Sdl.Scancode.up};
      d = keys.{Sdl.Scancode.down};
      c = keys.{Sdl.Scancode.c};
    }
  in
  {
    game_state with
    player = Player.update game_state.player pressed game_state.map;
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
  let bg =
    match !background with
    | Some x -> x
    | None -> raise (Failure "fuck")
  in
  let m = game_state.map in
  let tiles = !tiles in
  let draw_tile n tile_x tile_y =
    draw_texture render (Util.unpack_get tiles n) (tile_size * tile_x)
      (tile_size * tile_y) tile_size tile_size
  in
  draw_texture render bg 0 0 screen_w  screen_h;
  for i = 0 to tile_screen_col - 1 do
    for j = 0 to tile_screen_row - 1 do
      let n = Map.get_tile m i j in
      draw_tile n i j
    done
  done

let draw_player render game_state =
  let p = game_state.player in
  let x,y = Player.get_x p, Player.get_y p in
  draw_texture render
    (Util.unpack_get !player_sprites (Player.sprite p 0))
    x y tile_size tile_size



let draw_rain1 render x y w h =
  match Sdl.set_render_draw_color render 65 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () ->
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ()

let draw_rain2 render x y w h =
  match Sdl.set_render_draw_color render 55 65 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () ->
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ()

let draw_rain3 render x y w h =
  match Sdl.set_render_draw_color render 45 55 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () ->
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ()

let draw_rain4 render x y w h =
  match Sdl.set_render_draw_color render 66 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () ->
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ()
let draw_rain5 render x y w h =
  match Sdl.set_render_draw_color render 66 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () ->
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ()
let draw_rain6 render x y w h =
  match Sdl.set_render_draw_color render 66 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () ->
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ()
let draw_rain7 render x y w h =
  match Sdl.set_render_draw_color render 66 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () ->
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ()
let draw_rain8 render x y w h =
  match Sdl.set_render_draw_color render 66 75 255 108 with
  | Error (`Msg e) ->
      Sdl.log "Set render draw color error: %s" e;
      exit 1
  | Ok () ->
      let rect = Some (Sdl.Rect.create ~x ~y ~w ~h) in
      match Sdl.render_fill_rect render rect with
      | Error (`Msg e) ->
          Sdl.log "Render fill rect error: %s" e;
          exit 1
      | Ok () -> ()

let repaint render game_state =
  match Sdl.set_render_draw_blend_mode render Sdl.Blend.mode_blend with
  | Error (`Msg e) ->
      Sdl.log "Set render draw blend mode error: %s" e;
      exit 1
  | Ok () ->
      match Sdl.render_clear render with
      | Error (`Msg e) ->
          Sdl.log "Render clear error: %s" e;
          exit 1
      | Ok () ->
          draw_map render game_state;  (* Draw the map first *)
          draw_player render game_state;  (* Draw the player on top of the map *)
          if game_state.flip = 1 then
            draw_rain1 render 280 game_state.rain1 6 50 
            else
            draw_rain1 render 280 game_state.rain1 6 50;  
          if game_state.flip = 1 then
            draw_rain2 render 150 game_state.rain2 6 40
            else
            draw_rain2 render 150 game_state.rain2 6 40;
          if game_state.flip = 1 then
            draw_rain3 render 410 game_state.rain3 6 55
            else
            draw_rain3 render 410 game_state.rain3 6 55;
          if game_state.flip = 1 then
            draw_rain4 render 540 game_state.rain4 6 55  
            else
            draw_rain4 render 540 game_state.rain4 6 55;  
          if game_state.flip = 1 then
            draw_rain5 render 30 game_state.rain5 6 55  
            else
            draw_rain5 render 30 game_state.rain5 6 55;   
          if game_state.flip = 1 then
            draw_rain6 render 670 game_state.rain6 6 55  
            else
            draw_rain6 render 670 game_state.rain6 6 55;  
          if game_state.flip = 1 then
            draw_rain7 render 800 game_state.rain7 6 55
            else
            draw_rain7 render 800 game_state.rain7 6 55;  
          if game_state.flip = 1 then
            draw_rain8 render 925 game_state.rain8 6 55  
            else
            draw_rain8 render 925 game_state.rain8 6 55;  

          Sdl.render_present render;
          Sdl.pump_events ()


(* let repaint render game_state =
  match Sdl.render_clear render with
  | Error (`Msg e) ->
      Sdl.log "Render clear error: %s" e;
      exit 1
  | Ok () ->
      draw_map render game_state;
      draw_player render game_state;
      Sdl.render_present render;
      Sdl.pump_events () *)

(*****************************************************************)
(* Init + Game Loop: the entry point into the game and game loop *)
(*****************************************************************)




let draw_interval = 0.016666

(* let rec loop render gs =
  let next_draw = Unix.gettimeofday () +. draw_interval in
  let gs_updated = update gs in
  repaint render gs_updated;
  if next_draw > Unix.gettimeofday () then
    Unix.sleepf (next_draw -. Unix.gettimeofday ())
  else ();
  if (Sdl.get_keyboard_state ()).{Sdl.Scancode.q} = 1 then ()
  else loop render gs_updated *)

let rec loop render gs =
  let next_draw = Unix.gettimeofday () +. draw_interval in
  let gs_updated = update gs in
  repaint render gs_updated;
  if next_draw > Unix.gettimeofday () then
    Unix.sleepf (next_draw -. Unix.gettimeofday ())
  else ();
  if (Sdl.get_keyboard_state ()).{Sdl.Scancode.q} = 1 then ()
  else
    let rain1 =
      if gs_updated.rain1 > 750 then 
        (-100)
      else
        gs_updated.rain1
    in
    let rain2 = 
      if gs_updated.rain2 > 700 then 
        (-50)
      else
        gs_updated.rain2
    in
    let rain3 = 
      if gs_updated.rain3 > 700 then 
        (-250)
      else
        gs_updated.rain3
    in
    let rain4 = 
      if gs_updated.rain4 > 700 then 
        (-200)
      else
        gs_updated.rain4
    in
    let rain5 = 
      if gs_updated.rain5 > 700 then 
        (-320)
      else
        gs_updated.rain5
    in
    let rain6 = 
      if gs_updated.rain6 > 700 then 
        (-275)
      else
        gs_updated.rain6
    in
    let rain7 = 
      if gs_updated.rain7 > 700 then 
        (-175)
      else
        gs_updated.rain7
    in
    let rain8 = 
      if gs_updated.rain8 > 700 then 
        (-75)
      else
        gs_updated.rain8
    in
    let updated_rect = {
      gs_updated with
      rain1 = rain1 + 11;
      rain2 = rain2+ 9;
      rain3 = rain3+ 11;
      rain4 = rain4+ 13;
      rain5 = rain5+ 10;
      rain6 = rain6+ 14;
      rain7 = rain7+ 11;
      rain8 = rain8+ 9;
    } in
    loop render updated_rect


let main () =
  Sdl.init Sdl.Init.(video + events) |> ignore;
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
        rain1 = 0;
        rain2 = -100;
        rain3 = -55;
        rain4 = -20;
        rain5 = -150;
        rain6 = -250;
        rain7 = -66;
        rain8 = -96;
      } 
    in
    loop render gs;
    Sdl.destroy_window w;
    Sdl.quit ();
    exit 0
    
  

let () = main ()