(* Screen Settings *)
let original_tile_size = 16 (* 16x16 tile *)
let scale = 3
let tile_size = scale * original_tile_size (* 48x48 tile *)
let tile_screen_col = 16
let tile_screen_row = 12
let screen_w = tile_screen_col * tile_size (* 768 pixels *)
let screen_h = tile_screen_row * tile_size (* 576 pixels *)

(* Pre-rendered Sprite Data *)
let make_texture render path =
  let open Tsdl in
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

let player_sprites = ref (Array.make 0 None)

let tiles = ref (Array.make 0 None)

let background = ref None

let load_sprites render =
  let f x = Some (make_texture render x) in
  player_sprites := List.map f Player.sprite_set |> Array.of_list;
  tiles := List.map f Tile.tile_set |> Array.of_list;
  background := Some (make_texture render "background.bmp")