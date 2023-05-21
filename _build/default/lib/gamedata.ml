(* Screen Settings *)
let original_tile_size = 16 (* 16x16 tile *)
let scale = 3
let tile_size = scale * original_tile_size (* 48x48 tile *)
let tile_screen_col = 20
let tile_screen_row = 15
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

let player_sprite_names =
  List.map
    (fun x -> "player/" ^ x ^ ".bmp")
    [
      "right-up";
      "left-up";
      "right-down";
      "left-down";
      "walk-left";
      "left";
      "walk-right";
      "right";
      "dash-right";
      "dash-left";
    ]

let tiles = ref (Array.make 0 None)
let regex = Str.regexp "tile[0-9]+.bmp"

let tile_cmp t1 t2 =
  let f x = String.length x - 8 |> String.sub x 4 |> int_of_string in
  let cmp1, cmp2 = (f t1, f t2) in
  cmp1 - cmp2

let tile_names =
  List.filter
    (fun x -> Str.string_match regex x 0)
    ("sprites/tile" |> Sys.readdir |> Array.to_list)
  |> List.sort tile_cmp
  |> List.map (fun x -> "tile/" ^ x)

let background = ref None

let load_sprites render =
  let f x = Some (make_texture render x) in
  player_sprites := List.map f player_sprite_names |> Array.of_list;
  tiles := List.map f tile_names |> Array.of_list;
  background := Some (make_texture render "background/background.bmp")
