(* Screen Settings *)
val tile_size : int
val tile_screen_col : int
val tile_screen_row : int
val screen_w : int
val screen_h : int

(* Pre-rendered Sprite Data *)
val player_sprites : Tsdl.Sdl.texture option array ref
val tiles : Tsdl.Sdl.texture option array ref
val background: Tsdl.Sdl.texture option ref
val load_sprites : Tsdl.Sdl.renderer -> unit