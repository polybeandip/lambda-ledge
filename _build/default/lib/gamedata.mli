val tile_size : int
(** [tile_size] is the length in pixels of all square tiles drawn on screen *)

val tile_screen_col : int
(** [tile_size_col] is the number of tiles that can be displayed horizontally on
    screen *)

val tile_screen_row : int
(** [tile_size_row] is the number of tiles that can be displayed vertically on
    screen *)

val screen_w : int
(** [screen_w] is the width in pixels of the screen *)

val screen_h : int
(** [screen_h] is the height in pixels of the screen *)

val player_sprites : Tsdl.Sdl.texture option array ref
(** [player_sprites] is a reference to an array of [Tsdl.Sdl.texture] options of
    player sprites.

    We do this because rendering a [Tsdl.Sdl.texture] from a bmp file is an
    expensive operation. Instead of constantly rerendering textures several
    millions of times across our game, we render all player sprites once and
    store them in [player_sprites]. Then, whenever we need to draw a given
    player sprite, we simply retrive it's assoicated texture from
    [player_sprites] .

    Why an array of texture options instead of an array of textures you ask?
    There isn't a good answer, and quite frankly, this is mostly a hack having
    to do with SDL architecture. If we used an array of textures, then, in order
    to initalize [player_sprites], we'd need to already have an array to
    textures pass into the [ref] function. However, values of type texture can
    only be made using values of type Tsdl.Sdl.window, and the first time we
    build a window is inside main.ml, which is outside the scope of module
    Gamedata. Using texture options gives a quick and dirty way to get the
    initalization done: simply set [player_sprites] to an array with [None]. *)

val tiles : Tsdl.Sdl.texture option array ref
(** [tiles] is a reference to an array of [Tsdl.Sdl.texture] options of tile
    sprites. It serves an identical purpose to [player_sprites] but for tiles,
    and it's an array of texture options for the same reason [player_sprites] *)

val background : Tsdl.Sdl.texture option ref
(** [background] is a reference to the texture of the background image for our
    game. As before, it's a texture option for initalization reasons *)

val load_sprites : Tsdl.Sdl.renderer -> unit
(** [load_sprites] renders all sprites and assigns each to either
    [player_sprites], [tiles], or [background]*)

val make_texture : Tsdl.Sdl.renderer -> string -> Tsdl.Sdl.texture
(** [make_texture render path] is a Tsdl.Sdl.texture for the image at path
    [path], built from renderer [render] *)
