open Gamedata

type t = {
  coords : int Array.t Array.t;
  spawn : int * int;
  exit : int * int;
}

exception BadMap of string

let format_err = "The map text file is not formatted correctly"
let firstline_err = "first line of map text should be four ints"

let parse_map ic =
  let matrix = Array.make_matrix tile_screen_col tile_screen_row 0 in
  let set i j v = Array.set matrix.(j) i v in
  let rec read_line ch ln =
    let line = try input_line ch with End_of_file -> "EOF" in
    if line = "EOF" then ()
    else
      let split = List.map int_of_string (String.split_on_char ' ' line) in
      let index = ref ~-1 in
      List.iter
        (fun x ->
          index := !index + 1;
          set ln !index x)
        split;
      read_line ch (ln + 1)
  in
  read_line ic 0;
  close_in ic;
  matrix

let make_map data =
  try
    let ic = open_in data in
    let line = input_line ic in
    match List.map int_of_string (String.split_on_char ' ' line) with
    | [ spawn_x; spawn_y; end_x; end_y ] ->
        {
          coords = parse_map ic;
          spawn = (spawn_x, spawn_y);
          exit = (end_x, end_y);
        }
    | _ -> raise (BadMap firstline_err)
  with
  | BadMap _ -> raise (BadMap firstline_err)
  | _ -> raise (BadMap format_err)

let get_tile m i j = m.coords.(i).(j)
let get_spawn m = m.spawn
let get_exit m = m.exit
let in_solid m x y = get_tile m x y |> Tile.solid
