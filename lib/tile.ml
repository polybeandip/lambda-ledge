let regex = Str.regexp "tile[0-9]+.bmp"

let tile_cmp t1 t2 =
  let f x = String.length x - 8 |> String.sub x 4 |> int_of_string in
  let cmp1, cmp2 = (f t1, f t2) in
  cmp1 - cmp2

let tile_set =
  List.filter
    (fun x -> Str.string_match regex x 0)
    ("sprites" |> Sys.readdir |> Array.to_list)
  |> List.sort tile_cmp

let solid t =
  if t = 2 then true
  else false
