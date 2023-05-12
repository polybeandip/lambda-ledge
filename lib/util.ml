let unpack_get array i =
  match Array.get array i with
  | Some x -> x
  | None -> raise (Failure "weird_get failed")