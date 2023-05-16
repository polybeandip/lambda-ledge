(**[unpack_get array i] is v if the element at index [i] in [array] is Some v *)
let unpack_get array i =
  match Array.get array i with
  | Some x -> x
  | None -> raise (Failure "weird_get failed")

(**[cap max v] is [max] if [v] is >= [max], [-max] if [v] <= [-max], and [v]
   otherwise*)
let cap max v = if v >= max then max else if v <= -max then -max else v
