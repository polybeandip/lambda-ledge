let solid t =
  match t with
  | 1 | 2 | 3 | 4 | 5 | 6 -> true
  | _ -> false

let recharge t = t = 11

let spike t = 
  match t with
  | 7 | 8 | 9 | 10 -> true
  | _ -> false
