type 'a t =
  | Return of 'a
  | Ask of (string -> Tyxml.Html.doc) * (Uri.t -> 'a t)

let rec (>>=) t f =
  match t with
  | Return x -> f x
  | Ask (render, cont) -> Ask (render, fun uri -> cont uri >>= f)

let return x = Return x
let ask render = Ask (render, return)