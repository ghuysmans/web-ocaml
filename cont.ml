type res = (Tyxml.Html.doc, Cohttp.Code.status_code * string) result

type 'a t =
  | Return of 'a
  | Ask of (string -> res) * (Uri.t -> 'a t)
  | Await : 'b Lwt.t * ('b -> 'a t) -> 'a t

let rec (>>=) t f =
  match t with
  | Return x -> f x
  | Ask (render, cont) -> Ask (render, fun uri -> cont uri >>= f)
  | Await (lwt, g) -> Await (lwt, fun x -> g x >>= f)

let return x = Return x
let ask render = Ask (render, return)
let await lwt = Await (lwt, return)
