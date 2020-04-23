type 'a t =
  | Return of 'a
  | Ask of (string -> Tyxml.Html.doc) * (Uri.t -> 'a t)
  | Await : 'b Lwt.t * ('b -> 'a t) -> 'a t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val return : 'a -> 'a t
val ask : (string -> Tyxml.Html.doc) -> Uri.t t
val await : 'a Lwt.t -> 'a t
