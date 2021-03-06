type res = (Tyxml.Html.doc, Cohttp.Code.status_code * string) result

type 'a t =
  | Return of 'a
  | Ask of (string -> res) * (Uri.t -> 'a t)
  | Await : 'b Lwt.t * ('b -> 'a t) -> 'a t
  | Stub : (string -> 'a t) * (unit -> res t) -> 'a t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val return : 'a -> 'a t
val ask : (string -> res) -> Uri.t t
val await : 'a Lwt.t -> 'a t
val stub : (unit -> res t) -> string t

val map : ('a -> 'b t) -> 'a list -> 'b list t
val fold : ('a -> 'b -> 'a t) -> 'b list -> 'a t -> 'a t
