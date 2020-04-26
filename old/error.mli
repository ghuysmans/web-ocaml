(*
 * Error monad (like checked exceptions in Java, but better):
 * no try construct is used, errors are just converted to "normal" results.
 * This fits well the Web server case: almost everything ends up in a page!
 *)


(* unit (success) *)
val return : 'a -> [> `Ok of 'a ] Lwt.t

(* unit (failure) *)
val fail : 'a -> [> `Error of 'a ] Lwt.t

(* bind (like Lwt's >>=) *)
val ( >>> ) :
  [< `Error of 'a | `Ok of 'b ] Lwt.t ->
  ('b -> ([> `Error of 'a ] as 'c) Lwt.t) ->
  'c Lwt.t

(* bind that never fails (like Lwt's >|=) *)
val ( >|> ) :
  [< `Error of 'a | `Ok of 'b ] Lwt.t ->
  ('b -> 'c) ->
  [> `Error of 'a | `Ok of 'c ] Lwt.t

(* error transformation *)
val ( >>! ) :
  [< `Error of 'a | `Ok of 'b ] Lwt.t ->
  ('a -> ([> `Ok of 'b ] as 'c) Lwt.t) ->
  'c Lwt.t

(* Lwt's run *)
val get : [< `Ok of 'a ] Lwt.t -> 'a Lwt.t

(* sequential map *)
val map_s:
  ('a -> [< `Error of 'b | `Ok of 'c ] Lwt.t) ->
  'a list ->
  [> `Error of 'b | `Ok of 'c list ] Lwt.t
