(* Error monad, see error.mli *)

let return x =
  Lwt.return (`Ok x)

let fail e =
  Lwt.return (`Error e)

open Lwt.Infix

let (>>>) x f =
  x >>= function
  | `Ok x -> f x
  | `Error e -> fail e

let (>|>) x f =
  x >>> fun x -> return (f x)

let (>>!) x f =
  x >>= function
  | `Ok _ as x -> Lwt.return x
  | `Error e -> f e

let get x =
  x >>= function
  | `Ok x -> Lwt.return x

let map_s f =
  let rec aux l = function
    | [] -> return (List.rev l)
    | x :: t -> f x >>> fun x -> aux (x :: l) t in
  aux []


(* tests *)

let _transformed =
  return 42
  >|> (fun x -> 2*x)
  >|> string_of_int

let _different_errors =
  return 42
  >>> (fun _ -> fail (`I 12))
  >>> (fun _ -> fail (`S "nok"))
