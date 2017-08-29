open Error

type t = {
  name: string;
  first: int;
  second: int option;
}

let compute t =
  (* let's fake an asynchronous computation with return *)
  return (
    match t.second with
    | Some n -> t.name, (t.first + n) / 2
    | None -> t.name, t.first
  )

let read uri =
  let open Error in
  let open Form in
  read uri "name" |> required |> get >>> fun name ->
  read uri "first" |> to_int |> required |> get >>> fun first ->
  read uri "second" |> to_int |> get >>> fun second ->
  return {name; first; second}
