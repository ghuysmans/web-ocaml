open Lwt.Infix
open Error

type t = {
  name: string;
  first: int;
  second: int option;
}

let form i =
  let o = i |> Helpers.default_to {name=""; first=42; second=None} in
  let open Tyxml.Html in
  let open Form in
  form `Post [
    h2 [pcdata "Form"];
    label "Name: " (make_string "name" o.name |> input);
    label "First number: " (make_int "first" o.first |> input);
    label "Second number: " (make_int_o "second" o.second |> input);
    button [pcdata "Submit"];
  ]

let compute t =
  (* let's fake an asynchronous computation with return *)
  return (
    match t.second with
    | Some n -> (t.first + n) / 2
    | None -> t.first
  )

let template page_title content =
  let open Tyxml.Html in
  let title = title (pcdata page_title) in
  let h1 = h1 [pcdata page_title] in
  html (head title []) (body (h1 :: content))

let view_form t =
  template "Average" [form t]

let view_result n r =
  let open Tyxml.Html in
  template "Average" [
    h2 [pcdata "Result"];
    p [pcdata @@ "Hi " ^ n ^ ". The answer is " ^ string_of_int r]
  ]

let view_error f _ =
  let open Tyxml.Html in
  template "Error" [pcdata f]

let read uri =
  let open Error in
  let open Form in
  read uri "name" |> required |> get >>> fun name ->
  read uri "first" |> to_int |> required |> get >>> fun first ->
  read uri "second" |> to_int |> get >>> fun second ->
  return {name; first; second}

let calc post =
  match post with
  | None ->
    let v = view_form None in
    return (`Page v)
  | Some p ->
    read p >>> fun t ->
    if t.first = 42 then
      return (`Redirect "http://xkcd.com")
    else
      compute t
      >|> view_result t.name >|> fun v ->
      `Page v

let router uri headers post =
  let h = Cohttp.Header.init_with "Content-Type" "text/html" in
  calc post (* TODO routing here *)
  >>! (function
    | `Invalid (s, e) -> return (`Page (view_error s e))
  )
  |> get
  >>= function
    | `Page p -> My_server.respond `OK h p
    | `Redirect r -> My_server.respond_redirect (Uri.of_string r) h ""


let () =
  (
    My_server.create (`TCP (`Port 8000)) router
  ) |> Lwt_main.run
