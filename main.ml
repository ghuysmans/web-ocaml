open Lwt.Infix
open Error

let view_invalid f _ =
  let open Tyxml.Html in
  (* FIXME use the second argument! *)
  Template.template "Invalid field" [pcdata f]

let calc post =
  match post with
  | None ->
    let v = Calc_view.form None in
    return (`Page v)
  | Some p ->
    Calc.read p >>> fun t ->
    if t.Calc.first = 42 then
      return (`Redirect "http://xkcd.com")
    else
      Calc.compute t
      >|> Calc_view.result >|> fun v ->
      `Page v

let router uri headers post =
  let h = Cohttp.Header.init_with "Content-Type" "text/html" in
  calc post (* TODO routing here *)
  >>! (function
    | `Invalid (s, e) -> return (`Page (view_invalid s e))
  )
  |> get
  >>= function
    | `Page p -> My_server.respond `OK h p
    | `Redirect r -> My_server.respond_redirect (Uri.of_string r) h ""


let () =
  (
    My_server.create (`TCP (`Port 8000)) router
  ) |> Lwt_main.run
