open Lwt.Infix

let cont = Hashtbl.create 10

let router _uri _headers post =
  let t =
    match post with
    | None -> App.app ()
    | Some post ->
      match Uri.get_query_param post "k" with
      | None ->
        Cont.return (Template.template "error" [Tyxml.Html.txt "missing k"])
      | Some k ->
        let k = int_of_string k in
        match Hashtbl.find_opt cont k with
        | None ->
          Cont.return (Template.template "error" [Tyxml.Html.txt "bad k"])
        | Some f ->
          Hashtbl.remove cont k;
          f post
  in
  let respond doc =
    let h = Cohttp.Header.init_with "Content-Type" "text/html" in
    My_server.respond `OK h doc
  in
  let rec step = function
    | Cont.Return doc -> respond doc
    | Ask (render, f) ->
      let id = Random.bits () in
      Hashtbl.replace cont id f;
      let url = string_of_int id in
      respond (render url)
    | Await (lwt, f) ->
      lwt >>= fun x -> step (f x)
  in
  step t


let () = Lwt_main.run (
  My_server.create (`TCP (`Port 8000)) router
)
