open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

let buffer_size = 2048

let respond status headers page =
  let buf = Buffer.create buffer_size in
  let fmt = Format.formatter_of_buffer buf in
  Tyxml.Html.pp () fmt page;
  let body = Buffer.contents buf in
  Server.respond_string ~headers ~status ~body ()

let create mode route =
  let callback _ req body =
    let uri = req |> Request.uri in
    let meth = req |> Request.meth in
    let headers = req |> Request.headers in
    body |> Cohttp_lwt.Body.to_string >>= fun body ->
    let post =
      if Code.compare_method meth `POST = 0 then
        Some (Uri.of_string ("//x/?" ^ body))
      else
        None in
    route uri headers post in
  Server.create ~mode (Server.make ~callback ())

let respond_redirect uri headers path =
  let uri = Uri.with_path uri path in
  Server.respond_redirect ~headers ~uri ()

type doc = Html_types.html Tyxml_html.elt
type router = (unit -> Cont.res Cont.t) Routes.router

let cont = Hashtbl.create 10
let stubs = Hashtbl.create 10

let make router uri _headers post =
  let t =
    match post with
    | None ->
      if Uri.path uri = "/stub" then (
        match Uri.get_query_param uri "k" with
        | None ->
          Cont.return (Result.Error (`Bad_request, "missing k"))
        | Some k ->
          let k = int_of_string k in
          match Hashtbl.find_opt stubs k with
          | None ->
            Cont.return (Result.Error (`Gone, "bad k"))
          | Some f ->
            Hashtbl.remove stubs k;
            f ()
      )
      else
        (match Routes.match' router ~target:(Uri.path uri) with
        | None -> Cont.return (Result.Error (`Not_found, "not found"))
        | Some r -> r ())
    | Some post ->
      match Uri.get_query_param post "k" with
      | None ->
        Cont.return (Result.Error (`Bad_request, "missing k"))
      | Some k ->
        let k = int_of_string k in
        match Hashtbl.find_opt cont k with
        | None ->
          Cont.return (Result.Error (`Gone, "bad k"))
        | Some f ->
          Hashtbl.remove cont k;
          f post
  in
  let respond e =
    let h = Cohttp.Header.init_with "Content-Type" "text/html" in
    let code, doc =
      match e with
      | Ok doc -> `OK, doc
      | Error (code, message) ->
        let doc = Template.template "Error" [Tyxml.Html.txt message] in
        code, doc
    in
    respond code h doc
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
    | Stub (main, other) ->
      let id = Random.bits () in
      Hashtbl.replace stubs id other;
      let url = "/stub?k=" ^ string_of_int id in
      step (main url)
  in
  step t
