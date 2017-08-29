open Lwt
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
    body |> Cohttp_lwt_body.to_string >>= fun body ->
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
