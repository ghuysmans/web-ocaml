open Cont
open Tyxml.Html

let rec get_list acc =
  ask (fun target -> Result.Ok (Template.template "form" [
    form ~a:[a_method `Post] [
      input ~a:[a_input_type `Hidden; a_name "k"; a_value target] ();
      input ~a:[a_autofocus (); a_name "q"] ();
    ];
  ])) >>= fun uri ->
  match Uri.get_query_param uri "q" with
  | None | Some "" -> return (List.rev acc)
  | Some x -> get_list (x :: acc)

let index () =
  (*
  Printf.printf "secret?\n";
  let n = Scanf.scanf "%d\n" (fun x -> x) in
  *)
  let id = Random.bits () in
  Printf.printf "started %d\n%!" id;
  get_list [] >>= fun l ->
  await (Lwt_unix.sleep 1.0) >>= fun () ->
  return @@ Result.Ok (Template.template "result" [
    p [txt (Printf.sprintf "id=%d" id)];
    ul (List.map (fun x -> li [txt x]) l);
  ])

let router : My_server.router = Routes.(one_of [
  nil @--> index;
])
