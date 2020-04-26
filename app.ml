open Cont
open Tyxml.Html

type t = {
  name: string;
  age: int;
  animals: string list;
}

let default = {
  name = "new";
  age = 42;
  animals = [];
}

let data = ref [
  {name = "Alice"; age = 18; animals = ["fish"]};
  {name = "Bob"; age = 19; animals = ["cat"; "donkey"]};
]

let listbox _ = div [txt "TODO"]

let rec get title ?err {name; age; _} =
  ask (fun target -> Result.Ok (Template.template title [
    (match err with
    | None -> txt ""
    | Some e -> p ~a:[a_class ["error"]] [txt e]);
    form ~a:[a_method `Post] [
      input ~a:[a_input_type `Hidden; a_name "k"; a_value target] ();
      input ~a:[a_name "name"; a_value name] ();
      input ~a:[a_name "age"; a_input_type `Number; a_value (string_of_int age)] ();
      listbox "animals";
      button [txt "Send"];
    ]
  ])) >>= fun uri ->
  let p = Uri.get_query_param uri in
  match p "name", p "age", p "animals" with
  | Some name, Some age, _animals ->
    return {name; age = int_of_string age; animals = []}
  | None, _, _ -> get ~err:"missing name" title default
  | _, None, _ -> get ~err:"missing age" title default
  (*
  | _, _, None -> get ~err:"missing animals" title
  *)

let edit_r () = Routes.(int /? nil)

let edit id () =
  if id >= 0 && id < List.length !data then
    get "edit" (List.nth !data id) >>= fun nw ->
    data := List.mapi (fun id' old -> if id = id' then nw else old) !data;
    return (Result.Ok (Template.template "done" [
      txt "updated record #";
      txt (string_of_int id);
    ]))
  else
    return (Result.Error (`Not_found, "no such person"))

let link route id l =
  let attr =
    match id with
    | None -> []
    | Some id -> [a_href (Routes.sprintf (route ()) id)]
  in
  a ~a:attr l

(* TODO understand why not
let link route id l =
  match id with
  | None -> span l (* FIXME? *)
  | Some id -> a ~a:[a_href (Routes.sprintf (route ()) id)] l
*)

let pp ?id {name; age; animals} =
  div ~a:[a_class ["person"]] [
    link edit_r id [b [txt name]];
    txt ", ";
    txt (string_of_int age);
    txt " years old";
    ul (List.map (fun x -> li [txt x]) animals);
  ]

let index () =
  await (Lwt_unix.sleep 1.0) >>= fun () ->
  return @@ Result.Ok (Template.template "result" @@
    List.mapi (fun id -> pp ~id) !data
  )

let add () =
  get "new" default >>= fun t ->
  data := t :: !data;
  return @@ Result.Ok (Template.template "new" [pp t])

let list () =
  let id = Random.bits () in
  let x = [txt ("comes from " ^ string_of_int id)] in
  stub (fun () -> return @@ Result.Ok (Template.template "k" x)) >>= fun k ->
  stub (fun () -> return @@ Result.Ok (Template.template "l" x)) >>= fun l ->
  return @@ Result.Ok (Template.template "list" [
    p [txt (string_of_int id)];
    ul [
      li [a ~a:[a_href k] [txt "go to k"]];
      li [a ~a:[a_href l] [txt "go to l"]];
    ];
  ])


let router : My_server.router = Routes.(one_of [
  nil @--> index;
  s "add" /? nil @--> add;
  edit_r () @--> edit;
  s "list" /? nil @--> list;
])
