open Calc

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

let form t =
  Template.template "Average" [form t]

let result (n, r) =
  let open Tyxml.Html in
  Template.template "Average" [
    h2 [pcdata "Result"];
    p [pcdata @@ "Hi " ^ n ^ ". The answer is " ^ string_of_int r]
  ]
