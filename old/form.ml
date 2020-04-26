type i = {
  name: string;
}

type 'a typ =
  | Option: 'a typ option * 'a typ (* used by required *) -> 'a option typ
  | String: string -> string typ
  | Int: int * int option * int option -> int typ
  | Float: float -> float typ
  | Select: (string * string) list * string -> string typ
  | Bool: bool -> bool typ
  | Text: string -> string typ

type ('v, 'e) t =
  | Valid of 'v
  | Invalid of 'v * 'e


(* dumb constructors *)

let make_string name x =
  Valid (String x), {name}

let make_string_o name x =
  let x = match x with
    | None -> None
    | Some x ->
      let x = String.trim x in
      if x = "" then None else Some (String x) in
  Valid (Option (x, String "")), {name}

let make_int name ?min ?max x =
  Valid (Int (x, min, max)), {name}

let make_int_o name ?min ?max x =
  let x = match x with
    | Some x -> Some (Int (x, min, max))
    | None -> None in
  Valid (Option (x, Int (0, min, max))), {name}

let make_float name x =
  Valid (Float x), {name}

let make_float_o name x =
  let x = match x with
    | Some x -> Some (Float x)
    | None -> None in
  Valid (Option (x, Float 0.)), {name}

let make_select name l x =
  Valid (Select (l, x)), {name}

let make_select_o name l x =
  let x = match x with
    | Some x -> Some (Select (l, x))
    | None -> None in
  (* we keep the list in the dummy field! *)
  Valid (Option (x, Select (l, ""))), {name}

let make_bool name x =
  Valid (Bool x), {name}

let make_bool_o name x =
  let x = match x with
    | Some x -> Some (Bool x)
    | None -> None in
  Valid (Option (x, Bool false)), {name}

let make_text name x =
  Valid (Text x), {name}

let make_text_o name x =
  let x = match x with
    | Some x -> Some (Text x)
    | None -> None in
  Valid (Option (x, Text "")), {name}


(* reads an optional string (which can later be converted) *)
let read post name =
  Uri.get_query_param post name |> make_string_o name

(* makes a value required *)
let required = function
  | Invalid (Option (Some value, _), e), i -> Invalid (value, e), i
  | Invalid (Option (None, dummy), e), i -> Invalid (dummy, e), i
  | Valid (Option (Some value, _)), i -> Valid (value), i
  | Valid (Option (None, dummy)), i -> Invalid (dummy, `Required), i

(* extracts a typed value *)
let rec extract: type a. a typ -> a = function
  | Option (Some v, _) -> Some (extract v)
  | Option (None, _) -> None
  | String s -> s
  | Int (i, _, _) -> i
  | Float f -> f
  | Select (_, s) -> s
  | Bool b -> b
  | Text t -> t

(* extracts a validated value *)
let get = function
  | Invalid (_, error), {name} -> Error.fail (`Invalid (name, error))
  | Valid value, _ -> Error.return (extract value)


(* converters *)

let convert err conv default = function
  | Invalid (Option (None, _), e), i ->
    Invalid (Option (None, default), e), i
  | Invalid (Option (Some _, _), e), i ->
    Invalid (Option (Some default, default), e), i
  | Valid (Option (None, _)), i ->
    Valid (Option (None, default)), i
  | Valid (Option (Some value, _)), i ->
    try
      match conv value with
      | `OK v -> Valid (Option (Some v, v)), i
      | err -> Invalid (Option (Some default, default), err), i
    with _ ->
      Invalid (Option (Some default, default), err), i

let to_int ?min ?max x =
  let f = function
    | Select _ | Text _ -> `NaN
    | String s ->
      let i = int_of_string s in
      match min, max with
        | Some min, _ when i < min -> `Too_small min
        | _, Some max when i > max -> `Too_large max
        | _ -> `OK (Int (i, min, max)) in
  convert `NaN f (Int (0, min, max)) x

let to_float x =
  let f = function
    | String s -> `OK (Float (float_of_string s))
    | Select _ | Text _ -> `NaN in
  convert `NaN f (Float 0.) x


open Tyxml.Html

(* FIXME use label *)
let input (x, {name}) =
  (* FIXME display the error *)
  let value, _err = match x with
    | Valid value -> value, None
    | Invalid (value, err) -> value, Some err in
  let rec output: type a. bool -> a typ -> _ = fun required -> function
    | Option (None, default) -> output false default
    | Option (Some x, _) -> output false x
    | Bool b ->
      let attrs = [a_input_type `Checkbox; a_value "1"] in
      input ~a:(attrs @ if b then [a_checked ()] else []) ()
    | o ->
      let attrs = a_name name :: if required then [a_required ()] else [] in
      match o with
      | Option _ -> failwith "ill-formed typ: nested Option"
      | Bool _ -> failwith "ill-formed typ: optional Bool" (* TODO radio *)
      | String s -> input ~a:(a_value s :: attrs) ()
      | Select (l, s) ->
        let options = List.map (fun (k, v) ->
          let a = if k = s then [a_selected ()] else [] in
          option ~a (txt v)
        ) l in
        select ~a:[a_name name] options
      | Float f ->
        let s = string_of_float f in
        input ~a:(a_input_type `Number :: a_value s :: attrs) ()
      | Int (i, min, max) ->
        let s = string_of_int i in
        let bounds =
          (match min with None -> [] | Some m -> [a_input_min (`Number m)]) @
          (match max with None -> [] | Some m -> [a_input_max (`Number m)]) in
        input ~a:([a_input_type `Number; a_value s] @ bounds @ attrs) ()
      | Text s ->
        textarea ~a:attrs (txt s) in
  output true value

let label lbl control =
  label [txt lbl; control]

let form meth content =
  form ~a:[a_method meth] content
