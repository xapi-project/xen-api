(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

open CommonFunctions
module DT = Datamodel_types

let sr_xml' = ref ""

let _ =
  Arg.parse
    [
      ( "-s"
      , Arg.Set_string sr_xml'
      , "specifies the location of the XE_SR_ERRORCODES.xml file"
      )
    ]
    (fun x -> raise (Arg.Bad ("Found anonymous argument " ^ x)))
    "Generates the C# SDK for the XenAPI. See -help."

let sr_xml = !sr_xml'

let destdir = "autogen-out/src"

let templdir = "templates"

let friendly_names = Hashtbl.create 10

let rec friendly_names_all errors =
  Hashtbl.iter update_entries errors ;
  let keys = Hashtbl.fold get_keys friendly_names [] in
  let keys' = List.sort String.compare keys in
  let values = List.map (Hashtbl.find friendly_names) keys' in
  List.combine keys' values

and update_entries _ error =
  let name = error.DT.err_name in
  let desc = error.DT.err_doc in
  if Hashtbl.mem friendly_names name then
    ()
  else
    Hashtbl.add friendly_names name desc

and get_keys key _ tail = key :: tail

let rec get_node k = function
  | Xml.Element (k', _, [Xml.PCData v]) :: _ when k = k' ->
      v
  | _ :: xs ->
      get_node k xs
  | [] ->
      ""

let parse_sr_xml filename =
  let update_entry children =
    let description = get_node "description" children in
    let value = get_node "value" children in
    let key = "SR_BACKEND_FAILURE_" ^ value in
    if Hashtbl.mem friendly_names key then
      ()
    else
      Hashtbl.add friendly_names key description
  in
  let rec parse_xml = function
    | Xml.Element ("SM-errorcodes", _, children) ->
        List.iter parse_xml children
    | Xml.Element ("code", _, children) ->
        update_entry children
    | _ ->
        ()
  in
  try parse_xml (Xml.parse_file filename)
  with Xml.Error e as exn ->
    Printf.eprintf "%s\n%!" (Xml.error e) ;
    raise exn

let parse_resx filename =
  let update_entry attrs children =
    let key = List.assoc "name" attrs in
    let value = get_node "value" children in
    Hashtbl.replace friendly_names key value
  in
  let rec parse_xml = function
    | Xml.Element ("root", _, children) ->
        List.iter parse_xml children
    | Xml.Element ("data", attrs, children) ->
        update_entry attrs children
    | _ ->
        ()
  in
  try parse_xml (Xml.parse_file filename)
  with Xml.Error e as exn ->
    Printf.eprintf "%s\n%!" (Xml.error e) ;
    raise exn

let _ =
  let resx_file = "FriendlyErrorNames.resx" in
  let designer_file = "FriendlyErrorNames.Designer.cs" in
  parse_resx resx_file ;
  parse_sr_xml sr_xml ;
  let errors = friendly_names_all Datamodel.errors in
  let json =
    `O
      [
        ( "i18n_errors"
        , `A
            (List.map
               (fun (x, y) ->
                 `O
                   [
                     ("i18n_error_key", `String x)
                   ; ("i18n_error_description", `String y)
                   ; ( "i8in_error_description_replace_newlines"
                     , `String
                         (Str.global_replace (Str.regexp_string "\n")
                            "\n        ///   " y
                         )
                     )
                   ]
               )
               errors
            )
        )
      ]
  in
  render_file ("FriendlyErrorNames.mustache", resx_file) json templdir destdir ;
  render_file
    ("FriendlyErrorNames.Designer.mustache", designer_file)
    json templdir destdir
