(* Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

open CommonFunctions
open Gen_go_helper

let render_enums enums destdir =
  let header =
    render_template "FileHeader.mustache"
      (`O [("modules", `Null)])
      ~newline:true ()
  in
  let enums = render_template "Enum.mustache" enums () |> String.trim in
  let rendered = header ^ enums ^ "\n" in
  generate_file ~rendered ~destdir ~output_file:"enums.go"

let render_api_messages_and_errors destdir =
  let obj =
    `O
      [
        ("api_errors", `A Json.api_errors)
      ; ("api_messages", `A Json.api_messages)
      ; ("modules", `Null)
      ]
  in
  let header = render_template "FileHeader.mustache" obj ~newline:true () in
  let error_rendered =
    header ^ render_template "APIErrors.mustache" obj ~newline:true ()
  in
  let messages_rendered =
    header ^ render_template "APIMessages.mustache" obj ~newline:true ()
  in
  generate_file ~rendered:error_rendered ~destdir ~output_file:"api_errors.go" ;
  generate_file ~rendered:messages_rendered ~destdir
    ~output_file:"api_messages.go"

let render_convert_header () =
  let name s = `O [("name", `String s); ("sname", `Null)] in
  let obj =
    `O
      [
        ("name", `String "convert")
      ; ( "modules"
        , `O
            [
              ("import", `Bool true)
            ; ( "items"
              , `A (List.map name ["fmt"; "math"; "reflect"; "strconv"; "time"])
              )
            ]
        )
      ]
  in
  render_template "FileHeader.mustache" obj ~newline:true ()

let render_converts destdir =
  let event = render_template "ConvertBatch.mustache" Convert.event_batch () in
  let interface =
    render_template "ConvertInterface.mustache" Convert.interface ()
  in
  let param_types = TypesOfMessages.of_params objects in
  let result_types = TypesOfMessages.of_results objects in
  let generate types of_json =
    types
    |> List.map (fun ty ->
           let params = Convert.of_ty ty in
           let template = Convert.template_of_convert params in
           let json : Mustache.Json.t = of_json params in
           render_template template json ()
       )
    |> String.concat ""
  in
  let rendered =
    let serializes_rendered = generate param_types Convert.of_serialize in
    let deserializes_rendered = generate result_types Convert.of_deserialize in
    render_convert_header ()
    ^ serializes_rendered
    ^ deserializes_rendered
    ^ event
    ^ String.trim interface
    ^ "\n"
  in
  generate_file ~rendered ~destdir ~output_file:"convert.go"

let main destdir =
  render_api_messages_and_errors destdir ;
  let enums = Json.all_enums objects in
  render_enums enums destdir ;
  render_converts destdir ;
  let objects = Json.xenapi objects in
  List.iter
    (fun (name, obj) ->
      let header_rendered =
        render_template "FileHeader.mustache" obj ~newline:true ()
      in
      let record_rendered = render_template "Record.mustache" obj () in
      let rendered = header_rendered ^ record_rendered in
      let output_file = name ^ ".go" in
      generate_file ~rendered ~destdir ~output_file
    )
    objects

let _ =
  let destdir = ref "." in
  Arg.parse
    [
      ( "--destdir"
      , Arg.Set_string destdir
      , "the destination directory for the generated files"
      )
    ]
    (fun x -> Printf.fprintf stderr "Ignoring unknown parameter: %s\n%!" x)
    "Generates Go SDK." ;
  let destdir = !destdir // "src" in
  Xapi_stdext_unix.Unixext.mkdir_rec destdir 0o755 ;
  main destdir
