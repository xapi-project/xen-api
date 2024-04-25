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
  let obj =
    `O
      [
        ("name", `String "convert")
      ; ( "modules"
        , `O
            [
              ("import", `Bool true)
            ; ( "items"
              , `A
                  [
                    `O [("name", `String "fmt"); ("sname", `Null)]
                  ; `O [("name", `String "math"); ("sname", `Null)]
                  ; `O [("name", `String "reflect"); ("sname", `Null)]
                  ; `O [("name", `String "strconv"); ("sname", `Null)]
                  ; `O [("name", `String "time"); ("sname", `Null)]
                  ]
              )
            ]
        )
      ]
  in
  render_template "FileHeader.mustache" obj ~newline:true ()

let render_converts converts destdir =
  let templates =
    [
      "ConvertSimpleType.mustache"
    ; "ConvertInt.mustache"
    ; "ConvertFloat.mustache"
    ; "ConvertTime.mustache"
    ; "ConvertRef.mustache"
    ; "ConvertSet.mustache"
    ; "ConvertRecord.mustache"
    ; "ConvertInterface.mustache"
    ; "ConvertMap.mustache"
    ; "ConvertEnum.mustache"
    ; "ConvertBatch.mustache"
    ; "ConvertOption.mustache"
    ]
  in
  let rendered =
    let rendered =
      templates
      |> List.map (fun template -> render_template template converts ())
      |> String.concat ""
      |> String.trim
    in
    render_convert_header () ^ rendered ^ "\n"
  in
  generate_file ~rendered ~destdir ~output_file:"convert.go"

let main destdir =
  render_api_messages_and_errors destdir ;
  let enums = Json.all_enums objects in
  render_enums enums destdir ;
  let objects, converts = objs_and_convert_functions objects in
  render_converts converts destdir ;
  List.iter
    (fun (name, obj) ->
      let header_rendered =
        render_template "FileHeader.mustache" obj ~newline:true ()
      in
      let options_rendered = render_template "Option.mustache" obj () in
      let record_rendered =
        render_template "Record.mustache" obj ~newline:true ()
      in
      let methods_rendered = render_template "Methods.mustache" obj () in
      let rendered =
        let rendered =
          [header_rendered; options_rendered; record_rendered; methods_rendered]
          |> String.concat ""
          |> String.trim
        in
        rendered ^ "\n"
      in
      generate_file ~rendered ~destdir ~output_file:(name ^ ".go")
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
