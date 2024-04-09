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

let render_api_messages_and_errors () =
  let obj =
    `O
      [
        ("api_errors", `A Json.api_errors)
      ; ("api_messages", `A Json.api_messages)
      ; ("modules", `Null)
      ]
  in
  let header = render_template "FileHeader.mustache" obj ^ "\n" in
  let error_rendered =
    header ^ render_template "APIErrors.mustache" obj ^ "\n"
  in
  let messages_rendered =
    header ^ render_template "APIMessages.mustache" obj ^ "\n"
  in
  generate_file error_rendered "api_errors.go" ;
  generate_file messages_rendered "api_messages.go"

let main () =
  render_api_messages_and_errors () ;
  let objects = Json.xenapi objects in
  List.iter
    (fun (name, obj) ->
      let header_rendered = render_template "FileHeader.mustache" obj ^ "\n" in
      let enums_rendered = render_template "Enum.mustache" obj in
      let record_rendered = render_template "Record.mustache" obj in
      let rendered = header_rendered ^ enums_rendered ^ record_rendered in
      let output_file = name ^ ".go" in
      generate_file rendered output_file
    )
    objects

let _ = main ()
