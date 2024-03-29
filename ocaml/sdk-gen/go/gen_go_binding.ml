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

open Gen_go_helper

let main () =
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
