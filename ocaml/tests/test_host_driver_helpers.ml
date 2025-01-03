(*
   Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

open Xapi_host_driver_helpers

let note =
  Alcotest.testable
    (Fmt.of_to_string (fun n ->
         Printf.sprintf "{typ=%d; name=%s; desc=%s}" (Int32.to_int n.typ) n.name
           n.desc
     )
    )
    ( = )

let versions =
  [
    (".note.XenServer", Some "v2.1.3+0.1fix")
  ; (".note.XenServerTwo", Some "2.0.0-rc.2")
  ; (".note.Linux", None)
  ; (".note.gnu.build-id", None)
  ]

let get_version_test =
  List.map
    (fun (filename, expected) ->
      let test_version () =
        let parsed_ver = Result.to_option (get_version filename) in
        Printf.printf "%s\n" filename ;
        Alcotest.(check (option string))
          "ELF notes should be parsed properly" expected parsed_ver
      in
      ( Printf.sprintf {|Validation of ELF note parsing: "%s"|} filename
      , `Quick
      , test_version
      )
    )
    versions

let notes =
  [
    (".note.XenServer", [{typ= 1l; name= "XenServer"; desc= "v2.1.3+0.1fix"}])
  ; ( ".note.XenServerTwo"
    , [
        {typ= 2l; name= "XenServer"; desc= "Built on December 25th"}
      ; {typ= 1l; name= "XenServer"; desc= "2.0.0-rc.2"}
      ]
    )
  ; (".note.Linux", [{typ= 599l; name= "Linux"; desc= "4.19.0+1"}])
  ; ( ".note.gnu.build-id"
    , [{typ= 1l; name= "gnu.build-id"; desc= "\x00\x00\x00"}]
    )
  ]

let note_parsing_test =
  List.map
    (fun (filename, expected) ->
      let test_note () =
        let parsed =
          match get_notes filename with Ok res -> res | Error e -> failwith e
        in
        Printf.printf "%s\n" filename ;
        Alcotest.(check (list note))
          "ELF notes should be parsed properly" expected parsed
      in
      ( Printf.sprintf {|Validation of ELF note parsing: "%s"|} filename
      , `Quick
      , test_note
      )
    )
    notes

let () =
  Suite_init.harness_init () ;
  Alcotest.run "Test Host Driver Helpers suite"
    [
      ("Test_host_driver_helpers.get_note", note_parsing_test)
    ; ("Test_host_driver_helpers.get_version", get_version_test)
    ]
