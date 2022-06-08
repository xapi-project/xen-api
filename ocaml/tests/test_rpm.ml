(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Test_highlevel
open Rpm

let fields_of_pkg =
  Fmt.Dump.
    [
      field "name" (fun (r : Pkg.t) -> r.name) string
    ; field "epoch" (fun (r : Pkg.t) -> Epoch.to_string r.epoch) string
    ; field "version" (fun (r : Pkg.t) -> r.version) string
    ; field "release" (fun (r : Pkg.t) -> r.release) string
    ; field "arch" (fun (r : Pkg.t) -> r.arch) string
    ]
  

module PkgOfFullnameTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = Line of string | FilePath of string

    type output_t = (Pkg.t option, exn) result

    let string_of_input_t = function
      | Line s ->
          "Line: " ^ s
      | FilePath p ->
          "File " ^ p

    let string_of_output_t =
      Fmt.(
        str "%a"
          Dump.(result ~ok:(option @@ record @@ fields_of_pkg) ~error:exn)
      )
  end

  exception Can_not_parse of string

  let transform input =
    try
      match input with
      | Io.Line line ->
          Ok (Pkg.of_fullname line)
      | Io.FilePath p -> (
          let rec for_each_line ic =
            match input_line ic with
            | line -> (
              match Pkg.of_fullname line with
              | None
              | Some Pkg.{name= ""; _}
              | Some Pkg.{version= ""; _}
              | Some Pkg.{release= ""; _}
              | Some Pkg.{arch= ""; _} ->
                  raise (Can_not_parse line)
              | _ ->
                  for_each_line ic
            )
            | exception End_of_file ->
                ()
          in
          let in_ch = open_in p in
          try for_each_line in_ch ; close_in in_ch ; Ok None
          with e -> close_in in_ch ; raise e
        )
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        (Io.Line "libpath-utils-0.2.1-29.el7.x86", Ok None)
      ; (Io.Line "libpath-utils.x86_64", Ok None)
      ; ( Io.Line "libpath-utils-0.2.1-29.el7.noarch"
        , Ok
            (Some
               Pkg.
                 {
                   name= "libpath-utils"
                 ; epoch= None
                 ; version= "0.2.1"
                 ; release= "29.el7"
                 ; arch= "noarch"
                 }
               
            )
        )
      ; ( Io.Line "libpath-utils-0.2.1-29.el7.x86_64"
        , Ok
            (Some
               Pkg.
                 {
                   name= "libpath-utils"
                 ; epoch= None
                 ; version= "0.2.1"
                 ; release= "29.el7"
                 ; arch= "x86_64"
                 }
               
            )
        )
      ; ( Io.Line "libpath-utils-2:0.2.1-29.el7.x86_64"
        , Ok
            (Some
               Pkg.
                 {
                   name= "libpath-utils"
                 ; epoch= Some 2
                 ; version= "0.2.1"
                 ; release= "29.el7"
                 ; arch= "x86_64"
                 }
               
            )
        )
      ; ( Io.Line "libpath-utils-(none):0.2.1-29.el7.x86_64"
        , Ok
            (Some
               Pkg.
                 {
                   name= "libpath-utils"
                 ; epoch= None
                 ; version= "0.2.1"
                 ; release= "29.el7"
                 ; arch= "x86_64"
                 }
               
            )
        )
      ; (Io.Line "libpath-utils-:0.2.1-29.el7.x86_64", Ok None)
      ; (Io.Line "libpath-utils-2:0.2.1-29.el7x86_64", Ok None)
      ; (* all RPM packages installed by default *)
        (Io.FilePath "test_data/repository_pkg_of_fullname_all", Ok None)
      ]
end)

module PkgCompareVersionStringsTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string * string

    type output_t = string

    let string_of_input_t = Fmt.(str "%a" Dump.(pair string string))

    let string_of_output_t = Fmt.(str "%a" Dump.string)
  end

  let transform (s1, s2) =
    Pkg.string_of_order (Pkg.compare_version_strings s1 s2)

  let tests =
    `QuickAndAutoDocumented
      [
        (("1.2.3", "1.2.4"), "<")
      ; (("1.2.3", "1.2.3"), "=")
      ; (("1.2.3", "1.2"), ">")
      ; (("1.0011", "1.9"), ">")
      ; (("1.05", "1.5"), "=")
      ; (("1.0", "1"), ">")
      ; (("1.0", "1.a"), ">")
      ; (("2.50", "2.5"), ">")
      ; (("XS3", "xs2"), "<")
      ; (("1.2.3", "1.2.3a"), ">")
      ; (("xs4", "xs.4"), "=")
      ; (("2a", "2.0"), "<")
      ; (("2a", "2b"), "<")
      ; (("1.0", "1.xs2"), ">")
      ; (("1.0_xs", "1.0.xs"), "=")
      ; (("1.0x3", "1.0x04"), ">")
      ; (("1.0O3", "1.0O04"), ">")
      ]
end)

let tests =
  make_suite "rpm_"
    [
      ("pkg_of_fullname", PkgOfFullnameTest.tests)
    ; ("pkg_compare_version_strings", PkgCompareVersionStringsTest.tests)
    ]

let () = Alcotest.run "Rpm" tests
