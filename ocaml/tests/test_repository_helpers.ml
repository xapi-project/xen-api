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
open Repository_helpers

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

let fields_of_update =
  Fmt.Dump.
    [
      field "name" (fun (r : Update.t) -> r.name) string
    ; field "arch" (fun (r : Update.t) -> r.arch) string
    ; field "old_epoch"
        (fun (r : Update.t) -> Option.map Epoch.to_string r.old_epoch)
        (option string)
    ; field "old_version" (fun (r : Update.t) -> r.old_version) (option string)
    ; field "old_release" (fun (r : Update.t) -> r.old_release) (option string)
    ; field "new_epoch"
        (fun (r : Update.t) -> Epoch.to_string r.new_epoch)
        string
    ; field "new_version" (fun (r : Update.t) -> r.new_version) string
    ; field "new_release" (fun (r : Update.t) -> r.new_release) string
    ; field "update_id" (fun (r : Update.t) -> r.update_id) (option string)
    ; field "repository" (fun (r : Update.t) -> r.repository) string
    ]
  

module UpdateOfJsonTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = (Update.t, exn) result

    let string_of_input_t s = s

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(record @@ fields_of_update) ~error:exn))
  end

  let transform input =
    try Ok (Update.of_json (Yojson.Basic.from_string input)) with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        (* A complete normal case *)
        ( {|
          {
            "name": "libpath-utils",
            "arch": "x86_64",
            "oldEpochVerRel": {
              "epoch": "(none)",
              "version": "0.2.1",
              "release": "29.el7"
            },
            "newEpochVerRel": {
              "epoch": "(none)",
              "version": "0.2.2",
              "release": "10.el7"
            },
            "updateId": "UPDATE-0000",
            "repository": "regular"
          }
          |}
        , Ok
            Update.
              {
                name= "libpath-utils"
              ; arch= "x86_64"
              ; old_epoch= Some None
              ; old_version= Some "0.2.1"
              ; old_release= Some "29.el7"
              ; new_epoch= None
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id= Some "UPDATE-0000"
              ; repository= "regular"
              }
            
        )
      ; (* No old version, old release and updateId *)
        ( {|
          {
            "name": "libpath-utils",
            "arch": "x86_64",
            "newEpochVerRel": {
              "epoch": "(none)",
              "version": "0.2.2",
              "release": "10.el7"
            },
            "repository": "regular"
          }
          |}
        , Ok
            Update.
              {
                name= "libpath-utils"
              ; arch= "x86_64"
              ; old_epoch= None
              ; old_version= None
              ; old_release= None
              ; new_epoch= None
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id= None
              ; repository= "regular"
              }
            
        )
      ; (* Missing arch *)
        ( {|
          {
            "name": "libpath-utils",
            "oldEpochVerRel": {
              "epoch": "(none)",
              "version": "0.2.1",
              "release": "29.el7"
            },
            "newEpochVerRel": {
              "epoch": "(none)",
              "version": "0.2.2",
              "release": "10.el7"
            },
            "updateId": "UPDATE-0000",
            "repository": "regular"
          }
          |}
        , Error
            Api_errors.(
              Server_error
                (internal_error, ["Can't construct an update from json"])
            )
        )
        (* A complete normal case with epoch *)
      ; ( {|
          {
            "name": "libpath-utils",
            "arch": "x86_64",
            "oldEpochVerRel": {
              "epoch": "1",
              "version": "0.2.1",
              "release": "29.el7"
            },
            "newEpochVerRel": {
              "epoch": "2",
              "version": "0.2.2",
              "release": "10.el7"
            },
            "updateId": "UPDATE-0000",
            "repository": "regular"
          }
          |}
        , Ok
            Update.
              {
                name= "libpath-utils"
              ; arch= "x86_64"
              ; old_epoch= Some (Some 1)
              ; old_version= Some "0.2.1"
              ; old_release= Some "29.el7"
              ; new_epoch= Some 2
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id= Some "UPDATE-0000"
              ; repository= "regular"
              }
            
        )
      ]
end)

module GuidanceSetAssertValidGuidanceTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = Guidance.t list

    type output_t = (unit, exn) result

    let string_of_input_t l =
      Fmt.(str "%a" Dump.(list string)) (List.map Guidance.to_string l)

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(any "()") ~error:exn))
  end

  let transform input =
    try Ok (GuidanceSet.assert_valid_guidances input) with e -> Error e

  let tests =
    let open Guidance in
    `QuickAndAutoDocumented
      [
        ([], Ok ())
      ; ([RebootHost], Ok ())
      ; ([RestartToolstack], Ok ())
      ; ([RestartDeviceModel], Ok ())
      ; ([EvacuateHost], Ok ())
      ; ([EvacuateHost; RestartToolstack], Ok ())
      ; ([RestartDeviceModel; RestartToolstack], Ok ())
      ; ( [RestartDeviceModel; EvacuateHost]
        , Error
            Api_errors.(
              Server_error
                ( internal_error
                , [GuidanceSet.error_msg [RestartDeviceModel; EvacuateHost]]
                )
            )
        )
      ; ( [EvacuateHost; RestartToolstack; RestartDeviceModel]
        , Error
            Api_errors.(
              Server_error
                ( internal_error
                , [
                    GuidanceSet.error_msg
                      [EvacuateHost; RestartToolstack; RestartDeviceModel]
                  ]
                )
            )
        )
      ; ( [RebootHost; RestartToolstack]
        , Error
            Api_errors.(
              Server_error
                ( internal_error
                , [GuidanceSet.error_msg [RebootHost; RestartToolstack]]
                )
            )
        )
      ; ( [RebootHost; RestartDeviceModel]
        , Error
            Api_errors.(
              Server_error
                ( internal_error
                , [GuidanceSet.error_msg [RebootHost; RestartDeviceModel]]
                )
            )
        )
      ; ( [RebootHost; EvacuateHost]
        , Error
            Api_errors.(
              Server_error
                ( internal_error
                , [GuidanceSet.error_msg [RebootHost; EvacuateHost]]
                )
            )
        )
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

module ApplicabilityEval = Generic.MakeStateless (struct
  module Io = struct
    (*  ( (installed_epoch, installed_version, installed_release) *
     *    (inequality * (epoch, version * release)) ) *)
    type input_t =
      (int option * string * string) * (string * (int option * string * string))

    type output_t = (bool, exn) result

    let string_of_input_t =
      Test_printers.(
        pair
          (tuple3 (option int) string string)
          (pair string (tuple3 (option int) string string))
      )

    let string_of_output_t = Fmt.(str "%a" Dump.(result ~ok:bool ~error:exn))
  end

  let transform ((e1, v1, r1), (ineq, (e2, v2, r2))) =
    try
      let applicability =
        Applicability.
          {
            name= ""
          ; arch= ""
          ; inequality= Some (Applicability.inequality_of_string ineq)
          ; epoch= e2
          ; version= v2
          ; release= r2
          }
        
      in

      Ok (Applicability.eval ~epoch:e1 ~version:v1 ~release:r1 ~applicability)
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        (((None, "1.2.3", "3.el7"), ("gt", (None, "1.2.3", "4.el7"))), Ok false)
      ; (((None, "1.2.3", "3.el7"), ("gt", (None, "1.2.4", "3.el7"))), Ok false)
      ; (((None, "1.2.3", "3.el7"), ("gt", (None, "1.2.3", "2.el7"))), Ok true)
      ; (((None, "1.2.3", "3.el7"), ("gt", (None, "1.2.2", "3.el7"))), Ok true)
      ; (((None, "1.2.3", "3.el7"), ("lt", (None, "1.2.3", "2.el7"))), Ok false)
      ; (((None, "1.2.3", "3.el7"), ("lt", (None, "1.2.2", "3.el7"))), Ok false)
      ; (((None, "1.2.3", "3.el7"), ("lt", (None, "1.2.3", "4.el7"))), Ok true)
      ; (((None, "1.2.3", "3.el7"), ("lt", (None, "1.2.4", "3.el7"))), Ok true)
      ; (((None, "1.2.3", "3.el7"), ("eq", (None, "1.2.3", "3.el7"))), Ok true)
      ; (((None, "1.2.3", "3.el7"), ("eq", (None, "1.2.4", "3.el7"))), Ok false)
      ; (((None, "1.2.3", "3.el7"), ("gte", (None, "1.2.3", "4.el7"))), Ok false)
      ; (((None, "1.2.3", "3.el7"), ("gte", (None, "1.2.4", "3.el7"))), Ok false)
      ; (((None, "1.2.3", "3.el7"), ("gte", (None, "1.2.3", "3.el7"))), Ok true)
      ; (((None, "1.2.3", "3.el7"), ("gte", (None, "1.2.3", "2.el7"))), Ok true)
      ; (((None, "1.2.3", "3.el7"), ("gte", (None, "1.2.2", "3.el7"))), Ok true)
      ; (((None, "1.2.3", "3.el7"), ("lte", (None, "1.2.3", "2.el7"))), Ok false)
      ; (((None, "1.2.3", "3.el7"), ("lte", (None, "1.2.2", "3.el7"))), Ok false)
      ; (((None, "1.2.3", "3.el7"), ("lte", (None, "1.2.3", "3.el7"))), Ok true)
      ; (((None, "1.2.3", "3.el7"), ("lte", (None, "1.2.3", "4.el7"))), Ok true)
      ; (((None, "1.2.3", "3.el7"), ("lte", (None, "1.2.4", "3.el7"))), Ok true)
      ; ( ((None, "1.2.3", "3.el7"), ("let", (None, "1.2.3", "3.el7")))
        , Error Applicability.Invalid_inequality
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gt", (Some 3, "1.2.3", "4.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gt", (Some 3, "1.2.4", "3.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gt", (Some 3, "1.2.3", "2.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gt", (Some 3, "1.2.2", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lt", (Some 3, "1.2.3", "2.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lt", (Some 3, "1.2.2", "3.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lt", (Some 3, "1.2.3", "4.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lt", (Some 3, "1.2.4", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("eq", (Some 3, "1.2.3", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("eq", (Some 3, "1.2.4", "3.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gte", (Some 3, "1.2.3", "4.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gte", (Some 3, "1.2.4", "3.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gte", (Some 3, "1.2.3", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gte", (Some 3, "1.2.3", "2.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gte", (Some 3, "1.2.2", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lte", (Some 3, "1.2.3", "2.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lte", (Some 3, "1.2.2", "3.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lte", (Some 3, "1.2.3", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lte", (Some 3, "1.2.3", "4.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lte", (Some 3, "1.2.4", "3.el7")))
        , Ok true
        )
      ; (((Some 1, "1.2.3", "3.el7"), ("gt", (None, "1.2.3", "4.el7"))), Ok true)
      ; (((Some 1, "1.2.3", "3.el7"), ("gt", (None, "1.2.4", "3.el7"))), Ok true)
      ; (((Some 1, "1.2.3", "3.el7"), ("gt", (None, "1.2.3", "2.el7"))), Ok true)
      ; (((Some 1, "1.2.3", "3.el7"), ("gt", (None, "1.2.2", "3.el7"))), Ok true)
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lt", (None, "1.2.3", "2.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lt", (None, "1.2.2", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lt", (None, "1.2.3", "4.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lt", (None, "1.2.4", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("eq", (None, "1.2.3", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("eq", (None, "1.2.4", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gte", (None, "1.2.3", "4.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gte", (None, "1.2.4", "3.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gte", (None, "1.2.3", "3.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gte", (None, "1.2.3", "2.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gte", (None, "1.2.2", "3.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lte", (None, "1.2.3", "2.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lte", (None, "1.2.2", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lte", (None, "1.2.3", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lte", (None, "1.2.3", "4.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lte", (None, "1.2.4", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gt", (Some 2, "1.2.3", "4.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gt", (Some 2, "1.2.4", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gt", (Some 2, "1.2.3", "2.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gt", (Some 2, "1.2.2", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lt", (Some 2, "1.2.3", "2.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lt", (Some 2, "1.2.2", "3.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lt", (Some 2, "1.2.3", "4.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lt", (Some 2, "1.2.4", "3.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("eq", (Some 2, "1.2.3", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("eq", (Some 2, "1.2.4", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gte", (Some 2, "1.2.3", "4.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gte", (Some 2, "1.2.4", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gte", (Some 2, "1.2.3", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gte", (Some 2, "1.2.3", "2.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("gte", (Some 2, "1.2.2", "3.el7")))
        , Ok false
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lte", (Some 2, "1.2.3", "2.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lte", (Some 2, "1.2.2", "3.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lte", (Some 2, "1.2.3", "3.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lte", (Some 2, "1.2.3", "4.el7")))
        , Ok true
        )
      ; ( ((Some 1, "1.2.3", "3.el7"), ("lte", (Some 2, "1.2.4", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gt", (Some 2, "1.2.3", "4.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gt", (Some 2, "1.2.4", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gt", (Some 2, "1.2.3", "2.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gt", (Some 2, "1.2.2", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lt", (Some 2, "1.2.3", "2.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lt", (Some 2, "1.2.2", "3.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lt", (Some 2, "1.2.3", "4.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lt", (Some 2, "1.2.4", "3.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("eq", (Some 2, "1.2.3", "3.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("eq", (Some 2, "1.2.4", "3.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gte", (Some 2, "1.2.3", "4.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gte", (Some 2, "1.2.4", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gte", (Some 2, "1.2.3", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gte", (Some 2, "1.2.3", "2.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("gte", (Some 2, "1.2.2", "3.el7")))
        , Ok true
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lte", (Some 2, "1.2.3", "2.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lte", (Some 2, "1.2.2", "3.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lte", (Some 2, "1.2.3", "3.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lte", (Some 2, "1.2.3", "4.el7")))
        , Ok false
        )
      ; ( ((Some 3, "1.2.3", "3.el7"), ("lte", (Some 2, "1.2.4", "3.el7")))
        , Ok false
        )
      ]
end)

module UpdateInfoMetaDataOfXml = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = (RepoMetaData.t, exn) result

    let string_of_input_t x = x

    let fields =
      Fmt.Dump.
        [
          field "checksum" (fun (r : RepoMetaData.t) -> r.checksum) string
        ; field "location" (fun (r : RepoMetaData.t) -> r.location) string
        ]
      

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(record @@ fields) ~error:exn))
  end

  let transform input =
    try Ok RepoMetaData.(of_xml (Xml.parse_string input) UpdateInfo)
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        (* no data node *)
        ( {|
            <repomd>
            </repomd>
           |}
        , Error Api_errors.(Server_error (invalid_repomd_xml, []))
        )
      ; (* no updateinfo node *)
        ( {|
            <repomd>
              <data type="primary"></data>
            </repomd>
           |}
        , Error Api_errors.(Server_error (invalid_repomd_xml, []))
        )
      ; (* duplicate updateinfo *)
        ( {|
            <repomd>
              <data type="updateinfo">
                <checksum type="sha256">123abc</checksum>
                <location href="repodata/123abc.xml.gz"/>
              </data>
              <data type="updateinfo">
                <checksum type="sha256">123abc</checksum>
                <location href="repodata/123abc.xml.gz"/>
              </data>
            </repomd>
           |}
        , Error Api_errors.(Server_error (invalid_repomd_xml, []))
        )
      ; (* missing checksum *)
        ( {|
            <repomd>
              <data type="updateinfo">
                <location href="repodata/123abc.xml.gz"/>
              </data>
            </repomd>
           |}
        , Error Api_errors.(Server_error (invalid_repomd_xml, []))
        )
      ; (* missing location *)
        ( {|
            <repomd>
              <data type="updateinfo">
                <checksum type="sha256">123abc</checksum>
                <location href=""/>
              </data>
            </repomd>
           |}
        , Error Api_errors.(Server_error (invalid_repomd_xml, []))
        )
      ; (* normal case *)
        ( {|
            <repomd>
              <data type="updateinfo">
                <checksum type="sha256">123abc</checksum>
                <location href="repodata/123abc.xml.gz"/>
              </data>
            </repomd>
           |}
        , Ok
            RepoMetaData.
              {checksum= "123abc"; location= "repodata/123abc.xml.gz"}
            
        )
      ]
end)

let fields_of_updateinfo =
  Fmt.Dump.
    [
      field "id" (fun (r : UpdateInfo.t) -> r.id) string
    ; field "summary" (fun (r : UpdateInfo.t) -> r.summary) string
    ; field "description" (fun (r : UpdateInfo.t) -> r.description) string
    ; field "rec_guidance"
        (fun (r : UpdateInfo.t) -> UpdateInfo.guidance_to_string r.rec_guidance)
        string
    ; field "abs_guidance"
        (fun (r : UpdateInfo.t) -> UpdateInfo.guidance_to_string r.abs_guidance)
        string
    ; field "guidance_applicabilities"
        (fun (r : UpdateInfo.t) ->
          List.map Applicability.to_string r.guidance_applicabilities
        )
        (list string)
    ; field "spec_info" (fun (r : UpdateInfo.t) -> r.spec_info) string
    ; field "url" (fun (r : UpdateInfo.t) -> r.url) string
    ; field "update_type" (fun (r : UpdateInfo.t) -> r.update_type) string
    ]
  

module UpdateInfoOfXml = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = ((string * UpdateInfo.t) list, exn) result

    let string_of_input_t s = s

    let string_of_output_t =
      Fmt.(
        str "%a"
          Dump.(
            result
              ~ok:(list (pair string (record @@ fields_of_updateinfo)))
              ~error:exn
          )
      )
  end

  let transform input =
    try Ok (UpdateInfo.of_xml (Xml.parse_string input)) with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        (* No "updates" node *)
        ( {|
            <pdates>
            </pdates>
          |}
        , Error Api_errors.(Server_error (invalid_updateinfo_xml, []))
        )
      ; (* No update in updateinfo.xml *)
        ({|
            <updates>
            </updates>
          |}, Ok [])
      ; (* Missing update_type *)
        ( {|
            <updates>
              <update type="">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
              </update>
            </updates>
          |}
        , Error Api_errors.(Server_error (invalid_updateinfo_xml, []))
        )
      ; (* Missing id *)
        ( {|
            <updates>
              <update type="security">
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
              </update>
            </updates>
          |}
        , Error Api_errors.(Server_error (invalid_updateinfo_xml, []))
        )
      ; (* Missing summary *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
              </update>
            </updates>
          |}
        , Error Api_errors.(Server_error (invalid_updateinfo_xml, []))
        )
      ; (* Missing description *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <description/>
                <summary>summary</summary>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
              </update>
            </updates>
          |}
        , Ok
            [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= ""
                  ; rec_guidance= None
                  ; abs_guidance= None
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
        )
      ; (* Duplicate update ID *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
              </update>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
              </update>
            </updates>
          |}
        , Error Api_errors.(Server_error (invalid_updateinfo_xml, []))
        )
      ; (* Single update *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
              </update>
            </updates>
          |}
        , Ok
            [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= None
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
        )
      ; (* Two updates *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
              </update>
              <update type="security">
                <id>UPDATE-0001</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
              </update>
            </updates>
          |}
        , Ok
            [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= None
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ; ( "UPDATE-0001"
              , UpdateInfo.
                  {
                    id= "UPDATE-0001"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= None
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
        )
      ; (* Single update with guidances *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidance>RestartDeviceModel</recommended_guidance>
                <absolute_guidance>RebootHost</absolute_guidance>
                <guidance_applicabilities>
                  <applicability>
                    <name>xsconsole</name>
                    <inequality>gte</inequality>
                    <epoch>None</epoch>
                    <version>10.1.0</version>
                    <release>25</release>
                    <arch>x86_64</arch>
                  </applicability>
                  <applicability>
                    <name>xsconsole</name>
                    <inequality>lt</inequality>
                    <epoch>None</epoch>
                    <version>10.1.0</version>
                    <release>25</release>
                    <arch>x86_64</arch>
                  </applicability>
                </guidance_applicabilities>
              </update>
            </updates>
          |}
        , Ok
            [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= Some Guidance.RestartDeviceModel
                  ; abs_guidance= Some Guidance.RebootHost
                  ; guidance_applicabilities=
                      [
                        Applicability.
                          {
                            name= "xsconsole"
                          ; arch= "x86_64"
                          ; inequality= Some Gte
                          ; epoch= None
                          ; version= "10.1.0"
                          ; release= "25"
                          }
                        
                      ; Applicability.
                          {
                            name= "xsconsole"
                          ; arch= "x86_64"
                          ; inequality= Some Lt
                          ; epoch= None
                          ; version= "10.1.0"
                          ; release= "25"
                          }
                        
                      ]
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
        )
      ]
end)

module AssertUrlIsValid = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string * string list

    type output_t = (unit, exn) result

    let string_of_input_t = Fmt.(str "%a" Dump.(pair string (list string)))

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(any "()") ~error:exn))
  end

  let transform (url, domain_name_allowlist) =
    Xapi_globs.repository_domain_name_allowlist := domain_name_allowlist ;
    try Ok (assert_url_is_valid ~url) with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        ( ("htt://a.b.c", [])
        , Error Api_errors.(Server_error (invalid_base_url, ["htt://a.b.c"]))
        )
      ; ( ("http://a.b.c", ["c.com"; "d.com"])
        , Error Api_errors.(Server_error (invalid_base_url, ["http://a.b.c"]))
        )
      ; (("https://a.b.c", []), Ok ())
      ; (("http://a.b.c", []), Ok ())
      ; (("http://a.b.c.com", ["c.com"; "d.com"]), Ok ())
      ; ( ("http://a.b.c.comm", ["c.com"; "d.com"])
        , Error
            Api_errors.(Server_error (invalid_base_url, ["http://a.b.c.comm"]))
        )
      ; (("http://a.b...c.com", ["c.com"; "d.com"]), Ok ())
      ; ( ("http://a.b.cc.com", ["c.com"; "d.com"])
        , Error
            Api_errors.(Server_error (invalid_base_url, ["http://a.b.cc.com"]))
        )
      ; (("http://a.b.c.com//", ["c.com"; "d.com"]), Ok ())
      ; (("http://a.b.c.com/a/b", ["c.com"; "d.com"]), Ok ())
      ; (("http://a.b.c.com/a/b/", ["c.com"; "d.com"]), Ok ())
      ; (("https://a.b.d.com//", ["c.com"; "d.com"]), Ok ())
      ; (("https://a.b.d.com/a/b", ["c.com"; "d.com"]), Ok ())
      ; (("https://a.b.d.com/a/b/", ["c.com"; "d.com"]), Ok ())
      ]
end)

module WriteYumConfig = Generic.MakeStateless (struct
  module Io = struct
    (*           ( (source_url, binary_url),  (need_gpg_check, gpgkey_path) ) *)
    type input_t = (string option * string) * (bool * string option)

    type output_t = (string, exn) result

    let string_of_input_t =
      Fmt.(
        str "%a"
          Dump.(pair (pair (option string) string) (pair bool (option string)))
      )

    let string_of_output_t = Fmt.(str "%a" Dump.(result ~ok:string ~error:exn))
  end

  let repo_name = "unittest"

  let repo_suffix = ".repo"

  let tmp_dir = Filename.get_temp_dir_name ()

  let gpgkey_path = "unittest-gpgkey"

  let transform ((source_url, binary_url), (gpg_check, name)) =
    Xapi_globs.yum_repos_config_dir := tmp_dir ;
    Xapi_globs.repository_gpgcheck := gpg_check ;
    Xapi_globs.rpm_gpgkey_dir := tmp_dir ;
    (* Create empty gpgkey file if it is needed *)
    Option.iter
      (fun n ->
        if n = gpgkey_path then close_out (open_out (Filename.concat tmp_dir n))
      )
      name ;
    let rec read_from_in_channel acc ic =
      match input_line ic with
      | line ->
          (read_from_in_channel [@tailcall]) (acc ^ line ^ "\n") ic
      | exception End_of_file ->
          acc
    in
    let repo_file_path = Filename.concat tmp_dir (repo_name ^ repo_suffix) in
    let finally () =
      try
        Unix.unlink repo_file_path ;
        Option.iter
          (fun n -> if n <> "" then Unix.unlink (Filename.concat tmp_dir n))
          name
      with _ -> ()
    in
    let gpgkey_path' = Option.value ~default:"" name in
    try
      (* The path of file which will be written by write_yum_config *)
      write_yum_config ~source_url ~binary_url ~repo_gpgcheck:true
        ~gpgkey_path:gpgkey_path' ~repo_name ;
      let in_ch = open_in repo_file_path in
      let content = read_from_in_channel "" in_ch in
      close_in in_ch ; finally () ; Ok content
    with e -> finally () ; Error e

  let url = "https://a.b.c/repository"

  let content1 =
    Printf.sprintf
      {|[%s]
name=%s
baseurl=%s
enabled=0
repo_gpgcheck=1
gpgcheck=1
gpgkey=file://%s
|}
      repo_name repo_name url
      (Filename.concat tmp_dir gpgkey_path)

  let content2 =
    Printf.sprintf
      {|[%s]
name=%s
baseurl=%s
enabled=0
repo_gpgcheck=0
gpgcheck=0
|}
      repo_name repo_name url

  let src_content1 =
    Printf.sprintf
      {|
[%s-source]
name=%s-source
baseurl=%s
enabled=0
repo_gpgcheck=1
gpgcheck=1
gpgkey=file://%s
|}
      repo_name repo_name url
      (Filename.concat tmp_dir gpgkey_path)

  let src_content2 =
    Printf.sprintf
      {|

[%s-source]
name=%s-source
baseurl=%s
enabled=0
repo_gpgcheck=0
gpgcheck=0
|}
      repo_name repo_name url

  let tests =
    `QuickAndAutoDocumented
      [
        ( ((None, url), (true, Some "non-exists"))
        , Error
            Api_errors.(
              Server_error (internal_error, ["gpg key file does not exist"])
            )
        )
      ; ( ((None, url), (true, None))
        , Error
            Api_errors.(
              Server_error
                (internal_error, ["gpg key file is not a regular file"])
            )
        )
      ; ( ((None, url), (true, Some ""))
        , Error
            Api_errors.(
              Server_error
                (internal_error, ["gpg key file is not a regular file"])
            )
        )
      ; (((None, url), (true, Some gpgkey_path)), Ok content1)
      ; (((None, url), (false, None)), Ok content2)
      ; (((None, url), (false, Some gpgkey_path)), Ok content2)
      ; ( ((Some url, url), (true, Some gpgkey_path))
        , Ok (content1 ^ src_content1)
        )
      ; (((Some url, url), (false, None)), Ok (content2 ^ src_content2))
      ; ( ((Some url, url), (false, Some gpgkey_path))
        , Ok (content2 ^ src_content2)
        )
      ]
end)

module EvalGuidanceForOneUpdate = Generic.MakeStateless (struct
  module Io = struct
    type input_t = (string * UpdateInfo.t) list * Update.t

    type output_t = Guidance.t option

    let string_of_input_t =
      Fmt.(
        str "%a"
          Dump.(
            pair
              (list (pair string (record @@ fields_of_updateinfo)))
              (record @@ fields_of_update)
          )
      )

    let string_of_output_t g =
      Fmt.(str "%a" Dump.(string)) (UpdateInfo.guidance_to_string g)
  end

  let transform (updates_info, update) =
    eval_guidance_for_one_update ~updates_info ~update ~kind:Guidance.Absolute

  let tests =
    `QuickAndAutoDocumented
      [
        (* Update ID in update can't be found in updateinfo list *)
        ( ( []
          , Update.
              {
                (* No id here *)
                name= "xsconsole"
              ; arch= "x86_64"
              ; old_epoch= None
              ; old_version= None
              ; old_release= None
              ; new_epoch= None
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id= Some "UPDATE-0000"
              ; repository= "regular"
              }
            
          )
        , None
        )
      ; (* Update ID in update can't be found in updateinfo list *)
        ( ( [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= Some Guidance.EvacuateHost
                  ; abs_guidance= Some Guidance.RebootHost
                  ; guidance_applicabilities= []
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ; ( "UPDATE-0001"
              , UpdateInfo.
                  {
                    id= "UPDATE-0001"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= Some Guidance.EvacuateHost
                  ; abs_guidance= Some Guidance.RebootHost
                  ; guidance_applicabilities= []
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
          , Update.
              {
                name= "xsconsole"
              ; arch= "x86_64"
              ; old_epoch= Some None
              ; old_version= Some "0.2.1"
              ; old_release= Some "29.el7"
              ; new_epoch= None
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id=
                  Some "UPDATE-0002" (* This ID can't be found in above *)
              ; repository= "regular"
              }
            
          )
        , None
        )
      ; (* No update ID in update *)
        ( ( [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= Some Guidance.EvacuateHost
                  ; abs_guidance= Some Guidance.RebootHost
                  ; guidance_applicabilities= []
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
          , Update.
              {
                name= "xsconsole"
              ; arch= "x86_64"
              ; old_epoch= Some None
              ; old_version= Some "0.2.1"
              ; old_release= Some "29.el7"
              ; new_epoch= None
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id= None (* This is None *)
              ; repository= "regular"
              }
            
          )
        , None
        )
      ; (* Empty applicabilities *)
        ( ( [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= None
                  ; guidance_applicabilities= []
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ; ( "UPDATE-0001"
              , UpdateInfo.
                  {
                    id= "UPDATE-0001"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= Some Guidance.RebootHost
                  ; guidance_applicabilities= [] (* No applicabilities *)
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
          , Update.
              {
                name= "xsconsole"
              ; arch= "x86_64"
              ; old_epoch= Some None
              ; old_version= Some "0.2.1"
              ; old_release= Some "29.el7"
              ; new_epoch= None
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id= Some "UPDATE-0001"
              ; repository= "regular"
              }
            
          )
        , Some Guidance.RebootHost
        )
      ; (* Matched applicability *)
        ( ( [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= None
                  ; guidance_applicabilities= []
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ; ( "UPDATE-0001"
              , UpdateInfo.
                  {
                    id= "UPDATE-0001"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= Some Guidance.RestartDeviceModel
                  ; guidance_applicabilities=
                      [
                        Applicability.
                          {
                            name= "xsconsole"
                          ; arch= "x86_64"
                          ; inequality=
                              Some Lte
                              (* old version 0.2.0 is less than 0.2.1 *)
                          ; epoch= None
                          ; version= "0.2.1"
                          ; release= "29.el7"
                          }
                        
                      ]
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
          , Update.
              {
                name= "xsconsole"
              ; arch= "x86_64"
              ; old_epoch= Some None
              ; old_version= Some "0.2.0"
              ; old_release= Some "29.el7"
              ; new_epoch= None
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id= Some "UPDATE-0001"
              ; repository= "regular"
              }
            
          )
        , Some Guidance.RestartDeviceModel
        )
      ; (* Matched in multiple applicabilities *)
        ( ( [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= None
                  ; guidance_applicabilities= []
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ; ( "UPDATE-0001"
              , UpdateInfo.
                  {
                    id= "UPDATE-0001"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= Some Guidance.RestartDeviceModel
                  ; guidance_applicabilities=
                      [
                        Applicability.
                          {
                            name= "xsconsole"
                          ; arch= "x86_64"
                          ; inequality=
                              Some Gt
                              (* Unmatch: old version 0.2.1 is equal to 0.2.1 *)
                          ; epoch= None
                          ; version= "0.2.1"
                          ; release= "29.el7"
                          }
                        
                      ; Applicability.
                          {
                            name= "xsconsole"
                          ; arch= "x86_64"
                          ; inequality=
                              Some Eq
                              (* Match: old version 0.2.1 is equal to 0.2.1 *)
                          ; epoch= None
                          ; version= "0.2.1"
                          ; release= "29.el7"
                          }
                        
                      ]
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
          , Update.
              {
                name= "xsconsole"
              ; arch= "x86_64"
              ; old_epoch= Some None
              ; old_version= Some "0.2.1"
              ; old_release= Some "29.el7"
              ; new_epoch= None
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id= Some "UPDATE-0001"
              ; repository= "regular"
              }
            
          )
        , Some Guidance.RestartDeviceModel
        )
      ; (* No matched applicability *)
        ( ( [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= None
                  ; guidance_applicabilities= []
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ; ( "UPDATE-0001"
              , UpdateInfo.
                  {
                    id= "UPDATE-0001"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= Some Guidance.RestartDeviceModel
                  ; guidance_applicabilities=
                      [
                        Applicability.
                          {
                            name= "xsconsole"
                          ; arch= "x86_64"
                          ; inequality=
                              Some Lte
                              (* Unmatch: old version 0.2.1 is greater than 0.2.0 *)
                          ; epoch= None
                          ; version= "0.2.0"
                          ; release= "29.el7"
                          }
                        
                      ]
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
          , Update.
              {
                name= "xsconsole"
              ; arch= "x86_64"
              ; old_epoch= Some None
              ; old_version= Some "0.2.1"
              ; old_release= Some "29.el7"
              ; new_epoch= None
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id= Some "UPDATE-0001"
              ; repository= "regular"
              }
            
          )
        , None
        )
      ; (* Unmatched arch *)
        ( ( [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= None
                  ; guidance_applicabilities= []
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ; ( "UPDATE-0001"
              , UpdateInfo.
                  {
                    id= "UPDATE-0001"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= Some Guidance.RestartDeviceModel
                  ; guidance_applicabilities=
                      [
                        Applicability.
                          {
                            name= "xsconsole"
                          ; arch=
                              "x86_64" (* Unmatch: arch of update is x86_64 *)
                          ; inequality= Some Lte
                          ; epoch= None
                          ; version= "0.2.1"
                          ; release= "29.el7"
                          }
                        
                      ]
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
          , Update.
              {
                name= "xsconsole"
              ; arch= "noarch"
              ; old_epoch= Some None
              ; old_version= Some "0.2.1"
              ; old_release= Some "29.el7"
              ; new_epoch= None
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id= Some "UPDATE-0001"
              ; repository= "regular"
              }
            
          )
        , None
        )
      ; (* Matched in multiple applicabilities with epoch *)
        ( ( [
              ( "UPDATE-0000"
              , UpdateInfo.
                  {
                    id= "UPDATE-0000"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= None
                  ; guidance_applicabilities= []
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ; ( "UPDATE-0001"
              , UpdateInfo.
                  {
                    id= "UPDATE-0001"
                  ; summary= "summary"
                  ; description= "description"
                  ; rec_guidance= None
                  ; abs_guidance= Some Guidance.RestartDeviceModel
                  ; guidance_applicabilities=
                      [
                        Applicability.
                          {
                            name= "xsconsole"
                          ; arch= "x86_64"
                          ; inequality=
                              Some Gt
                              (* Unmatch: old version 0.2.1 is equal to 0.2.1 *)
                          ; epoch= None
                          ; version= "0.2.1"
                          ; release= "29.el7"
                          }
                        
                      ; Applicability.
                          {
                            name= "xsconsole"
                          ; arch= "x86_64"
                          ; inequality=
                              Some Eq
                              (* Match: old version 0.2.1 is equal to 0.2.1 *)
                          ; epoch= Some 1
                          ; version= "0.1.1"
                          ; release= "29.el7"
                          }
                        
                      ]
                  ; spec_info= "special info"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  }
                
              )
            ]
          , Update.
              {
                name= "xsconsole"
              ; arch= "x86_64"
              ; old_epoch= Some (Some 1)
              ; old_version= Some "0.1.1"
              ; old_release= Some "29.el7"
              ; new_epoch= Some 1
              ; new_version= "0.2.2"
              ; new_release= "10.el7"
              ; update_id= Some "UPDATE-0001"
              ; repository= "regular"
              }
            
          )
        , Some Guidance.RestartDeviceModel
        )
      ]
end)

module GetUpdateInJson = Generic.MakeStateless (struct
  module Io = struct
    (* (name.arch, Pkg.t) list: installed RPM packages
     * Pkg.t * (string option * string): pkg, (update_id, repo)
     *)
    type input_t = (string * Pkg.t) list * (Pkg.t * (string option * string))

    type output_t = (Yojson.Basic.t, exn) result

    let string_of_input_t =
      Fmt.(
        str "%a"
          Dump.(
            pair
              (list (pair string (record @@ fields_of_pkg)))
              (pair (record @@ fields_of_pkg) (pair (option string) string))
          )
      )

    let string_of_output_t = function
      | Ok j ->
          Fmt.(str "%a" Dump.(string)) (Yojson.Basic.to_string j)
      | Error e ->
          Fmt.(str "%a" exn) e
  end

  let transform (installed_pkgs, (new_pkg, (update_id, repo))) =
    try Ok (get_update_in_json ~installed_pkgs (new_pkg, update_id, repo))
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        (* Not from expected repository *)
        ( ( [
              ( "xsconsole.x86_64"
              , Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "0.2.1"
                  ; release= "29.el7"
                  ; arch= "x86_64"
                  }
                
              )
            ; ( "libpath-utils.noarch"
              , Pkg.
                  {
                    name= "libpath-utils"
                  ; epoch= None
                  ; version= "0.2.1"
                  ; release= "29.el7"
                  ; arch= "noarch"
                  }
                
              )
            ]
          , ( Pkg.
                {
                  name= "libpath-utils"
                ; epoch= None
                ; version= "0.2.2"
                ; release= "1.el7"
                ; arch= "noarch"
                }
              
            , (None, "epel")
            )
            (* repository name is "epel" *)
          )
        , Error
            Api_errors.(
              Server_error
                (internal_error, ["Found update from unmanaged repository"])
            )
        )
      ; (* A normal case in which installed packages are not required *)
        ( ( [] (* No installed packages provided *)
          , ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "0.2.2"
                ; release= "9.el7"
                ; arch= "x86_64"
                }
              
            , (None, "local-regular")
            )
          )
        , Ok
            (`Assoc
              [
                ("name", `String "xsconsole")
              ; ("arch", `String "x86_64")
              ; ( "newEpochVerRel"
                , `Assoc
                    [
                      ("epoch", `String "(none)")
                    ; ("version", `String "0.2.2")
                    ; ("release", `String "9.el7")
                    ]
                )
              ; ("updateId", `Null)
              ; ("repository", `String "regular")
              ]
              )
        )
      ; (* A normal case *)
        ( ( [
              ( "xsconsole.x86_64"
              , Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "0.2.1"
                  ; release= "29.el7"
                  ; arch= "x86_64"
                  }
                
              )
            ; ( "libpath-utils.noarch"
              , Pkg.
                  {
                    name= "libpath-utils"
                  ; epoch= None
                  ; version= "0.2.1"
                  ; release= "29.el7"
                  ; arch= "noarch"
                  }
                
              )
            ]
          , ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "0.2.2"
                ; release= "9.el7"
                ; arch= "x86_64"
                }
              
            , (None, "local-regular")
            )
          )
        , Ok
            (`Assoc
              [
                ("name", `String "xsconsole")
              ; ("arch", `String "x86_64")
              ; ( "newEpochVerRel"
                , `Assoc
                    [
                      ("epoch", `String "(none)")
                    ; ("version", `String "0.2.2")
                    ; ("release", `String "9.el7")
                    ]
                )
              ; ("updateId", `Null)
              ; ("repository", `String "regular")
              ; ( "oldEpochVerRel"
                , `Assoc
                    [
                      ("epoch", `String "(none)")
                    ; ("version", `String "0.2.1")
                    ; ("release", `String "29.el7")
                    ]
                )
              ]
              )
        )
      ; (* A package with update ID *)
        ( ( [
              ( "xsconsole.x86_64"
              , Pkg.
                  {
                    name= "sconsole"
                  ; epoch= None
                  ; version= "0.2.1"
                  ; release= "29.el7"
                  ; arch= "x86_64"
                  }
                
              )
            ; ( "libpath-utils.noarch"
              , Pkg.
                  {
                    name= "libpath-utils"
                  ; epoch= None
                  ; version= "0.2.1"
                  ; release= "29.el7"
                  ; arch= "noarch"
                  }
                
              )
            ]
          , ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "0.2.2"
                ; release= "9.el7"
                ; arch= "x86_64"
                }
              
            , (Some "UPDATE-01", "local-regular")
            )
          )
        , Ok
            (`Assoc
              [
                ("name", `String "xsconsole")
              ; ("arch", `String "x86_64")
              ; ( "newEpochVerRel"
                , `Assoc
                    [
                      ("epoch", `String "(none)")
                    ; ("version", `String "0.2.2")
                    ; ("release", `String "9.el7")
                    ]
                )
              ; ("updateId", `String "UPDATE-01")
              ; ("repository", `String "regular")
              ; ( "oldEpochVerRel"
                , `Assoc
                    [
                      ("epoch", `String "(none)")
                    ; ("version", `String "0.2.1")
                    ; ("release", `String "29.el7")
                    ]
                )
              ]
              )
        )
      ; (* A normal case with epoch *)
        ( ( [
              ( "xsconsole.x86_64"
              , Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "0.2.1"
                  ; release= "29.el7"
                  ; arch= "x86_64"
                  }
                
              )
            ; ( "libpath-utils.noarch"
              , Pkg.
                  {
                    name= "libpath-utils"
                  ; epoch= Some 2
                  ; version= "0.2.1"
                  ; release= "29.el7"
                  ; arch= "noarch"
                  }
                
              )
            ]
          , ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= Some 1
                ; version= "0.1.1"
                ; release= "9.el7"
                ; arch= "x86_64"
                }
              
            , (None, "local-regular")
            )
          )
        , Ok
            (`Assoc
              [
                ("name", `String "xsconsole")
              ; ("arch", `String "x86_64")
              ; ( "newEpochVerRel"
                , `Assoc
                    [
                      ("epoch", `String "1")
                    ; ("version", `String "0.1.1")
                    ; ("release", `String "9.el7")
                    ]
                )
              ; ("updateId", `Null)
              ; ("repository", `String "regular")
              ; ( "oldEpochVerRel"
                , `Assoc
                    [
                      ("epoch", `String "(none)")
                    ; ("version", `String "0.2.1")
                    ; ("release", `String "29.el7")
                    ]
                )
              ]
              )
        )
      ]
end)

module ConsolidateUpdatesOfHost = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = Yojson.Basic.t * UpdateIdSet.t

    let string_of_input_t s = s

    let string_of_output_t (j, u) =
      Fmt.(str "%a" Dump.(string)) (Yojson.Basic.to_string j)
      ^ ", "
      ^ Fmt.(str "%a" Dump.(list string)) (UpdateIdSet.elements u)
  end

  let updateinfo =
    UpdateInfo.
      {
        id= ""
      ; summary= "summary"
      ; description= "description"
      ; rec_guidance= None
      ; abs_guidance= None
      ; guidance_applicabilities= []
      ; spec_info= "special info"
      ; url= "https://update.details.info"
      ; update_type= "security"
      }
    

  let updates_info =
    [
      ( "UPDATE-0000"
      , {
          updateinfo with
          id= "UPDATE-0000"
        ; rec_guidance= Some Guidance.EvacuateHost
        }
      )
    ; ( "UPDATE-0001"
      , {
          updateinfo with
          id= "UPDATE-0001"
        ; rec_guidance= Some Guidance.RebootHost
        }
      )
    ; ( "UPDATE-0002"
      , {
          updateinfo with
          id= "UPDATE-0002"
        ; rec_guidance= Some Guidance.RestartDeviceModel
        }
      )
    ; ( "UPDATE-0003"
      , {
          updateinfo with
          id= "UPDATE-0003"
        ; rec_guidance= Some Guidance.EvacuateHost
        }
      )
    ]

  let host = "string_of_host_ref"

  let transform updates =
    consolidate_updates_of_host ~repository_name:"regular" ~updates_info host
      (Yojson.Basic.from_string updates)

  let tests =
    `QuickAndAutoDocumented
      [
        ( (* No updates *)
          {|
            { "updates": [],
              "accumulative_updates": []
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ("recommended-guidance", `List [])
              ; ("absolute-guidance", `List [])
              ; ("RPMS", `List [])
              ; ("updates", `List [])
              ]
          , UpdateIdSet.empty
          )
        )
      ; (* Two updates come from two updateinfo *)
        ( {|
            {
              "updates":
              [
                {
                  "name": "xsconsole",
                  "arch": "x86_64",
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": null,
                  "repository": "regular"
                },
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": null,
                  "repository": "regular"
                }
              ],

              "accumulative_updates":
              [
                {
                  "name": "xsconsole",
                  "arch": "x86_64",
                  "oldEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0000",
                  "repository": "regular"
                },
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "oldEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0001",
                  "repository": "regular"
                }
              ]
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ("recommended-guidance", `List [`String "RebootHost"])
              ; ("absolute-guidance", `List [])
              ; ( "RPMS"
                , `List
                    [
                      `String "xsconsole-0.2.2-9.el7.x86_64.rpm"
                    ; `String "libpath-utils-0.2.2-9.el7.noarch.rpm"
                    ]
                )
              ; ("updates", `List [`String "UPDATE-0000"; `String "UPDATE-0001"])
              ]
          , UpdateIdSet.of_list ["UPDATE-0000"; "UPDATE-0001"]
          )
        )
      ; (* One update comes from one updateinfo *)
        ( {|
            {
              "updates":
              [
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": null,
                  "repository": "regular"
                }
              ],
              "accumulative_updates":
              [
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "oldEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0001",
                  "repository": "regular"
                }
              ]
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ("recommended-guidance", `List [`String "RebootHost"])
              ; ("absolute-guidance", `List [])
              ; ("RPMS", `List [`String "libpath-utils-0.2.2-9.el7.noarch.rpm"])
              ; ("updates", `List [`String "UPDATE-0001"])
              ]
          , UpdateIdSet.of_list ["UPDATE-0001"]
          )
        )
      ; (* One update, but no updateinfo *)
        ( {|
            {
              "updates":
              [
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": null,
                  "repository": "regular"
                }
              ],
              "accumulative_updates":
              [
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "oldEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0003",
                  "repository": "regular"
                }
              ]
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ("recommended-guidance", `List [`String "EvacuateHost"])
              ; ("absolute-guidance", `List [])
              ; ("RPMS", `List [`String "libpath-utils-0.2.2-9.el7.noarch.rpm"])
              ; ("updates", `List [`String "UPDATE-0003"])
              ]
          , UpdateIdSet.of_list ["UPDATE-0003"]
          )
        )
      ; (* Two updates: one from update, another from non-update *)
        ( {|
            {
              "updates":
              [
                {
                  "name": "xsconsole",
                  "arch": "x86_64",
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": null,
                  "repository": "base"
                },
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": null,
                  "repository": "regular"
                }
              ],
              "accumulative_updates":
              [
                {
                  "name": "xsconsole",
                  "arch": "x86_64",
                  "oldEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0000",
                  "repository": "base"
                },
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "oldEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0001",
                  "repository": "regular"
                }
              ]
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ("recommended-guidance", `List [`String "RebootHost"])
              ; ("absolute-guidance", `List [])
              ; ( "RPMS"
                , `List
                    [
                      `String "xsconsole-0.2.2-9.el7.x86_64.rpm"
                    ; `String "libpath-utils-0.2.2-9.el7.noarch.rpm"
                    ]
                )
              ; ("updates", `List [`String "UPDATE-0001"])
              ]
          , UpdateIdSet.of_list ["UPDATE-0001"]
          )
        )
      ; (* Two updates come from two updateinfo with epoch *)
        ( {|
            {
              "updates":
              [
                {
                  "name": "xsconsole",
                  "arch": "x86_64",
                  "newEpochVerRel": {
                    "epoch": "1",
                    "version": "0.1.2",
                    "release": "9.el7"
                  },
                  "updateId": null,
                  "repository": "regular"
                },
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "newEpochVerRel": {
                    "epoch": "2",
                    "version": "0.1.2",
                    "release": "9.el7"
                  },
                  "updateId": null,
                  "repository": "regular"
                }
              ],
              "accumulative_updates":
              [
                {
                  "name": "xsconsole",
                  "arch": "x86_64",
                  "oldEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "1",
                    "version": "0.1.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0000",
                  "repository": "regular"
                },
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "oldEpochVerRel": {
                    "epoch": "1",
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "2",
                    "version": "0.1.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0001",
                  "repository": "regular"
                }
              ]
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ("recommended-guidance", `List [`String "RebootHost"])
              ; ("absolute-guidance", `List [])
              ; ( "RPMS"
                , `List
                    [
                      `String "xsconsole-1:0.1.2-9.el7.x86_64.rpm"
                    ; `String "libpath-utils-2:0.1.2-9.el7.noarch.rpm"
                    ]
                )
              ; ("updates", `List [`String "UPDATE-0000"; `String "UPDATE-0001"])
              ]
          , UpdateIdSet.of_list ["UPDATE-0000"; "UPDATE-0001"]
          )
        )
      ; (* 2 updates, 4 accumulative_updates *)
        ( {|
            {
              "updates":
              [
                {
                  "name": "xsconsole",
                  "arch": "x86_64",
                  "newEpochVerRel": {
                    "epoch": "1",
                    "version": "0.1.2",
                    "release": "10.el7"
                  },
                  "updateId": null,
                  "repository": "regular"
                },
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "newEpochVerRel": {
                    "epoch": "2",
                    "version": "0.1.2",
                    "release": "9.el7"
                  },
                  "updateId": null,
                  "repository": "regular"
                }
              ],
              "accumulative_updates":
              [
                {
                  "name": "xsconsole",
                  "arch": "x86_64",
                  "oldEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "1",
                    "version": "0.1.1",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0001",
                  "repository": "regular"
                },
                {
                  "name": "xsconsole",
                  "arch": "x86_64",
                  "oldEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "1",
                    "version": "0.1.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0002",
                  "repository": "regular"
                },
                {
                  "name": "xsconsole",
                  "arch": "x86_64",
                  "oldEpochVerRel": {
                    "epoch": "(none)",
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "1",
                    "version": "0.1.2",
                    "release": "10.el7"
                  },
                  "updateId": "UPDATE-0003",
                  "repository": "regular"
                },
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "newEpochVerRel": {
                    "epoch": "2",
                    "version": "0.1.2",
                    "release": "9.el7"
                  },
                  "newEpochVerRel": {
                    "epoch": "2",
                    "version": "0.1.2",
                    "release": "9.el7"
                  },
                  "updateId": null,
                  "repository": "regular"
                }
              ]
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ("recommended-guidance", `List [`String "RebootHost"])
              ; ("absolute-guidance", `List [])
              ; ( "RPMS"
                , `List
                    [
                      `String "xsconsole-1:0.1.2-10.el7.x86_64.rpm"
                    ; `String "libpath-utils-2:0.1.2-9.el7.noarch.rpm"
                    ]
                )
              ; ( "updates"
                , `List
                    [
                      `String "UPDATE-0001"
                    ; `String "UPDATE-0002"
                    ; `String "UPDATE-0003"
                    ]
                )
              ]
          , UpdateIdSet.of_list ["UPDATE-0001"; "UPDATE-0002"; "UPDATE-0003"]
          )
        )
      ]
end)

module ParseUpdateInfoList = Generic.MakeStateless (struct
  module Io = struct
    type input_t = (Pkg.t * string) list * string

    type output_t = (Pkg.t * string) list

    let string_of_input_t =
      Fmt.(
        str "%a"
          Dump.(pair (list (pair (record @@ fields_of_pkg) string)) string)
      )

    let string_of_output_t =
      Fmt.(str "%a" Dump.(list (pair (record @@ fields_of_pkg) string)))
  end

  let transform (l, line) = parse_updateinfo_list l line

  let tests =
    `QuickAndAutoDocumented
      [
        ( ( [
              ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "0.2.2"
                  ; release= "7.el7"
                  ; arch= "noarch"
                  }
                
              , "UPDATE-0000"
              )
            ; ( Pkg.
                  {
                    name= "libpath-utils"
                  ; epoch= None
                  ; version= "0.2.2"
                  ; release= "7.el7"
                  ; arch= "noarch"
                  }
                
              , "UPDATE-0001"
              )
            ]
          , "UPDATE-0002 security xsconsole-0.2.2-7.el7.x86_64"
          )
        , [
            ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "0.2.2"
                ; release= "7.el7"
                ; arch= "x86_64"
                }
              
            , "UPDATE-0002"
            )
          ; ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "0.2.2"
                ; release= "7.el7"
                ; arch= "noarch"
                }
              
            , "UPDATE-0000"
            )
          ; ( Pkg.
                {
                  name= "libpath-utils"
                ; epoch= None
                ; version= "0.2.2"
                ; release= "7.el7"
                ; arch= "noarch"
                }
              
            , "UPDATE-0001"
            )
          ]
        )
      ; ( ( [
              ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "0.2.1"
                  ; release= "7.el7"
                  ; arch= "x86_64"
                  }
                
              , "UPDATE-0000"
              )
            ; ( Pkg.
                  {
                    name= "libpath-utils"
                  ; epoch= None
                  ; version= "0.2.2"
                  ; release= "7.el7"
                  ; arch= "noarch"
                  }
                
              , "UPDATE-0001"
              )
            ]
          , "UPDATE-0002 security xsconsole-0.2.2-7.el7.x86_64"
          )
        , [
            ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "0.2.2"
                ; release= "7.el7"
                ; arch= "x86_64"
                }
              
            , "UPDATE-0002"
            )
          ; ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "0.2.1"
                ; release= "7.el7"
                ; arch= "x86_64"
                }
              
            , "UPDATE-0000"
            )
          ; ( Pkg.
                {
                  name= "libpath-utils"
                ; epoch= None
                ; version= "0.2.2"
                ; release= "7.el7"
                ; arch= "noarch"
                }
              
            , "UPDATE-0001"
            )
          ]
        )
      ; ( ( [
              ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "0.2.1"
                  ; release= "7.el7"
                  ; arch= "x86_64"
                  }
                
              , "UPDATE-0000"
              )
            ; ( Pkg.
                  {
                    name= "libpath-utils"
                  ; epoch= None
                  ; version= "0.2.2"
                  ; release= "7.el7"
                  ; arch= "noarch"
                  }
                
              , "UPDATE-0001"
              )
            ]
          , "UPDATE-0002 security xsconsole-1:0.1.2-7.el7.x86_64"
          )
        , [
            ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= Some 1
                ; version= "0.1.2"
                ; release= "7.el7"
                ; arch= "x86_64"
                }
              
            , "UPDATE-0002"
            )
          ; ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "0.2.1"
                ; release= "7.el7"
                ; arch= "x86_64"
                }
              
            , "UPDATE-0000"
            )
          ; ( Pkg.
                {
                  name= "libpath-utils"
                ; epoch= None
                ; version= "0.2.2"
                ; release= "7.el7"
                ; arch= "noarch"
                }
              
            , "UPDATE-0001"
            )
          ]
        )
      ]
end)

module GuidanceSetResortGuidancesTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = Guidance.guidance_kind * Guidance.t list

    type output_t = Guidance.t list

    let string_of_input_t (kind, l) =
      let kind' =
        match kind with
        | Guidance.Recommended ->
            "Recommended"
        | Guidance.Absolute ->
            "Absolute"
      in
      kind'
      ^ ", "
      ^ Fmt.(str "%a" Dump.(list string)) (List.map Guidance.to_string l)

    let string_of_output_t l =
      Fmt.(str "%a" Dump.(list string)) (List.map Guidance.to_string l)
  end

  let transform (kind, guidances) =
    guidances
    |> GuidanceSet.of_list
    |> GuidanceSet.resort_guidances ~kind
    |> GuidanceSet.elements

  let tests =
    `QuickAndAutoDocumented
      [
        ((Guidance.Recommended, [Guidance.RebootHost]), [Guidance.RebootHost])
      ; ( (Guidance.Recommended, [Guidance.RebootHost; Guidance.RebootHost])
        , [Guidance.RebootHost]
        )
      ; ( ( Guidance.Recommended
          , [Guidance.RebootHost; Guidance.RestartDeviceModel]
          )
        , [Guidance.RebootHost]
        )
      ; ((Guidance.Absolute, [Guidance.EvacuateHost]), [])
      ; ( ( Guidance.Recommended
          , [Guidance.EvacuateHost; Guidance.RestartDeviceModel]
          )
        , [Guidance.EvacuateHost]
        )
      ]
end)

module PruneAccumulativeUpdates = Generic.MakeStateless (struct
  module Io = struct
    (* (pkg, update_id) list : accumulateive *)
    (* (pkg, repo) list : latest *)
    (* (name_arch, pkg) list : installed *)
    type input_t = (Pkg.t * string) list * (Pkg.t * string) list

    type output_t = (Pkg.t * string option * string) list

    let string_of_input_t =
      Fmt.(
        str "%a"
          Dump.(
            pair
              (list (pair (record @@ fields_of_pkg) string))
              (list (pair (record @@ fields_of_pkg) string))
          )
      )

    let string_of_output_t l =
      List.fold_left
        (fun acc (pkg, uid, repo) ->
          acc
          ^ "\n"
          ^ Fmt.(str "%a" Dump.(record @@ fields_of_pkg)) pkg
          ^ ", "
          ^ Fmt.(str "%a" Dump.(option string)) uid
          ^ ", "
          ^ repo
        )
        "" l
  end

  let installed_pkgs =
    [
      ( "xsconsole.x86_64"
      , Pkg.
          {
            name= "xsconsole"
          ; epoch= None
          ; version= "1.0.3"
          ; release= "1.0.0.xs8"
          ; arch= "x86_64"
          }
        
      )
    ; ( "libpath-utils.noarch"
      , Pkg.
          {
            name= "libpath-utils"
          ; epoch= None
          ; version= "1.0.3"
          ; release= "1.0.0.xs8"
          ; arch= "noarch"
          }
        
      )
    ; ( "qemu-dp.x86_64"
      , Pkg.
          {
            name= "qemu-dp"
          ; epoch= Some 2
          ; version= "2.12.0"
          ; release= "2.0.11.xs8"
          ; arch= "x86_64"
          }
        
      )
    ]

  let transform (accumulative_updates, latest_updates) =
    prune_accumulative_updates ~accumulative_updates ~latest_updates
      ~installed_pkgs

  let tests =
    `QuickAndAutoDocumented
      [
        ( (* all acc updates are older than or euqal to installed *)
          (* input *)
          ( (* accumulative updates*)
            [
              ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.0"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "UPDATE-00"
              )
            ; ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.3"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "UPDATE-03"
              )
            ]
          , (* latest updates *)
            [
              ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.4"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "regular"
              )
            ]
          )
        , (* output *)
          []
        )
      ; ( (* one acc update is newer than installed but older than the latest one *)
          (* input *)
          ( (* accumulative updates*)
            [
              ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.0"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "UPDATE-00"
              )
            ; ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.4"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "UPDATE-04"
              )
            ]
          , (* latest updates *)
            [
              ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.5"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "regular"
              )
            ]
          )
        , (* output *)
          [
            ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "1.0.4"
                ; release= "1.0.0.xs8"
                ; arch= "x86_64"
                }
              
            , Some "UPDATE-04"
            , "regular"
            )
          ]
        )
      ; ( (* two acc updates are newer than installed and one of them is equal to the latest one *)
          (* input *)
          ( (* accumulative updates*)
            [
              ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.4"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "UPDATE-04"
              )
            ; ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.5"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "UPDATE-05"
              )
            ]
          , (* latest updates *)
            [
              ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.5"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "regular"
              )
            ]
          )
        , (* output *)
          [
            ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "1.0.4"
                ; release= "1.0.0.xs8"
                ; arch= "x86_64"
                }
              
            , Some "UPDATE-04"
            , "regular"
            )
          ; ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "1.0.5"
                ; release= "1.0.0.xs8"
                ; arch= "x86_64"
                }
              
            , Some "UPDATE-05"
            , "regular"
            )
          ]
        )
      ; ( (* one acc update is equal to the latest one and another is newer that it *)
          (* input *)
          ( (* accumulative updates*)
            [
              ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.6"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "UPDATE-06"
              )
            ; ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.5"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "UPDATE-05"
              )
            ]
          , (* latest updates *)
            [
              ( Pkg.
                  {
                    name= "xsconsole"
                  ; epoch= None
                  ; version= "1.0.5"
                  ; release= "1.0.0.xs8"
                  ; arch= "x86_64"
                  }
                
              , "regular"
              )
            ]
          )
        , (* output *)
          [
            ( Pkg.
                {
                  name= "xsconsole"
                ; epoch= None
                ; version= "1.0.5"
                ; release= "1.0.0.xs8"
                ; arch= "x86_64"
                }
              
            , Some "UPDATE-05"
            , "regular"
            )
          ]
        )
      ]
end)

let tests =
  make_suite "repository_helpers_"
    [
      ("pkg_of_fullname", PkgOfFullnameTest.tests)
    ; ("update_of_json", UpdateOfJsonTest.tests)
    ; ("assert_valid_guidances", GuidanceSetAssertValidGuidanceTest.tests)
    ; ("pkg_compare_version_strings", PkgCompareVersionStringsTest.tests)
    ; ("applicability_eval", ApplicabilityEval.tests)
    ; ("updateinfo_metadata_of_xml", UpdateInfoMetaDataOfXml.tests)
    ; ("updateinfo_of_xml", UpdateInfoOfXml.tests)
    ; ("assert_url_is_valid", AssertUrlIsValid.tests)
    ; ("write_yum_config", WriteYumConfig.tests)
    ; ("eval_guidance_for_one_update", EvalGuidanceForOneUpdate.tests)
    ; ("get_update_in_json", GetUpdateInJson.tests)
    ; ("consolidate_updates_of_host", ConsolidateUpdatesOfHost.tests)
    ; ("parse_updateinfo_list", ParseUpdateInfoList.tests)
    ; ("resort_guidances", GuidanceSetResortGuidancesTest.tests)
    ; ("prune_accumulative_updates", PruneAccumulativeUpdates.tests)
    ]

let () = Alcotest.run "Repository Helpers" tests
