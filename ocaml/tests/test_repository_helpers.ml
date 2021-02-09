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

let fields_of_pkg = Fmt.Dump.([
    field "name" (fun (r:Pkg.t) -> r.name) string
  ; field "version" (fun (r:Pkg.t) -> r.version) string
  ; field "release" (fun (r:Pkg.t) -> r.release) string
  ; field "arch" (fun (r:Pkg.t) -> r.arch) string
  ])

module PkgOfFullnameTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = Line of string | FilePath of string

    type output_t = (Pkg.t option, exn) result

    let string_of_input_t = function
      | Line s -> "Line: " ^ s
      | FilePath p -> "File " ^ p

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(option @@ record @@ fields_of_pkg) ~error:exn))
  end

  exception Can_not_parse of string

  let transform input =
    try
      (match input with
       | Io.Line line ->
         Ok (Pkg.of_fullname line)
       | Io.FilePath p ->
         let rec for_each_line ic =
           match input_line ic with
           | line ->
             begin match Pkg.of_fullname line with
               | None
               | Some Pkg.{name=""; _}
               | Some Pkg.{version=""; _}
               | Some Pkg.{release=""; _}
               | Some Pkg.{arch=""; _} ->
                 raise (Can_not_parse line)
               | _ ->
                 for_each_line ic
             end
           | exception End_of_file -> ()
         in
         let in_ch = open_in p in
         (try
            for_each_line in_ch;
            close_in in_ch;
            Ok None
          with e ->
            close_in in_ch;
            raise e))
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        (Io.Line "libpath-utils-0.2.1-29.el7.x86",
         Ok None)
      ; (Io.Line "libpath-utils.x86_64",
         Error Api_errors.(Server_error (internal_error,
                                         [Pkg.error_msg "libpath-utils.x86_64"])))
      ; (Io.Line "libpath-utils-0.2.1-29.el7.noarch",
         Ok (Some Pkg.{name="libpath-utils"; version="0.2.1"; release="29.el7"; arch="noarch"}))
      ; (Io.Line "libpath-utils-0.2.1-29.el7.x86_64",
         Ok (Some Pkg.{name="libpath-utils"; version="0.2.1"; release="29.el7"; arch="x86_64"}))
      ; (* all RPM packages installed by default *)
        (Io.FilePath "test_data/repository_pkg_of_fullname_all",
         Ok None)
      ]
end)

let fields_of_update = Fmt.Dump.([
    field "name" (fun (r:Update.t) -> r.name) string
  ; field "arch" (fun (r:Update.t) -> r.arch) string
  ; field "old_version" (fun (r:Update.t) -> r.old_version) (option string)
  ; field "old_release" (fun (r:Update.t) -> r.old_release) (option string)
  ; field "new_version" (fun (r:Update.t) -> r.new_version) string
  ; field "new_release" (fun (r:Update.t) -> r.new_release) string
  ; field "update_id" (fun (r:Update.t) -> r.update_id) (option string)
  ])

module UpdateOfJsonTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = (Update.t, exn) result

    let string_of_input_t s =  s

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(record @@ fields_of_update) ~error:exn))
  end

  let transform input =
    try Ok (Update.of_json (Yojson.Basic.from_string input))
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [ (* A complete normal case *)
        ( {|
          {
            "name": "libpath-utils",
            "arch": "x86_64",
            "oldVerRel": {
              "version": "0.2.1",
              "release": "29.el7"
            },
            "newVerRel": {
              "version": "0.2.2",
              "release": "10.el7"
            },
            "updateId": "UPDATE-0000"
          }
          |},
          Ok Update.{
                name="libpath-utils"
              ; arch="x86_64"
              ; old_version=Some "0.2.1"
              ; old_release=Some "29.el7"
              ; new_version="0.2.2"
              ; new_release="10.el7"
              ; update_id=Some "UPDATE-0000"
              }
        )
      ; (* No old version, old release and updateId *)
        ( {|
          {
            "name": "libpath-utils",
            "arch": "x86_64",
            "newVerRel": {
              "version": "0.2.2",
              "release": "10.el7"
            }
          }
          |},
          Ok Update.{
                name="libpath-utils"
              ; arch="x86_64"
              ; old_version=None
              ; old_release=None
              ; new_version="0.2.2"
              ; new_release="10.el7"
              ; update_id=None
              }
        )
      ; (* Missing arch *)
        ( {|
          {
            "name": "libpath-utils",
            "oldVerRel": {
              "version": "0.2.1",
              "release": "29.el7"
            },
            "newVerRel": {
              "version": "0.2.2",
              "release": "10.el7"
            },
            "updateId": "UPDATE-0000"
          }
          |},
          Error Yojson.Basic.Util.(
              Type_error ("Expected string, got null", `Null))
        )
      ]
end)

module GuidanceAssertValidGuidanceTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = Guidance.t list

    type output_t = (unit, exn) result

    let string_of_input_t l =
      Fmt.(str "%a" Dump.(list string)) (List.map Guidance.to_string l)

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(any "()") ~error:exn))
  end

  let transform input =
    try Ok (Guidance.assert_valid_guidances input)
    with e -> Error e

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
      ; ([RestartDeviceModel; EvacuateHost], Ok ())
      ; ([EvacuateHost; RestartToolstack; RestartDeviceModel], Ok ())
      ; ([RebootHost; RestartToolstack],
         Error Api_errors.(Server_error (internal_error,
                                         [Guidance.error_msg [RebootHost; RestartToolstack]])))
      ; ([RebootHost; RestartDeviceModel],
         Error Api_errors.(Server_error (internal_error,
                                         [Guidance.error_msg [RebootHost; RestartDeviceModel]])))
      ; ([RebootHost; EvacuateHost],
         Error Api_errors.(Server_error (internal_error,
                                         [Guidance.error_msg [RebootHost; EvacuateHost]])))
      ]
end)

module ApplicabilityCompareVersionStringsTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string * string

    type output_t = string

    let string_of_input_t = Fmt.(str "%a" Dump.(pair string string))

    let string_of_output_t = Fmt.(str "%a" Dump.string)
  end

  let transform (s1, s2) =
    Applicability.string_of_order (Applicability.compare_version_strings s1 s2)

  let tests =
    `QuickAndAutoDocumented
      [
        (("1.2.3", "1.2.4"), "<")
      ; (("1.2.3", "1.2.3"), "=")
      ; (("1.2.3", "1.2"), ">")
      ; (("1.0011", "1.9"), ">")
      ; (("1.05", "1.5"),  "=")
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
    (*  ( (installed_version, installed_release) * (inequality * (version * release)) ) *)
    type input_t = (string * string) * (string * (string * string))

    type output_t = (bool, exn) result

    let string_of_input_t =
      Fmt.(str "%a" Dump.(pair (pair string string) (pair string (pair string string))))

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(bool) ~error:exn))
  end

  let transform ((v1, r1), (ineq, (v2, r2))) =
    try
      let applicability = Applicability.{
          name=""
        ; arch=""
        ; inequality=(Some (Applicability.inequality_of_string ineq))
        ; epoch=""
        ; version=v2
        ; release=r2
        }
      in
      Ok (Applicability.eval v1 r1 applicability)
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        ( ( ("1.2.3", "3.el7"), ("gt", ("1.2.3", "4.el7")) ), Ok false )
      ; ( ( ("1.2.3", "3.el7"), ("gt", ("1.2.4", "3.el7")) ), Ok false )
      ; ( ( ("1.2.3", "3.el7"), ("gt", ("1.2.3", "2.el7")) ), Ok true )
      ; ( ( ("1.2.3", "3.el7"), ("gt", ("1.2.2", "3.el7")) ), Ok true )

      ; ( ( ("1.2.3", "3.el7"), ("lt", ("1.2.3", "2.el7")) ), Ok false )
      ; ( ( ("1.2.3", "3.el7"), ("lt", ("1.2.2", "3.el7")) ), Ok false )
      ; ( ( ("1.2.3", "3.el7"), ("lt", ("1.2.3", "4.el7")) ), Ok true )
      ; ( ( ("1.2.3", "3.el7"), ("lt", ("1.2.4", "3.el7")) ), Ok true )

      ; ( ( ("1.2.3", "3.el7"), ("eq", ("1.2.3", "3.el7")) ), Ok true )
      ; ( ( ("1.2.3", "3.el7"), ("eq", ("1.2.4", "3.el7")) ), Ok false)

      ; ( ( ("1.2.3", "3.el7"), ("gte", ("1.2.3", "4.el7")) ), Ok false )
      ; ( ( ("1.2.3", "3.el7"), ("gte", ("1.2.4", "3.el7")) ), Ok false )
      ; ( ( ("1.2.3", "3.el7"), ("gte", ("1.2.3", "3.el7")) ), Ok true )
      ; ( ( ("1.2.3", "3.el7"), ("gte", ("1.2.3", "2.el7")) ), Ok true )
      ; ( ( ("1.2.3", "3.el7"), ("gte", ("1.2.2", "3.el7")) ), Ok true )

      ; ( ( ("1.2.3", "3.el7"), ("lte", ("1.2.3", "2.el7")) ), Ok false )
      ; ( ( ("1.2.3", "3.el7"), ("lte", ("1.2.2", "3.el7")) ), Ok false )
      ; ( ( ("1.2.3", "3.el7"), ("lte", ("1.2.3", "3.el7")) ), Ok true )
      ; ( ( ("1.2.3", "3.el7"), ("lte", ("1.2.3", "4.el7")) ), Ok true )
      ; ( ( ("1.2.3", "3.el7"), ("lte", ("1.2.4", "3.el7")) ), Ok true )

      ; ( ( ("1.2.3", "3.el7"), ("let", ("1.2.3", "3.el7")) ),
          Error Applicability.Invalid_inequality)
      ]
end)

module UpdateInfoMetaDataOfXml = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = (UpdateInfoMetaData.t, exn) result

    let string_of_input_t x = x

    let fields = Fmt.Dump.([
        field "checksum" (fun (r:UpdateInfoMetaData.t) -> r.checksum) string
      ; field "location" (fun (r:UpdateInfoMetaData.t) -> r.location) string
      ])

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(record @@ fields) ~error:exn))
  end

  let transform input =
    try Ok (UpdateInfoMetaData.of_xml (Xml.parse_string input))
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [ (* no data node *)
        ( {|
            <repomd>
            </repomd>
           |},
          Error Api_errors.(Server_error (invalid_repomd_xml, []))
        )

      ; (* no updateinfo node *)
        ( {|
            <repomd>
              <data type="primary"></data>
            </repomd>
           |},
          Error Api_errors.(Server_error (invalid_repomd_xml, []))
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
           |},
          Error Api_errors.(Server_error (invalid_repomd_xml, []))
        )

      ; (* missing checksum *)
        ( {|
            <repomd>
              <data type="updateinfo">
                <location href="repodata/123abc.xml.gz"/>
              </data>
            </repomd>
           |},
          Error Api_errors.(Server_error (invalid_repomd_xml, []))
        )

      ; (* missing location *)
        ( {|
            <repomd>
              <data type="updateinfo">
                <checksum type="sha256">123abc</checksum>
                <location href=""/>
              </data>
            </repomd>
           |},
          Error Api_errors.(Server_error (invalid_repomd_xml, []))
        )

      ; (* normal case *)
        ( {|
            <repomd>
              <data type="updateinfo">
                <checksum type="sha256">123abc</checksum>
                <location href="repodata/123abc.xml.gz"/>
              </data>
            </repomd>
           |},
          Ok (UpdateInfoMetaData.{checksum="123abc"; location="repodata/123abc.xml.gz"})
        )
      ]
end)

let fields_of_updateinfo = Fmt.Dump.([
    field "id" (fun (r:UpdateInfo.t) -> r.id) string
  ; field "summary" (fun (r:UpdateInfo.t) -> r.summary) string
  ; field "description" (fun (r:UpdateInfo.t) -> r.description) string
  ; field "rec_guidances" (fun (r:UpdateInfo.t) ->
      (List.map Guidance.to_string r.rec_guidances)) (list string)
  ; field "abs_guidances" (fun (r:UpdateInfo.t) ->
      (List.map Guidance.to_string r.abs_guidances)) (list string)
  ; field "guidance_applicabilities" (fun (r:UpdateInfo.t) ->
      (List.map Applicability.to_string r.guidance_applicabilities)) (list string)
  ; field "spec_info" (fun (r:UpdateInfo.t) -> r.spec_info) string
  ; field "url" (fun (r:UpdateInfo.t) -> r.url) string
  ; field "update_type" (fun (r:UpdateInfo.t) -> r.update_type) string
  ])

module UpdateInfoOfXml = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = ((string * UpdateInfo.t) list, exn) result

    let string_of_input_t s = s

    let string_of_output_t =
      Fmt.(str "%a" Dump.(
          result ~ok:(list (pair string (record @@ fields_of_updateinfo))) ~error:exn))
  end

  let transform input =
    try Ok (UpdateInfo.of_xml (Xml.parse_string input))
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [ (* No "updates" node *)
        (
          {|
            <pdates>
            </pdates>
          |},
          Error Api_errors.(Server_error (invalid_updateinfo_xml, []))
        )

      ; (* No update in updateinfo.xml *)
        (
          {|
            <updates>
            </updates>
          |},
          Ok []
        )

      ; (* Missing update_type *)
        (
          {|
            <updates>
              <update type="">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidances/>
                <absolute_guidances/>
                <guidance_applicabilities/>
              </update>
            </updates>
          |},
          Error Api_errors.(Server_error (invalid_updateinfo_xml, []))
        )

      ; (* Missing id *)
        (
          {|
            <updates>
              <update type="security">
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidances/>
                <absolute_guidances/>
                <guidance_applicabilities/>
              </update>
            </updates>
          |},
          Error Api_errors.(Server_error (invalid_updateinfo_xml, []))
        )

      ; (* Missing summary *)
        (
          {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidances/>
                <absolute_guidances/>
                <guidance_applicabilities/>
              </update>
            </updates>
          |},
          Error Api_errors.(Server_error (invalid_updateinfo_xml, [])))

      ; (* Missing description *)
        (
          {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidances/>
                <absolute_guidances/>
                <guidance_applicabilities/>
              </update>
            </updates>
          |},
          Error Api_errors.(Server_error (invalid_updateinfo_xml, []))
        )

      ; (* Duplicate update ID *)
        (
          {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidances/>
                <absolute_guidances/>
                <guidance_applicabilities/>
              </update>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidances/>
                <absolute_guidances/>
                <guidance_applicabilities/>
              </update>
            </updates>
          |},
          Error Api_errors.(Server_error (invalid_updateinfo_xml, []))
        )

      ; (* Single update *)
        (
          {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidances/>
                <absolute_guidances/>
                <guidance_applicabilities/>
              </update>
            </updates>
          |},
          Ok [ ("UPDATE-0000",
                UpdateInfo.{
                  id = "UPDATE-0000"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = []
                ; guidance_applicabilities = []
                ; spec_info = "special information"
                ; url = "https://update.details.info"
                ; update_type = "security"})
             ]
        )

      ; (* Two updates *)
        (
          {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidances/>
                <absolute_guidances/>
                <guidance_applicabilities/>
              </update>
              <update type="security">
                <id>UPDATE-0001</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidances/>
                <absolute_guidances/>
                <guidance_applicabilities/>
              </update>
            </updates>
          |},
          Ok [
              (
                "UPDATE-0000",
                UpdateInfo.{
                    id = "UPDATE-0000"
                  ; summary = "summary"
                  ; description = "description"
                  ; rec_guidances = []
                  ; abs_guidances = []
                  ; guidance_applicabilities = []
                  ; spec_info = "special information"
                  ; url = "https://update.details.info"
                  ; update_type = "security"
                  }
              )
            ; (
                "UPDATE-0001",
                UpdateInfo.{
                  id = "UPDATE-0001"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = []
                ; guidance_applicabilities = []
                ; spec_info = "special information"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ]
        )

      ; (* Single update with guidances *)
        (
          {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidances>
                  <guidance>RestartDeviceModel</guidance>
                </recommended_guidances>
                <absolute_guidances>
                  <guidance>RestartDeviceModel</guidance>
                  <guidance>EvacuateHost</guidance>
                </absolute_guidances>
                <guidance_applicabilities>
                  <applicability>
                    <name>xsconsole</name>
                    <inequality>gte</inequality>
                    <epoch>0</epoch>
                    <version>10.1.0</version>
                    <release>25</release>
                    <arch>x86_64</arch>
                  </applicability>
                  <applicability>
                    <name>xsconsole</name>
                    <inequality>lt</inequality>
                    <epoch>0</epoch>
                    <version>10.1.0</version>
                    <release>25</release>
                    <arch>x86_64</arch>
                  </applicability>
                </guidance_applicabilities>
              </update>
            </updates>
          |},
          Ok [ ( "UPDATE-0000",
                 UpdateInfo.{
                   id = "UPDATE-0000"
                 ; summary = "summary"
                 ; description = "description"
                 ; rec_guidances = [Guidance.of_string "RestartDeviceModel"]
                 ; abs_guidances = [
                     Guidance.of_string "RestartDeviceModel"
                   ; Guidance.of_string "EvacuateHost"
                   ]
                 ; guidance_applicabilities = [
                     Applicability.{
                       name = "xsconsole"
                     ; arch = "x86_64"
                     ; inequality = Some Gte
                     ; epoch = "0"
                     ; version = "10.1.0"
                     ; release = "25"
                     }
                   ; Applicability.{
                       name = "xsconsole"
                     ; arch = "x86_64"
                     ; inequality = Some Lt
                     ; epoch = "0"
                     ; version = "10.1.0"
                     ; release = "25"
                     }
                   ]
                 ; spec_info = "special information"
                 ; url = "https://update.details.info"
                 ; update_type = "security"
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
    Xapi_globs.repository_domain_name_allowlist := domain_name_allowlist;
    try Ok (assert_url_is_valid ~url)
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        (("htt://a.b.c", []),
         Error Api_errors.(Server_error (invalid_base_url, ["htt://a.b.c"])))
      ; (("http://a.b.c", ["c.com"; "d.com"]),
         Error Api_errors.(Server_error (invalid_base_url, ["http://a.b.c"])))
      ; (("https://a.b.c", []), Ok ())
      ; (("http://a.b.c", []), Ok ())
      ; (("http://a.b.c.com", ["c.com"; "d.com"]), Ok ())
      ; (("http://a.b.c.comm", ["c.com"; "d.com"]),
         Error Api_errors.(Server_error (invalid_base_url, ["http://a.b.c.comm"])))
      ; (("http://a.b...c.com", ["c.com"; "d.com"]), Ok ())
      ; (("http://a.b.cc.com", ["c.com"; "d.com"]),
         Error Api_errors.(Server_error (invalid_base_url, ["http://a.b.cc.com"])))
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
    (*           ( (source_url, binary_url),  (need_gpg_check, gpgkey_name) ) *)
    type input_t = (string option * string) * (bool * string option)

    type output_t = (string, exn) result

    let string_of_input_t =
      Fmt.(str "%a" Dump.(pair (pair (option string) string) (pair bool (option string))))

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(string) ~error:exn))
  end

  let repo_name = "unittest"

  let repo_suffix = ".repo"

  let gpgkey_name = "unittest.gpgkey"

  let tmp_dir = Filename.get_temp_dir_name ()

  let transform ((src_url, bin_url), (gpg_check, name)) =
    Xapi_globs.yum_repos_config_dir := tmp_dir;
    Xapi_globs.repository_gpgcheck := gpg_check;
    Xapi_globs.rpm_gpgkey_dir := tmp_dir;
    Xapi_globs.repository_gpgkey_name := gpgkey_name;
    (* Create empty gpgkey file if it is needed *)
    Option.iter (fun n ->
        Xapi_globs.repository_gpgkey_name := n;
        if n <> "" then close_out (open_out (Filename.concat tmp_dir n))
    ) name;
    let rec read_from_in_channel acc ic =
      match input_line ic with
      | line ->
        (read_from_in_channel[@tailcall]) (acc ^ line ^ "\n") ic
      | exception End_of_file -> acc
    in
    let repo_file_path = Filename.concat tmp_dir (repo_name ^ repo_suffix) in
    let finally () =
      try
        Unix.unlink repo_file_path;
        Option.iter (fun n ->
            if n <> "" then Unix.unlink (Filename.concat tmp_dir n)
        ) name
      with _ -> ()
    in
    try
      (* The path of file which will be written by write_yum_config *)
      write_yum_config ~source_url:src_url bin_url repo_name;
      let in_ch = open_in repo_file_path in
      let content = read_from_in_channel "" in_ch in
      close_in in_ch;
      finally ();
      Ok content
    with e ->
      finally ();
      Error e

  let url = "https://a.b.c/repository"

  let content1 = Printf.sprintf {|[%s]
name=%s
baseurl=%s
enabled=0
gpgcheck=1
gpgkey=file://%s
|} repo_name repo_name url (Filename.concat tmp_dir gpgkey_name)

  let content2 = Printf.sprintf {|[%s]
name=%s
baseurl=%s
enabled=0
gpgcheck=0
|} repo_name repo_name url

  let src_content1 = Printf.sprintf {|
[%s-source]
name=%s-source
baseurl=%s
enabled=0
gpgcheck=1
gpgkey=file://%s
|} repo_name repo_name url (Filename.concat tmp_dir gpgkey_name)

  let src_content2 = Printf.sprintf {|

[%s-source]
name=%s-source
baseurl=%s
enabled=0
gpgcheck=0
|} repo_name repo_name url

  let tests =
    `QuickAndAutoDocumented
      [
        (((None, url), (true, None)),
         Error Api_errors.(Server_error (internal_error, ["gpg key file does not exist"])))
      ; (((None, url), (true, Some "")),
         Error Api_errors.(Server_error (internal_error, ["gpg key file is not a regular file"])))
      ; (((None, url), (true, Some gpgkey_name)), Ok content1)
      ; (((None, url), (false, None)), Ok content2)
      ; (((None, url), (false, Some gpgkey_name)), Ok content2)
      ; (((Some url, url), (true, Some gpgkey_name)), Ok (content1 ^ src_content1))
      ; (((Some url, url), (false, None)), Ok (content2 ^ src_content2))
      ; (((Some url, url), (false, Some gpgkey_name)), Ok (content2 ^ src_content2))
      ]
end)

module EvalGuidanceForOneUpdate = Generic.MakeStateless (struct
  module Io = struct
    type input_t = ((string * UpdateInfo.t) list * Update.t)

    type output_t = GuidanceStrSet.t

    let string_of_input_t =
      Fmt.(str "%a" Dump.(
          pair
            ( list ( pair string (record @@ fields_of_updateinfo) ) )
            (record @@ fields_of_update)))

    let string_of_output_t s = Fmt.(str "%a" Dump.(list string)) (GuidanceStrSet.elements s)
  end

  let transform (updates_info, update) =
    eval_guidance_for_one_update ~updates_info ~update ~kind:Guidance.Absolute

  let tests =
    `QuickAndAutoDocumented
      [
        (* Update ID in update can't be found in updateinfo list *)
        ( ( [],
            Update.{
              (* No id here *)
              name = "xsconsole"
            ; arch = "x86_64"
            ; old_version = None
            ; old_release = None
            ; new_version = "0.2.2"
            ; new_release = "10.el7"
            ; update_id = (Some "UPDATE-0000")
            }
          ),
          GuidanceStrSet.empty)

      ; (* Update ID in update can't be found in updateinfo list *)
        ( ( [ ( "UPDATE-0000",
                UpdateInfo.{
                  id = "UPDATE-0000"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = [Guidance.EvacuateHost]
                ; abs_guidances = [Guidance.EvacuateHost]
                ; guidance_applicabilities = []
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ; ( "UPDATE-0001",
                UpdateInfo.{
                  id = "UPDATE-0001"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = [Guidance.EvacuateHost]
                ; abs_guidances = [Guidance.EvacuateHost]
                ; guidance_applicabilities = []
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ],
            Update.{
              name = "xsconsole"
            ; arch = "x86_64"
            ; old_version = (Some "0.2.1")
            ; old_release = (Some "29.el7")
            ; new_version = "0.2.2"
            ; new_release = "10.el7"
            ; update_id = (Some "UPDATE-0002") (* This ID can't be found in above *)
            }
          ),
          GuidanceStrSet.empty
        )

      ; (* No update ID in update *)
        ( ( [ ( "UPDATE-0000",
                UpdateInfo.{
                  id = "UPDATE-0000"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = [Guidance.EvacuateHost]
                ; abs_guidances = [Guidance.EvacuateHost]
                ; guidance_applicabilities = []
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ],
            Update.{
              name = "xsconsole"
            ; arch = "x86_64"
            ; old_version = (Some "0.2.1")
            ; old_release = (Some "29.el7")
            ; new_version = "0.2.2"
            ; new_release = "10.el7"
            ; update_id = None (* This is None *)
            }
          ),
          GuidanceStrSet.empty
        )

      ; (* Empty applicabilities *)
        ( ( [ ( "UPDATE-0000",
                UpdateInfo.{
                  id = "UPDATE-0000"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = []
                ; guidance_applicabilities = []
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ; ( "UPDATE-0001",
                UpdateInfo.{
                  id = "UPDATE-0001"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = [Guidance.EvacuateHost]
                ; guidance_applicabilities = [] (* No applicabilities *)
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ],
            Update.{
              name = "xsconsole"
            ; arch = "x86_64"
            ; old_version = (Some "0.2.1")
            ; old_release = (Some "29.el7")
            ; new_version = "0.2.2"
            ; new_release = "10.el7"
            ; update_id = (Some "UPDATE-0001")
            }
          ),
          (GuidanceStrSet.of_list ["EvacuateHost"])
        )

      ; (* Matched applicability *)
        ( ( [ ( "UPDATE-0000",
                UpdateInfo.{
                  id = "UPDATE-0000"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = []
                ; guidance_applicabilities = []
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ; ( "UPDATE-0001",
                UpdateInfo.{
                  id = "UPDATE-0001"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = [Guidance.EvacuateHost; Guidance.RestartDeviceModel]
                ; guidance_applicabilities = [
                    Applicability.{
                      name = "xsconsole"
                    ; arch = "x86_64"
                    ; inequality = Some Lte  (* old version 0.2.0 is less than 0.2.1 *)
                    ; epoch = "0"
                    ; version = "0.2.1"
                    ; release = "29.el7"
                    }
                  ]
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ],
            Update.{
              name = "xsconsole"
            ; arch = "x86_64"
            ; old_version = (Some "0.2.0")
            ; old_release = (Some "29.el7")
            ; new_version = "0.2.2"
            ; new_release = "10.el7"
            ; update_id = (Some "UPDATE-0001")
            }
          ),
          (GuidanceStrSet.of_list ["EvacuateHost"; "RestartDeviceModel"])
        )

      ; (* Matched in multiple applicabilities *)
        ( ( [ ( "UPDATE-0000",
                UpdateInfo.{
                  id = "UPDATE-0000"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = []
                ; guidance_applicabilities = []
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ; ( "UPDATE-0001",
                UpdateInfo.{
                  id = "UPDATE-0001"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = [Guidance.EvacuateHost; Guidance.RestartDeviceModel]
                ; guidance_applicabilities = [
                      Applicability.{
                        name = "xsconsole"
                      ; arch = "x86_64"
                      ; inequality = Some Gt  (* Unmatch: old version 0.2.1 is equal to 0.2.1 *)
                      ; epoch = "0"
                      ; version = "0.2.1"
                      ; release = "29.el7"
                      }
                    ; Applicability.{
                        name = "xsconsole"
                      ; arch = "x86_64"
                      ; inequality = Some Eq (* Match: old version 0.2.1 is equal to 0.2.1 *)
                      ; epoch = "0"
                      ; version = "0.2.1"
                      ; release = "29.el7"
                      }
                  ]
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ],
            Update.{
              name = "xsconsole"
            ; arch = "x86_64"
            ; old_version = (Some "0.2.1")
            ; old_release = (Some "29.el7")
            ; new_version = "0.2.2"
            ; new_release = "10.el7"
            ; update_id = (Some "UPDATE-0001")
            }
          ),
          (GuidanceStrSet.of_list ["EvacuateHost"; "RestartDeviceModel"])
        )

      ; (* No matched applicability *)
        ( ( [ ( "UPDATE-0000",
                UpdateInfo.{
                  id = "UPDATE-0000"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = []
                ; guidance_applicabilities = []
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ; ( "UPDATE-0001",
                UpdateInfo.{
                  id = "UPDATE-0001"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = [Guidance.EvacuateHost; Guidance.RestartDeviceModel]
                ; guidance_applicabilities = [
                    Applicability.{
                      name = "xsconsole"
                    ; arch = "x86_64"
                    ; inequality = Some Lte (* Unmatch: old version 0.2.1 is greater than 0.2.0 *)
                    ; epoch = "0"
                    ; version = "0.2.0"
                    ; release = "29.el7"
                    }
                  ]
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ],
            Update.{
              name = "xsconsole"
            ; arch = "x86_64"
            ; old_version = (Some "0.2.1")
            ; old_release = (Some "29.el7")
            ; new_version = "0.2.2"
            ; new_release = "10.el7"
            ; update_id = (Some "UPDATE-0001")
            }
          ),
          GuidanceStrSet.empty
        )

      ; (* Unmatched arch *)
        ( ( [ ( "UPDATE-0000",
                UpdateInfo.{
                  id = "UPDATE-0000"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = []
                ; guidance_applicabilities = []
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ; ( "UPDATE-0001",
                UpdateInfo.{
                  id = "UPDATE-0001"
                ; summary = "summary"
                ; description = "description"
                ; rec_guidances = []
                ; abs_guidances = [Guidance.EvacuateHost; Guidance.RestartDeviceModel]
                ; guidance_applicabilities = [
                    Applicability.{
                      name = "xsconsole"
                    ; arch = "x86_64" (* Unmatch: arch of update is x86_64 *)
                    ; inequality = Some Lte
                    ; epoch = "0"
                    ; version = "0.2.1"
                    ; release = "29.el7"
                    }
                  ]
                ; spec_info = "special info"
                ; url = "https://update.details.info"
                ; update_type = "security"
                }
              )
            ],
            Update.{
              name = "xsconsole"
            ; arch = "noarch"
            ; old_version = Some "0.2.1"
            ; old_release = Some "29.el7"
            ; new_version = "0.2.2"
            ; new_release = "10.el7"
            ; update_id = Some "UPDATE-0001"
            }
          ),
          GuidanceStrSet.empty
        )
      ]
end)

module GetRpmUpdateInJson = Generic.MakeStateless (struct
  module Io = struct
    (* (name.arch, updateId) list: from "yum updateinfo list updates"
     * (name.arch, Pkg.t) list): from "rpm -qa"
     * string: output line of "yum list updates" *)
    type input_t = ((string * string) list * (string * Pkg.t) list) * string

    type output_t = Yojson.Basic.t option

    let string_of_input_t =
      Fmt.(str "%a" Dump.(
          pair
            (pair
               ( list ( pair string string ) )
               ( list ( pair string (record @@ fields_of_pkg)) ))
            string
        ))

    let string_of_output_t j =
      Fmt.(str "%a" Dump.(option string)) (Option.map Yojson.Basic.to_string j)
  end

  let transform ((rpm2updates, installed_pkgs), line) =
    get_rpm_update_in_json ~rpm2updates ~installed_pkgs line

  let repo_name = !Xapi_globs.local_repo_name

  let tests =
    `QuickAndAutoDocumented
      [
        (* Not from expected repository *)
        ( ( ( [ ("xsconsole.x86_64", "UPDATE-0000")
              ; ("libpath-utils.noarch", "UPDATE-0001")
              ],
              [ ( "xsconsole.x86_64",
                  Pkg.{
                    name="libpath-utils"
                  ; version="0.2.1"
                  ; release="29.el7"
                  ; arch="noarch"
                  }
                )
              ; ( "libpath-utils.noarch",
                  Pkg.{
                    name="libpath-utils"
                  ; version="0.2.1"
                  ; release="29.el7"
                  ; arch="noarch"
                  }
                )
              ]
            ),
            "libpath-utils.noarch    0.2.2-1.el7      epel" (* repository name is "epel" *)
          ),
          None
        )

      ; (* A normal case in which installed packages are not required *)
        ( ( ( [ ("xsconsole.x86_64", "UPDATE-0000")
              ; ("libpath-utils.noarch", "UPDATE-0001")
              ],
              [] (* No installed packages provided *)
            ),
            "xsconsole.x86_64  0.2.2-9.el7    " ^ repo_name
          ),
          Some (
            `Assoc [
                ("name", `String "xsconsole")
              ; ("arch", `String "x86_64")
              ; ( "newVerRel",
                  `Assoc [
                    ("version", `String "0.2.2")
                  ; ("release", `String "9.el7")
                  ]
                )
              ; ("updateId", `String "UPDATE-0000")
              ])
        )

      ; (* A normal case *)
        ( ( ( [ ("xsconsole.x86_64", "UPDATE-0000")
              ; ("libpath-utils.noarch", "UPDATE-0001")
              ],
              [ ( "xsconsole.x86_64",
                  Pkg.{
                    name="libpath-utils"
                  ; version="0.2.1"
                  ; release="29.el7"
                  ; arch="noarch"
                  }
                )
              ; ( "libpath-utils.noarch",
                  Pkg.{
                    name="libpath-utils"
                  ; version="0.2.1"
                  ; release="29.el7"
                  ; arch="noarch"
                  }
                )
              ]
            ),
            "xsconsole.x86_64  0.2.2-9.el7    " ^ repo_name
          ),
          Some (
            `Assoc [
                ( "name", `String "xsconsole" )
              ; ( "arch", `String "x86_64" )
              ; ( "newVerRel",
                  `Assoc [
                    ("version", `String "0.2.2")
                  ; ("release", `String "9.el7")
                  ]
                )
              ; ( "updateId", `String "UPDATE-0000" )
              ; ( "oldVerRel",
                  `Assoc [
                    ("version", `String "0.2.1")
                  ; ("release", `String "29.el7")
                  ]
                )
              ])
        )

      ; (* A package to be updated is not in updateinfo.xml *)
        ( ( ( [ ("libpath-utils.noarch", "UPDATE-0001") ],
              [ ( "xsconsole.x86_64",
                  Pkg.{
                    name="libpath-utils"
                  ; version="0.2.1"
                  ; release="29.el7"
                  ; arch="noarch"
                  }
                )
              ; ( "libpath-utils.noarch",
                  Pkg.{
                    name="libpath-utils"
                  ; version="0.2.1"
                  ; release="29.el7"
                  ; arch="noarch"
                  }
                )
              ]
            ),
            "xsconsole.x86_64  0.2.2-9.el7    " ^ repo_name
          ),
          Some (
            `Assoc [
                ( "name", `String "xsconsole" )
              ; ( "arch", `String "x86_64" )
              ; ( "newVerRel",
                  `Assoc [
                    ("version", `String "0.2.2")
                  ; ("release", `String "9.el7")
                  ]
                )
              ; ( "updateId", `Null )
              ; ( "oldVerRel",
                  `Assoc [
                    ("version", `String "0.2.1")
                  ; ("release", `String "29.el7")
                  ]
                )
              ])
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
      ^ ", " ^
      Fmt.(str "%a" Dump.(list string)) (UpdateIdSet.elements u)
  end

  let updateinfo = UpdateInfo.{
      id = ""
    ; summary = "summary"
    ; description = "description"
    ; rec_guidances = []
    ; abs_guidances = []
    ; guidance_applicabilities = []
    ; spec_info = "special info"
    ; url = "https://update.details.info"
    ; update_type = "security"
    }

  let updates_info = [
      ( "UPDATE-0000", { updateinfo with id = "UPDATE-0000";
                                         rec_guidances = [Guidance.EvacuateHost] } )
    ; ( "UPDATE-0001", { updateinfo with id = "UPDATE-0001";
                                         rec_guidances = [] } )
  ]

  let host = "string_of_host_ref"

  let transform updates =
    consolidate_updates_of_host ~updates_info host (Yojson.Basic.from_string updates)

  let tests =
    `QuickAndAutoDocumented
      [
        ( (* No updates *)
          {|
            { "updates": [] }
          |},
          ( `Assoc [
                ("ref", `String host)
              ; ("recommended-guidance", `List [])
              ; ("absolute-guidance", `List [])
              ; ("RPMS", `List [])
              ; ("updates", `List [])
              ],
            UpdateIdSet.empty
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
                  "oldVerRel": {
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newVerRel": {
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0000"
                },
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "oldVerRel": {
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newVerRel": {
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0001"
                }
              ]
            }
          |},
          ( `Assoc [
                ("ref", `String host)
              ; ("recommended-guidance", `List [`String "EvacuateHost"])
              ; ("absolute-guidance", `List [])
              ; ("RPMS", `List [`String "libpath-utils-0.2.2-9.el7.noarch.rpm";
                                `String "xsconsole-0.2.2-9.el7.x86_64.rpm"])
              ; ("updates", `List [`String "UPDATE-0000"; `String "UPDATE-0001"])
              ],
            (UpdateIdSet.of_list ["UPDATE-0000"; "UPDATE-0001"])
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
                  "oldVerRel": {
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newVerRel": {
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0001"
                }
              ]
            }
          |},
          ( `Assoc [
                ("ref", `String host)
              ; ("recommended-guidance", `List [])
              ; ("absolute-guidance", `List [])
              ; ("RPMS", `List [`String "libpath-utils-0.2.2-9.el7.noarch.rpm"])
              ; ("updates", `List [`String "UPDATE-0001"])
              ],
            (UpdateIdSet.of_list ["UPDATE-0001"])
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
                  "oldVerRel": {
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newVerRel": {
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": "UPDATE-0003"
                }
              ]
            }
          |},
          ( `Assoc [
                ("ref", `String host)
              ; ("recommended-guidance", `List [])
              ; ("absolute-guidance", `List [])
              ; ("RPMS", `List [`String "libpath-utils-0.2.2-9.el7.noarch.rpm"])
              ; ("updates", `List [`String "UPDATE-0003"])
              ],
            (UpdateIdSet.of_list ["UPDATE-0003"])
          )
        )

      ; (* One update, but no updateID *)
        ( {|
            {
              "updates":
              [
                {
                  "name": "libpath-utils",
                  "arch": "noarch",
                  "oldVerRel": {
                    "version": "0.2.1",
                    "release": "29.el7"
                  },
                  "newVerRel": {
                    "version": "0.2.2",
                    "release": "9.el7"
                  },
                  "updateId": null
                }
              ]
            }
          |},
          ( `Assoc [
                ("ref", `String host)
              ; ("recommended-guidance", `List [])
              ; ("absolute-guidance", `List [])
              ; ("RPMS", `List [`String "libpath-utils-0.2.2-9.el7.noarch.rpm"])
              ; ("updates", `List [])
              ],
            UpdateIdSet.empty
          )
        )
      ]
end)

module ParseUpdateInfoList = Generic.MakeStateless (struct
  module Io = struct
    type input_t = (string * (string * string * string)) list * string

    type output_t = (string * (string * string * string)) list

    let string_of_input_t =
      Test_printers.(
        pair
          ( list ( pair string (tuple3 string string string) ) )
          string
      )

    let string_of_output_t =
      Test_printers.( list (pair string (tuple3 string string string)) )
  end

  let transform (l, line) =
    parse_updateinfo_list l line

  let tests =
    `QuickAndAutoDocumented
      [
        ( ( [ ("xsconsole.noarch", ("0.2.2", "7.el7", "UPDATE-0000"))
            ; ("libpath-utils.noarch", ("0.2.2", "7.el7", "UPDATE-0001"))
            ],
            "UPDATE-0002 security xsconsole-0.2.2-7.el7.x86_64"
          ),
          [ ("xsconsole.x86_64", ("0.2.2", "7.el7", "UPDATE-0002"))
          ; ("xsconsole.noarch", ("0.2.2", "7.el7", "UPDATE-0000"))
          ; ("libpath-utils.noarch", ("0.2.2", "7.el7", "UPDATE-0001"))
          ]
        )

      ; ( ( [ ("xsconsole.x86_64", ("0.2.1", "7.el7", "UPDATE-0000"))
            ; ("libpath-utils.noarch", ("0.2.2", "7.el7", "UPDATE-0001"))
            ],
            "UPDATE-0002 security xsconsole-0.2.2-7.el7.x86_64"
          ),
          [ ("xsconsole.x86_64", ("0.2.2", "7.el7", "UPDATE-0002"))
          ; ("libpath-utils.noarch", ("0.2.2", "7.el7", "UPDATE-0001"))
          ]
        )
      ]
end)

let tests =
  make_suite "repository_helpers_"
    [
      ("pkg_of_fullname", PkgOfFullnameTest.tests)
    ; ("update_of_json", UpdateOfJsonTest.tests)
    ; ("guidance_assert_valid_guidances", GuidanceAssertValidGuidanceTest.tests)
    ; ("applicability_compare_version_strings", ApplicabilityCompareVersionStringsTest.tests)
    ; ("applicability_eval", ApplicabilityEval.tests)
    ; ("updateinfo_metadata_of_xml", UpdateInfoMetaDataOfXml.tests)
    ; ("updateinfo_of_xml", UpdateInfoOfXml.tests)
    ; ("assert_url_is_valid", AssertUrlIsValid.tests)
    ; ("write_yum_config", WriteYumConfig.tests)
    ; ("eval_guidance_for_one_update", EvalGuidanceForOneUpdate.tests)
    ; ("get_rpm_update_in_json", GetRpmUpdateInJson.tests)
    ; ("consolidate_updates_of_host", ConsolidateUpdatesOfHost.tests)
    ; ("parse_updateinfo_list", ParseUpdateInfoList.tests)
    ]
