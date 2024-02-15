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
open Updateinfo

let fields_of_pkg =
  Fmt.Dump.
    [
      field "name" (fun (r : Pkg.t) -> r.name) string
    ; field "epoch" (fun (r : Pkg.t) -> Epoch.to_string r.epoch) string
    ; field "version" (fun (r : Pkg.t) -> r.version) string
    ; field "release" (fun (r : Pkg.t) -> r.release) string
    ; field "arch" (fun (r : Pkg.t) -> r.arch) string
    ]

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
    ; field "guidance"
        (fun (r : UpdateInfo.t) -> GuidanceInUpdateInfo.to_string r.guidance)
        string
    ; field "guidance_applicabilities"
        (fun (r : UpdateInfo.t) ->
          List.map Applicability.to_string r.guidance_applicabilities
        )
        (list string)
    ; field "spec_info" (fun (r : UpdateInfo.t) -> r.spec_info) string
    ; field "url" (fun (r : UpdateInfo.t) -> r.url) string
    ; field "update_type" (fun (r : UpdateInfo.t) -> r.update_type) string
    ; field "livepatches"
        (fun (r : UpdateInfo.t) ->
          List.map
            (fun x -> x |> LivePatch.to_json |> Yojson.Basic.pretty_to_string)
            r.livepatches
        )
        (list string)
    ; field "issued"
        (fun (r : UpdateInfo.t) -> Xapi_stdext_date.Date.to_string r.issued)
        string
    ; field "severity"
        (fun (r : UpdateInfo.t) -> Severity.to_string r.severity)
        string
    ; field "title" (fun (r : UpdateInfo.t) -> r.title) string
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
    let open Guidance in
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
                  ; guidance=
                      [
                        (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ; (Livepatch, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued= Xapi_stdext_date.Date.epoch
                  ; severity= Severity.None
                  ; title= "title"
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
                <issued date="2023-05-12 08:37:49"/>
                <severity>High</severity>
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
                  ; guidance=
                      [
                        (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ; (Livepatch, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued=
                      Xapi_stdext_date.Date.of_string "2023-05-12T08:37:49Z"
                  ; severity= Severity.High
                  ; title= "title"
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
                <issued date="2023-05-12 08:37:49"/>
                <severity>High</severity>
              </update>
              <update type="security">
                <id>UPDATE-0001</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
                <issued date="2023-05-12 08:37:50"/>
                <severity>None</severity>
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
                  ; guidance=
                      [
                        (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ; (Livepatch, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued=
                      Xapi_stdext_date.Date.of_string "2023-05-12T08:37:49Z"
                  ; severity= Severity.High
                  ; title= "title"
                  }
              )
            ; ( "UPDATE-0001"
              , UpdateInfo.
                  {
                    id= "UPDATE-0001"
                  ; summary= "summary"
                  ; description= "description"
                  ; guidance=
                      [
                        (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ; (Livepatch, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued=
                      Xapi_stdext_date.Date.of_string "2023-05-12T08:37:50Z"
                  ; severity= Severity.None
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* Single update with deprecated guidances only *)
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
                <issued date="2023-05-12 08:37:49"/>
                <severity>High</severity>
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
                  ; guidance=
                      [
                        (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ; (Livepatch, [])
                      ]
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
                  ; livepatches= []
                  ; issued=
                      Xapi_stdext_date.Date.of_string "2023-05-12T08:37:49Z"
                  ; severity= Severity.High
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* Single update with unknown guidance *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidance>NewGuidance</recommended_guidance>
                <absolute_guidance>NewGuidance</absolute_guidance>
                <guidance>
                  <mandatory>
                    <value>NewGuidance</value>
                  </mandatory>
                  <full>
                    <value>NewGuidance</value>
                    <value>RestartVM</value>
                  </full>
                  <recommended>
                    <value>NewGuidance</value>
                  </recommended>
                </guidance>
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
                <issued date="2023-05-12 08:37:49"/>
                <severity>High</severity>
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
                  ; guidance=
                      [
                        (Recommended, [RebootHost])
                      ; (Full, [RebootHost; RestartVM])
                      ; (Mandatory, [RebootHost])
                      ; (Livepatch, [])
                      ]
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
                  ; livepatches= []
                  ; issued=
                      Xapi_stdext_date.Date.of_string "2023-05-12T08:37:49Z"
                  ; severity= Severity.High
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* Single update with livepatches and livepatch guidance *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance>
                  <livepatch>
                    <value>RestartToolstack</value>
                  </livepatch>
                </guidance>
                <guidance_applicabilities/>
                <livepatch_guidance>RestartToolstack</livepatch_guidance>
                <livepatches>
                  <livepatch component="kernel" base="4.19.19-8.0.19.xs8" to="4.19.19-8.0.21.xs8" base-buildid="8346194f2e98a228f5a595b13ecabd43a99fada0"/>
                  <livepatch component="kernel" base="4.19.19-8.0.20.xs8" to="4.19.19-8.0.21.xs8" base-buildid="9346194f2e98a228f5a595b13ecabd43a99fada0"/>
                </livepatches>
                <issued date="2023-05-12 08:37:49"/>
                <severity>High</severity>
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
                  ; guidance=
                      [
                        (Livepatch, [RestartToolstack])
                      ; (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches=
                      [
                        LivePatch.
                          {
                            component= Kernel
                          ; base_build_id=
                              "8346194f2e98a228f5a595b13ecabd43a99fada0"
                          ; base_version= "4.19.19"
                          ; base_release= "8.0.19.xs8"
                          ; to_version= "4.19.19"
                          ; to_release= "8.0.21.xs8"
                          }
                      ; LivePatch.
                          {
                            component= Kernel
                          ; base_build_id=
                              "9346194f2e98a228f5a595b13ecabd43a99fada0"
                          ; base_version= "4.19.19"
                          ; base_release= "8.0.20.xs8"
                          ; to_version= "4.19.19"
                          ; to_release= "8.0.21.xs8"
                          }
                      ]
                  ; issued=
                      Xapi_stdext_date.Date.of_string "2023-05-12T08:37:49Z"
                  ; severity= Severity.High
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* Single update with livepatches and unknown livepatch guidance *)
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
                <guidance>
                  <livepatch>
                    <value>NewGuidance</value>
                  </livepatch>
                </guidance>
                <livepatch_guidance>NewGuidance</livepatch_guidance>
                <livepatches>
                  <livepatch component="kernel" base="4.19.19-8.0.19.xs8" to="4.19.19-8.0.21.xs8" base-buildid="8346194f2e98a228f5a595b13ecabd43a99fada0"/>
                  <livepatch component="kernel" base="4.19.19-8.0.20.xs8" to="4.19.19-8.0.21.xs8" base-buildid="9346194f2e98a228f5a595b13ecabd43a99fada0"/>
                </livepatches>
                <issued date="2023-05-12 08:37:49"/>
                <severity>High</severity>
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
                  ; guidance=
                      [
                        (Livepatch, [RebootHost])
                      ; (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches=
                      [
                        LivePatch.
                          {
                            component= Kernel
                          ; base_build_id=
                              "8346194f2e98a228f5a595b13ecabd43a99fada0"
                          ; base_version= "4.19.19"
                          ; base_release= "8.0.19.xs8"
                          ; to_version= "4.19.19"
                          ; to_release= "8.0.21.xs8"
                          }
                      ; LivePatch.
                          {
                            component= Kernel
                          ; base_build_id=
                              "9346194f2e98a228f5a595b13ecabd43a99fada0"
                          ; base_version= "4.19.19"
                          ; base_release= "8.0.20.xs8"
                          ; to_version= "4.19.19"
                          ; to_release= "8.0.21.xs8"
                          }
                      ]
                  ; issued=
                      Xapi_stdext_date.Date.of_string "2023-05-12T08:37:49Z"
                  ; severity= Severity.High
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* Single update with livepatch guidance but empty livepatch *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <special_info>special information</special_info>
                <guidance>
                  <livepatch>
                    <value>RestartDeviceModel</value>
                  </livepatch>
                </guidance>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
                <livepatch_guidance>RestartDeviceModel</livepatch_guidance>
                <livepatches>
                </livepatches>
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
                  ; guidance=
                      [
                        (Livepatch, [RestartDeviceModel])
                      ; (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued= Xapi_stdext_date.Date.epoch
                  ; severity= Severity.None
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* Single update with valid livepatches *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <guidance>
                  <livepatch>
                    <value>RestartToolstack</value>
                  </livepatch>
                </guidance>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
                <livepatch_guidance>RestartToolstack</livepatch_guidance>
                <livepatches>
                  <livepatch component="toolstack" base="4.19.19-8.0.20.xs8" to="4.19.19-8.0.21.xs8" base-buildid="9346194f2e98a228f5a595b13ecabd43a99fada0"/>
                  <livepatch component="kernel" base="4.19.19-8.0.20.xs8" to="4.19.19-8.0.21.xs8" base-buildid="9346194f2e98a228f5a595b13ecabd43a99fada0"/>
                </livepatches>
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
                  ; guidance=
                      [
                        (Livepatch, [RestartToolstack])
                      ; (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches=
                      [
                        LivePatch.
                          {
                            component= Kernel
                          ; base_build_id=
                              "9346194f2e98a228f5a595b13ecabd43a99fada0"
                          ; base_version= "4.19.19"
                          ; base_release= "8.0.20.xs8"
                          ; to_version= "4.19.19"
                          ; to_release= "8.0.21.xs8"
                          }
                      ]
                  ; issued= Xapi_stdext_date.Date.epoch
                  ; severity= Severity.None
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* Single update with invalid livepatches *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>description</description>
                <guidance>
                  <livepatch>
                    <value>RestartToolstack</value>
                  </livepatch>
                </guidance>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
                <livepatch_guidance>RestartToolstack</livepatch_guidance>
                <livepatches>
                  <livepatch component="xen" base="4.19.19-8.0.20.xs8" to="4.19.19-8.0.21.xs8" base-buildid=""/>
                  <livepatch component="kernel" base="4.19.19-8.0.20.xs8" base-buildid="9346194f2e98a228f5a595b13ecabd43a99fada0"/>
                </livepatches>
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
                  ; guidance=
                      [
                        (Livepatch, [RestartToolstack])
                      ; (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued= Xapi_stdext_date.Date.epoch
                  ; severity= Severity.None
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* guidance in new format: empty guidance *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>empty guidance</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidance>EvacuateHost</recommended_guidance>
                <absolute_guidance>RebootHost</absolute_guidance>
                <guidance_applicabilities/>
                <guidance/>
                <livepatch_guidance>RestartDeviceModel</livepatch_guidance>
                <livepatches>
                  <livepatch component="xen" base="4.19.19-8.0.20.xs8" to="4.19.19-8.0.21.xs8" base-buildid=""/>
                  <livepatch component="kernel" base="4.19.19-8.0.20.xs8" base-buildid="9346194f2e98a228f5a595b13ecabd43a99fada0"/>
                </livepatches>
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
                  ; description= "empty guidance"
                  ; guidance=
                      [
                        (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ; (Livepatch, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued= Xapi_stdext_date.Date.epoch
                  ; severity= Severity.None
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* guidance in new format only: empty guidance *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>guidance in new format only: empty guidance</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <guidance_applicabilities/>
                <guidance/>
                <livepatches>
                  <livepatch component="xen" base="4.19.19-8.0.20.xs8" to="4.19.19-8.0.21.xs8" base-buildid=""/>
                  <livepatch component="kernel" base="4.19.19-8.0.20.xs8" base-buildid="9346194f2e98a228f5a595b13ecabd43a99fada0"/>
                </livepatches>
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
                  ; description= "guidance in new format only: empty guidance"
                  ; guidance=
                      [
                        (Mandatory, [])
                      ; (Recommended, [])
                      ; (Full, [])
                      ; (Livepatch, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued= Xapi_stdext_date.Date.epoch
                  ; severity= Severity.None
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* guidance in new format: empty mandatory and full *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>empty mandatory and full</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidance>EvacuateHost</recommended_guidance>
                <absolute_guidance>RebootHost</absolute_guidance>
                <guidance_applicabilities/>
                <guidance>
                  <mandatory/>
                  <full>
                  </full>
                </guidance>
                <livepatch_guidance>RestartDeviceModel</livepatch_guidance>
                <livepatches>
                  <livepatch component="xen" base="4.19.19-8.0.20.xs8" to="4.19.19-8.0.21.xs8" base-buildid=""/>
                  <livepatch component="kernel" base="4.19.19-8.0.20.xs8" base-buildid="9346194f2e98a228f5a595b13ecabd43a99fada0"/>
                </livepatches>
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
                  ; description= "empty mandatory and full"
                  ; guidance=
                      [
                        (Full, [])
                      ; (Mandatory, [])
                      ; (Recommended, [])
                      ; (Livepatch, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued= Xapi_stdext_date.Date.epoch
                  ; severity= Severity.None
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* guidance in new format: mandatory only *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>mandatory only</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidance>EvacuateHost</recommended_guidance>
                <absolute_guidance>RebootHost</absolute_guidance>
                <guidance_applicabilities/>
                <guidance>
                  <mandatory>
                    <value>RestartDeviceModel</value>
                    <value>EvacuateHost</value>
                    <value>RestartToolstack</value>
                  </mandatory>
                </guidance>
                <livepatch_guidance>RestartDeviceModel</livepatch_guidance>
                <livepatches>
                  <livepatch component="xen" base="4.19.19-8.0.20.xs8" to="4.19.19-8.0.21.xs8" base-buildid=""/>
                  <livepatch component="kernel" base="4.19.19-8.0.20.xs8" base-buildid="9346194f2e98a228f5a595b13ecabd43a99fada0"/>
                </livepatches>
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
                  ; description= "mandatory only"
                  ; guidance=
                      [
                        ( Mandatory
                        , [RestartDeviceModel; EvacuateHost; RestartToolstack]
                        )
                      ; (Recommended, [])
                      ; (Full, [])
                      ; (Livepatch, [])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued= Xapi_stdext_date.Date.epoch
                  ; severity= Severity.None
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* guidance in new format: mandatory, recommended, full and livepatch *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>mandatory, recommended, full and livepatch</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidance>EvacuateHost</recommended_guidance>
                <absolute_guidance>RebootHost</absolute_guidance>
                <guidance_applicabilities/>
                <guidance>
                  <mandatory>
                    <value>RestartToolstack</value>
                  </mandatory>
                  <recommended>
                    <value>EvacuateHost</value>
                  </recommended>
                  <livepatch>
                    <value>RestartDeviceModel</value>
                  </livepatch>
                  <full>
                    <value>RebootHost</value>
                  </full>
                </guidance>
                <livepatch_guidance>RestartDeviceModel</livepatch_guidance>
                <livepatches>
                  <livepatch component="xen" base="4.19.19-8.0.20.xs8" to="4.19.19-8.0.21.xs8" base-buildid=""/>
                  <livepatch component="kernel" base="4.19.19-8.0.20.xs8" base-buildid="9346194f2e98a228f5a595b13ecabd43a99fada0"/>
                </livepatches>
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
                  ; description= "mandatory, recommended, full and livepatch"
                  ; guidance=
                      [
                        (Full, [RebootHost])
                      ; (Livepatch, [RestartDeviceModel])
                      ; (Recommended, [EvacuateHost])
                      ; (Mandatory, [RestartToolstack])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued= Xapi_stdext_date.Date.epoch
                  ; severity= Severity.None
                  ; title= "title"
                  }
              )
            ]
        )
      ; (* guidance in new format: mandatory, recommended, full and livepatch *)
        ( {|
            <updates>
              <update type="security">
                <id>UPDATE-0000</id>
                <title>title</title>
                <summary>summary</summary>
                <description>RestartVM in mandatory</description>
                <special_info>special information</special_info>
                <url>https://update.details.info</url>
                <recommended_guidance>EvacuateHost</recommended_guidance>
                <absolute_guidance>RebootHost</absolute_guidance>
                <guidance_applicabilities/>
                <guidance>
                  <mandatory>
                    <value>RestartVM</value>
                  </mandatory>
                  <recommended>
                    <value>EvacuateHost</value>
                  </recommended>
                  <livepatch>
                    <value>RestartDeviceModel</value>
                  </livepatch>
                  <full>
                    <value>RebootHost</value>
                  </full>
                </guidance>
                <livepatch_guidance>RestartDeviceModel</livepatch_guidance>
                <livepatches>
                  <livepatch component="xen" base="4.19.19-8.0.20.xs8" to="4.19.19-8.0.21.xs8" base-buildid=""/>
                  <livepatch component="kernel" base="4.19.19-8.0.20.xs8" base-buildid="9346194f2e98a228f5a595b13ecabd43a99fada0"/>
                </livepatches>
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
                  ; description= "RestartVM in mandatory"
                  ; guidance=
                      [
                        (Full, [RebootHost])
                      ; (Livepatch, [RestartDeviceModel])
                      ; (Recommended, [EvacuateHost])
                      ; (Mandatory, [RestartVM])
                      ]
                  ; guidance_applicabilities= []
                  ; spec_info= "special information"
                  ; url= "https://update.details.info"
                  ; update_type= "security"
                  ; livepatches= []
                  ; issued= Xapi_stdext_date.Date.epoch
                  ; severity= Severity.None
                  ; title= "title"
                  }
              )
            ]
        )
      ]
end)

let tests =
  make_suite "updateinfo_"
    [
      ("applicability_eval", ApplicabilityEval.tests)
    ; ("updateinfo_metadata_of_xml", UpdateInfoMetaDataOfXml.tests)
    ; ("updateinfo_of_xml", UpdateInfoOfXml.tests)
    ]

let () = Alcotest.run "Updateinfo" tests
