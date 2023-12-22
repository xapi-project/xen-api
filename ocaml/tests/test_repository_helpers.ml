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
      ; (("http://192.168.0.2", []), Ok ())
      ; (("https://192.168.0.2", []), Ok ())
      ; (("http://192.168.0.2", ["192.168.0.2"]), Ok ())
      ; (("https://192.168.0.2", ["192.168.0.2"]), Ok ())
      ; ( ("http://192.168.0.256", ["192.168.0.256"])
        , Error
            Api_errors.(
              Server_error (invalid_base_url, ["http://192.168.0.256"])
            )
        )
      ; (("http://c.com", ["c.com"]), Ok ())
      ; ( ("http://c.com", ["c.com"; "d.."; ".e.."])
        , Error
            Api_errors.(
              Server_error (invalid_repository_domain_allowlist, ["d.."; ".e.."])
            )
        )
      ; (("http://a.b.c.com", ["c.com"; "d.com"]), Ok ())
      ; ( ("http://a.b.c.comm", ["c.com"; "d.com"])
        , Error
            Api_errors.(Server_error (invalid_base_url, ["http://a.b.c.comm"]))
        )
      ; ( ("http://a.b...c.com", ["c.com"; "d.com"])
        , Error
            Api_errors.(Server_error (invalid_base_url, ["http://a.b...c.com"]))
        )
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
    type input_t = {
        updates_info: (string * UpdateInfo.t) list
      ; update: Update.t
      ; upd_ids_of_livepatches: string list
      ; kind: Guidance.kind
    }

    type output_t = Guidance.t list

    let fields_of_input =
      Fmt.Dump.
        [
          field "updates_info"
            (fun (r : input_t) ->
              List.map
                (fun (upd_id, upd_info) ->
                  Printf.sprintf "(%s, %s)" upd_id
                    (UpdateInfo.to_json upd_info
                    |> Yojson.Basic.pretty_to_string
                    )
                )
                r.updates_info
            )
            (list string)
        ; field "update" (fun (r : input_t) -> Update.to_string r.update) string
        ; field "upd_ids_of_livepatches"
            (fun (r : input_t) ->
              r.upd_ids_of_livepatches
              |> String.concat ";"
              |> Printf.sprintf "[%s]"
            )
            string
        ; field "kind"
            (fun (r : input_t) -> Guidance.kind_to_string r.kind)
            string
        ]

    let string_of_input_t = Fmt.(str "%a" Dump.(record @@ fields_of_input))

    let string_of_output_t l =
      Fmt.(str "%a" Dump.(list string)) (List.map Guidance.to_string l)
  end

  let transform Io.{updates_info; update; upd_ids_of_livepatches; kind} =
    eval_guidance_for_one_update ~updates_info ~update ~kind
      ~upd_ids_of_livepatches:(UpdateIdSet.of_list upd_ids_of_livepatches)

  let tests =
    let open Io in
    let open Guidance in
    `QuickAndAutoDocumented
      [
        (* Update ID in update can't be found in updateinfo list *)
        ( {
            updates_info= []
          ; update=
              Update.
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
          ; upd_ids_of_livepatches= []
          ; kind= Mandatory
          }
        , []
        )
      ; (* Update ID in update can't be found in updateinfo list *)
        ( {
            updates_info=
              [
                ( "UPDATE-0000"
                , UpdateInfo.
                    {
                      id= "UPDATE-0000"
                    ; summary= "summary"
                    ; description= "description"
                    ; guidance=
                        [
                          (Mandatory, [EvacuateHost])
                        ; (Recommended, [])
                        ; (Full, [RebootHost])
                        ; (Livepatch, [])
                        ]
                    ; guidance_applicabilities= []
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
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
                          (Mandatory, [EvacuateHost])
                        ; (Recommended, [])
                        ; (Full, [RebootHost])
                        ; (Livepatch, [])
                        ]
                    ; guidance_applicabilities= []
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
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
          ; upd_ids_of_livepatches= []
          ; kind= Mandatory
          }
        , []
        )
      ; (* No update ID in update *)
        ( {
            updates_info=
              [
                ( "UPDATE-0000"
                , UpdateInfo.
                    {
                      id= "UPDATE-0000"
                    ; summary= "summary"
                    ; description= "description"
                    ; guidance=
                        [
                          (Mandatory, [EvacuateHost])
                        ; (Recommended, [])
                        ; (Full, [RebootHost])
                        ; (Livepatch, [])
                        ]
                    ; guidance_applicabilities= []
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
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
          ; upd_ids_of_livepatches= []
          ; kind= Recommended
          }
        , []
        )
      ; (* Empty applicabilities *)
        ( {
            updates_info=
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
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
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
                        ; (Recommended, [RebootHost])
                        ; (Full, [])
                        ; (Livepatch, [])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
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
          ; upd_ids_of_livepatches= []
          ; kind= Recommended
          }
        , [RebootHost]
        )
      ; (* Matched applicability *)
        ( {
            updates_info=
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
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
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
                          (Mandatory, [RestartDeviceModel])
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
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
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
          ; upd_ids_of_livepatches= []
          ; kind= Mandatory
          }
        , [RestartDeviceModel]
        )
      ; (* Matched in multiple applicabilities *)
        ( {
            updates_info=
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
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
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
                          (Mandatory, [EvacuateHost])
                        ; (Recommended, [RestartDeviceModel])
                        ; (Full, [])
                        ; (Livepatch, [])
                        ]
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
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
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
          ; upd_ids_of_livepatches= []
          ; kind= Recommended
          }
        , [RestartDeviceModel]
        )
      ; (* No matched applicability *)
        ( {
            updates_info=
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
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
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
                          (Mandatory, [RestartDeviceModel])
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
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
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
          ; upd_ids_of_livepatches= []
          ; kind= Mandatory
          }
        , []
        )
      ; (* Unmatched arch *)
        ( {
            updates_info=
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
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
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
                          (Mandatory, [RestartDeviceModel])
                        ; (Recommended, [])
                        ; (Full, [])
                        ; (Livepatch, [])
                        ]
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
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
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
          ; upd_ids_of_livepatches= []
          ; kind= Mandatory
          }
        , []
        )
      ; (* Matched in multiple applicabilities with epoch *)
        ( {
            updates_info=
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
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
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
                        ; (Recommended, [RestartDeviceModel; RestartToolstack])
                        ; (Full, [])
                        ; (Livepatch, [])
                        ]
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
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
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
          ; upd_ids_of_livepatches= []
          ; kind= Recommended
          }
        , [RestartDeviceModel; RestartToolstack]
        )
      ; (* livepatch_guidance *)
        ( {
            updates_info=
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
                        ; (Recommended, [RebootHost])
                        ; (Full, [])
                        ; (Livepatch, [RestartDeviceModel; RestartToolstack])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches=
                        [
                          LivePatch.
                            {
                              component= Livepatch.Xen
                            ; base_build_id=
                                "9346194f2e98a228f5a595b13ecabd43a99fada0"
                            ; base_version= "4.13.4"
                            ; base_release= "10.22.xs8"
                            ; to_version= "4.13.4"
                            ; to_release= "10.23.xs8"
                            }
                        ; LivePatch.
                            {
                              component= Livepatch.Xen
                            ; base_build_id=
                                "8346194f2e98a228f5a595b13ecabd43a99fada0"
                            ; base_version= "4.13.4"
                            ; base_release= "10.21.xs8"
                            ; to_version= "4.13.4"
                            ; to_release= "10.23.xs8"
                            }
                        ]
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
                {
                  name= "xen-hypervisor"
                ; arch= "x86_64"
                ; old_epoch= Some None
                ; old_version= Some "4.13.4"
                ; old_release= Some "10.22.xs8"
                ; new_epoch= None
                ; new_version= "4.13.4"
                ; new_release= "10.23.xs8"
                ; update_id= Some "UPDATE-0000"
                ; repository= "regular"
                }
          ; upd_ids_of_livepatches= ["UPDATE-0000"]
          ; kind= Recommended
          }
        , [RestartDeviceModel; RestartToolstack]
        )
      ; (* livepatch_guidance - empty *)
        ( {
            updates_info=
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
                        ; (Recommended, [RebootHost])
                        ; (Full, [])
                        ; (Livepatch, [])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches=
                        [
                          LivePatch.
                            {
                              component= Livepatch.Xen
                            ; base_build_id=
                                "9346194f2e98a228f5a595b13ecabd43a99fada0"
                            ; base_version= "4.13.4"
                            ; base_release= "10.22.xs8"
                            ; to_version= "4.13.4"
                            ; to_release= "10.23.xs8"
                            }
                        ]
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
                {
                  name= "xen-hypervisor"
                ; arch= "x86_64"
                ; old_epoch= Some None
                ; old_version= Some "4.13.4"
                ; old_release= Some "10.22.xs8"
                ; new_epoch= None
                ; new_version= "4.13.4"
                ; new_release= "10.23.xs8"
                ; update_id= Some "UPDATE-0000"
                ; repository= "regular"
                }
          ; upd_ids_of_livepatches= ["UPDATE-0000"]
          ; kind= Recommended
          }
        , []
        )
      ; (* livepatch_guidance: livepatch does not come from RPM update UPDATE-0001.
         * And the RPM update UPDATE-0001 requires RebootHost.
         *)
        ( {
            updates_info=
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
                        ; (Livepatch, [RestartDeviceModel])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches=
                        [
                          LivePatch.
                            {
                              component= Livepatch.Xen
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
                    ; title= ""
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
                        ; (Recommended, [RebootHost])
                        ; (Full, [])
                        ; (Livepatch, [RestartToolstack])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches=
                        [
                          LivePatch.
                            {
                              component= Livepatch.Xen
                            ; base_build_id=
                                "8346194f2e98a228f5a595b13ecabd43a99fada0"
                            ; base_version= "4.19.19"
                            ; base_release= "8.0.21.xs8"
                            ; to_version= "4.19.19"
                            ; to_release= "8.0.22.xs8"
                            }
                        ]
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
                {
                  name= "xen-hypervisor"
                ; arch= "x86_64"
                ; old_epoch= Some None
                ; old_version= Some "4.19.19"
                ; old_release= Some "8.0.20.xs8"
                ; new_epoch= None
                ; new_version= "4.19.19"
                ; new_release= "8.0.22.xs8"
                ; update_id= Some "UPDATE-0001"
                ; repository= "regular"
                }
          ; upd_ids_of_livepatches= ["UPDATE-0000"]
          ; kind= Recommended
          }
        , [RebootHost]
        )
      ; (* livepatch_guidance: livepatch comes from the RPM update UPDATE-001 *)
        ( {
            updates_info=
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
                        ; (Recommended, [RebootHost])
                        ; (Full, [])
                        ; (Livepatch, [RestartDeviceModel])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches=
                        [
                          LivePatch.
                            {
                              component= Livepatch.Xen
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
                    ; title= ""
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
                        ; (Recommended, [RebootHost])
                        ; (Full, [])
                        ; (Livepatch, [RestartToolstack])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches=
                        [
                          LivePatch.
                            {
                              component= Livepatch.Xen
                            ; base_build_id=
                                "8346194f2e98a228f5a595b13ecabd43a99fada0"
                            ; base_version= "4.19.19"
                            ; base_release= "8.0.21.xs8"
                            ; to_version= "4.19.19"
                            ; to_release= "8.0.22.xs8"
                            }
                        ; LivePatch.
                            {
                              component= Livepatch.Xen
                            ; base_build_id=
                                "9346194f2e98a228f5a595b13ecabd43a99fada0"
                            ; base_version= "4.19.19"
                            ; base_release= "8.0.20.xs8"
                            ; to_version= "4.19.19"
                            ; to_release= "8.0.22.xs8"
                            }
                        ]
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
                {
                  name= "xen-hypervisor"
                ; arch= "x86_64"
                ; old_epoch= Some None
                ; old_version= Some "4.19.19"
                ; old_release= Some "8.0.20.xs8"
                ; new_epoch= None
                ; new_version= "4.19.19"
                ; new_release= "8.0.22.xs8"
                ; update_id= Some "UPDATE-0001"
                ; repository= "regular"
                }
          ; upd_ids_of_livepatches= ["UPDATE-0001"]
          ; kind= Recommended
          }
        , [RestartToolstack]
        )
      ; (* livepatch_guidance: latest update doesn't have livepatch and recommended is empty *)
        ( {
            updates_info=
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
                        ; (Livepatch, [RestartDeviceModel])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches=
                        [
                          LivePatch.
                            {
                              component= Livepatch.Xen
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
                    ; title= ""
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
                        ; (Recommended, [RestartToolstack])
                        ; (Full, [])
                        ; (Livepatch, [])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
                {
                  name= "xen-hypervisor"
                ; arch= "x86_64"
                ; old_epoch= Some None
                ; old_version= Some "4.19.19"
                ; old_release= Some "8.0.20.xs8"
                ; new_epoch= None
                ; new_version= "4.19.19"
                ; new_release= "8.0.22.xs8"
                ; update_id= Some "UPDATE-0001"
                ; repository= "regular"
                }
          ; upd_ids_of_livepatches= ["UPDATE-0000"]
          ; kind= Recommended
          }
        , [RestartToolstack]
        )
      ; (* livepatch_guidance: latest update doesn't have livepatch but recommended is RebootHost *)
        ( {
            updates_info=
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
                        ; (Livepatch, [RestartToolstack])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches=
                        [
                          LivePatch.
                            {
                              component= Livepatch.Xen
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
                    ; title= ""
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
                        ; (Recommended, [RebootHost])
                        ; (Full, [])
                        ; (Livepatch, [])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
                {
                  name= "xen-hypervisor"
                ; arch= "x86_64"
                ; old_epoch= Some None
                ; old_version= Some "4.19.19"
                ; old_release= Some "10.20.xs8"
                ; new_epoch= None
                ; new_version= "4.19.19"
                ; new_release= "8.0.22.xs8"
                ; update_id= Some "UPDATE-0001"
                ; repository= "regular"
                }
          ; upd_ids_of_livepatches= ["UPDATE-0000"]
          ; kind= Recommended
          }
        , [RebootHost]
        )
      ; (* livepatch_guidance: is overwhelmed by aother update *)
        ( {
            updates_info=
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
                        ; (Recommended, [RestartVM])
                        ; (Full, [])
                        ; (Livepatch, [RestartVM])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches=
                        [
                          LivePatch.
                            {
                              component= Livepatch.Xen
                            ; base_build_id=
                                "9346194f2e98a228f5a595b13ecabd43a99fada0"
                            ; base_version= "4.13.4"
                            ; base_release= "10.24.xs8"
                            ; to_version= "4.13.4"
                            ; to_release= "10.25.xs8"
                            }
                        ; LivePatch.
                            {
                              component= Livepatch.Kernel
                            ; base_build_id=
                                "8346194f2e98a228f5a595b13ecabd43a99fada0"
                            ; base_version= "4.19.19"
                            ; base_release= "8.0.20.xs8"
                            ; to_version= "4.19.19"
                            ; to_release= "8.0.21.xs8"
                            }
                        ]
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
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
                        ; (Recommended, [RebootHost])
                        ; (Full, [])
                        ; (Livepatch, [RestartDeviceModel])
                        ]
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatches=
                        [
                          LivePatch.
                            {
                              component= Livepatch.Kernel
                            ; base_build_id=
                                "8346194f2e98a228f5a595b13ecabd43a99fada0"
                            ; base_version= "4.19.19"
                            ; base_release= "8.0.20.xs8"
                            ; to_version= "4.19.19"
                            ; to_release= "8.0.22.xs8"
                            }
                        ]
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    ; title= ""
                    }
                )
              ]
          ; update=
              Update.
                {
                  name= "kernel"
                ; arch= "x86_64"
                ; old_epoch= Some None
                ; old_version= Some "4.19.19"
                ; old_release= Some "10.20.xs8"
                ; new_epoch= None
                ; new_version= "4.19.19"
                ; new_release= "8.0.22.xs8"
                ; update_id= Some "UPDATE-0001"
                ; repository= "regular"
                }
          ; upd_ids_of_livepatches= ["UPDATE-0000"]
          ; kind= Recommended
          }
        , [RebootHost]
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
      ; guidance=
          [(Mandatory, []); (Recommended, []); (Full, []); (Livepatch, [])]
      ; guidance_applicabilities= []
      ; spec_info= "special info"
      ; url= "https://update.details.info"
      ; update_type= "security"
      ; livepatches= []
      ; issued= Xapi_stdext_date.Date.epoch
      ; severity= Severity.None
      ; title= ""
      }

  let updates_info =
    let open Guidance in
    [
      ( "UPDATE-0000"
      , {
          updateinfo with
          id= "UPDATE-0000"
        ; guidance=
            [
              (Mandatory, [EvacuateHost])
            ; (Recommended, [])
            ; (Full, [])
            ; (Livepatch, [])
            ]
        }
      )
    ; ( "UPDATE-0001"
      , {
          updateinfo with
          id= "UPDATE-0001"
        ; guidance=
            [
              (Mandatory, [RebootHost])
            ; (Recommended, [])
            ; (Full, [])
            ; (Livepatch, [])
            ]
        }
      )
    ; ( "UPDATE-0002"
      , {
          updateinfo with
          id= "UPDATE-0002"
        ; guidance=
            [
              (Mandatory, [RestartDeviceModel])
            ; (Recommended, [])
            ; (Full, [])
            ; (Livepatch, [])
            ]
        }
      )
    ; ( "UPDATE-0003"
      , {
          updateinfo with
          id= "UPDATE-0003"
        ; guidance=
            [
              (Mandatory, [EvacuateHost])
            ; (Recommended, [])
            ; (Full, [])
            ; (Livepatch, [])
            ]
        }
      )
    ]

  let host = "string_of_host_ref"

  let transform updates =
    consolidate_updates_of_host ~repository_name:"regular" ~updates_info host
      (Yojson.Basic.from_string updates)
    |> fun (x, y) -> (HostUpdates.to_json x, y)

  let tests =
    `QuickAndAutoDocumented
      [
        ( (* No updates *)
          {|
            { "updates": [],
              "accumulative_updates": [],
              "applied_livepatches": []
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ( "guidance"
                , `Assoc
                    [
                      ("mandatory", `List [])
                    ; ("recommended", `List [])
                    ; ("full", `List [])
                    ]
                )
              ; ("RPMS", `List [])
              ; ("updates", `List [])
              ; ("livepatches", `List [])
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
              ],

              "applied_livepatches": []
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ( "guidance"
                , `Assoc
                    [
                      ("mandatory", `List [`String "RebootHost"])
                    ; ("recommended", `List [])
                    ; ("full", `List [])
                    ]
                )
              ; ( "RPMS"
                , `List
                    [
                      `String "xsconsole-0.2.2-9.el7.x86_64.rpm"
                    ; `String "libpath-utils-0.2.2-9.el7.noarch.rpm"
                    ]
                )
              ; ("updates", `List [`String "UPDATE-0000"; `String "UPDATE-0001"])
              ; ("livepatches", `List [])
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
              ],
              "applied_livepatches": []
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ( "guidance"
                , `Assoc
                    [
                      ("mandatory", `List [`String "RebootHost"])
                    ; ("recommended", `List [])
                    ; ("full", `List [])
                    ]
                )
              ; ("RPMS", `List [`String "libpath-utils-0.2.2-9.el7.noarch.rpm"])
              ; ("updates", `List [`String "UPDATE-0001"])
              ; ("livepatches", `List [])
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
              ],
              "applied_livepatches": []
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ( "guidance"
                , `Assoc
                    [
                      ("mandatory", `List [`String "EvacuateHost"])
                    ; ("recommended", `List [])
                    ; ("full", `List [])
                    ]
                )
              ; ("RPMS", `List [`String "libpath-utils-0.2.2-9.el7.noarch.rpm"])
              ; ("updates", `List [`String "UPDATE-0003"])
              ; ("livepatches", `List [])
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
              ],
              "applied_livepatches": []
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ( "guidance"
                , `Assoc
                    [
                      ("mandatory", `List [`String "RebootHost"])
                    ; ("recommended", `List [])
                    ; ("full", `List [])
                    ]
                )
              ; ( "RPMS"
                , `List
                    [
                      `String "xsconsole-0.2.2-9.el7.x86_64.rpm"
                    ; `String "libpath-utils-0.2.2-9.el7.noarch.rpm"
                    ]
                )
              ; ("updates", `List [`String "UPDATE-0001"])
              ; ("livepatches", `List [])
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
              ],
              "applied_livepatches": []
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ( "guidance"
                , `Assoc
                    [
                      ("mandatory", `List [`String "RebootHost"])
                    ; ("recommended", `List [])
                    ; ("full", `List [])
                    ]
                )
              ; ( "RPMS"
                , `List
                    [
                      `String "xsconsole-1:0.1.2-9.el7.x86_64.rpm"
                    ; `String "libpath-utils-2:0.1.2-9.el7.noarch.rpm"
                    ]
                )
              ; ("updates", `List [`String "UPDATE-0000"; `String "UPDATE-0001"])
              ; ("livepatches", `List [])
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
              ],
              "applied_livepatches": []
            }
          |}
        , ( `Assoc
              [
                ("ref", `String host)
              ; ( "guidance"
                , `Assoc
                    [
                      ("mandatory", `List [`String "RebootHost"])
                    ; ("recommended", `List [])
                    ; ("full", `List [])
                    ]
                )
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
              ; ("livepatches", `List [])
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

module GuidanceSetResortTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = Guidance.t list

    type output_t = Guidance.t list

    let string_of_input_t l =
      Fmt.(str "%a" Dump.(list string)) (List.map Guidance.to_string l)

    let string_of_output_t = string_of_input_t
  end

  let transform guidances =
    guidances
    |> GuidanceSet.of_list
    |> GuidanceSet.resort
    |> GuidanceSet.elements

  let tests =
    let open Guidance in
    `QuickAndAutoDocumented
      [
        ([], [])
      ; ([EvacuateHost], [EvacuateHost])
      ; ([EvacuateHost; RestartDeviceModel], [EvacuateHost])
      ; ( [EvacuateHost; RestartDeviceModel; RestartToolstack]
        , [RestartToolstack; EvacuateHost]
        )
      ; ( [EvacuateHost; RestartDeviceModel; RestartToolstack; RebootHost]
        , [RebootHost]
        )
      ; ([EvacuateHost; RestartDeviceModel; RebootHost], [RebootHost])
      ; ([EvacuateHost; RestartToolstack], [RestartToolstack; EvacuateHost])
      ; ([EvacuateHost; RestartToolstack; RebootHost], [RebootHost])
      ; ([EvacuateHost; RebootHost], [RebootHost])
      ; ([RestartDeviceModel], [RestartDeviceModel])
      ; ( [RestartDeviceModel; RestartToolstack]
        , [RestartToolstack; RestartDeviceModel]
        )
      ; ([RestartDeviceModel; RestartToolstack; RebootHost], [RebootHost])
      ; ([RestartDeviceModel; RebootHost], [RebootHost])
      ; ([RestartToolstack], [RestartToolstack])
      ; ([RestartToolstack; RebootHost], [RebootHost])
      ; ([RebootHost], [RebootHost])
      ; ([RestartVM], [RestartVM])
      ; ([RestartVM; EvacuateHost], [EvacuateHost; RestartVM])
      ; ( [RestartVM; EvacuateHost; RestartDeviceModel]
        , [EvacuateHost; RestartVM]
        )
      ; ( [RestartVM; EvacuateHost; RestartDeviceModel; RestartToolstack]
        , [RestartToolstack; EvacuateHost; RestartVM]
        )
      ; ( [
            RestartVM
          ; EvacuateHost
          ; RestartDeviceModel
          ; RestartToolstack
          ; RebootHost
          ]
        , [RebootHost; RestartVM]
        )
      ; ( [RestartVM; EvacuateHost; RestartDeviceModel; RebootHost]
        , [RebootHost; RestartVM]
        )
      ; ( [RestartVM; EvacuateHost; RestartToolstack]
        , [RestartToolstack; EvacuateHost; RestartVM]
        )
      ; ( [RestartVM; EvacuateHost; RestartToolstack; RebootHost]
        , [RebootHost; RestartVM]
        )
      ; ([RestartVM; EvacuateHost; RebootHost], [RebootHost; RestartVM])
      ; ([RestartVM; RestartDeviceModel], [RestartVM])
      ; ( [RestartVM; RestartDeviceModel; RestartToolstack]
        , [RestartToolstack; RestartVM]
        )
      ; ( [RestartVM; RestartDeviceModel; RestartToolstack; RebootHost]
        , [RebootHost; RestartVM]
        )
      ; ([RestartVM; RestartDeviceModel; RebootHost], [RebootHost; RestartVM])
      ; ([RestartVM; RestartToolstack], [RestartToolstack; RestartVM])
      ; ([RestartVM; RestartToolstack; RebootHost], [RebootHost; RestartVM])
      ; ([RestartVM; RebootHost], [RebootHost; RestartVM])
      ]
end)

module GuidanceSetReduceTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = Guidance.t list * Guidance.t list

    type output_t = Guidance.t list

    let string_of_input_t (l1, l2) =
      Fmt.(
        str "%a + %a"
          Dump.(list string)
          (List.map Guidance.to_string l1)
          Dump.(list string)
          (List.map Guidance.to_string l2)
      )

    let string_of_output_t l =
      Fmt.(str "%a" Dump.(list string)) (List.map Guidance.to_string l)
  end

  let transform (l1, l2) =
    let open GuidanceSet in
    reduce (of_list l1) (of_list l2) |> elements

  let tests =
    let open Guidance in
    `QuickAndAutoDocumented
      [
        (([], []), [])
      ; (([], [EvacuateHost]), [EvacuateHost])
      ; (([], [RebootHost]), [RebootHost])
      ; (([], [RestartDeviceModel]), [RestartDeviceModel])
      ; ( ([], [RestartToolstack; EvacuateHost])
        , [RestartToolstack; EvacuateHost]
        )
      ; ( ([], [RestartToolstack; RestartDeviceModel])
        , [RestartToolstack; RestartDeviceModel]
        )
      ; (([], [RestartToolstack]), [RestartToolstack])
      ; (([EvacuateHost], [EvacuateHost]), [])
      ; (([EvacuateHost], [RebootHost]), [RebootHost])
      ; (([EvacuateHost], [RestartDeviceModel]), [])
      ; (([EvacuateHost], [RestartToolstack; EvacuateHost]), [RestartToolstack])
      ; ( ([EvacuateHost], [RestartToolstack; RestartDeviceModel])
        , [RestartToolstack]
        )
      ; (([EvacuateHost], [RestartToolstack]), [RestartToolstack])
      ; (([EvacuateHost], []), [])
      ; (([RebootHost], [EvacuateHost]), [])
      ; (([RebootHost], [RebootHost]), [])
      ; (([RebootHost], [RestartDeviceModel]), [])
      ; (([RebootHost], [RestartToolstack; EvacuateHost]), [])
      ; (([RebootHost], [RestartToolstack; RestartDeviceModel]), [])
      ; (([RebootHost], [RestartToolstack]), [])
      ; (([RebootHost], []), [])
      ; (([RestartDeviceModel], [EvacuateHost]), [EvacuateHost])
      ; (([RestartDeviceModel], [RebootHost]), [RebootHost])
      ; (([RestartDeviceModel], [RestartDeviceModel]), [])
      ; ( ([RestartDeviceModel], [RestartToolstack; EvacuateHost])
        , [RestartToolstack; EvacuateHost]
        )
      ; ( ([RestartDeviceModel], [RestartToolstack; RestartDeviceModel])
        , [RestartToolstack]
        )
      ; (([RestartDeviceModel], [RestartToolstack]), [RestartToolstack])
      ; (([RestartDeviceModel], []), [])
      ; (([RestartToolstack; EvacuateHost], [EvacuateHost]), [])
      ; (([RestartToolstack; EvacuateHost], [RebootHost]), [RebootHost])
      ; (([RestartToolstack; EvacuateHost], [RestartDeviceModel]), [])
      ; ( ([RestartToolstack; EvacuateHost], [RestartToolstack; EvacuateHost])
        , []
        )
      ; ( ( [RestartToolstack; EvacuateHost]
          , [RestartToolstack; RestartDeviceModel]
          )
        , []
        )
      ; (([RestartToolstack; EvacuateHost], [RestartToolstack]), [])
      ; (([RestartToolstack; EvacuateHost], []), [])
      ; ( ([RestartToolstack; RestartDeviceModel], [EvacuateHost])
        , [EvacuateHost]
        )
      ; (([RestartToolstack; RestartDeviceModel], [RebootHost]), [RebootHost])
      ; (([RestartToolstack; RestartDeviceModel], [RestartDeviceModel]), [])
      ; ( ( [RestartToolstack; RestartDeviceModel]
          , [RestartToolstack; EvacuateHost]
          )
        , [EvacuateHost]
        )
      ; ( ( [RestartToolstack; RestartDeviceModel]
          , [RestartToolstack; RestartDeviceModel]
          )
        , []
        )
      ; (([RestartToolstack; RestartDeviceModel], [RestartToolstack]), [])
      ; (([RestartToolstack; RestartDeviceModel], []), [])
      ; (([RestartToolstack], [EvacuateHost]), [EvacuateHost])
      ; (([RestartToolstack], [RebootHost]), [RebootHost])
      ; (([RestartToolstack], [RestartDeviceModel]), [RestartDeviceModel])
      ; (([RestartToolstack], [RestartToolstack; EvacuateHost]), [EvacuateHost])
      ; ( ([RestartToolstack], [RestartToolstack; RestartDeviceModel])
        , [RestartDeviceModel]
        )
      ; (([RestartToolstack], [RestartToolstack]), [])
      ; (([RestartToolstack], []), [])
      ; (([RestartVM; EvacuateHost], [EvacuateHost]), [])
      ; (([RestartVM; EvacuateHost], [RebootHost; RestartVM]), [RebootHost])
      ; (([RestartVM; EvacuateHost], [RebootHost]), [RebootHost])
      ; (([RestartVM; EvacuateHost], [RestartDeviceModel]), [])
      ; ( ([RestartVM; EvacuateHost], [RestartToolstack; EvacuateHost])
        , [RestartToolstack]
        )
      ; ( ([RestartVM; EvacuateHost], [RestartToolstack; RestartDeviceModel])
        , [RestartToolstack]
        )
      ; (([RestartVM; EvacuateHost], [RestartToolstack]), [RestartToolstack])
      ; (([RestartVM; EvacuateHost], [RestartVM; EvacuateHost]), [])
      ; ( ( [RestartVM; EvacuateHost]
          , [RestartVM; RestartToolstack; EvacuateHost]
          )
        , [RestartToolstack]
        )
      ; ( ([RestartVM; EvacuateHost], [RestartVM; RestartToolstack])
        , [RestartToolstack]
        )
      ; (([RestartVM; EvacuateHost], [RestartVM]), [])
      ; (([RestartVM; EvacuateHost], []), [])
      ; (([RestartVM; RestartToolstack; EvacuateHost], [EvacuateHost]), [])
      ; ( ([RestartVM; RestartToolstack; EvacuateHost], [RebootHost; RestartVM])
        , [RebootHost]
        )
      ; ( ([RestartVM; RestartToolstack; EvacuateHost], [RebootHost])
        , [RebootHost]
        )
      ; (([RestartVM; RestartToolstack; EvacuateHost], [RestartDeviceModel]), [])
      ; ( ( [RestartVM; RestartToolstack; EvacuateHost]
          , [RestartToolstack; EvacuateHost]
          )
        , []
        )
      ; ( ( [RestartVM; RestartToolstack; EvacuateHost]
          , [RestartToolstack; RestartDeviceModel]
          )
        , []
        )
      ; (([RestartVM; RestartToolstack; EvacuateHost], [RestartToolstack]), [])
      ; ( ( [RestartVM; RestartToolstack; EvacuateHost]
          , [RestartVM; EvacuateHost]
          )
        , []
        )
      ; ( ( [RestartVM; RestartToolstack; EvacuateHost]
          , [RestartVM; RestartToolstack; EvacuateHost]
          )
        , []
        )
      ; ( ( [RestartVM; RestartToolstack; EvacuateHost]
          , [RestartVM; RestartToolstack]
          )
        , []
        )
      ; (([RestartVM; RestartToolstack; EvacuateHost], [RestartVM]), [])
      ; (([RestartVM; RestartToolstack; EvacuateHost], []), [])
      ; (([RestartVM; RestartToolstack], [EvacuateHost]), [EvacuateHost])
      ; (([RestartVM; RestartToolstack], [RebootHost; RestartVM]), [RebootHost])
      ; (([RestartVM; RestartToolstack], [RebootHost]), [RebootHost])
      ; (([RestartVM; RestartToolstack], [RestartDeviceModel]), [])
      ; ( ([RestartVM; RestartToolstack], [RestartToolstack; EvacuateHost])
        , [EvacuateHost]
        )
      ; ( ([RestartVM; RestartToolstack], [RestartToolstack; RestartDeviceModel])
        , []
        )
      ; (([RestartVM; RestartToolstack], [RestartToolstack]), [])
      ; ( ([RestartVM; RestartToolstack], [RestartVM; EvacuateHost])
        , [EvacuateHost]
        )
      ; ( ( [RestartVM; RestartToolstack]
          , [RestartVM; RestartToolstack; EvacuateHost]
          )
        , [EvacuateHost]
        )
      ; (([RestartVM; RestartToolstack], [RestartVM; RestartToolstack]), [])
      ; (([RestartVM; RestartToolstack], [RestartVM]), [])
      ; (([RestartVM; RestartToolstack], []), [])
      ; (([RestartVM], [EvacuateHost]), [EvacuateHost])
      ; (([RestartVM], [RebootHost; RestartVM]), [RebootHost])
      ; (([RestartVM], [RebootHost]), [RebootHost])
      ; (([RestartVM], [RestartDeviceModel]), [])
      ; ( ([RestartVM], [RestartToolstack; EvacuateHost])
        , [RestartToolstack; EvacuateHost]
        )
      ; ( ([RestartVM], [RestartToolstack; RestartDeviceModel])
        , [RestartToolstack]
        )
      ; (([RestartVM], [RestartToolstack]), [RestartToolstack])
      ; (([RestartVM], [RestartVM; EvacuateHost]), [EvacuateHost])
      ; ( ([RestartVM], [RestartVM; RestartToolstack; EvacuateHost])
        , [RestartToolstack; EvacuateHost]
        )
      ; (([RestartVM], [RestartVM; RestartToolstack]), [RestartToolstack])
      ; (([RestartVM], [RestartVM]), [])
      ; (([RestartVM], []), [])
      ; (([], [RestartVM; EvacuateHost]), [EvacuateHost; RestartVM])
      ; ( ([], [RestartVM; RestartToolstack; EvacuateHost])
        , [RestartToolstack; EvacuateHost; RestartVM]
        )
      ; (([], [RestartVM; RestartToolstack]), [RestartToolstack; RestartVM])
      ; (([], [RestartVM]), [RestartVM])
      ]
end)

module GuidanceSetReduceCascadedListTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = (Guidance.kind * Guidance.t list) list

    type output_t = input_t

    let string_of_input_t l =
      let string_of_kind_guidances (kind, gs) =
        Fmt.(
          str "%a: %a"
            Dump.(string)
            (Guidance.kind_to_string kind)
            Dump.(list string)
            (List.map Guidance.to_string gs)
        )
      in
      Fmt.(str "%a" Dump.(list string)) (List.map string_of_kind_guidances l)

    let string_of_output_t = string_of_input_t
  end

  let transform l =
    l
    |> List.map (fun (k, l') -> (k, GuidanceSet.of_list l'))
    |> GuidanceSet.reduce_cascaded_list
    |> List.map (fun (k, s') -> (k, GuidanceSet.elements s'))

  let tests =
    let open Guidance in
    `QuickAndAutoDocumented
      [
        ([], [])
      ; ( [(Mandatory, []); (Recommended, []); (Full, [])]
        , [(Mandatory, []); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [])
          ; (Recommended, [])
          ; (Full, [RestartToolstack; RestartDeviceModel])
          ]
        , [
            (Mandatory, [])
          ; (Recommended, [])
          ; (Full, [RestartToolstack; RestartDeviceModel])
          ]
        )
      ; ( [
            (Mandatory, [])
          ; (Recommended, [RestartToolstack])
          ; (Full, [RestartDeviceModel])
          ]
        , [
            (Mandatory, [])
          ; (Recommended, [RestartToolstack])
          ; (Full, [RestartDeviceModel])
          ]
        )
      ; ( [
            (Mandatory, [])
          ; (Recommended, [RestartToolstack])
          ; (Full, [RebootHost])
          ]
        , [
            (Mandatory, [])
          ; (Recommended, [RestartToolstack])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [
            (Mandatory, [])
          ; (Recommended, [RestartToolstack; RestartDeviceModel])
          ; (Full, [RebootHost])
          ]
        , [
            (Mandatory, [])
          ; (Recommended, [RestartToolstack; RestartDeviceModel])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [(Mandatory, [RestartToolstack]); (Recommended, []); (Full, [])]
        , [(Mandatory, [RestartToolstack]); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [RestartToolstack])
          ; (Recommended, [])
          ; (Full, [RestartToolstack])
          ]
        , [(Mandatory, [RestartToolstack]); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [RestartToolstack])
          ; (Recommended, [])
          ; (Full, [RebootHost])
          ]
        , [
            (Mandatory, [RestartToolstack])
          ; (Recommended, [])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [
            (Mandatory, [RestartToolstack; RestartDeviceModel])
          ; (Recommended, [])
          ; (Full, [])
          ]
        , [
            (Mandatory, [RestartToolstack; RestartDeviceModel])
          ; (Recommended, [])
          ; (Full, [])
          ]
        )
      ; ( [
            (Mandatory, [RestartToolstack; RestartDeviceModel])
          ; (Recommended, [])
          ; (Full, [RebootHost])
          ]
        , [
            (Mandatory, [RestartToolstack; RestartDeviceModel])
          ; (Recommended, [])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [])
          ; (Full, [RestartDeviceModel])
          ]
        , [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [])
          ; (Full, [])
          ]
        )
      ; ( [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [RestartDeviceModel])
          ; (Full, [RestartToolstack])
          ]
        , [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [])
          ; (Full, [])
          ]
        )
      ; ( [
            (Mandatory, [RestartDeviceModel])
          ; (Recommended, [])
          ; (Full, [RebootHost])
          ]
        , [
            (Mandatory, [RestartDeviceModel])
          ; (Recommended, [])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [
            (Mandatory, [RestartDeviceModel])
          ; (Recommended, [RestartToolstack])
          ; (Full, [RebootHost])
          ]
        , [
            (Mandatory, [RestartDeviceModel])
          ; (Recommended, [RestartToolstack])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [(Mandatory, [RebootHost]); (Recommended, []); (Full, [])]
        , [(Mandatory, [RebootHost]); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [RestartDeviceModel])
          ; (Recommended, [])
          ; (Full, [RestartToolstack; RestartDeviceModel])
          ]
        , [
            (Mandatory, [RestartDeviceModel])
          ; (Recommended, [])
          ; (Full, [RestartToolstack])
          ]
        )
      ; ( [
            (Mandatory, [RebootHost])
          ; (Recommended, [])
          ; (Full, [RestartToolstack; RestartDeviceModel])
          ]
        , [(Mandatory, [RebootHost]); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [RebootHost])
          ; (Recommended, [RestartDeviceModel])
          ; (Full, [RestartToolstack])
          ]
        , [(Mandatory, [RebootHost]); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [EvacuateHost])
          ; (Recommended, [])
          ; (Full, [RestartDeviceModel])
          ]
        , [(Mandatory, [EvacuateHost]); (Recommended, []); (Full, [])]
        )
      ; ( [(Mandatory, [EvacuateHost]); (Recommended, []); (Full, [RebootHost])]
        , [(Mandatory, [EvacuateHost]); (Recommended, []); (Full, [RebootHost])]
        )
      ; ( [
            (Mandatory, [RebootHost; RestartVM])
          ; (Recommended, [])
          ; (Full, [RestartVM])
          ]
        , [(Mandatory, [RebootHost; RestartVM]); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [])
          ; (Full, [RebootHost; RestartVM])
          ]
        , [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [])
          ; (Full, [RebootHost; RestartVM])
          ]
        )
      ; ( [
            (Mandatory, [RebootHost; RestartVM])
          ; (Recommended, [EvacuateHost])
          ; (Full, [RebootHost; RestartVM])
          ]
        , [(Mandatory, [RebootHost; RestartVM]); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [RebootHost])
          ; (Recommended, [RestartVM; EvacuateHost])
          ; (Full, [RebootHost])
          ]
        , [(Mandatory, [RebootHost]); (Recommended, [RestartVM]); (Full, [])]
        )
      ; ( [
            (Mandatory, [RebootHost])
          ; (Recommended, [EvacuateHost])
          ; (Full, [RestartVM])
          ]
        , [(Mandatory, [RebootHost]); (Recommended, []); (Full, [RestartVM])]
        )
      ; ( [
            (Mandatory, [RebootHost; RestartVM])
          ; (Recommended, [RestartToolstack])
          ; (Full, [RebootHost; RestartVM])
          ]
        , [(Mandatory, [RebootHost; RestartVM]); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [RestartToolstack])
          ; (Recommended, [EvacuateHost])
          ; (Full, [RestartVM])
          ]
        , [
            (Mandatory, [RestartToolstack])
          ; (Recommended, [EvacuateHost])
          ; (Full, [RestartVM])
          ]
        )
      ; ( [
            (Mandatory, [RestartVM; RestartToolstack])
          ; (Recommended, [])
          ; (Full, [RebootHost])
          ]
        , [
            (Mandatory, [RestartToolstack; RestartVM])
          ; (Recommended, [])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [RestartVM])
          ; (Full, [RebootHost])
          ]
        , [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [RestartVM])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [
            (Mandatory, [RebootHost; RestartVM])
          ; (Recommended, [EvacuateHost])
          ; (Full, [RebootHost])
          ]
        , [(Mandatory, [RebootHost; RestartVM]); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [RestartVM; RestartToolstack])
          ; (Recommended, [EvacuateHost])
          ; (Full, [RebootHost; RestartVM])
          ]
        , [
            (Mandatory, [RestartToolstack; RestartVM])
          ; (Recommended, [EvacuateHost])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [])
          ; (Full, [RestartVM])
          ]
        , [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [])
          ; (Full, [RestartVM])
          ]
        )
      ; ( [
            (Mandatory, [RestartVM])
          ; (Recommended, [RestartToolstack])
          ; (Full, [RebootHost; RestartVM])
          ]
        , [
            (Mandatory, [RestartVM])
          ; (Recommended, [RestartToolstack])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [
            (Mandatory, [RebootHost])
          ; (Recommended, [])
          ; (Full, [RebootHost; RestartVM])
          ]
        , [(Mandatory, [RebootHost]); (Recommended, []); (Full, [RestartVM])]
        )
      ; ( [
            (Mandatory, [RebootHost])
          ; (Recommended, [RestartDeviceModel])
          ; (Full, [RestartVM])
          ]
        , [(Mandatory, [RebootHost]); (Recommended, []); (Full, [RestartVM])]
        )
      ; ( [
            (Mandatory, [RestartDeviceModel])
          ; (Recommended, [RestartToolstack])
          ; (Full, [RestartVM])
          ]
        , [
            (Mandatory, [RestartDeviceModel])
          ; (Recommended, [RestartToolstack])
          ; (Full, [RestartVM])
          ]
        )
      ; ( [
            (Mandatory, [RestartToolstack])
          ; (Recommended, [RestartVM; EvacuateHost])
          ; (Full, [RebootHost])
          ]
        , [
            (Mandatory, [RestartToolstack])
          ; (Recommended, [EvacuateHost; RestartVM])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [
            (Mandatory, [RebootHost])
          ; (Recommended, [RestartToolstack; EvacuateHost])
          ; (Full, [RebootHost; RestartVM])
          ]
        , [(Mandatory, [RebootHost]); (Recommended, []); (Full, [RestartVM])]
        )
      ; ( [
            (Mandatory, [RestartVM])
          ; (Recommended, [RestartDeviceModel])
          ; (Full, [RestartVM])
          ]
        , [(Mandatory, [RestartVM]); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [RestartVM; RestartToolstack])
          ; (Recommended, [EvacuateHost])
          ; (Full, [RebootHost])
          ]
        , [
            (Mandatory, [RestartToolstack; RestartVM])
          ; (Recommended, [EvacuateHost])
          ; (Full, [RebootHost])
          ]
        )
      ; ( [
            (Mandatory, [RestartDeviceModel])
          ; (Recommended, [RebootHost])
          ; (Full, [RestartVM])
          ]
        , [
            (Mandatory, [RestartDeviceModel])
          ; (Recommended, [RebootHost])
          ; (Full, [RestartVM])
          ]
        )
      ; ( [
            (Mandatory, [RebootHost; RestartVM])
          ; (Recommended, [RestartToolstack; RestartDeviceModel])
          ; (Full, [RebootHost; RestartVM])
          ]
        , [(Mandatory, [RebootHost; RestartVM]); (Recommended, []); (Full, [])]
        )
      ; ( [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [RestartDeviceModel])
          ; (Full, [RestartVM])
          ]
        , [
            (Mandatory, [RestartToolstack; EvacuateHost])
          ; (Recommended, [])
          ; (Full, [RestartVM])
          ]
        )
      ; ( [
            (Mandatory, [RestartToolstack])
          ; (Recommended, [RestartVM])
          ; (Full, [RebootHost])
          ]
        , [
            (Mandatory, [RestartToolstack])
          ; (Recommended, [RestartVM])
          ; (Full, [RebootHost])
          ]
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
        (fun acc (pkg, upd_id, repo) ->
          acc
          ^ "\n"
          ^ Fmt.(str "%a" Dump.(record @@ fields_of_pkg)) pkg
          ^ ", "
          ^ Fmt.(str "%a" Dump.(option string)) upd_id
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

module PruneUpdateInfoForLivepatches = Generic.MakeStateless (struct
  module Io = struct
    type input_t = LivePatch.t list * UpdateInfo.t

    type output_t = UpdateInfo.t

    let string_of_input_t (l, updateinfo) =
      Fmt.(str "%a" Dump.(pair (list string) string))
        ( List.map LivePatch.to_string l
        , UpdateInfo.to_json updateinfo |> Yojson.Basic.pretty_to_string
        )

    let string_of_output_t x =
      Fmt.(str "%a" Dump.(string))
        (UpdateInfo.to_json x |> Yojson.Basic.pretty_to_string)
  end

  let transform (l, updateinfo) =
    let s = LivePatchSet.of_list l in
    prune_updateinfo_for_livepatches s updateinfo

  let lp0 =
    LivePatch.
      {
        component= Livepatch.Xen
      ; base_build_id= "ab7e6a47709be565aae76099bd36ae93fd6da5f4"
      ; base_version= "4.13.4"
      ; base_release= "10.24.xs8"
      ; to_version= "4.13.4"
      ; to_release= "10.25.xs8"
      }

  let lp1 =
    LivePatch.
      {
        component= Livepatch.Kernel
      ; base_build_id= "2cc28689364587682593b6a72e2a586d29996bb9"
      ; base_version= "4.19.19"
      ; base_release= "8.0.20.xs8"
      ; to_version= "4.13.4"
      ; to_release= "8.0.21.xs8"
      }

  let updateinfo =
    UpdateInfo.
      {
        id= "UPDATE-00"
      ; summary= "SUMMARY"
      ; description= "DESCRIPTION"
      ; guidance=
          [(Mandatory, []); (Recommended, []); (Full, []); (Livepatch, [])]
      ; guidance_applicabilities= []
      ; spec_info= "SPEC_INFO"
      ; url= "URL"
      ; update_type= "UPDATE_TYPE"
      ; livepatches= []
      ; issued= Xapi_stdext_date.Date.epoch
      ; severity= Severity.None
      ; title= ""
      }

  let tests =
    `QuickAndAutoDocumented
      [
        ( ([lp0], {updateinfo with livepatches= []})
        , {updateinfo with livepatches= []}
        )
      ; ( ([lp0], {updateinfo with livepatches= [lp0; lp1]})
        , {updateinfo with livepatches= [lp0]}
        )
      ; ( ([], {updateinfo with livepatches= [lp0; lp1]})
        , {updateinfo with livepatches= []}
        )
      ]
end)

module ParseOutputOfYumUpgradeDryRun = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = ((Pkg.t * string) list * string option, string) result

    let string_of_input_t s = s

    let string_of_output_t =
      Fmt.(
        str "%a"
          Dump.(
            result
              ~ok:
                (pair
                   (list (pair (record @@ fields_of_pkg) string))
                   (option string)
                )
              ~error:string
          )
      )
  end

  let transform input = YumUpgradeOutput.parse_output_of_dry_run input

  let output1 =
    Xapi_stdext_unix.Unixext.string_of_file "test_data/yum_upgrade.output1"

  let output2 =
    Xapi_stdext_unix.Unixext.string_of_file "test_data/yum_upgrade.output2"

  let output3 =
    Xapi_stdext_unix.Unixext.string_of_file "test_data/yum_upgrade.output3"

  let output4 =
    Xapi_stdext_unix.Unixext.string_of_file "test_data/yum_upgrade.output4"

  let output5 =
    Xapi_stdext_unix.Unixext.string_of_file "test_data/yum_upgrade.output5"

  let tests =
    `QuickAndAutoDocumented
      [
        ("", Ok ([], None))
      ; ("A\n", Ok ([], None))
      ; ( output1
        , Ok
            ( [
                ( Pkg.
                    {
                      name= "amd-microcode"
                    ; epoch= None
                    ; version= "20220930"
                    ; release= "2.xs8"
                    ; arch= "noarch"
                    }
                , "remote-399dcec8-9ee7-0fb1-b1c9-f70fde0d1edb"
                )
              ; ( Pkg.
                    {
                      name= "device-mapper-multipath-libs"
                    ; epoch= None
                    ; version= "0.4.9"
                    ; release= "136.xs8"
                    ; arch= "x86_64"
                    }
                , "remote-399dcec8-9ee7-0fb1-b1c9-f70fde0d1edb"
                )
              ; ( Pkg.
                    {
                      name= "libfdt"
                    ; epoch= None
                    ; version= "1.6.0"
                    ; release= "1.xs8"
                    ; arch= "x86_64"
                    }
                , "remote-399dcec8-9ee7-0fb1-b1c9-f70fde0d1edb"
                )
              ; ( Pkg.
                    {
                      name= "microsemi-aacraid"
                    ; epoch= None
                    ; version= "1.2.1.60001"
                    ; release= "1.xs8"
                    ; arch= "x86_64"
                    }
                , "remote-399dcec8-9ee7-0fb1-b1c9-f70fde0d1edb"
                )
              ; ( Pkg.
                    {
                      name= "qemu"
                    ; epoch= Some 2
                    ; version= "4.2.1"
                    ; release= "5.2.1.xs8"
                    ; arch= "x86_64"
                    }
                , "remote-399dcec8-9ee7-0fb1-b1c9-f70fde0d1edb"
                )
              ; ( Pkg.
                    {
                      name= "qemu-dp"
                    ; epoch= Some 2
                    ; version= "7.0.0"
                    ; release= "3.xs8"
                    ; arch= "x86_64"
                    }
                , "remote-399dcec8-9ee7-0fb1-b1c9-f70fde0d1edb"
                )
              ]
            , Some "/tmp/yum_save_tx.2023-04-03.04-59.z6T4rI.yumtx"
            )
        )
      ; ( output2
        , Ok
            ( [
                ( Pkg.
                    {
                      name= "curl"
                    ; epoch= None
                    ; version= "7.61.1"
                    ; release= "30.el8"
                    ; arch= "x86_64"
                    }
                , "baseos"
                )
              ; ( Pkg.
                    {
                      name= "distribution-gpg-keys"
                    ; epoch= None
                    ; version= "1.85"
                    ; release= "1.el8"
                    ; arch= "noarch"
                    }
                , "epel"
                )
              ; ( Pkg.
                    {
                      name= "openssl"
                    ; epoch= Some 1
                    ; version= "1.1.1k"
                    ; release= "9.el8"
                    ; arch= "x86_64"
                    }
                , "baseos"
                )
              ; ( Pkg.
                    {
                      name= "util-linux"
                    ; epoch= None
                    ; version= "2.32.1"
                    ; release= "41.el8"
                    ; arch= "x86_64"
                    }
                , "baseos"
                )
              ]
            , None
            )
        )
      ; ( output3
        , Ok
            ( [
                ( Pkg.
                    {
                      name= "xenserver-telemetry"
                    ; epoch= None
                    ; version= "1.0.1"
                    ; release= "31.2.g8c48473.xs8"
                    ; arch= "noarch"
                    }
                , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                )
              ]
            , None
            )
        )
      ; ( output4
        , Error
            "Can't parse output from yum upgrade (dry run): : Error: \
             xenserver-telemetry conflicts with \
             python2-bitarray-0.8.3-2.xs8.x86_64"
        )
      ; ( output5
        , Ok
            ( [
                ( Pkg.
                    {
                      name= "amd-microcode"
                    ; epoch= None
                    ; version= "20220930"
                    ; release= "2.xs8"
                    ; arch= "noarch"
                    }
                , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                )
              ; ( Pkg.
                    {
                      name= "python2-bitarray"
                    ; epoch= None
                    ; version= "0.8.3"
                    ; release= "2.xs8"
                    ; arch= "x86_64"
                    }
                , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                )
              ; ( Pkg.
                    {
                      name= "xenserver-telemetry"
                    ; epoch= None
                    ; version= "1.0.1"
                    ; release= "71.2.g8c48473.xs8"
                    ; arch= "noarch"
                    }
                , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                )
              ]
            , None
            )
        )
      ]
end)

module GetLatestUpdatesFromRedundancy = Generic.MakeStateless (struct
  module Io = struct
    type input_t = bool * ((Pkg.t * string) list option * (Pkg.t * string) list)

    type output_t = ((Pkg.t * string) list, exn) result

    let string_of_input_t =
      Fmt.(
        str "%a"
          Dump.(
            pair bool
              (pair
                 (option (list (pair (record @@ fields_of_pkg) string)))
                 (list (pair (record @@ fields_of_pkg) string))
              )
          )
      )

    let string_of_output_t = function
      | Ok pkgs ->
          Fmt.(str "%a" Dump.(list (pair (record @@ fields_of_pkg) string)) pkgs)
      | Error e ->
          Fmt.(str "%a" exn) e
  end

  let transform (fail_on_error, (l1, l2)) =
    try
      Ok
        (get_latest_updates_from_redundancy ~fail_on_error ~pkgs:l1
           ~fallback_pkgs:l2
        )
    with e -> Error e

  let e =
    Api_errors.(
      Server_error
        ( internal_error
        , ["Failed to parse output of 'yum upgrade (dry run)' correctly"]
        )
    )

  let tests =
    `QuickAndAutoDocumented
      [
        ((false, (None, [])), Ok [])
      ; ((true, (None, [])), Error e)
      ; ((true, (Some [], [])), Ok [])
      ; (* Unexpected extra pkgs from 'yum upgrade', raise error *)
        ( ( true
          , ( Some
                [
                  ( Pkg.
                      {
                        name= "amd-microcode"
                      ; epoch= None
                      ; version= "20220930"
                      ; release= "2.xs8"
                      ; arch= "noarch"
                      }
                  , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                  )
                ; ( Pkg.
                      {
                        name= "python2-bitarray"
                      ; epoch= None
                      ; version= "0.8.3"
                      ; release= "2.xs8"
                      ; arch= "x86_64"
                      }
                  , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                  )
                ]
            , []
            )
          )
        , Error e
        )
      ; (* Unexpected extra pkgs from 'yum upgrade', falling back *)
        ( ( false
          , ( Some
                [
                  ( Pkg.
                      {
                        name= "amd-microcode"
                      ; epoch= None
                      ; version= "20220930"
                      ; release= "2.xs8"
                      ; arch= "noarch"
                      }
                  , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                  )
                ; ( Pkg.
                      {
                        name= "python2-bitarray"
                      ; epoch= None
                      ; version= "0.8.3"
                      ; release= "2.xs8"
                      ; arch= "x86_64"
                      }
                  , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                  )
                ]
            , []
            )
          )
        , Ok []
        )
      ; (* Same results, use 'yum upgrade' *)
        ( ( true
          , ( Some
                [
                  ( Pkg.
                      {
                        name= "amd-microcode"
                      ; epoch= None
                      ; version= "20220930"
                      ; release= "2.xs8"
                      ; arch= "noarch"
                      }
                  , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                  )
                ; ( Pkg.
                      {
                        name= "python2-bitarray"
                      ; epoch= None
                      ; version= "0.8.3"
                      ; release= "2.xs8"
                      ; arch= "x86_64"
                      }
                  , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                  )
                ]
            , [
                ( Pkg.
                    {
                      name= "amd-microcode"
                    ; epoch= None
                    ; version= "20220930"
                    ; release= "2.xs8"
                    ; arch= "noarch"
                    }
                , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                )
              ; ( Pkg.
                    {
                      name= "python2-bitarray"
                    ; epoch= None
                    ; version= "0.8.3"
                    ; release= "2.xs8"
                    ; arch= "x86_64"
                    }
                , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                )
              ]
            )
          )
        , Ok
            [
              ( Pkg.
                  {
                    name= "amd-microcode"
                  ; epoch= None
                  ; version= "20220930"
                  ; release= "2.xs8"
                  ; arch= "noarch"
                  }
              , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
              )
            ; ( Pkg.
                  {
                    name= "python2-bitarray"
                  ; epoch= None
                  ; version= "0.8.3"
                  ; release= "2.xs8"
                  ; arch= "x86_64"
                  }
              , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
              )
            ]
        )
      ; (* is a subset; use 'yum upgrade' *)
        ( ( false
          , ( Some
                [
                  ( Pkg.
                      {
                        name= "amd-microcode"
                      ; epoch= None
                      ; version= "20220930"
                      ; release= "2.xs8"
                      ; arch= "noarch"
                      }
                  , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                  )
                ]
            , [
                ( Pkg.
                    {
                      name= "amd-microcode"
                    ; epoch= None
                    ; version= "20220930"
                    ; release= "2.xs8"
                    ; arch= "noarch"
                    }
                , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                )
              ; ( Pkg.
                    {
                      name= "python2-bitarray"
                    ; epoch= None
                    ; version= "0.8.3"
                    ; release= "2.xs8"
                    ; arch= "x86_64"
                    }
                , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
                )
              ]
            )
          )
        , Ok
            [
              ( Pkg.
                  {
                    name= "amd-microcode"
                  ; epoch= None
                  ; version= "20220930"
                  ; release= "2.xs8"
                  ; arch= "noarch"
                  }
              , "local-bd74070c-897f-d2bc-654a-a3f87d47f6b6"
              )
            ]
        )
      ]
end)

module SetPendingGuidance = Generic.MakeStateless (struct
  module Io = struct
    (* ([host pending guidance list], [VMs pending guidance lists]), [coming guidance list] *)
    type input_t =
      (Guidance.t list * (string * Guidance.t list) list) * Guidance.t list

    (* ([host pending guidance list], [VMs pending guidance lists]) *)
    type output_t = Guidance.t list * (string * Guidance.t list) list

    let string_of_pending (host_pending, vms_pending) =
      Fmt.(
        str "Host: %a; VMs: %a"
          Dump.(list string)
          (List.map Guidance.to_string host_pending)
          Dump.(list (pair string (list string)))
          (List.map
             (fun (vm, l) -> (vm, List.map Guidance.to_string l))
             vms_pending
          )
      )

    let string_of_input_t (pending, coming) =
      Fmt.(
        str "Pending: %s. Coming: %a"
          (string_of_pending pending)
          Dump.(list string)
          (List.map Guidance.to_string coming)
      )

    let string_of_output_t = string_of_pending
  end

  let transform ((host_pending, vms_pending), coming) =
    (* Use two hash tables to simulate the host's pending list and the VMs' pending lists *)
    let host_tbl :
        ( string
        , [ `reboot_host
          | `restart_toolstack
          | `reboot_host_on_livepatch_failure
          | `reboot_host_on_xen_livepatch_failure
          | `reboot_host_on_kernel_livepatch_failure ]
          list
        )
        Hashtbl.t =
      Hashtbl.create 1
    in
    let vms_tbl : (string, [`restart_device_model | `restart_vm] list) Hashtbl.t
        =
      Hashtbl.create 3
    in
    (* Only one host in the table *)
    let host_ref = "host_ref" in
    let open Guidance in
    let host_to_pending = function
      | RebootHost ->
          Some `reboot_host
      | RebootHostOnLivePatchFailure ->
          Some `reboot_host_on_livepatch_failure
      | RebootHostOnXenLivePatchFailure ->
          Some `reboot_host_on_xen_livepatch_failure
      | RebootHostOnKernelLivePatchFailure ->
          Some `reboot_host_on_kernel_livepatch_failure
      | RestartToolstack ->
          Some `restart_toolstack
      | _ ->
          None
    in
    let vm_to_pending = function
      | RestartDeviceModel ->
          Some `restart_device_model
      | RestartVM ->
          Some `restart_vm
      | _ ->
          None
    in
    let to_guidance = Guidance.of_pending_guidance in
    Hashtbl.add host_tbl host_ref (List.filter_map host_to_pending host_pending) ;
    let vm_mapper (vm_ref, l) = (vm_ref, List.filter_map vm_to_pending l) in
    Hashtbl.add_seq vms_tbl (List.to_seq (List.map vm_mapper vms_pending)) ;
    let ops =
      let host_get () =
        Hashtbl.find host_tbl host_ref |> List.map to_guidance
      in
      let host_add value =
        Hashtbl.find host_tbl host_ref
        |> List.cons value
        |> Hashtbl.replace host_tbl host_ref
      in
      let host_remove value =
        Hashtbl.find host_tbl host_ref
        |> List.filter (fun g -> g <> value)
        |> Hashtbl.replace host_tbl host_ref
      in
      let vms_get () =
        Hashtbl.to_seq vms_tbl
        |> List.of_seq
        |> List.map (fun (vm_ref, l) -> (vm_ref, List.map to_guidance l))
      in
      let vm_add vm_ref value =
        Hashtbl.find vms_tbl vm_ref
        |> List.cons value
        |> Hashtbl.replace vms_tbl vm_ref
      in
      let vm_remove vm_ref value =
        Hashtbl.find vms_tbl vm_ref
        |> List.filter (fun g -> g <> value)
        |> Hashtbl.replace vms_tbl vm_ref
      in
      {host_get; host_add; host_remove; vms_get; vm_add; vm_remove}
    in
    (* transform *)
    set_pending_guidances ~ops ~coming ;
    (* return result *)
    ( ops.host_get ()
      |> List.sort (fun g1 g2 -> String.compare (to_string g1) (to_string g2))
    , ops.vms_get () |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
    )

  let tests =
    let open Guidance in
    `QuickAndAutoDocumented
      [
        ((([], []), []), ([], []))
      ; ((([], []), [RebootHost]), ([RebootHost], []))
      ; ( (([], [("vm1", [RestartDeviceModel]); ("vm2", [])]), [RebootHost])
        , ([RebootHost], [("vm1", [RestartDeviceModel]); ("vm2", [])])
        )
      ; ((([], []), [RestartDeviceModel]), ([], []))
      ; ( (([RebootHost], [("vm1", []); ("vm2", [])]), [RebootHost])
        , ([RebootHost], [("vm1", []); ("vm2", [])])
        )
      ; ( (([RestartToolstack], [("vm1", []); ("vm2", [])]), [RebootHost])
        , ([RebootHost; RestartToolstack], [("vm1", []); ("vm2", [])])
        )
      ; ( ( ([RestartToolstack], [("vm1", [RestartDeviceModel]); ("vm2", [])])
          , [RebootHost]
          )
        , ( [RebootHost; RestartToolstack]
          , [("vm1", [RestartDeviceModel]); ("vm2", [])]
          )
        )
      ; ( ( ([RestartToolstack], [("vm1", [RestartDeviceModel]); ("vm2", [])])
          , [RestartDeviceModel]
          )
        , ( [RestartToolstack]
          , [("vm1", [RestartDeviceModel]); ("vm2", [RestartDeviceModel])]
          )
        )
      ; ( (([RebootHostOnLivePatchFailure], [("vm1", [])]), [RebootHost])
        , ([RebootHost; RebootHostOnLivePatchFailure], [("vm1", [])])
        )
      ; ( (([RebootHost], [("vm1", [])]), [RebootHostOnLivePatchFailure])
        , ([RebootHost; RebootHostOnLivePatchFailure], [("vm1", [])])
        )
      ; ( ( ( []
            , [("vm1", []); ("vm2", [RestartDeviceModel]); ("vm3", [RestartVM])]
            )
          , [RebootHost]
          )
        , ( [RebootHost]
          , [("vm1", []); ("vm2", [RestartDeviceModel]); ("vm3", [RestartVM])]
          )
        )
      ; ( ( ( [RestartToolstack]
            , [("vm1", []); ("vm2", [RestartDeviceModel]); ("vm3", [RestartVM])]
            )
          , [RebootHost]
          )
        , ( [RebootHost; RestartToolstack]
          , [("vm1", []); ("vm2", [RestartDeviceModel]); ("vm3", [RestartVM])]
          )
        )
      ; ( ( ( [RestartToolstack]
            , [("vm1", []); ("vm2", [RestartDeviceModel]); ("vm3", [RestartVM])]
            )
          , [RestartToolstack]
          )
        , ( [RestartToolstack]
          , [("vm1", []); ("vm2", [RestartDeviceModel]); ("vm3", [RestartVM])]
          )
        )
      ; ( ( ( [RestartToolstack]
            , [("vm1", []); ("vm2", [RestartDeviceModel]); ("vm3", [RestartVM])]
            )
          , [RestartDeviceModel]
          )
        , ( [RestartToolstack]
          , [
              ("vm1", [RestartDeviceModel])
            ; ("vm2", [RestartDeviceModel])
            ; ("vm3", [RestartDeviceModel; RestartVM])
            ]
          )
        )
      ; ( ( ( [RestartToolstack]
            , [("vm1", []); ("vm2", [RestartDeviceModel]); ("vm3", [RestartVM])]
            )
          , [RestartVM]
          )
        , ( [RestartToolstack]
          , [
              ("vm1", [RestartVM])
            ; ("vm2", [RestartVM; RestartDeviceModel])
            ; ("vm3", [RestartVM])
            ]
          )
        )
      ; ( ( ( [RestartToolstack; RebootHostOnXenLivePatchFailure]
            , [("vm1", []); ("vm2", [RestartDeviceModel]); ("vm3", [RestartVM])]
            )
          , [RestartToolstack]
          )
        , ( [RebootHostOnXenLivePatchFailure; RestartToolstack]
          , [("vm1", []); ("vm2", [RestartDeviceModel]); ("vm3", [RestartVM])]
          )
        )
      ; ( ( ( [RebootHost; RebootHostOnKernelLivePatchFailure]
            , [("vm1", []); ("vm2", [RestartDeviceModel]); ("vm3", [RestartVM])]
            )
          , [RestartToolstack; RestartVM]
          )
        , ( [RebootHost; RebootHostOnKernelLivePatchFailure; RestartToolstack]
          , [
              ("vm1", [RestartVM])
            ; ("vm2", [RestartVM; RestartDeviceModel])
            ; ("vm3", [RestartVM])
            ]
          )
        )
      ]
end)

module MergeLivepatchFailures = Generic.MakeStateless (struct
  module Io = struct
    (* (previous_failures, (applied, failed)) *)
    type input_t =
      Guidance.t list * (Livepatch.component list * Livepatch.component list)

    (* to_be_removed, to_be_added *)
    type output_t = Guidance.t list * Guidance.t list

    let string_of_input_t (previous_failures, (applied, failed)) =
      Fmt.(str "%a" Dump.(pair (list string) (pair (list string) (list string))))
        ( List.map Guidance.to_string previous_failures
        , ( List.map Livepatch.string_of_component applied
          , List.map Livepatch.string_of_component failed
          )
        )

    let string_of_output_t (to_be_removed, to_be_added) =
      Fmt.(str "%a" Dump.(pair (list string) (list string)))
        ( List.map Guidance.to_string to_be_removed
        , List.map Guidance.to_string to_be_added
        )
  end

  let transform (previous_failures, (applied, failed)) =
    merge_livepatch_failures ~previous_failures ~applied ~failed

  let tests =
    let open Guidance in
    let open Livepatch in
    `QuickAndAutoDocumented
      [
        (([], ([Xen; Kernel], [])), ([], []))
      ; (([], ([Kernel], [Xen])), ([], [RebootHostOnXenLivePatchFailure]))
      ; (([], ([Kernel], [])), ([], []))
      ; (([], ([Xen], [Kernel])), ([], [RebootHostOnKernelLivePatchFailure]))
      ; (([], ([Xen], [])), ([], []))
      ; ( ([], ([], [Xen; Kernel]))
        , ( []
          , [
              RebootHostOnKernelLivePatchFailure; RebootHostOnXenLivePatchFailure
            ]
          )
        )
      ; (([], ([], [Kernel])), ([], [RebootHostOnKernelLivePatchFailure]))
      ; (([], ([], [Xen])), ([], [RebootHostOnXenLivePatchFailure]))
      ; (([], ([], [])), ([], []))
      ; ( ([RebootHostOnXenLivePatchFailure], ([Xen; Kernel], []))
        , ([RebootHostOnXenLivePatchFailure], [])
        )
      ; (([RebootHostOnXenLivePatchFailure], ([Kernel], [Xen])), ([], []))
      ; (([RebootHostOnXenLivePatchFailure], ([Kernel], [])), ([], []))
      ; ( ([RebootHostOnXenLivePatchFailure], ([Xen], [Kernel]))
        , ( [RebootHostOnXenLivePatchFailure]
          , [RebootHostOnKernelLivePatchFailure]
          )
        )
      ; ( ([RebootHostOnXenLivePatchFailure], ([Xen], []))
        , ([RebootHostOnXenLivePatchFailure], [])
        )
      ; ( ([RebootHostOnXenLivePatchFailure], ([], [Xen; Kernel]))
        , ([], [RebootHostOnKernelLivePatchFailure])
        )
      ; ( ([RebootHostOnXenLivePatchFailure], ([], [Kernel]))
        , ([], [RebootHostOnKernelLivePatchFailure])
        )
      ; (([RebootHostOnXenLivePatchFailure], ([], [Xen])), ([], []))
      ; (([RebootHostOnXenLivePatchFailure], ([], [])), ([], []))
      ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
          , ([Xen; Kernel], [])
          )
        , ( [RebootHostOnKernelLivePatchFailure; RebootHostOnXenLivePatchFailure]
          , []
          )
        )
      ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
          , ([Kernel], [Xen])
          )
        , ([RebootHostOnKernelLivePatchFailure], [])
        )
      ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
          , ([Kernel], [])
          )
        , ([RebootHostOnKernelLivePatchFailure], [])
        )
      ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
          , ([Xen], [Kernel])
          )
        , ([RebootHostOnXenLivePatchFailure], [])
        )
      ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
          , ([Xen], [])
          )
        , ([RebootHostOnXenLivePatchFailure], [])
        )
      ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
          , ([], [Xen; Kernel])
          )
        , ([], [])
        )
      ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
          , ([], [Kernel])
          )
        , ([], [])
        )
      ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
          , ([], [Xen])
          )
        , ([], [])
        )
      ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
          , ([], [])
          )
        , ([], [])
        )
      ; ( ([RebootHostOnKernelLivePatchFailure], ([Xen; Kernel], []))
        , ([RebootHostOnKernelLivePatchFailure], [])
        )
      ; ( ([RebootHostOnKernelLivePatchFailure], ([Kernel], [Xen]))
        , ( [RebootHostOnKernelLivePatchFailure]
          , [RebootHostOnXenLivePatchFailure]
          )
        )
      ; ( ([RebootHostOnKernelLivePatchFailure], ([Kernel], []))
        , ([RebootHostOnKernelLivePatchFailure], [])
        )
      ; (([RebootHostOnKernelLivePatchFailure], ([Xen], [Kernel])), ([], []))
      ; (([RebootHostOnKernelLivePatchFailure], ([Xen], [])), ([], []))
      ; ( ([RebootHostOnKernelLivePatchFailure], ([], [Xen; Kernel]))
        , ([], [RebootHostOnXenLivePatchFailure])
        )
      ; (([RebootHostOnKernelLivePatchFailure], ([], [Kernel])), ([], []))
      ; ( ([RebootHostOnKernelLivePatchFailure], ([], [Xen]))
        , ([], [RebootHostOnXenLivePatchFailure])
        )
      ; (([RebootHostOnKernelLivePatchFailure], ([], [])), ([], []))
      ]
end)

let tests =
  make_suite "repository_helpers_"
    [
      ("update_of_json", UpdateOfJsonTest.tests)
    ; ("assert_url_is_valid", AssertUrlIsValid.tests)
    ; ("write_yum_config", WriteYumConfig.tests)
    ; ("eval_guidance_for_one_update", EvalGuidanceForOneUpdate.tests)
    ; ("get_update_in_json", GetUpdateInJson.tests)
    ; ("consolidate_updates_of_host", ConsolidateUpdatesOfHost.tests)
    ; ("parse_updateinfo_list", ParseUpdateInfoList.tests)
    ; ("guidance_set_resort", GuidanceSetResortTest.tests)
    ; ("guidance_set_reduce", GuidanceSetReduceTest.tests)
    ; ( "guidance_set_reduce_cascaded_list"
      , GuidanceSetReduceCascadedListTest.tests
      )
    ; ("prune_accumulative_updates", PruneAccumulativeUpdates.tests)
    ; ("prune_updateinfo_for_livepatches", PruneUpdateInfoForLivepatches.tests)
    ; ( "parse_output_of_yum_upgrade_dry_run"
      , ParseOutputOfYumUpgradeDryRun.tests
      )
    ; ( "get_latest_updates_from_redundancy"
      , GetLatestUpdatesFromRedundancy.tests
      )
    ; ("set_pending_guidances", SetPendingGuidance.tests)
    ; ("merge_livepatch_failures", MergeLivepatchFailures.tests)
    ]

let () = Alcotest.run "Repository Helpers" tests
