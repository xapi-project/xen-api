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

let rec combination' acc = function
  | [] ->
      acc @ [[]]
  | hd :: tl ->
      let acc' = [[hd]] @ List.map (fun l -> hd :: l) acc @ acc in
      combination' acc' tl

let combination l = combination' [] l

let join l1 l2 =
  List.fold_left
    (fun acc e1 -> List.fold_left (fun acc' e2 -> (e1, e2) :: acc') acc l2)
    [] l1

let join3 l2 l1 =
  List.fold_left
    (fun acc (x1, x2) ->
      List.fold_left (fun acc' y -> (x1, x2, y) :: acc') acc l2
    )
    [] l1

let join4 l2 l1 =
  List.fold_left
    (fun acc (x1, x2, x3) ->
      List.fold_left (fun acc' y -> (x1, x2, x3, y) :: acc') acc l2
    )
    [] l1

let guidances_to_string gs =
  "[" ^ (List.map Guidance.to_string gs |> String.concat "; ") ^ "]"

let livepatch_components_to_string components =
  "["
  ^ (List.map Livepatch.string_of_component components |> String.concat "; ")
  ^ "]"

let livepatching_result_to_string succeeded failed =
  let to_string = livepatch_components_to_string in
  "(" ^ to_string succeeded ^ ", " ^ to_string failed ^ ")"

let exhaustive_livepatching_results =
  let open Livepatch in
  [
    ([], [])
  ; ([], [Xen])
  ; ([], [Kernel])
  ; ([], [Xen; Kernel])
  ; ([Xen], [])
  ; ([Xen], [Kernel])
  ; ([Kernel], [])
  ; ([Kernel], [Xen])
  ; ([Xen; Kernel], [])
  ]

exception Mismatched_exhaustive_input of string * string

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
    ; field "livepatch_guidance"
        (fun (r : UpdateInfo.t) ->
          UpdateInfo.guidance_to_string r.livepatch_guidance
        )
        string
    ; field "livepatches"
        (fun (r : UpdateInfo.t) ->
          List.map
            (fun x -> x |> LivePatch.to_json |> Yojson.Basic.pretty_to_string)
            r.livepatches
        )
        (list string)
    ]

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
    type input_t = {
        updates_info: (string * UpdateInfo.t) list
      ; update: Update.t
      ; upd_ids_of_livepatches: string list
      ; upd_ids_of_failed_livepatches: string list
    }

    type output_t = Guidance.t option

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
        ; field "upd_ids_of_failed_livepatches"
            (fun (r : input_t) ->
              r.upd_ids_of_failed_livepatches
              |> String.concat ";"
              |> Printf.sprintf "[%s]"
            )
            string
        ]

    let string_of_input_t = Fmt.(str "%a" Dump.(record @@ fields_of_input))

    let string_of_output_t g =
      Fmt.(str "%a" Dump.(string)) (UpdateInfo.guidance_to_string g)
  end

  let transform
      Io.
        {
          updates_info
        ; update
        ; upd_ids_of_livepatches
        ; upd_ids_of_failed_livepatches
        } =
    eval_guidance_for_one_update ~updates_info ~update
      ~kind:Guidance.Recommended
      ~upd_ids_of_livepatches:(UpdateIdSet.of_list upd_ids_of_livepatches)
      ~upd_ids_of_failed_livepatches:
        (UpdateIdSet.of_list upd_ids_of_failed_livepatches)

  let tests =
    let open Io in
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
          ; upd_ids_of_failed_livepatches= []
          }
        , None
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
                    ; rec_guidance= Some Guidance.EvacuateHost
                    ; abs_guidance= Some Guidance.RebootHost
                    ; guidance_applicabilities= []
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
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
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
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
          ; upd_ids_of_failed_livepatches= []
          }
        , None
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
                    ; rec_guidance= Some Guidance.EvacuateHost
                    ; abs_guidance= Some Guidance.RebootHost
                    ; guidance_applicabilities= []
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
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
          ; upd_ids_of_failed_livepatches= []
          }
        , None
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
                    ; rec_guidance= None
                    ; abs_guidance= None
                    ; guidance_applicabilities= []
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    }
                )
              ; ( "UPDATE-0001"
                , UpdateInfo.
                    {
                      id= "UPDATE-0001"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RebootHost
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
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
          ; upd_ids_of_failed_livepatches= []
          }
        , Some Guidance.RebootHost
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
                    ; rec_guidance= None
                    ; abs_guidance= None
                    ; guidance_applicabilities= []
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    }
                )
              ; ( "UPDATE-0001"
                , UpdateInfo.
                    {
                      id= "UPDATE-0001"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RestartDeviceModel
                    ; abs_guidance= None
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
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
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
          ; upd_ids_of_failed_livepatches= []
          }
        , Some Guidance.RestartDeviceModel
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
                    ; rec_guidance= None
                    ; abs_guidance= None
                    ; guidance_applicabilities= []
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    }
                )
              ; ( "UPDATE-0001"
                , UpdateInfo.
                    {
                      id= "UPDATE-0001"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RestartDeviceModel
                    ; abs_guidance= None
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
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
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
          ; upd_ids_of_failed_livepatches= []
          }
        , Some Guidance.RestartDeviceModel
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
                    ; rec_guidance= None
                    ; abs_guidance= None
                    ; guidance_applicabilities= []
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    }
                )
              ; ( "UPDATE-0001"
                , UpdateInfo.
                    {
                      id= "UPDATE-0001"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RestartDeviceModel
                    ; abs_guidance= None
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
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
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
          ; upd_ids_of_failed_livepatches= []
          }
        , None
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
                    ; rec_guidance= None
                    ; abs_guidance= None
                    ; guidance_applicabilities= []
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    }
                )
              ; ( "UPDATE-0001"
                , UpdateInfo.
                    {
                      id= "UPDATE-0001"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RestartDeviceModel
                    ; abs_guidance= None
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
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
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
          ; upd_ids_of_failed_livepatches= []
          }
        , None
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
                    ; rec_guidance= None
                    ; abs_guidance= None
                    ; guidance_applicabilities= []
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
                    }
                )
              ; ( "UPDATE-0001"
                , UpdateInfo.
                    {
                      id= "UPDATE-0001"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RestartDeviceModel
                    ; abs_guidance= None
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
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
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
          ; upd_ids_of_failed_livepatches= []
          }
        , Some Guidance.RestartDeviceModel
        )
      ; (* livepatch_guidance: Some _ *)
        ( {
            updates_info=
              [
                ( "UPDATE-0000"
                , UpdateInfo.
                    {
                      id= "UPDATE-0000"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RebootHost
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= Some Guidance.RestartDeviceModel
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
          ; upd_ids_of_failed_livepatches= []
          }
        , Some Guidance.RestartDeviceModel
        )
      ; (* livepatch_guidance - None *)
        ( {
            updates_info=
              [
                ( "UPDATE-0000"
                , UpdateInfo.
                    {
                      id= "UPDATE-0000"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RebootHost
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
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
          ; upd_ids_of_failed_livepatches= []
          }
        , None
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
                    ; rec_guidance= None
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= Some Guidance.RestartDeviceModel
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
                    }
                )
              ; ( "UPDATE-0001"
                , UpdateInfo.
                    {
                      id= "UPDATE-0001"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RebootHost
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= Some Guidance.RestartToolstack
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
          ; upd_ids_of_failed_livepatches= []
          }
        , Some Guidance.RebootHost
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
                    ; rec_guidance= Some Guidance.RebootHost
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= Some Guidance.RestartDeviceModel
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
                    }
                )
              ; ( "UPDATE-0001"
                , UpdateInfo.
                    {
                      id= "UPDATE-0001"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RebootHost
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= Some Guidance.RestartToolstack
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
          ; upd_ids_of_failed_livepatches= []
          }
        , Some Guidance.RestartToolstack
        )
      ; (* livepatch_guidance: latest update doesn't have livepatch and recommendedGuidance is None *)
        ( {
            updates_info=
              [
                ( "UPDATE-0000"
                , UpdateInfo.
                    {
                      id= "UPDATE-0000"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RebootHost
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
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
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
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
          ; upd_ids_of_failed_livepatches= []
          }
        , None
        )
      ; (* livepatch_guidance: latest update doesn't have livepatch and recommendedGuidance is RebootHost *)
        ( {
            updates_info=
              [
                ( "UPDATE-0000"
                , UpdateInfo.
                    {
                      id= "UPDATE-0000"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RebootHost
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
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
                    }
                )
              ; ( "UPDATE-0001"
                , UpdateInfo.
                    {
                      id= "UPDATE-0001"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RebootHost
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
                    ; livepatches= []
                    ; issued= Xapi_stdext_date.Date.epoch
                    ; severity= Severity.None
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
          ; upd_ids_of_failed_livepatches= []
          }
        , Some Guidance.RebootHost
        )
      ; (* livepatch_guidance: failure of applying livepatch *)
        ( {
            updates_info=
              [
                ( "UPDATE-0000"
                , UpdateInfo.
                    {
                      id= "UPDATE-0000"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RebootHost
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= Some Guidance.RestartToolstack
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
                    }
                )
              ; ( "UPDATE-0001"
                , UpdateInfo.
                    {
                      id= "UPDATE-0001"
                    ; summary= "summary"
                    ; description= "description"
                    ; rec_guidance= Some Guidance.RebootHost
                    ; abs_guidance= None
                    ; guidance_applicabilities= [] (* No applicabilities *)
                    ; spec_info= "special info"
                    ; url= "https://update.details.info"
                    ; update_type= "security"
                    ; livepatch_guidance= None
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
          ; upd_ids_of_failed_livepatches= ["UPDATE-0001"]
          }
        , None
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
      ; livepatch_guidance= None
      ; livepatches= []
      ; issued= Xapi_stdext_date.Date.epoch
      ; severity= Severity.None
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
              ; ("recommended-guidance", `List [])
              ; ("absolute-guidance", `List [])
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
              ; ("recommended-guidance", `List [`String "RebootHost"])
              ; ("absolute-guidance", `List [])
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
              ; ("recommended-guidance", `List [`String "EvacuateHost"])
              ; ("absolute-guidance", `List [])
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
    |> GuidanceSet.resort_guidances
         ~remove_evacuations:(kind = Guidance.Absolute)
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
      ; rec_guidance= None
      ; abs_guidance= None
      ; guidance_applicabilities= []
      ; spec_info= "SPEC_INFO"
      ; url= "URL"
      ; update_type= "UPDATE_TYPE"
      ; livepatch_guidance= None
      ; livepatches= []
      ; issued= Xapi_stdext_date.Date.epoch
      ; severity= Severity.None
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
          Fmt.(
            str "%a" Dump.(list (pair (record @@ fields_of_pkg) string)) pkgs
          )
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

module MergeWithUnappliedGuidances' = struct
  module Io = struct
    (* (unapplied, coming) *)
    type input_t = Guidance.t list * Guidance.t list

    (* (to_be_added, to_be_removed, unchanged) *)
    type output_t = Guidance.t list * Guidance.t list * Guidance.t list

    let string_of_input_t (unapplied, coming) =
      Fmt.(str "%a" Dump.(pair (list string) (list string)))
        ( List.map Guidance.to_string unapplied
        , List.map Guidance.to_string coming
        )

    let string_of_output_t (to_be_added, to_be_removed, unchanged) =
      "to_be_added:"
      ^ Fmt.(str "%a" Dump.(list string))
          (List.map Guidance.to_string to_be_added)
      ^ ", to_be_removed:"
      ^ Fmt.(str "%a" Dump.(list string))
          (List.map Guidance.to_string to_be_removed)
      ^ ", unchanged:"
      ^ Fmt.(str "%a" Dump.(list string)) (List.map Guidance.to_string unchanged)
  end

  let transform (unapplied, coming) =
    merge_with_unapplied_guidances ~unapplied ~coming

  let tests' =
    let open Guidance in
    [
      (([], []), ([], [], []))
    ; (([], [RebootHost]), ([RebootHost], [], []))
    ; (([], [RebootHost; RestartToolstack]), ([RebootHost], [], []))
    ; (([], [RestartToolstack]), ([RestartToolstack], [], []))
    ; (([], [RebootHost; RestartDeviceModel]), ([RebootHost], [], []))
    ; ( ([], [RebootHost; RestartToolstack; RestartDeviceModel])
      , ([RebootHost], [], [])
      )
    ; ( ([], [RestartToolstack; RestartDeviceModel])
      , ([RestartToolstack; RestartDeviceModel], [], [])
      )
    ; (([], [RestartDeviceModel]), ([RestartDeviceModel], [], []))
    ; (([RebootHost], []), ([], [], [RebootHost]))
    ; (([RebootHost], [RebootHost]), ([RebootHost], [], []))
    ; (([RebootHost], [RebootHost; RestartToolstack]), ([RebootHost], [], []))
    ; (([RebootHost], [RestartToolstack]), ([], [], [RebootHost]))
    ; (([RebootHost], [RebootHost; RestartDeviceModel]), ([RebootHost], [], []))
    ; ( ([RebootHost], [RebootHost; RestartToolstack; RestartDeviceModel])
      , ([RebootHost], [], [])
      )
    ; ( ([RebootHost], [RestartToolstack; RestartDeviceModel])
      , ([], [], [RebootHost])
      )
    ; (([RebootHost], [RestartDeviceModel]), ([], [], [RebootHost]))
    ; ( ([RebootHost; RestartToolstack], [])
      , ([], [RestartToolstack], [RebootHost])
      )
    ; ( ([RebootHost; RestartToolstack], [RebootHost])
      , ([RebootHost], [RestartToolstack], [])
      )
    ; ( ([RebootHost; RestartToolstack], [RebootHost; RestartToolstack])
      , ([RebootHost], [RestartToolstack], [])
      )
    ; ( ([RebootHost; RestartToolstack], [RestartToolstack])
      , ([], [RestartToolstack], [RebootHost])
      )
    ; ( ([RebootHost; RestartToolstack], [RebootHost; RestartDeviceModel])
      , ([RebootHost], [RestartToolstack], [])
      )
    ; ( ( [RebootHost; RestartToolstack]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        )
      , ([RebootHost], [RestartToolstack], [])
      )
    ; ( ([RebootHost; RestartToolstack], [RestartToolstack; RestartDeviceModel])
      , ([], [RestartToolstack], [RebootHost])
      )
    ; ( ([RebootHost; RestartToolstack], [RestartDeviceModel])
      , ([], [RestartToolstack], [RebootHost])
      )
    ; (([RestartToolstack], []), ([], [], [RestartToolstack]))
    ; ( ([RestartToolstack], [RebootHost])
      , ([RebootHost], [RestartToolstack], [])
      )
    ; ( ([RestartToolstack], [RebootHost; RestartToolstack])
      , ([RebootHost], [RestartToolstack], [])
      )
    ; (([RestartToolstack], [RestartToolstack]), ([RestartToolstack], [], []))
    ; ( ([RestartToolstack], [RebootHost; RestartDeviceModel])
      , ([RebootHost], [RestartToolstack], [])
      )
    ; ( ([RestartToolstack], [RebootHost; RestartToolstack; RestartDeviceModel])
      , ([RebootHost], [RestartToolstack], [])
      )
    ; ( ([RestartToolstack], [RestartToolstack; RestartDeviceModel])
      , ([RestartToolstack; RestartDeviceModel], [], [])
      )
    ; ( ([RestartToolstack], [RestartDeviceModel])
      , ([RestartDeviceModel], [], [RestartToolstack])
      )
    ; ( ([RebootHost; RestartDeviceModel], [])
      , ([], [RestartDeviceModel], [RebootHost])
      )
    ; ( ([RebootHost; RestartDeviceModel], [RebootHost])
      , ([RebootHost], [RestartDeviceModel], [])
      )
    ; ( ([RebootHost; RestartDeviceModel], [RebootHost; RestartToolstack])
      , ([RebootHost], [RestartDeviceModel], [])
      )
    ; ( ([RebootHost; RestartDeviceModel], [RestartToolstack])
      , ([], [RestartDeviceModel], [RebootHost])
      )
    ; ( ([RebootHost; RestartDeviceModel], [RebootHost; RestartDeviceModel])
      , ([RebootHost], [RestartDeviceModel], [])
      )
    ; ( ( [RebootHost; RestartDeviceModel]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        )
      , ([RebootHost], [RestartDeviceModel], [])
      )
    ; ( ( [RebootHost; RestartDeviceModel]
        , [RestartToolstack; RestartDeviceModel]
        )
      , ([], [RestartDeviceModel], [RebootHost])
      )
    ; ( ([RebootHost; RestartDeviceModel], [RestartDeviceModel])
      , ([], [RestartDeviceModel], [RebootHost])
      )
    ; ( ([RebootHost; RestartToolstack; RestartDeviceModel], [])
      , ([], [RestartToolstack; RestartDeviceModel], [RebootHost])
      )
    ; ( ([RebootHost; RestartToolstack; RestartDeviceModel], [RebootHost])
      , ([RebootHost], [RestartToolstack; RestartDeviceModel], [])
      )
    ; ( ( [RebootHost; RestartToolstack; RestartDeviceModel]
        , [RebootHost; RestartToolstack]
        )
      , ([RebootHost], [RestartToolstack; RestartDeviceModel], [])
      )
    ; ( ([RebootHost; RestartToolstack; RestartDeviceModel], [RestartToolstack])
      , ([], [RestartToolstack; RestartDeviceModel], [RebootHost])
      )
    ; ( ( [RebootHost; RestartToolstack; RestartDeviceModel]
        , [RebootHost; RestartDeviceModel]
        )
      , ([RebootHost], [RestartToolstack; RestartDeviceModel], [])
      )
    ; ( ( [RebootHost; RestartToolstack; RestartDeviceModel]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        )
      , ([RebootHost], [RestartToolstack; RestartDeviceModel], [])
      )
    ; ( ( [RebootHost; RestartToolstack; RestartDeviceModel]
        , [RestartToolstack; RestartDeviceModel]
        )
      , ([], [RestartToolstack; RestartDeviceModel], [RebootHost])
      )
    ; ( ( [RebootHost; RestartToolstack; RestartDeviceModel]
        , [RestartDeviceModel]
        )
      , ([], [RestartToolstack; RestartDeviceModel], [RebootHost])
      )
    ; ( ([RestartToolstack; RestartDeviceModel], [])
      , ([], [], [RestartToolstack; RestartDeviceModel])
      )
    ; ( ([RestartToolstack; RestartDeviceModel], [RebootHost])
      , ([RebootHost], [RestartToolstack; RestartDeviceModel], [])
      )
    ; ( ([RestartToolstack; RestartDeviceModel], [RebootHost; RestartToolstack])
      , ([RebootHost], [RestartToolstack; RestartDeviceModel], [])
      )
    ; ( ([RestartToolstack; RestartDeviceModel], [RestartToolstack])
      , ([RestartToolstack], [], [RestartDeviceModel])
      )
    ; ( ( [RestartToolstack; RestartDeviceModel]
        , [RebootHost; RestartDeviceModel]
        )
      , ([RebootHost], [RestartToolstack; RestartDeviceModel], [])
      )
    ; ( ( [RestartToolstack; RestartDeviceModel]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        )
      , ([RebootHost], [RestartToolstack; RestartDeviceModel], [])
      )
    ; ( ( [RestartToolstack; RestartDeviceModel]
        , [RestartToolstack; RestartDeviceModel]
        )
      , ([RestartToolstack; RestartDeviceModel], [], [])
      )
    ; ( ([RestartToolstack; RestartDeviceModel], [RestartDeviceModel])
      , ([RestartDeviceModel], [], [RestartToolstack])
      )
    ; (([RestartDeviceModel], []), ([], [], [RestartDeviceModel]))
    ; ( ([RestartDeviceModel], [RebootHost])
      , ([RebootHost], [RestartDeviceModel], [])
      )
    ; ( ([RestartDeviceModel], [RebootHost; RestartToolstack])
      , ([RebootHost], [RestartDeviceModel], [])
      )
    ; ( ([RestartDeviceModel], [RestartToolstack])
      , ([RestartToolstack], [], [RestartDeviceModel])
      )
    ; ( ([RestartDeviceModel], [RebootHost; RestartDeviceModel])
      , ([RebootHost], [RestartDeviceModel], [])
      )
    ; ( ( [RestartDeviceModel]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        )
      , ([RebootHost], [RestartDeviceModel], [])
      )
    ; ( ([RestartDeviceModel], [RestartToolstack; RestartDeviceModel])
      , ([RestartToolstack; RestartDeviceModel], [], [])
      )
    ; ( ([RestartDeviceModel], [RestartDeviceModel])
      , ([RestartDeviceModel], [], [])
      )
    ]

  let string_of_inputs input1 input2 =
    "(" ^ guidances_to_string input1 ^ ", " ^ guidances_to_string input2 ^ ")"

  let strings_of_tests_inputs =
    List.map
      (fun ((input1, input2), _) -> string_of_inputs input1 input2)
      tests'

  let strings_of_exhaustive_inputs =
    let open Guidance in
    let exhaustive_unapplied_inputs =
      combination [RebootHost; RestartToolstack; RestartDeviceModel]
      |> List.map (fun l -> List.sort Guidance.compare l)
    in
    let exhaustive_coming =
      combination [RebootHost; RestartToolstack; RestartDeviceModel]
      |> List.map (fun l -> List.sort Guidance.compare l)
    in
    join exhaustive_unapplied_inputs exhaustive_coming
    |> List.map (fun (input1, input2) -> string_of_inputs input1 input2)

  let tests = `QuickAndAutoDocumented tests'
end

module MergeWithUnappliedGuidances =
  Generic.MakeStateless (MergeWithUnappliedGuidances')

module MergeLivepatchFailures' = struct
  module Io = struct
    (* (previous_failures, (succeeded, failed)) *)
    type input_t =
      Guidance.t list * (Livepatch.component list * Livepatch.component list)

    (* failure_guidances *)
    type output_t = Guidance.t list

    let string_of_input_t (previous_failures, (succeeded, failed)) =
      Fmt.(
        str "%a" Dump.(pair (list string) (pair (list string) (list string)))
      )
        ( List.map Guidance.to_string previous_failures
        , ( List.map Livepatch.string_of_component succeeded
          , List.map Livepatch.string_of_component failed
          )
        )

    let string_of_output_t failure_guidances =
      Fmt.(str "%a" Dump.(list string))
        (List.map Guidance.to_string failure_guidances)
  end

  let transform (previous_failures, (succeeded, failed)) =
    merge_livepatch_failures' ~previous_failures ~succeeded ~failed

  let tests' =
    let open Guidance in
    let open Livepatch in
    [
      (([], ([Xen; Kernel], [])), [])
    ; (([], ([Kernel], [Xen])), [RebootHostOnXenLivePatchFailure])
    ; (([], ([Kernel], [])), [])
    ; (([], ([Xen], [Kernel])), [RebootHostOnKernelLivePatchFailure])
    ; (([], ([Xen], [])), [])
    ; ( ([], ([], [Xen; Kernel]))
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; (([], ([], [Kernel])), [RebootHostOnKernelLivePatchFailure])
    ; (([], ([], [Xen])), [RebootHostOnXenLivePatchFailure])
    ; (([], ([], [])), [])
    ; (([RebootHostOnLivePatchFailure], ([Xen; Kernel], [])), [])
    ; ( ([RebootHostOnLivePatchFailure], ([Kernel], [Xen]))
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ([RebootHostOnLivePatchFailure], ([Kernel], []))
      , [RebootHostOnLivePatchFailure]
      )
    ; ( ([RebootHostOnLivePatchFailure], ([Xen], [Kernel]))
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; ( ([RebootHostOnLivePatchFailure], ([Xen], []))
      , [RebootHostOnLivePatchFailure]
      )
    ; ( ([RebootHostOnLivePatchFailure], ([], [Xen; Kernel]))
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ([RebootHostOnLivePatchFailure], ([], [Kernel]))
      , [RebootHostOnLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ([RebootHostOnLivePatchFailure], ([], [Xen]))
      , [RebootHostOnLivePatchFailure; RebootHostOnXenLivePatchFailure]
      )
    ; ( ([RebootHostOnLivePatchFailure], ([], []))
      , [RebootHostOnLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnXenLivePatchFailure]
        , ([Xen; Kernel], [])
        )
      , []
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnXenLivePatchFailure]
        , ([Kernel], [Xen])
        )
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnXenLivePatchFailure]
        , ([Kernel], [])
        )
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnXenLivePatchFailure]
        , ([Xen], [Kernel])
        )
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnXenLivePatchFailure]
        , ([Xen], [])
        )
      , [RebootHostOnLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnXenLivePatchFailure]
        , ([], [Xen; Kernel])
        )
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnXenLivePatchFailure]
        , ([], [Kernel])
        )
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnXenLivePatchFailure]
        , ([], [Xen])
        )
      , [RebootHostOnLivePatchFailure; RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnXenLivePatchFailure]
        , ([], [])
        )
      , [RebootHostOnXenLivePatchFailure; RebootHostOnLivePatchFailure]
      )
    ; (([RebootHostOnXenLivePatchFailure], ([Xen; Kernel], [])), [])
    ; ( ([RebootHostOnXenLivePatchFailure], ([Kernel], [Xen]))
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ([RebootHostOnXenLivePatchFailure], ([Kernel], []))
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ([RebootHostOnXenLivePatchFailure], ([Xen], [Kernel]))
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; (([RebootHostOnXenLivePatchFailure], ([Xen], [])), [])
    ; ( ([RebootHostOnXenLivePatchFailure], ([], [Xen; Kernel]))
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ([RebootHostOnXenLivePatchFailure], ([], [Kernel]))
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ([RebootHostOnXenLivePatchFailure], ([], [Xen]))
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ([RebootHostOnXenLivePatchFailure], ([], []))
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([Xen; Kernel], [])
        )
      , []
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([Kernel], [Xen])
        )
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([Kernel], [])
        )
      , [RebootHostOnLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([Xen], [Kernel])
        )
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([Xen], [])
        )
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([], [Xen; Kernel])
        )
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([], [Kernel])
        )
      , [RebootHostOnLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([], [Xen])
        )
      , [RebootHostOnKernelLivePatchFailure; RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [RebootHostOnLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([], [])
        )
      , [RebootHostOnKernelLivePatchFailure; RebootHostOnLivePatchFailure]
      )
    ; ( ( [
            RebootHostOnLivePatchFailure
          ; RebootHostOnXenLivePatchFailure
          ; RebootHostOnKernelLivePatchFailure
          ]
        , ([Xen; Kernel], [])
        )
      , []
      )
    ; ( ( [
            RebootHostOnLivePatchFailure
          ; RebootHostOnXenLivePatchFailure
          ; RebootHostOnKernelLivePatchFailure
          ]
        , ([Kernel], [Xen])
        )
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [
            RebootHostOnLivePatchFailure
          ; RebootHostOnXenLivePatchFailure
          ; RebootHostOnKernelLivePatchFailure
          ]
        , ([Kernel], [])
        )
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [
            RebootHostOnLivePatchFailure
          ; RebootHostOnXenLivePatchFailure
          ; RebootHostOnKernelLivePatchFailure
          ]
        , ([Xen], [Kernel])
        )
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [
            RebootHostOnLivePatchFailure
          ; RebootHostOnXenLivePatchFailure
          ; RebootHostOnKernelLivePatchFailure
          ]
        , ([Xen], [])
        )
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [
            RebootHostOnLivePatchFailure
          ; RebootHostOnXenLivePatchFailure
          ; RebootHostOnKernelLivePatchFailure
          ]
        , ([], [Xen; Kernel])
        )
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [
            RebootHostOnLivePatchFailure
          ; RebootHostOnXenLivePatchFailure
          ; RebootHostOnKernelLivePatchFailure
          ]
        , ([], [Kernel])
        )
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [
            RebootHostOnLivePatchFailure
          ; RebootHostOnXenLivePatchFailure
          ; RebootHostOnKernelLivePatchFailure
          ]
        , ([], [Xen])
        )
      , [RebootHostOnKernelLivePatchFailure; RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [
            RebootHostOnLivePatchFailure
          ; RebootHostOnXenLivePatchFailure
          ; RebootHostOnKernelLivePatchFailure
          ]
        , ([], [])
        )
      , [RebootHostOnKernelLivePatchFailure; RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([Xen; Kernel], [])
        )
      , []
      )
    ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([Kernel], [Xen])
        )
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([Kernel], [])
        )
      , [RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([Xen], [Kernel])
        )
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([Xen], [])
        )
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([], [Xen; Kernel])
        )
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([], [Kernel])
        )
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([], [Xen])
        )
      , [RebootHostOnKernelLivePatchFailure; RebootHostOnXenLivePatchFailure]
      )
    ; ( ( [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
        , ([], [])
        )
      , [RebootHostOnKernelLivePatchFailure; RebootHostOnXenLivePatchFailure]
      )
    ; (([RebootHostOnKernelLivePatchFailure], ([Xen; Kernel], [])), [])
    ; ( ([RebootHostOnKernelLivePatchFailure], ([Kernel], [Xen]))
      , [RebootHostOnXenLivePatchFailure]
      )
    ; (([RebootHostOnKernelLivePatchFailure], ([Kernel], [])), [])
    ; ( ([RebootHostOnKernelLivePatchFailure], ([Xen], [Kernel]))
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; ( ([RebootHostOnKernelLivePatchFailure], ([Xen], []))
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; ( ([RebootHostOnKernelLivePatchFailure], ([], [Xen; Kernel]))
      , [RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure]
      )
    ; ( ([RebootHostOnKernelLivePatchFailure], ([], [Kernel]))
      , [RebootHostOnKernelLivePatchFailure]
      )
    ; ( ([RebootHostOnKernelLivePatchFailure], ([], [Xen]))
      , [RebootHostOnKernelLivePatchFailure; RebootHostOnXenLivePatchFailure]
      )
    ; ( ([RebootHostOnKernelLivePatchFailure], ([], []))
      , [RebootHostOnKernelLivePatchFailure]
      )
    ]

  let string_of_inputs failures succeeded failed =
    "("
    ^ guidances_to_string failures
    ^ ", "
    ^ livepatching_result_to_string succeeded failed
    ^ ")"

  let strings_of_tests_inputs =
    List.map
      (fun ((failures, (succeeded, failed)), _) ->
        string_of_inputs failures succeeded failed
      )
      tests'

  let strings_of_exhaustive_inputs =
    (* (previous_failures, (succeeded, failed)) *)
    let open Guidance in
    let exhaustive_previous_failures =
      combination
        [
          RebootHostOnLivePatchFailure
        ; RebootHostOnXenLivePatchFailure
        ; RebootHostOnKernelLivePatchFailure
        ]
      |> List.map (fun l -> List.sort Guidance.compare l)
    in
    join exhaustive_previous_failures exhaustive_livepatching_results
    |> List.map (fun (failures, (succeeded, failed)) ->
           string_of_inputs failures succeeded failed
       )

  let tests = `QuickAndAutoDocumented tests'
end

module MergeLivepatchFailures = Generic.MakeStateless (MergeLivepatchFailures')

module MergePendingGuidances' = struct
  module Io = struct
    (* (host_pending_guidances, vms_pending_guidances, coming_guidances, succ_lps, fail_lps) *)
    type input_t =
      (string * Guidance.t list)
      * (string * Guidance.t list) list
      * Guidance.t list
      * (Livepatch.component list * Livepatch.component list)

    (* updated: (hosts_pending_guidances, vms_pending_guidances) *)
    type output_t =
      (string * Guidance.t list) list * (string * Guidance.t list) list

    let of_guidances gs = List.map Guidance.to_string gs

    let of_many_guidances gss =
      List.map (fun (host_or_vm, gs) -> (host_or_vm, of_guidances gs)) gss

    let string_of_input_t
        ( host_pending_guidances
        , vms_pending_guidances
        , coming_guidances
        , (succ_lps, fail_lps)
        ) =
      "host_pending_guidances:"
      ^ Fmt.(str "%a" Dump.(pair string (list string)))
          ( host_pending_guidances |> fun (h, guidances) ->
            (h, of_guidances guidances)
          )
      ^ ", vms_pending_guidances:"
      ^ Fmt.(str "%a" Dump.(list (pair string (list string))))
          (of_many_guidances vms_pending_guidances)
      ^ ", coming_guidances:"
      ^ Fmt.(str "%a" Dump.(list string))
          (List.map Guidance.to_string coming_guidances)
      ^ ", livepatching_results:"
      ^ Fmt.(str "%a" Dump.(pair (list string) (list string)))
          ( List.map Livepatch.string_of_component succ_lps
          , List.map Livepatch.string_of_component fail_lps
          )

    let string_of_output_t (hosts_pending_guidances, vms_pending_guidances) =
      "hosts_pending_guidances:"
      ^ Fmt.(str "%a" Dump.(list (pair string (list string))))
          (of_many_guidances hosts_pending_guidances)
      ^ ", vms_pending_guidances:"
      ^ Fmt.(str "%a" Dump.(list (pair string (list string))))
          (of_many_guidances vms_pending_guidances)
  end

  let transform
      ( host_pending_guidances
      , vms_pending_guidances
      , coming_guidances
      , (succ_lps, fail_lps)
      ) =
    let hosts_pending_guidances' :
        ( string
        , [> `reboot_host
          | `reboot_host_on_livepatch_failure
          | `restart_device_model
          | `restart_toolstack
          | `reboot_host_on_xen_livepatch_failure
          | `reboot_host_on_kernel_livepatch_failure ]
          list
        )
        Hashtbl.t =
      Hashtbl.create 1
    in
    let vms_pending_guidances' :
        ( string
        , [> `reboot_host
          | `reboot_host_on_livepatch_failure
          | `restart_device_model
          | `restart_toolstack
          | `reboot_host_on_xen_livepatch_failure
          | `reboot_host_on_kernel_livepatch_failure ]
          list
        )
        Hashtbl.t =
      Hashtbl.create 4
    in
    let to_pendings gss =
      List.map
        (fun (host_or_vm, gs) ->
          (host_or_vm, List.filter_map Guidance.to_pending_guidance gs)
        )
        gss
    in
    Hashtbl.add_seq hosts_pending_guidances'
      (List.to_seq (to_pendings [host_pending_guidances])) ;
    Hashtbl.add_seq vms_pending_guidances'
      (List.to_seq (to_pendings vms_pending_guidances)) ;
    let merge_method action host_or_vm =
      match (action, host_or_vm) with
      | `add_pending_guidances, `host ->
          fun ref_str value ->
            let guidances = Hashtbl.find hosts_pending_guidances' ref_str in
            value :: List.filter (fun g -> g <> value) guidances
            |> Hashtbl.replace hosts_pending_guidances' ref_str
      | `add_pending_guidances, `vm ->
          fun ref_str value ->
            let guidances = Hashtbl.find vms_pending_guidances' ref_str in
            value :: List.filter (fun g -> g <> value) guidances
            |> Hashtbl.replace vms_pending_guidances' ref_str
      | `remove_pending_guidances, `host ->
          fun ref_str value ->
            let guidances = Hashtbl.find hosts_pending_guidances' ref_str in
            Hashtbl.replace hosts_pending_guidances' ref_str
              (List.filter (fun g -> g <> value) guidances)
      | `remove_pending_guidances, `vm ->
          fun ref_str value ->
            let guidances = Hashtbl.find vms_pending_guidances' ref_str in
            Hashtbl.replace vms_pending_guidances' ref_str
              (List.filter (fun g -> g <> value) guidances)
    in
    merge_pending_guidances ~merge_method ~host_pending_guidances
      ~vms_pending_guidances ~coming_guidances ~succ_lps ~fail_lps ;
    ( hosts_pending_guidances'
      |> Hashtbl.to_seq
      |> List.of_seq
      |> List.map (fun (host, pendings) ->
             (host, List.map Guidance.of_pending_guidance pendings)
         )
    , vms_pending_guidances'
      |> Hashtbl.to_seq
      |> List.of_seq
      |> List.map (fun (vm, pendings) ->
             (vm, List.map Guidance.of_pending_guidance pendings)
         )
    )

  let tests' =
    let open Guidance in
    let open Livepatch in
    [
      ( (("host1", []), [], [RebootHost], ([Xen; Kernel], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ( (("host1", []), [], [RebootHost], ([Kernel], [Xen]))
      , ([("host1", [RebootHostOnXenLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", []), [], [RebootHost], ([Kernel], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ( (("host1", []), [], [RebootHost], ([Xen], [Kernel]))
      , ([("host1", [RebootHostOnKernelLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", []), [], [RebootHost], ([Xen], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ( (("host1", []), [], [RebootHost], ([], [Xen; Kernel]))
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHostOnXenLivePatchFailure
              ; RebootHost
              ]
            )
          ]
        , []
        )
      )
    ; ( (("host1", []), [], [RebootHost], ([], [Kernel]))
      , ([("host1", [RebootHostOnKernelLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", []), [], [RebootHost], ([], [Xen]))
      , ([("host1", [RebootHostOnXenLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", []), [], [RebootHost], ([], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ((("host1", []), [], [], ([Xen; Kernel], [])), ([("host1", [])], []))
    ; ( (("host1", []), [], [], ([Kernel], [Xen]))
      , ([("host1", [RebootHostOnXenLivePatchFailure])], [])
      )
    ; ((("host1", []), [], [], ([Kernel], [])), ([("host1", [])], []))
    ; ( (("host1", []), [], [], ([Xen], [Kernel]))
      , ([("host1", [RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ((("host1", []), [], [], ([Xen], [])), ([("host1", [])], []))
    ; ( (("host1", []), [], [], ([], [Xen; Kernel]))
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHostOnXenLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( (("host1", []), [], [], ([], [Kernel]))
      , ([("host1", [RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( (("host1", []), [], [], ([], [Xen]))
      , ([("host1", [RebootHostOnXenLivePatchFailure])], [])
      )
    ; ((("host1", []), [], [], ([], [])), ([("host1", [])], []))
    ; ( (("host1", [RebootHost]), [], [RebootHost], ([Xen; Kernel], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [RebootHost], ([Kernel], [Xen]))
      , ([("host1", [RebootHostOnXenLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [RebootHost], ([Kernel], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [RebootHost], ([Xen], [Kernel]))
      , ([("host1", [RebootHostOnKernelLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [RebootHost], ([Xen], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [RebootHost], ([], [Xen; Kernel]))
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHostOnXenLivePatchFailure
              ; RebootHost
              ]
            )
          ]
        , []
        )
      )
    ; ( (("host1", [RebootHost]), [], [RebootHost], ([], [Kernel]))
      , ([("host1", [RebootHostOnKernelLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [RebootHost], ([], [Xen]))
      , ([("host1", [RebootHostOnXenLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [RebootHost], ([], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [], ([Xen; Kernel], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [], ([Kernel], [Xen]))
      , ([("host1", [RebootHostOnXenLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [], ([Kernel], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [], ([Xen], [Kernel]))
      , ([("host1", [RebootHostOnKernelLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [], ([Xen], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [], ([], [Xen; Kernel]))
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHostOnXenLivePatchFailure
              ; RebootHost
              ]
            )
          ]
        , []
        )
      )
    ; ( (("host1", [RebootHost]), [], [], ([], [Kernel]))
      , ([("host1", [RebootHostOnKernelLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [], ([], [Xen]))
      , ([("host1", [RebootHostOnXenLivePatchFailure; RebootHost])], [])
      )
    ; ( (("host1", [RebootHost]), [], [], ([], []))
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen; Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([Kernel], [Xen])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([Kernel], [])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen], [Kernel])
        )
      , ([("host1", [RebootHostOnKernelLivePatchFailure; RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHost
              ; RebootHostOnXenLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHost
              ; RebootHostOnXenLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Xen])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , []
        , ([Xen; Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , []
        , ([Kernel], [Xen])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , []
        , ([Kernel], [])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , []
        , ([Xen], [Kernel])
        )
      , ([("host1", [RebootHostOnKernelLivePatchFailure; RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , []
        , ([Xen], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , []
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHost
              ; RebootHostOnXenLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , []
        , ([], [Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHost
              ; RebootHostOnXenLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , []
        , ([], [Xen])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnXenLivePatchFailure])
        , []
        , []
        , ([], [])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen; Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([Kernel], [Xen])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([Kernel], [])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen], [Kernel])
        )
      , ([("host1", [RebootHostOnKernelLivePatchFailure; RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHost
              ; RebootHostOnXenLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHost
              ; RebootHostOnXenLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Xen])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHostOnXenLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHostOnXenLivePatchFailure])
        , []
        , []
        , ([Xen; Kernel], [])
        )
      , ([("host1", [])], [])
      )
    ; ( (("host1", [RebootHostOnXenLivePatchFailure]), [], [], ([Kernel], [Xen]))
      , ([("host1", [RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( (("host1", [RebootHostOnXenLivePatchFailure]), [], [], ([Kernel], []))
      , ([("host1", [RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( (("host1", [RebootHostOnXenLivePatchFailure]), [], [], ([Xen], [Kernel]))
      , ([("host1", [RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( (("host1", [RebootHostOnXenLivePatchFailure]), [], [], ([Xen], []))
      , ([("host1", [])], [])
      )
    ; ( ( ("host1", [RebootHostOnXenLivePatchFailure])
        , []
        , []
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHostOnXenLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( (("host1", [RebootHostOnXenLivePatchFailure]), [], [], ([], [Kernel]))
      , ( [
            ( "host1"
            , [
                RebootHostOnKernelLivePatchFailure
              ; RebootHostOnXenLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( (("host1", [RebootHostOnXenLivePatchFailure]), [], [], ([], [Xen]))
      , ([("host1", [RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( (("host1", [RebootHostOnXenLivePatchFailure]), [], [], ([], []))
      , ([("host1", [RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen; Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([Kernel], [Xen])
        )
      , ([("host1", [RebootHostOnXenLivePatchFailure; RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen], [Kernel])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen], [])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHost
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Kernel])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Xen])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHost
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([Xen; Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([Kernel], [Xen])
        )
      , ([("host1", [RebootHostOnXenLivePatchFailure; RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([Xen], [Kernel])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([Xen], [])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHost
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([], [Kernel])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([], [Xen])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHost
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([], [])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([Xen; Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([Kernel], [Xen])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([Kernel], [])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([Xen], [Kernel])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([Xen], [])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([], [Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([], [Xen])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([], [])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([Xen; Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([Kernel], [Xen])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([Kernel], [])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([Xen], [Kernel])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([Xen], [])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([], [Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([], [Xen])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHost
            ; RebootHostOnXenLivePatchFailure
            ; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([], [])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([Xen; Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([Kernel], [Xen])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([Kernel], [])
        )
      , ([("host1", [RebootHost; RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([Xen], [Kernel])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([Xen], [])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([], [Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([], [Xen])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , [RebootHost]
        , ([], [])
        )
      , ( [
            ( "host1"
            , [
                RebootHost
              ; RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([Xen; Kernel], [])
        )
      , ([("host1", [])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([Kernel], [Xen])
        )
      , ([("host1", [RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([Kernel], [])
        )
      , ([("host1", [RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([Xen], [Kernel])
        )
      , ([("host1", [RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([Xen], [])
        )
      , ([("host1", [RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([], [Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([], [Xen])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ( "host1"
          , [
              RebootHostOnXenLivePatchFailure; RebootHostOnKernelLivePatchFailure
            ]
          )
        , []
        , []
        , ([], [])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen; Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([Kernel], [Xen])
        )
      , ([("host1", [RebootHostOnXenLivePatchFailure; RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([Kernel], [])
        )
      , ([("host1", [RebootHost])], [])
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen], [Kernel])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([Xen], [])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHost
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Kernel])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [Xen])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHost
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , [RebootHost]
        , ([], [])
        )
      , ([("host1", [RebootHost; RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([Xen; Kernel], [])
        )
      , ([("host1", [])], [])
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([Kernel], [Xen])
        )
      , ([("host1", [RebootHostOnXenLivePatchFailure])], [])
      )
    ; ( (("host1", [RebootHostOnKernelLivePatchFailure]), [], [], ([Kernel], []))
      , ([("host1", [])], [])
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([Xen], [Kernel])
        )
      , ([("host1", [RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( (("host1", [RebootHostOnKernelLivePatchFailure]), [], [], ([Xen], []))
      , ([("host1", [RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( ( ("host1", [RebootHostOnKernelLivePatchFailure])
        , []
        , []
        , ([], [Xen; Kernel])
        )
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( (("host1", [RebootHostOnKernelLivePatchFailure]), [], [], ([], [Kernel]))
      , ([("host1", [RebootHostOnKernelLivePatchFailure])], [])
      )
    ; ( (("host1", [RebootHostOnKernelLivePatchFailure]), [], [], ([], [Xen]))
      , ( [
            ( "host1"
            , [
                RebootHostOnXenLivePatchFailure
              ; RebootHostOnKernelLivePatchFailure
              ]
            )
          ]
        , []
        )
      )
    ; ( (("host1", [RebootHostOnKernelLivePatchFailure]), [], [], ([], []))
      , ([("host1", [RebootHostOnKernelLivePatchFailure])], [])
      )
    ]

  let tests'' =
    let open Guidance in
    [
      ( ( ("host1", [])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartDeviceModel]
        , ([], [])
        )
      , ( [("host1", [])]
        , [("vm2", [RestartDeviceModel]); ("vm1", [RestartDeviceModel])]
        )
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ( [("host1", [RestartToolstack])]
        , [("vm2", [RestartDeviceModel]); ("vm1", [RestartDeviceModel])]
        )
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartToolstack]
        , ([], [])
        )
      , ( [("host1", [RestartToolstack])]
        , [("vm2", [RestartDeviceModel]); ("vm1", [])]
        )
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , []
        , ([], [])
        )
      , ([("host1", [])], [("vm2", [RestartDeviceModel]); ("vm1", [])])
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [])]
        , [RestartDeviceModel]
        , ([], [])
        )
      , ( [("host1", [])]
        , [("vm2", [RestartDeviceModel]); ("vm1", [RestartDeviceModel])]
        )
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [])]
        , [RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ( [("host1", [RestartToolstack])]
        , [("vm2", [RestartDeviceModel]); ("vm1", [RestartDeviceModel])]
        )
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( (("host1", []), [("vm1", []); ("vm2", [])], [RestartToolstack], ([], []))
      , ([("host1", [RestartToolstack])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( (("host1", []), [("vm1", []); ("vm2", [])], [RebootHost], ([], []))
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( (("host1", []), [("vm1", []); ("vm2", [])], [], ([], []))
      , ([("host1", [])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , []
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [])]
        , [RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [])]
        , [RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [])]
        , [RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( (("host1", [RebootHost]), [("vm1", []); ("vm2", [])], [], ([], []))
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , []
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RebootHost; RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , []
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartDeviceModel]
        , ([], [])
        )
      , ( [("host1", [RestartToolstack])]
        , [("vm2", [RestartDeviceModel]); ("vm1", [RestartDeviceModel])]
        )
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ( [("host1", [RestartToolstack])]
        , [("vm2", [RestartDeviceModel]); ("vm1", [RestartDeviceModel])]
        )
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RestartToolstack]
        , ([], [])
        )
      , ( [("host1", [RestartToolstack])]
        , [("vm2", [RestartDeviceModel]); ("vm1", [])]
        )
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost; RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , [RebootHost]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [RestartDeviceModel])]
        , []
        , ([], [])
        )
      , ( [("host1", [RestartToolstack])]
        , [("vm2", [RestartDeviceModel]); ("vm1", [])]
        )
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RestartDeviceModel]
        , ([], [])
        )
      , ( [("host1", [RestartToolstack])]
        , [("vm2", [RestartDeviceModel]); ("vm1", [RestartDeviceModel])]
        )
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ( [("host1", [RestartToolstack])]
        , [("vm2", [RestartDeviceModel]); ("vm1", [RestartDeviceModel])]
        )
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartToolstack; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartDeviceModel]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RestartToolstack])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost; RestartToolstack]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( ( ("host1", [RestartToolstack])
        , [("vm1", []); ("vm2", [])]
        , [RebootHost]
        , ([], [])
        )
      , ([("host1", [RebootHost])], [("vm2", []); ("vm1", [])])
      )
    ; ( (("host1", [RestartToolstack]), [("vm1", []); ("vm2", [])], [], ([], []))
      , ([("host1", [RestartToolstack])], [("vm2", []); ("vm1", [])])
      )
    ]

  let string_of_host (host, guidances) =
    "(" ^ host ^ ", " ^ guidances_to_string guidances ^ ")"

  let string_of_vms vms_pending_guidances =
    vms_pending_guidances
    |> List.map (fun (vm, guidances) ->
           "(" ^ vm ^ ", " ^ guidances_to_string guidances ^ ")"
       )
    |> String.concat "; "
    |> fun s -> "[" ^ s ^ "]"

  let string_of_inputs host_pending_guidances vms_pending_guidances
      coming_guidances succ_lps fail_lps =
    "("
    ^ string_of_host host_pending_guidances
    ^ ", "
    ^ string_of_vms vms_pending_guidances
    ^ ", "
    ^ guidances_to_string coming_guidances
    ^ ", "
    ^ livepatching_result_to_string succ_lps fail_lps
    ^ ")"

  let strings_of_tests_inputs' =
    List.map
      (fun ( ( host_pending_guidances
             , vms_pending_guidances
             , coming_guidances
             , (succ_lps, fail_lps)
             )
           , _
           ) ->
        string_of_inputs host_pending_guidances vms_pending_guidances
          coming_guidances succ_lps fail_lps
      )
      tests'

  let strings_of_exhaustive_inputs' =
    let open Guidance in
    let exhaustive_host_pending_guidances =
      combination
        [
          RebootHost
        ; RebootHostOnXenLivePatchFailure
        ; RebootHostOnKernelLivePatchFailure
        ]
      |> List.map (fun l -> ("host1", List.sort Guidance.compare l))
    in
    let exhaustive_vms_pending_guidances = [[]] in
    let exhaustive_guidances = combination [RebootHost] in
    join exhaustive_host_pending_guidances exhaustive_vms_pending_guidances
    |> join3 exhaustive_guidances
    |> join4 exhaustive_livepatching_results
    |> List.map
         (fun
           ( host_pending_guidances
           , vms_pending_guidances
           , coming_guidances
           , (succ_lps, fail_lps)
           )
         ->
           string_of_inputs host_pending_guidances vms_pending_guidances
             coming_guidances succ_lps fail_lps
       )

  let strings_of_tests_inputs'' =
    List.map
      (fun ( ( host_pending_guidances
             , vms_pending_guidances
             , coming_guidances
             , (succ_lps, fail_lps)
             )
           , _
           ) ->
        string_of_inputs host_pending_guidances vms_pending_guidances
          coming_guidances succ_lps fail_lps
      )
      tests''

  let strings_of_exhaustive_inputs'' =
    let open Guidance in
    let exhaustive_host_pending_guidances =
      combination [RebootHost; RestartToolstack]
      |> List.map (fun l -> ("host1", List.sort Guidance.compare l))
    in
    let exhaustive_vms_pending_guidances =
      [[("vm1", []); ("vm2", [])]; [("vm1", []); ("vm2", [RestartDeviceModel])]]
    in
    let exhaustive_guidances =
      combination [RebootHost; RestartToolstack; RestartDeviceModel]
      |> List.map (fun l -> List.sort Guidance.compare l)
    in
    let exhaustive_livepatching_results = [([], [])] in
    join exhaustive_host_pending_guidances exhaustive_vms_pending_guidances
    |> join3 exhaustive_guidances
    |> join4 exhaustive_livepatching_results
    |> List.map
         (fun
           ( host_pending_guidances
           , vms_pending_guidances
           , coming_guidances
           , (succ_lps, fail_lps)
           )
         ->
           string_of_inputs host_pending_guidances vms_pending_guidances
             coming_guidances succ_lps fail_lps
       )

  let tests = `QuickAndAutoDocumented (tests' @ tests'')
end

module MergePendingGuidances = Generic.MakeStateless (MergePendingGuidances')

module Exhaustiveness = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string list * string list

    type output_t = (unit, exn) result

    let string_of_input_t =
      Fmt.(str "%a" Dump.(pair (list string) (list string)))

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(any "()") ~error:exn))
  end

  let transform (exhaustive_inputs, tests_inputs) =
    try
      Ok
        (List.iter2
           (fun exhaustive manual ->
             match exhaustive = manual with
             | true ->
                 ()
             | false ->
                 raise (Mismatched_exhaustive_input (exhaustive, manual))
           )
           exhaustive_inputs tests_inputs
        )
    with e -> Error e

  let tests =
    `QuickAndAutoDocumented
      [
        ( ( MergeWithUnappliedGuidances'.strings_of_exhaustive_inputs
          , MergeWithUnappliedGuidances'.strings_of_tests_inputs
          )
        , Ok ()
        )
      ; ( ( MergeLivepatchFailures'.strings_of_exhaustive_inputs
          , MergeLivepatchFailures'.strings_of_tests_inputs
          )
        , Ok ()
        )
      ; ( ( MergePendingGuidances'.strings_of_exhaustive_inputs'
          , MergePendingGuidances'.strings_of_tests_inputs'
          )
        , Ok ()
        )
      ; ( ( MergePendingGuidances'.strings_of_exhaustive_inputs''
          , MergePendingGuidances'.strings_of_tests_inputs''
          )
        , Ok ()
        )
      ]
end)

let tests =
  make_suite "repository_helpers_"
    [
      ("update_of_json", UpdateOfJsonTest.tests)
    ; ("assert_valid_guidances", GuidanceSetAssertValidGuidanceTest.tests)
    ; ("assert_url_is_valid", AssertUrlIsValid.tests)
    ; ("write_yum_config", WriteYumConfig.tests)
    ; ("eval_guidance_for_one_update", EvalGuidanceForOneUpdate.tests)
    ; ("get_update_in_json", GetUpdateInJson.tests)
    ; ("consolidate_updates_of_host", ConsolidateUpdatesOfHost.tests)
    ; ("parse_updateinfo_list", ParseUpdateInfoList.tests)
    ; ("resort_guidances", GuidanceSetResortGuidancesTest.tests)
    ; ("merge_with_unapplied_guidances", MergeWithUnappliedGuidances.tests)
    ; ("merge_livepatch_failures", MergeLivepatchFailures.tests)
    ; ("merge_pending_guidances", MergePendingGuidances.tests)
    ; ("prune_accumulative_updates", PruneAccumulativeUpdates.tests)
    ; ("prune_updateinfo_for_livepatches", PruneUpdateInfoForLivepatches.tests)
    ; ( "parse_output_of_yum_upgrade_dry_run"
      , ParseOutputOfYumUpgradeDryRun.tests
      )
    ; ( "get_latest_updates_from_redundancy"
      , GetLatestUpdatesFromRedundancy.tests
      )
    ; ("exhaustiveness", Exhaustiveness.tests)
    ]

let () = Alcotest.run "Repository Helpers" tests
