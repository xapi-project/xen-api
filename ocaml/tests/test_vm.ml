(*
 * Copyright (C) 2017 Citrix Systems Inc.
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

module VMSetBiosStrings = Generic.MakeStateful (struct
  module Io = struct
    type input_t = (string * string) list

    type output_t = ((string * string) list, exn) result

    let pp_list_assoc fmt_fst fmt_snd =
      Fmt.(Dump.list @@ pair ~sep:(any "=") fmt_fst fmt_snd)

    let string_of_input_t = Fmt.(str "%a" (pp_list_assoc string string))

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:(pp_list_assoc string string) ~error:exn))
  end

  module State = Test_state.XapiDb

  let name_label = "a"

  let load_input __context _ =
    ignore (Test_common.make_vm ~__context ~name_label ())

  let extract_output __context value =
    try
      let self =
        List.hd (Db.VM.get_by_name_label ~__context ~label:name_label)
      in
      Xapi_vm.set_bios_strings ~__context ~self ~value ;
      Ok (Db.VM.get_bios_strings ~__context ~self)
    with e -> Error e

  let big_str = String.make (Constants.bios_string_limit_size + 1) 'x'

  let non_printable_str1 = Printf.sprintf "xyz%c" (Char.chr 31)

  let non_printable_str2 = Printf.sprintf "xyz%c" (Char.chr 127)

  let bios_str1 = [("bios-vendor", "Test"); ("bios-version", "Test Inc. A08")]

  let bios_str2 =
    [
      ("system-manufacturer", "Test Inc.")
    ; ("system-product-name", "Test bios strings")
    ; ("system-version", "8.1.1 SP1 build 8901")
    ; ("system-serial-number", "test-test-test-test")
    ]

  let bios_str3 = [("enclosure-asset-tag", "testassettag12345")]

  let bios_str4 =
    [
      ("baseboard-manufacturer", "TestB Inc.")
    ; ("baseboard-product-name", "TestB pro")
    ; ("baseboard-version", "TestB v1.0")
    ; ("baseboard-serial-number", "TestB s12345")
    ; ("baseboard-asset-tag", "TestB asset")
    ; ("baseboard-location-in-chassis", "TestB loc")
    ]

  let default_settings =
    [
      ("bios-vendor", "Xen")
    ; ("bios-version", "")
    ; ("system-manufacturer", "Xen")
    ; ("system-product-name", "HVM domU")
    ; ("system-version", "")
    ; ("system-serial-number", "")
    ; ("baseboard-manufacturer", "")
    ; ("baseboard-product-name", "")
    ; ("baseboard-version", "")
    ; ("baseboard-serial-number", "")
    ; ("baseboard-asset-tag", "")
    ; ("baseboard-location-in-chassis", "")
    ; ("enclosure-asset-tag", "")
    ; ("hp-rombios", "")
    ; ("oem-1", "Xen")
    ; ("oem-2", "MS_VM_CERT/SHA1/bdbeb6e0a816d43fa6d3fe8aaef04c2bad9d3e3d")
    ]

  let update_list dl nl =
    List.map
      (fun (k, v) ->
        if List.mem_assoc k nl then
          (k, List.assoc k nl)
        else
          (k, v)
      )
      dl

  let rec combination (l : ('a * 'b) list list) =
    match l with
    | [] ->
        []
    | [hd] ->
        [hd]
    | hd :: tl ->
        let r = combination tl in
        (hd :: List.map (fun v -> hd @ v) r) @ r

  let tests =
    (* Correct value *)
    let valid_settings =
      combination [bios_str1; bios_str2; bios_str3; bios_str4]
    in
    let tests =
      List.concat
        [
          List.map
            (fun settings ->
              (settings, Ok (update_list default_settings settings))
            )
            valid_settings
        ; [
            (* Invalid BIOS string key *)
            ( [("xxxx", "test")]
            , Error
                Api_errors.(
                  Server_error (invalid_value, ["xxxx"; "Unknown key"])
                )
            )
          ; (* Empty value *)
            ( [("enclosure-asset-tag", "")]
            , Error
                Api_errors.(
                  Server_error
                    ( invalid_value
                    , ["enclosure-asset-tag"; "Value provided is empty"]
                    )
                )
            )
          ; (* Value having more than 512 charactors *)
            ( [("enclosure-asset-tag", big_str)]
            , Error
                Api_errors.(
                  Server_error
                    ( invalid_value
                    , [
                        "enclosure-asset-tag"
                      ; Printf.sprintf "%s has length more than %d characters"
                          big_str Constants.bios_string_limit_size
                      ]
                    )
                )
            )
          ; (* Value having non printable ascii characters *)
            ( [("enclosure-asset-tag", non_printable_str1)]
            , Error
                Api_errors.(
                  Server_error
                    ( invalid_value
                    , [
                        "enclosure-asset-tag"
                      ; non_printable_str1
                        ^ " has non-printable ASCII characters"
                      ]
                    )
                )
            )
          ; ( [("enclosure-asset-tag", non_printable_str2)]
            , Error
                Api_errors.(
                  Server_error
                    ( invalid_value
                    , [
                        "enclosure-asset-tag"
                      ; non_printable_str2
                        ^ " has non-printable ASCII characters"
                      ]
                    )
                )
            )
          ]
        ]
    in
    `QuickAndAutoDocumented tests
end)

module VMSecurebootCertificatesState = struct
  (* Test that the default value of secureboot_certificates_state is `ok *)
  let test_default_state () =
    let __context = Test_common.make_test_database () in
    let vm = Test_common.make_vm ~__context () in
    let state = Db.VM.get_secureboot_certificates_state ~__context ~self:vm in
    Alcotest.(check string)
      "default state is ok" "ok"
      (Record_util.vm_secureboot_certificates_state_to_string state)

  (* Test schedule when state is update_required -> update_scheduled *)
  let test_schedule_from_update_required () =
    let __context = Test_common.make_test_database () in
    let vm = Test_common.make_vm ~__context () in
    (* Set the state to update_required *)
    Db.VM.set_secureboot_certificates_state ~__context ~self:vm
      ~value:`update_required ;
    (* Call the schedule function *)
    Xapi_vm.schedule_secureboot_certs_state_update ~__context ~self:vm ;
    (* Check that the state is now update_scheduled *)
    let state = Db.VM.get_secureboot_certificates_state ~__context ~self:vm in
    Alcotest.(check string)
      "state is update_scheduled" "update_scheduled"
      (Record_util.vm_secureboot_certificates_state_to_string state)

  (* Test schedule when state is ok -> raises OPERATION_NOT_ALLOWED *)
  let test_schedule_from_ok_raises () =
    let __context = Test_common.make_test_database () in
    let vm = Test_common.make_vm ~__context () in
    (* State is ok by default *)
    Alcotest.check_raises "raises operation_not_allowed"
      Api_errors.(
        Server_error
          ( operation_not_allowed
          , ["VM.secureboot_certificates_state is ok, expected update_required"]
          )
      )
      (fun () ->
        Xapi_vm.schedule_secureboot_certs_state_update ~__context ~self:vm
      )

  (* Test schedule when state is error -> raises OPERATION_NOT_ALLOWED *)
  let test_schedule_from_error_raises () =
    let __context = Test_common.make_test_database () in
    let vm = Test_common.make_vm ~__context () in
    Db.VM.set_secureboot_certificates_state ~__context ~self:vm ~value:`error ;
    Alcotest.check_raises "raises operation_not_allowed"
      Api_errors.(
        Server_error
          ( operation_not_allowed
          , [
              "VM.secureboot_certificates_state is error, expected \
               update_required"
            ]
          )
      )
      (fun () ->
        Xapi_vm.schedule_secureboot_certs_state_update ~__context ~self:vm
      )

  (* Test schedule when state is already update_scheduled -> idempotent *)
  let test_schedule_from_update_scheduled_is_idempotent () =
    let __context = Test_common.make_test_database () in
    let vm = Test_common.make_vm ~__context () in
    Db.VM.set_secureboot_certificates_state ~__context ~self:vm
      ~value:`update_scheduled ;
    (* Should not raise, should be idempotent *)
    Xapi_vm.schedule_secureboot_certs_state_update ~__context ~self:vm ;
    let state = Db.VM.get_secureboot_certificates_state ~__context ~self:vm in
    Alcotest.(check string)
      "state remains update_scheduled" "update_scheduled"
      (Record_util.vm_secureboot_certificates_state_to_string state)

  let tests =
    [
      ("test_default_state", `Quick, test_default_state)
    ; ("test_schedule_from_update_required", `Quick, test_schedule_from_update_required)
    ; ("test_schedule_from_ok_raises", `Quick, test_schedule_from_ok_raises)
    ; ("test_schedule_from_error_raises", `Quick, test_schedule_from_error_raises)
    ; ( "test_schedule_from_update_scheduled_is_idempotent"
      , `Quick
      , test_schedule_from_update_scheduled_is_idempotent
      )
    ]
end

let tests =
  [
    ("test_vm_set_bios_strings", VMSetBiosStrings.tests)
  ; ("test_vm_secureboot_certificates_state", VMSecurebootCertificatesState.tests)
  ]
