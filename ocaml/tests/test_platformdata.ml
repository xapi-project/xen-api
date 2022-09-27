(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

module Key_values = struct
  let invalid_values = ["foo"; ""; "banana"; "2"]

  let true_values = ["TRUE"; "tRuE"; "1"; "true"]

  let false_values = ["FALSE"; "fAlSe"; "0"; "false"]

  let valid_platformdata = [[]]

  let key = "test_key"

  let test_truthiness p =
    Vm_platform.is_true ~key ~platformdata:p ~default:false

  let test_validity p = Vm_platform.is_valid ~key ~platformdata:p

  let test_value typ f value expected =
    let title = Printf.sprintf "%s values: %s" typ value in
    ( title
    , `Quick
    , fun () ->
        Alcotest.(check bool)
          (Printf.sprintf "'%s' must evaluate to %b" value expected)
          expected
          (f [(key, value)])
    )

  let truthy_value_tests =
    let test v = test_value "truthy" test_truthiness v true in
    List.map test true_values

  let falsy_value_tests =
    let test v = test_value "falsy" test_truthiness v false in
    List.map test false_values

  let invalid_value_tests =
    let test v = test_value "invalid" test_validity v false in
    List.map test invalid_values

  let empty_platformdata_tests =
    [
      ( "Validity with empty platformdata"
      , `Quick
      , fun () ->
          Alcotest.(check bool)
            "Any key in an empty platformdata must be valid" (test_validity [])
            true
      )
    ]

  let tests =
    List.concat
      [
        truthy_value_tests
      ; falsy_value_tests
      ; invalid_value_tests
      ; empty_platformdata_tests
      ]
end

module Licensing = struct
  (* Nested_virt is restricted in the default test database *)

  (* List of platform keys and whether they must be restricted when 'Nested_virt' is restricted.
     true -> must be restricted
     false -> must not be restricted
  *)
  let nested_virt_checks =
    [
      ([], false)
    ; ([("foo", "bar"); ("baz", "moo"); ("nested-virt", "true")], true)
    ; ([("nested-virt", "TRUE")], true)
    ; ([("nested-virt", "false")], false)
    ; ([("nested-virt", "1")], true)
    ; ([("nested-virt", "0")], false)
    ; ([("nested-virt", "true")], true)
    ]

  let pp_platform = Fmt.Dump.(list @@ pair string string)

  let test_nested_virt_licensing (platform, should_raise) () =
    let __context = Test_common.make_test_database () in

    let pool = Db.Pool.get_all ~__context |> List.hd in
    let test_checks =
      if should_raise then
        Alcotest.check_raises
          (Format.asprintf "Failed to raise an exception for platform map: '%a'"
             pp_platform platform
          )
          Api_errors.(Server_error (license_restriction, ["Nested_virt"]))
      else
        fun f ->
      try f ()
      with
      | Api_errors.(Server_error (typ, ["Nested_virt"])) as e
      when typ = Api_errors.license_restriction
      ->
        Alcotest.fail
          (Format.asprintf "Unexpectedly raised '%a' for platform map: '%a'"
             Fmt.exn e pp_platform platform
          )
    in

    test_checks (fun () ->
        Db.Pool.set_restrictions ~__context ~self:pool
          ~value:[("restrict_nested_virt", "true")] ;
        Vm_platform.check_restricted_flags ~__context platform
    ) ;
    (* If the feature is unrestricted, nothing should raise an exception *)
    Db.Pool.set_restrictions ~__context ~self:pool
      ~value:[("restrict_nested_virt", "false")] ;
    Vm_platform.check_restricted_flags ~__context platform

  let tests =
    let test (p, r) =
      ( Format.asprintf "Nested_virt licensing: %a" pp_platform p
      , `Quick
      , test_nested_virt_licensing (p, r)
      )
    in
    List.map test nested_virt_checks
end

let firmware_type_printer v =
  v
  |> Rpcmarshal.marshal Xenops_types.Vm.typ_of_firmware_type
  |> Jsonrpc.to_string

let uefi = Xenops_types.Vm.Uefi Xenops_types.Nvram_uefi_variables.default_t

module SanityCheck = Generic.MakeStateless (struct
  module Io = struct
    type input_t =
      (string * string) list
      * Xenops_types.Vm.firmware_type
      * bool
      * int64
      * int64
      * [`hvm | `pv | `pv_in_pvh]

    type output_t = ((string * string) list, exn) result

    let string_of_input_t
        (platformdata, firmware, filter, vcpu_max, vcpu_startup, domain_type) =
      Printf.sprintf
        "(platformdata = %s, firmware = %s, filter_out_unknowns = %b, vcpu_max \
         = %Ld,\n\
        \          vcpu_at_startup = %Ld, domain_type = %s)"
        (platformdata |> Test_printers.(assoc_list string string))
        (firmware |> firmware_type_printer)
        filter vcpu_max vcpu_startup
        (Record_util.domain_type_to_string domain_type)

    let pp_list_assoc fmt_fst fmt_snd =
      Fmt.(Dump.list @@ pair ~sep:(any "=") fmt_fst fmt_snd)

    let string_of_output_t x =
      Fmt.(
        str "%a" Dump.(result ~ok:(pp_list_assoc string string) ~error:exn) x
      )
  end

  let transform
      ( platformdata
      , firmware
      , filter_out_unknowns
      , vcpu_max
      , vcpu_at_startup
      , domain_type
      ) =
    try
      Ok
        (Vm_platform.sanity_check ~platformdata ~firmware ~vcpu_max
           ~vcpu_at_startup ~domain_type ~filter_out_unknowns
        )
    with e -> Error e

  let tests =
    let usb_defaults = [("usb", "true"); ("usb_tablet", "true")] in
    let make_firmware_ok dm firmware =
      ( ([("device-model", dm)], firmware, false, 0L, 0L, `hvm)
      , Ok (usb_defaults @ [("device-model", dm)])
      )
    in
    let open Xenops_interface.Vm in
    `QuickAndAutoDocumented
      [
        (* Check that we can filter out unknown platform flags. *)
        ( ( [
              ("nonsense", "abc")
            ; ("pae", "true")
            ; ("whatever", "def")
            ; ("viridian", "true")
            ]
          , Bios
          , true
          , 0L
          , 0L
          , `pv
          )
        , Ok (usb_defaults @ [("pae", "true"); ("viridian", "true")])
        )
      ; (* Check that usb and usb_tablet are turned on by default. *)
        (([], Bios, false, 0L, 0L, `pv), Ok usb_defaults)
      ; (* Check that an invalid tsc_mode gets filtered out. *)
        (([("tsc_mode", "17")], Bios, false, 0L, 0L, `pv), Ok usb_defaults)
      ; (* Check that an invalid parallel port gets filtered out. *)
        ( ([("parallel", "/dev/random")], Bios, false, 0L, 0L, `pv)
        , Ok usb_defaults
        )
      ; (* Check that we can't set usb_tablet to true if usb is false. *)
        ( ([("usb", "false"); ("usb_tablet", "true")], Bios, false, 0L, 0L, `pv)
        , Ok [("usb", "false"); ("usb_tablet", "false")]
        )
      ; (* Check that we can fully disable usb. *)
        ( ([("usb", "false"); ("usb_tablet", "false")], Bios, false, 0L, 0L, `pv)
        , Ok [("usb", "false"); ("usb_tablet", "false")]
        )
      ; (* Check that we can disable the parallel port. *)
        ( ([("parallel", "none")], Bios, false, 0L, 0L, `pv)
        , Ok (usb_defaults @ [("parallel", "none")])
        )
      ; (* Check that a set of valid fields is unchanged (apart from
           the ordering, which changes due to the implementation of
           List.update_assoc). *)
        ( ( [
              ("parallel", "/dev/parport2")
            ; ("pae", "true")
            ; ("usb_tablet", "false")
            ; ("tsc_mode", "2")
            ; ("viridian", "true")
            ; ("usb", "true")
            ]
          , Bios
          , false
          , 0L
          , 0L
          , `pv
          )
        , Ok
            [
              ("usb", "true")
            ; ("usb_tablet", "false")
            ; ("parallel", "/dev/parport2")
            ; ("pae", "true")
            ; ("tsc_mode", "2")
            ; ("viridian", "true")
            ]
        )
      ; (* Check that combination of valid and invalid fields is dealt with
           correctly. *)
        ( ( [
              ("pae", "true")
            ; ("parallel", "/dev/parport0")
            ; ("tsc_mode", "blah")
            ]
          , Bios
          , false
          , 0L
          , 0L
          , `pv
          )
        , Ok (usb_defaults @ [("pae", "true"); ("parallel", "/dev/parport0")])
        )
      ; (* Check VCPUs configuration - hvm success scenario *)
        ( ([("cores-per-socket", "3")], Bios, false, 6L, 6L, `hvm)
        , Ok (usb_defaults @ [("cores-per-socket", "3")])
        )
      ; (* Check VCPUs configuration - pvm success scenario *)
        ( ([("cores-per-socket", "3")], Bios, false, 0L, 0L, `pv)
        , Ok (usb_defaults @ [("cores-per-socket", "3")])
        )
      ; (* Check VCPUs configuration - hvm failure scenario *)
        ( ([("cores-per-socket", "4")], Bios, false, 6L, 6L, `hvm)
        , Error
            (Api_errors.Server_error
               (Api_errors.vcpu_max_not_cores_per_socket_multiple, ["6"; "4"])
            )
        )
      ; (* Check VCPUs configuration - hvm failure scenario*)
        ( ([("cores-per-socket", "0")], Bios, false, 6L, 6L, `hvm)
        , Error
            (Api_errors.Server_error
               (Api_errors.vcpu_max_not_cores_per_socket_multiple, ["6"; "0"])
            )
        )
      ; (* Check VCPUs configuration - hvm failure scenario*)
        ( ([("cores-per-socket", "-1")], Bios, false, 6L, 6L, `hvm)
        , Error
            (Api_errors.Server_error
               (Api_errors.vcpu_max_not_cores_per_socket_multiple, ["6"; "-1"])
            )
        )
      ; (* Check VCPUs configuration - hvm failure scenario*)
        ( ([("cores-per-socket", "abc")], Bios, false, 6L, 5L, `hvm)
        , Error
            (Api_errors.Server_error
               (Api_errors.invalid_value, ["platform:cores-per-socket"; "abc"])
            )
        )
      ; (* Check BIOS configuration - qemu trad *)
        make_firmware_ok "qemu-trad" Bios
      ; make_firmware_ok "qemu-upstream" Bios
      ; make_firmware_ok "qemu-upstream-compat" Bios
      ; (* Check UEFI configuration - qemu upstream *)
        make_firmware_ok "qemu-upstream" uefi
      ; make_firmware_ok "qemu-upstream-compat" uefi
      ; make_firmware_ok "qemu-upstream-uefi" uefi
      ; (* Check UEFI configuration - qemu-trad incompatibility *)
        ( ([("device-model", "qemu-trad")], uefi, false, 0L, 0L, `hvm)
        , Error
            (Api_errors.Server_error
               ( Api_errors.invalid_value
               , [
                   "platform:device-model"
                 ; "UEFI boot is not supported with qemu-trad"
                 ]
               )
            )
        )
      ; (* Check BIOS configuration - qemu-upstream-uefi incompatibility *)
        ( ([("device-model", "qemu-upstream-uefi")], Bios, false, 0L, 0L, `hvm)
        , Error
            (Api_errors.Server_error
               ( Api_errors.invalid_value
               , [
                   "platform:device-model"
                 ; "BIOS boot is not supported with qemu-upstream-uefi"
                 ]
               )
            )
        )
      ]
end)

let tests =
  [
    ( "platform_data"
    , List.concat [Key_values.tests; Licensing.tests; SanityCheck.tests]
    )
  ]
