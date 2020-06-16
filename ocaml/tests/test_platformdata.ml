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

let firmware_type_printer v =
  v
  |> Rpcmarshal.marshal Xenops_types.Vm.typ_of_firmware_type
  |> Jsonrpc.to_string

let uefi = Xenops_types.Vm.Uefi Xenops_types.Nvram_uefi_variables.default_t

module SanityCheck = Generic.MakeStateless (struct
  module Io = struct
    type input_t =
      (string * string) list
      * Xenops_types.Vm.firmware_type option
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
        (firmware |> Test_printers.option firmware_type_printer)
        filter vcpu_max vcpu_startup
        (Record_util.domain_type_to_string domain_type)

    let pp_list_assoc fmt_fst fmt_snd =
      Fmt.(Dump.list @@ pair ~sep:(any "=") fmt_fst fmt_snd)

    let string_of_output_t x =
      Fmt.(
        str "%a" Dump.(result ~ok:(pp_list_assoc string string) ~error:exn) x)
  end

  let transform
      ( platformdata
      , firmware
      , filter_out_unknowns
      , vcpu_max
      , vcpu_at_startup
      , domain_type ) =
    try
      Ok
        (Vm_platform.sanity_check ~platformdata ?firmware ~vcpu_max
           ~vcpu_at_startup ~domain_type ~filter_out_unknowns)
    with e -> Error e

  let tests =
    let usb_defaults = [("usb", "true"); ("usb_tablet", "true")] in
    let make_firmware_ok dm firmware =
      ( ([("device-model", dm)], firmware, false, 0L, 0L, `hvm)
      , Ok (usb_defaults @ [("device-model", dm)]) )
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
          , None
          , true
          , 0L
          , 0L
          , `pv )
        , Ok (usb_defaults @ [("pae", "true"); ("viridian", "true")]) )
      ; (* Check that usb and usb_tablet are turned on by default. *)
        (([], None, false, 0L, 0L, `pv), Ok usb_defaults)
      ; (* Check that an invalid tsc_mode gets filtered out. *)
        (([("tsc_mode", "17")], None, false, 0L, 0L, `pv), Ok usb_defaults)
      ; (* Check that an invalid parallel port gets filtered out. *)
        ( ([("parallel", "/dev/random")], None, false, 0L, 0L, `pv)
        , Ok usb_defaults )
      ; (* Check that we can't set usb_tablet to true if usb is false. *)
        ( ([("usb", "false"); ("usb_tablet", "true")], None, false, 0L, 0L, `pv)
        , Ok [("usb", "false"); ("usb_tablet", "false")] )
      ; (* Check that we can fully disable usb. *)
        ( ([("usb", "false"); ("usb_tablet", "false")], None, false, 0L, 0L, `pv)
        , Ok [("usb", "false"); ("usb_tablet", "false")] )
      ; (* Check that we can disable the parallel port. *)
        ( ([("parallel", "none")], None, false, 0L, 0L, `pv)
        , Ok (usb_defaults @ [("parallel", "none")]) )
      ; (* Check that a set of valid fields is unchanged (apart from
           			 * the ordering, which changes due to the implementation of
           			 * List.update_assoc). *)
        ( ( [
              ("parallel", "/dev/parport2")
            ; ("pae", "true")
            ; ("usb_tablet", "false")
            ; ("tsc_mode", "2")
            ; ("viridian", "true")
            ; ("usb", "true")
            ]
          , None
          , false
          , 0L
          , 0L
          , `pv )
        , Ok
            [
              ("usb", "true")
            ; ("usb_tablet", "false")
            ; ("parallel", "/dev/parport2")
            ; ("pae", "true")
            ; ("tsc_mode", "2")
            ; ("viridian", "true")
            ] )
      ; (* Check that combination of valid and invalid fields is dealt with
           			 * correctly. *)
        ( ( [
              ("pae", "true")
            ; ("parallel", "/dev/parport0")
            ; ("tsc_mode", "blah")
            ]
          , None
          , false
          , 0L
          , 0L
          , `pv )
        , Ok (usb_defaults @ [("pae", "true"); ("parallel", "/dev/parport0")])
        )
      ; (* Check VCPUs configuration - hvm success scenario*)
        ( ([("cores-per-socket", "3")], None, false, 6L, 6L, `hvm)
        , Ok (usb_defaults @ [("cores-per-socket", "3")]) )
      ; (* Check VCPUs configuration - pvm success scenario*)
        ( ([("cores-per-socket", "3")], None, false, 0L, 0L, `pv)
        , Ok (usb_defaults @ [("cores-per-socket", "3")]) )
      ; (* Check VCPUs configuration - hvm failure scenario*)
        ( ([("cores-per-socket", "4")], None, false, 6L, 6L, `hvm)
        , Error
            (Api_errors.Server_error
               ( Api_errors.invalid_value
               , [
                   "platform:cores-per-socket (value 4)"
                 ; "VCPUs_max (value 6) must be a multiple of cores-per-socket"
                 ] )) )
      ; (* Check VCPUs configuration - hvm failure scenario*)
        ( ([("cores-per-socket", "0")], None, false, 6L, 6L, `hvm)
        , Error
            (Api_errors.Server_error
               ( Api_errors.invalid_value
               , [
                   "platform:cores-per-socket (value 0)"
                 ; "VCPUs_max (value 6) must be a multiple of cores-per-socket"
                 ] )) )
      ; (* Check VCPUs configuration - hvm failure scenario*)
        ( ([("cores-per-socket", "-1")], None, false, 6L, 6L, `hvm)
        , Error
            (Api_errors.Server_error
               ( Api_errors.invalid_value
               , [
                   "platform:cores-per-socket (value -1)"
                 ; "VCPUs_max (value 6) must be a multiple of cores-per-socket"
                 ] )) )
      ; (* Check VCPUs configuration - hvm failure scenario*)
        ( ([("cores-per-socket", "abc")], None, false, 6L, 5L, `hvm)
        , Error
            (Api_errors.Server_error
               ( Api_errors.invalid_value
               , [
                   "platform:cores-per-socket (value abc)"
                 ; "Value is not a valid int"
                 ] )) )
      ; (* Check BIOS configuration - qemu trad *)
        make_firmware_ok "qemu-trad" (Some Bios)
      ; make_firmware_ok "qemu-upstream" (Some Bios)
      ; make_firmware_ok "qemu-upstream-compat" (Some Bios)
      ; (* Check UEFI configuration - qemu upstream *)
        make_firmware_ok "qemu-upstream" (Some uefi)
      ; make_firmware_ok "qemu-upstream-compat" (Some uefi)
      ; make_firmware_ok "qemu-upstream-uefi" (Some uefi)
      ; (* Check UEFI configuration - qemu-trad incompatibility *)
        ( ([("device-model", "qemu-trad")], Some uefi, false, 0L, 0L, `hvm)
        , Error
            (Api_errors.Server_error
               ( Api_errors.invalid_value
               , [
                   "platform:device-model"
                 ; "UEFI boot is not supported with qemu-trad"
                 ] )) )
      ; (* Check BIOS configuration - qemu-upstream-uefi incompatibility *)
        ( ( [("device-model", "qemu-upstream-uefi")]
          , Some Bios
          , false
          , 0L
          , 0L
          , `hvm )
        , Error
            (Api_errors.Server_error
               ( Api_errors.invalid_value
               , [
                   "platform:device-model"
                 ; "BIOS boot is not supported with qemu-upstream-uefi"
                 ] )) )
      ]
end)

let tests = [("platform_data_sanity_check", SanityCheck.tests)]
