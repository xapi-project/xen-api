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

open Stdext
open OUnit
open Test_highlevel

module SanityCheck = Generic.Make(struct
    module Io = struct
      type input_t = ((string * string) list * bool * int64 * int64 * [ `hvm | `pv | `pv_in_pvh ])
      type output_t = (exn, (string * string) list) Either.t

      let string_of_input_t (platformdata, filter, vcpu_max, vcpu_startup, domain_type) =
        Printf.sprintf "(platformdata = %s, filter_out_unknowns = %b, vcpu_max = %Ld,
          vcpu_at_startup = %Ld, domain_type = %s)"
          (platformdata |> Test_printers.(assoc_list string string))
          (filter) (vcpu_max) (vcpu_startup) (Record_util.domain_type_to_string domain_type)

      let string_of_output_t = Test_printers.(either exn (assoc_list string string))
    end

    let transform (platformdata, filter_out_unknowns, vcpu_max, vcpu_at_startup, domain_type) =
      try Either.Right (Vm_platform.sanity_check ~platformdata ~vcpu_max
                          ~vcpu_at_startup ~domain_type ~filter_out_unknowns)
      with e -> Either.Left e

    let tests =
      let usb_defaults = [
        "usb", "true";
        "usb_tablet", "true";
      ] in
      [
        (* Check that we can filter out unknown platform flags. *)
        (([
            "nonsense", "abc";
            "pae", "true";
            "whatever", "def";
            "viridian", "true";
          ], true, 0L, 0L, `pv),
         Either.Right (usb_defaults @
                       [
                         "pae", "true";
                         "viridian", "true";
                       ]));
        (* Check that usb and usb_tablet are turned on by default. *)
        (([], false, 0L, 0L, `pv),
         Either.Right (usb_defaults));
        (* Check that an invalid tsc_mode gets filtered out. *)
        ((["tsc_mode", "17";], false, 0L, 0L, `pv),
         Either.right (usb_defaults));
        (* Check that an invalid parallel port gets filtered out. *)
        ((["parallel", "/dev/random"], false, 0L, 0L, `pv),
         Either.Right (usb_defaults));
        (* Check that we can't set usb_tablet to true if usb is false. *)
        (([
            "usb", "false";
            "usb_tablet", "true";
          ], false, 0L, 0L, `pv),
         Either.Right ([
             "usb", "false";
             "usb_tablet", "false";
           ]));
        (* Check that we can fully disable usb. *)
        (([
            "usb", "false";
            "usb_tablet", "false";
          ], false, 0L, 0L, `pv),
         Either.Right ([
             "usb", "false";
             "usb_tablet", "false";
           ]));
        (* Check that we can disable the parallel port. *)
        ((["parallel", "none"], false, 0L, 0L, `pv),
         Either.Right (usb_defaults @
                       ["parallel", "none"]));
        (* Check that a set of valid fields is unchanged (apart from
           			 * the ordering, which changes due to the implementation of
           			 * List.update_assoc). *)
        (([
            "parallel", "/dev/parport2";
            "pae", "true";
            "usb_tablet", "false";
            "tsc_mode", "2";
            "viridian", "true";
            "usb", "true";
          ], false, 0L, 0L, `pv),
         Either.Right ([
             "usb", "true";
             "usb_tablet", "false";
             "parallel", "/dev/parport2";
             "pae", "true";
             "tsc_mode", "2";
             "viridian", "true";
           ]));
        (* Check that combination of valid and invalid fields is dealt with
           			 * correctly. *)
        (([
            "pae", "true";
            "parallel", "/dev/parport0";
            "tsc_mode", "blah";
          ], false, 0L, 0L, `pv),
         Either.Right (usb_defaults @
                       [
                         "pae", "true";
                         "parallel", "/dev/parport0";
                       ]));
        (* Check VCPUs configuration - hvm success scenario*)
        (([
            "cores-per-socket", "3";
          ], false, 6L, 6L, `hvm),
         Either.Right (usb_defaults @
                       [
                         "cores-per-socket", "3";
                       ]));
        (* Check VCPUs configuration - pvm success scenario*)
        (([
            "cores-per-socket", "3";
          ], false, 0L, 0L, `pv),
         Either.Right (usb_defaults @
                       [
                         "cores-per-socket", "3";
                       ]));
        (* Check VCPUs configuration - hvm failure scenario*)
        (([
            "cores-per-socket", "4";
          ], false, 6L, 6L, `hvm),
         Either.Left (Api_errors.Server_error(Api_errors.invalid_value,
                                              ["platform:cores-per-socket";
                                               "VCPUs_max must be a multiple of this field"])));
        (* Check VCPUs configuration - hvm failure scenario*)
        (([
            "cores-per-socket", "abc";
          ], false, 6L, 5L, `hvm),
         Either.Left(Api_errors.Server_error(Api_errors.invalid_value,
                                             ["platform:cores-per-socket";
                                              "value = abc is not a valid int"])));
      ]
  end)

let test =
  "platformdata" >:::
  [
    "test_platform_sanity_check" >::: SanityCheck.tests
  ]
