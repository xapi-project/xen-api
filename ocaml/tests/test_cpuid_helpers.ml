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
open Cpuid_helpers

module StringOfFeatures = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array

    type output_t = string

    let string_of_input_t = Test_printers.(array int64)

    let string_of_output_t = Test_printers.string
  end

  let transform = Cpuid_helpers.string_of_features

  let tests =
    `QuickAndAutoDocumented
      [
        ([|0L; 2L; 123L|], "00000000-00000002-0000007b")
      ; ([|0L|], "00000000")
      ; ([||], "")
      ]
end)

module FeaturesOfString = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = int64 array

    let string_of_input_t = Test_printers.string

    let string_of_output_t = Test_printers.(array int64)
  end

  let transform = Cpuid_helpers.features_of_string

  let tests =
    `QuickAndAutoDocumented
      [
        ("00000000-00000002-0000007b", [|0L; 2L; 123L|])
      ; ("00000000", [|0L|])
      ; ("", [||])
      ]
end)

module RoundTripFeaturesToFeatures = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array

    type output_t = int64 array

    let string_of_input_t = Test_printers.(array int64)

    let string_of_output_t = Test_printers.(array int64)
  end

  let transform x =
    x |> Cpuid_helpers.string_of_features |> Cpuid_helpers.features_of_string

  let tests =
    `QuickAndAutoDocumented
      (List.map (fun x -> (x, x)) [[|0L; 1L; 123L|]; [|1L|]; [|0L|]; [||]])
end)

module RoundTripStringToString = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = string

    let string_of_input_t = Test_printers.string

    let string_of_output_t = Test_printers.string
  end

  let transform x =
    x |> Cpuid_helpers.features_of_string |> Cpuid_helpers.string_of_features

  let tests =
    `QuickAndAutoDocumented
      (List.map
         (fun x -> (x, x))
         ["00000000-00000002-0000007b"; "00000001"; "00000000"; ""])
end)

module ParseFailure = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = exn

    let string_of_input_t = Test_printers.string

    let string_of_output_t = Test_printers.exn
  end

  exception NoExceptionRaised

  let transform x =
    try
      ignore (Cpuid_helpers.features_of_string x) ;
      raise NoExceptionRaised
    with e -> e

  let tests =
    `QuickAndAutoDocumented
      (List.map
         (fun x -> (x, InvalidFeatureString x))
         ["foo bar baz"; "fgfg-1234"; "0123-foo"; "foo-0123"; "-1234"; "1234-"])
end)

module Extend = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array * int64 array

    type output_t = int64 array

    let string_of_input_t = Test_printers.(pair (array int64) (array int64))

    let string_of_output_t = Test_printers.(array int64)
  end

  let transform (arr0, arr1) = Cpuid_helpers.extend arr0 arr1

  let tests =
    `QuickAndAutoDocumented
      [
        (([||], [||]), [||])
      ; (([||], [|0L; 2L|]), [|0L; 2L|])
      ; (([|1L|], [||]), [||])
      ; (([|1L|], [|0L|]), [|1L|])
      ; (([|1L|], [|0L; 2L|]), [|1L; 2L|])
      ; (([|1L; 0L|], [|0L; 2L|]), [|1L; 0L|])
      ; (([|1L; 0L|], [|0L; 2L; 4L; 9L|]), [|1L; 0L; 4L; 9L|])
      ]
end)

module ZeroExtend = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array * int

    type output_t = int64 array

    let string_of_input_t = Test_printers.(pair (array int64) int)

    let string_of_output_t = Test_printers.(array int64)
  end

  let transform (arr, len) = Cpuid_helpers.zero_extend arr len

  let tests =
    `QuickAndAutoDocumented
      [
        (([|1L|], 2), [|1L; 0L|])
      ; (([|1L|], 1), [|1L|])
      ; (([||], 2), [|0L; 0L|])
      ; (([||], 1), [|0L|])
      ; (([||], 0), [||])
      ; (([|1L; 2L|], 0), [||])
      ; (([|1L; 2L|], 1), [|1L|])
      ; (([|1L; 2L|], 2), [|1L; 2L|])
      ]
end)

module Intersect = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array * int64 array

    type output_t = int64 array

    let string_of_input_t = Test_printers.(pair (array int64) (array int64))

    let string_of_output_t = Test_printers.(array int64)
  end

  let transform (a, b) = Cpuid_helpers.intersect a b

  let tests =
    `QuickAndAutoDocumented
      [
        (* Intersect should follow monoid laws - identity and commutativity *)
        (([||], [||]), [||])
      ; (([|1L; 2L; 3L|], [||]), [|1L; 2L; 3L|])
      ; (([||], [|1L; 2L; 3L|]), [|1L; 2L; 3L|])
      ; (([|7L; 3L|], [|5L|]), [|5L; 0L|])
      ; (([|5L|], [|7L; 3L|]), [|5L; 0L|])
      ; (([|1L|], [|1L|]), [|1L|])
      ; (([|1L|], [|1L; 0L|]), [|1L; 0L|])
      ; (([|1L; 2L; 3L|], [|1L; 1L; 1L|]), [|1L; 0L; 1L|])
      ; (([|1L; 2L; 3L|], [|0L; 0L; 0L|]), [|0L; 0L; 0L|])
      ; (([|0b00000000L|], [|0b11111111L|]), [|0b00000000L|])
      ; (([|0b11111111L|], [|0b11111111L|]), [|0b11111111L|])
      ; (([|0b01111111L|], [|0b11111111L|]), [|0b01111111L|])
      ; (([|0b00000111L|], [|0b00001111L|]), [|0b00000111L|])
      ; (([|0b00011111L|], [|0b00001111L|]), [|0b00001111L|])
      ; ( ([|0b00000000L; 0b11111111L|], [|0b11111111L; 0b00000000L|])
        , [|0b00000000L; 0b00000000L|] )
      ; ( ([|0b11111111L; 0b01010101L|], [|0b11111111L; 0b01010101L|])
        , [|0b11111111L; 0b01010101L|] )
      ; ( ([|0b01111111L; 0b10000000L|], [|0b11111111L; 0b00000000L|])
        , [|0b01111111L; 0b00000000L|] )
      ; ( ([|0b00000111L; 0b11100000L|], [|0b00001111L; 0b11110000L|])
        , [|0b00000111L; 0b11100000L|] )
      ]
end)

module Equality = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array * int64 array

    type output_t = bool

    let string_of_input_t = Test_printers.(pair (array int64) (array int64))

    let string_of_output_t = Test_printers.bool
  end

  let transform (a, b) = Cpuid_helpers.(is_equal a b)

  let tests =
    `QuickAndAutoDocumented
      [
        (([||], [||]), true)
      ; (([|1L; 2L; 3L|], [|1L; 2L; 3L|]), true)
      ; (([|1L; 2L; 3L|], [||]), false)
      ; (([||], [|1L; 2L; 3L|]), false)
      ; (([|7L; 0L|], [|7L|]), true)
      ; (([|7L|], [|7L; 0L|]), true)
      ; (([|7L; 1L; 0L|], [|7L; 1L|]), true)
      ; (([|7L; 1L|], [|7L; 1L|]), true)
      ; (([|7L; 1L|], [|7L|]), false)
      ; (([|7L|], [|7L; 1L|]), false)
      ; (([|1L; 7L|], [|7L; 1L|]), false)
      ]
end)

module Comparisons = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array * int64 array

    type output_t = bool * bool

    let string_of_input_t = Test_printers.(pair (array int64) (array int64))

    let string_of_output_t = Test_printers.(pair bool bool)
  end

  let transform (a, b) = Cpuid_helpers.(is_subset a b, is_strict_subset a b)

  let tests =
    `QuickAndAutoDocumented
      [
        (* The following are counterintuitive, because intersection with the empty
         * set is treated as identity, and intersection is used in is_subset.
         * Since there are no empty feature sets in reality, these are artificial
         * scenarios. *)
        (([||], [||]), (true, false))
      ; (([|1L; 2L; 3L|], [||]), (true, true))
      ; (([||], [|1L; 2L; 3L|]), (false, false))
      ; (* Note that feature flags are automatically zero-extended when compared.
         * These tests are relevant in upgrade scenarios, if new CPUID leaves are
         * introduced. *)
        (([|7L; 3L|], [|5L|]), (false, false))
      ; (([|5L|], [|7L; 3L|]), (true, true))
      ; (([|1L|], [|1L|]), (true, false))
      ; (([|1L|], [|1L; 0L|]), (true, false))
      ; (([|1L; 0L|], [|1L|]), (true, false))
      ; (* Below are the more common cases *)
        ( ( features_of_string
              "07cbfbff-04082201-20100800-00000001-00000000-00000000-00000000-00000000-00000000"
          , features_of_string
              "07c9cbf5-80082201-20100800-00000001-00000000-00000000-00000000-00000000-00000000"
          )
        , (false, false) )
      ; (([|0b00000000L|], [|0b11111111L|]), (true, true))
      ; (([|0b11111111L|], [|0b11111111L|]), (true, false))
      ; (([|0b01111111L|], [|0b11111111L|]), (true, true))
      ; (([|0b00000111L|], [|0b00001111L|]), (true, true))
      ; (([|0b00011111L|], [|0b00001111L|]), (false, false))
      ; ( ([|0b00000000L; 0b11111111L|], [|0b11111111L; 0b00000000L|])
        , (false, false) )
      ; ( ([|0b11111111L; 0b01010101L|], [|0b11111111L; 0b01010101L|])
        , (true, false) )
      ; ( ([|0b01111111L; 0b10000000L|], [|0b11111111L; 0b00000000L|])
        , (false, false) )
      ; ( ([|0b00000111L; 0b11100000L|], [|0b00001111L; 0b11110000L|])
        , (true, true) )
      ]
end)

module Accessors = Generic.MakeStateless (struct
  module Io = struct
    type input_t = (string * string) list

    type output_t = string * int * int * int64 array * int64 array

    let string_of_input_t = Test_printers.(assoc_list string string)

    let string_of_output_t =
      Test_printers.(tuple5 string int int (array int64) (array int64))
  end

  let transform record =
    let open Map_check in
    ( getf vendor record
    , getf socket_count record
    , getf cpu_count record
    , getf features_pv record
    , getf features_hvm record )

  let tests =
    `QuickAndAutoDocumented
      [
        ( [
            ("vendor", "Intel")
          ; ("socket_count", "1")
          ; ("cpu_count", "1")
          ; ("features_pv", "00000001-00000002-00000003")
          ; ("features_hvm", "0000000a-0000000b-0000000c")
          ]
        , ("Intel", 1, 1, [|1L; 2L; 3L|], [|0xaL; 0xbL; 0xcL|]) )
      ; ( [
            ("vendor", "Amd")
          ; ("socket_count", "6")
          ; ("cpu_count", "24")
          ; ("features_pv", "00000001")
          ; ("features_hvm", "")
          ]
        , ("Amd", 6, 24, [|1L|], [||]) )
      ]
end)

module Setters = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string * int * int * int64 array * int64 array

    type output_t = (string * string) list

    let string_of_input_t =
      Test_printers.(tuple5 string int int (array int64) (array int64))

    let string_of_output_t = Test_printers.(assoc_list string string)
  end

  let transform (name, sockets, cpus, pv, hvm) =
    let open Map_check in
    []
    |> setf vendor name
    |> setf socket_count sockets
    |> setf cpu_count cpus
    |> setf features_pv pv
    |> setf features_hvm hvm
    |> List.sort compare

  let tests =
    `QuickAndAutoDocumented
      [
        ( ("Intel", 1, 1, [|1L; 2L; 3L|], [|0xaL; 0xbL; 0xcL|])
        , List.sort compare
            [
              ("vendor", "Intel")
            ; ("socket_count", "1")
            ; ("cpu_count", "1")
            ; ("features_pv", "00000001-00000002-00000003")
            ; ("features_hvm", "0000000a-0000000b-0000000c")
            ] )
      ; ( ("Amd", 6, 24, [|1L|], [||])
        , List.sort compare
            [
              ("vendor", "Amd")
            ; ("socket_count", "6")
            ; ("cpu_count", "24")
            ; ("features_pv", "00000001")
            ; ("features_hvm", "")
            ] )
      ]
end)

module Modifiers = Generic.MakeStateless (struct
  module Io = struct
    type input_t = (string * string) list

    type output_t = (string * string) list

    let string_of_input_t = Test_printers.(assoc_list string string)

    let string_of_output_t = Test_printers.(assoc_list string string)
  end

  let transform record =
    let open Map_check in
    record
    |> setf vendor (getf vendor record)
    |> setf socket_count (getf socket_count record)
    |> setf cpu_count (getf cpu_count record)
    |> setf features_pv (getf features_pv record)
    |> setf features_hvm (getf features_hvm record)
    |> List.sort compare

  let tests =
    `QuickAndAutoDocumented
      [
        ( [
            ("cpu_count", "1")
          ; ("features_hvm", "0000000a-0000000b-0000000c")
          ; ("features_pv", "00000001-00000002-00000003")
          ; ("socket_count", "1")
          ; ("vendor", "Intel")
          ]
        , [
            ("cpu_count", "1")
          ; ("features_hvm", "0000000a-0000000b-0000000c")
          ; ("features_pv", "00000001-00000002-00000003")
          ; ("socket_count", "1")
          ; ("vendor", "Intel")
          ] )
      ]
end)

let domain_type : API.domain_type Test_printers.printer =
  Record_util.domain_type_to_string

module ResetCPUFlags = Generic.MakeStateful (struct
  module Io = struct
    type input_t = (string * API.domain_type) list

    type output_t = string list

    let string_of_input_t = Test_printers.(list (pair string domain_type))

    let string_of_output_t = Test_printers.(list string)
  end

  module State = Test_state.XapiDb

  let features_hvm = "feedface-feedface"

  let features_pv = "deadbeef-deadbeef"

  let load_input __context cases =
    let cpu_info =
      [
        ("cpu_count", "1")
      ; ("socket_count", "1")
      ; ("vendor", "Abacus")
      ; ("features_pv", features_pv)
      ; ("features_hvm", features_hvm)
      ]
    in
    List.iter
      (fun self -> Db.Host.set_cpu_info ~__context ~self ~value:cpu_info)
      (Db.Host.get_all ~__context) ;
    Db.Pool.set_cpu_info ~__context
      ~self:(Db.Pool.get_all ~__context |> List.hd)
      ~value:cpu_info ;
    let vms =
      List.map
        (fun (name_label, domain_type) ->
          Test_common.make_vm ~__context ~name_label ~domain_type ())
        cases
    in
    List.iter (fun vm -> Cpuid_helpers.reset_cpu_flags ~__context ~vm) vms

  let extract_output __context vms =
    let get_flags (label, _) =
      let self = List.hd (Db.VM.get_by_name_label ~__context ~label) in
      let flags = Db.VM.get_last_boot_CPU_flags ~__context ~self in
      try List.assoc Xapi_globs.cpu_info_features_key flags
      with Not_found -> ""
    in
    List.map get_flags vms

  (* Tuples of ((features_hvm * features_pv) list, (expected last_boot_CPU_flags) *)
  let tests =
    `QuickAndAutoDocumented
      [
        ([("a", `hvm)], [features_hvm])
      ; ([("a", `pv)], [features_pv])
      ; ([("a", `pv_in_pvh)], [features_hvm])
      ; ( [("a", `hvm); ("b", `pv); ("c", `pv_in_pvh)]
        , [features_hvm; features_pv; features_hvm] )
      ]
end)

let string_of_unit_result =
  Fmt.(str "%a" Dump.(result ~ok:(any "()") ~error:exn))

module AssertVMIsCompatible = Generic.MakeStateful (struct
  module Io = struct
    type input_t = string * API.domain_type * (string * string) list

    type output_t = (unit, exn) result

    let string_of_input_t =
      Test_printers.(tuple3 string domain_type (assoc_list string string))

    let string_of_output_t = string_of_unit_result
  end

  module State = Test_state.XapiDb

  let features_hvm_host =
    "feedface-feedface-feedface-feedface-feedface-00000000-feedface-feedface-feedface-feedface-feedface-feedface-feedface-feedface-feedface"

  let features_pv_host =
    "deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef"

  let features_hvm =
    "feedface-feedface-feedface-feedface-feedface-00000810-feedface-feedface-feedface-feedface-feedface-feedface-feedface-feedface-feedface"

  let features_pv =
    "deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef-deadbeef"

  let load_input __context (name_label, domain_type, last_boot_flags) =
    let cpu_info =
      [
        ("cpu_count", "1")
      ; ("socket_count", "1")
      ; ("vendor", "Abacus")
      ; ("features_pv", features_pv)
      ; ("features_hvm", features_hvm)
      ; ("features_pv_host", features_pv_host)
      ; ("features_hvm_host", features_hvm_host)
      ]
    in
    List.iter
      (fun self -> Db.Host.set_cpu_info ~__context ~self ~value:cpu_info)
      (Db.Host.get_all ~__context) ;
    let cpu_info =
      List.filter
        (fun (k, _) -> not (Astring.String.is_suffix ~affix:"host" k))
        cpu_info
    in
    Db.Pool.set_cpu_info ~__context
      ~self:(Db.Pool.get_all ~__context |> List.hd)
      ~value:cpu_info ;
    let self = Test_common.make_vm ~__context ~name_label ~domain_type () in
    Db.VM.set_last_boot_CPU_flags ~__context ~self ~value:last_boot_flags ;
    let metrics = Db.VM.get_metrics ~__context ~self in
    Db.VM_metrics.set_current_domain_type ~__context ~self:metrics
      ~value:domain_type ;
    Db.VM.set_power_state ~__context ~self ~value:`Running

  let extract_output __context (label, _, _) =
    let host = List.hd @@ Db.Host.get_all ~__context in
    let vm = List.hd (Db.VM.get_by_name_label ~__context ~label) in
    try Ok (Cpuid_helpers.assert_vm_is_compatible ~__context ~vm ~host ()) with
    (* Filter out opaquerefs which make matching this exception difficult *)
    | Api_errors.Server_error (vm_incompatible_with_this_host, data) ->
        Printexc.print_backtrace stderr ;
        Error
          (Api_errors.Server_error
             ( vm_incompatible_with_this_host
             , List.filter
                 (fun s ->
                   not @@ Astring.String.is_prefix ~affix:"OpaqueRef:" s)
                 data ))
    | e ->
        Error e

  let tests =
    `QuickAndAutoDocumented
      [
        (* HVM *)
        ( ( "a"
          , `hvm
          , Xapi_globs.
              [
                (cpu_info_vendor_key, "Abacus")
              ; (cpu_info_features_key, features_hvm)
              ] )
        , Ok () )
      ; ( ( "a"
          , `hvm
          , Xapi_globs.
              [
                (cpu_info_vendor_key, "Abacus")
              ; ( cpu_info_features_key
                , "cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe"
                )
              ] )
        , Error
            Api_errors.(
              Server_error
                ( vm_incompatible_with_this_host
                , [
                    "VM last booted on a CPU with features this host's CPU \
                     does not have."
                  ] )) )
      ; ( ( "a"
          , `hvm
          , Xapi_globs.
              [
                (cpu_info_vendor_key, "Napier's Bones")
              ; (cpu_info_features_key, features_hvm)
              ] )
        , Error
            Api_errors.(
              Server_error
                ( vm_incompatible_with_this_host
                , [
                    "VM last booted on a host which had a CPU from a different \
                     vendor."
                  ] )) )
      ; (* PV *)
        ( ( "a"
          , `pv
          , Xapi_globs.
              [
                (cpu_info_vendor_key, "Abacus")
              ; (cpu_info_features_key, features_pv)
              ] )
        , Ok () )
      ; ( ( "a"
          , `pv
          , Xapi_globs.
              [
                (cpu_info_vendor_key, "Abacus")
              ; ( cpu_info_features_key
                , "cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe-cafecafe"
                )
              ] )
        , Error
            Api_errors.(
              Server_error
                ( vm_incompatible_with_this_host
                , [
                    "VM last booted on a CPU with features this host's CPU \
                     does not have."
                  ] )) )
      ; ( ( "a"
          , `pv
          , Xapi_globs.
              [
                (cpu_info_vendor_key, "Napier's Bones")
              ; (cpu_info_features_key, features_pv)
              ] )
        , Error
            Api_errors.(
              Server_error
                ( vm_incompatible_with_this_host
                , [
                    "VM last booted on a host which had a CPU from a different \
                     vendor."
                  ] )) )
      ; (* TAA *)
        ( ( "a"
          , `hvm
          , Xapi_globs.
              [
                (cpu_info_vendor_key, "Abacus")
              ; ( cpu_info_features_key
                , "feedface-feedface-feedface-feedface-feedface-00000810-feedface-feedface-feedface-feedface-feedface-feedface-feedface-feedface-feedface"
                )
              ] )
        , Ok () )
      ; ( ( "a"
          , `hvm
          , Xapi_globs.
              [
                (cpu_info_vendor_key, "Abacus")
              ; ( cpu_info_features_key
                , "feedface-feedface-feedface-feedface-feedface-00000811-feedface-feedface-feedface-feedface-feedface-feedface-feedface-feedface-feedface"
                )
              ] )
        , Error
            Api_errors.(
              Server_error
                ( vm_incompatible_with_this_host
                , [
                    "VM last booted on a CPU with features this host's CPU \
                     does not have."
                  ] )) )
      ]
end)

let tests =
  make_suite "cpuid_helpers_"
    [
      ("string_of_features", StringOfFeatures.tests)
    ; ("features_of_string", FeaturesOfString.tests)
    ; ("roundtrip_features_to_features", RoundTripFeaturesToFeatures.tests)
    ; ("roundtrip_string_to_features", RoundTripStringToString.tests)
    ; ("parse_failure", ParseFailure.tests)
    ; ("extend", Extend.tests)
    ; ("zero_extend", ZeroExtend.tests)
    ; ("intersect", Intersect.tests)
    ; ("equality", Equality.tests)
    ; ("comparisons", Comparisons.tests)
    ; ("accessors", Accessors.tests)
    ; ("setters", Setters.tests)
    ; ("modifiers", Modifiers.tests)
    ; ("reset_cpu_flags", ResetCPUFlags.tests)
    ; ("test_assert_vm_is_compatible", AssertVMIsCompatible.tests)
    ]
