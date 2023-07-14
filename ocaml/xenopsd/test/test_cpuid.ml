(*
 * Copyright (C) Cloud Software Group.
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
open Cpuid

module StringOfFeatures = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array

    type output_t = string

    let string_of_input_t = Test_printers.(array int64)

    let string_of_output_t = Test_printers.string
  end

  let transform = string_of_features

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

  let transform = features_of_string

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

  let transform x = x |> string_of_features |> features_of_string

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

  let transform x = x |> features_of_string |> string_of_features

  let tests =
    `QuickAndAutoDocumented
      (List.map
         (fun x -> (x, x))
         ["00000000-00000002-0000007b"; "00000001"; "00000000"; ""]
      )
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
      ignore (features_of_string x) ;
      raise NoExceptionRaised
    with e -> e

  let tests =
    `QuickAndAutoDocumented
      (List.map
         (fun x -> (x, InvalidFeatureString x))
         ["foo bar baz"; "fgfg-1234"; "0123-foo"; "foo-0123"; "-1234"; "1234-"]
      )
end)

module Extend = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array * int64 array

    type output_t = int64 array

    let string_of_input_t = Test_printers.(pair (array int64) (array int64))

    let string_of_output_t = Test_printers.(array int64)
  end

  let transform (arr0, arr1) = extend arr0 arr1

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

  let transform (arr, len) = zero_extend arr len

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

  let transform (a, b) = intersect a b

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
        , [|0b00000000L; 0b00000000L|]
        )
      ; ( ([|0b11111111L; 0b01010101L|], [|0b11111111L; 0b01010101L|])
        , [|0b11111111L; 0b01010101L|]
        )
      ; ( ([|0b01111111L; 0b10000000L|], [|0b11111111L; 0b00000000L|])
        , [|0b01111111L; 0b00000000L|]
        )
      ; ( ([|0b00000111L; 0b11100000L|], [|0b00001111L; 0b11110000L|])
        , [|0b00000111L; 0b11100000L|]
        )
      ]
end)

module Equality = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int64 array * int64 array

    type output_t = bool

    let string_of_input_t = Test_printers.(pair (array int64) (array int64))

    let string_of_output_t = Test_printers.bool
  end

  let transform (a, b) = is_equal a b

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

  let transform (a, b) = (is_subset a b, is_strict_subset a b)

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
        , (false, false)
        )
      ; (([|0b00000000L|], [|0b11111111L|]), (true, true))
      ; (([|0b11111111L|], [|0b11111111L|]), (true, false))
      ; (([|0b01111111L|], [|0b11111111L|]), (true, true))
      ; (([|0b00000111L|], [|0b00001111L|]), (true, true))
      ; (([|0b00011111L|], [|0b00001111L|]), (false, false))
      ; ( ([|0b00000000L; 0b11111111L|], [|0b11111111L; 0b00000000L|])
        , (false, false)
        )
      ; ( ([|0b11111111L; 0b01010101L|], [|0b11111111L; 0b01010101L|])
        , (true, false)
        )
      ; ( ([|0b01111111L; 0b10000000L|], [|0b11111111L; 0b00000000L|])
        , (false, false)
        )
      ; ( ([|0b00000111L; 0b11100000L|], [|0b00001111L; 0b11110000L|])
        , (true, true)
        )
      ]
end)

let tests =
  make_suite "cpuid_"
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
    ]
