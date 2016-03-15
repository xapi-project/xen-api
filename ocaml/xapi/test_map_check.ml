(*
 * Copyright (C) 2006-2015 Citrix Systems Inc.
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
open Map_check
open OUnit
open Test_common
open Test_highlevel

let string_of_requirement requirement =
  Printf.sprintf "{key = \"%s\"; default_value = \"%s\"}"
    requirement.key (Test_printers.(option string) requirement.default_value)

let true_fun = (fun _ -> true)

let false_fun = (fun _ -> false)

module AddDefaults = Generic.Make(struct
    module Io = struct
      type input_t = (requirement list) * ((string * string) list)
      type output_t = (string * string) list

      let string_of_input_t =
        Test_printers.(assoc_pair
                         (list string_of_requirement)
                         (assoc_list string string))
      let string_of_output_t = Test_printers.(assoc_list string string)
    end

    let transform (requirements, old_map) = add_defaults requirements old_map

    let tests = [
      (* If default value is None, no value should be added. *)
      (
        [{key = "abc"; default_value = None; is_valid_value = true_fun}],
        []
      ),
      [];
      (* If default value is Some _, the default should be added. *)
      (
        [{key = "abc"; default_value = Some "def"; is_valid_value = true_fun}],
        []
      ),
      ["abc", "def"];
      (* If default value is None, an existing value should not be overwritten. *)
      (
        [{key = "abc"; default_value = None; is_valid_value = true_fun}],
        ["abc", "ghi"]
      ),
      ["abc", "ghi"];
      (* If default value is Some _, an existing value should not be overwritten. *)
      (
        [{key = "abc"; default_value = Some "def"; is_valid_value = true_fun}],
        ["abc", "ghi"]
      ),
      ["abc", "ghi"];
    ]
  end)

module ValidateKVPair = Generic.Make(struct
    module Io = struct
      type input_t = requirement list * string * string
      type output_t = (exn, unit) Either.t

      let string_of_input_t (requirements, key, value) =
        Printf.sprintf "%s, %s, %s"
          ((Test_printers.list string_of_requirement) requirements) key value
      let string_of_output_t = Test_printers.(either exn unit)
    end

    let transform (requirements, key, value) =
      try Either.Right (validate_kvpair "test_field" requirements (key, value))
      with e -> Either.Left e

    let tests = [
      (* If all values are valid, the exception should not be thrown. *)
      (
        [{key = "abc"; default_value = None; is_valid_value = true_fun}],
        "abc", "def"
      ),
      Either.Right ();
      (* If there is no valid value, the exception should always be thrown. *)
      (
        [{key = "abc"; default_value = None; is_valid_value = false_fun}],
        "abc", "def"
      ),
      Either.Left (Api_errors.(Server_error
                                 (invalid_value, ["test_field"; "abc = def"])));
    ]
  end)

module Accessors = Generic.Make(struct
    module Io = struct
      type input_t = string * (string * string) list
      type output_t = int

      let string_of_input_t = Test_printers.(pair string (list (pair string string)))
      let string_of_output_t = Test_printers.int
    end

    let transform (key, map) =
      getf (field key int) map

    let tests = [
      ("a", ["a", "1"]), 1;
    ]
  end)

let string_of_ty = function
  | String -> "String"
  | _ -> ""

let string_of_ks ks =
  let field,kss = ks in
  List.map (fun (a, b) ->
    let inner_string = List.map (fun (c, d) -> let e, f = d in
    c ^ "," ^ (string_of_ty e) ^ f ) b |> String.concat ";" in
    "[" ^ a ^ "," ^ "[" ^ inner_string ^ "]]"
  ) kss
  |> String.concat ";"

module AssertAllKeys = Generic.Make(struct
  module Io = struct
    type input_t = string *
      (string * (string * (string * (Map_check.key_type * string))list)list) *
      ((string * string) list) *
      ((string * string) list)
    type output_t = (string * string) list

    let string_of_input_t (ty, ks, value, db) = Printf.sprintf "frequency=%s, keys=%s, input_value=%s, db_value=%s"
      ty
      (string_of_ks ks)
      (Test_printers.(assoc_list string string) value)
      (Test_printers.(assoc_list string string) db)
    let string_of_output_t = Test_printers.(assoc_list string string)
  end

  let transform (ty, ks, value, db) = assert_all_keys ty ks value db

  let tests = [
    (* Tests for hourly snapshots *)
    ("hourly", ("", ["hourly", ["min",(String,"")]]), ["min","30"], ["min", "0"]), ["min","30"];
    ("hourly", ("", ["hourly", ["min",(String,"")]]), ["hour","1";"min","0"], ["min", "0"]), ["min","0"];
    ("hourly", ("", ["hourly", ["min",(String,"")]]), ["day","Monday";"hour","1";"min","0"], ["min", "0"]), ["min","0"];

    (* Change hourly snapshots to daily and weekly *)
    ("daily", ("", ["daily", ["hour",(String,"");"min",(String,"")]]), ["hour","10";"min","30"], ["min", "0"]), ["hour","10";"min","30"];
    ("weekly", ("", ["weekly", ["day",(String,"");"hour",(String,"");"min",(String,"")]]), ["day","Monday";"hour","10";"min","30"], ["min","0"]), ["day","Monday";"hour","10";"min","30"];

    (* Tests for daily snapshots *)
    ("daily", ("", ["daily", ["hour",(String,""); "min",(String,"")]]), ["hour","10";"min","30"], ["hour","0";"min","0"]), ["hour","10";"min","30"];
    ("daily", ("", ["daily", ["hour",(String,""); "min",(String,"")]]), ["day","Monday";"hour","0";"min","0"], ["hour","0";"min","0"]), ["hour","0";"min","0"];
    ("daily", ("", ["daily", ["hour",(String,""); "min",(String,"")]]), ["min","30"], ["hour","0";"min","0"]), ["hour","0";"min","30"];
    ("daily", ("", ["daily", ["hour",(String,""); "min",(String,"")]]), ["hour","10"], ["hour","0";"min","0"]), ["hour","10";"min","0"];

    (* Change daily snapshots to hourly and weekly *)
    ("hourly", ("", ["hourly", ["min",(String,"")]]), ["min","30"], ["hour","0";"min", "0"]), ["min","30"];
    ("weekly", ("", ["weekly", ["day",(String,"");"hour",(String,"");"min",(String,"")]]), ["day","Monday";"hour","10";"min","30"], ["hour","0";"min","0"]), ["day","Monday";"hour","10";"min","30"];

    (* Tests for weekly snapshots *)
    ("weekly", ("", ["weekly", ["day",(String,"");"hour",(String,"");"min",(String,"")]]), ["day","Monday";"hour","10";"min","30"], ["day","Wednesday";"hour","0";"min","0"]), ["day","Monday";"hour","10";"min","30"];
    ("weekly", ("", ["weekly", ["day",(String,"");"hour",(String,"");"min",(String,"")]]), ["day","Wednesday"], ["day","Monday";"hour","0";"min","0"]), ["day","Wednesday";"hour","0";"min","0"];
    ("weekly", ("", ["weekly", ["day",(String,"");"hour",(String,"");"min",(String,"")]]), ["hour","10"], ["day","Monday";"hour","0";"min","0"]), ["day","Monday";"hour","10";"min","0"];
    ("weekly", ("", ["weekly", ["day",(String,"");"hour",(String,"");"min",(String,"")]]), ["min","30"], ["day","Monday";"hour","0";"min","0"]), ["day","Monday";"hour","0";"min","30"];

    (* Change weekly snapshots to hourly and daily *)
    ("hourly", ("", ["hourly", ["min",(String,"")]]), ["min","30"], ["day","Monday";"hour","0";"min","0"]), ["min","30"];
    ("daily", ("", ["daily", ["hour",(String,""); "min",(String,"")]]), ["hour","10";"min","30"], ["day","Monday";"hour","0";"min","0"]), ["hour","10";"min","30"];
  ]
end)

module AssertKeys = Generic.Make(struct
  module Io = struct
    type input_t = string *
      (string * (string * (string * (Map_check.key_type * string))list)list) *
      ((string * string) list) *
      ((string * string) list)
    type output_t = (exn, (string * string) list) Either.t
             
    let string_of_input_t (ty, ks, value, db) = Printf.sprintf "keys=%s, input_value=%s, db_value=%s"
      (string_of_ks ks)
      (Test_printers.(assoc_list string string) value)
      (Test_printers.(assoc_list string string) db)
    let string_of_output_t = Test_printers.(either exn (assoc_list string string))
  end

  let transform (ty, ks, value, db) =
    try Either.Right (assert_keys ty ks value db)
    with e -> Either.Left e
 
    let tests = [
      (* Tests hourly keys *)
      ("", ("", ["", ["min",(String,"")]]), ["min","30"], ["min", "0"]), Either.Right (["min","30"]);
      ("", ("", ["", ["min",(String,"")]]), ["hour","0"], ["min", "0"]), Either.Left (Api_errors.(Server_error (invalid_value, [":hour"; "0"])));
      ("", ("", ["", ["min",(String,"")]]), ["day","Monday"], ["min", "0"]), Either.Left (Api_errors.(Server_error (invalid_value, [":day"; "Monday"])));

      (* Tests daily keys *)
      ("", ("", ["", ["hour",(String,"");"min",(String,"")]]), ["hour","10";"min","30"], ["hour","0";"min","0"]), Either.Right (["hour","10";"min","30"]);
      ("", ("", ["", ["hour",(String,"");"min",(String,"")]]), ["hour","10"], ["hour","0";"min","0"]), Either.Right (["hour","10";"min","0"]);
      ("", ("", ["", ["hour",(String,"");"min",(String,"")]]), ["min","30"], ["hour","0";"min","0"]), Either.Right (["hour","0";"min","30"]);
      ("", ("", ["", ["hour",(String,"");"min",(String,"")]]), ["day","Monday"], ["hour","0";"min","0"]), Either.Left (Api_errors.(Server_error (invalid_value, [":day"; "Monday"])));

      (* Tests weekly keys *)
      ("", ("", ["", ["day",(String,"");"hour",(String,"");"min",(String,"")]]), ["day","Wednesday";"hour","10";"min","30"], ["day","Monday";"hour","0";"min","0"]), Either.Right (["day","Wednesday";"hour","10";"min","30"]);
      ("", ("", ["", ["day",(String,"");"hour",(String,"");"min",(String,"")]]), ["day","Wednesday"], ["day","Monday";"hour","0";"min","0"]), Either.Right (["day","Wednesday";"hour","0";"min","0"]);
      ("", ("", ["", ["day",(String,"");"hour",(String,"");"min",(String,"")]]), ["hour","10"], ["day","Monday";"hour","0";"min","0"]), Either.Right (["day","Monday";"hour","10";"min","0"]);
      ("", ("", ["", ["day",(String,"");"hour",(String,"");"min",(String,"")]]), ["min","30"], ["day","Monday";"hour","0";"min","0"]), Either.Right (["day","Monday";"hour","0";"min","30"]);
    ]

end)

let test =
  "test_map_check" >:::
  [
    "test_add_defaults" >::: AddDefaults.tests;
    "test_validate_kvpair" >::: ValidateKVPair.tests;
    "test_accessors" >::: Accessors.tests;
    "test_assert_all_keys" >::: AssertAllKeys.tests;
    "test_assert_keys" >::: AssertKeys.tests;
  ]
