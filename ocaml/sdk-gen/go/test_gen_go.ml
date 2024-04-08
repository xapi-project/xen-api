(* Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

open Test_highlevel
open CommonFunctions
open Gen_go_helper

let test_data_dir = "test_data"

let string_of_file filename =
  string_of_file (test_data_dir // filename) |> String.trim

let check_true str = Alcotest.(check bool) str true

module SnakeToCamelTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = string

    let string_of_input_t = Test_printers.string

    let string_of_output_t = Test_printers.string
  end

  let transform = snake_to_camel

  let tests =
    `QuickAndAutoDocumented
      [
        ("ni_hao-Nanjin", "NiHaoNanjin")
      ; ("ni_hao", "NiHao")
      ; ("nanjing", "Nanjing")
      ]
end)

let rec is_same_struct_of_value (value1 : Mustache.Json.value)
    (value2 : Mustache.Json.value) =
  match (value1, value2) with
  | `Null, _ | _, `Null ->
      true
  | `Bool _, `Bool _ ->
      true
  | `String _, `String _ ->
      true
  | `Float _, `Float _ ->
      true
  | `O o1, `O o2 ->
      let keys1 = List.sort compare (List.map fst o1) in
      let keys2 = List.sort compare (List.map fst o2) in
      if keys1 <> keys2 then
        false
      else
        List.for_all
          (fun key ->
            is_same_struct_of_value (List.assoc key o1) (List.assoc key o2)
          )
          keys1
  | `A [], `A [] ->
      true
  | `A [], `A (x :: xs) ->
      List.for_all (fun obj -> is_same_struct_of_value x obj) xs
  | `A (x :: xs), `A ys ->
      List.for_all (fun obj -> is_same_struct_of_value x obj) (xs @ ys)
  | _ ->
      false

let is_same_struct (obj1 : Mustache.Json.t) (obj2 : Mustache.Json.t) =
  match (obj1, obj2) with
  | `O o1, `O o2 ->
      let keys1 = List.sort compare (List.map fst o1) in
      let keys2 = List.sort compare (List.map fst o2) in
      if keys1 <> keys2 then
        false
      else
        List.for_all
          (fun key ->
            is_same_struct_of_value (List.assoc key o1) (List.assoc key o2)
          )
          keys1
  | `A [], `A [] ->
      true
  | `A [], `A (x :: xs) ->
      List.for_all (fun obj -> is_same_struct_of_value x obj) xs
  | `A (x :: xs), `A ys ->
      List.for_all (fun obj -> is_same_struct_of_value x obj) (xs @ ys)
  | _ ->
      false

let rec string_of_json_value (value : Mustache.Json.value) : string =
  match value with
  | `Null ->
      "null"
  | `Bool b ->
      string_of_bool b
  | `Float f ->
      string_of_float f
  | `String s ->
      "\"" ^ s ^ "\""
  | `A arr ->
      "[" ^ String.concat ", " (List.map string_of_json_value arr) ^ "]"
  | `O obj ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (k, v) -> "\"" ^ k ^ "\": " ^ string_of_json_value v)
             obj
          )
      ^ "}"

let string_of_json (json : Mustache.Json.t) : string =
  match json with
  | `A arr ->
      "[" ^ String.concat ", " (List.map string_of_json_value arr) ^ "]"
  | `O obj ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (k, v) -> "\"" ^ k ^ "\": " ^ string_of_json_value v)
             obj
          )
      ^ "}"

let record : Mustache.Json.t =
  `O
    [
      ("name", `String "Session")
    ; ("description", `String "A session")
    ; ("session", `Bool true)
    ; ("event", `Bool false)
    ; ( "fields"
      , `A
          [
            `O
              [
                ("name", `String "UUID")
              ; ("description", `String "Unique identifier/object reference")
              ; ("type", `String "string")
              ]
          ; `O
              [
                ("name", `String "ThisHost")
              ; ("description", `String "Currently connected host")
              ; ("type", `String "HostRef")
              ]
          ]
      )
    ]

let header : Mustache.Json.t =
  `O
    [
      ( "modules"
      , `O
          [
            ("import", `Bool true)
          ; ( "items"
            , `A
                [
                  `O [("name", `String "fmt"); ("sname", `Null)]
                ; `O [("name", `String "time"); ("sname", `String "time1")]
                ]
            )
          ]
      )
    ]

let enums : Mustache.Json.t =
  `O
    [
      ( "enums"
      , `A
          [
            `O
              [
                ("name", `String "VMTelemetryFrequency")
              ; ( "values"
                , `A
                    [
                      `O
                        [
                          ("value", `String "daily")
                        ; ("doc", `String "Run telemetry task daily")
                        ; ("name", `String "VMTelemetryFrequencyDaily")
                        ; ("type", `String "VMTelemetryFrequency")
                        ]
                    ; `O
                        [
                          ("value", `String "weekly")
                        ; ("doc", `String "Run telemetry task weekly")
                        ; ("name", `String "VMTelemetryFrequencyWeekly")
                        ; ("type", `String "VMTelemetryFrequency")
                        ]
                    ]
                )
              ]
          ]
      )
    ]

let api_errors : Mustache.Json.t =
  `O
    [
      ( "api_errors"
      , `A
          [
            `O [("name", `String "MESSAGE_DEPRECATED")]
          ; `O [("name", `String "MESSAGE_REMOVED")]
          ]
      )
    ]

let api_messages : Mustache.Json.t =
  `O
    [
      ( "api_messages"
      , `A
          [
            `O [("name", `String "HA_STATEFILE_LOST")]
          ; `O [("name", `String "METADATA_LUN_HEALTHY")]
          ]
      )
    ]

module TemplatesTest = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string * Mustache.Json.t

    type output_t = string

    let string_of_input_t (template, json) =
      "The template is " ^ template ^ " with json: " ^ string_of_json json

    let string_of_output_t = Test_printers.string
  end

  let transform (template, json) = render_template template json |> String.trim

  let file_header_rendered = string_of_file "file_header.go"

  let record_rendered = string_of_file "record.go"

  let enums_rendered = string_of_file "enum.go"

  let api_errors_rendered = string_of_file "api_errors.go"

  let api_messages_rendered = string_of_file "api_messages.go"

  let tests =
    `QuickAndAutoDocumented
      [
        (("FileHeader.mustache", header), file_header_rendered)
      ; (("Record.mustache", record), record_rendered)
      ; (("Enum.mustache", enums), enums_rendered)
      ; (("APIErrors.mustache", api_errors), api_errors_rendered)
      ; (("APIMessages.mustache", api_messages), api_messages_rendered)
      ]
end)

let generated_json_tests =
  let merge (obj1 : Mustache.Json.t) (obj2 : Mustache.Json.t) =
    match (obj1, obj2) with
    | `O list1, `O list2 ->
        `O (list1 @ list2)
    | _ ->
        `O []
  in
  let jsons () =
    let json = enums |> merge record |> merge header in
    let objects = Json.xenapi objects in
    check_true "Mustache.Json of records has right structure"
    @@ List.for_all (fun (_, obj) -> is_same_struct obj json) objects
  in
  let errors_and_messages () =
    let errors = `O [("api_errors", `A Json.api_errors)] in
    let messages = `O [("api_messages", `A Json.api_messages)] in
    check_true "Mustache.Json of errors and messages has right structure"
    @@ (is_same_struct errors api_errors && is_same_struct messages api_messages)
  in
  [
    ("jsons", `Quick, jsons)
  ; ("errors_and_messages", `Quick, errors_and_messages)
  ]

let tests =
  make_suite "gen_go_binding_"
    [
      ("snake_to_camel", SnakeToCamelTest.tests)
    ; ("templates", TemplatesTest.tests)
    ; ("generated_mustache_jsons", generated_json_tests)
    ]

let () = Alcotest.run "Gen go binding" tests
