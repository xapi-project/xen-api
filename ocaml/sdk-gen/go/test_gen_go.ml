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

let schema_check keys checker members =
  let compare_keys lst1 lst2 =
    let sorted_lst1 = List.sort String.compare lst1 in
    let sorted_lst2 = List.sort String.compare lst2 in
    List.compare String.compare sorted_lst1 sorted_lst2 = 0
  in
  let keys' = List.map (fun (k, _) -> k) members in
  compare_keys keys keys' && List.for_all checker members

(* field *)
let verify_field_member = function
  | "name", `String _ | "description", `String _ | "type", `String _ ->
      true
  | _ ->
      false

let field_keys = ["name"; "description"; "type"]

let verify_field = function
  | `O members ->
      schema_check field_keys verify_field_member members
  | _ ->
      false

(* module *)
let verify_module_member = function
  | "name", `String _ ->
      true
  | "sname", `Null ->
      true
  | _ ->
      false

let module_keys = ["name"; "sname"]

let verify_modules_item = function
  | `O members ->
      schema_check module_keys verify_module_member members
  | _ ->
      false

(* modules *)
let modules_keys = ["import"; "items"]

let verify_modules_member = function
  | "import", `Bool _ ->
      true
  | "items", `A items ->
      List.for_all verify_modules_item items
  | _ ->
      false

let enum_values_keys = ["value"; "doc"; "name"; "type"]

(* enums *)
let verify_enum_values_member = function
  | "value", `String _
  | "doc", `String _
  | "name", `String _
  | "type", `String _ ->
      true
  | _ ->
      false

let verify_enum_values : Mustache.Json.value -> bool = function
  | `O values ->
      schema_check enum_values_keys verify_enum_values_member values
  | _ ->
      false

let enum_keys = ["name"; "values"]

let verify_enum_content : string * Mustache.Json.value -> bool = function
  | "name", `String _ ->
      true
  | "values", `A values ->
      List.for_all verify_enum_values values
  | _ ->
      false

let verify_enum : Mustache.Json.value -> bool = function
  | `O values ->
      schema_check enum_keys verify_enum_content values
  | _ ->
      false

let verify_enums : Mustache.Json.t -> bool = function
  | `O [("enums", `A enums)] ->
      List.for_all verify_enum enums
  | _ ->
      false

(* obj *)
let verify_obj_member = function
  | "name", `String _ | "description", `String _ ->
      true
  | "event", `Bool _ | "event", `Null ->
      true
  | "session", `Bool _ | "session", `Null ->
      true
  | "fields", `A fields ->
      List.for_all verify_field fields
  | "modules", `Null ->
      true
  | "modules", `O members ->
      schema_check modules_keys verify_modules_member members
  | _ ->
      false

let obj_keys =
  [
    "name"
  ; "name_internal"
  ; "description"
  ; "fields"
  ; "modules"
  ; "event"
  ; "session"
  ]

let verify_obj = function
  | `O members ->
      schema_check obj_keys verify_obj_member members
  | _ ->
      false

let verify_msg_or_error_member = function
  | "name", `String _ | "value", `String _ ->
      true
  | _ ->
      false

let keys_in_error_or_msg = ["name"; "value"]

let verify_msgs_or_errors lst =
  let verify_msg_or_error = function
    | `O members ->
        schema_check keys_in_error_or_msg verify_msg_or_error_member members
    | _ ->
        false
  in
  List.for_all verify_msg_or_error lst

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
            `O
              [
                ("name", `String "MessageDeprecated")
              ; ("value", `String "MESSAGE_DEPRECATED")
              ]
          ; `O
              [
                ("name", `String "MessageRemoved")
              ; ("value", `String "MESSAGE_REMOVED")
              ]
          ]
      )
    ]

let api_messages : Mustache.Json.t =
  `O
    [
      ( "api_messages"
      , `A
          [
            `O
              [
                ("name", `String "HaStatefileLost")
              ; ("value", `String "HA_STATEFILE_LOST")
              ]
          ; `O
              [
                ("name", `String "MetadataLunHealthy")
              ; ("value", `String "METADATA_LUN_HEALTHY")
              ]
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

  let transform (template, json) =
    render_template template json () |> String.trim

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

module TestGeneratedJson = struct
  let verify description verify_func actual =
    Alcotest.(check bool) description true (verify_func actual)

  let test_enums () =
    let enums = Json.all_enums objects in
    verify "enums" verify_enums enums

  let test_obj () =
    Json.xenapi objects
    |> List.iter (fun (name, obj) -> verify name verify_obj obj)

  let test_errors_and_msgs () =
    verify "errors_and_msgs" verify_msgs_or_errors
      (Json.api_errors @ Json.api_messages)

  let tests =
    [
      ("enums", `Quick, test_enums)
    ; ("objs", `Quick, test_obj)
    ; ("errors_and_msgs", `Quick, test_errors_and_msgs)
    ]
end

let tests =
  make_suite "gen_go_binding_"
    [
      ("snake_to_camel", SnakeToCamelTest.tests)
    ; ("templates", TemplatesTest.tests)
    ; ("generated_mustache_jsons", TestGeneratedJson.tests)
    ]

let () = Alcotest.run "Gen go binding" tests
