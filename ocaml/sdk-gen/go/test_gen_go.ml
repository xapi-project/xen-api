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

let result_keys = ["type"; "func_partial_type"]

let verify_result_member = function
  | "type", `String _ | "func_partial_type", `String _ ->
      true
  | _ ->
      false

let verify_option = function
  | `O members ->
      schema_check result_keys verify_result_member members
  | _ ->
      false

let error_keys = ["name"; "doc"]

let verify_error_member = function
  | "name", `String _ | "doc", `String _ ->
      true
  | _ ->
      false

let verify_error = function
  | `O error ->
      schema_check error_keys verify_error_member error
  | _ ->
      false

let param_keys =
  [
    "session"
  ; "type"
  ; "name"
  ; "name_internal"
  ; "doc"
  ; "func_partial_type"
  ; "param_ignore"
  ; "session_class"
  ; "first"
  ]

let verify_param_member = function
  | "session", `Bool _
  | "first", `Bool _
  | "type", `String _
  | "name", `String _
  | "name_internal", `String _
  | "doc", `String _
  | "func_partial_type", `String _ ->
      true
  | "param_ignore", `Bool _ | "param_ignore", `Null ->
      true
  | "session_class", `Bool _ | "session_class", `Null ->
      true
  | _ ->
      false

let verify_param = function
  | `O param ->
      schema_check param_keys verify_param_member param
  | _ ->
      false

let verify_message_member = function
  | "method_name", `String _
  | "class_name", `String _
  | "class_name_exported", `String _
  | "method_name_exported", `String _ ->
      true
  | "description", `String _ | "description", `Null ->
      true
  | "result", `Null ->
      true
  | "result", `O result ->
      schema_check result_keys verify_result_member result
  | "params", `A params ->
      List.for_all verify_param params
  | "errors", `A errors ->
      List.for_all verify_error errors
  | "async", `Bool _
  | "has_error", `Bool _
  | "session_class", `Bool _
  | "session_login", `Bool _
  | "session_logout", `Bool _ ->
      true
  | "errors", `Null ->
      true
  | _ ->
      false

let message_keys =
  [
    "method_name"
  ; "class_name"
  ; "class_name_exported"
  ; "method_name_exported"
  ; "description"
  ; "result"
  ; "params"
  ; "errors"
  ; "session_class"
  ; "session_login"
  ; "session_logout"
  ; "has_error"
  ; "async"
  ]

let verify_message = function
  | `O members ->
      schema_check message_keys verify_message_member members
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
  | "name", `String _ | "description", `String _ | "name_internal", `String _ ->
      true
  | "event", `Bool _ | "event", `Null ->
      true
  | "session", `Bool _ | "session", `Null ->
      true
  | "fields", `A fields ->
      List.for_all verify_field fields
  | "option", `A options ->
      List.for_all verify_option options
  | "messages", `A messages ->
      List.for_all verify_message messages
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
  ; "messages"
  ; "option"
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

let verify_serialize_member = function
  | "func_partial_type", `String _ | "type", `String _ ->
      true
  | _ ->
      false

let serialize_keys = ["func_partial_type"; "type"]

let verify_serialize = function
  | `O items ->
      schema_check serialize_keys verify_serialize_member items
  | _ ->
      false

let type_keys = ["serialize"; "deserialize"]

let verify_simple_type_member = function
  | "serialize", `A serializes ->
      List.for_all verify_serialize serializes
  | "deserialize", `A deserializes ->
      List.for_all verify_serialize deserializes
  | _ ->
      false

let verify_option_serialize_member = function
  | "func_partial_type", `String _ ->
      true
  | _ ->
      false

let option_serialize_keys = ["func_partial_type"]

let verify_option_serialize = function
  | `O items ->
      schema_check option_serialize_keys verify_option_serialize_member items
  | _ ->
      false

let verify_option_member = function
  | "serialize", `A serializes ->
      List.for_all verify_option_serialize serializes
  | "deserialize", `A deserializes ->
      List.for_all verify_option_serialize deserializes
  | _ ->
      false

let time_keys = ["serialize"; "deserialize"; "time_format"]

let verify_time_member = function
  | "serialize", `A serializes ->
      List.for_all verify_serialize serializes
  | "deserialize", `A deserializes ->
      List.for_all verify_serialize deserializes
  | "time_format", `String _ ->
      true
  | _ ->
      false

let verify_set_serialize_member = function
  | "func_partial_type", `String _
  | "type", `String _
  | "item_func_partial_type", `String _ ->
      true
  | _ ->
      false

let serialize_set_keys = ["func_partial_type"; "type"; "item_func_partial_type"]

let verify_set_serialize = function
  | `O items ->
      schema_check serialize_set_keys verify_set_serialize_member items
  | _ ->
      false

let verify_set_member = function
  | "serialize", `A serializes ->
      List.for_all verify_set_serialize serializes
  | "deserialize", `A deserializes ->
      List.for_all verify_set_serialize deserializes
  | _ ->
      false

let record_field_keys =
  ["name"; "name_internal"; "name_exported"; "func_partial_type"; "type_option"]

let verify_record_field_member = function
  | "name", `String _
  | "name_internal", `String _
  | "func_partial_type", `String _
  | "type_option", `Bool _
  | "name_exported", `String _ ->
      true
  | _ ->
      false

let verify_record_field = function
  | `O items ->
      schema_check record_field_keys verify_record_field_member items
  | _ ->
      false

let verify_record_serialize_member = function
  | "func_partial_type", `String _ | "type", `String _ ->
      true
  | "fields", `A fields ->
      List.for_all verify_record_field fields
  | _ ->
      false

let serialize_record_keys = ["func_partial_type"; "type"; "fields"]

let verify_record_serialize = function
  | `O items ->
      schema_check serialize_record_keys verify_record_serialize_member items
  | _ ->
      false

let verify_record_member = function
  | "serialize", `A serializes ->
      List.for_all verify_record_serialize serializes
  | "deserialize", `A deserializes ->
      List.for_all verify_record_serialize deserializes
  | _ ->
      false

let deserialize_keys = ["deserialize"]

let verify_interface_member = function
  | "deserialize", `A deserializes ->
      List.for_all verify_serialize deserializes
  | _ ->
      false

let batch_element_keys =
  ["name"; "name_internal"; "name_exported"; "func_partial_type"]

let verify_batch_element_member = function
  | "func_partial_type", `String _
  | "name", `String _
  | "name_internal", `String _
  | "name_exported", `String _ ->
      true
  | _ ->
      false

let verify_batch_element = function
  | `O items ->
      schema_check batch_element_keys verify_batch_element_member items
  | _ ->
      false

let batch_deserialize_keys = ["func_partial_type"; "type"; "elements"]

let verify_batch_deserialize_member = function
  | "func_partial_type", `String _ | "type", `String _ ->
      true
  | "elements", `A elements ->
      List.for_all verify_batch_element elements
  | _ ->
      false

let verify_batch_deserialize = function
  | `O items ->
      schema_check batch_deserialize_keys verify_batch_deserialize_member items
  | _ ->
      false

let verify_batch_member = function
  | "deserialize", `A deserializes ->
      List.for_all verify_batch_deserialize deserializes
  | _ ->
      false

let map_serialize_keys = ["func_partial_type"; "type"; "key_type"; "value_type"]

let verify_map_serialize_member = function
  | "type", `String _
  | "key_type", `String _
  | "func_partial_type", `String _
  | "value_type", `String _ ->
      true
  | _ ->
      false

let verify_map_serialize = function
  | `O items ->
      schema_check map_serialize_keys verify_map_serialize_member items
  | _ ->
      false

let verify_map_member = function
  | "serialize", `A serializes ->
      List.for_all verify_map_serialize serializes
  | "deserialize", `A deserializes ->
      List.for_all verify_map_serialize deserializes
  | _ ->
      false

let enum_item_keys = ["value"; "name"]

let verify_enum_item_member = function
  | "name", `String _ | "value", `String _ ->
      true
  | _ ->
      false

let verify_enum_item = function
  | `O members ->
      schema_check enum_item_keys verify_enum_item_member members
  | _ ->
      false

let enum_serialize_keys = ["func_partial_type"; "type"; "items"]

let verify_enum_serialize_member = function
  | "func_partial_type", `String _ | "type", `String _ ->
      true
  | "items", `A items ->
      List.for_all verify_enum_item items
  | _ ->
      false

let verify_enum_serialize = function
  | `O items ->
      schema_check enum_serialize_keys verify_enum_serialize_member items
  | _ ->
      false

let verify_enum_member = function
  | "serialize", `A serializes ->
      List.for_all verify_enum_serialize serializes
  | "deserialize", `A deserializes ->
      List.for_all verify_enum_serialize deserializes
  | _ ->
      false

let verify_convert_member = function
  | "simple_type", `O members
  | "int", `O members
  | "float", `O members
  | "ref", `O members ->
      schema_check type_keys verify_simple_type_member members
  | "time", `O members ->
      schema_check time_keys verify_time_member members
  | "set", `O members ->
      schema_check type_keys verify_set_member members
  | "record", `O members ->
      schema_check type_keys verify_record_member members
  | "interface", `O members ->
      schema_check deserialize_keys verify_interface_member members
  | "map", `O members ->
      schema_check type_keys verify_map_member members
  | "enum", `O members ->
      schema_check type_keys verify_enum_member members
  | "batch", `O members ->
      schema_check deserialize_keys verify_batch_member members
  | "option", `O members ->
      schema_check type_keys verify_option_member members
  | _ ->
      false

let convert_keys =
  [
    "simple_type"
  ; "int"
  ; "float"
  ; "time"
  ; "ref"
  ; "set"
  ; "record"
  ; "interface"
  ; "map"
  ; "enum"
  ; "batch"
  ; "option"
  ]

let verify_converts = function
  | `O members ->
      schema_check convert_keys verify_convert_member members
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

let messages : Mustache.Json.t =
  `O
    [
      ( "messages"
      , `A
          [
            `O
              [
                ("session_class", `Bool false)
              ; ("session_login", `Bool false)
              ; ("session_logout", `Bool false)
              ; ("class_name", `String "host")
              ; ("name_internal", `String "host")
              ; ("method_name", `String "get_log")
              ; ("method_name_exported", `String "GetLog")
              ; ("description", `String "GetLog Get the host log file")
              ; ("async", `Bool true)
              ; ( "params"
                , `A
                    [
                      `O
                        [
                          ("type", `String "SessionRef")
                        ; ("name", `String "session_id")
                        ; ("name_internal", `String "sessionID")
                        ; ("func_partial_type", `String "SessionRef")
                        ; ("session", `Bool true)
                        ; ("session_class", `Bool false)
                        ; ("first", `Bool true)
                        ]
                    ; `O
                        [
                          ("type", `String "HostRef")
                        ; ("name", `String "host")
                        ; ("name_internal", `String "host")
                        ; ("func_partial_type", `String "HostRef")
                        ; ("session_class", `Bool false)
                        ; ("session", `Bool false)
                        ]
                    ]
                )
              ; ( "result"
                , `O
                    [
                      ("type", `String "string")
                    ; ("func_partial_type", `String "String")
                    ]
                )
              ; ("has_error", `Bool false)
              ; ("errors", `A [])
              ]
          ; `O
              [
                ("session_class", `Bool true)
              ; ("session_login", `Bool true)
              ; ("session_logout", `Bool false)
              ; ("class_name", `String "session")
              ; ("name_internal", `String "")
              ; ("method_name", `String "login_with_password")
              ; ("method_name_exported", `String "LoginWithPassword")
              ; ( "description"
                , `String
                    "Attempt to authenticate the user); returning a session \
                     reference if successful"
                )
              ; ("async", `Bool false)
              ; ( "params"
                , `A
                    [
                      `O
                        [
                          ("type", `String "string")
                        ; ("name", `String "uname")
                        ; ("name_internal", `String "uname")
                        ; ("func_partial_type", `String "String")
                        ; ("first", `Bool true)
                        ; ("session_class", `Bool true)
                        ; ("session", `Bool true)
                        ]
                    ; `O
                        [
                          ("type", `String "string")
                        ; ("name", `String "pwd")
                        ; ("name_internal", `String "pwd")
                        ; ("func_partial_type", `String "String")
                        ; ("session_class", `Bool true)
                        ; ("session", `Bool true)
                        ]
                    ]
                )
              ; ( "result"
                , `O
                    [
                      ("type", `String "SessionRef")
                    ; ("func_partial_type", `String "SessionRef")
                    ]
                )
              ; ("has_error", `Bool true)
              ; ( "errors"
                , `A
                    [
                      `O
                        [
                          ("name", `String "SESSION_AUTHENTICATION_FAILED")
                        ; ( "doc"
                          , `String
                              "The credentials given by the user are incorrect"
                          )
                        ]
                    ]
                )
              ]
          ; `O
              [
                ("session_class", `Bool true)
              ; ("session_logout", `Bool true)
              ; ("session_login", `Bool false)
              ; ("class_name", `String "session")
              ; ("class_name_exported", `String "Session")
              ; ("method_name", `String "logout")
              ; ("method_name_exported", `String "Logout")
              ; ("description", `String "Logout Log out of a session")
              ; ("async", `Bool false)
              ; ( "params"
                , `A
                    [
                      `O
                        [
                          ("type", `String "SessionRef")
                        ; ("name", `String "session_id")
                        ; ("name_internal", `String "sessionID")
                        ; ("func_partial_type", `String "SessionRef")
                        ; ("param_ignore", `Bool true)
                        ; ("session_class", `Bool true)
                        ; ("session", `Bool true)
                        ]
                    ; `O
                        [
                          ("type", `String "string")
                        ; ("name", `String "test_param")
                        ; ("name_internal", `String "testParam")
                        ; ("func_partial_type", `String "String")
                        ; ("first", `Bool true)
                        ; ("session_class", `Bool true)
                        ; ("session", `Bool true)
                        ]
                    ]
                )
              ; ("result", `Null)
              ; ("has_error", `Bool false)
              ; ("errors", `A [])
              ]
          ]
      )
    ]

let simple_type_convert : Mustache.Json.t =
  let array =
    [
      `O [("func_partial_type", `String "String"); ("type", `String "string")]
    ; `O [("func_partial_type", `String "Bool"); ("type", `String "bool")]
    ]
  in
  `O [("simple_type", `O [("serialize", `A array); ("deserialize", `A array)])]

let int_convert : Mustache.Json.t =
  let array =
    [`O [("func_partial_type", `String "Int"); ("type", `String "int")]]
  in
  `O [("int", `O [("serialize", `A array); ("deserialize", `A array)])]

let float_convert : Mustache.Json.t =
  let array =
    [`O [("func_partial_type", `String "Float"); ("type", `String "float64")]]
  in
  `O [("float", `O [("serialize", `A array); ("deserialize", `A array)])]

let time_convert : Mustache.Json.t =
  let array =
    [`O [("func_partial_type", `String "Time"); ("type", `String "time.Time")]]
  in
  `O [("time", `O [("serialize", `A array); ("deserialize", `A array)])]

let ref_string_convert : Mustache.Json.t =
  let array =
    [`O [("func_partial_type", `String "VMRef"); ("type", `String "VMRef")]]
  in
  `O [("ref", `O [("serialize", `A array); ("deserialize", `A array)])]

let set_convert : Mustache.Json.t =
  let serialize =
    [
      `O
        [
          ("func_partial_type", `String "SRRefSet")
        ; ("type", `String "SRRef")
        ; ("item_func_partial_type", `String "SRRef")
        ]
    ]
  in
  let deserialize =
    [
      `O
        [
          ("func_partial_type", `String "StringSet")
        ; ("type", `String "string")
        ; ("item_func_partial_type", `String "String")
        ]
    ]
  in
  `O
    [("set", `O [("serialize", `A serialize); ("deserialize", `A deserialize)])]

let record_convert : Mustache.Json.t =
  let array =
    [
      `O
        [
          ("func_partial_type", `String "VBDRecord")
        ; ("type", `String "VBDRecord")
        ; ( "fields"
          , `A
              [
                `O
                  [
                    ("name", `String "uuid")
                  ; ("name_internal", `String "uuid")
                  ; ("name_exported", `String "UUID")
                  ; ("func_partial_type", `String "String")
                  ; ("type_option", `Bool false)
                  ]
              ; `O
                  [
                    ("name", `String "allowed_operations")
                  ; ("name_internal", `String "allowedOperations")
                  ; ("name_exported", `String "AllowedOperations")
                  ; ("func_partial_type", `String "EnumVbdOperationsSet")
                  ; ("type_option", `Bool false)
                  ]
              ]
          )
        ]
    ]
  in
  `O [("record", `O [("serialize", `A array); ("deserialize", `A array)])]

let interface_convert : Mustache.Json.t =
  let array =
    [
      `O
        [
          ("func_partial_type", `String "RecordInterface")
        ; ("type", `String "RecordInterface")
        ]
    ]
  in
  `O [("interface", `O [("deserialize", `A array)])]

let map_convert : Mustache.Json.t =
  let deserialize =
    [
      `O
        [
          ("func_partial_type", `String "PBDRefToPBDRecordMap")
        ; ("type", `String "map[PBDRef]PBDRecord")
        ; ("key_type", `String "PBDRef")
        ; ("value_type", `String "PBDRecord")
        ]
    ]
  in
  let serialize =
    [
      `O
        [
          ("func_partial_type", `String "VIFRefToStringMap")
        ; ("type", `String "map[VIFRef]string")
        ; ("key_type", `String "VIFRef")
        ; ("value_type", `String "String")
        ]
    ]
  in
  `O
    [("map", `O [("serialize", `A serialize); ("deserialize", `A deserialize)])]

let enum_convert : Mustache.Json.t =
  let array =
    [
      `O
        [
          ("func_partial_type", `String "EnumTaskStatusType")
        ; ("type", `String "TaskStatusType")
        ; ( "items"
          , `A
              [
                `O
                  [
                    ("name", `String "TaskStatusTypePending")
                  ; ("value", `String "pending")
                  ]
              ; `O
                  [
                    ("name", `String "TaskStatusTypeSuccess")
                  ; ("value", `String "success")
                  ]
              ]
          )
        ]
    ]
  in
  `O [("enum", `O [("serialize", `A array); ("deserialize", `A array)])]

let batch_convert : Mustache.Json.t =
  let array =
    [
      `O
        [
          ("func_partial_type", `String "EventBatch")
        ; ("type", `String "EventBatch")
        ; ( "elements"
          , `A
              [
                `O
                  [
                    ("name", `String "token")
                  ; ("name_internal", `String "token")
                  ; ("name_exported", `String "Token")
                  ; ("func_partial_type", `String "String")
                  ]
              ; `O
                  [
                    ("name", `String "valid_ref_counts")
                  ; ("name_internal", `String "validRefCounts")
                  ; ("name_exported", `String "ValidRefCounts")
                  ; ("func_partial_type", `String "StringToIntMap")
                  ]
              ]
          )
        ]
    ]
  in
  `O [("batch", `O [("deserialize", `A array)])]

let option_convert : Mustache.Json.t =
  let array = [`O [("func_partial_type", `String "SrStatRecord")]] in
  `O [("option", `O [("serialize", `A array); ("deserialize", `A array)])]

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

  let methods_rendered = string_of_file "methods.go"

  let api_errors_rendered = string_of_file "api_errors.go"

  let api_messages_rendered = string_of_file "api_messages.go"

  let simple_type_rendered = string_of_file "simple_type.go"

  let int_rendered = string_of_file "int.go"

  let float_rendered = string_of_file "float.go"

  let time_rendered = string_of_file "time.go"

  let string_ref_rendered = string_of_file "ref.go"

  let set_rendered = string_of_file "set.go"

  let record_convert_rendered = string_of_file "record_convert.go"

  let interface_rendered = string_of_file "interface.go"

  let map_rendered = string_of_file "map.go"

  let enum_rendered = string_of_file "enum_convert.go"

  let batch_rendered = string_of_file "batch.go"

  let option_rendered = string_of_file "option.go"

  let tests =
    `QuickAndAutoDocumented
      [
        (("FileHeader.mustache", header), file_header_rendered)
      ; (("Record.mustache", record), record_rendered)
      ; (("Enum.mustache", enums), enums_rendered)
      ; (("Methods.mustache", messages), methods_rendered)
      ; (("APIErrors.mustache", api_errors), api_errors_rendered)
      ; (("APIMessages.mustache", api_messages), api_messages_rendered)
      ; ( ("ConvertSimpleType.mustache", simple_type_convert)
        , simple_type_rendered
        )
      ; (("ConvertInt.mustache", int_convert), int_rendered)
      ; (("ConvertFloat.mustache", float_convert), float_rendered)
      ; (("ConvertTime.mustache", time_convert), time_rendered)
      ; (("ConvertRef.mustache", ref_string_convert), string_ref_rendered)
      ; (("ConvertSet.mustache", set_convert), set_rendered)
      ; (("ConvertRecord.mustache", record_convert), record_convert_rendered)
      ; (("ConvertInterface.mustache", interface_convert), interface_rendered)
      ; (("ConvertMap.mustache", map_convert), map_rendered)
      ; (("ConvertEnum.mustache", enum_convert), enum_rendered)
      ; (("ConvertBatch.mustache", batch_convert), batch_rendered)
      ; (("ConvertOption.mustache", option_convert), option_rendered)
      ]
end)

module TestGeneratedJson = struct
  let verify description verify_func actual =
    Alcotest.(check bool) description true (verify_func actual)

  let test_enums () =
    let enums = Json.all_enums objects in
    verify "enums" verify_enums enums

  let objs, converts = objs_and_convert_functions objects

  let test_obj () =
    List.iter (fun (name, obj) -> verify name verify_obj obj) objs

  let test_converts () = verify "schema" verify_converts converts

  let test_errors_and_msgs () =
    verify "errors_and_msgs" verify_msgs_or_errors
      (Json.api_errors @ Json.api_messages)

  let tests =
    [
      ("enums", `Quick, test_enums)
    ; ("objs", `Quick, test_obj)
    ; ("converts", `Quick, test_converts)
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
