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
    type input_t = bool * string

    type output_t = string

    let string_of_input_t = Fmt.(str "%a" Dump.(pair bool string))

    let string_of_output_t = Test_printers.string
  end

  let transform (internal, str) = snake_to_camel ~internal str

  let tests =
    `QuickAndAutoDocumented
      [
        ((false, "ni_hao-Nanjin"), "NiHaoNanjin")
      ; ((false, "ni_hao"), "NiHao")
      ; ((false, "nanjing"), "Nanjing")
      ; ((false, "uuid"), "UUID")
      ; ((false, "get_by_uuid"), "GetByUUID")
      ; ((true, "uuid"), "uuid")
      ; ((true, "PIF_Metrics"), "pifMetrics")
      ; ((true, "VM_guest_metrics"), "vmGuestMetrics")
      ; ((true, "Network"), "network")
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
  | "name", `String _
  | "description", `String _
  | "type", `String _
  | "json_name", `String _ ->
      true
  | _ ->
      false

let field_keys = ["name"; "description"; "type"; "json_name"]

let verify_field = function
  | `O members ->
      schema_check field_keys verify_field_member members
  | _ ->
      false

let result_keys = ["type"; "func_name_suffix"]

let verify_result_member = function
  | "type", `String _ | "func_name_suffix", `String _ ->
      true
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
    "is_session_id"
  ; "type"
  ; "name"
  ; "name_internal"
  ; "doc"
  ; "func_name_suffix"
  ; "first"
  ]

let verify_param_member = function
  | "is_session_id", `Bool _
  | "first", `Bool _
  | "type", `String _
  | "name", `String _
  | "name_internal", `String _
  | "doc", `String _
  | "func_name_suffix", `String _ ->
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
  | "method_name_exported", `String _
  | "version", `String _ ->
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
  | "async", `Bool _ | "has_error", `Bool _ | "errors", `Null ->
      true
  | _ ->
      false

let verify_sesseion_message_member = function
  | "method_name", `String _
  | "class_name", `String _
  | "class_name_exported", `String _
  | "method_name_exported", `String _
  | "version", `String _ ->
      true
  | "description", `String _ | "description", `Null ->
      true
  | "result", `Null ->
      true
  | "result", `O result ->
      schema_check result_keys verify_result_member result
  | "params", `A params ->
      List.for_all verify_param params
  | "func_params", `A params ->
      List.for_all verify_param params
  | "errors", `A errors ->
      List.for_all verify_error errors
  | "async", `Bool _
  | "has_error", `Bool _
  | "session_login", `Bool _
  | "session_logout", `Bool _
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
  ; "has_error"
  ; "async"
  ; "version"
  ]

let session_message_keys =
  ["session_login"; "session_logout"; "func_params"] @ message_keys

let verify_message = function
  | `O members ->
      let class_name =
        List.assoc_opt "class_name" members |> Option.value ~default:`Null
      in
      if class_name <> `String "session" then
        schema_check message_keys verify_message_member members
      else
        schema_check session_message_keys verify_sesseion_message_member members
  | _ ->
      false

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

let modules_keys = ["import"; "items"]

let verify_modules_member = function
  | "import", `Bool _ ->
      true
  | "items", `A items ->
      List.for_all verify_modules_item items
  | _ ->
      false

let enum_values_keys = ["value"; "doc"; "name"; "type"]

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

let option_keys = ["type"; "type_name_suffix"]

let verify_option_member = function
  | "type", `String _ | "type_name_suffix", `String _ ->
      true
  | _ ->
      false

let verify_option = function
  | `O members ->
      schema_check option_keys verify_option_member members
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
  | "messages", `A messages ->
      List.for_all verify_message messages
  | "option", `A options ->
      List.for_all verify_option options
  | "modules", `Null ->
      true
  | "modules", `O members ->
      schema_check modules_keys verify_modules_member members
  | _ ->
      false

let obj_keys =
  [
    "name"
  ; "description"
  ; "name_internal"
  ; "fields"
  ; "messages"
  ; "modules"
  ; "event"
  ; "session"
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

let verify_simple_convert_member = function
  | "func_name_suffix", `String _ | "type", `String _ ->
      true
  | _ ->
      false

let verify_release_member = function
  | "branding", `String _ | "code_name", `String _ ->
      true
  | "first", `Bool _ ->
      true
  | "version_index", `Float _
  | "version_major", `Float _
  | "version_minor", `Float _ ->
      true
  | _ ->
      false

let verify_simple_convert_keys = ["func_name_suffix"; "type"]

let verify_simple_convert = function
  | `O items ->
      schema_check verify_simple_convert_keys verify_simple_convert_member items
  | _ ->
      false

let verify_option_convert_member = function
  | "func_name_suffix", `String _ ->
      true
  | _ ->
      false

let option_convert_keys = ["func_name_suffix"]

let verify_option_convert = function
  | `O items ->
      schema_check option_convert_keys verify_option_convert_member items
  | _ ->
      false

let verify_set_convert_member = function
  | "func_name_suffix", `String _
  | "type", `String _
  | "item_func_suffix", `String _ ->
      true
  | _ ->
      false

let convert_set_keys = ["func_name_suffix"; "type"; "item_func_suffix"]

let verify_set_convert = function
  | `O items ->
      schema_check convert_set_keys verify_set_convert_member items
  | _ ->
      false

let record_field_keys =
  ["name"; "name_internal"; "name_exported"; "func_name_suffix"; "type_option"]

let verify_record_field_member = function
  | "name", `String _
  | "name_internal", `String _
  | "func_name_suffix", `String _
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

let verify_record_convert_member = function
  | "func_name_suffix", `String _ | "type", `String _ ->
      true
  | "fields", `A fields ->
      List.for_all verify_record_field fields
  | _ ->
      false

let convert_record_keys = ["func_name_suffix"; "type"; "fields"]

let verify_record_convert = function
  | `O items ->
      schema_check convert_record_keys verify_record_convert_member items
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

let enum_convert_keys = ["func_name_suffix"; "type"; "items"]

let verify_enum_convert_member = function
  | "func_name_suffix", `String _ | "type", `String _ ->
      true
  | "items", `A items ->
      List.for_all verify_enum_item items
  | _ ->
      false

let verify_enum_convert = function
  | `O items ->
      schema_check enum_convert_keys verify_enum_convert_member items
  | _ ->
      false

let map_convert_keys = ["func_name_suffix"; "type"; "key_type"; "value_type"]

let verify_map_convert_member = function
  | "type", `String _
  | "key_type", `String _
  | "func_name_suffix", `String _
  | "value_type", `String _ ->
      true
  | _ ->
      false

let verify_map_convert = function
  | `O items ->
      schema_check map_convert_keys verify_map_convert_member items
  | _ ->
      false

let release_keys =
  [
    "branding"
  ; "code_name"
  ; "version_major"
  ; "version_minor"
  ; "first"
  ; "version_index"
  ]

let verify_release = function
  | `O members ->
      schema_check release_keys verify_release_member members
  | _ ->
      false

let version_keys =
  ["API_VERSION_MAJOR"; "API_VERSION_MINOR"; "latest_version_index"; "releases"]

let verify_version_member = function
  | "latest_version_index", `Float _
  | "API_VERSION_MAJOR", `Float _
  | "API_VERSION_MINOR", `Float _ ->
      true
  | "releases", `A releases ->
      List.for_all verify_release releases
  | _ ->
      false

let verify_version = function
  | `O members ->
      schema_check version_keys verify_version_member members
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
              ; ("json_name", `String "uuid")
              ; ("description", `String "Unique identifier/object reference")
              ; ("type", `String "string")
              ]
          ; `O
              [
                ("name", `String "ThisHost")
              ; ("json_name", `String "thishost")
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

let simple_type_convert : Mustache.Json.t =
  let array =
    [
      `O [("func_name_suffix", `String "String"); ("type", `String "string")]
    ; `O [("func_name_suffix", `String "Bool"); ("type", `String "bool")]
    ]
  in
  `O [("serialize", `A array); ("deserialize", `A array)]

let int_convert : Mustache.Json.t =
  let array =
    [`O [("func_name_suffix", `String "Int"); ("type", `String "int")]]
  in
  `O [("serialize", `A array); ("deserialize", `A array)]

let float_convert : Mustache.Json.t =
  let array =
    [`O [("func_name_suffix", `String "Float"); ("type", `String "float64")]]
  in
  `O [("serialize", `A array); ("deserialize", `A array)]

let time_convert : Mustache.Json.t =
  let array =
    [`O [("func_name_suffix", `String "Time"); ("type", `String "time.Time")]]
  in
  `O [("serialize", `A array); ("deserialize", `A array)]

let ref_string_convert : Mustache.Json.t =
  let array =
    [`O [("func_name_suffix", `String "VMRef"); ("type", `String "VMRef")]]
  in
  `O [("serialize", `A array); ("deserialize", `A array)]

let set_convert : Mustache.Json.t =
  let serialize =
    [
      `O
        [
          ("func_name_suffix", `String "SRRefSet")
        ; ("type", `String "SRRef")
        ; ("item_func_suffix", `String "SRRef")
        ]
    ]
  in
  let deserialize =
    [
      `O
        [
          ("func_name_suffix", `String "StringSet")
        ; ("type", `String "string")
        ; ("item_func_suffix", `String "String")
        ]
    ]
  in
  `O [("serialize", `A serialize); ("deserialize", `A deserialize)]

let record_convert : Mustache.Json.t =
  let array =
    [
      `O
        [
          ("func_name_suffix", `String "VBDRecord")
        ; ("type", `String "VBDRecord")
        ; ( "fields"
          , `A
              [
                `O
                  [
                    ("name", `String "uuid")
                  ; ("name_internal", `String "uuid")
                  ; ("name_exported", `String "UUID")
                  ; ("func_name_suffix", `String "String")
                  ; ("type_option", `Bool false)
                  ]
              ; `O
                  [
                    ("name", `String "allowed_operations")
                  ; ("name_internal", `String "allowedOperations")
                  ; ("name_exported", `String "AllowedOperations")
                  ; ("func_name_suffix", `String "EnumVbdOperationsSet")
                  ; ("type_option", `Bool false)
                  ]
              ]
          )
        ]
    ]
  in
  `O [("serialize", `A array); ("deserialize", `A array)]

let map_convert : Mustache.Json.t =
  let deserialize =
    [
      `O
        [
          ("func_name_suffix", `String "PBDRefToPBDRecordMap")
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
          ("func_name_suffix", `String "VIFRefToStringMap")
        ; ("type", `String "map[VIFRef]string")
        ; ("key_type", `String "VIFRef")
        ; ("value_type", `String "String")
        ]
    ]
  in
  `O [("serialize", `A serialize); ("deserialize", `A deserialize)]

let enum_convert : Mustache.Json.t =
  let array =
    [
      `O
        [
          ("func_name_suffix", `String "EnumTaskStatusType")
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
  `O [("serialize", `A array); ("deserialize", `A array)]

let option_convert : Mustache.Json.t =
  let array = [`O [("func_name_suffix", `String "SrStatRecord")]] in
  `O [("serialize", `A array); ("deserialize", `A array)]

let session_messages : Mustache.Json.t =
  `O
    [
      ( "messages"
      , `A
          [
            `O
              [
                ("session_login", `Bool true)
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
              ; ("version", `String "miami")
              ; ( "func_params"
                , `A
                    [
                      `O
                        [
                          ("type", `String "string")
                        ; ("name", `String "uname")
                        ; ("name_internal", `String "uname")
                        ; ("func_name_suffix", `String "String")
                        ; ("first", `Bool true)
                        ; ("is_session_id", `Bool false)
                        ]
                    ; `O
                        [
                          ("type", `String "string")
                        ; ("name", `String "pwd")
                        ; ("name_internal", `String "pwd")
                        ; ("func_name_suffix", `String "String")
                        ; ("is_session_id", `Bool false)
                        ]
                    ]
                )
              ; ( "params"
                , `A
                    [
                      `O
                        [
                          ("type", `String "string")
                        ; ("name", `String "uname")
                        ; ("name_internal", `String "uname")
                        ; ("func_name_suffix", `String "String")
                        ; ("first", `Bool true)
                        ; ("is_session_id", `Bool false)
                        ]
                    ; `O
                        [
                          ("type", `String "string")
                        ; ("name", `String "pwd")
                        ; ("name_internal", `String "pwd")
                        ; ("func_name_suffix", `String "String")
                        ; ("is_session_id", `Bool false)
                        ]
                    ]
                )
              ; ( "result"
                , `O
                    [
                      ("type", `String "SessionRef")
                    ; ("func_name_suffix", `String "SessionRef")
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
                ("session_logout", `Bool true)
              ; ("session_login", `Bool false)
              ; ("class_name", `String "session")
              ; ("class_name_exported", `String "Session")
              ; ("method_name", `String "logout")
              ; ("method_name_exported", `String "Logout")
              ; ("description", `String "Logout Log out of a session")
              ; ("async", `Bool false)
              ; ("func_params", `A [])
              ; ("version", `String "miami")
              ; ( "params"
                , `A
                    [
                      `O
                        [
                          ("type", `String "SessionRef")
                        ; ("name", `String "session_id")
                        ; ("name_internal", `String "sessionID")
                        ; ("func_name_suffix", `String "SessionRef")
                        ; ("is_session_id", `Bool true)
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

let messages : Mustache.Json.t =
  `O
    [
      ( "messages"
      , `A
          [
            `O
              [
                ("class_name", `String "host")
              ; ("name_internal", `String "host")
              ; ("method_name", `String "get_log")
              ; ("method_name_exported", `String "GetLog")
              ; ("description", `String "GetLog Get the host log file")
              ; ("async", `Bool true)
              ; ("version", `String "miami")
              ; ( "params"
                , `A
                    [
                      `O
                        [
                          ("type", `String "SessionRef")
                        ; ("name", `String "session_id")
                        ; ("name_internal", `String "sessionID")
                        ; ("func_name_suffix", `String "SessionRef")
                        ; ("session", `Bool true)
                        ; ("session_class", `Bool false)
                        ; ("first", `Bool true)
                        ]
                    ; `O
                        [
                          ("type", `String "HostRef")
                        ; ("name", `String "host")
                        ; ("name_internal", `String "host")
                        ; ("func_name_suffix", `String "HostRef")
                        ; ("first", `Bool false)
                        ]
                    ]
                )
              ; ( "result"
                , `O
                    [
                      ("type", `String "string")
                    ; ("func_name_suffix", `String "String")
                    ]
                )
              ; ("has_error", `Bool false)
              ; ("errors", `A [])
              ]
          ]
      )
    ]

let api_versions : Mustache.Json.t =
  `O
    [
      ("latest_version_index", `Float 2.)
    ; ( "releases"
      , `A
          [
            `O
              [
                ("branding", `String "XenServer 4.0")
              ; ("code_name", `String "rio")
              ; ("version_major", `Float 1.)
              ; ("version_minor", `Float 1.)
              ; ("first", `Bool true)
              ]
          ; `O
              [
                ("branding", `String "XenServer 4.1")
              ; ("code_name", `String "miami")
              ; ("version_major", `Float 1.)
              ; ("version_minor", `Float 2.)
              ; ("first", `Bool false)
              ]
          ]
      )
    ]

let option =
  `O
    [
      ( "option"
      , `A
          [
            `O
              [
                ("type", `String "string")
              ; ("type_name_suffix", `String "String")
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

  let methods_rendered = string_of_file "methods.go"

  let session_method_rendered = string_of_file "session_method.go"

  let api_errors_rendered = string_of_file "api_errors.go"

  let api_messages_rendered = string_of_file "api_messages.go"

  let float_convert_rendered = string_of_file "float_convert.go"

  let time_convert_rendered = string_of_file "time_convert.go"

  let api_versions_rendered = string_of_file "api_versions.go"

  let option_rendered = "type OptionString *string"

  let simple_type_rendered = string_of_file "simple_type_convert.go"

  let int_convert_rendered = string_of_file "int_convert.go"

  let string_ref_rendered = string_of_file "ref_convert.go"

  let set_convert_rendered = string_of_file "set_convert.go"

  let record_convert_rendered = string_of_file "record_convert.go"

  let interface_convert_rendered = string_of_file "interface_convert.go"

  let map_convert_rendered = string_of_file "map_convert.go"

  let enum_convert_rendered = string_of_file "enum_convert.go"

  let batch_convert_rendered = string_of_file "batch_convert.go"

  let option_convert_rendered = string_of_file "option_convert.go"

  let tests =
    `QuickAndAutoDocumented
      [
        (("FileHeader.mustache", header), file_header_rendered)
      ; (("Record.mustache", record), record_rendered)
      ; (("Enum.mustache", enums), enums_rendered)
      ; (("Methods.mustache", messages), methods_rendered)
      ; (("SessionMethod.mustache", session_messages), session_method_rendered)
      ; (("APIErrors.mustache", api_errors), api_errors_rendered)
      ; (("APIMessages.mustache", api_messages), api_messages_rendered)
      ; ( ("ConvertSimpleType.mustache", simple_type_convert)
        , simple_type_rendered
        )
      ; (("ConvertInt.mustache", int_convert), int_convert_rendered)
      ; (("ConvertFloat.mustache", float_convert), float_convert_rendered)
      ; (("ConvertTime.mustache", time_convert), time_convert_rendered)
      ; (("ConvertRef.mustache", ref_string_convert), string_ref_rendered)
      ; (("ConvertSet.mustache", set_convert), set_convert_rendered)
      ; (("ConvertRecord.mustache", record_convert), record_convert_rendered)
      ; ( ("ConvertInterface.mustache", Convert.interface)
        , interface_convert_rendered
        )
      ; (("ConvertMap.mustache", map_convert), map_convert_rendered)
      ; (("ConvertEnum.mustache", enum_convert), enum_convert_rendered)
      ; (("ConvertBatch.mustache", Convert.event_batch), batch_convert_rendered)
      ; (("ConvertOption.mustache", option_convert), option_convert_rendered)
      ; (("APIVersions.mustache", api_versions), api_versions_rendered)
      ; (("Option.mustache", option), option_rendered)
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

  let test_versions () = verify "versions" verify_version json_releases

  let tests =
    [
      ("enums", `Quick, test_enums)
    ; ("objs", `Quick, test_obj)
    ; ("errors_and_msgs", `Quick, test_errors_and_msgs)
    ; ("versions", `Quick, test_versions)
    ]
end

module SuffixOfTypeTest = Generic.MakeStateless (struct
  open Datamodel_types

  module Io = struct
    type input_t = ty

    type output_t = string

    let string_of_input_t = Json.suffix_of_type

    let string_of_output_t = Test_printers.string
  end

  let transform = Json.suffix_of_type

  let tests =
    `QuickAndAutoDocumented
      [
        (SecretString, "String")
      ; (String, "String")
      ; (Int, "Int")
      ; (Float, "Float")
      ; (Bool, "Bool")
      ; (Enum ("update_sync", [("a", "b"); ("c", "d")]), "EnumUpdateSync")
      ; (Set String, "StringSet")
      ; (Map (Int, String), "IntToStringMap")
      ; (Ref "pool", "PoolRef")
      ; (Record "pool", "PoolRecord")
      ; (Option String, "String")
      ]
end)

module GroupParamsTest = Generic.MakeStateless (struct
  open Datamodel_types
  open Datamodel_common

  let string_of_message msg = msg.msg_obj_name ^ "." ^ msg.msg_name

  let string_of_param param = param.param_name ^ ":" ^ param.param_doc

  let string_of_group (latest, params, rel_version) =
    Printf.sprintf "(latest = %b, release_version = %s, params = [%s])" latest
      rel_version
      (Test_printers.list string_of_param params)

  let string_of_groups groups =
    Printf.sprintf "param_groups : [%s]}"
      (Test_printers.list string_of_group groups)

  module Io = struct
    type input_t = message

    type output_t = ((bool * param list * string) list, string) result

    let string_of_input_t = string_of_message

    let string_of_output_t = function
      | Ok groups ->
          Fmt.(str "%a" Dump.string) (string_of_groups groups)
      | Error e ->
          Fmt.(str "%a" Dump.string) e
  end

  let transform message =
    try Ok (Json.group_params message) with Failure e -> Error e

  let network =
    {
      param_type= Ref _network
    ; param_name= "network"
    ; param_doc= "Network to add the bonded PIF to"
    ; param_release= miami_release
    ; param_default= None
    }

  let members =
    {
      param_type= Set (Ref _pif)
    ; param_name= "members"
    ; param_doc= "PIFs to add to this bond"
    ; param_release= miami_release
    ; param_default= None
    }

  let mac =
    {
      param_type= String
    ; param_name= "MAC"
    ; param_doc= "The MAC address to use on the bond itself."
    ; param_release= miami_release
    ; param_default= None
    }

  let mode =
    {
      param_type= Enum ("bond_mode", [("balance-slb", "Source-level balancing")])
    ; param_name= "mode"
    ; param_doc= "Bonding mode to use for the new bond"
    ; param_release= boston_release
    ; param_default= Some (VEnum "balance-slb")
    }

  let properties =
    {
      param_type= Map (String, String)
    ; param_name= "properties"
    ; param_doc= "Additional configuration parameters specific to the bond mode"
    ; param_release= tampa_release
    ; param_default= Some (VMap [])
    }

  let num_release = numbered_release "1.250.0"

  let numbered_release_param =
    {
      param_type= String
    ; param_name= "param"
    ; param_doc= "A parm for testing"
    ; param_release= num_release
    ; param_default= None
    }

  let group1 = [network; members; mac]

  let group2 = group1 @ [mode]

  let group3 = group2 @ [properties]

  let group4 = group3 @ [numbered_release_param]

  let msg_with_session =
    {
      msg_name= "create"
    ; msg_params= group4
    ; msg_result= Some (Ref "Bond", "The reference of the created Bond object")
    ; msg_errors= []
    ; msg_doc= "Create an interface bond"
    ; msg_async= true
    ; msg_session= true
    ; msg_secret= false
    ; msg_pool_internal= false
    ; msg_db_only= false
    ; msg_release= miami_release
    ; msg_lifecycle= Lifecycle.from []
    ; msg_has_effect= true
    ; msg_force_custom= None
    ; msg_no_current_operations= false
    ; msg_tag= Custom
    ; msg_obj_name= "Bond"
    ; msg_custom_marshaller= false
    ; msg_hide_from_docs= false
    ; msg_allowed_roles= Some ["pool-admin"; "pool-operator"]
    ; msg_map_keys_roles= []
    ; msg_doc_tags= []
    ; msg_forward_to= None
    }

  let num_version = published_release_for_param num_release.internal

  let msg_with_session_expected =
    [
      (true, session_id :: group4, num_version)
    ; (false, session_id :: group4, num_version)
    ; (false, session_id :: group3, rel_tampa)
    ; (false, session_id :: group2, rel_boston)
    ; (false, session_id :: group1, rel_miami)
    ]

  let msg_with_session_with_only_param =
    {msg_with_session with msg_params= [network]}

  let msg_with_session_with_only_param_expected =
    [
      (true, [session_id; network], rel_miami)
    ; (false, [session_id; network], rel_miami)
    ]

  let msg_without_session = {msg_with_session with msg_session= false}

  let msg_without_session_expected =
    [
      (true, group4, num_version)
    ; (false, group4, num_version)
    ; (false, group3, rel_tampa)
    ; (false, group2, rel_boston)
    ; (false, group1, rel_miami)
    ]

  let msg_without_session_with_only_param =
    {msg_with_session with msg_session= false; msg_params= [network]}

  let msg_without_session_with_only_param_expected =
    [(true, [network], rel_miami); (false, [network], rel_miami)]

  (*Message has session param, but has no other params.*)
  let msg_with_session_without_param = {msg_with_session with msg_params= []}

  let msg_with_session_without_param_expected =
    let version =
      published_release_for_param
        msg_with_session_without_param.msg_release.internal
    in
    [(true, [session_id], version); (false, [session_id], version)]

  let msg_without_session_without_param =
    {msg_with_session with msg_params= []; msg_session= false}

  (*Message which in session object has not session param and has no other params.*)
  let msg_with_session_without_param_in_session_object =
    {msg_with_session with msg_params= []; msg_obj_name= "Session"}

  let msg_with_session_without_param_in_session_object_expected =
    let version =
      published_release_for_param
        msg_with_session_without_param_in_session_object.msg_release.internal
    in
    [(true, [session_id], version); (false, [session_id], version)]

  (*Message which in session object has not session param and has no other params.*)
  let msg_without_session_without_param_in_session_object =
    {
      msg_with_session with
      msg_params= []
    ; msg_obj_name= "Session"
    ; msg_session= false
    }

  let tests =
    `QuickAndAutoDocumented
      [
        (msg_with_session, Ok msg_with_session_expected)
      ; ( msg_with_session_with_only_param
        , Ok msg_with_session_with_only_param_expected
        )
      ; (msg_without_session, Ok msg_without_session_expected)
      ; ( msg_without_session_with_only_param
        , Ok msg_without_session_with_only_param_expected
        )
      ; ( msg_with_session_without_param
        , Ok msg_with_session_without_param_expected
        )
      ; ( msg_without_session_without_param
        , Error "Empty params group should not exist."
        )
      ; ( msg_with_session_without_param_in_session_object
        , Ok msg_with_session_without_param_in_session_object_expected
        )
      ; ( msg_without_session_without_param_in_session_object
        , Error "Empty params group should not exist."
        )
      ]
end)

module TestConvertGeneratedJson = struct
  open Convert

  let verify description verify_func actual =
    Alcotest.(check bool) description true (verify_func actual)

  let param_types = TypesOfMessages.of_params objects

  let result_types = TypesOfMessages.of_results objects

  let verify_func = function
    | Simple _ | Int _ | Float _ | Time _ | Ref _ ->
        verify_simple_convert
    | Option _ ->
        verify_option_convert
    | Set _ ->
        verify_set_convert
    | Record _ ->
        verify_record_convert
    | Enum _ ->
        verify_enum_convert
    | Map _ ->
        verify_map_convert

  let convert_param_name = function
    | Simple _ ->
        "simple"
    | Int _ ->
        "int"
    | Float _ ->
        "float"
    | Time _ ->
        "time"
    | Ref _ ->
        "ref"
    | Option _ ->
        "option"
    | Set _ ->
        "set"
    | Record _ ->
        "record"
    | Enum _ ->
        "enum"
    | Map _ ->
        "map"

  let test types () =
    List.iter
      (fun ty ->
        let param = Convert.of_ty ty in
        let obj = Convert.to_json param in
        let verify_func = verify_func param in
        verify (convert_param_name param) verify_func obj
      )
      types

  let tests =
    [
      ("serialize", `Quick, test param_types)
    ; ("deserialize", `Quick, test result_types)
    ]
end

module StringOfTyWithEnumsTest = struct
  open Datamodel_types
  module StringMap = Json.StringMap

  let verify description verify_func actual =
    Alcotest.(check bool) description true (verify_func actual)

  let verify_string (ty, enums) = ty = "string" && enums = StringMap.empty

  let test_string () =
    let ty, enums = Json.string_of_ty_with_enums String in
    verify "String" verify_string (ty, enums)

  let test_secret_string () =
    let ty, enums = Json.string_of_ty_with_enums SecretString in
    verify "SecretString" verify_string (ty, enums)

  let verify_float (ty, enums) = ty = "float64" && enums = StringMap.empty

  let test_float () =
    let ty, enums = Json.string_of_ty_with_enums Float in
    verify "Float" verify_float (ty, enums)

  let verify_bool (ty, enums) = ty = "bool" && enums = StringMap.empty

  let test_bool () =
    let ty, enums = Json.string_of_ty_with_enums Bool in
    verify "bool" verify_bool (ty, enums)

  let verify_datetime (ty, enums) = ty = "time.Time" && enums = StringMap.empty

  let test_datetime () =
    let ty, enums = Json.string_of_ty_with_enums DateTime in
    verify "datetime" verify_datetime (ty, enums)

  let enum_lst = [("a", "b"); ("c", "d")]

  let verify_enum (ty, enums) =
    ty = "UpdateSync" && enums = StringMap.singleton "UpdateSync" enum_lst

  let test_enum () =
    let ty, enums =
      Json.string_of_ty_with_enums (Enum ("update_sync", enum_lst))
    in
    verify "enum" verify_enum (ty, enums)

  let verify_ref (ty, enums) = ty = "PoolRef" && enums = StringMap.empty

  let test_ref () =
    let ty, enums = Json.string_of_ty_with_enums (Ref "pool") in
    verify "ref" verify_ref (ty, enums)

  let verify_record (ty, enums) = ty = "PoolRecord" && enums = StringMap.empty

  let test_record () =
    let ty, enums = Json.string_of_ty_with_enums (Record "pool") in
    verify "record" verify_record (ty, enums)

  let verify_option (ty, enums) = ty = "OptionString" && enums = StringMap.empty

  let test_option () =
    let ty, enums = Json.string_of_ty_with_enums (Option String) in
    verify "option" verify_option (ty, enums)

  let verify_map (ty, enums) =
    ty = "map[int]UpdateSync"
    && enums = StringMap.singleton "UpdateSync" enum_lst

  let test_map () =
    let ty, enums =
      Json.string_of_ty_with_enums (Map (Int, Enum ("update_sync", enum_lst)))
    in
    verify "map" verify_map (ty, enums)

  let tests =
    [
      ("String", `Quick, test_string)
    ; ("SecretString", `Quick, test_secret_string)
    ; ("Float", `Quick, test_float)
    ; ("Bool", `Quick, test_bool)
    ; ("DateTime", `Quick, test_datetime)
    ; ("Enum", `Quick, test_enum)
    ; ("Ref", `Quick, test_ref)
    ; ("Record", `Quick, test_record)
    ; ("Option", `Quick, test_option)
    ; ("Map", `Quick, test_map)
    ]
end

let tests =
  make_suite "gen_go_binding_"
    [
      ("snake_to_camel", SnakeToCamelTest.tests)
    ; ("suffix_of_type", SuffixOfTypeTest.tests)
    ; ("string_of_ty_with_enums", StringOfTyWithEnumsTest.tests)
    ; ("templates", TemplatesTest.tests)
    ; ("generated_mustache_jsons", TestGeneratedJson.tests)
    ; ("group_params", GroupParamsTest.tests)
    ; ("generated_convert_jsons", TestConvertGeneratedJson.tests)
    ]

let () = Alcotest.run "Gen go binding" tests
