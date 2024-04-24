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
  ; "func_partial_type"
  ; "first"
  ]

let verify_param_member = function
  | "is_session_id", `Bool _
  | "first", `Bool _
  | "type", `String _
  | "name", `String _
  | "name_internal", `String _
  | "doc", `String _
  | "func_partial_type", `String _ ->
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
  | "async", `Bool _ | "has_error", `Bool _ | "errors", `Null ->
      true
  | _ ->
      false

let verify_sesseion_message_member = function
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
  | "header_params", `A params ->
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
  ]

let session_message_keys =
  ["session_login"; "session_logout"; "header_params"] @ message_keys

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
  ]

let verify_obj = function
  | `O members ->
      schema_check obj_keys verify_obj_member members
  | _ ->
      false

let verify_msgs_or_errors lst =
  let verify_msg_or_error = function
    | `O [("name", `String _)] ->
        true
    | _ ->
        false
  in
  List.for_all verify_msg_or_error lst

let verify_simple_convert_member = function
  | "func_name_suffix", `String _ | "type", `String _ ->
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
              ; ( "header_params"
                , `A
                    [
                      `O
                        [
                          ("type", `String "string")
                        ; ("name", `String "uname")
                        ; ("name_internal", `String "uname")
                        ; ("func_partial_type", `String "String")
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
                        ; ("func_partial_type", `String "String")
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
                ("session_logout", `Bool true)
              ; ("session_login", `Bool false)
              ; ("class_name", `String "session")
              ; ("class_name_exported", `String "Session")
              ; ("method_name", `String "logout")
              ; ("method_name_exported", `String "Logout")
              ; ("description", `String "Logout Log out of a session")
              ; ("async", `Bool false)
              ; ("func_params", `A [])
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
              ; ( "params"
                , `A
                    [
                      `O
                        [
                          ("type", `String "SessionRef")
                        ; ("name", `String "session_id")
                        ; ("name_internal", `String "sessionID")
                        ; ("func_name_suffix", `String "SessionRef")
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
                    ; ("func_partial_type", `String "String")
                    ]
                )
              ; ("has_error", `Bool false)
              ; ("errors", `A [])
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

  let simple_type_rendered = string_of_file "simple_type_convert.go"

  let int_convert_rendered = string_of_file "int_convert.go"

  let float_convert_rendered = string_of_file "float_convert.go"

  let time_convert_rendered = string_of_file "time_convert.go"

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

let tests =
  make_suite "gen_go_binding_"
    [
      ("snake_to_camel", SnakeToCamelTest.tests)
    ; ("templates", TemplatesTest.tests)
    ; ("generated_mustache_jsons", TestGeneratedJson.tests)
    ; ("generated_convert_jsons", TestConvertGeneratedJson.tests)
    ]

let () = Alcotest.run "Gen go binding" tests
