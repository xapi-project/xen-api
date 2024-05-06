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
    verify "datetime" verify_record (ty, enums)

  let test_option () =
    let ty, enums = Json.string_of_ty_with_enums (Option String) in
    verify "datetime" verify_string (ty, enums)

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
    ]

let () = Alcotest.run "Gen go binding" tests
