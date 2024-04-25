(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

open Printf
open Str
open Datamodel_types
open Dm_api
open CommonFunctions
module DT = Datamodel_types
module DU = Datamodel_utils

(*Filter out all the bits of the data model we don't want to put in the api.
  For instance we don't want the things which are marked internal only, or the
  ones marked hide_from_docs*)
let api =
  Datamodel_utils.named_self := true ;

  let obj_filter _ = true in
  let field_filter field =
    (not field.internal_only) && List.mem "closed" field.release.internal
  in
  let message_filter msg =
    Datamodel_utils.on_client_side msg
    && (not msg.msg_hide_from_docs)
    && List.mem "closed" msg.msg_release.internal
  in
  filter obj_filter field_filter message_filter
    (Datamodel_utils.add_implicit_messages ~document_order:false
       (filter obj_filter field_filter message_filter Datamodel.all_api)
    )

(*Here we extract a list of objs (look in datamodel_types.ml for the structure definitions)*)
let classes = objects_of_api api

(*How shall we translate datamodel identifiers into Java, with its conventions about case, and reserved words?*)

let reserved_words = function
  | "class" ->
      "clazz"
  | "clone" ->
      "createClone"
  | "param-name" ->
      "param_name"
  | "interface" ->
      "iface"
  | "import" ->
      "import_"
  | s ->
      s

(* Given a XenAPI on-the-wire representation of an enum value, return the Java enum *)
let enum_of_wire x =
  global_replace (regexp_string "-") "_" (String.uppercase_ascii x)

let second_character_is_uppercase s =
  if String.length s < 2 then
    false
  else
    let second_char = String.sub s 1 1 in
    second_char = String.uppercase_ascii second_char

let transform s =
  if second_character_is_uppercase s then
    s
  else
    String.capitalize_ascii (reserved_words (String.uncapitalize_ascii s))

let class_case x =
  String.concat "" (List.map transform (Astring.String.cuts ~sep:"_" x))

let keywords = [("public", "_public")]

let keyword_map s =
  if List.mem_assoc s keywords then List.assoc s keywords else s

let camel_case s =
  let ss = Astring.String.cuts ~sep:"_" s |> List.map transform in
  let result =
    match ss with
    | [] ->
        ""
    | h :: tl ->
        let h' =
          if String.length h > 1 then
            let sndchar = String.sub h 1 1 in
            if sndchar = String.uppercase_ascii sndchar then
              h
            else
              String.uncapitalize_ascii h
          else
            String.uncapitalize_ascii h
        in
        h' ^ String.concat "" tl
  in
  keyword_map result

let exception_class_case x =
  String.concat ""
    (List.map
       (fun s -> String.capitalize_ascii (String.lowercase_ascii s))
       (Astring.String.cuts ~sep:"_" x)
    )

(*As we process the datamodel, we collect information about enumerations, types*)
(* and records, which we use to create Types.java later *)

let enums = Hashtbl.create 10

let records = Hashtbl.create 10

(*We want an empty mutable set to keep the types in.*)
module Ty = struct
  type t = DT.ty

  let compare = compare
end

module TypeSet = Set.Make (Ty)

let types = ref TypeSet.empty

(* Helper functions for types *)
let rec get_java_type ty =
  types := TypeSet.add ty !types ;
  match ty with
  | SecretString | String ->
      "String"
  | Int ->
      "Long"
  | Float ->
      "Double"
  | Bool ->
      "Boolean"
  | DateTime ->
      "Date"
  | Enum (name, ls) ->
      Hashtbl.replace enums name ls ;
      sprintf "Types.%s" (class_case name)
  | Set t1 ->
      sprintf "Set<%s>" (get_java_type t1)
  | Map (t1, t2) ->
      sprintf "Map<%s, %s>" (get_java_type t1) (get_java_type t2)
  | Ref x ->
      class_case x (* We want to hide all refs *)
  | Record x ->
      sprintf "%s.Record" (class_case x)
  | Option x ->
      get_java_type x

(*We'd like the list of XenAPI objects to appear as an enumeration so we can*)
(* switch on them, so add it using this mechanism*)
let switch_enum =
  Enum ("XenAPIObjects", List.map (fun x -> (x.name, x.description)) classes)

(*Helper function for get_marshall_function*)
let rec get_marshall_function_rec = function
  | SecretString | String ->
      "String"
  | Int ->
      "Long"
  | Float ->
      "Double"
  | Bool ->
      "Boolean"
  | DateTime ->
      "Date"
  | Enum (name, _) ->
      class_case name
  | Set t1 ->
      sprintf "SetOf%s" (get_marshall_function_rec t1)
  | Map (t1, t2) ->
      sprintf "MapOf%s%s"
        (get_marshall_function_rec t1)
        (get_marshall_function_rec t2)
  | Ref ty ->
      class_case ty (* We want to hide all refs *)
  | Record ty ->
      sprintf "%sRecord" (class_case ty)
  | Option ty ->
      get_marshall_function_rec ty

let get_marshall_function ty = "to" ^ get_marshall_function_rec ty

let get_java_type_or_void = function
  | None ->
      "void"
  | Some (ty, _) ->
      get_java_type ty

let get_method_deprecated_release_name message =
  match message.msg_release.internal_deprecated_since with
  | Some version ->
      Some (get_release_branding version)
  | None ->
      None

let field_default = function
  | SecretString | String ->
      {|""|}
  | Int ->
      "0"
  | Float ->
      "0.0"
  | Bool ->
      "false"
  | DateTime ->
      "new Date(0)"
  | Enum ("vif_locking_mode", _) ->
      "Types.VifLockingMode.NETWORK_DEFAULT" (* XOP-372 *)
  | Enum (name, _) ->
      sprintf "Types.%s.UNRECOGNIZED" (class_case name)
  | Set t1 ->
      sprintf "new LinkedHashSet<%s>()" (get_java_type t1)
  | Map (t1, t2) ->
      sprintf "new HashMap<%s, %s>()" (get_java_type t1) (get_java_type t2)
  | Ref ty ->
      sprintf {|new %s("OpaqueRef:NULL")|} (class_case ty)
  | Record _ ->
      assert false
  | Option _ ->
      "null"

let class_is_empty cls = cls.contents = []

(*This generates the special case code for marshalling the snapshot field in an Event.Record*)

let generate_snapshot_hack =
  {|
       Object a,b;
       a = map.get("snapshot");
       switch(|}
  ^ get_marshall_function switch_enum
  ^ {|(record.clazz)){
|}
  ^ String.concat "\n"
      (List.map
         (fun x ->
           "        case "
           ^ String.uppercase_ascii x
           ^ ": b = "
           ^ get_marshall_function (Record x)
           ^ "(a); break;"
         )
         (List.map
            (fun x -> x.name)
            (List.filter (fun x -> not (class_is_empty x)) classes)
         )
      )
  ^ {|
        default: 
           throw new RuntimeException("Internal error in auto-generated code whilst unmarshalling event snapshot");
      }
      record.snapshot = b;|}

let gen_marshall_record_field prefix field =
  let ty = get_marshall_function field.ty in
  let name = String.concat "_" (List.rev (field.field_name :: prefix)) in
  let name' = camel_case name in
  "        record." ^ name ^ " = " ^ ty ^ "(map.get(\"" ^ name' ^ "\"));\n"

let rec gen_marshall_record_namespace prefix (name, contents) =
  String.concat "\n"
    (List.map (gen_marshall_record_contents (name :: prefix)) contents)

and gen_marshall_record_contents prefix = function
  | Field f ->
      gen_marshall_record_field prefix f
  | Namespace (n, cs) ->
      gen_marshall_record_namespace prefix (n, cs)

(*don't generate for complicated types. They're not needed.*)

let rec gen_marshall_body = function
  | SecretString | String ->
      "return (String) object;\n"
  | Int ->
      "return Long.valueOf((String) object);\n"
  | Float ->
      "return (Double) object;\n"
  | Bool ->
      "return (Boolean) object;\n"
  | DateTime ->
      {|
      try {
        return (Date) object;
    } catch (ClassCastException e){
        //Occasionally the date comes back as an ocaml float rather than
        //in the xmlrpc format! Catch this and convert.
        return (new Date((long) (1000*Double.parseDouble((String) object))));
    }|}
  | Ref ty ->
      "return new" ^ class_case ty ^ "((String) object);\n"
  | Enum (name, _) ->
      {|try {
            return |}
      ^ class_case name
      ^ {|.valueOf(((String) object).toUpperCase().replace('-','_'));
        } catch (IllegalArgumentException ex) { 
            return |}
      ^ class_case name
      ^ {|.UNRECOGNIZED;
        }|}
  | Set ty ->
      let ty_name = get_java_type ty in
      let marshall_fn = get_marshall_function ty in
      {|Object[] items = (Object[]) object;
        Set<|}
      ^ ty_name
      ^ {|> result = new LinkedHashSet<>(); 
        for(Object item: items) {
          |}
      ^ ty_name
      ^ {| typed = |}
      ^ marshall_fn
      ^ {|(item); 
          result.add(typed);
        }
        return result;|}
  | Map (ty, ty') ->
      let ty_name = get_java_type ty in
      let ty_name' = get_java_type ty' in
      let marshall_fn = get_marshall_function ty in
      let marshall_fn' = get_marshall_function ty' in
      {|var map = (Map<Object, Object>)object;
        var result = new HashMap<|}
      ^ ty_name
      ^ {|,|}
      ^ ty_name'
      ^ {|>(); 
        for(var entry: map.entrySet()) {
          var key = |}
      ^ marshall_fn
      ^ {|(entry.getKey());
          var value = |}
      ^ marshall_fn'
      ^ {|(entry.getValue());
          result.put(key, value);
        }
        return result;|}
  | Record ty ->
      let contents = Hashtbl.find records ty in
      let cls_name = class_case ty in
      {|Map<String,Object> map = (Map<String,Object>) object;|}
      ^ cls_name
      ^ {|.Record record = new |}
      ^ cls_name
      ^ {| .Record(); |}
      ^ String.concat "" (List.map (gen_marshall_record_contents []) contents)
      ^
      (*Event.Record needs a special case to handle snapshots*)
      if ty = "event" then
        generate_snapshot_hack
      else
        "        return record;"
  | Option ty ->
      gen_marshall_body ty

let gen_error_field_name field =
  camel_case (String.concat "_" (Astring.String.cuts ~sep:" " field))

let populate_releases templdir class_dir =
  render_file
    ("APIVersion.mustache", "APIVersion.java")
    json_releases templdir class_dir

(*
  Populate JSON object for the Types.java template     
*)
let get_types_errors_json =
  let list_errors =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) Datamodel.errors []
  in
  List.map
    (fun (_, error) ->
      let class_name = exception_class_case error.err_name in
      let err_params =
        List.mapi
          (fun index value ->
            `O
              [
                ("name", `String (gen_error_field_name value))
              ; ("index", `Float (Int.to_float index))
              ; ("last", `Bool (index == List.length error.err_params - 1))
              ]
          )
          error.err_params
      in
      `O
        [
          ("description", `String (escape_xml error.err_doc))
        ; ("class_name", `String class_name)
        ; ("err_params", `A err_params)
        ]
    )
    list_errors

let get_types_enums_json =
  let list_enums = Hashtbl.fold (fun k v acc -> (k, v) :: acc) enums [] in
  List.map
    (fun (enum_name, enum_values) ->
      let class_name = class_case enum_name in
      let mapped_values =
        List.map
          (fun (name, description) ->
            let escaped_description =
              global_replace (regexp_string "*/") "* /" description
            in
            let final_description =
              global_replace (regexp_string "\n") "\n         * "
                escaped_description
            in
            `O
              [
                ("name", `String name)
              ; ("name_uppercase", `String (enum_of_wire name))
              ; ("description", `String final_description)
              ]
          )
          enum_values
      in
      `O [("class_name", `String class_name); ("values", `A mapped_values)]
    )
    list_enums

let get_types_json types =
  let list_types = TypeSet.fold (fun t acc -> t :: acc) !types [] in
  List.map
    (fun t ->
      let type_string = get_java_type t in
      let class_name = class_case type_string in
      let method_name = get_marshall_function t in
      (*Every type which may be returned by a function may also be the result of the*)
      (* corresponding asynchronous task. We therefore need to generate corresponding*)
      (* marshalling functions which can take the raw xml of the tasks result field*)
      (* and turn it into the corresponding type. Luckily, the only things returned by*)
      (* asynchronous tasks are object references and strings, so rather than implementing*)
      (* the general recursive structure we'll just make one for each of the classes*)
      (* that's been registered as a marshall-needing type*)
      let generate_reference_task_result_func =
        match t with Ref _ -> true | _ -> false
      in
      `O
        [
          ("name", `String type_string)
        ; ("class_name", `String class_name)
        ; ("method_name", `String method_name)
        ; ( "suppress_unchecked_warning"
          , `Bool (match t with Map _ | Record _ -> true | _ -> false)
          )
        ; ( "generate_reference_task_result_func"
          , `Bool generate_reference_task_result_func
          )
        ; ("method_body", `String (gen_marshall_body t))
        ]
    )
    list_types

let populate_types types templdir class_dir =
  let errors = get_types_errors_json in
  let enums = get_types_enums_json in
  let types = get_types_json types in
  let json =
    `O [("errors", `A errors); ("enums", `A enums); ("types", `A types)]
  in
  render_file ("Types.mustache", "Types.java") json templdir class_dir

let get_message_object cls message async_version params =
  let is_method_async = async_version in
  let return_type =
    if is_method_async then
      "Task"
    else if
      String.lowercase_ascii cls.name = "event"
      && String.lowercase_ascii message.msg_name = "from"
    then
      "EventBatch"
    else
      get_java_type_or_void message.msg_result
  in
  let return_description =
    match message.msg_result with
    | None ->
        get_java_type_or_void message.msg_result
    | Some (_, description) ->
        description
  in
  let returns_void = message.msg_result = None && not async_version in
  let record_parameters =
    List.map
      (fun parameter ->
        `O [("name_camel", `String (camel_case parameter.param_name))]
      )
      (List.filter
         (function {param_type= Record _; _} -> true | _ -> false)
         message.msg_params
      )
  in
  let is_deprecated =
    match message.msg_release.internal_deprecated_since with
    | Some _ ->
        true
    | None ->
        false
  in
  let deprecated_release =
    match get_method_deprecated_release_name message with
    | Some v ->
        get_release_branding v
    | None ->
        ""
  in
  let type_reference =
    if is_method_async then
      "Task"
    else if message.msg_result != None then
      return_type
    else
      ""
  in
  let parameters =
    List.map
      (fun parameter ->
        let publish_info = get_published_info_param message parameter in
        let name_camel = camel_case parameter.param_name in
        let description = escape_xml parameter.param_doc in
        `O
          [
            ("type", `String (get_java_type parameter.param_type))
          ; ( "is_record"
            , `Bool
                (match parameter.param_type with Record _ -> true | _ -> false)
            )
          ; ("name_camel", `String name_camel)
          ; ("description", `String description)
          ; ("publish_info", `String publish_info)
          ]
      )
      params
  in
  let error_definitions =
    List.map
      (fun error ->
        let exception_name = exception_class_case error.err_name in
        ("Types." ^ exception_name, escape_xml error.err_doc)
      )
      message.msg_errors
  in
  let errors =
    List.map
      (fun (name, description) ->
        `O [("name", `String name); ("description", `String description)]
      )
      error_definitions
  in
  let is_static = is_method_static message in
  let session_parameter =
    `O
      [
        ("type", `String "String")
      ; ("is_record", `Bool false)
      ; ("name_camel", `String "sessionReference")
      ; ("description", `String "")
      ; ("publish_info", `String "")
      ]
  in
  let non_static_reference_parameter =
    `O
      [
        ("type", `String "String")
      ; ("is_record", `Bool false)
      ; ("name_camel", `String "this.ref")
      ; ("description", `String "")
      ; ("publish_info", `String "")
      ]
  in
  let extra_method_parameters =
    match (message.msg_session, is_static) with
    | true, true ->
        [session_parameter]
    | true, false ->
        [session_parameter; non_static_reference_parameter]
    | false, true ->
        []
    | false, false ->
        [non_static_reference_parameter]
  in
  let rec set_is_last params acc =
    match params with
    | [] ->
        []
    | `O last :: [] ->
        `O (("is_last", `Bool true) :: last) :: acc
    | `O h :: tail ->
        `O (("is_last", `Bool false) :: h) :: set_is_last tail acc
  in
  let method_parameters =
    set_is_last (extra_method_parameters @ parameters) []
  in
  `O
    [
      ("return_type", `String return_type)
    ; ("is_async", `Bool async_version)
    ; ("return_description", `String return_description)
    ; ("returns_void", `Bool returns_void)
    ; ("is_static", `Bool is_static)
    ; ("name_camel", `String (camel_case message.msg_name))
    ; ("name", `String message.msg_name)
    ; ("publish_info", `String (get_published_info_message message cls))
    ; ("description", `String (escape_xml message.msg_doc))
    ; ("minimum_allowed_role", `String (get_minimum_allowed_role message))
    ; ("object_name", `String message.msg_obj_name)
    ; ("supports_session", `Bool message.msg_session)
    ; ("record_parameters", `A record_parameters)
    ; ("is_deprecated", `Bool is_deprecated)
    ; ("deprecated_release", `String deprecated_release)
    ; ("type_reference", `String type_reference)
    ; ("parameters", `A parameters)
    ; ("method_parameters", `A method_parameters)
    ; ("errors", `A errors)
    ]

let populate_class cls templdir class_dir =
  Hashtbl.replace records cls.name cls.contents ;
  let class_name = class_case cls.name in
  let rec content_fields content namespace_name =
    match content with
    | Field f ->
        let name_with_prefix =
          if namespace_name == "" then
            f.field_name
          else
            namespace_name ^ "_" ^ f.field_name
        in
        let name_camel = camel_case name_with_prefix in
        let ty = get_java_type f.ty in
        let publish_info = get_published_info_field f cls in
        let description = escape_xml f.field_description in
        let is_deprecated = f.lifecycle.state = Lifecycle.Deprecated_s in
        let deprecated_release =
          if is_deprecated then
            get_release_branding (get_deprecated_release f.lifecycle.transitions)
          else
            ""
        in
        [
          `O
            [
              ("name", `String name_with_prefix)
            ; ("name_camel", `String name_camel)
            ; ("default_value", `String (field_default f.ty))
            ; ("description", `String description)
            ; ("type", `String ty)
            ; ("publish_info", `String publish_info)
            ; ("is_deprecated", `Bool is_deprecated)
            ; ("deprecated_release", `String deprecated_release)
            ]
        ]
    | Namespace (name, contents) ->
        List.flatten (List.map (fun c -> content_fields c name) contents)
  in
  let fields =
    List.flatten (List.map (fun c -> content_fields c "") cls.contents)
  in
  let rec get_async_and_sync_methods methods acc =
    match methods with
    | [] ->
        acc
    | h :: tail ->
        let get_variants messages =
          (* we get the param groups outside of the mapping because we know it's always the same message *)
          let params = gen_param_groups h h.msg_params in
          match params with
          | [] ->
              List.map
                (fun (message, is_async) -> (message, is_async, []))
                messages
          | _ ->
              List.map
                (fun (message, is_async) ->
                  List.map (fun param -> (message, is_async, param)) params
                )
                messages
              |> List.flatten
        in
        if h.msg_async then
          get_variants [(h, true); (h, false)]
          @ get_async_and_sync_methods tail acc
        else
          get_variants [(h, false)] @ get_async_and_sync_methods tail acc
  in
  let async_and_sync_methods = get_async_and_sync_methods cls.messages [] in
  let methods =
    List.map
      (fun (message, async_version, params) ->
        get_message_object cls message async_version params
      )
      async_and_sync_methods
  in
  let json =
    `O
      [
        ("class_name", `String class_name)
      ; ("description", `String cls.description)
      ; ("publish_info", `String (get_published_info_class cls))
      ; ("is_empty_class", `Bool (class_is_empty cls))
      ; ("is_event_class", `Bool (cls.name = "event"))
      ; ("fields", `A fields)
      ; ("methods", `A methods)
      ]
  in
  render_file ("Class.mustache", class_name ^ ".java") json templdir class_dir

let _ =
  let templdir = "templates" in
  let class_dir = "autogen/xen-api/src/main/java/com/xensource/xenapi" in
  populate_releases templdir class_dir ;
  populate_types types templdir class_dir ;
  List.iter (fun cls -> populate_class cls templdir class_dir) classes ;

  let uncommented_license = string_of_file "LICENSE" in
  let class_license = open_out "autogen/xen-api/src/main/resources/LICENSE" in
  output_string class_license uncommented_license
