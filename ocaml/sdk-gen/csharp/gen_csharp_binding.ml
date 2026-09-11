(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

open Printf
open Datamodel
open Datamodel_common
open Datamodel_types
open Datamodel_utils
open Dm_api
open CommonFunctions
module DT = Datamodel_types
module DU = Datamodel_utils

module TypeSet = Set.Make (struct
  type t = DT.ty

  let compare = compare
end)

let get_deprecated_attribute message =
  let version = message.msg_release.internal_deprecated_since in
  match version with
  | None ->
      ""
  | Some versionString ->
      "[Deprecated(\"" ^ get_release_branding versionString ^ "\")]"

let destdir = "autogen-out/src"

let templdir = "templates"

let api =
  Datamodel_utils.named_self := true ;

  let field_filter field =
    (not field.internal_only) && List.mem "closed" field.release.internal
  in
  let message_filter msg =
    Datamodel_utils.on_client_side msg
    && (not msg.msg_hide_from_docs)
    && List.mem "closed" msg.msg_release.internal
  in
  let filter api = filter_by ~field:field_filter ~message:message_filter api in
  Datamodel.all_api
  |> filter
  |> Datamodel_utils.add_implicit_messages ~document_order:false
  |> filter

let classes =
  List.filter
    (fun x -> not (List.mem x.name ["debug"; "event"]))
    (objects_of_api api)

let enums = ref TypeSet.empty

let maps = ref TypeSet.empty

let enum_of_wire =
  Astring.String.map (fun x -> match x with '-' -> '_' | _ -> x)

let api_members = ref []

let session_param msg =
  {
    param_type= Ref _session
  ; param_name= "session"
  ; param_doc= "The session"
  ; param_default= None
  ; param_release= msg.msg_release
  }

let rec main () =
  render_file
    ("JsonRpcClient.mustache", "JsonRpcClient.cs")
    (gen_proxy CommonFunctions.JsonRpc)
    templdir destdir ;

  let msgJson =
    `O
      [
        ( "message_types"
        , `A
            (List.map
               (fun x -> `O [("message_type", `String (fst x))])
               !Api_messages.msgList
            )
        )
      ]
  in
  render_file ("Message2.mustache", "Message2.cs") msgJson templdir destdir ;

  classes
  |> List.filter (fun x -> x.name <> "session")
  |> List.iter gen_class_file ;
  TypeSet.iter gen_enum !enums ;
  gen_maps () ;
  render_file
    ("HTTP_actions.mustache", "HTTP_actions.cs")
    (gen_http_actions ()) templdir destdir ;
  gen_relations () ;
  render_file
    ("ApiVersion.mustache", "ApiVersion.cs")
    json_releases templdir destdir

(* ------------------- category: relations *)
and relations = Hashtbl.create 10

and gen_relations () =
  List.iter process_relations (relations_of_api api) ;
  let typelist =
    List.rev (Hashtbl.fold (fun k v acc -> (k, v) :: acc) relations [])
  in
  let json =
    `O
      [
        ( "types"
        , `A
            (List.map
               (fun (k, v) ->
                 `O
                   [
                     ("type", `String (exposed_class_name k))
                   ; ( "relations"
                     , `A
                         (List.map
                            (fun (x, y, z) ->
                              `O
                                [
                                  ("field", `String x)
                                ; ("manyType", `String y)
                                ; ("manyField", `String z)
                                ]
                            )
                            v
                         )
                     )
                   ]
               )
               typelist
            )
        )
      ]
  in
  render_file ("Relation.mustache", "Relation.cs") json templdir destdir

and process_relations ((oneClass, oneField), (manyClass, manyField)) =
  let value =
    (manyField, oneClass, oneField)
    :: Option.value (Hashtbl.find_opt relations manyClass) ~default:[]
  in
  Hashtbl.replace relations manyClass value

(* ------------------- category: http_actions *)
and gen_http_actions () =
  (* Each action has:
     (unique public name, (HTTP method, URI, whether to expose in SDK, [args to expose in SDK], [allowed_roles], [(sub-action,allowed_roles)]))
  *)
  let decl_of_sdkarg = function
    | String_query_arg s ->
        sprintf "string %s = null" (escaped s)
    | Int64_query_arg s ->
        sprintf "long? %s = null" (escaped s)
    | Bool_query_arg s ->
        sprintf "bool? %s = null" (escaped s)
    | Varargs_query_arg ->
        "params string[] args /* alternate names and values */"
  in
  let use_of_sdkarg = function
    | String_query_arg s | Int64_query_arg s | Bool_query_arg s ->
        sprintf {|"%s", %s|} s (escaped s)
    | Varargs_query_arg ->
        "args"
  in
  let filtered_actions =
    http_actions |> List.filter (fun (_, (_, _, sdk, _, _, _)) -> sdk)
  in
  `O
    [
      ( "http_actions"
      , `A
          (List.map
             (fun (name, (meth, uri, _, sdkargs, _, _)) ->
               `O
                 [
                   ("name", `String name)
                 ; ("isPut", `Bool (meth == Put))
                 ; ("isGet", `Bool (meth == Get))
                 ; ("uri", `String uri)
                 ; ( "args"
                   , `A
                       (List.map
                          (fun x ->
                            `O
                              [
                                ("arg_decl", `String (decl_of_sdkarg x))
                              ; ("arg_use", `String (use_of_sdkarg x))
                              ]
                          )
                          sdkargs
                       )
                   )
                 ]
             )
             filtered_actions
          )
      )
    ]

(* ------------------- category: classes *)
and gen_class_file cls =
  let name = exposed_class_name cls.name in
  if not (List.mem name !api_members) then api_members := name :: !api_members ;
  render_file ("Class.mustache", name ^ ".cs") (gen_class cls) templdir destdir

and gen_class cls =
  let classname = exposed_class_name cls.name in
  let messages =
    List.filter
      (fun msg -> String.compare msg.msg_name "get_all_records_where" <> 0)
      cls.messages
  in
  let rec flatten_contents contents =
    List.fold_left
      (fun l -> function
        | Field f ->
            f :: l
        | Namespace (_name, contents) ->
            flatten_contents contents @ l
        )
      [] contents
  in
  let fields =
    cls.contents
    |> flatten_contents
    |> List.filter (fun f -> not f.internal_only)
    |> List.rev
  in
  let fields_no_ops =
    fields |> List.filter (fun f -> full_name f <> "current_operations")
  in
  `O
    [
      ("class", `String classname)
    ; ("class_doc", `String (escape_xml cls.description))
    ; ("class_rel", `String (get_published_info_class cls))
    ; ("has_fields", `Bool (List.length fields > 0))
    ; ( "has_current_ops"
      , `Bool (List.length fields <> List.length fields_no_ops)
      )
    ; ( "fields_no_ops"
      , `A
          (List.map
             (fun f ->
               `O
                 [
                   ("field", `String (full_name f))
                 ; ("is_last", `Bool (is_last f fields_no_ops))
                 ]
             )
             fields_no_ops
          )
      )
    ; ( "all_fields"
      , `A
          (List.map
             (fun f ->
               `O
                 [
                   ("field", `String (full_name f))
                 ; ("is_last", `Bool (is_last f fields))
                 ; ("field_type", `String (exposed_type f.ty))
                 ; ("field_doc", `String (escape_xml f.field_description))
                 ; ("field_rel", `String (get_published_info_field f cls))
                 ; ( "field_default"
                   , `String (get_default_value_opt f.ty f.default_value true)
                   )
                 ; ( "field_marshalling"
                   , `String (convert_from_hashtable (full_name f) f.ty)
                   )
                 ; ("json_attr", `String (json_serialization_attr f))
                 ]
             )
             fields
          )
      )
    ; ( "all_methods"
      , `A
          (List.map
             (fun x ->
               let deprecated = function
                 | None ->
                     ""
                 | Some v ->
                     get_release_branding v
               in
               let has_return = function
                 | Some (_, _) ->
                     true
                 | None ->
                     false
               in
               let is_self_param p msg =
                 p.param_name = "self" || p.param_type = Ref msg.msg_obj_name
               in
               let self_params, _ =
                 List.partition (fun p -> is_self_param p x) x.msg_params
               in
               let get_param params =
                fun p ->
                 let p_name, p_info =
                   if is_self_param p x then
                     (sprintf "_%s" (String.lowercase_ascii classname), "")
                   else
                     ( sprintf "_%s" (String.lowercase_ascii p.param_name)
                     , get_published_info_param x p
                     )
                 in
                 let param_name_proxy =
                   if p.param_name = "session" then
                     sprintf "_session.opaque_ref"
                   else
                     p_name
                 in
                 `O
                   [
                     ("param_name", `String p_name)
                   ; ("param_name_proxy", `String param_name_proxy)
                   ; ("param_doc", `String (escape_xml p.param_doc))
                   ; ("param_rel", `String p_info)
                   ; ("param_type", `String (internal_type p.param_type))
                   ; ("is_last", `Bool (is_last p params))
                   ]
               in
               let get_overload =
                fun o ->
                 let params = session_param x :: (self_params @ o) in
                 `O
                   [
                     ("method", `String x.msg_name)
                   ; ("async", `Bool x.msg_async)
                   ; ( "deprecated"
                     , `String
                         (deprecated x.msg_release.internal_deprecated_since)
                     )
                   ; ("client_method", `String (proxy_msg_name classname x))
                   ; ("method_doc", `String x.msg_doc)
                   ; ("method_rel", `String (get_published_info_message x cls))
                   ; ("method_rbac", `String (get_minimum_allowed_role x))
                   ; ("has_return", `Bool (has_return x.msg_result))
                   ; ("params", `A (List.map (get_param params) params))
                   ; ("result", `String (exposed_type_opt x.msg_result))
                   ]
               in
               `O
                 [
                   ( "overloads"
                   , `A (List.map get_overload (gen_param_groups x x.msg_params))
                   )
                 ]
             )
             messages
          )
      )
    ]

and gen_proxy protocol =
  let all_methods = classes |> List.concat_map gen_proxy_class_methods in
  match protocol with
  | CommonFunctions.JsonRpc ->
      let json_method x = `O [("client_method", `String x)] in
      `O [("client_methods", `A (List.map json_method all_methods))]
  | _ ->
      raise Unknown_wire_protocol

and gen_overloads generator message =
  match message.msg_params with
  | [] ->
      [generator []]
  | _ ->
      let paramGroups = gen_param_groups message message.msg_params in
      List.map generator paramGroups

and gen_proxy_class_methods {name; messages; _} =
  let gen_message_overloads name message =
    let generator params = gen_proxy_method name message params in
    gen_overloads generator message
  in
  messages |> List.concat_map (gen_message_overloads name)

and gen_proxy_method classname message params =
  let proxy_msg_name = proxy_msg_name classname message in
  let paramsJsonWithTypes =
    proxy_params ~with_types:true message classname params
  in
  let paramsJsonNoTypes =
    proxy_params ~with_types:false message classname params
  in
  let return_word =
    match message.msg_result with Some (_, _) -> "return " | None -> ""
  in
  let param_converters =
    List.map (fun x -> json_converter x.param_type) params
  in
  let converters =
    json_converter_opt message.msg_result :: param_converters
    |> List.filter (fun x -> x <> "")
  in
  let async_converters =
    "new XenRefConverter<Task>()" :: param_converters
    |> List.filter (fun x -> x <> "")
  in
  let sync =
    sprintf
      "\n\
      \        public %s %s(%s)\n\
      \        {\n\
      \            var converters = new List<JsonConverter> {%s};\n\
      \            var serializer = CreateSerializer(converters);\n\
      \            %sRpc%s(\"%s.%s\", new JArray(%s), serializer);\n\
      \        }"
      (exposed_type_opt message.msg_result)
      proxy_msg_name paramsJsonWithTypes
      (String.concat ", " converters)
      return_word
      (json_deserialise_opt message.msg_result)
      classname message.msg_name paramsJsonNoTypes
  in
  let async =
    if message.msg_async then
      sprintf
        "\n\n\
        \        public XenRef<Task> async_%s(%s)\n\
        \        {\n\
        \            var converters = new List<JsonConverter> {%s};\n\
        \            var serializer = CreateSerializer(converters);\n\
        \            return Rpc<XenRef<Task>>(\"Async.%s.%s\", new JArray(%s), \
         serializer);\n\
        \        }"
        proxy_msg_name paramsJsonWithTypes
        (String.concat ", " async_converters)
        classname message.msg_name paramsJsonNoTypes
    else
      ""
  in
  sync ^ async

and proxy_params ~with_types message classname params =
  let refParam =
    sprintf
      ( if with_types then
          "string _%s"
        else
          "_%s ?? \"\""
      )
      (String.lowercase_ascii classname)
  in
  let args = List.map (proxy_param ~with_types) params in
  let args =
    if is_method_static message then
      args
    else
      refParam :: args
  in
  let args =
    if message.msg_session then
      ( if with_types then
          "string session"
        else
          "session"
      )
      :: args
    else
      args
  in
  String.concat ", " args

and proxy_param ~with_types p =
  if with_types then
    let exposed_type_json = function
      | Ref _ ->
          "string"
      | x ->
          exposed_type x
    in
    sprintf "%s _%s"
      (exposed_type_json p.param_type)
      (String.lowercase_ascii p.param_name)
  else
    json_param p

(* ------------------- category: enums *)
and gen_enum = function
  | Enum (name, contents) ->
      if not (List.mem name !api_members) then
        api_members := name :: !api_members ;
      render_file
        ("Enum.mustache", name ^ ".cs")
        (gen_enum' name contents) templdir destdir
  | _ ->
      assert false

and gen_enum' name contents =
  let members = List.filter (fun (x, _) -> x <> "unknown") contents in
  let enum_member (x, y) =
    `O
      [
        ("enum_member", `String (enum_of_wire x))
      ; ("enum_member_wire", `String x)
      ; ("enum_member_descr", `String y)
      ]
  in
  `O
    [
      ("enum", `String name); ("enum_members", `A (List.map enum_member members))
    ]

(* ------------------- category: maps *)
and gen_maps () =
  let mapList = List.rev (TypeSet.fold (fun x acc -> x :: acc) !maps []) in
  let json =
    `O
      [
        ( "all_maps"
        , `A
            (List.map
               (function
                 | Map (l, r) ->
                     `O
                       [
                         ("map_key", `String (exposed_type l))
                       ; ("map_value", `String (exposed_type r))
                       ; ( "sanitised_key"
                         , `String
                             (sanitise_function_name (exposed_type_as_literal l))
                         )
                       ; ( "sanitised_value"
                         , `String
                             (sanitise_function_name (exposed_type_as_literal r))
                         )
                       ; ( "proxy_key"
                         , `String (simple_convert_from_proxy "key" l)
                         )
                       ; ( "proxy_value"
                         , `String
                             (convert_from_proxy_hashtable_value "table[key]" r)
                         )
                       ]
                 | _ ->
                     `Null
                 )
               mapList
            )
        )
      ]
  in
  render_file ("Maps.mustache", "Maps.cs") json templdir destdir

(* ------------------- category: utility *)
and exposed_type_opt = function
  | Some (typ, _) ->
      exposed_type typ
  | None ->
      "void"

and exposed_type = function
  | SecretString | String ->
      "string"
  | Int ->
      "long"
  | Float ->
      "double"
  | Bool ->
      "bool"
  | DateTime ->
      "DateTime"
  | Ref name ->
      sprintf "XenRef<%s>" (exposed_class_name name)
  | Set (Ref name) ->
      sprintf "List<XenRef<%s>>" (exposed_class_name name)
  | Set (Enum (name, _) as x) ->
      enums := TypeSet.add x !enums ;
      sprintf "List<%s>" name
  | Set Int ->
      "long[]"
  | Set String ->
      "string[]"
  | Set (Set String) ->
      "string[][]"
  | Enum (name, _) as x ->
      enums := TypeSet.add x !enums ;
      name
  | Map (u, v) ->
      sprintf "Dictionary<%s, %s>" (exposed_type u) (exposed_type v)
  | Record name ->
      exposed_class_name name
  | Set (Record name) ->
      sprintf "List<%s>" (exposed_class_name name)
  | Option Int ->
      "long?"
  | Option Float ->
      "double?"
  | Option Bool ->
      "bool?"
  | Option DateTime ->
      "DateTime?"
  | Option x ->
      exposed_type x
  | x ->
      eprintf "%s" (Types.to_string x) ;
      assert false

and internal_type = function
  | Ref name when name = "session" ->
      "Session"
  | Ref _ ->
      (* THIS SHOULD BE: Printf.sprintf "XenRef<%s>" name *) "string"
  | Set (Ref name) ->
      Printf.sprintf "List<XenRef<%s>>" (exposed_class_name name)
  | x ->
      exposed_type x

and exposed_type_as_literal = function
  | Set String ->
      "string_array"
  | Map (u, v) ->
      sprintf "Dictionary_%s_%s" (exposed_type u) (exposed_type v)
  | x ->
      exposed_type x

and convert_from_proxy_hashtable_value thing ty =
  match ty with
  | Int ->
      sprintf "%s == null ? 0 : long.Parse((string)%s)" thing thing
  | SecretString | String ->
      sprintf "%s == null ? null : (string)%s" thing thing
  | Set String ->
      sprintf
        "%s == null ? new string[] {} : Array.ConvertAll<object, \
         string>((object[])%s, Convert.ToString)"
        thing thing
  | _ ->
      convert_from_proxy thing ty

and convert_from_proxy thing ty =
  match ty with
  | DateTime ->
      thing
  | Bool ->
      simple_convert_from_proxy thing ty
  | Float ->
      simple_convert_from_proxy thing ty
  | Int ->
      sprintf "%s == null ? 0 : %s" thing (simple_convert_from_proxy thing ty)
  | Set String ->
      sprintf "%s == null ? new string[] {} : %s" thing
        (simple_convert_from_proxy thing ty)
  | Enum (name, _) ->
      sprintf "%s == null ? (%s) 0 : %s" thing name
        (simple_convert_from_proxy thing ty)
  | Option x ->
      convert_from_proxy thing x
  | _ ->
      sprintf "%s == null ? null : %s" thing (simple_convert_from_proxy thing ty)

and convert_from_hashtable fname ty =
  let field = sprintf "\"%s\"" fname in
  match ty with
  | DateTime ->
      sprintf "Marshalling.ParseDateTime(table, %s)" field
  | Bool ->
      sprintf "Marshalling.ParseBool(table, %s)" field
  | Float ->
      sprintf "Marshalling.ParseDouble(table, %s)" field
  | Int ->
      sprintf "Marshalling.ParseLong(table, %s)" field
  | Ref name ->
      sprintf "Marshalling.ParseRef<%s>(table, %s)" (exposed_class_name name)
        field
  | SecretString | String ->
      sprintf "Marshalling.ParseString(table, %s)" field
  | Set String ->
      sprintf "Marshalling.ParseStringArray(table, %s)" field
  | Set (Ref name) ->
      sprintf "Marshalling.ParseSetRef<%s>(table, %s)" (exposed_class_name name)
        field
  | Set (Enum (name, _)) ->
      sprintf
        "Helper.StringArrayToEnumList<%s>(Marshalling.ParseStringArray(table, \
         %s))"
        name field
  | Enum (name, _) ->
      sprintf
        "(%s)Helper.EnumParseDefault(typeof(%s), \
         Marshalling.ParseString(table, %s))"
        name name field
  | Map (Ref name, Record _) ->
      sprintf "Marshalling.ParseMapRefRecord<%s, Proxy_%s>(table, %s)"
        (exposed_class_name name) (exposed_class_name name) field
  | Map (u, v) as x ->
      maps := TypeSet.add x !maps ;
      sprintf "%s(Marshalling.ParseHashTable(table, %s))"
        (sanitise_function_name
           (sprintf "Maps.ToDictionary_%s_%s"
              (exposed_type_as_literal u)
              (exposed_type_as_literal v)
           )
        )
        field
  | Record name ->
      sprintf
        "(%s)Marshalling.convertStruct(typeof(%s), \
         Marshalling.ParseHashTable(table, %s));"
        (exposed_class_name name) (exposed_class_name name) field
  | Set (Record name) ->
      sprintf "Marshalling.ParseStringArray(%s).Select(p => new %s(p)).ToList()"
        field (exposed_class_name name)
  | Set Int ->
      sprintf "Marshalling.ParseLongArray(table, %s)" field
  | Option x ->
      convert_from_hashtable fname x
  | x ->
      eprintf "%s %s" fname (Types.to_string x) ;
      assert false

and sanitise_function_name name =
  let is_normal_char c = not (List.mem c ['>'; '<'; ','; ' ']) in
  Astring.String.filter is_normal_char name

and simple_convert_from_proxy thing ty =
  match ty with
  | DateTime ->
      thing
  | Int ->
      sprintf "long.Parse(%s)" thing
  | Bool ->
      sprintf "(bool)%s" thing
  | Float ->
      sprintf "Convert.ToDouble(%s)" thing
  | Ref name ->
      sprintf "XenRef<%s>.Create(%s)" (exposed_class_name name) thing
  | SecretString | String ->
      thing
  | Set String ->
      sprintf "(string[])%s" thing
  | Set (Set String) ->
      sprintf "(string[][])%s" thing
  | Set (Ref name) ->
      sprintf "XenRef<%s>.Create(%s)" (exposed_class_name name) thing
  | Set (Enum (name, _)) ->
      sprintf "Helper.StringArrayToEnumList<%s>(%s)" name thing
  | Enum (name, _) ->
      sprintf "(%s)Helper.EnumParseDefault(typeof(%s), (string)%s)" name name
        thing
  | Map (Ref name, Record _) ->
      sprintf "XenRef<%s>.Create<Proxy_%s>(%s)" (exposed_class_name name)
        (exposed_class_name name) thing
  | Map (u, v) as x ->
      maps := TypeSet.add x !maps ;
      sprintf "%s(%s)"
        (sanitise_function_name
           (sprintf "Maps.ToDictionary_%s_%s"
              (exposed_type_as_literal u)
              (exposed_type_as_literal v)
           )
        )
        thing
  | Record name ->
      sprintf "new %s(%s)" (exposed_class_name name) thing
  | Set (Record name) ->
      sprintf "%s.Select(p => new %s(p)).ToList()" thing
        (exposed_class_name name)
  | Set Int ->
      sprintf "Helper.StringArrayToLongArray(%s)" thing
  | x ->
      eprintf "%s" (Types.to_string x) ;
      assert false

and proxy_msg_name classname msg =
  sprintf "%s_%s"
    (String.lowercase_ascii classname)
    (String.lowercase_ascii msg.msg_name)

and exposed_class_name classname = String.capitalize_ascii classname

and escaped = function
  | "params" ->
      "paramz"
  | "ref" ->
      "reff"
  | "public" ->
      "pubblic"
  | s ->
      s

and full_name field = escaped (String.concat "_" field.full_name)

and json_param p =
  let thing = String.lowercase_ascii p.param_name in
  match p.param_type with
  | Int | Float | Bool | DateTime ->
      sprintf "_%s" thing
  | SecretString | String | Ref _ ->
      sprintf "_%s ?? \"\"" thing
  | Enum _ ->
      sprintf "_%s.StringOf()" thing
  | Set (Ref _) ->
      sprintf "_%s == null ? new JArray() : JArray.FromObject(_%s, serializer)"
        thing thing
  | Set _ ->
      sprintf "_%s == null ? new JArray() : JArray.FromObject(_%s)" thing thing
  | Map (_, _) ->
      sprintf
        "_%s == null ? new JObject() : JObject.FromObject(_%s, serializer)"
        thing thing
  | Record _ ->
      sprintf "_%s.ToJObject()" thing
  | _ ->
      assert false

and json_deserialise_opt = function
  | Some (typ, _) ->
      sprintf "<%s>" (exposed_type typ)
  | None ->
      ""

and json_converter typ =
  match typ with
  | DateTime ->
      "new XenDateTimeConverter()"
  | Enum (name, _) ->
      sprintf "new %sConverter()" name
  | Ref name ->
      sprintf "new XenRefConverter<%s>()" (exposed_class_name name)
  | Set (Ref name) ->
      sprintf "new XenRefListConverter<%s>()" (exposed_class_name name)
  | Map (Ref u, Record _) ->
      sprintf "new XenRefXenObjectMapConverter<%s>()" (exposed_class_name u)
  | Map (Ref u, Ref v) ->
      sprintf "new XenRefXenRefMapConverter<%s, %s>()" (exposed_class_name u)
        (exposed_class_name v)
  | Map (Ref u, Int) ->
      sprintf "new XenRefLongMapConverter<%s>()" (exposed_class_name u)
  | Map (Ref u, String) ->
      sprintf "new XenRefStringMapConverter<%s>()" (exposed_class_name u)
  | Map (Ref u, Set String) ->
      sprintf "new XenRefStringSetMapConverter<%s>()" (exposed_class_name u)
  | Map (Ref u, Map (String, String)) ->
      sprintf "new XenRefStringStringMapMapConverter<%s>()"
        (exposed_class_name u)
  | Map (String, Ref v) ->
      sprintf "new StringXenRefMapConverter<%s>()" (exposed_class_name v)
  | Map (String, String) ->
      sprintf "new StringStringMapConverter()"
  | Map (Ref _, _) | Map (_, Ref _) ->
      failwith (sprintf "Need converter for %s" (exposed_type typ))
  | _ ->
      ""

and json_converter_opt = function
  | Some (typ, _) ->
      json_converter typ
  | None ->
      ""

and json_return_opt thing = function
  | Some (_, _) ->
      "return " ^ thing
  | None ->
      thing

and json_serialization_attr fr =
  match fr.ty with
  | DateTime ->
      "[JsonConverter(typeof(XenDateTimeConverter))]"
  | Enum (name, _) ->
      sprintf "[JsonConverter(typeof(%sConverter))]" name
  | Ref name ->
      sprintf "[JsonConverter(typeof(XenRefConverter<%s>))]"
        (exposed_class_name name)
  | Set (Ref name) ->
      sprintf "[JsonConverter(typeof(XenRefListConverter<%s>))]"
        (exposed_class_name name)
  | Map (Ref u, Record _) ->
      sprintf "[JsonConverter(typeof(XenRefObjectMapConverter<%s>))]"
        (exposed_class_name u)
  | Map (Ref u, Ref v) ->
      sprintf "[JsonConverter(typeof(XenRefXenRefMapConverter<%s, %s>))]"
        (exposed_class_name u) (exposed_class_name v)
  | Map (Ref u, Int) ->
      sprintf "[JsonConverter(typeof(XenRefLongMapConverter<%s>))]"
        (exposed_class_name u)
  | Map (Ref u, String) ->
      sprintf "[JsonConverter(typeof(XenRefStringMapConverter<%s>))]"
        (exposed_class_name u)
  | Map (String, Ref v) ->
      sprintf "[JsonConverter(typeof(StringXenRefMapConverter<%s>))]"
        (exposed_class_name v)
  | Map (String, String) ->
      "[JsonConverter(typeof(StringStringMapConverter))]"
  | Map (Ref u, Set String) ->
      sprintf "[JsonConverter(typeof(XenRefStringSetMapConverter<%s>))]"
        (exposed_class_name u)
  | Map (Ref _, _) | Map (_, Ref _) ->
      failwith (sprintf "Need converter for %s" fr.field_name)
  | _ ->
      ""

and get_default_value_opt ty value lang_default =
  let rec get_default_value = function
    | VString y ->
        ["\"" ^ y ^ "\""]
    | VInt y ->
        [Int64.to_string y]
    | VFloat y ->
        [sprintf "%.3f" y]
    | VBool y ->
        [string_of_bool y]
    | VDateTime y ->
        [
          Printf.sprintf
            "DateTime.ParseExact(\"%s\", \"yyyyMMddTHH:mm:ssZ\", \
             CultureInfo.InvariantCulture)"
            (Date.to_rfc3339 y)
        ]
    | VEnum y ->
        [enum_of_wire y]
    | VMap y ->
        List.map
          (fun (a, b) ->
            sprintf "{%s, %s}"
              (String.concat ", " (get_default_value a))
              (String.concat ", " (get_default_value b))
          )
          y
    | VSet y ->
        List.map (fun x -> String.concat ", " (get_default_value x)) y
    | VRef y ->
        if y = "" then
          ["Helper.NullOpaqueRef"]
        else
          [sprintf "\"%s\"" y]
  in
  match value with
  | Some y ->
      get_default_value_per_type ty (get_default_value y)
  | None ->
      if lang_default then
        get_default_value_per_type ty []
      else
        ""

and get_default_value_per_type ty thing =
  match ty with
  | DateTime | Int | Bool | Float ->
      if thing = [] then
        ""
      else
        sprintf " = %s" (String.concat ", " thing)
  | Ref _ ->
      sprintf " = new %s(%s)" (exposed_type ty)
        ( if thing = [] then
            "Helper.NullOpaqueRef"
          else
            String.concat ", " thing
        )
  | SecretString | String ->
      sprintf " = %s"
        ( if thing = [] then
            "\"\""
          else
            String.concat ", " thing
        )
  | Enum (name, _) ->
      if thing = [] then
        ""
      else
        sprintf " = %s.%s" name (String.concat ", " thing)
  | Set Int | Set String ->
      sprintf " = {%s}" (String.concat ", " thing)
  | Set (Ref name) ->
      sprintf " = new %s() {%s}" (exposed_type ty)
        ( if thing = [] then
            ""
          else
            String.concat ", "
              (List.map
                 (fun x ->
                   sprintf "new XenRef<%s>(%s)" (exposed_class_name name) x
                 )
                 thing
              )
        )
  | Set _ ->
      sprintf " = new %s() {%s}" (exposed_type ty) (String.concat ", " thing)
  | Map (u, v) ->
      sprintf " = new Dictionary<%s, %s>() {%s}" (exposed_type u)
        (exposed_type v) (String.concat ", " thing)
  | Record _ ->
      sprintf " = new %s()" (exposed_type ty)
  | Option x ->
      if thing = [] then
        ""
      else
        get_default_value_per_type x thing

let _ = main ()
