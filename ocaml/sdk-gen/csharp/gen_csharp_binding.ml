(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

open Printf
open Datamodel
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

let destdir = "autogen/src"

let templdir = "templates"

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

let classes =
  List.filter
    (fun x -> not (List.mem x.name ["debug"; "event"]))
    (objects_of_api api)

let enums = ref TypeSet.empty

let maps = ref TypeSet.empty

let enum_of_wire =
  Astring.String.map (fun x -> match x with '-' -> '_' | _ -> x)

let api_members = ref []

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
  let sorted_members = List.sort String.compare !api_members in
  let json =
    `O
      [
        ( "api_members"
        , `A (List.map (fun x -> `O [("api_member", `String x)]) sorted_members)
        )
      ]
  in
  render_file
    ("XenServer.csproj.mustache", "XenServer.csproj")
    json templdir destdir ;
  render_file
    ("ApiVersion.mustache", "ApiVersion.cs")
    json_releases templdir destdir

(* ------------------- category: relations *)
and relations = Hashtbl.create 10

and gen_relations () =
  let out_chan = open_out (Filename.concat destdir "Relation.cs") in
  let print format = fprintf out_chan format in
  List.iter process_relations (relations_of_api api) ;
  print
    "%s\n\n\
     using System;\n\
     using System.Collections.Generic;\n\n\
     namespace XenAPI\n\
     {\n\
    \    public partial class Relation\n\
    \    {\n\
    \        public readonly String field;\n\
    \        public readonly String manyType;\n\
    \        public readonly String manyField;\n\n\
    \        public Relation(String field, String manyType, String manyField)\n\
    \        {\n\
    \            this.field = field;\n\
    \            this.manyField = manyField;\n\
    \            this.manyType = manyType;\n\
    \        }\n\n\
    \        public static Dictionary<Type, Relation[]> GetRelations()\n\
    \        {\n\
    \            Dictionary<Type, Relation[]> relations = new Dictionary<Type, \
     Relation[]>();\n\n"
    Licence.bsd_two_clause ;
  Hashtbl.iter (gen_relations_by_type out_chan) relations ;
  print "\n            return relations;\n       }\n    }\n}\n"

and process_relations ((oneClass, oneField), (manyClass, manyField)) =
  let value =
    try (manyField, oneClass, oneField) :: Hashtbl.find relations manyClass
    with Not_found -> [(manyField, oneClass, oneField)]
  in
  Hashtbl.replace relations manyClass value

and gen_relations_by_type out_chan manyClass relations =
  let print format = fprintf out_chan format in
  print "            relations.Add(typeof(%s), new Relation[] {\n"
    (exposed_class_name manyClass) ;

  List.iter (gen_relation out_chan) relations ;

  print "            });\n\n"

and gen_relation out_chan (manyField, oneClass, oneField) =
  let print format = fprintf out_chan format in
  print "                new Relation(\"%s\", \"%s\", \"%s\"),\n" manyField
    oneClass oneField

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
  let delegate_type = function
    | Get ->
        "DataCopiedDelegate"
    | Put ->
        "UpdateProgressDelegate"
    | _ ->
        failwith "Unimplemented HTTP method"
  in
  let delegate_name = function
    | Get ->
        "dataCopiedDelegate"
    | Put ->
        "progressDelegate"
    | _ ->
        failwith "Unimplemented HTTP method"
  in
  let http_method = function
    | Get ->
        "Get"
    | Put ->
        "Put"
    | _ ->
        failwith "Unimplemented HTTP method"
  in
  let action_json (name, (meth, uri, _, sdkargs, _, _)) =
    let enhanced_args =
      [String_query_arg "task_id"; String_query_arg "session_id"] @ sdkargs
    in
    `O
      [
        ("name", `String name)
      ; ("delegate_type", `String (delegate_type meth))
      ; ("delegate_name", `String (delegate_name meth))
      ; ("http_method", `String (http_method meth))
      ; ("uri", `String uri)
      ; ( "sdkargs_decl"
        , `String
            (enhanced_args |> List.map decl_of_sdkarg |> String.concat ", ")
        )
      ; ( "sdkargs"
        , `String (enhanced_args |> List.map use_of_sdkarg |> String.concat ", ")
        )
      ]
  in
  let filtered_actions =
    http_actions |> List.filter (fun (_, (_, _, sdk, _, _, _)) -> sdk)
  in
  `O
    [
      ("licence", `String Licence.bsd_two_clause)
    ; ("http_actions", `A (List.map action_json filtered_actions))
    ]

(* ------------------- category: classes *)
and gen_class_file cls =
  let m = exposed_class_name cls.name in
  if not (List.mem m !api_members) then
    api_members := m :: !api_members ;
  let out_chan =
    open_out (Filename.concat destdir (exposed_class_name cls.name) ^ ".cs")
  in
  finally
    (fun () -> gen_class out_chan cls)
    ~always:(fun () -> close_out out_chan)

and gen_class out_chan cls =
  let print format = fprintf out_chan format in
  let exposed_class_name = exposed_class_name cls.name in
  let messages =
    List.filter
      (fun msg -> String.compare msg.msg_name "get_all_records_where" <> 0)
      cls.messages
  in
  let contents = cls.contents in
  let publishedInfo = get_published_info_class cls in

  print
    "%s\n\n\
     using System;\n\
     using System.Collections;\n\
     using System.Collections.Generic;\n\
     using System.ComponentModel;\n\
     using System.Globalization;\n\
     using System.Linq;\n\
     using Newtonsoft.Json;\n\n\n\
     namespace XenAPI\n\
     {\n\
    \    /// <summary>\n\
    \    /// %s%s\n\
    \    /// </summary>\n\
    \    public partial class %s : XenObject<%s>\n\
    \    {" Licence.bsd_two_clause
    (escape_xml cls.description)
    (if publishedInfo = "" then "" else "\n    /// " ^ publishedInfo)
    exposed_class_name exposed_class_name ;

  print
    "\n\
    \        #region Constructors\n\n\
    \        public %s()\n\
    \        {\n\
    \        }\n"
    exposed_class_name ;

  let print_internal_ctor = function
    | [] ->
        ()
    | cnt ->
        print "\n        public %s(%s)\n        {\n            %s\n        }\n"
          exposed_class_name
          (String.concat ",\n            "
             (List.rev (get_constructor_params cnt))
          )
          (String.concat "\n            " (List.rev (get_constructor_body cnt)))
  in
  print_internal_ctor contents ;

  print
    "\n\
    \        /// <summary>\n\
    \        /// Creates a new %s from a Hashtable.\n\
    \        /// Note that the fields not contained in the Hashtable\n\
    \        /// will be created with their default values.\n\
    \        /// </summary>\n\
    \        /// <param name=\"table\"></param>\n\
    \        public %s(Hashtable table)\n\
    \            : this()\n\
    \        {\n\
    \            UpdateFrom(table);\n\
    \        }\n"
    exposed_class_name exposed_class_name ;

  print "\n        #endregion\n\n" ;

  print
    "        /// <summary>\n\
    \        /// Updates each field of this instance with the value of\n\
    \        /// the corresponding field of a given %s.\n\
    \        /// </summary>\n\
    \        public override void UpdateFrom(%s record)\n\
    \        {\n"
    exposed_class_name exposed_class_name ;

  List.iter (gen_updatefrom_line out_chan) contents ;

  print "        }\n\n" ;

  print
    "        /// <summary>\n\
    \        /// Given a Hashtable with field-value pairs, it updates the \
     fields of this %s\n\
    \        /// with the values listed in the Hashtable. Note that only the \
     fields contained\n\
    \        /// in the Hashtable will be updated and the rest will remain the \
     same.\n\
    \        /// </summary>\n\
    \        /// <param name=\"table\"></param>\n\
    \        public void UpdateFrom(Hashtable table)\n\
    \        {\n"
    exposed_class_name ;

  List.iter (gen_hashtable_constructor_line out_chan) contents ;

  print "        }\n\n" ;

  let is_current_ops = function
    | Field f ->
        full_name f = "current_operations"
    | _ ->
        false
  in
  let current_ops, other_contents = List.partition is_current_ops contents in
  let check_refs =
    "if (ReferenceEquals(null, other))\n\
    \                return false;\n\
    \            if (ReferenceEquals(this, other))\n\
    \                return true;"
  in
  ( match current_ops with
  | [] ->
      print
        "        public bool DeepEquals(%s other)\n\
        \        {\n\
        \            %s\n\n\
        \            return " exposed_class_name check_refs
  | _ ->
      print
        "        public bool DeepEquals(%s other, bool ignoreCurrentOperations)\n\
        \        {\n\
        \            %s\n\n\
        \            if (!ignoreCurrentOperations && \
         !Helper.AreEqual2(current_operations, other.current_operations))\n\
        \                return false;\n\n\
        \            return " exposed_class_name check_refs
  ) ;

  ( match other_contents with
  | [] ->
      print "false"
  | _ ->
      print "%s"
        (String.concat " &&\n                "
           (List.map gen_equals_condition other_contents)
        )
  ) ;

  print
    ";\n\
    \        }\n\n\
    \        public override string SaveChanges(Session session, string \
     opaqueRef, %s server)\n\
    \        {\n\
    \            if (opaqueRef == null)\n\
    \            {" exposed_class_name ;

  if cls.gen_constructor_destructor then
    print
      "\n\
      \                var reference = create(session, this);\n\
      \                return reference == null ? null : reference.opaque_ref;\n"
  else
    print
      "\n\
      \                System.Diagnostics.Debug.Assert(false, \"Cannot create \
       instances of this type on the server\");\n\
      \                return \"\";\n" ;

  print "            }\n            else\n            {\n" ;

  gen_save_changes out_chan exposed_class_name messages contents ;

  print "\n            }\n        }\n" ;

  let gen_exposed_method_overloads cls message =
    let generator x = gen_exposed_method cls message x in
    gen_overloads generator message
  in
  let all_methods =
    messages |> List.map (gen_exposed_method_overloads cls) |> List.concat
  in
  List.iter (print "%s") all_methods ;
  List.iter (gen_exposed_field out_chan cls) contents ;
  print "    }\n}\n"

and get_constructor_params content = get_constructor_params' content []

and get_constructor_params' content elements =
  match content with
  | [] ->
      elements
  | Field fr :: others ->
      get_constructor_params' others
        (sprintf "%s %s" (exposed_type fr.ty) (full_name fr) :: elements)
  | Namespace (_, c) :: others ->
      get_constructor_params' (c @ others) elements

and get_constructor_body content = get_constructor_body' content []

and get_constructor_body' content elements =
  match content with
  | [] ->
      elements
  | Field fr :: others ->
      get_constructor_body' others
        (sprintf "this.%s = %s;" (full_name fr) (full_name fr) :: elements)
  | Namespace (_, c) :: others ->
      get_constructor_body' (c @ others) elements

and gen_hashtable_constructor_line out_chan content =
  let print format = fprintf out_chan format in

  match content with
  | Field fr ->
      print
        "            if (table.ContainsKey(\"%s\"))\n                %s = %s;\n"
        (full_name fr) (full_name fr)
        (convert_from_hashtable (full_name fr) fr.ty)
  | Namespace (_, c) ->
      List.iter (gen_hashtable_constructor_line out_chan) c

and gen_equals_condition content =
  match content with
  | Field fr ->
      sprintf "Helper.AreEqual2(_%s, other._%s)" (full_name fr) (full_name fr)
  | Namespace (_, c) ->
      String.concat " &&\n                " (List.map gen_equals_condition c)

and gen_updatefrom_line out_chan content =
  let print format = fprintf out_chan format in

  match content with
  | Field fr ->
      print "            %s = %s;\n" (full_name fr) ("record." ^ full_name fr)
  | Namespace (_, c) ->
      List.iter (gen_updatefrom_line out_chan) c

and gen_overloads generator message =
  match message.msg_params with
  | [] ->
      [generator []]
  | _ ->
      let paramGroups = gen_param_groups message message.msg_params in
      List.map generator paramGroups

and gen_exposed_method cls msg curParams =
  let classname = cls.name in
  let minimum_allowed_role = get_minimum_allowed_role msg in
  let proxyMsgName = proxy_msg_name classname msg in
  let exposed_ret_type = exposed_type_opt msg.msg_result in
  let paramSignature = exposed_params msg classname curParams in
  let paramsDoc = get_params_doc msg classname curParams in
  let jsonCallParams = exposed_call_params msg classname curParams in
  let publishInfo = get_published_info_message msg cls in
  let deprecatedInfo = get_deprecated_info_message msg in
  let deprecatedAttribute = get_deprecated_attribute msg in
  let deprecatedInfoString =
    if deprecatedInfo = "" then "" else "\n        /// " ^ deprecatedInfo
  in
  let deprecatedAttributeString =
    if deprecatedAttribute = "" then "" else "\n        " ^ deprecatedAttribute
  in
  let sync =
    sprintf
      "\n\
      \        /// <summary>\n\
      \        /// %s%s%s\n\
      \        /// </summary>%s%s\n\
      \        /// <remarks>\n\
      \        /// Minimum allowed role: %s\n\
      \        /// </remarks>\n\
      \        public static %s %s(%s)\n\
      \        {\n\
      \            %s;\n\
      \        }\n"
      msg.msg_doc
      (if publishInfo = "" then "" else "\n        /// " ^ publishInfo)
      deprecatedInfoString paramsDoc deprecatedAttributeString
      minimum_allowed_role exposed_ret_type msg.msg_name paramSignature
      (json_return_opt
         (sprintf "session.JsonRpcClient.%s(%s)" proxyMsgName jsonCallParams)
         msg.msg_result
      )
  in
  let async =
    if msg.msg_async then
      sprintf
        "\n\
        \        /// <summary>\n\
        \        /// %s%s%s\n\
        \        /// </summary>%s%s\n\
        \        /// <remarks>\n\
        \        /// Minimum allowed role: %s\n\
        \        /// </remarks>\n\
        \        public static XenRef<Task> async_%s(%s)\n\
        \        {\n\
        \          return session.JsonRpcClient.async_%s(%s);\n\
        \        }\n"
        msg.msg_doc
        (if publishInfo = "" then "" else "\n        /// " ^ publishInfo)
        deprecatedInfoString paramsDoc deprecatedAttributeString
        minimum_allowed_role msg.msg_name paramSignature proxyMsgName
        jsonCallParams
    else
      ""
  in
  sync ^ async

and get_params_doc msg classname params =
  let sessionDoc =
    "\n        /// <param name=\"session\">The session</param>"
  in
  let refDoc =
    if is_method_static msg then
      ""
    else if msg.msg_name = "get_by_permission" then
      sprintf
        "\n\
        \        /// <param name=\"_%s\">The opaque_ref of the given \
         permission</param>"
        (String.lowercase_ascii classname)
    else if msg.msg_name = "revert" then
      sprintf
        "\n\
        \        /// <param name=\"_%s\">The opaque_ref of the given \
         snapshotted state</param>"
        (String.lowercase_ascii classname)
    else
      sprintf
        "\n\
        \        /// <param name=\"_%s\">The opaque_ref of the given %s</param>"
        (String.lowercase_ascii classname)
        (String.lowercase_ascii classname)
  in
  String.concat ""
    (sessionDoc :: refDoc :: List.map (fun x -> get_param_doc msg x) params)

and get_param_doc msg x =
  let publishInfo = get_published_info_param msg x in
  sprintf "\n        /// <param name=\"_%s\">%s%s</param>"
    (String.lowercase_ascii x.param_name)
    (escape_xml x.param_doc)
    (if publishInfo = "" then "" else " " ^ publishInfo)

and exposed_params message classname params =
  let exposedParams = List.map exposed_param params in
  let refParam = sprintf "string _%s" (String.lowercase_ascii classname) in
  let exposedParams =
    if is_method_static message then
      exposedParams
    else
      refParam :: exposedParams
  in
  String.concat ", " ("Session session" :: exposedParams)

and exposed_param p =
  sprintf "%s _%s"
    (internal_type p.param_type)
    (String.lowercase_ascii p.param_name)

and exposed_call_params message classname params =
  let exposed_call_param p =
    let pName = String.lowercase_ascii p.param_name in
    sprintf "_%s" pName
  in
  let exposedParams = List.map exposed_call_param params in
  let name = String.lowercase_ascii classname in
  let refParam = sprintf "_%s" name in
  let exposedParams =
    if is_method_static message then
      exposedParams
    else
      refParam :: exposedParams
  in
  String.concat ", " ("session.opaque_ref" :: exposedParams)

(* 'messages' are methods, 'contents' are fields *)
and gen_save_changes out_chan exposed_class_name messages contents =
  let fields = List.flatten (List.map flatten_content contents) in
  let fields2 =
    List.filter
      (fun fr -> fr.qualifier == RW && not (List.mem "public" fr.full_name))
      fields
  in
  (* Find all StaticRO fields which have corresponding messages (methods) of the form set_readonlyField *)
  let readonlyFieldsWithSetters =
    List.filter
      (fun field ->
        field.qualifier == StaticRO
        && List.exists
             (fun msg ->
               msg.msg_name = String.concat "" ["set_"; full_name field]
             )
             messages
      )
      fields
  in
  let length = List.length fields2 + List.length readonlyFieldsWithSetters in
  let print format = fprintf out_chan format in
  if length == 0 then
    print
      "              throw new InvalidOperationException(\"This type has no \
       read/write properties\");"
  else (
    List.iter (gen_save_changes_to_field out_chan exposed_class_name) fields2 ;
    (* Generate calls to any set_ methods *)
    List.iter
      (gen_save_changes_to_field out_chan exposed_class_name)
      readonlyFieldsWithSetters ;
    print "\n                return null;"
  )

and flatten_content content =
  match content with
  | Field fr ->
      [fr]
  | Namespace (_, c) ->
      List.flatten (List.map flatten_content c)

and gen_save_changes_to_field out_chan exposed_class_name fr =
  let print format = fprintf out_chan format in
  let full_name_fr = full_name fr in
  let equality =
    (* Use AreEqual2 - see CA-19220 *)
    sprintf "Helper.AreEqual2(_%s, server._%s)" full_name_fr full_name_fr
  in
  print
    "                if (!%s)\n\
    \                {\n\
    \                    %s.set_%s(session, opaqueRef, _%s);\n\
    \                }\n"
    equality exposed_class_name full_name_fr full_name_fr

and gen_exposed_field out_chan cls content =
  match content with
  | Field fr ->
      let print format = fprintf out_chan format in
      let full_name_fr = full_name fr in
      let comp = sprintf "!Helper.AreEqual(value, _%s)" full_name_fr in
      let publishInfo = get_published_info_field fr cls in

      print
        "\n\
        \        /// <summary>\n\
        \        /// %s%s\n\
        \        /// </summary>%s\n\
        \        public virtual %s %s\n\
        \        {\n\
        \            get { return _%s; }"
        (escape_xml fr.field_description)
        (if publishInfo = "" then "" else "\n        /// " ^ publishInfo)
        (json_serialization_attr fr)
        (exposed_type fr.ty) full_name_fr full_name_fr ;

      print
        "\n\
        \            set\n\
        \            {\n\
        \                if (%s)\n\
        \                {\n\
        \                    _%s = value;\n\
        \                    NotifyPropertyChanged(\"%s\");\n\
        \                }\n\
        \            }\n\
        \        }" comp full_name_fr full_name_fr ;

      print "\n        private %s _%s%s;\n" (exposed_type fr.ty) full_name_fr
        (get_default_value_opt fr)
  | Namespace (_, c) ->
      List.iter (gen_exposed_field out_chan cls) c

and gen_proxy protocol =
  let all_methods =
    classes |> List.map gen_proxy_class_methods |> List.concat
  in
  match protocol with
  | CommonFunctions.JsonRpc ->
      let json_method x = `O [("client_method", `String x)] in
      `O [("client_methods", `A (List.map json_method all_methods))]
  | _ ->
      raise Unknown_wire_protocol

and gen_proxy_class_methods {name; messages; _} =
  let gen_message_overloads name message =
    let generator params = gen_proxy_method name message params in
    gen_overloads generator message
  in
  messages |> List.map (gen_message_overloads name) |> List.concat

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
        \        }" proxy_msg_name paramsJsonWithTypes
        (String.concat ", " async_converters)
        classname message.msg_name paramsJsonNoTypes
    else
      ""
  in
  sync ^ async

and proxy_params ~with_types message classname params =
  let refParam =
    sprintf
      (if with_types then "string _%s" else "_%s ?? \"\"")
      (String.lowercase_ascii classname)
  in
  let args = List.map (proxy_param ~with_types) params in
  let args = if is_method_static message then args else refParam :: args in
  let args =
    if message.msg_session then
      (if with_types then "string session" else "session") :: args
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
  let out_chan = open_out (Filename.concat destdir "Maps.cs") in
  finally (fun () -> gen_maps' out_chan) ~always:(fun () -> close_out out_chan)

and gen_maps' out_chan =
  let print format = fprintf out_chan format in

  print
    "%s\n\n\
     using System;\n\
     using System.Collections;\n\
     using System.Collections.Generic;\n\n\
    \     namespace XenAPI\n\
     {\n\
    \    internal class Maps\n\
    \    {" Licence.bsd_two_clause ;

  TypeSet.iter (gen_map_conversion out_chan) !maps ;

  print "\n    }\n}\n"

and gen_map_conversion out_chan = function
  | Map (l, r) ->
      let print format = fprintf out_chan format in
      let el = exposed_type l in
      let el_literal = exposed_type_as_literal l in
      let er = exposed_type r in
      let er_literal = exposed_type_as_literal r in

      print
        "\n\
        \        internal static Dictionary<%s, %s> \
         ToDictionary_%s_%s(Hashtable table)\n\
        \        {\n\
        \            Dictionary<%s, %s> result = new Dictionary<%s, %s>();\n\
        \            if (table != null)\n\
        \            {\n\
        \                foreach (string key in table.Keys)\n\
        \                {\n\
        \                    try\n\
        \                    {\n\
        \                        %s k = %s;\n\
        \                        %s v = %s;\n\
        \                        result[k] = v;\n\
        \                    }\n\
        \                    catch\n\
        \                    {\n\
        \                       // continue\n\
        \                    }\n\
        \                }\n\
        \            }\n\
        \            return result;\n\
        \        }\n\n"
        el er
        (sanitise_function_name el_literal)
        (sanitise_function_name er_literal)
        el er el er el
        (simple_convert_from_proxy "key" l)
        er
        (convert_from_proxy_hashtable_value "table[key]" r)
  (***)
  | _ ->
      assert false

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
  (*function*)
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
      sprintf "\n        [JsonConverter(typeof(XenDateTimeConverter))]"
  | Enum (name, _) ->
      sprintf "\n        [JsonConverter(typeof(%sConverter))]" name
  | Ref name ->
      sprintf "\n        [JsonConverter(typeof(XenRefConverter<%s>))]"
        (exposed_class_name name)
  | Set (Ref name) ->
      sprintf "\n        [JsonConverter(typeof(XenRefListConverter<%s>))]"
        (exposed_class_name name)
  | Map (Ref u, Record _) ->
      sprintf "\n        [JsonConverter(typeof(XenRefObjectMapConverter<%s>))]"
        (exposed_class_name u)
  | Map (Ref u, Ref v) ->
      sprintf
        "\n        [JsonConverter(typeof(XenRefXenRefMapConverter<%s, %s>))]"
        (exposed_class_name u) (exposed_class_name v)
  | Map (Ref u, Int) ->
      sprintf "\n        [JsonConverter(typeof(XenRefLongMapConverter<%s>))]"
        (exposed_class_name u)
  | Map (Ref u, String) ->
      sprintf "\n        [JsonConverter(typeof(XenRefStringMapConverter<%s>))]"
        (exposed_class_name u)
  | Map (String, Ref v) ->
      sprintf "\n        [JsonConverter(typeof(StringXenRefMapConverter<%s>))]"
        (exposed_class_name v)
  | Map (String, String) ->
      sprintf "\n        [JsonConverter(typeof(StringStringMapConverter))]"
  | Map (Ref _, _) | Map (_, Ref _) ->
      failwith (sprintf "Need converter for %s" fr.field_name)
  | _ ->
      ""

and get_default_value_opt field =
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
            (Date.to_string y)
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
        if y = "" then ["Helper.NullOpaqueRef"] else [sprintf "\"%s\"" y]
    | VCustom (_, y) ->
        get_default_value y
  in
  match field.default_value with
  | Some y ->
      get_default_value_per_type field.ty (get_default_value y)
  | None ->
      get_default_value_per_type field.ty []

and get_default_value_per_type ty thing =
  match ty with
  | DateTime | Int | Bool | Float ->
      if thing = [] then "" else sprintf " = %s" (String.concat ", " thing)
  | Ref _ ->
      sprintf " = new %s(%s)" (exposed_type ty)
        (if thing = [] then "Helper.NullOpaqueRef" else String.concat ", " thing)
  | SecretString | String ->
      sprintf " = %s" (if thing = [] then "\"\"" else String.concat ", " thing)
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
      if thing = [] then "" else get_default_value_per_type x thing

let _ = main ()
