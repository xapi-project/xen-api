(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

(* Generator of C bindings from the datamodel *)

open Printf
open Datamodel_types
open Datamodel_utils
open Dm_api
open CommonFunctions

module TypeSet = Set.Make (struct
  type t = Datamodel_types.ty

  let compare = compare
end)

let destdir = "autogen"

let templates_dir = "templates"

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
    && msg.msg_name <> "get"
    && msg.msg_name <> "get_data_sources"
  in
  filter obj_filter field_filter message_filter
    (Datamodel_utils.add_implicit_messages ~document_order:false
       (filter obj_filter field_filter message_filter Datamodel.all_api)
    )

let classes = objects_of_api api

module StringSet = Set.Make (struct
  type t = string

  let compare = String.compare
end)

let enums = ref TypeSet.empty

let maps = ref TypeSet.empty

let enum_maps = ref TypeSet.empty

let all_headers = ref []

let joined sep f l =
  let r = List.map f l in
  String.concat sep (List.filter (fun x -> String.compare x "" != 0) r)

let rec main () =
  let include_dir = Filename.concat destdir "include" in
  let src_dir = Filename.concat destdir "src" in

  gen_failure_h () ;
  gen_failure_c () ;

  let filtered_classes =
    List.filter
      (fun x -> not (List.mem x.name ["session"; "debug"; "data_source"]))
      classes
  in
  List.iter
    (fun x ->
      ( gen_class write_predecl predecl_filename x include_dir ;
        gen_class write_decl decl_filename x include_dir ;
        gen_class write_impl impl_filename x
      )
        src_dir
    )
    filtered_classes ;

  all_headers := List.map (fun x -> x.name) filtered_classes ;

  TypeSet.iter (gen_enum write_enum_decl decl_filename include_dir) !enums ;
  TypeSet.iter (gen_enum write_enum_impl impl_filename src_dir) !enums ;
  TypeSet.iter
    (gen_enum write_enum_internal_decl internal_decl_filename include_dir)
    !enums ;

  maps := TypeSet.add (Map (String, Int)) !maps ;
  maps := TypeSet.add (Map (Int, Int)) !maps ;
  maps := TypeSet.add (Map (String, Set String)) !maps ;
  maps := TypeSet.add (Map (String, Map (String, String))) !maps ;
  TypeSet.iter (gen_map write_map_decl decl_filename include_dir) !maps ;
  TypeSet.iter (gen_map write_map_impl impl_filename src_dir) !maps ;

  TypeSet.iter
    (gen_map write_enum_map_internal_decl internal_decl_filename include_dir)
    !enum_maps ;

  let class_records =
    filtered_classes
    |> List.map (fun {name; _} -> record_typename name)
    |> List.sort String.compare
  in
  let json1 =
    `O
      [
        ( "api_class_records"
        , `A
            (List.map
               (fun x -> `O [("api_class_record", `String x)])
               class_records
            )
        )
      ]
  in
  render_file
    ("xen_internal.mustache", "include/xen_internal.h")
    json1 templates_dir destdir ;

  let sorted_headers =
    List.sort String.compare (List.map decl_filename !all_headers)
  in
  let json2 =
    `O
      [
        ( "api_headers"
        , `A (List.map (fun x -> `O [("api_header", `String x)]) sorted_headers)
        )
      ]
  in
  render_file
    ("xen_all.h.mustache", "include/xen/api/xen_all.h")
    json2 templates_dir destdir

and gen_class f g clas targetdir =
  let out_chan = open_out (Filename.concat targetdir (g clas.name)) in
  Fun.protect (fun () -> f clas out_chan) ~finally:(fun () -> close_out out_chan)

and gen_enum f g targetdir = function
  | Enum (name, _) as x ->
      if not (List.mem name !all_headers) then
        all_headers := name :: !all_headers ;
      let out_chan = open_out (Filename.concat targetdir (g name)) in
      Fun.protect
        (fun () -> f x out_chan)
        ~finally:(fun () -> close_out out_chan)
  | _ ->
      assert false

and gen_map f g targetdir = function
  | Map (l, r) ->
      let name = mapname l r in
      if not (List.mem name !all_headers) then
        all_headers := name :: !all_headers ;
      let out_chan = open_out (Filename.concat targetdir (g name)) in
      Fun.protect
        (fun () -> f name l r out_chan)
        ~finally:(fun () -> close_out out_chan)
  | _ ->
      assert false

and write_predecl {name= classname; _} out_chan =
  let print format = fprintf out_chan format in
  let protect = protector (classname ^ "_decl") in
  let tn = typename classname in
  let record_tn = record_typename classname in
  let record_opt_tn = record_opt_typename classname in

  print_h_header out_chan protect ;

  if classname <> "event" then (
    print "typedef void *%s;\n\n" tn ;
    print "%s\n" (predecl_set tn)
  ) ;
  print "%s\n" (predecl record_tn) ;
  print "%s\n" (predecl_set record_tn) ;
  if classname <> "event" then (
    print "%s\n" (predecl record_opt_tn) ;
    print "%s\n" (predecl_set record_opt_tn)
  ) ;
  print_h_footer out_chan

and write_decl {name= classname; contents; description; messages; _} out_chan =
  let print format = fprintf out_chan format in
  let protect = protector classname in
  let tn = typename classname in
  let record_tn = record_typename classname in
  let record_opt_tn = record_opt_typename classname in
  let class_has_refs = true (* !!! *) in
  let needed = ref (StringSet.add (classname ^ "_decl") StringSet.empty) in
  let record = decl_record needed tn record_tn contents in
  let record_opt = decl_record_opt tn record_tn record_opt_tn in
  let message_decls =
    decl_messages needed classname
      (List.filter
         (fun x -> not (classname = "event" && x.msg_name = "from"))
         messages
      )
  in
  let full_stop =
    if Astring.String.is_suffix ~affix:"." description then "" else "."
  in

  let rec get_needed x =
    match x with
    | Field fr ->
        find_needed'' needed fr.ty
    | Namespace (_, cs) ->
        List.iter get_needed cs
  in
  List.iter get_needed contents ;

  print_h_header out_chan protect ;
  print "%s\n" (hash_includes !needed) ;

  print "\n\n%s\n\n\n"
    (Helper.comment false
       (sprintf "The %s class.\n\n%s%s" classname description full_stop)
    ) ;

  if classname <> "event" then (
    print "%s\n\n"
      (decl_free tn (String.lowercase_ascii classname) false "handle") ;
    print "%s\n" (decl_set tn false)
  ) ;
  print "%s\n" record ;
  if classname <> "event" then
    print "%s\n" record_opt ;
  print "%s\n\n" (decl_set record_tn class_has_refs) ;
  if classname <> "event" then
    print "%s\n\n" (decl_set record_opt_tn true) ;
  print "%s\n" message_decls ;
  print_h_footer out_chan

and predecl_set tn = predecl (tn ^ "_set")

and predecl tn = sprintf "struct %s;" tn

and decl_set tn referenced =
  let alloc_com =
    Helper.comment true (sprintf "Allocate a %s_set of the given size." tn)
  in

  sprintf
    "\n\
     typedef struct %s_set\n\
     {\n\
    \    size_t size;\n\
    \    %s *contents[];\n\
     } %s_set;\n\n\
     %s\n\
     extern %s_set *\n\
     %s_set_alloc(size_t size);\n\n\
     %s\n"
    tn tn tn alloc_com tn tn
    (decl_free (sprintf "%s_set" tn) "*set" referenced "set")

and decl_free tn cn referenced thing =
  let com =
    Helper.comment true
      (sprintf
         "Free the given %s%s.  The given %s must have been allocated by this \
          library."
         tn
         (if referenced then ", and all referenced values" else "")
         thing
      )
  in

  sprintf "%s\nextern void\n%s_free(%s %s);" com tn tn cn

and decl_record needed tn record_tn contents =
  sprintf
    "\n\
     typedef struct %s\n\
     {\n\
     %s    %s\n\
     } %s;\n\n\
     %s\n\
     extern %s *\n\
     %s_alloc(void);\n\n\
     %s\n"
    record_tn
    (if tn <> "xen_event" then sprintf "    %s handle;\n" tn else "")
    (record_fields contents needed)
    record_tn
    (Helper.comment true (sprintf "Allocate a %s." record_tn))
    record_tn record_tn
    (decl_free record_tn "*record" true "record")

and decl_record_opt tn record_tn record_opt_tn =
  sprintf
    "\n\
     typedef struct %s\n\
     {\n\
    \    bool is_record;\n\
    \    union\n\
    \    {\n\
    \        %s handle;\n\
    \        %s *record;\n\
    \    } u;\n\
     } %s;\n\n\
     %s\n\
     extern %s *\n\
     %s_alloc(void);\n\n\
     %s\n"
    record_opt_tn tn record_tn record_opt_tn
    (Helper.comment true (sprintf "Allocate a %s." record_opt_tn))
    record_opt_tn record_opt_tn
    (decl_free record_opt_tn "*record_opt" true "record_opt")

and record_fields contents needed =
  joined "\n    " (record_field needed "") contents

and record_field needed prefix content =
  match content with
  | Field fr ->
      sprintf "%s%s%s;"
        (c_type_of_ty needed true fr.ty)
        prefix (fieldname fr.field_name)
  | Namespace (p, c) ->
      joined "\n    " (record_field needed (prefix ^ fieldname p ^ "_")) c

and decl_messages needed classname messages =
  joined "\n\n" (decl_message needed classname) messages

and decl_message needed classname message =
  let message_sig = message_signature needed classname message in
  let messageAsyncVersion = decl_message_async needed classname message in
  sprintf "%s\n%sextern %s;\n%s"
    (get_message_comment message)
    (get_deprecated_message message)
    message_sig messageAsyncVersion

and decl_message_async needed classname message =
  if message.msg_async then (
    let messageSigAsync = message_signature_async needed classname message in
    needed := StringSet.add "task_decl" !needed ;
    sprintf "\n%s\n%sextern %s;\n"
      (get_message_comment message)
      (get_deprecated_message message)
      messageSigAsync
  ) else
    ""

and get_message_comment message =
  let full_stop =
    if Astring.String.is_suffix ~affix:"." message.msg_doc then "" else "."
  in
  let minimum_allowed_role = get_minimum_allowed_role message in
  let content =
    sprintf "%s%s\nMinimum allowed role: %s." message.msg_doc full_stop
      minimum_allowed_role
  in
  Helper.comment true content

and impl_messages needed classname messages =
  joined "\n\n" (impl_message needed classname) messages

and impl_message needed classname message =
  let message_sig = message_signature needed classname message in
  let param_count = List.length message.msg_params in

  let param_decl, param_call =
    if param_count = 0 then
      ("", "NULL")
    else
      let param_pieces = abstract_params message.msg_params in

      ( sprintf
          "    abstract_value param_values[] =\n\
          \        {\n\
          \            %s\n\
          \        };\n"
          param_pieces
      , "param_values"
      )
  in

  let result_bits =
    match message.msg_result with
    | Some res ->
        abstract_result_handling classname message.msg_name param_count res
    | None ->
        sprintf
          "    xen_call_(session, \"%s.%s\", %s, %d, NULL, NULL);\n\
          \    return session->ok;\n"
          classname message.msg_name param_call param_count
  in

  let messageAsyncImpl = impl_message_async needed classname message in
  sprintf "%s%s\n{\n%s\n%s}\n%s"
    (get_deprecated_message message)
    message_sig param_decl result_bits messageAsyncImpl

and impl_message_async needed classname message =
  if message.msg_async then
    let messageSigAsync = message_signature_async needed classname message in
    let param_count = List.length message.msg_params in

    let param_decl, _ =
      if param_count = 0 then
        ("", "NULL")
      else
        let param_pieces = abstract_params message.msg_params in

        ( sprintf
            "    abstract_value param_values[] =\n\
            \        {\n\
            \            %s\n\
            \        };\n"
            param_pieces
        , "param_values"
        )
    in

    let result_bits =
      abstract_result_handling_async classname message.msg_name param_count
    in
    sprintf "\n%s%s\n{\n%s\n%s}"
      (get_deprecated_message message)
      messageSigAsync param_decl result_bits
  else
    ""

and abstract_params params = joined ",\n            " abstract_param params

and abstract_param p =
  let ab_typ = abstract_type false p.param_type in
  sprintf "{ .type = &%s,\n              .u.%s_val = %s }" ab_typ
    (abstract_member p.param_type)
    (abstract_param_conv p.param_name p.param_type)

and abstract_param_conv name = function
  | Set _ | Map _ ->
      sprintf "(arbitrary_set *)%s" (paramname name)
  | Ref "session" ->
      sprintf "%s->session_id" (paramname name)
  | _ ->
      paramname name

and abstract_member = function
  | SecretString | String | Ref _ ->
      "string"
  | Enum _ ->
      "enum"
  | Int ->
      "int"
  | Float ->
      "float"
  | Bool ->
      "bool"
  | DateTime ->
      "datetime"
  | Set _ ->
      "set"
  | Map _ ->
      "set"
  | Record _ ->
      "struct"
  | x ->
      eprintf "%s" (Types.to_string x) ;
      assert false

and abstract_result_handling classname msg_name param_count = function
  | typ, _ -> (
      let call =
        if param_count = 0 then
          sprintf
            "xen_call_(session, \"%s.%s\", NULL, 0, &result_type, result);"
            classname msg_name
        else
          sprintf "XEN_CALL_(\"%s.%s\");" classname msg_name
      in

      match typ with
      | String | Ref _ | Int | Float | Bool | DateTime | Set _ | Map _ ->
          sprintf "%s\n\n%s    %s\n    return session->ok;\n"
            (abstract_result_type typ) (initialiser_of_ty typ) call
      | Record n ->
          let record_tn = record_typename n in
          sprintf
            "    abstract_type result_type = %s_abstract_type_;\n\n\
             %s    %s\n\n\
            \    if (session->ok)\n\
            \    {\n\
            \       (*result)->handle = xen_strdup_((*result)->uuid);\n\
            \    }\n\n\
            \    return session->ok;\n"
            record_tn
            (initialiser_of_ty (Record n))
            call
      | Enum (_, _) ->
          sprintf "%s\n    %s\n    return session->ok;\n"
            (abstract_result_type typ) call
      | x ->
          eprintf "%s" (Types.to_string x) ;
          assert false
    )

and abstract_result_handling_async classname msg_name param_count =
  let call =
    if param_count = 0 then
      sprintf
        "xen_call_(session, \"Async.%s.%s\", NULL, 0, &result_type, result);"
        classname msg_name
    else
      sprintf "XEN_CALL_(\"Async.%s.%s\");" classname msg_name
  in
  sprintf
    "    abstract_type result_type = abstract_type_string;\n\n\
    \    *result = NULL;\n\
    \    %s\n\
    \    return session->ok;\n"
    call

and abstract_record_field classname prefix prefix_caps content =
  match content with
  | Field fr ->
      let fn = fieldname fr.field_name in
      sprintf
        "{ .key = \"%s%s\",\n\
        \          .type = &%s,\n\
        \          .offset = offsetof(%s, %s%s) }" prefix_caps fr.field_name
        (abstract_type true fr.ty)
        (record_typename classname)
        prefix fn
  | Namespace (p, c) ->
      joined ",\n        "
        (abstract_record_field classname
           (prefix ^ fieldname p ^ "_")
           (prefix_caps ^ p ^ "_")
        )
        c

and abstract_result_type typ =
  let ab_typ = abstract_type false typ in
  sprintf "    abstract_type result_type = %s;" ab_typ

and abstract_type record = function
  | SecretString | String ->
      "abstract_type_string"
  | Enum (n, _) ->
      sprintf "%s_abstract_type_" (typename n)
  | Ref _ ->
      if record then
        "abstract_type_ref"
      else
        "abstract_type_string"
  | Int ->
      "abstract_type_int"
  | Float ->
      "abstract_type_float"
  | Bool ->
      "abstract_type_bool"
  | DateTime ->
      "abstract_type_datetime"
  | Set (Enum (n, _)) ->
      sprintf "%s_set_abstract_type_" (typename n)
  | Set (Record n) ->
      sprintf "%s_set_abstract_type_" (record_typename n)
  | Set memtype ->
      abstract_type record memtype ^ "_set"
  | Map (Ref _, Ref _) ->
      if record then
        "abstract_type_string_ref_map"
      else
        "abstract_type_string_string_map"
  | Map (Ref _, r) ->
      sprintf "abstract_type_string_%s_map" (name_of_ty r)
  | Map (l, Ref _) ->
      if record then
        sprintf "abstract_type_%s_ref_map" (name_of_ty l)
      else
        sprintf "abstract_type_%s_string_map" (name_of_ty l)
  | Map ((Enum (_, _) as l), r) ->
      mapname l r ^ "_abstract_type_"
  | Map (l, (Enum (_, _) as r)) ->
      mapname l r ^ "_abstract_type_"
  | Map (l, r) ->
      sprintf "abstract_type_" ^ mapname l r
  | Record n ->
      sprintf "%s_abstract_type_" (record_typename n)
  | Option n ->
      abstract_type record n

and get_deprecated_message message =
  let deprecatedMessage = get_deprecated_info_message message in
  if deprecatedMessage = "" then
    sprintf ""
  else
    sprintf "/* " ^ deprecatedMessage ^ " */\n"

and message_signature needed classname message =
  let front =
    {
      param_type= Ref "session"
    ; param_name= "session"
    ; param_doc= ""
    ; param_release= message.msg_release
    ; param_default= None
    }
    ::
    ( match message.msg_result with
    | Some res ->
        [
          {
            param_type= fst res
          ; param_name= "*result"
          ; param_doc= ""
          ; param_release= message.msg_release
          ; param_default= None
          }
        ]
    | None ->
        []
    )
  in
  let params = joined ", " (param needed) (front @ message.msg_params) in
  sprintf "bool\n%s(%s)" (messagename classname message.msg_name) params

and message_signature_async needed classname message =
  let sessionParam =
    {
      param_type= Ref "session"
    ; param_name= "session"
    ; param_doc= ""
    ; param_release= message.msg_release
    ; param_default= None
    }
  in
  let taskParam =
    {
      param_type= Ref "task"
    ; param_name= "*result"
    ; param_doc= ""
    ; param_release= message.msg_release
    ; param_default= None
    }
  in
  let params =
    joined ", " (param needed) (sessionParam :: taskParam :: message.msg_params)
  in
  sprintf "bool\n%s(%s)" (messagename_async classname message.msg_name) params

and param needed p =
  let t = p.param_type in
  let n = p.param_name in
  sprintf "%s%s" (c_type_of_ty needed false t) (paramname n)

and hash_includes needed =
  String.concat "\n"
    (List.sort String.compare
       (List.filter
          (function s -> s <> "")
          (List.map hash_include ("common" :: StringSet.elements needed))
       )
    )

and hash_include n =
  if Astring.String.is_suffix ~affix:"internal" n then
    sprintf "#include \"%s\"" (decl_filename n)
  else if n = "session" then
    ""
  else
    sprintf "#include <%s>" (decl_filename n)

and write_enum_decl x out_chan =
  match x with
  | Enum (name, contents) ->
      let print format = fprintf out_chan format in
      let protect = protector name in
      let tn = typename name in

      print_h_header out_chan protect ;

      print
        "\n\
         %s\n\n\n\
         enum %s\n\
         {\n\
         %s\n\
         };\n\n\n\
         typedef struct %s_set\n\
         {\n\
        \    size_t size;\n\
        \    enum %s contents[];\n\
         } %s_set;\n\n\
         %s\n\
         extern %s_set *\n\
         %s_set_alloc(size_t size);\n\n\
         %s\n\n\n\
         %s\n\
         extern const char *\n\
         %s_to_string(enum %s val);\n\n\n\
         %s\n\
         extern enum %s\n\
         %s_from_string(xen_session *session, const char *str);\n\n"
        (hash_include "common") tn
        (joined ",\n\n" (enum_entry name)
           (contents
           @ [("undefined", "Unknown to this version of the bindings.")]
           )
        )
        tn tn tn
        (Helper.comment true (sprintf "Allocate a %s_set of the given size." tn))
        tn tn
        (decl_free (sprintf "%s_set" tn) "*set" false "set")
        (Helper.comment true
           "Return the name corresponding to the given code.  This string must \
            not be modified or freed."
        )
        tn tn
        (Helper.comment true
           "Return the correct code for the given string, or set the session \
            object to failure and return an undefined value if the given \
            string does not match a known code."
        )
        tn tn ;

      print_h_footer out_chan
  | _ ->
      ()

and enum_entry enum_name = function
  | n, c ->
      sprintf "%s\n    XEN_%s_%s"
        (Helper.comment true ~indent:4 c)
        (String.uppercase_ascii enum_name)
        (Astring.String.map
           (fun x -> match x with '-' -> '_' | _ -> x)
           (String.uppercase_ascii n)
        )

and write_enum_impl x out_chan =
  match x with
  | Enum (name, contents) ->
      let print format = fprintf out_chan format in
      let tn = typename name in

      print
        "%s\n\n\
         #include <string.h>\n\n\
         %s\n\
         %s\n\
         %s\n\n\n\
         /*\n\
        \ * Maintain this in the same order as the enum declaration!\n\
        \ */\n\
         static const char *lookup_table[] =\n\
         {\n\
         %s\n\
         };\n\n\n\
         extern %s_set *\n\
         %s_set_alloc(size_t size)\n\
         {\n\
        \    return calloc(1, sizeof(%s_set) +\n\
        \                  size * sizeof(enum %s));\n\
         }\n\n\n\
         extern void\n\
         %s_set_free(%s_set *set)\n\
         {\n\
        \    free(set);\n\
         }\n\n\n\
         const char *\n\
         %s_to_string(enum %s val)\n\
         {\n\
        \    return lookup_table[val];\n\
         }\n\n\n\
         extern enum %s\n\
         %s_from_string(xen_session *session, const char *str)\n\
         {\n\
        \    (void)session;\n\
        \    return ENUM_LOOKUP(str, lookup_table);\n\
         }\n\n\n\
         const abstract_type %s_abstract_type_ =\n\
        \    {\n\
        \        .XEN_API_TYPE = ENUM,\n\
        \        .enum_marshaller =\n\
        \             (const char *(*)(int))&%s_to_string,\n\
        \        .enum_demarshaller =\n\
        \             (int (*)(xen_session *, const char *))&%s_from_string\n\
        \    };\n\n\n"
        Licence.bsd_two_clause (hash_include "internal") (hash_include name)
        (hash_include (name ^ "_internal"))
        (enum_lookup_entries (contents @ [("undefined", "")]))
        tn tn tn tn tn tn tn tn tn tn tn tn tn ;

      if name <> "event_operation" then
        print
          "const abstract_type %s_set_abstract_type_ =\n\
          \    {\n\
          \        .XEN_API_TYPE = SET,\n\
          \        .child = &%s_abstract_type_\n\
          \    };\n\n\n"
          tn tn
  | _ ->
      ()

and enum_lookup_entries contents = joined ",\n" enum_lookup_entry contents

and enum_lookup_entry = function n, _ -> sprintf "    \"%s\"" n

and write_enum_internal_decl x out_chan =
  match x with
  | Enum (name, _) ->
      let print format = fprintf out_chan format in
      let protect = protector (sprintf "%s_internal" name) in
      let tn = typename name in

      let set_abstract_type =
        if name = "event_operations" then
          ""
        else
          sprintf "extern const abstract_type %s_set_abstract_type_;\n" tn
      in

      print
        "%s\n\n\n\
         %s\n\n\n\
         #ifndef %s\n\
         #define %s\n\n\n\
         %s\n\n\n\
         extern const abstract_type %s_abstract_type_;\n\
         %s\n\n\
         #endif\n"
        Licence.bsd_two_clause
        (Helper.comment false
           (sprintf
              "Declarations of the abstract types used during demarshalling of \
               enum %s.  Internal to this library -- do not use from outside."
              tn
           )
        )
        protect protect (hash_include "internal") tn set_abstract_type
  | _ ->
      ()

and write_map_decl name l r out_chan =
  let print format = fprintf out_chan format in
  let tn = typename name in
  let protect = protector name in
  let needed = ref StringSet.empty in
  let alloc_com =
    Helper.comment true (sprintf "Allocate a %s of the given size." tn)
  in

  print_h_header out_chan protect ;
  print
    "\n\
     %s%s%s\n\n\n\
     typedef struct %s_contents\n\
     {\n\
    \  %skey;\n\
    \  %sval;\n\
     } %s_contents;\n\n\n\
     typedef struct %s\n\
     {\n\
    \    size_t size;\n\
    \    %s_contents contents[];\n\
     } %s;\n\n\
     %s\n\
     extern %s *\n\
     %s_alloc(size_t size);\n\n\
     %s\n\n"
    (hash_include "common") (hash_include_enum l) (hash_include_enum r) tn
    (c_type_of_ty needed false l)
    (c_type_of_ty needed true r)
    tn tn tn tn alloc_com tn tn
    (decl_free tn "*map" true "map") ;
  print_h_footer out_chan

and write_map_impl name l r out_chan =
  let print format = fprintf out_chan format in
  let tn = typename name in
  let l_free_impl = free_impl "map->contents[i].key" false l in
  let r_free_impl = free_impl "map->contents[i].val" true r in
  let needed = ref StringSet.empty in
  find_needed'' needed l ;
  find_needed'' needed r ;
  needed := StringSet.add "internal" !needed ;
  needed := StringSet.add name !needed ;
  ( match r with
  | Set String ->
      needed := StringSet.add "string_set" !needed
  | _ ->
      ()
  ) ;

  print
    "%s\n\n\n\
     %s\n\n\n\
     %s *\n\
     %s_alloc(size_t size)\n\
     {\n\
    \    %s *result = calloc(1, sizeof(%s) +\n\
    \    %s                  size * sizeof(struct %s_contents));\n\
    \    result->size = size;\n\
    \    return result;\n\
     }\n\n\n\
     void\n\
     %s_free(%s *map)\n\
     {\n"
    Licence.bsd_two_clause (hash_includes !needed) tn tn tn tn
    (String.make (String.length tn) ' ')
    tn tn tn ;

  if String.compare l_free_impl "" != 0 || String.compare r_free_impl "" != 0
  then
    print
      "    if (map == NULL)\n\
      \    {\n\
      \        return;\n\
      \    }\n\n\
      \    size_t n = map->size;\n\
      \    for (size_t i = 0; i < n; i++)\n\
      \    {\n\
      \        %s\n\
      \        %s\n\
      \    }\n\n"
      l_free_impl r_free_impl ;

  print "    free(map);\n}\n" ;

  match (l, r) with
  | Enum (_, _), _ ->
      gen_enum_map_abstract_type print l r
  | _, Enum (_, _) ->
      gen_enum_map_abstract_type print l r
  | _ ->
      ()

and gen_enum_map_abstract_type print l r =
  let tn = mapname l r in
  print
    "\n\n\
     static const struct_member %s_struct_members[] =\n\
    \    {\n\
    \        { .type = &%s,\n\
    \          .offset = offsetof(xen_%s_contents, key) },\n\
    \        { .type = &%s,\n\
    \          .offset = offsetof(xen_%s_contents, val) },\n\
    \    };\n\n\
     const abstract_type %s_abstract_type_ =\n\
    \    {\n\
    \       .XEN_API_TYPE = MAP,\n\
    \       .struct_size = sizeof(%s_struct_members),\n\
    \       .member_count =\n\
    \           sizeof(%s_struct_members) / sizeof(struct_member),\n\
    \       .members = %s_struct_members\n\
    \    };\n"
    tn (abstract_type false l) tn (abstract_type false r) tn tn tn tn tn

and write_enum_map_internal_decl name l r out_chan =
  let print format = fprintf out_chan format in
  let protect = protector (sprintf "%s_internal" name) in

  print_h_header out_chan protect ;
  print "\nextern const abstract_type %s_abstract_type_;\n\n" (mapname l r) ;
  print_h_footer out_chan

and hash_include_enum = function
  | Enum (x, _) ->
      "\n" ^ hash_include x
  | _ ->
      ""

and gen_failure_h () =
  let protect = protector "api_failure" in
  let out_chan =
    open_out (Filename.concat destdir "include/xen/api/xen_api_failure.h")
  in
  Fun.protect
    (fun () ->
      print_h_header out_chan protect ;
      gen_failure_enum out_chan ;
      gen_failure_funcs out_chan ;
      print_h_footer out_chan
    )
    ~finally:(fun () -> close_out out_chan)

and gen_failure_enum out_chan =
  let print format = fprintf out_chan format in
  print "\nenum xen_api_failure\n{\n%s\n};\n\n\n"
    (String.concat ",\n\n" (failure_enum_entries ()))

and failure_enum_entries () =
  let r = Hashtbl.fold failure_enum_entry Datamodel.errors [] in
  let r = List.sort (fun (x, _) (y, _) -> String.compare y x) r in
  let r =
    failure_enum_entry "UNDEFINED"
      {
        err_doc= "Unknown to this version of the bindings."
      ; err_params= []
      ; err_name= "UNDEFINED"
      }
      r
  in
  List.map (fun (_, y) -> y) (List.rev r)

and failure_enum_entry name err acc =
  ( name
  , sprintf "%s\n    %s"
      (Helper.comment true ~indent:4 err.Datamodel_types.err_doc)
      (failure_enum name)
  )
  :: acc

and gen_failure_funcs out_chan =
  let print format = fprintf out_chan format in
  print
    "%s\n\
     extern const char *\n\
     xen_api_failure_to_string(enum xen_api_failure val);\n\n\n\
     %s\n\
     extern enum xen_api_failure\n\
     xen_api_failure_from_string(const char *str);\n\n"
    (Helper.comment true
       "Return the name corresponding to the given code.  This string must not \
        be modified or freed."
    )
    (Helper.comment true
       "Return the correct code for the given string, or UNDEFINED if the \
        given string does not match a known code."
    )

and gen_failure_c () =
  let out_chan = open_out (Filename.concat destdir "src/xen_api_failure.c") in
  let print format = fprintf out_chan format in
  Fun.protect
    (fun () ->
      print
        "%s\n\n\
         #include \"xen_internal.h\"\n\
         #include <xen/api/xen_api_failure.h>\n\n\n\
         /*\n\
        \ * Maintain this in the same order as the enum declaration!\n\
        \ */\n\
         static const char *lookup_table[] =\n\
         {\n\
        \    %s\n\
         };\n\n\n\
         const char *\n\
         xen_api_failure_to_string(enum xen_api_failure val)\n\
         {\n\
        \    return lookup_table[val];\n\
         }\n\n\n\
         extern enum xen_api_failure\n\
         xen_api_failure_from_string(const char *str)\n\
         {\n\
        \    return ENUM_LOOKUP(str, lookup_table);\n\
         }\n\n\n"
        Licence.bsd_two_clause
        (String.concat ",\n    " (failure_lookup_entries ()))
    )
    ~finally:(fun () -> close_out out_chan)

and failure_lookup_entries () =
  List.sort String.compare
    (Hashtbl.fold failure_lookup_entry Datamodel.errors [])

and failure_lookup_entry name _ acc = sprintf "\"%s\"" name :: acc

and failure_enum name = "XEN_API_FAILURE_" ^ String.uppercase_ascii name

and write_impl {name= classname; contents; messages; _} out_chan =
  let is_event = classname = "event" in
  let print format = fprintf out_chan format in
  let needed = ref StringSet.empty in
  let tn = typename classname in
  let record_tn = record_typename classname in
  let record_opt_tn = record_opt_typename classname in
  let msgs =
    impl_messages needed classname
      (List.filter
         (fun x -> not (classname = "event" && x.msg_name = "from"))
         messages
      )
  in
  let record_free_handle =
    if classname = "event" then "" else "    free(record->handle);\n"
  in
  let record_free_impls =
    joined "\n    " (record_free_impl "record->") contents
  in
  let filtered_record_fields =
    let not_obj_uuid x =
      match x with Field r when r.field_name = "obj_uuid" -> false | _ -> true
    in
    if is_event then List.filter not_obj_uuid contents else contents
  in
  let record_fields =
    joined ",\n        "
      (abstract_record_field classname "" "")
      filtered_record_fields
  in
  let needed = ref StringSet.empty in
  find_needed needed messages ;
  needed := StringSet.add "internal" !needed ;
  needed := StringSet.add classname !needed ;

  let getAllRecordsExists =
    List.exists (fun x -> x.msg_name = "get_all_records") messages
  in
  let mappingName = sprintf "%s_%s" tn record_tn in

  let free_block =
    String.concat "\n"
      (( if is_event then
           []
         else
           [sprintf "XEN_FREE(%s)" tn; sprintf "XEN_SET_ALLOC_FREE(%s)" tn]
       )
      @ [
          sprintf "XEN_ALLOC(%s)" record_tn
        ; sprintf "XEN_SET_ALLOC_FREE(%s)" record_tn
        ]
      @
      if is_event then
        []
      else
        [
          sprintf "XEN_ALLOC(%s)" record_opt_tn
        ; sprintf "XEN_RECORD_OPT_FREE(%s)" tn
        ; sprintf "XEN_SET_ALLOC_FREE(%s)" record_opt_tn
        ]
      )
  in

  print "%s\n\n\n#include <stddef.h>\n#include <stdlib.h>\n\n%s\n\n\n%s\n\n\n"
    Licence.bsd_two_clause (hash_includes !needed) free_block ;

  print
    "static const struct_member %s_struct_members[] =\n\
    \    {\n\
    \        %s\n\
    \    };\n\n\
     const abstract_type %s_abstract_type_ =\n\
    \    {\n\
    \       .XEN_API_TYPE = STRUCT,\n\
    \       .struct_size = sizeof(%s),\n\
    \       .member_count =\n\
    \           sizeof(%s_struct_members) / sizeof(struct_member),\n\
    \       .members = %s_struct_members\n\
    \    };\n\n\n"
    record_tn record_fields record_tn record_tn record_tn record_tn ;

  print
    "const abstract_type %s_set_abstract_type_ =\n\
    \    {\n\
    \       .XEN_API_TYPE = SET,\n\
    \        .child = &%s_abstract_type_\n\
    \    };\n\n\n"
    record_tn record_tn ;

  if getAllRecordsExists then
    print
      "static const struct struct_member %s_members[] =\n\
       {\n\
      \    {\n\
      \        .type = &abstract_type_string,\n\
      \        .offset = offsetof(%s_map_contents, key)\n\
      \    },\n\
      \    {\n\
      \        .type = &%s_abstract_type_,\n\
      \        .offset = offsetof(%s_map_contents, val)\n\
      \    }\n\
       };\n\n\
       const abstract_type abstract_type_string_%s_map =\n\
       {\n\
      \    .XEN_API_TYPE = MAP,\n\
      \    .struct_size = sizeof(%s_map_contents),\n\
      \    .members = %s_members\n\
       };\n\n\n"
      mappingName mappingName record_tn mappingName record_tn mappingName
      mappingName ;

  print
    "void\n\
     %s_free(%s *record)\n\
     {\n\
    \    if (record == NULL)\n\
    \    {\n\
    \        return;\n\
    \    }\n\
     %s    %s\n\
    \    free(record);\n\
     }\n\n\n"
    record_tn record_tn record_free_handle record_free_impls ;

  print "%s\n" msgs

and find_needed needed messages = List.iter (find_needed' needed) messages

and find_needed' needed message =
  List.iter (fun p -> find_needed'' needed p.param_type) message.msg_params ;
  match message.msg_result with
  | Some (x, _) ->
      find_needed'' needed x
  | None ->
      ()

and find_needed'' needed = function
  | SecretString | String | Int | Float | Bool | DateTime ->
      ()
  | Enum (n, _) ->
      needed := StringSet.add (n ^ "_internal") !needed
  | Ref n ->
      needed := StringSet.add n !needed
  | Set (Ref n) ->
      needed := StringSet.add n !needed
  | Set (Enum (e, _)) ->
      needed := StringSet.add e !needed ;
      needed := StringSet.add (e ^ "_internal") !needed
  | Set (Record "event") ->
      needed := StringSet.add "event_operation_internal" !needed
  | Set _ ->
      ()
  | Map (l, r) ->
      let n = mapname l r in
      needed := StringSet.add n !needed ;
      needed := add_enum_map_internal !needed l r ;
      needed := add_enum_internal !needed l ;
      needed := add_enum_internal !needed r
  | Record n ->
      needed := StringSet.add n !needed
  | Option x ->
      find_needed'' needed x

and record_free_impl prefix = function
  | Field fr ->
      free_impl (prefix ^ fieldname fr.field_name) true fr.ty
  | Namespace (p, c) ->
      joined "\n    " (record_free_impl (prefix ^ fieldname p ^ "_")) c

and free_impl val_name record = function
  | SecretString | String ->
      sprintf "free(%s);" val_name
  | Int | Float | Bool | DateTime | Enum (_, _) ->
      ""
  | Ref n ->
      sprintf "%s_free(%s);"
        (if record then record_opt_typename n else typename n)
        val_name
  | Set (Ref n) ->
      sprintf "%s_opt_set_free(%s);" (record_typename n) val_name
  | Set (Enum (e, _)) ->
      sprintf "%s_set_free(%s);" (typename e) val_name
  | Set String ->
      sprintf "xen_string_set_free(%s);" val_name
  | Map (l, r) ->
      let n = mapname l r in
      sprintf "%s_free(%s);" (typename n) val_name
  | Record x ->
      sprintf "%s_free(%s);" (record_typename x) val_name
  | Set Int ->
      sprintf "xen_int_set_free(%s);" val_name
  | Option Int | Option Float | Option Bool | Option DateTime | Option (Enum _)
    ->
      sprintf "free(%s);" val_name
  | Option x ->
      free_impl val_name record x
  | x ->
      eprintf "%s" (Types.to_string x) ;
      assert false

and add_enum_internal needed = function
  | Enum (x, _) ->
      StringSet.add (x ^ "_internal") needed
  | _ ->
      needed

and add_enum_map_internal needed l r =
  match (l, r) with
  | Enum (_, _), _ ->
      StringSet.add (mapname l r ^ "_internal") needed
  | _, Enum (_, _) ->
      StringSet.add (mapname l r ^ "_internal") needed
  | _ ->
      needed

and c_type_of_ty needed record = function
  | SecretString | String ->
      "char *"
  | Int ->
      "int64_t "
  | Float ->
      "double "
  | Bool ->
      "bool "
  | DateTime ->
      "time_t "
  | Ref "session" ->
      "xen_session *"
  | Ref name ->
      needed := StringSet.add (name ^ "_decl") !needed ;
      if record then
        sprintf "struct %s *" (record_opt_typename name)
      else
        sprintf "%s " (typename name)
  | Enum (name, _) as x ->
      needed := StringSet.add name !needed ;
      enums := TypeSet.add x !enums ;
      c_type_of_enum name
  | Set (Ref name) ->
      needed := StringSet.add (name ^ "_decl") !needed ;
      if record then
        sprintf "struct %s_set *" (record_opt_typename name)
      else
        sprintf "struct %s_set *" (typename name)
  | Set (Enum (e, _) as x) ->
      let enum_typename = typename e in
      needed := StringSet.add e !needed ;
      enums := TypeSet.add x !enums ;
      sprintf "struct %s_set *" enum_typename
  | Set String ->
      needed := StringSet.add "string_set" !needed ;
      "struct xen_string_set *"
  | Set (Set String) ->
      needed := StringSet.add "string_set_set" !needed ;
      "struct xen_string_set_set *"
  | Set (Record n) ->
      needed := StringSet.add (n ^ "_decl") !needed ;
      sprintf "struct %s_set *" (record_typename n)
  | Set Int ->
      needed := StringSet.add "int_set" !needed ;
      "struct xen_int_set *"
  | Map (l, r) as x ->
      let n = mapname l r in
      needed := StringSet.add n !needed ;
      maps := TypeSet.add x !maps ;
      ( match (l, r) with
      | Enum (_, _), _ ->
          enum_maps := TypeSet.add x !enum_maps
      | _, Enum (_, _) ->
          enum_maps := TypeSet.add x !enum_maps
      | _ ->
          ()
      ) ;
      sprintf "%s *" (typename n)
  | Record n ->
      if record then
        sprintf "struct %s *" (record_typename n)
      else
        sprintf "%s *" (record_typename n)
  | Option Int ->
      "int64_t *"
  | Option Float ->
      "double *"
  | Option Bool ->
      "bool *"
  | Option DateTime ->
      "time_t *"
  | Option (Enum (name, _) as x) ->
      needed := StringSet.add name !needed ;
      enums := TypeSet.add x !enums ;
      c_type_of_enum name ^ " *"
  | Option n ->
      c_type_of_ty needed record n
  | x ->
      eprintf "%s" (Types.to_string x) ;
      assert false

and c_type_of_enum name = sprintf "enum %s " (typename name)

and initialiser_of_ty = function
  | SecretString | String | Ref _ | Set _ | Map _ | Record _ ->
      "    *result = NULL;\n"
  | _ ->
      ""

and mapname l r = sprintf "%s_%s_map" (name_of_ty l) (name_of_ty r)

and name_of_ty = function
  | SecretString | String ->
      "string"
  | Int ->
      "int"
  | Float ->
      "float"
  | Bool ->
      "bool"
  | DateTime ->
      "datetime"
  | Enum (x, _) ->
      x
  | Set x ->
      sprintf "%s_set" (name_of_ty x)
  | Ref x ->
      x
  | Map (l, r) ->
      sprintf "%s_%s_map" (name_of_ty l) (name_of_ty r)
  | Record n ->
      sprintf "%s" (record_typename n)
  | x ->
      eprintf "%s" (Types.to_string x) ;
      assert false

and decl_filename name =
  let dir =
    if Astring.String.is_suffix ~affix:"internal" name then "" else "xen/api/"
  in
  sprintf "%sxen_%s.h" dir (String.lowercase_ascii name)

and predecl_filename name =
  sprintf "xen/api/xen_%s_decl.h" (String.lowercase_ascii name)

and internal_decl_filename name =
  sprintf "xen_%s_internal.h" (String.lowercase_ascii name)

and impl_filename name = sprintf "xen_%s.c" (String.lowercase_ascii name)

and protector classname = sprintf "XEN_%s_H" (String.uppercase_ascii classname)

and typename classname = sprintf "xen_%s" (String.lowercase_ascii classname)

and record_typename classname = sprintf "%s_record" (typename classname)

and record_opt_typename classname = sprintf "%s_record_opt" (typename classname)

and messagename classname name =
  sprintf "xen_%s_%s"
    (String.lowercase_ascii classname)
    (String.lowercase_ascii name)

and messagename_async classname name =
  sprintf "xen_%s_%s_async"
    (String.lowercase_ascii classname)
    (String.lowercase_ascii name)

and keyword_map name =
  let keywords = [("class", "XEN_CLAZZ"); ("public", "pubblic")] in
  if List.mem_assoc name keywords then List.assoc name keywords else name

and paramname name = keyword_map (String.lowercase_ascii name)

and fieldname name = keyword_map (String.lowercase_ascii name)

and print_h_header out_chan protect =
  let print format = fprintf out_chan format in
  print "%s\n\n" Licence.bsd_two_clause ;
  print "#ifndef %s\n" protect ;
  print "#define %s\n\n" protect

and print_h_footer out_chan = fprintf out_chan "\n#endif\n"

and populate_version () =
  List.iter
    (fun x -> render_file x json_releases templates_dir destdir)
    [
      ("Makefile.mustache", "Makefile")
    ; ("xen_api_version.h.mustache", "include/xen/api/xen_api_version.h")
    ; ("xen_api_version.c.mustache", "src/xen_api_version.c")
    ]

let _ = main () ; populate_version ()
