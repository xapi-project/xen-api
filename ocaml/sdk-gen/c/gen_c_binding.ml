(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

(* Generator of the C SDK from the datamodel *)

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

let rec is_last x list =
  match list with
  | [] ->
      false
  | hd :: [] ->
      if hd = x then true else false
  | hd :: tl ->
      if hd = x then false else is_last x tl

let rec main () =
  let filtered_classes =
    List.filter
      (fun x -> not (List.mem x.name ["session"; "debug"; "data_source"]))
      classes
  in
  List.iter gen_decl filtered_classes ;
  List.iter gen_impl filtered_classes ;

  all_headers := List.map (fun x -> x.name) filtered_classes ;

  TypeSet.iter render_enum !enums ;

  maps := TypeSet.add (Map (String, Int)) !maps ;
  maps := TypeSet.add (Map (Int, Int)) !maps ;
  maps := TypeSet.add (Map (String, Set String)) !maps ;
  maps := TypeSet.add (Map (String, Map (String, String))) !maps ;

  TypeSet.iter (function Map (l, r) -> render_map_decl l r | _ -> ()) !maps ;
  TypeSet.iter (function Map (l, r) -> render_map_impl l r | _ -> ()) !maps ;

  TypeSet.iter
    (function Map (l, r) -> render_enum_map l r | _ -> ())
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
    !all_headers
    |> List.filter (fun x -> not (Astring.String.is_suffix ~affix:"internal" x))
    |> List.map String.lowercase_ascii
    |> List.sort String.compare
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

and gen_decl cls =
  let headers = ref (StringSet.add (cls.name ^ "_decl") StringSet.empty) in
  let rec get_needed = function
    | Field fr ->
        find_needed headers fr.ty
    | Namespace (_, cs) ->
        List.iter get_needed cs
  in
  List.iter get_needed cls.contents ;

  let asyncParams x =
    if x.msg_async then
      {
        param_type= Ref "task"
      ; param_name= "*result"
      ; param_doc= ""
      ; param_release= x.msg_release
      ; param_default= None
      }
      :: x.msg_params
    else
      x.msg_params
  in
  let syncParams x =
    match x.msg_result with
    | Some res ->
        {
          param_type= fst res
        ; param_name= "*result"
        ; param_doc= ""
        ; param_release= x.msg_release
        ; param_default= None
        }
        :: x.msg_params
    | None ->
        x.msg_params
  in
  let paramJson x =
    `O
      [
        ("param_name", `String (paramname x.param_name))
      ; ("param_type", `String (c_type_of_ty headers false x.param_type))
      ]
  in
  let json =
    `O
      [
        ("class_upper", `String (String.uppercase_ascii cls.name))
      ; ("class_lower", `String (String.lowercase_ascii cls.name))
      ; ("class_doc", `String (Helper.comment false (full_class_doc cls)))
      ; ("is_event", `Bool (cls.name = "event"))
      ; ( "headers"
        , `A
            (List.map
               (fun x -> `O [("header", `String x)])
               ("common" :: StringSet.elements !headers
               |> List.map String.lowercase_ascii
               |> List.sort String.compare
               |> List.filter (fun x ->
                      not (Astring.String.is_suffix ~affix:"internal" x)
                  )
               )
            )
        )
      ; ( "fields"
        , `A
            (cls
            |> Datamodel_utils.fields_of_obj
            |> List.map (fun field ->
                   `O
                     [
                       ( "field_name_lower"
                       , `String (fieldname (String.concat "_" field.full_name))
                       )
                     ; ( "field_type"
                       , `String (c_type_of_ty headers true field.ty)
                       )
                     ]
               )
            )
        )
      ; ( "messages"
        , `A
            (cls.messages
            |> List.filter (fun x ->
                   not (cls.name = "event" && x.msg_name = "from")
               )
            |> List.map (fun x ->
                   `O
                     [
                       ( "msg_name_lower"
                       , `String (String.lowercase_ascii x.msg_name)
                       )
                     ; ( "msg_doc"
                       , `String (Helper.comment true (full_msg_doc x))
                       )
                     ; ("is_async", `Bool x.msg_async)
                     ; ("sync_params", `A (List.map paramJson (syncParams x)))
                     ; ("async_params", `A (List.map paramJson (asyncParams x)))
                     ]
               )
            )
        )
      ]
  in
  render_file
    ( "class_decl.h.mustache"
    , sprintf "include/xen/api/xen_%s_decl.h" (String.lowercase_ascii cls.name)
    )
    json templates_dir destdir ;
  render_file
    ( "class.h.mustache"
    , sprintf "include/xen/api/xen_%s.h" (String.lowercase_ascii cls.name)
    )
    json templates_dir destdir

and gen_impl cls =
  let headers = ref StringSet.empty in
  let rec get_needed = function
    | Field fr ->
        find_needed headers fr.ty
    | Namespace (_, cs) ->
        List.iter get_needed cs
  in
  List.iter get_needed cls.contents ;

  List.iter
    (fun x ->
      List.iter (fun p -> find_needed headers p.param_type) x.msg_params ;
      match x.msg_result with
      | Some res ->
          find_needed headers (fst res)
      | None ->
          ()
    )
    cls.messages ;

  let allFields = cls |> Datamodel_utils.fields_of_obj in
  let result_type message =
    match message.msg_result with
    | Some res ->
        abstract_type false (fst res)
    | None ->
        ""
  in
  let init_result message =
    match message.msg_result with
    | Some res -> (
      match fst res with
      | SecretString | String | Ref _ | Set _ | Map _ | Record _ ->
          true
      | _ ->
          false
    )
    | None ->
        false
  in
  let is_result_record message =
    match message.msg_result with
    | Some res -> (
      match fst res with Record _ -> true | _ -> false
    )
    | None ->
        false
  in
  let asyncParams x =
    if x.msg_async then
      {
        param_type= Ref "task"
      ; param_name= "*result"
      ; param_doc= ""
      ; param_release= x.msg_release
      ; param_default= None
      }
      :: x.msg_params
    else
      x.msg_params
  in
  let syncParams x =
    match x.msg_result with
    | Some res ->
        {
          param_type= fst res
        ; param_name= "*result"
        ; param_doc= ""
        ; param_release= x.msg_release
        ; param_default= None
        }
        :: x.msg_params
    | None ->
        x.msg_params
  in
  let messageJson msg =
    let paramJson p =
      `O
        [
          ("param_name", `String (paramname p.param_name))
        ; ("param_type", `String (c_type_of_ty headers false p.param_type))
        ; ("abstract_param_type", `String (abstract_type false p.param_type))
        ; ("abstract_member", `String (abstract_member p.param_type))
        ; ( "abstract_member_conv"
          , `String (abstract_param_conv p.param_name p.param_type)
          )
        ; ("is_last", `Bool (is_last p msg.msg_params))
        ]
    in
    `O
      [
        ("msg_name_lower", `String (String.lowercase_ascii msg.msg_name))
      ; ("msg_name", `String msg.msg_name)
      ; ("msg_doc", `String (Helper.comment true (full_msg_doc msg)))
      ; ("is_async", `Bool msg.msg_async)
      ; ("sync_params", `A (List.map paramJson (syncParams msg)))
      ; ("async_params", `A (List.map paramJson (asyncParams msg)))
      ; ("msg_params", `A (List.map paramJson msg.msg_params))
      ; ("abstract_result_type", `String (result_type msg))
      ; ("has_params", `Bool (List.length msg.msg_params <> 0))
      ; ("param_count", `String (string_of_int (List.length msg.msg_params)))
      ; ("has_result", `Bool (String.compare (result_type msg) "" <> 0))
      ; ("init_result", `Bool (init_result msg))
      ; ("is_result_record", `Bool (is_result_record msg))
      ]
  in
  let fieldJson field =
    let fullName = String.concat "_" field.full_name in
    let freeing = free_impl ("record->" ^ fieldname fullName) true field.ty in
    `O
      [
        ("field_name_lower", `String (fieldname fullName))
      ; ("field_name", `String fullName)
      ; ("abstract_field_type", `String (abstract_type true field.ty))
      ; ("can_free", `Bool (freeing <> ""))
      ; ("free_record_field", `String freeing)
      ; ("is_last", `Bool (is_last field allFields))
      ]
  in
  let json =
    `O
      [
        ("class_name", `String cls.name)
      ; ("class_lower", `String (String.lowercase_ascii cls.name))
      ; ("is_event", `Bool (cls.name = "event"))
      ; ( "has_all_records"
        , `Bool
            (List.exists (fun x -> x.msg_name = "get_all_records") cls.messages)
        )
      ; ( "headers"
        , `A
            (List.map
               (fun x -> `O [("header", `String x)])
               (["common"; String.lowercase_ascii cls.name]
               |> List.sort String.compare
               )
            )
        )
      ; ( "internal_headers"
        , `A
            (List.map
               (fun x -> `O [("header", `String x)])
               ("internal" :: StringSet.elements !headers
               |> List.map String.lowercase_ascii
               |> List.sort String.compare
               |> List.filter (fun x ->
                      Astring.String.is_suffix ~affix:"internal" x
                  )
               )
            )
        )
      ; ("fields", `A (allFields |> List.map fieldJson))
      ; ( "messages"
        , `A
            (cls.messages
            |> List.filter (fun x ->
                   not (cls.name = "event" && x.msg_name = "from")
               )
            |> List.map messageJson
            )
        )
      ]
  in
  render_file
    ( "class.c.mustache"
    , sprintf "src/xen_%s.c" (String.lowercase_ascii cls.name)
    )
    json templates_dir destdir

and full_stop x = if Astring.String.is_suffix ~affix:"." x then "" else "."

and full_class_doc cls =
  let intro = sprintf "The %s class.\n\n" cls.name in
  intro ^ cls.description ^ full_stop cls.description

and full_msg_doc message =
  let role =
    sprintf "\nMinimum allowed role: %s." (get_minimum_allowed_role message)
  in
  let deprecated = get_deprecated_info_message message in
  let deprecated = if deprecated = "" then "" else "\n" ^ deprecated in
  message.msg_doc ^ full_stop message.msg_doc ^ role ^ deprecated

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
  | Set _ | Map _ ->
      "set"
  | Record _ ->
      "struct"
  | x ->
      eprintf "%s" (Types.to_string x) ;
      assert false

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

and replace_dashes x =
  Astring.String.map (fun y -> match y with '-' -> '_' | _ -> y) x

and render_enum x =
  match x with
  | Enum (name, contents) ->
      if not (List.mem name !all_headers) then
        all_headers := name :: !all_headers ;
      let json =
        `O
          [
            ("enum_name", `String name)
          ; ("enum_name_upper", `String (String.uppercase_ascii name))
          ; ("event_operations", `Bool (name = "event_operation"))
          ; ( "enum_values"
            , `A
                (List.map
                   (fun (n, c) ->
                     `O
                       [
                         ("enum_value", `String n)
                       ; ( "enum_value_doc"
                         , `String (Helper.comment true ~indent:4 c)
                         )
                       ; ( "enum_value_upper"
                         , `String (replace_dashes (String.uppercase_ascii n))
                         )
                       ]
                   )
                   contents
                )
            )
          ]
      in
      render_file
        ( "xen_enum_internal.h.mustache"
        , sprintf "include/xen_%s_internal.h" name
        )
        json templates_dir destdir ;
      render_file
        ("xen_enum.h.mustache", sprintf "include/xen/api/xen_%s.h" name)
        json templates_dir destdir ;
      render_file
        ("xen_enum.c.mustache", sprintf "src/xen_%s.c" name)
        json templates_dir destdir
  | _ ->
      ()

and render_enum_map l r =
  let x = mapname l r in
  let json =
    `O
      [
        ("map_upper", `String (String.uppercase_ascii x))
      ; ("map_lower", `String (String.lowercase_ascii x))
      ]
  in
  render_file
    ( "xen_enum_map_internal.h.mustache"
    , sprintf "include/xen_%s_internal.h" (String.lowercase_ascii x)
    )
    json templates_dir destdir

and render_map_decl l r =
  let headers = ref StringSet.empty in
  let add_enum_header = function
    | Enum (x, _) ->
        headers := StringSet.add x !headers
    | _ ->
        ()
  in
  add_enum_header l ;
  add_enum_header r ;
  let x = mapname l r in
  let json =
    `O
      [
        ("key_type_lower", `String (c_type_of_ty headers false l))
      ; ("val_type_lower", `String (c_type_of_ty headers true r))
      ; ("map_upper", `String (String.uppercase_ascii x))
      ; ("map_lower", `String (String.lowercase_ascii x))
      ; ( "headers"
        , `A
            (List.map
               (fun x -> `O [("header", `String x)])
               ("common" :: StringSet.elements !headers
               |> List.map String.lowercase_ascii
               |> List.sort String.compare
               |> List.filter (fun x ->
                      not (Astring.String.is_suffix ~affix:"internal" x)
                  )
               )
            )
        )
      ]
  in
  if not (List.mem x !all_headers) then all_headers := x :: !all_headers ;
  render_file
    ( "map.h.mustache"
    , sprintf "include/xen/api/xen_%s.h" (String.lowercase_ascii x)
    )
    json templates_dir destdir

and render_map_impl l r =
  let x = mapname l r in
  let headers = ref StringSet.empty in
  headers := StringSet.add x !headers ;
  find_needed headers l ;
  find_needed headers r ;

  let l_free_impl = free_impl "map->contents[i].key" false l in
  let r_free_impl = free_impl "map->contents[i].val" true r in
  let is_enum_map =
    match (l, r) with Enum (_, _), _ | _, Enum (_, _) -> true | _ -> false
  in
  let json =
    `O
      [
        ("abstract_type_key", `String (abstract_type false l))
      ; ("abstract_type_val", `String (abstract_type false r))
      ; ("map_upper", `String (String.uppercase_ascii x))
      ; ("map_lower", `String (String.lowercase_ascii x))
      ; ( "headers"
        , `A
            (List.map
               (fun x -> `O [("header", `String x)])
               ("common" :: StringSet.elements !headers
               |> List.map String.lowercase_ascii
               |> List.sort String.compare
               |> List.filter (fun x ->
                      not (Astring.String.is_suffix ~affix:"internal" x)
                  )
               )
            )
        )
      ; ( "internal_headers"
        , `A
            (List.map
               (fun x -> `O [("header", `String x)])
               ("internal" :: StringSet.elements !headers
               |> List.map String.lowercase_ascii
               |> List.sort String.compare
               |> List.filter (fun x ->
                      Astring.String.is_suffix ~affix:"internal" x
                  )
               )
            )
        )
      ; ("can_free_key", `Bool (String.compare l_free_impl "" != 0))
      ; ("can_free_val", `Bool (String.compare r_free_impl "" != 0))
      ; ( "can_free"
        , `Bool
            (String.compare l_free_impl "" != 0
            || String.compare r_free_impl "" != 0
            )
        )
      ; ("free_key", `String l_free_impl)
      ; ("free_val", `String r_free_impl)
      ; ("enum_map", `Bool is_enum_map)
      ]
  in
  if not (List.mem x !all_headers) then all_headers := x :: !all_headers ;
  render_file
    ("map.c.mustache", sprintf "src/xen_%s.c" (String.lowercase_ascii x))
    json templates_dir destdir

and gen_failure () =
  let errors =
    Hashtbl.fold
      (fun _ x acc ->
        (x.Datamodel_types.err_name, x.Datamodel_types.err_doc) :: acc
      )
      Datamodel.errors []
  in
  let errors = List.sort (fun (x, _) (y, _) -> String.compare x y) errors in
  let json =
    `O
      [
        ( "api_errors"
        , `A
            (List.map
               (fun (x, y) ->
                 `O
                   [
                     ("api_error", `String (String.uppercase_ascii x))
                   ; ("api_error_doc", `String (Helper.comment true ~indent:4 y))
                   ]
               )
               errors
            )
        )
      ]
  in
  render_file
    ("xen_api_failure.h.mustache", "include/xen/api/xen_api_failure.h")
    json templates_dir destdir ;
  render_file
    ("xen_api_failure.c.mustache", "src/xen_api_failure.c")
    json templates_dir destdir

and find_needed needed = function
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
      find_needed needed x

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
      sprintf "enum %s " (typename name)
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
      sprintf "enum %s *" (typename name)
  | Option n ->
      c_type_of_ty needed record n
  | x ->
      eprintf "%s" (Types.to_string x) ;
      assert false

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

and mapname l r = sprintf "%s_%s_map" (name_of_ty l) (name_of_ty r)

and typename classname = sprintf "xen_%s" (String.lowercase_ascii classname)

and record_typename classname = sprintf "%s_record" (typename classname)

and record_opt_typename classname = sprintf "%s_record_opt" (typename classname)

and keyword_map name =
  let keywords = [("class", "XEN_CLAZZ"); ("public", "pubblic")] in
  if List.mem_assoc name keywords then List.assoc name keywords else name

and paramname name = keyword_map (String.lowercase_ascii name)

and fieldname name = keyword_map (String.lowercase_ascii name)

and populate_version () =
  List.iter
    (fun x -> render_file x json_releases templates_dir destdir)
    [
      ("Makefile.mustache", "Makefile")
    ; ("xen_api_version.h.mustache", "include/xen/api/xen_api_version.h")
    ; ("xen_api_version.c.mustache", "src/xen_api_version.c")
    ]

let _ = main () ; gen_failure () ; populate_version ()
