(* Common definitions shared by multiple object declarations *)

open Datamodel_types
open Datamodel_roles

let get_oss_releases in_oss_since =
  match in_oss_since with
    None -> []
  | Some "3.0.3" -> ["3.0.3"]
  | _ -> raise UnspecifiedRelease

let get_product_releases in_product_since =
  let rec go_through_release_order rs =
    match rs with
      [] -> raise UnspecifiedRelease
    | x::xs when code_name_of_release x = in_product_since -> "closed"::in_product_since::(List.map code_name_of_release xs)
    | x::xs -> go_through_release_order xs
  in go_through_release_order release_order

let get_published lifecycle =
  try
    let _, published, _ = List.find (fun (t, _, _) -> t = Published) lifecycle in
    Some published
  with Not_found -> None

let get_deprecated lifecycle =
  try
    let _, deprecated, _ = List.find (fun (t, _, _) -> t = Deprecated) lifecycle in
    Some deprecated
  with Not_found -> None

let call ~name ?(doc="") ?(in_oss_since=Some "3.0.3") ?in_product_since ?internal_deprecated_since
    ?result ?(flags=[`Session;`Async])
    ?(effect=true) ?(tag=Custom) ?(errs=[]) ?(custom_marshaller=false) ?(db_only=false)
    ?(no_current_operations=false) ?(secret=false) ?(hide_from_docs=false)
    ?(pool_internal=false)
    ~allowed_roles
    ?(map_keys_roles=[])
    ?(params=[]) ?versioned_params ?lifecycle ?(doc_tags=[])
    ?forward_to () =
  (* if you specify versioned_params then these get put in the params field of the message record;
     	 * otherwise params go in with no default values and param_release=call_release...
     	 *)
  if lifecycle = None && in_product_since = None then
    failwith ("Lifecycle for message '" ^ name ^ "' not specified");
  let lifecycle = match lifecycle with
    | None ->
      let published = match in_product_since with
        | None -> []
        | Some rel -> [Published, rel, doc]
      in
      let deprecated = match internal_deprecated_since with
        | None -> []
        | Some rel -> [Deprecated, rel, ""]
      in
      published @ deprecated
    | Some l -> l
  in
  let call_release =
    {
      internal = (match get_published lifecycle with
          | Some published -> get_product_releases published
          | None -> ["closed"]);
      opensource = get_oss_releases in_oss_since;
      internal_deprecated_since = get_deprecated lifecycle;
    }
  in
  {
    msg_name = name;
    msg_params =
      (match versioned_params with
       | None ->
         List.map (fun (ptype, pname, pdoc) -> {param_type=ptype; param_name=pname;
                                                param_doc=pdoc; param_release=call_release; param_default=None}) params
       | Some ps -> ps);
    msg_result = result; msg_doc = doc;
    msg_session = List.mem `Session flags; msg_async = List.mem `Async flags;
    msg_db_only = db_only;
    msg_release = call_release;
    msg_lifecycle = lifecycle;
    msg_has_effect = effect; msg_tag = tag; msg_obj_name="";
    msg_force_custom = None;
    msg_errors = List.map (Hashtbl.find Datamodel_errors.errors) errs; msg_secret = secret;
    msg_custom_marshaller = custom_marshaller;
    msg_no_current_operations = no_current_operations;
    msg_hide_from_docs = hide_from_docs;
    msg_pool_internal = pool_internal;
    msg_allowed_roles = allowed_roles;
    msg_map_keys_roles = map_keys_roles;
    msg_doc_tags = doc_tags;
    msg_forward_to = forward_to;
  }

let errnames_of_call c =
  List.map (fun e -> e.err_name) c.msg_errors


(** Compute an enum constant corresponding to an operation, for current_operations,
    allowed_operations.*)
let operation_enum x =
  x.msg_name, Printf.sprintf "refers to the operation \"%s\"" x.msg_name

(** Make an object field record *)
let field ?(in_oss_since = Some "3.0.3") ?in_product_since ?(internal_only = false)
    ?internal_deprecated_since ?(ignore_foreign_key = false) ?(writer_roles=None) ?(reader_roles=None)
    ?(qualifier = RW) ?(ty = String) ?(effect = false) ?(default_value = None) ?(persist = true)
    ?(map_keys_roles=[]) (* list of (key_name,(writer_roles)) for a map field *)
    ?lifecycle ?(doc_tags=[]) name desc =
  (* in_product_since currently defaults to 'Some rel_rio', for backwards compatibility.
     	 * This should eventually become 'None'. *)
  let in_product_since = match in_product_since with None -> Some rel_rio | x -> x in
  if lifecycle = None && in_product_since = None then
    failwith ("Lifecycle for field '" ^ name ^ "' not specified");
  let lifecycle = match lifecycle with
    | None ->
      let published = match in_product_since with
        | None -> []
        | Some rel -> [Published, rel, desc]
      in
      let deprecated = match internal_deprecated_since with
        | None -> []
        | Some rel -> [Deprecated, rel, ""]
      in
      published @ deprecated
    | Some l -> l
  in
  let release =
    {
      internal = (match get_published lifecycle with
          | Some published -> get_product_releases published
          | None -> ["closed"]);
      opensource = get_oss_releases in_oss_since;
      internal_deprecated_since = get_deprecated lifecycle;
    }
  in
  Field {
    release = release;
    lifecycle=lifecycle;
    qualifier=qualifier; ty=ty; internal_only = internal_only; default_value = default_value;
    field_name=name;
    full_name=[ name ];
    field_description=desc;
    field_persist=persist;
    field_has_effect = effect;
    field_ignore_foreign_key = ignore_foreign_key;
    field_setter_roles = writer_roles;
    field_getter_roles = reader_roles;
    field_map_keys_roles = map_keys_roles;
    field_doc_tags = doc_tags;
  }

let uid ?(in_oss_since=Some "3.0.3") ?(reader_roles=None) ?lifecycle refname =
  field
    ~in_oss_since
    ?lifecycle
    ~qualifier:DynamicRO
    ~ty:(String)
    ~writer_roles:_R_POOL_ADMIN (* only the system should be able to create/modify uuids *)
    ~reader_roles
    "uuid"
    "Unique identifier/object reference"

let allowed_and_current_operations ?(writer_roles=None) ?(reader_roles=None) operations_type =
  [
    field ~writer_roles ~reader_roles ~persist:false ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Set operations_type) ~default_value:(Some (VSet [])) "allowed_operations" "list of the operations allowed in this state. This list is advisory only and the server state may have changed by the time this field is read by a client.";
    field ~writer_roles ~reader_roles ~persist:false ~in_oss_since:None ~qualifier:DynamicRO ~ty:(Map(String, operations_type)) ~default_value:(Some (VMap [])) "current_operations" "links each of the running tasks using this object (by reference) to a current_operation enum which describes the nature of the task.";
  ]


(** Make a Namespace (note effect on enclosing field.full_names) *)
let namespace ?(get_field_writer_roles=fun x->x) ?(get_field_reader_roles=fun x->x) ?(idempotent=false) ~name ~contents () =
  let rec prefix = function
    | Namespace(x, xs) -> Namespace(x, List.map prefix xs)
    | Field x -> Field { x with full_name = if idempotent then x.full_name else name :: x.full_name;
                                field_setter_roles=get_field_writer_roles x.field_setter_roles;
                                field_getter_roles=get_field_reader_roles x.field_getter_roles
                       } in
  Namespace(name, List.map prefix contents)

(** Many of the objects have a set of names of various lengths: *)
let names ?(writer_roles=None) ?(reader_roles=None) ?lifecycle in_oss_since qual =
  let field x y =
    field x y ~in_oss_since ~qualifier:qual ~writer_roles ~reader_roles
      ~default_value:(Some (VString "")) ?lifecycle in
  [
    field "label" "a human-readable name";
    field "description" "a notes field containing human-readable description"
  ]

let default_field_reader_roles = _R_ALL (* by default, all can read fields *)
let default_field_writer_roles = _R_POOL_ADMIN (* by default, only root can write to them *)

(** Create an object and map the object name into the messages *)
let create_obj ?lifecycle ~in_oss_since ?in_product_since ?(internal_deprecated_since=None) ~gen_constructor_destructor ~gen_events ~persist ~name ~descr ~doccomments ~contents ~messages ~in_db
    ?(contents_default_reader_roles=default_field_reader_roles) ?(contents_default_writer_roles=None)
    ?(implicit_messages_allowed_roles=_R_ALL) (* used in implicit obj msgs (get_all, etc) *)
    ?force_custom_actions:(force_custom_actions=None) (* None,Some(RW),Some(StaticRO) *)
    ~messages_default_allowed_roles ?(doc_tags=[])(* used in constructor, destructor and explicit obj msgs *)
    ?(msg_lifecycles = [])(* To specify lifecycle for automatic messages (e.g. constructor) when different to object lifecycle. *)
    () =
  let contents_default_writer_roles = if contents_default_writer_roles=None then messages_default_allowed_roles else contents_default_writer_roles in
  let get_field_reader_roles = function None->contents_default_reader_roles|r->r in
  let get_field_writer_roles = function None->contents_default_writer_roles|r->r in
  let get_msg_allowed_roles = function None->messages_default_allowed_roles|r->r in
  let contents = List.map (function
      | Namespace(n,cs)->namespace ~get_field_writer_roles ~get_field_reader_roles ~name:n ~contents:cs ~idempotent:true ()
      | Field f->Field{f with field_setter_roles=get_field_writer_roles f.field_setter_roles;
                              field_getter_roles=get_field_reader_roles f.field_getter_roles}
    ) contents in
  if lifecycle = None && in_product_since = None then
    failwith ("Lifecycle for class '" ^ name ^ "' not specified");
  let lifecycle = match lifecycle with
    | None ->
      let published = match in_product_since with
        | None -> []
        | Some rel -> [Published, rel, descr]
      in
      let deprecated = match internal_deprecated_since with
        | None -> []
        | Some rel -> [Deprecated, rel, ""]
      in
      published @ deprecated
    | Some l -> l
  in
  let release =
    {
      internal = (match get_published lifecycle with
          | Some published -> get_product_releases published
          | None -> ["closed"]);
      opensource = get_oss_releases in_oss_since;
      internal_deprecated_since = get_deprecated lifecycle;
    }
  in
  let msgs = List.map (fun m -> {m with msg_obj_name=name;msg_allowed_roles=get_msg_allowed_roles m.msg_allowed_roles}) messages in
  { name = name; description = descr; obj_lifecycle = lifecycle; messages = msgs; contents = contents;
    doccomments = doccomments; msg_lifecycles = msg_lifecycles;
    gen_constructor_destructor = gen_constructor_destructor; force_custom_actions = force_custom_actions;
    persist = persist; gen_events = gen_events; obj_release = release;
    in_database=in_db; obj_allowed_roles = messages_default_allowed_roles; obj_implicit_msg_allowed_roles = implicit_messages_allowed_roles;
    obj_doc_tags = doc_tags;
  }
