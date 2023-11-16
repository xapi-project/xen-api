(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

open Printf
open Datamodel_types

exception Unknown_wire_protocol

type wireProtocol = XmlRpc | JsonRpc

type rpm_version = {major: int; minor: int; micro: int}

let finally f ~(always : unit -> unit) =
  match f () with
  | result ->
      always () ; result
  | exception e ->
      always () ; raise e

let parse_to_rpm_version_option inputStr =
  try
    Scanf.sscanf inputStr "%d.%d.%d" (fun x y z ->
        Some {major= x; minor= y; micro= z}
    )
  with _ -> None

let string_of_file filename =
  let in_channel = open_in filename in
  finally
    (fun () ->
      let rec read_lines acc =
        try read_lines (input_line in_channel :: acc) with End_of_file -> acc
      in
      read_lines [] |> List.rev |> String.concat "\n"
    )
    ~always:(fun () -> close_in in_channel)

let with_output filename f =
  let io = open_out filename in
  finally (fun () -> f io) ~always:(fun () -> close_out io)

let escape_xml s =
  s
  |> Astring.String.cuts ~sep:"<" ~empty:true
  |> String.concat "&lt;"
  |> Astring.String.cuts ~sep:">" ~empty:true
  |> String.concat "&gt;"

let list_index_of x list =
  let rec index_rec i = function
    | [] ->
        raise Not_found
    | hd :: tl ->
        if hd = x then i else index_rec (i + 1) tl
  in
  try index_rec 0 list with Not_found -> -1

let is_method_static message =
  match message.msg_params with
  | [] ->
      true
  | {param_name= "self"; _} :: _ ->
      false
  | {param_type= ty; _} :: _ ->
      not (ty = Ref message.msg_obj_name)

and is_setter message = String.starts_with ~prefix:"set" message.msg_name

and is_getter message = String.starts_with ~prefix:"get" message.msg_name

and is_adder message = String.starts_with ~prefix:"add" message.msg_name

and is_remover message = String.starts_with ~prefix:"remove" message.msg_name

and is_constructor message =
  message.msg_tag = FromObject Make || message.msg_name = "create"

and is_real_constructor message = message.msg_tag = FromObject Make

and is_destructor message =
  message.msg_tag = FromObject Delete || message.msg_name = "destroy"

let code_name_order = release_order |> List.map code_name_of_release

let compare_versions x y =
  let option1 = parse_to_rpm_version_option x in
  let option2 = parse_to_rpm_version_option y in
  match (option1, option2) with
  | None, None ->
      let index1 = list_index_of x code_name_order in
      let index2 = list_index_of y code_name_order in
      index1 - index2
  | None, _ ->
      -1
  | _, None ->
      1
  | Some v1, Some v2 ->
      if v1.major = v2.major && v1.minor = v2.minor && v1.micro = v2.micro then
        0
      else if v1.major = v2.major && v1.minor = v2.minor then
        v1.micro - v2.micro
      else if v1.major = v2.major then
        v1.minor - v2.minor
      else
        v1.major - v2.major

let rec lifecycle_matcher milestone lifecycle =
  match lifecycle with
  | [] ->
      ""
  | (x, y, _) :: tl ->
      if x = milestone then
        y
      else
        lifecycle_matcher milestone tl

let get_published_release_for_param releases =
  let filtered =
    List.filter
      (fun x -> x <> "closed" && x <> "3.0.3" && x <> "debug")
      releases
  in
  match filtered with [] -> "" | hd :: _ -> hd

let get_prototyped_release lifecycle =
  lifecycle_matcher Lifecycle.Prototyped lifecycle

let get_published_release lifecycle =
  lifecycle_matcher Lifecycle.Published lifecycle

let get_release_branding codename =
  try
    let found =
      List.find (fun x -> code_name_of_release x = codename) release_order
    in
    found.branding
  with Not_found -> codename

let group_params_per_release params =
  let same_release p1 p2 =
    compare_versions
      (get_published_release_for_param p1.param_release.internal)
      (get_published_release_for_param p2.param_release.internal)
    = 0
  in
  let rec groupByRelease acc = function
    | [] ->
        acc
    | hd :: tl ->
        let l1, l2 = List.partition (same_release hd) tl in
        groupByRelease ((hd :: l1) :: acc) l2
  in
  List.rev (groupByRelease [] params)

let collate l =
  let rec collator x acc = function
    | [] ->
        List.rev List.(map rev acc)
    | hd :: tl ->
        collator (hd :: x) ((hd :: x) :: acc) tl
  in
  collator [] [] l

let gen_param_groups message params =
  let expRelease = get_prototyped_release message.msg_lifecycle.transitions in
  let paramGroups = group_params_per_release params in
  let overloadGroups = List.map List.concat (collate paramGroups) in
  let filter_self_param message params =
    match params with
    | [] ->
        []
    | {param_name= "self"; _} :: tl ->
        tl
    | {param_type= ty; _} :: tl when ty = Ref message.msg_obj_name ->
        tl
    | _ ->
        params
  in
  let valid x =
    match x with
    | [] when is_setter message ->
        false
    | [] when is_adder message ->
        false
    | [] when is_remover message ->
        false
    | _ ->
        true
  in
  List.filter valid
    (List.map
       (filter_self_param message)
       (if expRelease = "" then overloadGroups else [params])
    )

and get_minimum_allowed_role msg =
  let get_last_role recroles =
    let reversed_role_list = List.rev recroles in
    let rec get_role reversed_list =
      match reversed_list with
      | [] ->
          "Not Applicable"
      | last :: rest ->
          if not (Datamodel_roles.role_is_internal last) then
            last
          else
            get_role rest
    in
    get_role reversed_role_list
  in
  match msg.msg_allowed_roles with
  | None ->
      "Not Applicable"
  | Some roles ->
      get_last_role roles

(*** XML documentation ***)

and get_published_info_message message cls =
  let classRelease = get_published_release cls.obj_lifecycle.transitions in
  let msgRelease = get_published_release message.msg_lifecycle.transitions in
  let expRel = get_prototyped_release message.msg_lifecycle.transitions in
  if msgRelease = "" && expRel <> "" then
    sprintf "Experimental. First published in %s." (get_release_branding expRel)
  else
    let codename =
      if compare_versions msgRelease classRelease > 0 then
        msgRelease
      else
        classRelease
    in
    sprintf "First published in %s." (get_release_branding codename)

and get_deprecated_info_message message =
  let version = message.msg_release.internal_deprecated_since in
  match version with
  | None ->
      ""
  | Some x ->
      sprintf "Deprecated since %s." (get_release_branding x)

and get_published_info_param message param =
  let msgRelease = get_published_release message.msg_lifecycle.transitions in
  let paramRelease =
    get_published_release_for_param param.param_release.internal
  in
  if compare_versions paramRelease msgRelease > 0 then
    sprintf "First published in %s." (get_release_branding paramRelease)
  else
    ""

and get_published_info_class cls =
  let clsRelease = get_published_release cls.obj_lifecycle.transitions in
  sprintf "First published in %s." (get_release_branding clsRelease)

and get_published_info_field field cls =
  let clsRelease = get_published_release cls.obj_lifecycle.transitions in
  let fieldRelease = get_published_release field.lifecycle.transitions in
  let expRel = get_prototyped_release field.lifecycle.transitions in
  if fieldRelease = "" && expRel <> "" then
    sprintf "Experimental. First published in %s." (get_release_branding expRel)
  else if compare_versions fieldRelease clsRelease > 0 then
    sprintf "First published in %s." (get_release_branding fieldRelease)
  else
    ""

and render_template template_file json output_file =
  let templ = string_of_file template_file |> Mustache.of_string in
  let rendered = Mustache.render templ json in
  let out_chan = open_out output_file in
  finally
    (fun () -> output_string out_chan rendered)
    ~always:(fun () -> close_out out_chan)

let render_file (infile, outfile) json templates_dir dest_dir =
  let input_path = Filename.concat templates_dir infile in
  let output_path = Filename.concat dest_dir outfile in
  render_template input_path json output_path

let json_releases =
  let rec get_unique l =
    match l with
    | [] ->
        []
    | hd :: tl ->
        let remove_duplicates =
          List.filter (fun x ->
              not
                (x.version_major = hd.version_major
                && x.version_minor = hd.version_minor
                )
          )
        in
        hd :: get_unique (remove_duplicates tl)
  in
  let unique_version_bumps = get_unique release_order_full in
  let version_index_of x list =
    let rec index_rec i = function
      | [] ->
          raise Not_found
      | hd :: tl ->
          if
            hd.version_major = x.version_major
            && hd.version_minor = x.version_minor
          then
            i
          else
            index_rec (i + 1) tl
    in
    try index_rec 0 list with Not_found -> -1
  in
  let json_of_rel x =
    let y = version_index_of x unique_version_bumps + 1 in
    `O
      [
        ( "code_name"
        , `String (match x.code_name with Some r -> r | None -> "")
        )
      ; ("version_major", `Float (float_of_int x.version_major))
      ; ("version_minor", `Float (float_of_int x.version_minor))
      ; ("branding", `String x.branding)
      ; ("version_index", `Float (float_of_int y))
      ]
  in
  `O
    [
      ("API_VERSION_MAJOR", `Float (Int64.to_float Datamodel.api_version_major))
    ; ("API_VERSION_MINOR", `Float (Int64.to_float Datamodel.api_version_minor))
    ; ("releases", `A (List.map (fun x -> json_of_rel x) unique_version_bumps))
    ; ( "latest_version_index"
      , `Float (float_of_int (List.length unique_version_bumps))
      )
    ]
