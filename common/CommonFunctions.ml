(*
 * Copyright (c) Citrix Systems, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1) Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2) Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *)


open Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_unix
open Printf
open Datamodel_types

type wireProtocol = XmlRpc | JsonRpc

let with_output filename f =
  let io = open_out filename in
  finally
    (fun () -> f io)
    (fun () -> close_out io)


let rec list_distinct list =
  match list with
  | []                  -> []
  | [x]                 -> [x]
  | hd1::(hd2::_ as tl) -> if hd1 = hd2 then list_distinct tl
    else hd1::(list_distinct tl)

let rec list_last = function
  | x::[]  -> x
  | _::tl -> list_last tl
  | []     -> failwith "Cannot return the last element of an empty list."

and list_index_of x list =
  let rec index_rec i = function
    | []     -> raise Not_found
    | hd::tl -> if hd = x then i else index_rec (i+1) tl
  in
  try
    index_rec 0 list
  with Not_found -> -1

let rec gen_param_groups_for_releases releaseOrder params =
  match releaseOrder with
  | []     -> [("", [])]
  | hd::tl -> (hd, (List.filter (fun x -> List.mem hd x.param_release.internal) params))::(gen_param_groups_for_releases tl params)

and is_method_static message =
  match message.msg_params with
  | []                     -> true
  | {param_name="self"; _}::_ -> false
  | {param_type=ty; _}::_     -> not (ty = Ref message.msg_obj_name)

and get_method_params_list message =
  if is_method_static message then message.msg_params
  else List.tl message.msg_params

and code_name_of_release_ext x =
  try code_name_of_release x
  with UnspecifiedRelease -> ""

and code_name_order =
  release_order |> List.map code_name_of_release

and gen_param_groups message params =
  let expRelease = get_prototyped_release message.msg_lifecycle in
  let msgRelease = get_first_release message.msg_release.internal in
  let msgReleaseIndex = list_index_of msgRelease code_name_order in
  let paramGroups = gen_param_groups_for_releases code_name_order params in
  let rec getValid x = match x with
    | [] -> []
    | hd::tl -> let index = list_index_of (fst hd) code_name_order in
      let valid = if (not (index = -1)) && (not (msgReleaseIndex = -1)) && (index < msgReleaseIndex) then []
        else snd hd in
      valid::(getValid tl)
  in
  let filteredGroups = List.filter (fun x -> match x with | [] -> false | _ -> true) (getValid paramGroups) in
  if (not (expRelease = "")) then
    [params]
  else
    list_distinct filteredGroups

and get_release_name codename =
  try
    let found = List.find (fun x-> code_name_of_release x = codename) release_order in
    found.branding
  with | Not_found -> ""

and get_first_release releases =
  let filtered = List.filter (fun x -> List.mem x releases) code_name_order in
  match filtered with
  | []     -> ""
  | hd::_ -> hd

and get_first_release_string release =
  if release = "" then ""
  else sprintf "First published in %s." (get_release_name release)

and get_deprecated_info_message_string version =
  match version with
  | None -> ""
  | Some x -> sprintf "Deprecated since %s." (get_release_name x)

and get_prototyped_release lifecycle =
  match lifecycle with
  | [Prototyped, release, _] -> release
  | _                          -> ""

and get_prototyped_release_string lifecycle =
  match lifecycle with
  | [Prototyped, release, _] -> "Experimental. "^(get_first_release_string release)
  | _                          -> ""

and get_published_info_message message cls =
  let expRelease = get_prototyped_release_string message.msg_lifecycle in
  let clsRelease = get_first_release cls.obj_release.internal in
  let msgRelease =  get_first_release message.msg_release.internal in
  let clsReleaseIndex = list_index_of clsRelease code_name_order in
  let msgReleaseIndex = list_index_of msgRelease code_name_order in
  if (not (expRelease = "")) then
    expRelease
  else if (not (clsReleaseIndex = -1)) && (not (msgReleaseIndex = -1)) && (clsReleaseIndex < msgReleaseIndex) then
    get_first_release_string msgRelease
  else
    get_first_release_string clsRelease

and get_deprecated_info_message message =
  let msgDeprecated = message.msg_release.internal_deprecated_since in
  get_deprecated_info_message_string msgDeprecated

and get_published_info_param message param =
  let msgRelease = get_first_release message.msg_release.internal in
  let paramRelease = get_first_release param.param_release.internal in
  let msgReleaseIndex = list_index_of msgRelease code_name_order in
  let paramReleaseIndex = list_index_of paramRelease code_name_order in
  if (not (msgReleaseIndex = -1)) && (not (paramReleaseIndex = -1)) && (msgReleaseIndex < paramReleaseIndex) then
    get_first_release_string paramRelease
  else ""

and get_published_info_class cls =
  get_first_release_string (get_first_release cls.obj_release.internal)

and get_published_info_field field cls =
  let expRelease = get_prototyped_release_string field.lifecycle in
  let clsRelease = get_first_release cls.obj_release.internal in
  let fieldRelease =  get_first_release field.release.internal in
  let clsReleaseIndex = list_index_of clsRelease code_name_order in
  let fieldReleaseIndex = list_index_of fieldRelease code_name_order in
  if (not (expRelease = "")) then
    expRelease
  else if (not (clsReleaseIndex = -1)) && (not (fieldReleaseIndex = -1)) && (clsReleaseIndex < fieldReleaseIndex) then
    get_first_release_string fieldRelease
  else
    ""

and render_template template_file json output_file =
  let templ = Unixext.string_of_file template_file |> Mustache.of_string in
  let rendered = Mustache.render templ json in
  let out_chan = open_out output_file in
  finally (fun () -> output_string out_chan rendered)
    (fun () -> close_out out_chan)

let render_file (infile,outfile) json templates_dir dest_dir=
  let input_path = Filename.concat templates_dir infile in
  let output_path = Filename.concat dest_dir outfile in
  render_template input_path json output_path

let json_releases =
  let json_of_rel x y = `O [
      "code_name", `String (code_name_of_release_ext x);
      "version_major", `Float (float_of_int x.version_major);
      "version_minor", `Float (float_of_int x.version_minor);
      "branding", `String x.branding;
      "version_index", `Float (float_of_int y);
    ]
  in
  let rec get_unique l =
    match l with
    | [] -> []
    | hd::tl ->
      let remove_duplicates = List.filter (fun x -> not (x.version_major = hd.version_major && x.version_minor = hd.version_minor)) in
      hd::(get_unique (remove_duplicates tl))
  in
  let unique_version_bumps = get_unique release_order_full in
  `O [ "API_VERSION_MAJOR", `Float (Int64.to_float Datamodel.api_version_major);
       "API_VERSION_MINOR", `Float (Int64.to_float Datamodel.api_version_minor);
       "releases", `A (List.map (fun x -> json_of_rel x ((list_index_of x unique_version_bumps) + 1)) unique_version_bumps);
       "latest_version_index", `Float (float_of_int (List.length unique_version_bumps));]
