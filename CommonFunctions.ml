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

open Printf
open Datamodel_types
module DT = Datamodel_types

let rec list_distinct list =
  match list with
  | []                  -> []
  | [x]                 -> [x]
  | hd1::(hd2::_ as tl) -> if hd1 = hd2 then list_distinct tl
                             else hd1::(list_distinct tl)           

let rec list_last = function
  | x::[]  -> x
  | hd::tl -> list_last tl
  | []     -> failwith "Cannot return the last element of an empty list."

and list_index_of x list = 
  let rec index_rec i = function
    | []     -> raise Not_found
    | hd::tl -> if hd = x then i else index_rec (i+1) tl
  in
  try
    index_rec 0 list
    with Not_found -> -1

and list_is_empty list =
  match list with
  | [] -> true
  | _  -> false

let rec gen_param_groups_for_releases releaseOrder params =
  match releaseOrder with
  | []     -> [("", [])]
  | hd::tl -> (hd, (List.filter (fun x -> List.mem hd x.param_release.internal) params))::(gen_param_groups_for_releases tl params)

and is_method_static message =
  match message.msg_params with
  | []                     -> true
  | {param_name="self"}::_ -> false
  | {param_type=ty}::_     -> not (ty = Ref message.msg_obj_name)

and get_method_params_list message =
  if is_method_static message then message.msg_params
  else List.tl message.msg_params

and gen_param_groups message params =
  let expRelease = get_prototyped_release message.msg_lifecycle in
  let msgRelease = get_first_release message.msg_release.internal in
  let msgReleaseIndex = list_index_of msgRelease DT.release_order in
  let paramGroups = gen_param_groups_for_releases DT.release_order params in
  let rec getValid x = match x with
                       | [] -> []
                       | hd::tl -> let index = list_index_of (fst hd) DT.release_order in
                                   let valid = if (not (index = -1)) && (not (msgReleaseIndex = -1)) && (index < msgReleaseIndex) then []
                                               else snd hd in
                                   valid::(getValid tl)
                     in
  let filteredGroups = List.filter (fun x -> match x with | [] -> false | _ -> true) (getValid paramGroups) in
  if (not (expRelease = "")) then
    [params]
  else
    list_distinct filteredGroups

and get_release_name release =
  if      release = rel_rio                 then "XenServer 4.0"
  else if release = rel_miami               then "XenServer 4.1"
  else if release = rel_symc                then "XenServer 4.1.1"
  else if release = rel_orlando             then "XenServer 5.0"
  else if release = rel_orlando_update_1    then "XenServer 5.0 Update 1"
  else if release = rel_george              then "XenServer 5.5"
  else if release = rel_midnight_ride       then "XenServer 5.6"
  else if release = rel_cowley              then "XenServer 5.6 FP1"
  else if release = rel_boston              then "XenServer 6.0"
  else if release = rel_tampa               then "XenServer 6.1"
  else if release = rel_clearwater          then "XenServer 6.2"
  else if release = rel_vgpu_tech_preview   then "XenServer 6.2 SP1 Tech-Preview"
  else if release = rel_vgpu_productisation then "XenServer 6.2 SP1"
  else if release = rel_clearwater_felton   then "XenServer 6.2 SP1 Hotfix XS62ESP1004"
  else                                           ""

and get_first_release releases =
  let filtered = List.filter (fun x -> List.mem x releases) DT.release_order in
    match filtered with
    | []     -> ""
    | hd::tl -> hd

and get_first_release_string release =
  if release = "" then ""
  else sprintf "First published in %s." (get_release_name release)

and get_prototyped_release lifecycle =
  match lifecycle with
     | [Prototyped, release, doc] -> release
     | _                          -> ""

and get_prototyped_release_string lifecycle =
  match lifecycle with
     | [Prototyped, release, doc] -> "Experimental. "^(get_first_release_string release)
     | _                          -> ""

and get_published_info_message message cls =
  let expRelease = get_prototyped_release_string message.msg_lifecycle in 
  let clsRelease = get_first_release cls.obj_release.internal in 
  let msgRelease =  get_first_release message.msg_release.internal in
  let clsReleaseIndex = list_index_of clsRelease DT.release_order in
  let msgReleaseIndex = list_index_of msgRelease DT.release_order in
    if (not (expRelease = "")) then
      expRelease
    else if (not (clsReleaseIndex = -1)) && (not (msgReleaseIndex = -1)) && (clsReleaseIndex < msgReleaseIndex) then
      get_first_release_string msgRelease
    else
      get_first_release_string clsRelease

and get_published_info_param message param =
  let msgRelease = get_first_release message.msg_release.internal in 
  let paramRelease = get_first_release param.param_release.internal in
  let msgReleaseIndex = list_index_of msgRelease DT.release_order in
  let paramReleaseIndex = list_index_of paramRelease DT.release_order in 
    if (not (msgReleaseIndex = -1)) && (not (paramReleaseIndex = -1)) && (msgReleaseIndex < paramReleaseIndex) then
      get_first_release_string paramRelease
    else ""

and get_published_info_class cls =
  get_first_release_string (get_first_release cls.obj_release.internal)

and get_published_info_field field cls =
  let expRelease = get_prototyped_release_string field.lifecycle in 
  let clsRelease = get_first_release cls.obj_release.internal in 
  let fieldRelease =  get_first_release field.release.internal in
  let clsReleaseIndex = list_index_of clsRelease DT.release_order in
  let fieldReleaseIndex = list_index_of fieldRelease DT.release_order in
    if (not (expRelease = "")) then
      expRelease
    else if (not (clsReleaseIndex = -1)) && (not (fieldReleaseIndex = -1)) && (clsReleaseIndex < fieldReleaseIndex) then
      get_first_release_string fieldRelease
    else
      ""

