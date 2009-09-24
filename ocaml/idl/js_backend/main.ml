(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(* main.ml *)

open Datamodel_types
open Dm_api

let rec ty_to_js ty =
  match ty with
    | String -> "{ty:\"string\"}"
    | Int -> "{ty:\"int\"}"
    | Float -> "{ty:\"float\"}"
    | Bool -> "{ty:\"bool\"}"
    | DateTime -> "{ty:\"datetime\"}"
    | Enum (name,values) -> 
	Printf.sprintf "{ty:\"enum\",name:\"%s\",values:[%s]}" name 
	  (String.concat "," (List.map (fun (v,d) -> "\""^v^"\"") values))
    | Set (ty) -> Printf.sprintf "{ty:\"set\",contents:%s}" (ty_to_js ty)
    | Map (ty1,ty2) -> Printf.sprintf "{ty:\"map\",keys:%s,values:%s}"
	(ty_to_js ty1) (ty_to_js ty2)
    | Ref r -> Printf.sprintf "{ty:\"ref\",class:\"%s\"}" r
    | Record r -> Printf.sprintf "{ty:\"record\",name:\"%s\"}" r

let _ =
  let api = Datamodel_utils.add_implicit_messages (Datamodel.all_api) in
  let objs = objects_of_api api in
  let msgs = List.flatten (List.map (fun obj -> 
    let jsstruct = List.map (fun msg ->
      (
	(Datamodel_utils.wire_name ~sync:true obj msg),
	(let params = List.map (fun param -> (param.param_name,ty_to_js param.param_type,param.param_doc)) msg.msg_params in
	 if msg.msg_session 
	 then ("session_id","{ty:\"ref\",class:\"session\"}","The session reference")::params
	 else params),
	msg.msg_doc
      )) obj.messages
    in
    let js = List.map (fun (msgname,params,doc) ->
      Printf.sprintf "\"%s\":{params:[%s],doc:\"%s\"}" msgname 
	(String.concat "," 
	    (List.map (fun (name,ty,doc) -> 
	      Printf.sprintf "{name:\"%s\",ty:%s,doc:\"%s\"}" name ty doc) params))
	doc
    ) jsstruct in
    js    
  ) objs) in
  Printf.printf "var messages={%s};" (String.concat ",\n" msgs)

    
  
