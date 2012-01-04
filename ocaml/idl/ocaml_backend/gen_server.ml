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
module O = Ocaml_syntax
module DT = Datamodel_types
module DU = Datamodel_utils
module DM = Datamodel
module OU = Ocaml_utils
module Client = Gen_client
open DT

let module_name = "Make"
let async_module_name = "Async"

let _custom = "Custom"
let _forward = "Forward"
let _db_defaults = "Db_actions.DB_Action"
let _concurrency = "Concurrency"

let enable_debugging = ref false


let is_session_arg arg =
  let binding = O.string_of_param arg in
  let converter = O.type_of_param arg in
  ((binding = "session_id") && (converter = "ref_session"))

let from_xmlrpc arg =
	let binding = O.string_of_param arg in
	let converter = O.type_of_param arg in
	Printf.sprintf "let %s = From.%s \"%s\" %s in" binding converter binding binding

let read_msg_parameter msg_parameter =
  from_xmlrpc 
 
let debug msg args =
  if !enable_debugging
  then "D.debug \""^(String.escaped msg)^"\" " ^ (String.concat " " args) ^ ";" else "" 
    
let has_default_args args =
  let arg_has_default arg =
    match arg.DT.param_default with
      None -> false
    | Some _ -> true in
  let any_defaults = List.fold_left (fun e x -> e || x) false (List.map arg_has_default args) in
  any_defaults

(* ------------------------------------------------------------------------------------------
    Code to generate a single operation in server dispatcher
   ------------------------------------------------------------------------------------------ *)

let count_mandatory_message_parameters (msg: message) =
  (* Returns the number of mandatory parameters of a message *)
  let rec count_mandatory_parameters (params: param list) =
    match params with
    | [] -> 0
    | head::tail -> ((match head.param_default with
                      | None -> 1
                      | Some x -> 0) + 
                     (count_mandatory_parameters tail))
  in count_mandatory_parameters msg.msg_params

let operation (obj: obj) (x: message) =
  let msg_params = x.DT.msg_params in
  let msg_params_with_default_values = List.filter (fun p -> p.DT.param_default<>None) msg_params in
  let msg_params_without_default_values = List.filter (fun p -> p.DT.param_default=None) msg_params in

  let msg_without_default_values = {x with DT.msg_params=msg_params_without_default_values} in
  
  let all_args = Client.args_of_message obj x in

  let args_without_default_values = Client.args_of_message obj msg_without_default_values in
  
  (* Constructors use a <struct> on the wire *)
  let is_ctor = x.msg_tag = FromObject(Make) && Client.use_structure_in_ctor in    
    
  (* Result marshaller converts the result to a string for the Task table *)
  let result_marshaller = match x.msg_custom_marshaller, x.msg_result with
    | true, _ -> "(fun x -> [ x ])"
    | false, Some (ty,_) -> Printf.sprintf "(fun x -> [ To.%s x])" (OU.alias_of_ty ty)
    | false, None -> "(fun x -> [])" in

  let wire_name = DU.wire_name ~sync:true obj x in
  
  let string_args = if is_ctor then [O.string_of_param Client.session;"__structure"]
    else List.map O.string_of_param args_without_default_values in
  let is_non_constructor_with_defaults = not is_ctor && (has_default_args x.DT.msg_params) in
  let arg_pattern = String.concat "::" string_args in
  let arg_pattern =
    if is_non_constructor_with_defaults then arg_pattern^"::default_args"
    else arg_pattern^"::[]" in
  let name_pattern_match =
    Printf.sprintf
      "| \"%s\" ->\n" wire_name in

  (* Lookup the various fields from the constructor record *)
  let from_ctor_record =
    let fields = Client.ctor_fields obj in
    let of_field f =
      let binding = O.string_of_param (Client.param_of_field f) in
      let converter = "From." ^ (OU.alias_of_ty f.DT.ty) in
      let lookup_expr =
	match f.DT.default_value with
	  None -> Printf.sprintf "(my_assoc \"%s\" __structure)" (DU.wire_name_of_field f)
	| Some default ->
	    Printf.sprintf "(if (List.mem_assoc \"%s\" __structure) then (my_assoc \"%s\" __structure) else %s)"
	      (DU.wire_name_of_field f) (DU.wire_name_of_field f)
	      ("Xml.parse_string (\""^(Xml.to_string (Datamodel_values.to_xml default))^"\")") in
      Printf.sprintf "let %s = %s \"%s\" %s in"
	binding converter binding lookup_expr in
    String.concat "\n"
      ("let __structure = From.structure __structure in" ::
	 (List.map of_field fields)) in
    
  (* impl_fn = something like "VM.make ~__context" *)
  let impl_fn = 
    (* filter out the session_id *)
    let args_without_session = List.filter (function O.Named("session_id", _) -> false | _ -> true) all_args in
      Printf.sprintf "%s.%s %s %s"
	(OU.ocaml_of_obj_name obj.DT.name)
	x.msg_name
	("~__context:" ^ Gen_common.context_with_correct_database)
	(String.concat "" (List.map (fun arg -> " ~" ^ (O.string_of_param arg)) args_without_session)) in
        
  let has_async = Client.has_async x in
    
  let comments = List.concat [
			       if Gen_empty_custom.operation_requires_side_effect x
			       then [ "(* has side-effect (with locks and no automatic DB action) *)" ]
			       else [ "(* has no side-effect; should be handled by DB action *) "];
			       if has_async
			       then [ "(* has asynchronous mode *)" ]
			       else [ "(* has no asynchronous mode *)" ]
			     ] in

  (* Generate the unmarshalling code *)
  let rec add_counts i l =
    match l with
      [] -> []
    | x::xs -> (i,x)::(add_counts (i+1) xs) in
  let has_session_arg =
      if is_ctor then is_session_arg Client.session
      else List.exists (fun a->is_session_arg a) args_without_default_values
  in
  let rbac_check_begin = if has_session_arg
    then [
			"let arg_names = "^(List.fold_right (fun arg args -> "\""^arg^"\"::"^args) string_args (if is_non_constructor_with_defaults then ((List.fold_right (fun dp ss->"\""^(dp.DT.param_name)^"\"::"^ss) msg_params_with_default_values "")^"[]") else "[]"))^" in";
			"let key_names = "^(List.fold_right (fun arg args -> "\""^arg^"\"::"^args) (List.map (fun (k,_)->k) x.msg_map_keys_roles) "[]")^" in";
			"let rbac __context fn = Rbac.check session_id __call ~args:(arg_names,__params) ~keys:key_names ~__context ~fn in"]
    else [
    "let rbac __context fn = fn() in"
    ]
  in
  let rbac_check_end = if has_session_arg then [] else [] in
  let unmarshall_code =
    (
      (* If we're a constructor then unmarshall all the fields from the constructor record, passed as a struct *)
      if is_ctor then [from_xmlrpc Client.session; from_ctor_record]
	(* Otherwise, go read non-default fields from pattern match; if we have default fields then we need to
	   get those from the 'default_fields' arg *)
      else  List.map from_xmlrpc args_without_default_values)

    (* and for every default value we try to get this from default_args or default it *)
    @ (
      List.map
	(fun (param_count, default_param) ->
	   let param_name = default_param.DT.param_name in
	   let param_type = OU.alias_of_ty default_param.DT.param_type in
	   let try_and_get_default = Printf.sprintf "Server_helpers.nth %d default_args" param_count in
	   let default_value =
	     match default_param.DT.param_default with
	       None -> "** EXPECTED DEFAULT VALUE IN THIS PARAM **"
	     | Some default ->
		 "Xml.parse_string (\""^(Xml.to_string (Datamodel_values.to_xml default))^"\")" in
	   Printf.sprintf "let %s = From.%s \"%s\" (try %s with _ -> %s) in"
	     param_name param_type param_name try_and_get_default default_value
	)
	(add_counts 1 msg_params_with_default_values))
  in
  
  let may_be_side_effecting msg =
    match msg.msg_tag with
	FromField (Setter, _ ) | FromField (Add, _) | FromField(Remove, _) -> true
      | FromField _ -> false
      | FromObject Make | FromObject Delete | FromObject (Private Copy) -> true
      | FromObject _ -> false
      | Custom -> true in
    
		let session_check_exp =
			if x.msg_session
				then [ "Session_check.check " ^ (string_of_bool x.msg_pool_internal) ^ " session_id;" ]
				else []
			in

	let gen_body () =
		let ret = match x.msg_result with Some(ty, _) -> Some ty | _ -> None in
		let type_xml = Datamodel_types.TypeToXML.marshal ret in
		let module_prefix = if (Gen_empty_custom.operation_requires_side_effect x) then _custom else _db_defaults in
		let common_let_decs =
			[
				"let marshaller = "^result_marshaller^" in";
				"let local_op = fun ~__context ->(rbac __context (fun()->("^module_prefix^"."^impl_fn^"))) in";
				"let supports_async = "^(if has_async then "true" else "false")^" in";
				"let generate_task_for = "^(string_of_bool (not (List.mem obj.name DM.no_task_id_for)))^" in" ] in
		let side_effect_let_decs =
			if Gen_empty_custom.operation_requires_side_effect x then
				[
					Printf.sprintf "let forward_op = fun ~local_fn ~__context -> (rbac __context (fun()-> (%s.%s) )) in" _forward impl_fn
				]
			else 
				[
					Printf.sprintf "%s \"%s\";"
					(if may_be_side_effecting x then "ApiLogSideEffect.info" else "ApiLogRead.info")
					wire_name
				] in

		let body_exp =
			[
				Printf.sprintf "let resp = Server_helpers.do_dispatch %s %s \"%s\" __async supports_async __call local_op marshaller fd http_req __label generate_task_for in"
				(if x.msg_session then "~session_id" else "")
				(if Gen_empty_custom.operation_requires_side_effect x then "~forward_op" else "")
				(Xml.to_string type_xml);
				(*	"P.debug \"Server XML response: %s\" (Xml.to_string (XMLRPC.To.methodResponse resp));"; *)
				"resp"
			] in
		common_let_decs @ side_effect_let_decs @ body_exp in

  let all = String.concat "\n        "
    (comments @ unmarshall_code @ session_check_exp @ rbac_check_begin @ (gen_body()) @ rbac_check_end) in
    
    ("    " ^ name_pattern_match ^ "\n" ^
    ("begin\n"
   ^ "    match __params with\n"
   ^ "    | " ^ arg_pattern ^ " -> "
   ^ "         begin\n " ^ all ^ "\n"
   ^ "         end\n"
   ^ "    | _ -> "
   ^ "        Server_helpers.parameter_count_mismatch_failure __call " ^ 
              "\"" ^ (string_of_int (count_mandatory_message_parameters x)) ^ "\"" ^ 
              " (string_of_int ((List.length __params) - " ^ (if x.msg_session then "1" else "0") ^ "))"
   ^ "\nend"))


(* ------------------------------------------------------------------------------------------
    Code to generate whole module
   ------------------------------------------------------------------------------------------ *)      

let gen_module api : O.Module.t =
  (* For testing purposes the ocaml client and server are kept in sync *)
  let api = Client.client_api ~sync:true api in
  let obj (obj: obj) = List.map (operation obj) obj.messages in
  let all_objs = Dm_api.objects_of_api api in      

  O.Module.make
    ~name:module_name
    ~args:[_custom ^ " : Custom_actions." ^ Gen_empty_custom.signature_name;
	   _forward ^ " : Custom_actions." ^ Gen_empty_custom.signature_name]
    ~preamble:[
      "module D = Debug.Debugger(struct let name = \"dispatcher\" end)";
      "module ApiLogRead = Debug.Debugger(struct let name = \"api_readonly\" end)";
      "module ApiLogSideEffect = Debug.Debugger(struct let name = \"api_effect\" end)"
(*      "exception Invalid_operation"; *)
    ]
    ~elements:[
      O.Module.Let (
	O.Let.make
	  ~name: "dispatch_xml"
	  ~params: [ O.Anon(Some "http_req", "Http.Request.t"); 
		     O.Anon(Some "fd", "Unix.file_descr");
		     O.Anon(Some "xml", "xml") ]
	  ~ty: "response"
	  ~body: (
	    [ 
	      "let __call, __params = From.methodCall xml in";
	      "let __call, __params = transform __call __params in (* Potentially rewrite call, translating 'syntactic sugar' calls server-side  *) ";
	      "let __async = Server_helpers.is_async __call in";
	      "let __label = __call in";
	      "let __call = if __async then Server_helpers.remove_async_prefix __call else __call in";
	      "let subtask_of = if http_req.Http.Request.task <> None then http_req.Http.Request.task else http_req.Http.Request.subtask_of in";
	      "Server_helpers.exec_with_new_task (\"dispatch:\"^__call^\"\") ?subtask_of:(Pervasiveext.may Ref.of_string subtask_of) (fun __context ->";
(*
	      "if not (Hashtbl.mem supress_printing_for_these_messages __call) then ";
	      debug "%s %s" [ "__call"; "(if __async then \"(async)\" else \"\")" ];
*)
	      "Server_helpers.dispatch_exn_wrapper (fun () -> (match __call with ";
	    ] @ (List.flatten (List.map obj all_objs)) @ [
		"| \"system.listMethods\" -> ";
		"  XMLRPC.Raw [ To.string_set [" ] @
		begin 
		  let objmsgs obj = List.map (fun msg -> Printf.sprintf "\"%s\";" (DU.wire_name ~sync:true obj msg)) obj.messages in
		  let allmsg = List.map (fun obj -> String.concat "" (objmsgs obj)) all_objs in
		  allmsg
		end @ [
			" ]]";
			"| func -> ";
			"  if (try Scanf.sscanf func \"system.isAlive:%s\" (fun _ -> true) with _ -> false)";
			"  then XMLRPC.Success __params";
			"  else begin";
			"    if (try Scanf.sscanf func \"unknown-message-%s\" (fun _ -> false) with _ -> true)";
			"    then " ^ (debug "Unknown rpc \"%s\"" [ "__call" ]);
			"    Server_helpers.unknown_rpc_failure func";
			"  end";
		")))";
	    ]
	  ) ()
      );
		O.Module.Let (
			       O.Let.make
				 ~name: "dispatch"
				 ~params: [
					    O.Anon(Some "http_req", "Http.Request.t"); 
					    O.Anon(Some "fd", "Unix.file_descr");
					    O.Anon(Some "body", "string")
					  ]
				 ~ty: "unit"
				 ~body: [
					  "let xml = Xml.parse_string body in";
					  "  dispatch_xml http_req fd xml"
					] ())
	      ] ()


