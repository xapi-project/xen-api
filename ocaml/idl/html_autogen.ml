open Printf
open Stringext

open Datamodel_types
open Datamodel
open Datamodel_utils
open Dm_api
open Html_common
open Html_types

let prefix = "../../../"

(** Return an HTML fragment which links to all the fields and messages within a class *)
let class_summary x = 
  let description = div "description" [ Xml.PCData x.description ] in
  
  let fields = fields_of_obj x in
  let implicit_messages = List.filter from_object x.messages in
  let explicit_messages = List.filter custom x.messages in
  
  let field f = 
    let ty, name = strings_of_field f in
    li [ span "type" [ Xml.PCData ty ];
	 span "id" [ link_to_field ~prefix x f [ Xml.PCData name ] ];
	 p [ Xml.PCData ( (if f.release.internal_deprecated_since = None
		       then "" else "[deprecated]") ^ f.field_description ) ] ] in
  let implicit_m m = 
    let ret, name, args = strings_of_message x m in
    li [ Xml.PCData ret;
	 span "id" [ link_to_implicit_msg ~prefix m [ Xml.PCData name ] ];
	 span "type" [ Xml.PCData args ];
	 p [ Xml.PCData ( (if m.msg_release.internal_deprecated_since = None
		       then "" else "[deprecated]") ^ m.msg_doc ) ] ] in
  let explicit_m m = 
    let ret, name, args = strings_of_message x m in
    li [ span "type" [ Xml.PCData ret ];
	 span "id" [ link_to_custom_msg ~prefix m [ Xml.PCData name ] ];
	 span "type" [ Xml.PCData args ];
	 p [ Xml.PCData ( (if m.msg_release.internal_deprecated_since = None
		       then "" else "[deprecated]") ^ m.msg_doc ) ] ] in
  let dep = 
    if x.obj_release.internal_deprecated_since = None
    then []
    else [ div "tableheader" [ Xml.PCData "Warning" ];
	   ul [ li [ Xml.PCData "Class is deprecated" ] ] ] in
  p (dep @ [ description; div "tableheader" [ Xml.PCData "Fields" ];
	     ul (List.map field fields);
	     div "tableheader" [ Xml.PCData "Implicit messages" ];
	     ul (List.map implicit_m implicit_messages);
	     div "tableheader" [ Xml.PCData "Explicit messages" ];
	     ul (List.map explicit_m explicit_messages) ])
  
let field_summary obj f = 
  let table = Xml.Element("table", [], 
		      [ tr [ td [ span "key" [ Xml.PCData "Type:" ] ]; td [ span "type" [ Xml.PCData (string_of_ty f.ty) ] ] ];
			tr [ td []; td [ info_of_ty f.ty ] ];
			tr [ td [ span "key" [ Xml.PCData "Name:" ] ]; td [ span "id" [ Xml.PCData (String.concat "_" f.full_name) ] ] ];
			tr [ td []; td [ Xml.PCData f.field_description ] ];
		      ]) in
  let is_ro f = f.qualifier <> RW in
  let is_deprecated f = f.release.internal_deprecated_since <> None in
  let accessors = List.filter on_client_side (all_new_messages_of_field obj f) in
  let li_of_m m = 
    let ret, name, args = strings_of_message obj m in
    li [ span "type" [ Xml.PCData ret ];
	 span "id"   [ Xml.PCData name ];
	 span "type" [ Xml.PCData args ];
	 p [ Xml.PCData m.msg_doc ] ] in
  let accessors = ul (List.map li_of_m accessors) in

  let ro_ness = Xml.PCData (if is_ro f then "Field is read-only" else "Field is read-write") in
  let deprecated_ness = Xml.PCData (if is_deprecated f then "Warning: Field is deprecated" else "") in
  p [ table; ro_ness; deprecated_ness;
      div "tableheader" [ Xml.PCData "Accessors" ];
      accessors ]

let message_summary obj m = 
  let hdr x = tr [ td ~colspan:"3" ~cls:"tableheader" [ Xml.PCData x ] ] in
  let param_hdr = 
    let td = td ~cls:"tableheader_sub" in
    tr [ td [ Xml.PCData "name" ]; td [ Xml.PCData "type" ]; td [ Xml.PCData "description" ] ] in
  let error_hdr = 
    let td = td ~cls:"tableheader_sub" in
    tr [ td [ Xml.PCData "name" ]; td [ Xml.PCData "arguments" ]; td [ Xml.PCData "description" ] ] in
  let result_hdr = 
    [ hdr "Return value";
      tr [ td [ ]; td ~cls:"tableheader_sub" [ Xml.PCData "type" ]; td ~cls:"tableheader_sub" [ Xml.PCData "description" ] ] ] in

  let result = match m.msg_result, m.msg_custom_marshaller with
    | None, false -> []
    | None, true ->  result_hdr @

		     [ tr [ td [];
			   td [ Xml.PCData "raw XMLRPC" ];
			   td [ Xml.PCData "see notes below" ] ] ]
    | Some (ty, desc), _ -> result_hdr @
			    [ tr [ td [ ]; 
				   td [ Xml.PCData (string_of_ty ty) ];
				   td [ Xml.PCData desc ] ] ] in

  let parameter p = 
    tr [ td [ Xml.PCData p.param_name ];
	 td [ Xml.PCData (string_of_ty p.param_type) ];
	 td [ Xml.PCData p.param_doc ] ] in
  
  let error e = 
    tr [ td [ Xml.PCData e.err_name ];
	 td [ Xml.PCData (String.concat ", " e.err_params) ];
	 td [ Xml.PCData e.err_doc ] ] in

  let deprecated = 
    if m.msg_release.internal_deprecated_since = None
    then []
    else [ li [ Xml.PCData "Warning: Message is deprecated" ] ] in
  let sync = 
    if m.msg_async 
    then "Message has both a synchronous and asynchronous version"
    else "Message has only a synchronous version" in
  let tag = match m.msg_tag with
    | FromField(op, f) -> "Message is derived from field " ^ (String.concat "_" f.full_name)
    | FromObject op -> "This is an implicit message, derived from its class"
    | Custom -> "This is a custom message" in

  let table = Xml.Element("table", [], 
		      result 
		      @ [ hdr "Parameters" ]
		      @ [ param_hdr ]
		      @ (List.map parameter (expand_params obj m))
		      @ [ hdr "Errors" ]
		      @ [ error_hdr ]
		      @ (List.map error m.msg_errors)
		     ) in
  p [ p [ Xml.PCData m.msg_doc ];
      table;
      ul (deprecated @ [ li [ Xml.PCData sync ];
			 li [ Xml.PCData tag ] ]) ] 
			

let go api = 
  let objects = objects_of_api api in
  List.iter (fun x ->
	       let html = class_summary x in
	       let filename = filename_of_class ~ext:autogen x in
	       write_html html filename;
	       (* Fields *)
	       List.iter (fun f ->
			    let html = field_summary x f in
			    let filename = filename_of_field ~ext:autogen x f in
			    write_html html filename)
		 (fields_of_obj x);
	       (* Messages *)
	       let custom = List.filter custom x.messages in
	       let implicit = List.filter from_object x.messages in
	       List.iter (fun m -> write_html (message_summary x m) (filename_of_custom_msg ~ext:autogen m)) custom;
	       List.iter (fun m -> write_html (message_summary x m) (filename_of_implicit_msg ~ext:autogen m)) implicit

	    ) objects

