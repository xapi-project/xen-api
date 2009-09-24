open Printf
open Stringext
open Pervasiveext

open Datamodel_types
open Datamodel
open Datamodel_utils
open Dm_api
open Html_common


let td ?cls ?colspan x = 
  let cls = match cls with
    | None -> [ "class", "noborder" ] | Some c -> [ "class", c ] in
  let colspan = match colspan with
    | None -> [] | Some x -> [ "colspan", x ] in
  Xml.Element("td", cls @ colspan, x)
let tr ?cls x = 
  let cls = match cls with
    | None -> [] | Some c -> [ "class", c ] in
  Xml.Element("tr", cls, x)

let hr = Xml.Element("hr", [], [])
let p = Xml.Element("p", [], [])

let link_to_classes () = href class_index [ Xml.PCData "[ classes ]" ]
(*
  Xml.Element("img", [ "src", !diagram_basename ^ ".small.gif" ], []) 
]
*)

let rec string_of_ty = function
  | String -> "string"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | DateTime -> "datetime"
  | Enum (name, _) -> sprintf "enum %s" name
  | Set t -> sprintf "Set[%s]" (string_of_ty t)
  | Map (a, b) -> sprintf "Map[%s&rarr;%s]" (string_of_ty a) (string_of_ty b)
  | Ref x -> x
  | Record x -> sprintf "Record[%s]" x

let derived_from_field f x = match x.msg_tag with
  | FromField(_, f') -> f = f'
  | _ -> false

let custom x = match x.msg_tag with
  | Custom -> true
  | _ -> false

let from_object x = match x.msg_tag with
  | FromObject (Private _) -> false
  | FromObject _ -> true
  | _ -> false

let stripe = 
  let toggle = ref false in
  fun ?(change=false) xs ->
    if change then toggle := not(!toggle);
    tr ~cls:(if !toggle then "even" else "odd") xs

let message_args m = 
  let params = List.map (fun p -> hyperlink_of_ty p.param_type @ [ Xml.PCData p.param_name ]) m.msg_params in
  let params = 
    if List.length params > 1 
    then List.map (fun x -> x @ [ Xml.PCData "; " ]) (List.rev (List.tl (List.rev params))) @
      [ List.hd (List.rev params) ]
    else params in
  [ Xml.PCData "(" ] @ (List.concat params) @ [ Xml.PCData ")" ]

let decl_of_message m =
  (default [] (may (fun (t, _) -> hyperlink_of_ty t) m.msg_result)) @
    [ Xml.PCData m.msg_name ] @ (message_args m) 

let check = img "check.png"

let opensource release = 
  td ( (*if List.mem "3.0.3" release.opensource then [ check ] else*) [])
let enterprise release = 
  td ( (*if List.mem "closed" release.internal then [ check ] else*) []) 
let implementation release = 
  td ( (*if List.mem "implementation" release.internal then [ check ] else*) [])

let message m = 
  let param p = 
    stripe [ td [ Xml.PCData "" ];
	     td ~cls:"field" ((hyperlink_of_ty p.param_type) @ [ Xml.PCData p.param_name] );
	     td ~colspan:"4" [ Xml.PCData p.param_doc ] ] in
  let header = 
    stripe ~change:true
      [ td ~cls:"field" 
	  (match m.msg_result with Some (t, _) -> hyperlink_of_ty t | None -> [ Xml.PCData "void" ]);
	td ~cls:"bold" [ Xml.PCData m.msg_name ];
	td ~cls:"field" (message_args m);
	opensource m.msg_release;
	enterprise m.msg_release;
	implementation m.msg_release;
      ] in
  let descr = 
    stripe [ td [ Xml.PCData "" ]; td ~colspan:"5" [ Xml.PCData m.msg_doc ] ] in
  [ header ] @ (List.map param m.msg_params) @ [ descr ]

let rec content all_m = 
  let message ?(cls="message") m = 
    let summary = decl_of_message m in
    stripe [ td [ Xml.PCData "" ]; td ~cls ~colspan:"5" summary ] in function
  | Field fld ->
      let field = 
	stripe ~change:true [ td ~cls:"field" (hyperlink_of_ty fld.ty);
			      td ~cls:"bold" [ Xml.PCData (String.concat "/" fld.full_name) ];
			      td [ Xml.PCData fld.field_description ];
			      opensource fld.release;
			      enterprise fld.release;
			      implementation fld.release;
			    ] in
      let derived_ms = List.filter (derived_from_field fld) all_m in
      field :: (List.map (message ~cls:"derived") derived_ms)
  | Namespace (name, contents) ->
      List.concat (List.map (content all_m) contents) 

let obj x =
  let section x = tr [ 
    td ~cls:"section" ~colspan:"3" [ Xml.PCData x ];
    td ~cls:"section" [ Xml.PCData "" (* "OSS" *) ];
    td ~cls:"section" [ Xml.PCData "" (* "XE" *) ];
    td ~cls:"section" [ Xml.PCData "" (* "impl" *) ];
		     ] in
  let description = div "description" [ Xml.PCData x.description ] in
  let contents = List.concat (List.map (content x.messages) x.contents) in
  let messages = List.concat (List.map message (List.filter custom x.messages)) in
  let obj_messages = List.concat (List.map message (List.filter from_object x.messages)) in
  let table = Xml.Element("table", [ "class", "bigtable" ], 
		      [ 
			section "Implicit messages" ] @
			obj_messages @
			[ section "Fields" ] @
			contents @ 
			[ section "Custom messages" ] @
			messages) in
  html x.name ([ link_to_classes (); description; table; link_to_classes ()])

let of_enum name kv_list = 
    let heading = h1 ("enum " ^ name) in
    let table = 
      let pairs = List.map 
	(fun (k, v) -> stripe ~change:true [ td ~cls:"field" [ Xml.PCData k ];
					     td [ Xml.PCData v ] ]) kv_list in
      Xml.Element("table", [ "class", "bigtable" ], pairs) in
    html name [ link_to_classes(); heading; table; link_to_classes() ]

(** A tree of links used for building a navigation bar down the left *)
type tree = 
  | Leaf of (Xml.xml list)
  | Node of string * (tree list)

let images_file = "images/file.gif"
let images_folder = "images/folder.gif"

let rec nested_uls_of_tree = function
  | Leaf x -> li (img images_file :: x)
  | Node (label, []) ->
      li [ img images_folder; Xml.PCData label ]
  | Node (label, children) ->
      li [ img images_folder; Xml.PCData label;
	   ul (List.map nested_uls_of_tree children) ] 
    
(*
let rec trees_from_disk (path: string) = 
  let is_dir x = try (Unix.stat x).Unix.st_kind = Unix.S_DIR with _ -> false in
  let ls x = List.filter (fun x -> not(String.startswith ".") x) (Sys.readdir x) in
  List.map (fun x ->
	      if is_dir
	      then Node(x, trees_from_disk (path ^ "/" ^ x))
	      else Leaf 
			  
*)  
(*
let treeview api = 
  let objects = objects_of_api api in
  let tree_of_enums = 
    let all_types = Types.of_objects objects in
    List.concat (List.map (function Enum (name, _) -> [ Leaf [ Xml.PCData name ] ]
			   | _ -> []) all_types) in
  let tree_of_obj x = 
    Node (x.name,
	  [ Node ("Implicit messages",
		  List.map (fun m -> Leaf [ Xml.PCData (string_of_message m) ]) (List.filter from_object x.messages));
	    Node ("Fields",
		  List.map (fun f -> Leaf [ Xml.PCData (string_of_field f) ]) (fields_of_obj x));
	    Node ("Custom messages",
		  List.map (fun m -> Leaf [ Xml.PCData (string_of_message m) ]) (List.filter custom x.messages));
	  ]) in

  let tree = 
    Node ("API",
	  [ Node ("Enums", tree_of_enums);
	    Node ("Classes", List.map tree_of_obj objects) ]) in

  let html_tree = Xml.Element("ul", [ "id", "browser"; "class", "dir" ], 
			  [ nested_uls_of_tree tree ]) in

  html ~hdr:false ~scripts:[ "apitreeview.js" ] "API" [ html_tree ]
*)

let go api = 
  let objects = objects_of_api api in
(*  List.iter (fun x -> write_html (obj x) (filename_of_object x)) objects; *)
  (* consider all enums *)
  let all_types = Types.of_objects objects in
  List.iter (function Enum (name, kv_list) -> 
	       write_html (of_enum name kv_list) (name ^ ".html")
	     | _ -> ()) all_types
