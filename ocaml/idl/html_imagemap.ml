(** Generate the clickable imagemap *)

open Html_common

let parse_imagemap map image = 
  let doc = Xml.parse_file map in
  let name = match doc with
    | Xml.Element("map", attr, _) ->
	List.assoc "name" attr 
    | _ -> failwith "Failed to extract name from imagemap" in
  [ Xml.Element("img", [ "src", image; "usemap", "#" ^ name ], []); doc ]

let imagemap api = 
(*  let objects = Dm_api.objects_of_api api in*)
(*   let list = flat_list (List.map (fun x -> link_to_object x [ Xml.PCData x.name ]) objects) in*) 
  let map = parse_imagemap (!diagram_basename ^ ".map") (!diagram_basename ^ ".gif") in
  let title = [ h1 "Class relationship diagram";
		p [ Xml.PCData "Click on a class to view the associated fields and messages" ] ] in
  html ~hdr:false "Class index"  (title @ map)

let go api = 
  write_html (imagemap api) class_index
