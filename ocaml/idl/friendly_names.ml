open Dm_api
open Dm_api
open Datamodel_utils
open Stringext

module DT=Datamodel_types

let escape_rules = ['<', "&lt;";'>', "&gt;"]

(* Non-autogen things that are found in the FriendlyNames resx *)
let others : (string * string) list ref = ref []

let escape s = 
  String.escaped ~rules:escape_rules s

(* The hashtable where keys are the 'name' attribute of data tags, and values are the contents of their single child 'value' tag *)
let friendly_names = Hashtbl.create 10


let startswith str prefix =
  String.length str >= String.length prefix && String.compare (String.sub str 0 (String.length prefix)) prefix = 0


let trim x =
  if (startswith x "Label-") then String.sub x 5 (String.length x - 5)
  else if (startswith x "Description-") then String.sub x 12 (String.length x - 12)
  else x


(* A custom sorter for the hashtable. We trim off the Label- and Description- and sort by what's left. *)
let sorter x y =
  String.compare (trim x) (trim y)


(* objects_of_api is defined in dm_api.ml *)
let rec friendly_names_all api =
  List.iter update_entries (objects_of_api api);
  let keys   = Hashtbl.fold get_keys friendly_names [] in
  let keys'  = List.sort sorter keys in
  let values = List.map get_value keys' in
  let hashTableContents = List.combine keys' values (* Put the contents of the hash table into (key, value) tuples *) in
    (* Lastly add on the non-autogen things in the order they appeared in the resx *)
    (hashTableContents @ (List.rev !others))


and get_keys key _ tail =
  key :: tail


and get_value key = 
  Hashtbl.find friendly_names key


(* Iterates over the fields of the given api class *)
and update_entries obj =
  List.iter (update_entry obj.DT.name) (fields_of_obj obj)


(* Takes a field on an api object and puts default Label- and Description- entries in the hashtable if there aren't already custom ones there *)
and update_entry classname field =
  let full_name =  classname ^ "." ^ (String.concat "_" field.DT.full_name) in
  let label = String.concat "" ["Label-"; full_name] in
  let desc = String.concat "" ["Description-"; full_name] in
  begin
    if not (Hashtbl.mem friendly_names label) then
      Hashtbl.add friendly_names label full_name;
    if not (Hashtbl.mem friendly_names desc) then
      Hashtbl.add friendly_names desc (escape field.DT.field_description)
  end


(* Fills the hashtable 'friendly_names' with anything uin the given resx starting Label- or Description- *)
(* Assumed non-autogen strings with any other prefix are put into (key, value) pair list 'others' *)
let parse_resx filename =
  let rec get_value_node = function
    | Xml.Element("value", _, [Xml.PCData v]) :: _ -> v
    | _ :: xs -> get_value_node xs
    | [] -> ""
  in

  let update_entry attrs children =
    let key = List.assoc "name" attrs in
    let value = get_value_node children in
      if (startswith key "Label-" || startswith key "Description-") then
        Hashtbl.replace friendly_names key (escape value)
      else
        others := (key, (escape value)) :: !others
  in

  let rec parse_xml = function
    | Xml.Element("root", _, children) ->
	List.iter parse_xml children
    | Xml.Element("data", attrs, children) ->
        update_entry attrs children
    | _ -> ()
  in
    try
      parse_xml (Xml.parse_file filename)
    with
      Xml.Error e as exn ->
        Printf.eprintf "%s\n%!" (Xml.error e);
        raise exn
