(* Generate some extra information about types of fields and message parameters *)

open Datamodel_types
open Html_common
open Printf

(** A quick description of the type *)
let info_of_ty = function
  | String -> Xml.PCData "UTF-8 encoded character data"
  | Int -> Xml.PCData "64-bit integer"
  | Float -> Xml.PCData "IEEE-754 floating point number"
  | Bool -> Xml.PCData "boolean"
  | DateTime -> Xml.PCData "ISO-8601 encoded date (e.g 2008-01-01T12:34:00Z)"
  | Enum (name, kv) ->
      Xml.Element("table", [ "class", "enum" ], 
	      (tr [ td [ Xml.PCData "Value" ]; td [ Xml.PCData "Description" ] ])::
	      List.map (fun (k, v) -> tr [ td [ Xml.PCData k ]; td [ Xml.PCData v ] ]) kv)
  | Set t -> Xml.PCData (sprintf "an unordered set of entities of type: %s" (string_of_ty t))
  | Map (a, b) -> Xml.PCData (sprintf "mapping from %s to %s" (string_of_ty a) (string_of_ty b))
  | Ref x -> Xml.PCData (x ^ " reference")
  | Record x -> Xml.PCData (sprintf "all the fields of an object of type %s" x)
