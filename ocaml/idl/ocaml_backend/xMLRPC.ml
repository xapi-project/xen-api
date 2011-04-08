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
module D=Debug.Debugger(struct let name="xmlrpc" end)
open D

exception RunTimeTypeError of string * Xml.xml

let rtte name xml =
	error "Error: name='%s'; xml= %s" name (String.escaped (Xml.to_string xml));
	raise (RunTimeTypeError(name, xml))

type xmlrpc = Xml.xml

let pretty_print = function
	| Xml.Element(tag,_,_) -> "Element=" ^ String.escaped tag
	| Xml.PCData d         -> "PCData=" ^ String.escaped d

type response = 
  | Success of Xml.xml list (** normal result *)
  | Failure of string * (string list) (** failure/ exception in high-level code *)
  | Fault of (int32 * string) (** error in the XMLRPC handling *)
  | Raw of Xml.xml list (** Skipping the status *)

module ToString = struct
  let int64 = Int64.to_string
  let double = Printf.sprintf "%0.16g" 
  let string x = x
  let reference = Ref.string_of
end

module FromString = struct
  let int64 = Int64.of_string
  let double = float_of_string
  let string x = x
  let reference = Ref.of_string
end

module To = struct
  let pcdata string = Xml.PCData string

  let box tag vs = Xml.Element(tag, [], vs)

  let value v = box "value" [v]

  let name v = box "name" [pcdata v]

  let array vs = value (box "array" [box "data" vs])

  let boolean b = value (box "boolean" [pcdata (if b then "1" else "0")])

  let datetime s = value (box "dateTime.iso8601" [pcdata (Date.to_string s)])

  let double x = 
    let txt = match classify_float x with
      | FP_nan -> "NaN"
      | FP_infinite -> "NaN"
      | _ -> Printf.sprintf "%0.16g" x in
    value (box "double" [pcdata txt])

  let int n = value (box "i4" [pcdata (Int32.to_string n)])

  let methodCall name params = 
    box "methodCall"
      [box "methodName" [pcdata name];
       box "params" (List.map (fun param -> box "param" [param]) params)]

  let string = function
      "" -> box "value" []
    | string -> value (pcdata string)

  let structure fields =
    value (box "struct" (List.map (fun (k, v) -> box "member" [name k; v]) fields))

  let fault n s =
    let faultCode = box "member" [name "faultCode"; int n] in
    let faultString = box "member" [name "faultString"; string s] in
    box "fault" [box "struct" [faultCode; faultString]]

  let success (v: Xml.xml) = 
    structure [ "Status", string "Success";
		"Value", v ]

  let error code params = 
    let arr = string code :: (List.map string params) in
    structure [ "Status", string "Failure";
		"ErrorDescription", (array arr) ]

  let methodResponse response =
    box "methodResponse"
      [match response with
       | Success [] ->
	   let result = success (string "") in
	   box "params" [ box "param" [ result ] ]
       | Success [param] ->
	   let result = success param in
	   box "params" [ box "param" [ result ] ]
       | Failure(code, params) ->
	   let result = error code params in
	   box "params" [ box "param" [ result ] ]
       | Fault(n, s) ->
	   box "fault" [structure ["faultCode", int n;
				   "faultString", string s]]
       | Raw [param] ->
	   box "params" [ box "param" [param]]
	   | _ -> failwith "To.methodResponse"
	  ]
end

module From = struct
  let id x = x

  let pcdata f = function
    | Xml.PCData string -> f string
    | xml -> rtte "pcdata" xml
	
  let unbox ok f = function
    | Xml.Element(s, [], data) when List.mem s ok -> f data
    | xml -> rtte (Printf.sprintf "unbox: %s should contain '%s'" (pretty_print xml) (List.hd ok)) xml
	
  let singleton ok f xml =
    unbox ok (function [x] -> f x | y -> rtte (Printf.sprintf "singleton: {%s} should be the singleton {%s}" (String.concat ", " (List.map pretty_print y)) (List.hd ok)) xml) xml

  let pair ok f1 f2 xml =
    unbox ok (function [v1; v2] -> f1 v1, f2 v2 | _ -> rtte "pair " xml) xml

  let value f xml = singleton ["value"] f xml

  (* <name> is only ever used inside a <struct><member>
     CA-20001: it is possible for <name> to be blank *)
  let name f xml = unbox ["name"]
    (function
     | [ Xml.PCData string ] -> f string
     | [ ] -> 
	 debug "encountered <name/> within a <structure>";
	 f ""
	 | x -> rtte "From.name: should contain PCData" xml
    ) xml

  let check expected xml got =
    if got <> expected then rtte ("check " ^ expected) xml

  let nil = value (unbox ["nil"] (fun _ -> ()))

  let array f = value (singleton ["array"] (unbox ["data"] (List.map f)))

  let boolean = value (singleton ["boolean"] ((<>) (Xml.PCData "0")))

  let datetime x = Date.of_string (value (singleton ["dateTime.iso8601"] (pcdata id)) x)

  let double = value (singleton ["double"] (pcdata float_of_string))

  let int = value (singleton ["i4"; "int"] (pcdata Int32.of_string))

  let methodCall xml =
    pair ["methodCall"]
      (singleton ["methodName"] (pcdata id))
      (unbox ["params"] (List.map (singleton ["param"] id)))
    xml

  let string = function
    | Xml.Element("value", [], [Xml.PCData s])                  -> s
    | Xml.Element("value", [], [Xml.Element("string", [], [])])
    | Xml.Element("value", [], [])                              -> ""
    | xml                                                       -> value (singleton ["string"] (pcdata id)) xml

  let structure : Xml.xml -> (string * Xml.xml) list =
    singleton ["value"] (unbox ["struct"] (List.map (pair ["member"] (name id) id)))

  let success =
    unbox ["params"] (List.map (singleton ["param"] id))

  let status xml =
    let bindings = structure xml in
    try match string (List.assoc "Status" bindings) with
    | "Success" -> Success [ List.assoc "Value" bindings ]
    | "Failure" -> begin
	match array id (List.assoc "ErrorDescription" bindings) with
	| [] -> rtte "Empty array of error strings" (Xml.PCData "")
	| code::strings ->
	    Failure (string code, List.map string strings)
      end
    | _ -> raise Not_found
    with Not_found -> rtte "Status" xml

  let fault f =
    let aux m =
      int (List.assoc "faultCode" m), string (List.assoc "faultString" m) in
    singleton ["fault"] (fun xml -> aux (structure xml))

  let methodResponse xml =
    singleton ["methodResponse"]
      (function
       | Xml.Element("params", _, _) as xml -> begin match success xml with
       | [ xml ] -> status xml
       | _ -> rtte "Expected single return value (struct status)" xml
	 end
       | Xml.Element("fault", _, _) as xml ->
	   Fault (fault id xml)
       | xml -> rtte "response" xml)
   xml
end



module TypeToXML = struct
  open Datamodel_types

  let string x = Xml.Element(x, [], []) 
  let box tag vs = Xml.Element(tag, [], vs)
    
  let rec marshal_ = function
    | String         -> string "string"
    | Int            -> string "int"
    | Float          -> string "float"
    | Bool           -> string "bool"
    | DateTime       -> string "datetime"
    | Enum (name, _) -> box "enum" [ string name ]
    | Ref x          -> box "ref" [ string x ]
    | Set ty         -> box "set" [ marshal_ ty ]
    | Map (a, b)     -> box "map" [ marshal_ a; marshal_ b ]
    | Record x       -> box "record" [ string x ]

  let marshal = function
    | None   -> string "none"
    | Some x -> box "some" [ marshal_ x ]

  let rec unmarshal_ = function
    | Xml.Element("string", [], [])                    -> String
    | Xml.Element("int", [], [])                       -> Int
    | Xml.Element("float", [], [])                     -> Float
    | Xml.Element("datetime", [], [])                  -> DateTime
    | Xml.Element("enum", [], [Xml.Element(name, [], [])]) -> Enum(name, [])
    | Xml.Element("ref", [],  [Xml.Element(name, [], [])]) -> Ref name
    | Xml.Element("set", [], [t])                      -> Set(unmarshal_ t)
    | Xml.Element("map", [], [a;b])                    -> Map(unmarshal_ a, unmarshal_ b)
    | _ -> failwith "Type unmarshal error"

  let unmarshal = function
    | Xml.Element("none", [], [])  -> None
    | Xml.Element("some", [], [x]) -> Some (unmarshal_ x)
    | _ -> failwith "Type unmarshal error"

end

module Value = struct
  type t = 
    | String of string 
    | Int of int32
    | Float of float 
    | Bool of bool
    | DateTime of Date.iso8601
    | Enum of string
    | Set of t list
    | Map of (t * t) list
    | Ref of string

  let rec to_string = function
    | String x -> "\"" ^ String.escaped x ^ "\""
    | Int x -> Int32.to_string x
    | Float x -> string_of_float x
    | Bool x -> string_of_bool x
    | DateTime x -> Date.to_string x
    | Enum x -> x
    | Set xs -> "[ " ^ (String.concat "; " (List.map to_string xs)) ^ " ]"
    | Map kv -> "{ " ^ (String.concat "; " (List.map (fun (k, v) -> to_string k ^ " -> " ^ (to_string v)) kv)) ^ " }"
    | Ref x -> "Ref: " ^ x

  type result = 
    | OK of t option 
    | Error of string * (string list)
    | Bad of (int32 * string)

  module DT = Datamodel_types
  let interpret' dm_type xml : result = 

    let set f xml =
      From.array f xml in

    let map fk fv xml =
      let f m = fk (List.assoc "key" m), fv (List.assoc "value" m) in
      set (fun b -> f (From.structure b)) xml in

    match From.methodResponse xml with
    | Success [ x ] ->
	let rec f t x = match t with
	  | DT.String         -> String (From.string x)
	  | DT.Int            -> Int (From.int x)
	  | DT.Float          -> Float (From.double x)
	  | DT.Bool           -> Bool (From.boolean x)
	  | DT.DateTime       -> DateTime (From.datetime x)
	  | DT.Enum (name, _) -> Enum (From.string x)
	  | DT.Set t'         -> Set (set (f t') x)
	  | DT.Map (a, b)     -> Map (map (f a) (f b) x)
	  | DT.Ref _          -> Ref (From.string x) 
	  | DT.Record _       -> rtte "Value.interpret Record not supported" x
in
	begin match dm_type with
	| Some t -> OK (Some (f t x))
	| None -> OK None
	end
    | Success [] ->
	OK None
    | Failure (code, params) -> Error (code, params)
    | Fault x -> Bad x
    | _ -> failwith "Can't cope with this!"

  let interpret (ty: string) (v: string) : result option = 
    let parse x : Xml.xml option = try Some(Xml.parse_string x) with _ -> 
      Printf.fprintf stderr "Error reading string: %s" x; None in
    match (parse ty), (parse v) with
    | Some ty, Some v -> Some(interpret' (TypeToXML.unmarshal ty) v)
    | _, _ -> None

end
