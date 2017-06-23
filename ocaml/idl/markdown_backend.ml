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
open Printf

open Datamodel_types
open Datamodel
open Datamodel_utils
open Dm_api

open Stdext
open Xstringext
open Pervasiveext

let escape s =
  let sl = String.explode s in
  let esc_char =
    function
    | '\\' -> "&#92;"
    | '*' -> "&#42;"
    | '_' -> "&#95;"
    | '{' -> "&#123;"
    | '}' -> "&#125;"
    | '[' -> "&#91;"
    | ']' -> "&#93;"
    | '(' -> "&#40;"
    | ')' -> "&#41;"
    | '>' -> "&gt;"
    | '<' -> "&lt;"
    | '#' -> "&#35;"
    | '+' -> "&#43;"
    | '-' -> "&#45;"
    | '!' -> "&#33;"
    | c -> String.make 1 c in
  let escaped_list = List.map esc_char sl in
  String.concat "" escaped_list

let is_prim_type = function
  | String | Int | Float | Bool | DateTime -> true
  | _ -> false

let is_prim_opt_type = function
  | None -> true
  | Some (ty,_) -> is_prim_type ty

let rec of_ty_verbatim = function
  | String -> "string"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | DateTime -> "datetime"
  | Enum (name, things) -> name
  | Set x -> sprintf "%s set" (of_ty_verbatim x)
  | Map (a, b) -> sprintf "(%s -> %s) map" (of_ty_verbatim a) (of_ty_verbatim b)
  | Ref obj -> obj ^ " ref"
  | Record obj -> obj ^ " record"


let rec of_ty = function
  | String -> "string"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | DateTime -> "datetime"
  | Enum (name, things) -> escape name
  | Set x -> (of_ty x) ^ " set"
  | Map (a, b) -> "(" ^ (of_ty a) ^ " &rarr; " ^ (of_ty b) ^ ") map"
  | Ref obj -> (escape obj) ^ " ref"
  | Record obj -> (escape obj) ^ " record"

let of_ty_opt = function
    None -> "void" | Some(ty, _) -> of_ty ty

let of_ty_opt_verbatim = function
    None -> "void" | Some(ty, _) -> of_ty_verbatim ty

let desc_of_ty_opt = function
    None -> "" | Some(_, desc) -> desc

let string_of_qualifier = function
  | StaticRO   -> "_RO/constructor_"
  | DynamicRO  -> "_RO/runtime_"
  | RW         -> "_RW_"


let string_of_open_product release =
  if release.internal_deprecated_since = None then "" else "**Deprecated.** "

(* Make a markdown section for an API-specified message *)
let markdown_section_of_message printer x =
  let return_type = of_ty_opt_verbatim x.msg_result in

  printer (sprintf "#### RPC name: %s" (escape x.msg_name));
  printer "";
  if x.msg_release.internal_deprecated_since <> None then
  begin
    printer "This message is deprecated.";
    printer ""
  end;
  printer "_Overview:_";
  printer "";
  printer (escape x.msg_doc);
  printer "";
  printer "_Signature:_";
  printer "";
  printer "```";
  printer (of_ty_opt_verbatim x.msg_result);
  printer (sprintf "%s (%s)"
    x.msg_name
    (String.concat ", "
      ((if x.msg_session then "session_id s" else "")::
        (List.map (fun p -> of_ty_verbatim p.param_type ^ " " ^ p.param_name) x.msg_params)))
  );
  printer "```";

  if x.msg_params <> [] then begin
    printer "_Arguments:_";
    printer "";
    printer "|type|name|description|";
    printer "|:---|:---|:---|";

    let get_param_row p = sprintf "|`%s`|%s|%s|" (of_ty_verbatim p.param_type) (escape p.param_name) (escape p.param_doc) in
    List.iter (fun p -> printer (get_param_row p)) x.msg_params;
    printer "";

    printer (sprintf "_Return Type:_ `%s`" return_type);
    printer "";
    let descr= desc_of_ty_opt x.msg_result in
    if descr <> ""  then
      (printer (escape descr);
      printer "")
  end;

  if x.msg_errors <> [] then begin
    let error_codes = List.map (fun err -> sprintf "`%s`" err.err_name) x.msg_errors in
    printer (sprintf "_Possible Error Codes:_ %s"
                   (String.concat ", " error_codes));
    printer "";
  end

let print_field_table_of_obj printer x =
  printer (sprintf "### Fields for class: "^(escape x.name));
  printer "";
  if x.contents=[] then
    printer ("Class "^(escape x.name)^" has no fields.")
  else begin
    printer "|Field|Type|Qualifier|Description|";
    printer "|:---|:---|:---|:---|";

    let print_field_content printer ({release; qualifier; ty; field_description=description} as y) =
      let wired_name = Datamodel_utils.wire_name_of_field y in
        printer (sprintf "|%s|`%s`|%s|%s%s|"
          (escape wired_name) (of_ty_verbatim ty) (string_of_qualifier qualifier)
          (string_of_open_product release) (escape description))
    in

    x |> Datamodel_utils.fields_of_obj |> List.iter (print_field_content printer)
  end

let of_obj printer x =
  printer (sprintf "## Class: %s" (escape x.name));
  printer "";
  printer (escape x.description);
  printer "";
  print_field_table_of_obj printer x;
  printer "";
  printer (sprintf "### RPCs associated with class: "^(escape x.name));
  printer "";
  if x.messages=[] then
  begin
    printer (sprintf "Class %s has no additional RPCs associated with it." (escape x.name));
    printer ""
  end
  else
    List.iter (markdown_section_of_message printer) x.messages

let print_enum printer = function
  | Enum (name, options) ->
    printer (sprintf "|`enum %s`||" name);
    printer "|:---|:---|";
    let print_option (opt, description) = printer (sprintf "|`%s`|%s|" opt (escape description)) in
    List.iter print_option options;
    printer "";
  | _ -> ()

let error_doc printer { err_name=name; err_params=params; err_doc=doc } =
  printer (sprintf "#### %s" (escape name));
  printer "";
  printer (escape doc);
  printer "";
  if params = [] then
    printer "No parameters."
  else begin
    printer "_Signature:_";
    printer "```";
    printer (sprintf "%s(%s)" name (String.concat ", " params));
    printer "```"
  end;
  printer ""

let print_all printer api =
  (* Remove private messages that are only used internally (e.g. get_record_internal) *)
  let api = Dm_api.filter (fun _ -> true) (fun _ -> true)
      (fun msg -> match msg.msg_tag with (FromObject (Private _)) -> false | _ -> true) api in
  let system = objects_of_api api and relations = relations_of_api api in

  printer "
# API Reference

## Classes

The following classes are defined:

|Name|Description|
|:---|:---|";

  let first_sentence s = List.hd (String.split '.' s) in
  List.iter (fun obj -> printer (sprintf "|`%s`|%s|" obj.name (escape (first_sentence obj.description)))) system;

  printer "
## Relationships Between Classes

Fields that are bound together are shown in the following table:

|_object.field_|_object.field_|_relationship_|
|:---|:---|:---|";
  List.iter (function (((a, a_field), (b, b_field)) as rel) ->
      let c = Relations.classify api rel in
      printer (sprintf "|`%s.%s`|`%s.%s`|%s|"
        a a_field b b_field (Relations.string_of_classification c))
    ) relations;

  printer "
The following diagram represents bound fields (as specified above) diagramatically, using crow's foot notation to specify one-to-one, one-to-many or many-to-many relationships:

![Class relationships](img_xenapi_datamodel_graph 'Class relationships')

## Types

### Primitives

The following primitive types are used to specify methods and fields in the API Reference:

|Type|Description|
|:---|:---|
|string|text strings|
|int   |64-bit integers|
|float|IEEE double-precision floating-point numbers|
|bool|boolean|
|datetime|date and timestamp|

### Higher-order types

The following type constructors are used:

|Type|Description|
|:---|:---|
|_c_ ref|reference to an object of class _c_|
|_t_ set|a set of elements of type _t_|
|(_a &rarr; b_) map|a table mapping values of type _a_ to values of type _b_|

### Enumeration types

The following enumeration types are used:
";

  List.iter (print_enum printer) (Types.of_objects system);
  List.iter (fun x -> of_obj printer x) system;

    printer "
## Error Handling

When a low-level transport error occurs, or a request is malformed at the HTTP
or XML-RPC level, the server may send an XML-RPC Fault response, or the client
may simulate the same.  The client must be prepared to handle these errors,
though they may be treated as fatal.  On the wire, these are transmitted in a
form similar to this:

```xml
    <methodResponse>
      <fault>
        <value>
          <struct>
            <member>
                <name>faultCode</name>
                <value><int>-1</int></value>
              </member>
              <member>
                <name>faultString</name>
                <value><string>Malformed request</string></value>
            </member>
          </struct>
        </value>
      </fault>
    </methodResponse>
```

All other failures are reported with a more structured error response, to
allow better automatic response to failures, proper internationalisation of
any error message, and easier debugging.  On the wire, these are transmitted
like this:

```xml
    <struct>
      <member>
        <name>Status</name>
        <value>Failure</value>
      </member>
      <member>
        <name>ErrorDescription</name>
        <value>
          <array>
            <data>
              <value>MAP_DUPLICATE_KEY</value>
              <value>Customer</value>
              <value>eSpeil Inc.</value>
              <value>eSpeil Incorporated</value>
            </data>
          </array>
        </value>
      </member>
    </struct>
```

Note that `ErrorDescription` value is an array of string values. The
first element of the array is an error code; the remainder of the array are
strings representing error parameters relating to that code.  In this case,
the client has attempted to add the mapping _Customer &rarr;
eSpiel Incorporated_ to a Map, but it already contains the mapping
_Customer &rarr; eSpiel Inc._, and so the request has failed.

Each possible error code is documented in the following section.

### Error Codes
";

  (* Sort the errors alphabetically, then generate one section per code. *)
  let errs =
    Hashtbl.fold (fun name err acc -> (name, err) :: acc)
      Datamodel.errors []
  in
  List.iter (error_doc printer)
    (snd (List.split
            (List.sort (fun (n1, _) (n2, _)-> compare n1 n2) errs)))

let all api destdir =
  Stdext.Unixext.mkdir_rec destdir 0o755;
  let out_chan = open_out (Filename.concat destdir "api_ref_autogen.md") in
  let printer text =
    fprintf out_chan "%s" text;
    fprintf out_chan "\n"
  in
  finally (fun () -> print_all printer api)
          (fun () -> close_out out_chan)
