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
open Str
open Datamodel_types
open Dm_api
open CommonFunctions
module DT = Datamodel_types
module DU = Datamodel_utils

(*Filter out all the bits of the data model we don't want to put in the api.
  For instance we don't want the things which are marked internal only, or the
  ones marked hide_from_docs*)
let api =
  Datamodel_utils.named_self := true ;

  let obj_filter _ = true in
  let field_filter field =
    (not field.internal_only) && List.mem "closed" field.release.internal
  in
  let message_filter msg =
    Datamodel_utils.on_client_side msg
    && (not msg.msg_hide_from_docs)
    && List.mem "closed" msg.msg_release.internal
  in
  filter obj_filter field_filter message_filter
    (Datamodel_utils.add_implicit_messages ~document_order:false
       (filter obj_filter field_filter message_filter Datamodel.all_api)
    )

(*Here we extract a list of objs (look in datamodel_types.ml for the structure definitions)*)
let classes = objects_of_api api

let print_license file =
  output_string file Licence.bsd_two_clause ;
  output_string file "\n\n"

(*How shall we translate datamodel identifiers into Java, with its conventions about case, and reserved words?*)

let reserved_words = function
  | "class" ->
      "clazz"
  | "clone" ->
      "createClone"
  | "param-name" ->
      "param_name"
  | "interface" ->
      "iface"
  | "import" ->
      "import_"
  | s ->
      s

(* Given a XenAPI on-the-wire representation of an enum value, return the Java enum *)
let enum_of_wire x =
  global_replace (regexp_string "-") "_" (String.uppercase_ascii x)

let second_character_is_uppercase s =
  if String.length s < 2 then
    false
  else
    let second_char = String.sub s 1 1 in
    second_char = String.uppercase_ascii second_char

let transform s =
  if second_character_is_uppercase s then
    s
  else
    String.capitalize_ascii (reserved_words (String.uncapitalize_ascii s))

let class_case x =
  String.concat "" (List.map transform (Astring.String.cuts ~sep:"_" x))

let keywords = [("public", "_public")]

let keyword_map s =
  if List.mem_assoc s keywords then List.assoc s keywords else s

let camel_case s =
  let ss = Astring.String.cuts ~sep:"_" s |> List.map transform in
  let result =
    match ss with
    | [] ->
        ""
    | h :: tl ->
        let h' =
          if String.length h > 1 then
            let sndchar = String.sub h 1 1 in
            if sndchar = String.uppercase_ascii sndchar then
              h
            else
              String.uncapitalize_ascii h
          else
            String.uncapitalize_ascii h
        in
        h' ^ String.concat "" tl
  in
  keyword_map result

let exception_class_case x =
  String.concat ""
    (List.map
       (fun s -> String.capitalize_ascii (String.lowercase_ascii s))
       (Astring.String.cuts ~sep:"_" x)
    )

(*As we process the datamodel, we collect information about enumerations, types*)
(* and records, which we use to create Types.java later *)

let enums = Hashtbl.create 10

let records = Hashtbl.create 10

(*We want an empty mutable set to keep the types in.*)
module Ty = struct
  type t = DT.ty

  let compare = compare
end

module TypeSet = Set.Make (Ty)

let types = ref TypeSet.empty

(* Helper functions for types *)
let rec get_java_type ty =
  types := TypeSet.add ty !types ;
  match ty with
  | SecretString | String ->
      "String"
  | Int ->
      "Long"
  | Float ->
      "Double"
  | Bool ->
      "Boolean"
  | DateTime ->
      "Date"
  | Enum (name, ls) ->
      Hashtbl.replace enums name ls ;
      sprintf "Types.%s" (class_case name)
  | Set t1 ->
      sprintf "Set<%s>" (get_java_type t1)
  | Map (t1, t2) ->
      sprintf "Map<%s, %s>" (get_java_type t1) (get_java_type t2)
  | Ref x ->
      class_case x (* We want to hide all refs *)
  | Record x ->
      sprintf "%s.Record" (class_case x)
  | Option x ->
      get_java_type x

(*We'd like the list of XenAPI objects to appear as an enumeration so we can*)
(* switch on them, so add it using this mechanism*)
let switch_enum =
  Enum ("XenAPIObjects", List.map (fun x -> (x.name, x.description)) classes)

let _ = get_java_type switch_enum

(*Helper function for get_marshall_function*)
let rec get_marshall_function_rec = function
  | SecretString | String ->
      "String"
  | Int ->
      "Long"
  | Float ->
      "Double"
  | Bool ->
      "Boolean"
  | DateTime ->
      "Date"
  | Enum (name, _) ->
      class_case name
  | Set t1 ->
      sprintf "SetOf%s" (get_marshall_function_rec t1)
  | Map (t1, t2) ->
      sprintf "MapOf%s%s"
        (get_marshall_function_rec t1)
        (get_marshall_function_rec t2)
  | Ref ty ->
      class_case ty (* We want to hide all refs *)
  | Record ty ->
      sprintf "%sRecord" (class_case ty)
  | Option ty ->
      get_marshall_function_rec ty

(*get_marshall_function (Set(Map(Float,Bool)));; -> "toSetOfMapOfDoubleBoolean"*)
let get_marshall_function ty = "to" ^ get_marshall_function_rec ty

(* Generate the methods *)

let get_java_type_or_void = function
  | None ->
      "void"
  | Some (ty, _) ->
      get_java_type ty

(* Here are a lot of functions which ask questions of the messages associated with*)
(* objects, the answers to which are helpful when generating the corresponding java*)
(* functions. For instance is_method_static takes an object's message, and*)
(* determines whether it should be static or not in java, by looking at whether*)
(* it has a self parameter or not.*)

(*Similar functions for deprecation of methods*)
let get_method_deprecated message =
  message.msg_release.internal_deprecated_since <> None

let get_method_deprecated_string message =
  if get_method_deprecated message then
    "@Deprecated"
  else
    ""

let get_method_param {param_type= ty; param_name= name; _} =
  let ty = get_java_type ty in
  let name = camel_case name in
  sprintf "%s %s" ty name

let get_method_params_for_signature params =
  String.concat ", " ("Connection c" :: List.map get_method_param params)

let get_method_params_for_xml message params =
  let f = function
    | {param_type= Record _; param_name= name; _} ->
        camel_case name ^ "_map"
    | {param_name= name; _} ->
        camel_case name
  in
  match params with
  | [] ->
      if is_method_static message then
        []
      else
        ["this.ref"]
  | _ ->
      if is_method_static message then
        List.map f params
      else
        "this.ref" :: List.map f params

let gen_method_return_cast message =
  match message.msg_result with
  | None ->
      sprintf ""
  | Some (ty, _) ->
      sprintf " Types.%s(result)" (get_marshall_function ty)

let gen_method_return file cls message =
  if
    String.lowercase_ascii cls.name = "event"
    && String.lowercase_ascii message.msg_name = "from"
  then
    fprintf file "        return Types.toEventBatch(result);\n"
  else
    fprintf file "        return%s;\n" (gen_method_return_cast message)

let rec range = function 0 -> [] | i -> range (i - 1) @ [i]

(* Here is the main method generating function.*)
let gen_method file cls message params async_version =
  let deprecated_string = get_method_deprecated_string message in
  let return_type =
    if
      String.lowercase_ascii cls.name = "event"
      && String.lowercase_ascii message.msg_name = "from"
    then
      "EventBatch"
    else
      get_java_type_or_void message.msg_result
  in
  let method_static = if is_method_static message then "static " else "" in
  let method_name = camel_case message.msg_name in
  let paramString = get_method_params_for_signature params in
  let default_errors =
    [
      ( "BadServerResponse"
      , "Thrown if the response from the server contains an invalid status."
      )
    ; ("XenAPIException", "Thrown if the call failed.")
    ; ( "XmlRpcException"
      , "Thrown if the result of an asynchronous call could not be parsed."
      )
    ]
  in
  let publishInfo = get_published_info_message message cls in

  fprintf file "    /**\n" ;
  fprintf file "     * %s\n" (escape_xml message.msg_doc) ;
  if not (publishInfo = "") then fprintf file "     * %s\n" publishInfo ;
  if get_method_deprecated message then fprintf file "     * @deprecated\n" ;
  fprintf file "     *\n" ;
  fprintf file "     * @param c The connection the call is made on\n" ;

  List.iter
    (fun x ->
      let paramPublishInfo = get_published_info_param message x in
      fprintf file "     * @param %s %s%s\n" (camel_case x.param_name)
        (if x.param_doc = "" then "No description" else escape_xml x.param_doc)
        (if paramPublishInfo = "" then "" else " " ^ paramPublishInfo)
      )
    params ;

  ( if async_version then
      fprintf file "     * @return Task\n"
  else
    match message.msg_result with
    | None ->
        ()
    | Some (_, "") ->
        fprintf file "     * @return %s\n"
          (get_java_type_or_void message.msg_result)
    | Some (_, desc) ->
        fprintf file "     * @return %s\n" desc
  ) ;

  List.iter
    (fun x -> fprintf file "     * @throws %s %s\n" (fst x) (snd x))
    default_errors ;
  List.iter
    (fun x ->
      fprintf file "     * @throws Types.%s %s\n"
        (exception_class_case x.err_name)
        x.err_doc
      )
    message.msg_errors ;

  fprintf file "     */\n" ;

  if async_version then
    fprintf file "   %s public %sTask %sAsync(%s) throws\n" deprecated_string
      method_static method_name paramString
  else
    fprintf file "   %s public %s%s %s(%s) throws\n" deprecated_string
      method_static return_type method_name paramString ;

  let all_errors =
    List.map fst default_errors
    @ List.map
        (fun x -> "Types." ^ exception_class_case x.err_name)
        message.msg_errors
  in
  fprintf file "       %s {\n" (String.concat ",\n       " all_errors) ;

  if async_version then
    fprintf file "        String method_call = \"Async.%s.%s\";\n"
      message.msg_obj_name message.msg_name
  else
    fprintf file "        String method_call = \"%s.%s\";\n"
      message.msg_obj_name message.msg_name ;

  if message.msg_session then
    fprintf file "        String session = c.getSessionReference();\n"
  else
    () ;

  let record_params =
    List.filter
      (function {param_type= Record _; _} -> true | _ -> false)
      message.msg_params
  in

  List.iter
    (fun {param_name= s; _} ->
      let name = camel_case s in
      fprintf file "        Map<String, Object> %s_map = %s.toMap();\n" name
        name
      )
    record_params ;

  fprintf file "        Object[] method_params = {" ;

  let methodParamsList =
    if message.msg_session then
      "session" :: get_method_params_for_xml message params
    else
      get_method_params_for_xml message params
  in

  output_string file
    (String.concat ", "
       (List.map
          (fun s -> sprintf "Marshalling.toXMLRPC(%s)" s)
          methodParamsList
       )
    ) ;

  fprintf file "};\n" ;
  fprintf file
    "        Map response = c.dispatch(method_call, method_params);\n" ;

  ( if async_version then (
      fprintf file "        Object result = response.get(\"Value\");\n" ;
      fprintf file "        return Types.toTask(result);\n"
    ) else
      match message.msg_result with
      | None ->
          fprintf file ""
      | Some _ ->
          fprintf file "        Object result = response.get(\"Value\");\n" ;
          gen_method_return file cls message
  ) ;

  fprintf file "    }\n\n"

(*Some methods have an almost identical asynchronous counterpart, which returns*)
(* a Task reference rather than its usual return value*)
let gen_method_and_asynchronous_counterpart file cls message =
  let methodParams = get_method_params_list message in
  let generator x =
    if message.msg_async then gen_method file cls message x true ;
    gen_method file cls message x false
  in
  match methodParams with
  | [] ->
      generator []
  | _ ->
      let paramGroups = gen_param_groups message methodParams in
      List.iter generator paramGroups

(* Generate the record *)

(* The fields of an object are stored in trees in the datamodel, which means that*)
(* the next three functions, which are conceptually for generating the fields*)
(* of each class, and for the corresponding entries in the toString and toMap*)
(* functions are in fact implemented as three sets of three mutual recursions,*)
(* which take the trees apart. *)

let gen_record_field file prefix field cls =
  let ty = get_java_type field.ty in
  let name =
    camel_case (String.concat "_" (List.rev (field.field_name :: prefix)))
  in
  let publishInfo = get_published_info_field field cls in
  fprintf file "        /**\n" ;
  fprintf file "         * %s\n" (escape_xml field.field_description) ;
  if not (publishInfo = "") then fprintf file "         * %s\n" publishInfo ;
  fprintf file "         */\n" ;
  fprintf file "        public %s %s;\n" ty name

let rec gen_record_namespace file prefix (name, contents) cls =
  List.iter (gen_record_contents file (name :: prefix) cls) contents

and gen_record_contents file prefix cls = function
  | Field f ->
      gen_record_field file prefix f cls
  | Namespace (n, cs) ->
      gen_record_namespace file prefix (n, cs) cls

(***)

let gen_record_tostring_field file prefix field =
  let name = String.concat "_" (List.rev (field.field_name :: prefix)) in
  let name = camel_case name in
  fprintf file
    "            print.printf(\"%%1$20s: %%2$s\\n\", \"%s\", this.%s);\n" name
    name

let rec gen_record_tostring_namespace file prefix (name, contents) =
  List.iter (gen_record_tostring_contents file (name :: prefix)) contents

and gen_record_tostring_contents file prefix = function
  | Field f ->
      gen_record_tostring_field file prefix f
  | Namespace (n, cs) ->
      gen_record_tostring_namespace file prefix (n, cs)

(***)

let field_default = function
  | SecretString | String ->
      "\"\""
  | Int ->
      "0"
  | Float ->
      "0.0"
  | Bool ->
      "false"
  | DateTime ->
      "new Date(0)"
  | Enum ("vif_locking_mode", _) ->
      "Types.VifLockingMode.NETWORK_DEFAULT" (* XOP-372 *)
  | Enum (name, _) ->
      sprintf "Types.%s.UNRECOGNIZED" (class_case name)
  | Set t1 ->
      sprintf "new LinkedHashSet<%s>()" (get_java_type t1)
  | Map (t1, t2) ->
      sprintf "new HashMap<%s, %s>()" (get_java_type t1) (get_java_type t2)
  | Ref ty ->
      sprintf "new %s(\"OpaqueRef:NULL\")" (class_case ty)
  | Record _ ->
      assert false
  | Option _ ->
      "null"

let gen_record_tomap_field file prefix field =
  let name = String.concat "_" (List.rev (field.field_name :: prefix)) in
  let name' = camel_case name in
  let default = field_default field.ty in
  fprintf file "            map.put(\"%s\", this.%s == null ? %s : this.%s);\n"
    name name' default name'

let rec gen_record_tomap_contents file prefix = function
  | Field f ->
      gen_record_tomap_field file prefix f
  | Namespace (n, cs) ->
      List.iter (gen_record_tomap_contents file (n :: prefix)) cs

(*Generate the Record subclass for the given class, with its toString and toMap*)
(* methods. We're also modifying the records hash table as a side effect*)

let gen_record file cls =
  let class_name = class_case cls.name in
  let _ = Hashtbl.replace records cls.name cls.contents in
  let contents = cls.contents in
  fprintf file "    /**\n" ;
  fprintf file "     * Represents all the fields in a %s\n" class_name ;
  fprintf file "     */\n" ;
  fprintf file "    public static class Record implements Types.Record {\n" ;
  fprintf file "        public String toString() {\n" ;
  fprintf file "            StringWriter writer = new StringWriter();\n" ;
  fprintf file "            PrintWriter print = new PrintWriter(writer);\n" ;

  List.iter (gen_record_tostring_contents file []) contents ;
  (*for the Event.Record, we have to add in the snapshot field by hand, because it's not in the data model!*)
  if cls.name = "event" then
    fprintf file
      "            print.printf(\"%%1$20s: %%2$s\\n\", \"snapshot\", \
       this.snapshot);\n" ;

  fprintf file "            return writer.toString();\n" ;
  fprintf file "        }\n\n" ;
  fprintf file "        /**\n" ;
  fprintf file "         * Convert a %s.Record to a Map\n" cls.name ;
  fprintf file "         */\n" ;
  fprintf file "        public Map<String,Object> toMap() {\n" ;
  fprintf file
    "            Map<String,Object> map = new HashMap<String,Object>();\n" ;

  List.iter (gen_record_tomap_contents file []) contents ;
  if cls.name = "event" then
    fprintf file "            map.put(\"snapshot\", this.snapshot);\n" ;

  fprintf file "            return map;\n" ;
  fprintf file "        }\n\n" ;

  List.iter (gen_record_contents file [] cls) contents ;
  if cls.name = "event" then (
    fprintf file "        /**\n" ;
    fprintf file
      "         * The record of the database object that was added, changed or \
       deleted\n" ;
    fprintf file
      "         * (the actual type will be VM.Record, VBD.Record or similar)\n" ;
    fprintf file "         */\n" ;
    fprintf file "        public Object snapshot;\n"
  ) ;

  fprintf file "    }\n\n"

(* Generate the class *)

let class_is_empty cls = cls.contents = []

let gen_class cls folder =
  let class_name = class_case cls.name in
  let methods = cls.messages in
  let file = open_out (Filename.concat folder class_name ^ ".java") in
  let publishInfo = get_published_info_class cls in
  print_license file ;
  fprintf file
    "package com.xensource.xenapi;\n\n\
     import com.xensource.xenapi.Types.BadServerResponse;\n\
     import com.xensource.xenapi.Types.VersionException;\n\
     import com.xensource.xenapi.Types.XenAPIException;\n\n\
     import java.io.PrintWriter;\n\
     import java.io.StringWriter;\n\
     import java.util.Date;\n\
     import java.util.HashMap;\n\
     import java.util.LinkedHashSet;\n\
     import java.util.Map;\n\
     import java.util.Set;\n\n\
     import org.apache.xmlrpc.XmlRpcException;\n\n" ;
  fprintf file "/**\n" ;
  fprintf file " * %s\n" cls.description ;
  if not (publishInfo = "") then fprintf file " * %s\n" publishInfo ;
  fprintf file " *\n" ;
  fprintf file " * @author Citrix Systems, Inc.\n" ;
  fprintf file " */\n" ;
  fprintf file "public class %s extends XenAPIObject {\n\n" class_name ;

  if class_is_empty cls then
    fprintf file
      "\n    public String toWireString() {\n        return null;\n    }\n\n"
  else (
    fprintf file "    /**\n" ;
    fprintf file "     * The XenAPI reference (OpaqueRef) to this object.\n" ;
    fprintf file "     */\n" ;
    fprintf file "    protected final String ref;\n\n" ;
    fprintf file "    /**\n" ;
    fprintf file "     * For internal use only.\n" ;
    fprintf file "     */\n" ;
    fprintf file "    %s(String ref) {\n" class_name ;
    fprintf file "       this.ref = ref;\n" ;
    fprintf file "    }\n\n" ;
    fprintf file "    /**\n" ;
    fprintf file
      "     * @return The XenAPI reference (OpaqueRef) to this object.\n" ;
    fprintf file "     */\n" ;
    fprintf file "    public String toWireString() {\n" ;
    fprintf file "       return this.ref;\n" ;
    fprintf file "    }\n\n"
  ) ;

  if not (class_is_empty cls) then (
    fprintf file "    /**\n" ;
    fprintf file
      "     * If obj is a %s, compares XenAPI references for equality.\n"
      class_name ;
    fprintf file "     */\n" ;
    fprintf file "    @Override\n" ;
    fprintf file "    public boolean equals(Object obj)\n" ;
    fprintf file "    {\n" ;
    fprintf file "        if (obj != null && obj instanceof %s)\n" class_name ;
    fprintf file "        {\n" ;
    fprintf file "            %s other = (%s) obj;\n" class_name class_name ;
    fprintf file "            return other.ref.equals(this.ref);\n" ;
    fprintf file "        } else\n" ;
    fprintf file "        {\n" ;
    fprintf file "            return false;\n" ;
    fprintf file "        }\n" ;
    fprintf file "    }\n\n" ;

    (*hashcode*)
    fprintf file "    @Override\n" ;
    fprintf file "    public int hashCode()\n" ;
    fprintf file "    {\n" ;
    fprintf file "        return ref.hashCode();\n" ;
    fprintf file "    }\n\n" ;
    flush file ;
    gen_record file cls ;
    flush file
  ) ;

  List.iter (gen_method_and_asynchronous_counterpart file cls) methods ;

  flush file ;
  fprintf file "}" ;
  close_out file

(* Generate Marshalling Class *)

(*This generates the special case code for marshalling the snapshot field in an Event.Record*)
let generate_snapshot_hack file =
  fprintf file "\n" ;
  fprintf file "\n" ;
  fprintf file "        Object a,b;\n" ;
  fprintf file "        a=map.get(\"snapshot\");\n" ;
  fprintf file "        switch(%s(record.clazz))\n"
    (get_marshall_function switch_enum) ;
  fprintf file "        {\n" ;
  List.iter
    (fun x ->
      fprintf file "                case %17s: b = %25s(a); break;\n"
        (String.uppercase_ascii x)
        (get_marshall_function (Record x))
      )
    (List.map
       (fun x -> x.name)
       (List.filter (fun x -> not (class_is_empty x)) classes)
    ) ;
  fprintf file
    "                default: throw new RuntimeException(\"Internal error in \
     auto-generated code whilst unmarshalling event snapshot\");\n" ;
  fprintf file "        }\n" ;
  fprintf file "        record.snapshot = b;\n"

let gen_marshall_record_field file prefix field =
  let ty = get_marshall_function field.ty in
  let name = String.concat "_" (List.rev (field.field_name :: prefix)) in
  let name' = camel_case name in
  fprintf file "            record.%s = %s(map.get(\"%s\"));\n" name' ty name

let rec gen_marshall_record_namespace file prefix (name, contents) =
  List.iter (gen_marshall_record_contents file (name :: prefix)) contents

and gen_marshall_record_contents file prefix = function
  | Field f ->
      gen_marshall_record_field file prefix f
  | Namespace (n, cs) ->
      gen_marshall_record_namespace file prefix (n, cs) ;
      ()

(*Every type which may be returned by a function may also be the result of the*)
(* corresponding asynchronous task. We therefore need to generate corresponding*)
(* marshalling functions which can take the raw xml of the tasks result field*)
(* and turn it into the corresponding type. Luckily, the only things returned by*)
(* asynchronous tasks are object references and strings, so rather than implementing*)
(* the general recursive structure we'll just make one for each of the classes*)
(* that's been registered as a marshall-needing type*)

let generate_reference_task_result_func file clstr =
  fprintf file
    "    public static %s to%s(Task task, Connection connection) throws \
     XenAPIException, BadServerResponse, XmlRpcException, BadAsyncResult{\n"
    clstr clstr ;
  fprintf file
    "        return Types.to%s(parseResult(task.getResult(connection)));\n"
    clstr ;
  fprintf file "    }\n" ;
  fprintf file "\n"

let gen_task_result_func file = function
  | Ref ty ->
      generate_reference_task_result_func file (class_case ty)
  | _ ->
      ()

(*don't generate for complicated types. They're not needed.*)

let rec gen_marshall_body file = function
  | SecretString | String ->
      fprintf file "        return (String) object;\n"
  | Int ->
      fprintf file "        return Long.valueOf((String) object);\n"
  | Float ->
      fprintf file "        return (Double) object;\n"
  | Bool ->
      fprintf file "        return (Boolean) object;\n"
  | DateTime ->
      fprintf file
        "        try {\n\
        \            return (Date) object;\n\
        \        } catch (ClassCastException e){\n\
        \            //Occasionally the date comes back as an ocaml float \
         rather than\n\
        \            //in the xmlrpc format! Catch this and convert.\n\
        \            return (new Date((long) (1000*Double.parseDouble((String) \
         object))));\n\
        \        }\n"
  | Ref ty ->
      fprintf file "        return new %s((String) object);\n" (class_case ty)
  | Enum (name, _) ->
      fprintf file "        try {\n" ;
      fprintf file
        "            return %s.valueOf(((String) \
         object).toUpperCase().replace('-','_'));\n"
        (class_case name) ;
      fprintf file "        } catch (IllegalArgumentException ex) {\n" ;
      fprintf file "            return %s.UNRECOGNIZED;\n" (class_case name) ;
      fprintf file "        }\n"
  | Set ty ->
      let ty_name = get_java_type ty in
      let marshall_fn = get_marshall_function ty in
      fprintf file "        Object[] items = (Object[]) object;\n" ;
      fprintf file "        Set<%s> result = new LinkedHashSet<%s>();\n" ty_name
        ty_name ;
      fprintf file "        for(Object item: items) {\n" ;
      fprintf file "            %s typed = %s(item);\n" ty_name marshall_fn ;
      fprintf file "            result.add(typed);\n" ;
      fprintf file "        }\n" ;
      fprintf file "        return result;\n"
  | Map (ty, ty') ->
      let ty_name = get_java_type ty in
      let ty_name' = get_java_type ty' in
      let marshall_fn = get_marshall_function ty in
      let marshall_fn' = get_marshall_function ty' in
      fprintf file "        Map map = (Map) object;\n" ;
      fprintf file "        Map<%s,%s> result = new HashMap<%s,%s>();\n" ty_name
        ty_name' ty_name ty_name' ;
      fprintf file "        Set<Map.Entry> entries = map.entrySet();\n" ;
      fprintf file "        for(Map.Entry entry: entries) {\n" ;
      fprintf file "            %s key = %s(entry.getKey());\n" ty_name
        marshall_fn ;
      fprintf file "            %s value = %s(entry.getValue());\n" ty_name'
        marshall_fn' ;
      fprintf file "            result.put(key, value);\n" ;
      fprintf file "        }\n" ;
      fprintf file "        return result;\n"
  | Record ty ->
      let contents = Hashtbl.find records ty in
      let cls_name = class_case ty in
      fprintf file
        "        Map<String,Object> map = (Map<String,Object>) object;\n" ;
      fprintf file "        %s.Record record = new %s.Record();\n" cls_name
        cls_name ;
      List.iter (gen_marshall_record_contents file []) contents ;
      (*Event.Record needs a special case to handle snapshots*)
      if ty = "event" then generate_snapshot_hack file ;
      fprintf file "        return record;\n"
  | Option ty ->
      gen_marshall_body file ty

let rec gen_marshall_func file ty =
  match ty with
  | Option x ->
      if TypeSet.mem x !types then
        ()
      else
        gen_marshall_func file ty
  | _ ->
      let type_string = get_java_type ty in
      let fn_name = get_marshall_function ty in
      fprintf file "    public static %s %s(Object object) {\n" type_string
        fn_name ;
      fprintf file "        if (object == null) {\n" ;
      fprintf file "            return null;\n" ;
      fprintf file "        }\n" ;
      gen_marshall_body file ty ;
      fprintf file "    }\n\n"

let gen_enum file name ls =
  let name = class_case name in
  let ls =
    ("UNRECOGNIZED", "The value does not belong to this enumeration") :: ls
  in
  fprintf file "    public enum %s {\n" name ;
  let to_member_declaration (name, description) =
    let escaped_description =
      global_replace (regexp_string "*/") "* /" description
    in
    let final_description =
      global_replace (regexp_string "\n") "\n         * " escaped_description
    in
    "        /**\n"
    ^ "         * "
    ^ final_description
    ^ "\n"
    ^ "         */\n"
    ^ "        "
    ^ enum_of_wire name
  in
  fprintf file "%s" (String.concat ",\n" (List.map to_member_declaration ls)) ;
  fprintf file ";\n" ;
  fprintf file "        public String toString() {\n" ;
  List.iter
    (fun (enum, _) ->
      fprintf file "            if (this == %s) return \"%s\";\n"
        (enum_of_wire enum) enum
      )
    ls ;
  fprintf file "        /* This can never be reached */\n" ;
  fprintf file "        return \"illegal enum\";\n" ;
  fprintf file "        }\n" ;
  fprintf file "\n    }\n\n"

let gen_enums file = Hashtbl.iter (gen_enum file) enums

let gen_error_field_name field =
  camel_case (String.concat "_" (Astring.String.cuts ~sep:" " field))

let gen_error_field_names fields = List.map gen_error_field_name fields

let gen_error_fields file field =
  fprintf file "        public final String %s;\n" field

let gen_error file name params =
  let name = exception_class_case name in
  let fields = gen_error_field_names params.err_params in
  let constructor_params =
    String.concat ", " (List.map (fun field -> "String " ^ field) fields)
  in

  fprintf file "    /**\n" ;
  fprintf file "     * %s\n" (escape_xml params.err_doc) ;
  fprintf file "     */\n" ;
  fprintf file "    public static class %s extends XenAPIException {\n" name ;

  List.iter (gen_error_fields file) fields ;

  fprintf file "\n        /**\n" ;
  fprintf file "         * Create a new %s\n" name ;
  fprintf file "         */\n" ;
  fprintf file "        public %s(%s) {\n" name constructor_params ;
  fprintf file "            super(\"%s\");\n" (escape_xml params.err_doc) ;

  List.iter (fun s -> fprintf file "            this.%s = %s;\n" s s) fields ;

  fprintf file "        }\n\n" ;
  fprintf file "    }\n\n"

let gen_method_error_throw file name error =
  let class_name = exception_class_case name in
  let paramsStr =
    String.concat ", "
      (List.map
         (fun i -> sprintf "p%i" i)
         (range (List.length error.err_params))
      )
  in

  fprintf file "            if (ErrorDescription[0].equals(\"%s\"))\n" name ;
  fprintf file "            {\n" ;

  (* Prepare the parameters to the Exception constructor *)
  List.iter
    (fun i ->
      fprintf file
        "                String p%i = ErrorDescription.length > %i ? \
         ErrorDescription[%i] : \"\";\n"
        i i i
      )
    (range (List.length error.err_params)) ;

  fprintf file "                throw new Types.%s(%s);\n" class_name paramsStr ;
  fprintf file "            }\n"

let gen_types_class folder =
  let class_name = "Types" in
  let file = open_out (Filename.concat folder class_name ^ ".java") in
  print_license file ;
  fprintf file
    "package com.xensource.xenapi;\n\n\
     import java.util.Date;\n\
     import java.util.Map;\n\
     import java.util.HashMap;\n\
     import java.util.Set;\n\
     import java.util.LinkedHashSet;\n\
     import java.io.IOException;\n\n\
     import java.util.regex.Pattern;\n\
     import java.util.regex.Matcher;\n\n\
     import org.apache.xmlrpc.XmlRpcException;\n\n\
     /**\n\
    \ * This class holds vital marshalling functions, enum types and exceptions.\n\
    \ *\n\
    \ * @author Citrix Systems, Inc.\n\
    \ */\n\
     public class Types\n\
     {\n\
    \    /**\n\
    \     * Interface for all Record classes\n\
    \     */\n\
    \    public static interface Record\n\
    \    {\n\
    \        /**\n\
    \         * Convert a Record to a Map\n\
    \         */\n\
    \        Map<String, Object> toMap();\n\
    \    }\n\n\
    \    /**\n\
    \     * Helper method.\n\
    \     */\n\
    \    private static String[] ObjectArrayToStringArray(Object[] objArray)\n\
    \    {\n\
    \        String[] result = new String[objArray.length];\n\
    \        for (int i = 0; i < objArray.length; i++)\n\
    \        {\n\
    \            result[i] = (String) objArray[i];\n\
    \        }\n\
    \        return result;\n\
    \    }\n\n\
    \    /**\n\
    \     * Base class for all XenAPI Exceptions\n\
    \     */\n\
    \    public static class XenAPIException extends IOException {\n\
    \        public final String shortDescription;\n\
    \        public final String[] errorDescription;\n\n\
    \        XenAPIException(String shortDescription)\n\
    \        {\n\
    \            this.shortDescription = shortDescription;\n\
    \            this.errorDescription = null;\n\
    \        }\n\n\
    \        XenAPIException(String[] errorDescription)\n\
    \        {\n\
    \            this.errorDescription = errorDescription;\n\n\
    \            if (errorDescription.length > 0)\n\
    \            {\n\
    \                shortDescription = errorDescription[0];\n\
    \            } else\n\
    \            {\n\
    \                shortDescription = \"\";\n\
    \            }\n\
    \        }\n\n\
    \        public String toString()\n\
    \        {\n\
    \            if (errorDescription == null)\n\
    \            {\n\
    \                return shortDescription;\n\
    \            } else if (errorDescription.length == 0)\n\
    \            {\n\
    \                return \"\";\n\
    \            }\n\
    \            StringBuilder sb = new StringBuilder();\n\
    \            for (int i = 0; i < errorDescription.length - 1; i++)\n\
    \            {\n\
    \                sb.append(errorDescription[i]);\n\
    \            }\n\
    \            sb.append(errorDescription[errorDescription.length - 1]);\n\n\
    \            return sb.toString();\n\
    \        }\n\
    \    }\n\
    \    /**\n\
    \     * Thrown if the response from the server contains an invalid status.\n\
    \     */\n\
    \    public static class BadServerResponse extends XenAPIException\n\
    \    {\n\
    \        public BadServerResponse(Map response)\n\
    \        {\n\
    \            super(ObjectArrayToStringArray((Object[]) \
     response.get(\"ErrorDescription\")));\n\
    \        }\n\
    \    }\n\n\
    \    public static class BadAsyncResult extends XenAPIException\n\
    \    {\n\
    \        public final String result;\n\n\
    \        public BadAsyncResult(String result)\n\
    \        {\n\
    \            super(result);\n\
    \            this.result = result;\n\
    \        }\n\
    \    }\n\n\
    \    /*\n\
    \     * A call has been made which should not be made against this version \
     of host.\n\
    \     * Probably the host is out of date and cannot handle this call, or is\n\
    \     * unable to comply with the details of the call.\n\
    \     */\n\
    \    public static class VersionException extends XenAPIException\n\
    \    {\n\
    \        public final String result;\n\n\
    \        public VersionException(String result)\n\
    \        {\n\
    \            super(result);\n\
    \            this.result = result;\n\
    \        }\n\
    \    }\n\n\
    \    private static String parseResult(String result) throws BadAsyncResult\n\
    \    {\n\
    \        Pattern pattern = Pattern.compile(\"<value>(.*)</value>\");\n\
    \        Matcher matcher = pattern.matcher(result);\n\
    \        matcher.find();\n\n\
    \        if (matcher.groupCount() != 1)\n\
    \        {\n\
    \            throw new Types.BadAsyncResult(\"Can't interpret: \" + result);\n\
    \        }\n\n\
    \        return matcher.group(1);\n\
    \    }\n\
    \  " ;

  fprintf file
    "    /**\n\
    \     * Checks the provided server response was successful. If the call \
     failed, throws a XenAPIException. If the server\n\
    \     * returned an invalid response, throws a BadServerResponse. \
     Otherwise, returns the server response as passed in.\n\
    \     */\n\
    \    static Map checkResponse(Map response) throws XenAPIException, \
     BadServerResponse\n\
    \    {\n\
    \        if (response.get(\"Status\").equals(\"Success\"))\n\
    \        {\n\
    \            return response;\n\
    \        }\n\n\
    \        if (response.get(\"Status\").equals(\"Failure\"))\n\
    \        {\n\
    \            String[] ErrorDescription = \
     ObjectArrayToStringArray((Object[]) response.get(\"ErrorDescription\"));\n\n" ;

  Hashtbl.iter (gen_method_error_throw file) Datamodel.errors ;

  fprintf file
    "\n\
    \            // An unknown error occurred\n\
    \            throw new Types.XenAPIException(ErrorDescription);\n\
    \        }\n\n\
    \        throw new BadServerResponse(response);\n\
    \    }\n\n" ;

  gen_enums file ;
  fprintf file "\n" ;
  Hashtbl.iter (gen_error file) Datamodel.errors ;
  fprintf file "\n" ;
  TypeSet.iter (gen_marshall_func file) !types ;
  fprintf file "\n" ;
  TypeSet.iter (gen_task_result_func file) !types ;
  fprintf file
    "\n\
    \        public static EventBatch toEventBatch(Object object) {\n\
    \        if (object == null) {\n\
    \            return null;\n\
    \        }\n\n\
    \        Map map = (Map) object;\n\
    \        EventBatch batch = new EventBatch();\n\
    \        batch.token = toString(map.get(\"token\"));\n\
    \        batch.validRefCounts = map.get(\"valid_ref_counts\");\n\
    \        batch.events = toSetOfEventRecord(map.get(\"events\"));\n\
    \        return batch;\n\
    \    }" ;
  fprintf file "}\n"

(* Now run it *)

let populate_releases templdir class_dir =
  render_file
    ("APIVersion.mustache", "APIVersion.java")
    json_releases templdir class_dir

let gen_get_all_records_test classes templdir sample_dir =
  let class_records =
    classes
    |> List.filter (fun {obj_lifecycle; _} ->
           not (List.exists (fun (x, _, _) -> x = Removed) obj_lifecycle)
       )
    |> List.filter (fun {messages; _} ->
           List.exists (fun x -> x.msg_name = "get_all_records") messages
       )
    |> List.map (fun {name; _} -> class_case name)
    |> List.sort String.compare
  in
  let json =
    `O
      [
        ( "api_class_records"
        , `A
            (List.map
               (fun x -> `O [("api_class_record", `String x)])
               class_records
            )
        )
      ]
  in
  render_file
    ("GetAllRecordsOfAllTypes.mustache", "GetAllRecordsOfAllTypes.java")
    json templdir sample_dir

let _ =
  let templdir = "templates" in
  let class_dir = "autogen/xen-api/src/main/java/com/xensource/xenapi" in
  let sample_dir =
    "autogen/xen-api-samples/src/main/java/com/xensource/xenapi/samples"
  in
  List.iter (fun x -> gen_class x class_dir) classes ;
  gen_types_class class_dir ;
  populate_releases templdir class_dir ;
  gen_get_all_records_test classes templdir sample_dir ;

  let uncommented_license = string_of_file "LICENSE" in
  let class_license = open_out "autogen/xen-api/src/main/resources/LICENSE" in
  let sample_license =
    open_out "autogen/xen-api-samples/src/main/resources/LICENSE"
  in
  output_string class_license uncommented_license ;
  output_string sample_license uncommented_license
