(*
 * Copyright (c) Cloud Software Group, Inc.
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
      sprintf "HashSet<%s>" (get_java_type t1)
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
    ; ("XenAPIException", "if the call failed.")
    ; ( "JsonProcessingException"
      , "if the request's payload or the response's payload cannot be written \
         or read as valid JSON."
      )
    ; ("IOException", "if an I/O error occurs when sending or receiving.")
    ]
  in
  let publishInfo = get_published_info_message message cls in

  fprintf file "    /**\n" ;
  fprintf file "     * %s\n" (escape_xml message.msg_doc) ;
  fprintf file "     * Minimum allowed role: %s\n"
    (get_minimum_allowed_role message) ;
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
    fprintf file "        String methodCall = \"Async.%s.%s\";\n"
      message.msg_obj_name message.msg_name
  else
    fprintf file "        String methodCall = \"%s.%s\";\n" message.msg_obj_name
      message.msg_name ;

  if message.msg_session then
    fprintf file "        String sessionReference = c.getSessionReference();\n"
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

  fprintf file "        Object[] methodParameters = {" ;

  let methodParamsList =
    if message.msg_session then
      "sessionReference" :: get_method_params_for_xml message params
    else
      get_method_params_for_xml message params
  in

  output_string file
    (String.concat ", " (List.map (fun s -> sprintf "%s" s) methodParamsList)) ;

  fprintf file "};\n" ;

  if message.msg_result != None || async_version then
    fprintf file "        var typeReference = new TypeReference<%s>(){};\n"
      (if async_version then "Task" else return_type) ;

  let last_statement =
    match message.msg_result with
    | None when not async_version ->
        "        c.dispatch(methodCall, methodParameters);\n"
    | _ ->
        "        return c.dispatch(methodCall, methodParameters, typeReference);\n"
  in
  fprintf file "%s" last_statement ;

  fprintf file "    }\n\n"

(*Some methods have an almost identical asynchronous counterpart, which returns*)
(* a Task reference rather than its usual return value*)
let gen_method_and_asynchronous_counterpart file cls message =
  let generator x =
    if message.msg_async then gen_method file cls message x true ;
    gen_method file cls message x false
  in
  match message.msg_params with
  | [] ->
      generator []
  | _ ->
      let paramGroups = gen_param_groups message message.msg_params in
      List.iter generator paramGroups

(* Generate the record *)

(* The fields of an object are stored in trees in the datamodel, which means that*)
(* the next three functions, which are conceptually for generating the fields*)
(* of each class, and for the corresponding entries in the toString and toMap*)
(* functions are in fact implemented as three sets of three mutual recursions,*)
(* which take the trees apart. *)

let gen_record_field file prefix field cls =
  let ty = get_java_type field.ty in
  let full_name = String.concat "_" (List.rev (field.field_name :: prefix)) in
  let name = camel_case full_name in
  let publishInfo = get_published_info_field field cls in
  fprintf file "        /**\n" ;
  fprintf file "         * %s\n" (escape_xml field.field_description) ;
  if not (publishInfo = "") then fprintf file "         * %s\n" publishInfo ;
  fprintf file "         */\n" ;
  fprintf file "        @JsonProperty(\"%s\")\n" full_name ;
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
  fprintf file "            Map<String,Object> map = new HashMap<>();\n" ;

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
     import com.fasterxml.jackson.annotation.JsonProperty;\n\
     import com.fasterxml.jackson.core.JsonProcessingException;\n\
     import com.fasterxml.jackson.core.type.TypeReference;\n\
     import com.xensource.xenapi.Types.BadServerResponse;\n\
     import com.xensource.xenapi.Types.XenAPIException;\n\n\
     import java.io.PrintWriter;\n\
     import java.io.StringWriter;\n\
     import java.util.*;\n\
     import java.io.IOException;\n\n" ;
  fprintf file "/**\n" ;
  fprintf file " * %s\n" cls.description ;
  if not (publishInfo = "") then fprintf file " * %s\n" publishInfo ;
  fprintf file " *\n" ;
  fprintf file " * @author Cloud Software Group, Inc.\n" ;
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
    fprintf file "        if (obj instanceof %s)\n" class_name ;
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
    let comment =
      "        /**\n"
      ^ "         * "
      ^ final_description
      ^ "\n"
      ^ "         */\n"
    in
    let json_property =
      if name != "UNRECOGNIZED" then
        "@JsonProperty(\"" ^ name ^ "\")"
      else
        "@JsonEnumDefaultValue"
    in
    comment ^ "        " ^ json_property ^ "\n" ^ "        " ^ enum_of_wire name
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

  fprintf file "            if (errorName.equals(\"%s\"))\n" name ;
  fprintf file "            {\n" ;

  (* Prepare the parameters to the Exception constructor *)
  List.iter
    (fun i ->
      fprintf file
        "                String p%i = errorData.length > %i ? \
        errorData[%i] : \"\";\n"
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
    import java.util.Map;\n\n\
    import com.fasterxml.jackson.annotation.JsonEnumDefaultValue;\n\
    import com.fasterxml.jackson.annotation.JsonProperty;\n\
    import java.io.IOException;\n\n\
     /**\n\
    \ * This class holds enum types and exceptions.\n\
    \ */\n\
     public class Types\n\
     {\n\
    \    /**\n\
    \     * Interface for all Record classes\n\
    \     */\n\
    \    public interface Record\n\
    \    {\n\
    \        /**\n\
    \         * Convert a Record to a Map\n\
    \         */\n\
    \        Map<String, Object> toMap();\n\
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
    \        public BadServerResponse(JsonRpcResponseError responseError)\n\
    \        {\n\
    \            super(String.valueOf(responseError));\n\
    \        }\n\
    \    }\n\n\
    \  " ;

  fprintf file
    "    /**\n\
    \     * Checks the provided server response was successful. If the call \
     failed, throws a XenAPIException. If the server\n\
    \     * returned an invalid response, throws a BadServerResponse. \
     Otherwise, returns the server response as passed in.\n\
    \     */\n\
    \    public static void checkError(JsonRpcResponseError response) throws XenAPIException, \
     BadServerResponse\n\
    \    {\n\
    \        var errorData = response.data;
    \        if(errorData.length == 0){
    \            throw new BadServerResponse(response);
    \        }
    \        var errorName = errorData[0];\n\n" ;

  Hashtbl.iter (gen_method_error_throw file) Datamodel.errors ;

  fprintf file
    "\n\
    \        // An unknown error occurred\n\
    \        throw new Types.XenAPIException(errorData);\n\
    \     }\n\n" ;

  gen_enums file ;
  fprintf file "\n" ;
  Hashtbl.iter (gen_error file) Datamodel.errors ;
  fprintf file "\n" ;
  fprintf file "}\n"

(* Now run it *)

let populate_releases templdir class_dir =
  render_file
    ("APIVersion.mustache", "APIVersion.java")
    json_releases templdir class_dir

let _ =
  let templdir = "templates" in
  let class_dir = "autogen/xen-api/src/main/java/com/xensource/xenapi" in
  List.iter (fun x -> gen_class x class_dir) classes ;
  gen_types_class class_dir ;
  populate_releases templdir class_dir ;

  let uncommented_license = string_of_file "LICENSE" in
  let class_license = open_out "autogen/xen-api/src/main/resources/LICENSE" in
  output_string class_license uncommented_license
