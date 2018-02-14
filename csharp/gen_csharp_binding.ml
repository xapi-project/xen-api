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


open Xapi_stdext_std.Xstringext
open Xapi_stdext_pervasives.Pervasiveext
open Printf
open Datamodel
open Datamodel_types
open Datamodel_utils
open Dm_api
open CommonFunctions

module DT = Datamodel_types
module DU = Datamodel_utils

module TypeSet = Set.Make(struct
    type t = DT.ty
    let compare = compare
  end)

let open_source' = ref false
let destdir'    = ref ""
let sr_xml'     = ref ""
let resx_file'  = ref ""
let templdir'     = ref ""

let get_deprecated_attribute_string version =
  match version with
  | None -> ""
  | Some versionString -> "[Deprecated(\"" ^ get_release_name versionString ^ "\")]"

let get_deprecated_attribute message =
  let version = message.msg_release.internal_deprecated_since in
  get_deprecated_attribute_string version

let _ =
  Arg.parse
    [
      "-r", Arg.Set_string resx_file', "specifies the location of the FriendlyErrorNames.resx file";
      "-s", Arg.Set_string sr_xml', "specifies the location of the XE_SR_ERRORCODES.xml file";
      "-o", Arg.Set open_source', "requests a version of the API filtered for open source";
      "-d", Arg.Set_string destdir', "specifies the destination directory for the generated files";
      "-t", Arg.Set_string templdir', "the directory with the template (mustache) files";
    ]
    (fun x -> raise (Arg.Bad ("Found anonymous argument " ^ x)))
    ("Generates C# bindings for the XenAPI. See -help.")

let open_source = !open_source'
let destdir = !destdir'
let sr_xml = !sr_xml'
let resx_file = !resx_file'
let templdir = !templdir'


let api =
  Datamodel_utils.named_self := true;

  let obj_filter _ = true in
  let field_filter field =
    (not field.internal_only) &&
    ((not open_source && (List.mem "closed" field.release.internal)) ||
     (open_source && (List.mem "3.0.3" field.release.opensource)))
  in
  let message_filter msg =
    Datamodel_utils.on_client_side msg &&
    (* XXX: C# binding generates get_all_records some other way *)
    (msg.msg_tag <> (FromObject GetAllRecords)) &&
    ((not open_source && (List.mem "closed" msg.msg_release.internal)) ||
     (open_source && (List.mem "3.0.3" msg.msg_release.opensource)))
  in
  filter obj_filter field_filter message_filter
    (Datamodel_utils.add_implicit_messages ~document_order:false
       (filter obj_filter field_filter message_filter Datamodel.all_api))

let classes = objects_of_api api
let enums = ref TypeSet.empty
let maps = ref TypeSet.empty

let generated x =
  not (List.mem x.name ["session"; "debug"; "event"])

let proxy_generated x =
  not (List.mem x.name ["debug"; "event"])

let joined sep f l =
  let r = List.map f l in
  String.concat sep
    (List.filter (fun x -> String.compare x "" != 0) r)


let escape_xml s =
  let esc_char = function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | c -> String.make 1 c
  in
  String.concat "" (List.map esc_char (String.explode s))

let enum_of_wire = String.replace "-" "_"

let api_members = ref []

let rec main() =
  gen_proxy CommonFunctions.XmlRpc;
  gen_proxy CommonFunctions.JsonRpc;
  let filtered_classes = List.filter (fun x-> generated x) classes in
  List.iter gen_class_file filtered_classes;
  TypeSet.iter gen_enum !enums;
  gen_maps();
  gen_http_actions();
  gen_relations();
  let sorted_members = List.sort String.compare !api_members in
  let json = `O ["api_members", `A (List.map (fun x -> `O ["api_member", `String x];) sorted_members); ] in
  render_file ("XenServer.csproj.mustache", "XenServer.csproj") json templdir destdir


(* ------------------- category: relations *)

and relations = Hashtbl.create 10

and gen_relations() =
  let out_chan = open_out (Filename.concat destdir "Relation.cs") in
  let print format = fprintf out_chan format in
  List.iter process_relations (relations_of_api api);
  print
    "%s

using System;
using System.Collections.Generic;

namespace XenAPI
{
    public partial class Relation
    {
        public readonly String field;
        public readonly String manyType;
        public readonly String manyField;

        public Relation(String field, String manyType, String manyField)
        {
            this.field = field;
            this.manyField = manyField;
            this.manyType = manyType;
        }

        public static Dictionary<Type, Relation[]> GetRelations()
        {
            Dictionary<Type, Relation[]> relations = new Dictionary<Type, Relation[]>();

" Licence.bsd_two_clause;
  Hashtbl.iter (gen_relations_by_type out_chan) relations;
  print
    "
            return relations;
       }
    }
}
"
and string_ends str en =
  let len = String.length en in
  String.sub str ((String.length str) - len) len = en

and process_relations ((oneClass, oneField), (manyClass, manyField)) =
  let value =
    try
      (manyField, oneClass, oneField) :: (Hashtbl.find relations manyClass)
    with Not_found ->
      begin
        [(manyField, oneClass, oneField)]
      end
  in
  Hashtbl.replace relations manyClass value

and gen_relations_by_type out_chan manyClass relations =
  let print format = fprintf out_chan format in
  print "            relations.Add(typeof(Proxy_%s), new Relation[] {\n" (exposed_class_name manyClass);

  List.iter (gen_relation out_chan) relations;

  print "            });\n\n";

and gen_relation out_chan (manyField, oneClass, oneField) =
  let print format = fprintf out_chan format in
  print "                new Relation(\"%s\", \"%s\", \"%s\"),\n" manyField oneClass oneField

(* ------------------- category: http_actions *)

and gen_http_actions() =
  let out_chan = open_out (Filename.concat destdir "HTTP_actions.cs") in
  let print format = fprintf out_chan format in

  let print_header() = print
      "%s

using System;
using System.Text;
using System.Net;

namespace XenAPI
{
    public partial class HTTP_actions
    {
        private static void Get(HTTP.DataCopiedDelegate dataCopiedDelegate, HTTP.FuncBool cancellingDelegate, int timeout_ms,
            string hostname, string remotePath, IWebProxy proxy, string localPath, params object[] args)
        {
            HTTP.Get(dataCopiedDelegate, cancellingDelegate, HTTP.BuildUri(hostname, remotePath, args), proxy, localPath, timeout_ms);
        }

        private static void Put(HTTP.UpdateProgressDelegate progressDelegate, HTTP.FuncBool cancellingDelegate, int timeout_ms,
            string hostname, string remotePath, IWebProxy proxy, string localPath, params object[] args)
        {
            HTTP.Put(progressDelegate, cancellingDelegate, HTTP.BuildUri(hostname, remotePath, args), proxy, localPath, timeout_ms);
        }"
      Licence.bsd_two_clause
  in

  let print_footer() = print "\n    }\n}\n" in

  let decl_of_sdkarg = function
      String_query_arg s -> "string " ^ (escaped s)
    | Int64_query_arg s -> "long " ^ (escaped s)
    | Bool_query_arg s -> "bool " ^ (escaped s)
    | Varargs_query_arg -> "params string[] args /* alternate names & values */"
  in

  let use_of_sdkarg =  function
      String_query_arg s
    | Int64_query_arg s
    | Bool_query_arg s -> "\"" ^ s ^ "\", " ^ (escaped s)  (* "s", s *)
    | Varargs_query_arg -> "args"
  in

  let string1 = function
      Get -> "HTTP.DataCopiedDelegate dataCopiedDelegate"
    | Put -> "HTTP.UpdateProgressDelegate progressDelegate"
    | _ -> failwith "Unimplemented HTTP method"
  in

  let string2 = function
      Get -> "Get(dataCopiedDelegate"
    | Put -> "Put(progressDelegate"
    | _ -> failwith "Unimplemented HTTP method"
  in

  let enhanced_args args =
    [String_query_arg "task_id"; String_query_arg "session_id"] @ args
  in

  let print_one_action_core name meth uri sdkargs =
    print "

        public static void %s(%s, HTTP.FuncBool cancellingDelegate, int timeout_ms,
            string hostname, IWebProxy proxy, string path, %s)
        {
            %s, cancellingDelegate, timeout_ms, hostname, \"%s\", proxy, path,
                %s);
        }"
      name
      (string1 meth)
      (String.concat ", " (List.map decl_of_sdkarg (enhanced_args sdkargs)))
      (string2 meth)
      uri
      (String.concat ", " (List.map use_of_sdkarg (enhanced_args sdkargs)))
  in

  let print_one_action(name, (meth, uri, sdk, sdkargs, _, _)) =
    match sdk with
    | false -> ()
    | true -> print_one_action_core name meth uri sdkargs
  in

  print_header();
  List.iter print_one_action http_actions;
  print_footer();

  (* ------------------- category: classes *)


and gen_class_file cls =
  let m = exposed_class_name cls.name in
  if not (List.mem m !api_members) then
    api_members := m::!api_members;
  let out_chan = open_out (Filename.concat destdir (exposed_class_name cls.name)^".cs")
  in
  finally (fun () -> gen_class out_chan cls)
    (fun () -> close_out out_chan)

and gen_class out_chan cls =
  let print format = fprintf out_chan format in
  let exposed_class_name = exposed_class_name cls.name in
  let messages = List.filter (fun msg -> (String.compare msg.msg_name "get_all_records_where" != 0)) cls.messages in
  let contents = cls.contents in
  let publishedInfo = get_published_info_class cls in

  print
    "%s

using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Globalization;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;


namespace XenAPI
{
    /// <summary>
    /// %s%s
    /// </summary>
    public partial class %s : XenObject<%s>
    {"

    Licence.bsd_two_clause
    (escape_xml cls.description) (if publishedInfo = "" then "" else "\n    /// "^publishedInfo)
    exposed_class_name exposed_class_name;

  (* Generate bits for Message type *)
  if cls.name = "message" then
    begin
      print "
        public enum MessageType { %s };

        public MessageType Type
        {
            get
            {
                switch (this.name)
                {"
        (String.concat ", " ((List.map fst !Api_messages.msgList) @ ["unknown"]));

      List.iter (fun x -> print "
                    case \"%s\":
                        return MessageType.%s;" x x) (List.map fst !Api_messages.msgList);

      print
        "
                    default:
                        return MessageType.unknown;
                }
            }
        }
"
    end;

  print
    "
        public %s()
        {
        }
"
    exposed_class_name;

  let print_internal_ctor = function
    | []            -> ()
    | _ as cnt -> print
                    "
        public %s(%s)
        {
            %s
        }
"
                    exposed_class_name
                    (String.concat ",\n            " (List.rev (get_constructor_params cnt)))
                    (String.concat "\n            " (List.rev (get_constructor_body cnt)))
  in print_internal_ctor contents;

  print
    "
        /// <summary>
        /// Creates a new %s from a Proxy_%s.
        /// </summary>
        /// <param name=\"proxy\"></param>
        public %s(Proxy_%s proxy)
        {
            this.UpdateFromProxy(proxy);
        }

        public override void UpdateFrom(%s update)
        {
"
    exposed_class_name exposed_class_name
    exposed_class_name exposed_class_name
    exposed_class_name;

  List.iter (gen_updatefrom_line out_chan) contents;

  print
    "        }

        internal void UpdateFromProxy(Proxy_%s proxy)
        {
"
    exposed_class_name;

  List.iter (gen_constructor_line out_chan) contents;

  print
    "        }

        public Proxy_%s ToProxy()
        {
            Proxy_%s result_ = new Proxy_%s();
"
    exposed_class_name
    exposed_class_name exposed_class_name;

  List.iter (gen_to_proxy_line out_chan) contents;

  print
    "            return result_;
        }
";

  print "
        /// <summary>
        /// Creates a new %s from a Hashtable.
        /// </summary>
        /// <param name=\"table\"></param>
        public %s(Hashtable table)
        {
"
    exposed_class_name exposed_class_name;

  List.iter (gen_hashtable_constructor_line out_chan) contents;

  print
    "        }

        ";

  let is_current_ops = function
    | Field f -> (full_name f = "current_operations")
    | _ -> false
  in
  let (current_ops, other_contents) = List.partition is_current_ops contents in
  let check_refs = "if (ReferenceEquals(null, other))
                return false;
            if (ReferenceEquals(this, other))
                return true;" in
  (match current_ops with
   | [] ->
     print "public bool DeepEquals(%s other)
        {
            %s

            return " exposed_class_name check_refs
   | _ ->
     print "public bool DeepEquals(%s other, bool ignoreCurrentOperations)
        {
            %s

            if (!ignoreCurrentOperations && !Helper.AreEqual2(this.current_operations, other.current_operations))
                return false;

            return " exposed_class_name check_refs);

  (match other_contents with
   | [] -> print "false"
   | _ ->  print "%s" (String.concat " &&
                " (List.map gen_equals_condition other_contents)));

  print ";
        }

        internal static List<%s> ProxyArrayToObjectList(Proxy_%s[] input)
        {
            var result = new List<%s>();
            foreach (var item in input)
                result.Add(new %s(item));

            return result;
        }

        public override string SaveChanges(Session session, string opaqueRef, %s server)
        {
            if (opaqueRef == null)
            {"
    exposed_class_name exposed_class_name exposed_class_name exposed_class_name exposed_class_name;

  if cls.gen_constructor_destructor then
    print "
                var reference = create(session, this);
                return reference == null ? null : reference.opaque_ref;
"
  else
    print
      "
                System.Diagnostics.Debug.Assert(false, \"Cannot create instances of this type on the server\");
                return \"\";
";

  print
    "            }
            else
            {
";

  gen_save_changes out_chan exposed_class_name messages contents;

  print
    "
            }
        }";

  List.iter (gen_exposed_method_overloads out_chan cls) (List.filter (fun x -> not x.msg_hide_from_docs) messages);

  (* Don't create duplicate get_all_records call *)
  if not (List.exists (fun msg -> String.compare msg.msg_name "get_all_records" = 0) messages) &&
     List.mem cls.name expose_get_all_messages_for
  then gen_exposed_method out_chan cls (get_all_records_method cls.name) [];

  List.iter (gen_exposed_field out_chan cls) contents;

  print
    "    }
}
";

and get_all_records_method classname =
  { default_message with
    msg_name = "get_all_records";
    msg_params = [];
    msg_result = Some (Map(Ref classname, Record classname),
                       sprintf "A map from %s to %s.Record" classname classname);
    msg_doc = sprintf "Get all the %s Records at once, in a single XML RPC call" classname;
    msg_session = true; msg_async = false;
    msg_release = {opensource=["3.0.3"]; internal=["closed"; "debug"]; internal_deprecated_since=None};
    msg_lifecycle = [];
    msg_has_effect = false; msg_tag = Custom;
    msg_obj_name = classname;
    msg_errors = []; msg_secret = false;
    msg_custom_marshaller = false;
    msg_no_current_operations = false;
    msg_hide_from_docs = false;
    msg_pool_internal = false;
    msg_db_only = false;
    msg_force_custom = None;
    msg_allowed_roles = None;
    msg_map_keys_roles = [];
    msg_doc_tags = [];
  };

and get_constructor_params content =
  get_constructor_params' content []

and get_constructor_params' content elements =
  match content with
    [] -> elements
  | (Field fr)::others -> get_constructor_params' others ((sprintf "%s %s" (exposed_type fr.ty) (full_name fr))::elements)
  | (Namespace (_, c))::others -> get_constructor_params' (c@others) elements

and get_constructor_body content =
  get_constructor_body' content []

and get_constructor_body' content elements =
  match content with
    [] -> elements
  | (Field fr)::others -> get_constructor_body' others ((sprintf "this.%s = %s;" (full_name fr) (full_name fr))::elements)
  | (Namespace (_, c))::others -> get_constructor_body' (c@others) elements

and gen_constructor_line out_chan content =
  let print format = fprintf out_chan format in

  match content with
    Field fr ->
    print
      "            %s = %s;
" (full_name fr) (convert_from_proxy ("proxy." ^ (full_name fr)) fr.ty)

  | Namespace (_, c) -> List.iter (gen_constructor_line out_chan) c

and gen_hashtable_constructor_line out_chan content =
  let print format = fprintf out_chan format in

  match content with
  | Field fr ->
    print
      "            %s = %s;
" (full_name fr) (convert_from_hashtable (full_name fr) fr.ty)

  | Namespace (_, c) -> List.iter (gen_hashtable_constructor_line out_chan) c

and gen_equals_condition content =
  match content with
  | Field fr -> "Helper.AreEqual2(this._" ^ (full_name fr) ^ ", other._" ^ (full_name fr) ^ ")"
  | Namespace (_, c) -> String.concat " &&
                " (List.map gen_equals_condition c);

and gen_updatefrom_line out_chan content =
  let print format = fprintf out_chan format in

  match content with
    Field fr ->
    print
      "            %s = %s;
" (full_name fr) ("update." ^ (full_name fr))
  | Namespace (_, c) -> List.iter (gen_updatefrom_line out_chan) c

and gen_to_proxy_line out_chan content =
  let print format = fprintf out_chan format in

  match content with
    Field fr ->
    print
      "            result_.%s = %s;
" (full_name fr) (convert_to_proxy (full_name fr) fr.ty)

  | Namespace (_, c) -> List.iter (gen_to_proxy_line out_chan) c

and gen_overload out_chan classname message generator =
  let methodParams = get_method_params_list message in
  match methodParams with
  | [] -> generator []
  | _  -> let paramGroups =  gen_param_groups message methodParams in
    List.iter generator paramGroups

and gen_exposed_method_overloads out_chan cls message =
  let generator = fun x -> gen_exposed_method out_chan cls message x in
  gen_overload out_chan cls.name message generator

and gen_exposed_method out_chan cls msg curParams =
  let classname = cls.name in
  let print format = fprintf out_chan format in
  let proxyMsgName = proxy_msg_name classname msg in
  let exposed_ret_type = exposed_type_opt msg.msg_result in
  let paramSignature = exposed_params msg classname curParams in
  let paramsDoc = get_params_doc msg classname curParams in
  let callParams = exposed_call_params ~json:false msg classname curParams in
  let jsonCallParams = exposed_call_params ~json:true msg classname curParams in
  let publishInfo = get_published_info_message msg cls in
  let deprecatedInfo = get_deprecated_info_message msg in
  let deprecatedAttribute = get_deprecated_attribute msg in
  let deprecatedInfoString = (if deprecatedInfo = "" then "" else "\n        /// "^deprecatedInfo) in
  let deprecatedAttributeString = (if deprecatedAttribute = "" then "" else "\n        "^deprecatedAttribute) in
  print "
        /// <summary>
        /// %s%s%s
        /// </summary>%s%s
        public static %s %s(%s)
        {
            if (session.JsonRpcClient != null)
                %s;
            else
                %s;
        }\n"
    msg.msg_doc (if publishInfo = "" then "" else "\n        /// "^publishInfo)
    deprecatedInfoString
    paramsDoc
    deprecatedAttributeString
    exposed_ret_type
    msg.msg_name paramSignature
    (json_return_opt (sprintf "session.JsonRpcClient.%s(%s)" proxyMsgName jsonCallParams) msg.msg_result)
    (convert_from_proxy_opt (sprintf "session.proxy.%s(%s).parse()" proxyMsgName callParams) msg.msg_result);
  if msg.msg_async then
    print "
        /// <summary>
        /// %s%s%s
        /// </summary>%s%s
        public static XenRef<Task> async_%s(%s)
        {
          if (session.JsonRpcClient != null)
              return session.JsonRpcClient.async_%s(%s);
          else
              return XenRef<Task>.Create(session.proxy.async_%s(%s).parse());
        }\n"
      msg.msg_doc (if publishInfo = "" then "" else "\n        /// "^publishInfo)
      deprecatedInfoString
      paramsDoc
      deprecatedAttributeString
      msg.msg_name paramSignature
      proxyMsgName jsonCallParams
      proxyMsgName callParams

and returns_xenobject msg =
  match msg.msg_result with
  |  Some (Record r, _) -> true
  |  _ -> false

and get_params_doc msg classname params =
  let sessionDoc = "\n        /// <param name=\"session\">The session</param>" in
  let refDoc =  if is_method_static msg then ""
    else if (msg.msg_name = "get_by_permission") then
      sprintf "\n        /// <param name=\"_%s\">The opaque_ref of the given permission</param>" (String.lowercase_ascii classname)
    else if (msg.msg_name = "revert") then
      sprintf "\n        /// <param name=\"_%s\">The opaque_ref of the given snapshotted state</param>" (String.lowercase_ascii classname)
    else sprintf "\n        /// <param name=\"_%s\">The opaque_ref of the given %s</param>"
        (String.lowercase_ascii classname) (String.lowercase_ascii classname) in
  String.concat "" (sessionDoc::(refDoc::(List.map (fun x -> get_param_doc msg x) params)))

and get_param_doc msg x =
  let publishInfo = get_published_info_param msg x in
  sprintf "\n        /// <param name=\"_%s\">%s%s</param>" (String.lowercase_ascii x.param_name) (escape_xml x.param_doc)
    (if publishInfo = "" then "" else " "^publishInfo)

and exposed_params message classname params =
  let exposedParams = List.map exposed_param params in
  let refParam = sprintf "string _%s" (String.lowercase_ascii classname) in
  let exposedParams = if is_method_static message then exposedParams else refParam::exposedParams in
  String.concat ", " ("Session session"::exposedParams)

and exposed_param p =
  sprintf "%s _%s" (internal_type p.param_type) (String.lowercase_ascii p.param_name)

and exposed_call_params ~json message classname params =
  let exposed_call_param json p =
    let pName = String.lowercase_ascii p.param_name in
    if json then sprintf "_%s" pName
    else convert_to_proxy (sprintf "_%s" pName) p.param_type
  in
  let exposedParams = List.map (exposed_call_param json) params in
  let name = String.lowercase_ascii classname in
  let refParam = if json then sprintf "_%s" name else sprintf "_%s ?? \"\"" name in
  let exposedParams = if is_method_static message then exposedParams else refParam::exposedParams in
  String.concat ", " ("session.uuid"::exposedParams)


(* 'messages' are methods, 'contents' are fields *)
and gen_save_changes out_chan exposed_class_name messages contents =
  let fields = List.flatten (List.map flatten_content contents) in
  let fields2 = List.filter (fun fr -> fr.qualifier == RW && (not (List.mem "public" fr.full_name))) fields in
  (* Find all StaticRO fields which have corresponding messages (methods) of the form set_readonlyField *)
  let readonlyFieldsWithSetters = List.filter (fun field -> field.qualifier == StaticRO && List.exists (fun msg -> msg.msg_name = (String.concat "" ["set_"; full_name field])) messages) fields in
  let length = List.length fields2 + List.length readonlyFieldsWithSetters in
  let print format = fprintf out_chan format in
  if length == 0 then
    print
      "              throw new InvalidOperationException(\"This type has no read/write properties\");"
  else
    (List.iter (gen_save_changes_to_field out_chan exposed_class_name) fields2;
     (* Generate calls to any set_ methods *)
     List.iter (gen_save_changes_to_field out_chan exposed_class_name) readonlyFieldsWithSetters;
     print
       "
                return null;";)


and flatten_content content =
  match content with
    Field fr -> [ fr ]
  | Namespace (_, c) -> List.flatten (List.map flatten_content c)


and gen_save_changes_to_field out_chan exposed_class_name fr =
  let print format = fprintf out_chan format in
  let full_name_fr = full_name fr in
  let equality =
    (* Use AreEqual2 - see CA-19220 *)
    sprintf "Helper.AreEqual2(_%s, server._%s)" full_name_fr full_name_fr
  in
  print
    "                if (!%s)
                {
                    %s.set_%s(session, opaqueRef, _%s);
                }
" equality exposed_class_name full_name_fr full_name_fr


and ctor_call classname =
  let fields = Datamodel_utils.fields_of_obj (Dm_api.get_obj_by_name api ~objname:classname) in
  let fields2 = ctor_fields fields in
  let args = (List.map (fun fr -> "p." ^ (full_name fr)) fields2) in
  String.concat ", " ("session.uuid" :: args)


and gen_exposed_field out_chan cls content =
  match content with
  | Field fr ->
    let print format = fprintf out_chan format in
    let full_name_fr = full_name fr in
    let comp = sprintf "!Helper.AreEqual(value, _%s)" full_name_fr in
    let publishInfo = get_published_info_field fr cls in

    print "
        /// <summary>
        /// %s%s
        /// </summary>%s
        public virtual %s %s
        {
            get { return _%s; }" (escape_xml fr.field_description)
      (if publishInfo = "" then "" else "\n        /// "^publishInfo)
      (json_serialization_attr fr.ty)
      (exposed_type fr.ty) full_name_fr full_name_fr;

    print
      "
            set
            {
                if (%s)
                {
                    _%s = value;
                    Changed = true;
                    NotifyPropertyChanged(\"%s\");
                }
            }
        }" comp full_name_fr full_name_fr;

    print "
        private %s _%s%s;\n" (exposed_type fr.ty) full_name_fr
      (get_default_value_opt fr)

  | Namespace (_, c) -> List.iter (gen_exposed_field out_chan cls) c

(* ------------------- category: proxy classes *)

and gen_proxy protocol =
  let output_file =
    match protocol with
    | CommonFunctions.XmlRpc -> "Proxy.cs"
    | CommonFunctions.JsonRpc -> "JsonRpcClient.cs"
  in
  let out_chan = open_out (Filename.concat destdir output_file)
  in
  finally (fun () -> gen_proxy' protocol out_chan)
    (fun () -> close_out out_chan)

and gen_proxy' protocol out_chan =
  let print format = fprintf out_chan format in
  print "%s" Licence.bsd_two_clause;
  match protocol with
  | CommonFunctions.XmlRpc -> print "

using System;
using System.Collections;
using System.Collections.Generic;

using CookComputing.XmlRpc;


namespace XenAPI
{
    public partial interface Proxy : IXmlRpcProxy
    {
        [XmlRpcMethod(\"event.get_record\")]
        Response<Proxy_Event>
        event_get_record(string session, string _event);

        [XmlRpcMethod(\"event.get_by_uuid\")]
        Response<string>
        event_get_by_uuid(string session, string _uuid);

        [XmlRpcMethod(\"event.get_id\")]
        Response<string>
        event_get_id(string session, string _event);

        [XmlRpcMethod(\"event.set_id\")]
        Response<string>
        event_set_id(string session, string _event, string _id);

        [XmlRpcMethod(\"event.register\")]
        Response<string>
        event_register(string session, string [] _classes);

        [XmlRpcMethod(\"event.unregister\")]
        Response<string>
        event_unregister(string session, string [] _classes);

        [XmlRpcMethod(\"event.next\")]
        Response<Proxy_Event[]>
        event_next(string session);

        [XmlRpcMethod(\"event.from\")]
        Response<Events>
        event_from(string session, string [] _classes, string _token, double _timeout);
";
    List.iter (fun x -> if proxy_generated x then gen_proxy_for_class protocol out_chan x) classes;
    print
      "    }

";
    List.iter (fun x -> if proxy_generated x then gen_proxyclass out_chan x) classes;
    print
      "}
"
  | CommonFunctions.JsonRpc -> print "

using System;
using System.Collections.Generic;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using Newtonsoft.Json.Linq;


namespace XenAPI
{
    public partial class JsonRpcClient
    {
        public Event event_get_record(string session, string _event)
        {
            var converters = new List<JsonConverter> {};
            var serializer = JsonSerializer.Create(new JsonSerializerSettings { Converters = converters });
            return Rpc<Event>(\"event.get_record\", new JArray(session, _event ?? \"\"), serializer);
        }

        public string event_get_by_uuid(string session, string _uuid)
        {
            var converters = new List<JsonConverter> {};
            var serializer = JsonSerializer.Create(new JsonSerializerSettings { Converters = converters });
            return Rpc<string>(\"event.get_by_uuid\", new JArray(session, _uuid ?? \"\"), serializer);
        }

        public long event_get_id(string session, string _event)
        {
            var converters = new List<JsonConverter> {};
            var serializer = JsonSerializer.Create(new JsonSerializerSettings { Converters = converters });
            return Rpc<long>(\"event.get_id\", new JArray(session, _event ?? \"\"), serializer);
        }

        public void event_set_id(string session, string _event, long _id)
        {
            var converters = new List<JsonConverter> {};
            var serializer = JsonSerializer.Create(new JsonSerializerSettings { Converters = converters });
            Rpc(\"event.set_id\", new JArray(session, _event ?? \"\", _id), serializer);
        }

        public void event_register(string session, string[] _classes)
        {
            var converters = new List<JsonConverter> {};
            var serializer = JsonSerializer.Create(new JsonSerializerSettings { Converters = converters });
            Rpc(\"event.register\", new JArray(session, JArray.FromObject(_classes ?? new string[] {})), serializer);
        }

        public void event_unregister(string session, string[] _classes)
        {
            var converters = new List<JsonConverter> {};
            var serializer = JsonSerializer.Create(new JsonSerializerSettings { Converters = converters });
            Rpc(\"event.unregister\", new JArray(session, JArray.FromObject(_classes ?? new string[] {})), serializer);
        }

        public EventBatch event_from(string session, string[] _classes, string _token, double _timeout)
        {
            var converters = new List<JsonConverter> {};
            var serializer = JsonSerializer.Create(new JsonSerializerSettings { Converters = converters });
            return Rpc<EventBatch>(\"event.from\", new JArray(session, JArray.FromObject(_classes ?? new string[] {}), _token ?? \"\", _timeout), serializer);
        }
";
    List.iter (fun x -> if proxy_generated x then gen_proxy_for_class protocol out_chan x) classes;
    print "
    }
}
"

and gen_proxy_for_class protocol out_chan {name=classname; messages=messages} =
  (* Generate each of the proxy methods (but not the internal-only ones that are marked hide_from_docs) *)
  List.iter (gen_proxy_method_overloads protocol out_chan classname) (List.filter (fun x -> not x.msg_hide_from_docs) messages);
  if (not (List.exists (fun msg -> String.compare msg.msg_name "get_all_records" = 0) messages)) then
    gen_proxy_method protocol out_chan classname (get_all_records_method classname) []

and gen_proxy_method_overloads protocol out_chan classname message =
  let generator = fun x -> gen_proxy_method protocol out_chan classname message x in
  gen_overload out_chan classname message generator

and gen_proxy_method protocol out_chan classname message params =
  let print format = fprintf out_chan format in
  let proxy_ret_type = proxy_type_opt message.msg_result in
  let proxy_msg_name = proxy_msg_name classname message in
  let proxyParams = proxy_params ~with_types:true ~json:false message classname params in
  let paramsJsonWithTypes = proxy_params ~with_types:true ~json:true message classname params in
  let paramsJsonNoTypes = proxy_params ~with_types:false ~json:true message classname params in
  match protocol with
  | CommonFunctions.XmlRpc ->
    print "
        [XmlRpcMethod(\"%s.%s\")]
        Response<%s>
        %s(%s);
" classname message.msg_name proxy_ret_type proxy_msg_name proxyParams;

    if message.msg_async then
      print "
        [XmlRpcMethod(\"Async.%s.%s\")]
        Response<string>
        async_%s(%s);
" classname message.msg_name proxy_msg_name proxyParams;

  | CommonFunctions.JsonRpc ->
    let return_word =
      match message.msg_result with
      | Some (typ, _) -> "return "
      | None -> ""
    in
    let param_converters = List.map (fun x -> json_converter x.param_type) params in
    let converters = (json_converter_opt message.msg_result)::param_converters |> List.filter (fun x-> x <> "") in
    let async_converters = "new XenRefConverter<Task>()":: param_converters |> List.filter (fun x-> x <> "") in
    print "
        public %s %s(%s)
        {
            var converters = new List<JsonConverter> {%s};
            var serializer = JsonSerializer.Create(new JsonSerializerSettings {Converters = converters});
            %sRpc%s(\"%s.%s\", new JArray(%s), serializer);
        }
" (exposed_type_opt message.msg_result) proxy_msg_name paramsJsonWithTypes
      (String.concat ", " converters)
      return_word (json_deserialise_opt message.msg_result) classname message.msg_name
      paramsJsonNoTypes;

    if message.msg_async then
      print "
        public XenRef<Task> async_%s(%s)
        {
            var converters = new List<JsonConverter> {%s};
            var serializer = JsonSerializer.Create(new JsonSerializerSettings {Converters = converters});
            return Rpc<XenRef<Task>>(\"Async.%s.%s\", new JArray(%s), serializer);
        }
" proxy_msg_name paramsJsonWithTypes (String.concat ", " async_converters)
        classname message.msg_name paramsJsonNoTypes


and proxy_params ~with_types ~json message classname params =
  let refParam =
    if json then
      sprintf (if with_types then "string _%s" else "_%s ?? \"\"") (String.lowercase_ascii classname)
    else
      sprintf (if with_types then "string _%s" else "_%s") (String.lowercase_ascii classname)
  in
  let args = List.map (proxy_param ~with_types ~json) params in
  let args = if is_method_static message then args else refParam::args in
  let args =
    if message.msg_session then (if with_types then "string session" else "session") :: args
    else args
  in
  String.concat ", " args

and proxy_param ~with_types ~json p =
  if json then (
    if with_types then (
      let exposed_type_json = function
        | Ref name -> "string"
        | _ as x -> exposed_type x
      in
      sprintf "%s _%s" (exposed_type_json p.param_type) (String.lowercase_ascii p.param_name)
    )
    else
      json_param p
  )
  else (
    if with_types then
      sprintf "%s _%s" (proxy_type p.param_type) (String.lowercase_ascii p.param_name)
    else
      sprintf "_%s" (String.lowercase_ascii p.param_name)
  )


and ctor_fields fields =
  List.filter (function { DT.qualifier = (DT.StaticRO | DT.RW) } -> true | _ -> false) fields


and gen_proxyclass out_chan {name=classname; contents=contents} =
  let print format = fprintf out_chan format in

  print
    "    [XmlRpcMissingMapping(MappingAction.Ignore)]
    public class Proxy_%s
    {
" (exposed_class_name classname);

  List.iter (gen_proxy_field out_chan) contents;

  print
    "    }

"


and gen_proxy_field out_chan content =
  match content with
    Field fr ->
    let print format = fprintf out_chan format in

    print
      "        public %s %s;
" (proxy_type fr.ty) (full_name fr)

  | Namespace (_, c) -> List.iter (gen_proxy_field out_chan) c


(* ------------------- category: enums *)


and gen_enum = function
  | Enum(name, contents) ->
    if not (List.mem name !api_members) then
      api_members := name::!api_members;
    let out_chan = open_out (Filename.concat destdir (name ^ ".cs"))
    in
    finally (fun () -> gen_enum' name contents out_chan)
      (fun () -> close_out out_chan)
  | _ -> assert false


and gen_enum' name contents out_chan =
  let print format = fprintf out_chan format in

  print "%s

using Newtonsoft.Json;


namespace XenAPI
{
    [JsonConverter(typeof(%sConverter))]
    public enum %s
    {
        " Licence.bsd_two_clause name name;

  print "%s" (joined ", " gen_enum_line contents);

  if not (has_unknown_entry contents) then
    print ", unknown";

  print "
    }

    public static class %s_helper
    {
        public static string ToString(%s x)
        {
            return x.StringOf();
        }
    }

    public static partial class EnumExt
    {
        public static string StringOf(this %s x)
        {
            switch (x)
            {
" name name name;

  List.iter (fun (wire, _) ->
      print "                case %s.%s:\n                    return \"%s\";\n" name (enum_of_wire wire) wire
    ) contents;

  print "                default:
                    return \"unknown\";
            }
        }
    }

    internal class %sConverter : XenEnumConverter
    {
        public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
        {
            writer.WriteValue(((%s)value).StringOf());
        }
    }
}
" name name


and gen_enum_line content =
  enum_of_wire (fst content)


and has_unknown_entry contents =
  let rec f = function
    | x :: xs -> if String.lowercase_ascii (fst x) = "unknown" then true else f xs
    | []      -> false
  in
  f contents


(* ------------------- category: maps *)


and gen_maps() =
  let out_chan = open_out (Filename.concat destdir "Maps.cs")
  in
  finally (fun () -> gen_maps' out_chan)
    (fun () -> close_out out_chan)


and gen_maps' out_chan =
  let print format = fprintf out_chan format in

  print "%s

using System;
using System.Collections;
using System.Collections.Generic;
using CookComputing.XmlRpc;


namespace XenAPI
{
    internal class Maps
    {"
    Licence.bsd_two_clause;

  TypeSet.iter (gen_map_conversion out_chan) !maps;

  print "
    }
}
"

and gen_map_conversion out_chan = function
    Map(l, r) ->
    let print format = fprintf out_chan format in
    let el = exposed_type l in
    let el_literal = exposed_type_as_literal l in
    let er = exposed_type r in
    let er_literal = exposed_type_as_literal r in

    print "
        internal static Dictionary<%s, %s> convert_from_proxy_%s_%s(Object o)
        {
            Hashtable table = (Hashtable)o;
            Dictionary<%s, %s> result = new Dictionary<%s, %s>();
            if (table != null)
            {
                foreach (string key in table.Keys)
                {
                    try
                    {
                        %s k = %s;
                        %s v = %s;
                        result[k] = v;
                    }
                    catch
                    {
                        continue;
                    }
                }
            }
            return result;
        }

        internal static Hashtable convert_to_proxy_%s_%s(Dictionary<%s, %s> table)
        {
            var result = new XmlRpcStruct();
            if (table != null)
            {
                foreach (%s key in table.Keys)
                {
                    try
                    {
                        %s k = %s;
                        %s v = %s;
                        result[k] = v;
                    }
                    catch
                    {
                        continue;
                    }
                }
            }
            return result;
        }

" el er (sanitise_function_name el_literal) (sanitise_function_name er_literal)
      el er el er
      el (convert_from_proxy_never_null_string "key" l)
      er (convert_from_proxy_hashtable_value "table[key]" r)
      (sanitise_function_name el_literal) (sanitise_function_name er_literal) el er el
      (proxy_type l) (convert_to_proxy "key" l)
      (proxy_type r) (convert_to_proxy "table[key]" r)
  (***)

  | _ -> assert false


(* ------------------- category: utility *)


and proxy_type_opt = function
    Some (typ, _) -> proxy_type typ
  | None -> "string"


and proxy_type = function
  | String              -> "string"
  | Int                 -> "string"
  | Float               -> "double"
  | Bool                -> "bool"
  | DateTime            -> "DateTime"
  | Ref name            -> "string"
  | Set (Record name)   -> "Proxy_" ^ exposed_class_name name ^ "[]"
  | Set _               -> "string []"
  | Enum _              -> "string"
  | Map _               -> "Object"
  | Record name         -> "Proxy_" ^ exposed_class_name name

and exposed_type_opt = function
    Some (typ, _) -> exposed_type typ
  | None -> "void"

and exposed_type = function
  | String                  -> "string"
  | Int                     -> "long"
  | Float                   -> "double"
  | Bool                    -> "bool"
  | DateTime                -> "DateTime"
  | Ref name                -> sprintf "XenRef<%s>" (exposed_class_name name)
  | Set(Ref name)           -> sprintf "List<XenRef<%s>>" (exposed_class_name name)
  | Set(Enum(name, _) as x) -> enums := TypeSet.add x !enums;
    sprintf "List<%s>" name
  | Set(Int)                -> "long[]"
  | Set(String)             -> "string[]"
  | Enum(name, _) as x      -> enums := TypeSet.add x !enums; name
  | Map(u, v)               -> sprintf "Dictionary<%s, %s>" (exposed_type u)
                                 (exposed_type v)
  | Record name             -> exposed_class_name name
  | Set(Record name)        -> sprintf "List<%s>" (exposed_class_name name)
  | _                       -> assert false


and internal_type = function
  | Ref name                -> (* THIS SHOULD BE: Printf.sprintf "XenRef<%s>" name *) "string"
  | Set(Ref name)           -> Printf.sprintf "List<XenRef<%s>>" (exposed_class_name name)
  | x                       -> exposed_type x


and exposed_type_as_literal = function
  | Set(String)             -> "string_array"
  | Map(u, v)               -> sprintf "Dictionary_%s_%s" (exposed_type u) (exposed_type v)
  | x                       -> exposed_type x

and convert_from_proxy_opt thing = function
    Some (typ, _) -> "return " ^ simple_convert_from_proxy thing typ
  | None -> thing

and convert_from_proxy_hashtable_value thing ty =
  match ty with
  | Set(String)         -> sprintf "%s == null ? new string[] {} : Array.ConvertAll<object, string>((object[])%s, Convert.ToString)" thing thing
  | _                   -> convert_from_proxy thing ty

and convert_from_proxy thing ty = (*function*)
  match ty with
  | DateTime            -> thing
  | Bool                -> simple_convert_from_proxy thing ty
  | Float               -> simple_convert_from_proxy thing ty
  | Int                 -> sprintf "%s == null ? 0 : %s" thing (simple_convert_from_proxy thing ty)
  | Set(String)         -> sprintf "%s == null ? new string[] {} : %s" thing (simple_convert_from_proxy thing ty)
  | Enum(name, _)       -> sprintf "%s == null ? (%s) 0 : %s" thing name (simple_convert_from_proxy thing ty)
  | _                   -> sprintf "%s == null ? null : %s" thing (simple_convert_from_proxy thing ty)

and convert_from_proxy_never_null_string thing ty = (* for when 'thing' is never null and is a string - i.e. it is a key in a hashtable *)
  match ty with
  | DateTime            -> thing
  | String              -> thing
  | Int                 -> sprintf "long.Parse(%s)" thing
  | _                   -> simple_convert_from_proxy thing ty

and convert_from_hashtable fname ty =
  let field = sprintf "\"%s\"" fname in
  match ty with
  | DateTime            -> sprintf "Marshalling.ParseDateTime(table, %s)" field
  | Bool                -> sprintf "Marshalling.ParseBool(table, %s)" field
  | Float               -> sprintf "Marshalling.ParseDouble(table, %s)" field
  | Int                 -> sprintf "Marshalling.ParseLong(table, %s)" field
  | Ref name            -> sprintf "Marshalling.ParseRef<%s>(table, %s)" (exposed_class_name name) field
  | String              -> sprintf "Marshalling.ParseString(table, %s)" field
  | Set(String)         -> sprintf "Marshalling.ParseStringArray(table, %s)" field
  | Set(Ref name)       -> sprintf "Marshalling.ParseSetRef<%s>(table, %s)" (exposed_class_name name) field
  | Set(Enum(name, _))  -> sprintf "Helper.StringArrayToEnumList<%s>(Marshalling.ParseStringArray(table, %s))" name field
  | Enum(name, _)       -> sprintf "(%s)Helper.EnumParseDefault(typeof(%s), Marshalling.ParseString(table, %s))" name name field
  | Map(Ref name, Record _) -> sprintf "Marshalling.ParseMapRefRecord<%s, Proxy_%s>(table, %s)" (exposed_class_name name) (exposed_class_name name) field
  | Map(u, v) as x      ->
    maps := TypeSet.add x !maps;
    sprintf "%s(Marshalling.ParseHashTable(table, %s))"
      (sanitise_function_name (sprintf "Maps.convert_from_proxy_%s_%s" (exposed_type_as_literal u) (exposed_type_as_literal v))) field
  | Record name         ->
    sprintf "new %s((Proxy_%s)table[%s])"
      (exposed_class_name name) (exposed_class_name name) field
  | Set(Record name)    ->
    sprintf "%s.ProxyArrayToObjectList(Marshalling.ParseStringArray(%s))"
      (exposed_class_name name) field
  | Set(Int)            -> sprintf "Marshalling.ParseLongArray(table, %s)" field
  | _                   -> assert false

and sanitise_function_name name =
  String.implode (List.filter (fun c -> c<>'>' && c<>'<' && c<>',' && c<>' ') (String.explode name))

and simple_convert_from_proxy thing ty =
  match ty with
  | DateTime            -> thing
  | Int                 -> sprintf "long.Parse((string)%s)" thing
  | Bool                -> sprintf "(bool)%s" thing
  | Float               -> sprintf "Convert.ToDouble(%s)" thing
  | Ref name            -> sprintf "XenRef<%s>.Create(%s)" (exposed_class_name name) thing
  | String              -> sprintf "(string)%s" thing
  | Set(String)         -> sprintf "(string [])%s" thing
  | Set(Ref name)       -> sprintf "XenRef<%s>.Create(%s)" (exposed_class_name name) thing
  | Set(Enum(name, _))  -> sprintf "Helper.StringArrayToEnumList<%s>(%s)" name thing
  | Enum(name, _)       -> sprintf "(%s)Helper.EnumParseDefault(typeof(%s), (string)%s)" name name thing
  | Map(Ref name, Record _) -> sprintf "XenRef<%s>.Create<Proxy_%s>(%s)" (exposed_class_name name) (exposed_class_name name) thing
  | Map(u, v) as x      ->
    maps := TypeSet.add x !maps;
    sprintf "%s(%s)"
      (sanitise_function_name (sprintf "Maps.convert_from_proxy_%s_%s" (exposed_type_as_literal u) (exposed_type_as_literal v))) thing
  | Record name         ->
    sprintf "new %s((Proxy_%s)%s)"
      (exposed_class_name name) (exposed_class_name name) thing
  | Set(Record name)    ->
    sprintf "%s.ProxyArrayToObjectList(%s)"
      (exposed_class_name name) thing
  | Set(Int)            ->
    sprintf "Helper.StringArrayToLongArray(%s)" thing
  | _                   -> assert false


and convert_to_proxy thing ty =
  match ty with
  | DateTime            -> thing
  | Int                 -> sprintf "%s.ToString()" thing
  | Bool
  | Float               -> thing
  | Ref _               -> sprintf "%s ?? \"\"" thing
  | String              -> sprintf "%s ?? \"\"" thing
  | Enum (name,_)       -> sprintf "%s_helper.ToString(%s)" name thing
  | Set (Ref name)         -> sprintf "(%s != null) ? Helper.RefListToStringArray(%s) : new string[] {}" thing thing
  | Set(String)         -> thing
  | Set (Int) -> sprintf "(%s != null) ? Helper.LongArrayToStringArray(%s) : new string[] {}" thing thing
  | Set(Enum(_, _))  -> sprintf "(%s != null) ? Helper.ObjectListToStringArray(%s) : new string[] {}" thing thing
  | Map(u, v) as x      -> maps := TypeSet.add x !maps;
    sprintf "%s(%s)"
      (sanitise_function_name (sprintf "Maps.convert_to_proxy_%s_%s" (exposed_type_as_literal u) (exposed_type_as_literal v))) thing
  | Record name         -> sprintf "%s.ToProxy()" thing
  | _                   -> assert false


and proxy_msg_name classname msg =
  sprintf "%s_%s" (String.lowercase_ascii classname) (String.lowercase_ascii msg.msg_name)


and exposed_class_name classname =
  String.capitalize_ascii classname

and escaped = function
  | "params" -> "paramz"
  | "ref" -> "reff"
  | "public" -> "pubblic"
  | s -> s

and full_name field =
  escaped (String.concat "_" field.full_name)

and is_readonly field =
  match field.qualifier with
    RW   -> "false"
  | _    -> "true"


and is_static_readonly field =
  match field.qualifier with
    StaticRO     -> "true"
  | DynamicRO    -> "false"
  | _            -> "false"

and json_param p =
  let thing = String.lowercase_ascii p.param_name in
  match p.param_type with
  | Int
  | Float
  | Bool
  | DateTime       -> sprintf "_%s" thing
  | String
  | Ref _          -> sprintf "_%s ?? \"\"" thing
  | Enum _         -> sprintf "_%s.StringOf()" thing
  | Set (Ref name) -> sprintf "_%s == null ? new JArray() : JArray.FromObject(_%s, serializer)" thing thing
  | Set u          -> sprintf "_%s == null ? new JArray() : JArray.FromObject(_%s)" thing thing
  | Map (u, v)     -> sprintf "_%s == null ? new JObject() : JObject.FromObject(_%s, serializer)" thing thing
  | Record name    -> sprintf "_%s.ToJObject()" thing

and json_deserialise_opt = function
  | Some (typ, _) -> sprintf "<%s>" (exposed_type typ)
  | None -> ""

and json_converter = function
  | DateTime              -> "new XenDateTimeConverter()"
  | Enum (name, _)        -> sprintf "new %sConverter()" name
  | Ref name              -> sprintf "new XenRefConverter<%s>()" (exposed_class_name name)
  | Set (Ref name)        -> sprintf "new XenRefListConverter<%s>()" (exposed_class_name name)
  | Map (Ref u, Record v) -> sprintf "new XenRefXenObjectMapConverter<%s>()" (exposed_class_name u)
  | Map (Ref u, Ref v)    -> sprintf "new XenRefXenRefMapConverter<%s, %s>()" (exposed_class_name u) (exposed_class_name v)
  | Map (Ref u, String)   -> sprintf "new XenRefStringMapConverter<%s>()" (exposed_class_name u)
  | Map (Ref u, Set(String)) -> sprintf "new XenRefStringSetMapConverter<%s>()" (exposed_class_name u)
  | Map (String, Ref v)   -> sprintf "new StringXenRefMapConverter<%s>()" (exposed_class_name v)
  | _                     -> ""

and json_converter_opt = function
  | Some (typ, _) -> json_converter typ
  | None -> ""

and json_return_opt thing = function
  | Some (typ, _) -> "return " ^ thing
  | None -> thing

and json_serialization_attr = function
  | DateTime              -> sprintf "\n        [JsonConverter(typeof(XenDateTimeConverter))]"
  | Enum (name, _)        -> sprintf "\n        [JsonConverter(typeof(%sConverter))]" name
  | Ref name              -> sprintf "\n        [JsonConverter(typeof(XenRefConverter<%s>))]" (exposed_class_name name)
  | Set (Ref name)        -> sprintf "\n        [JsonConverter(typeof(XenRefListConverter<%s>))]" (exposed_class_name name)
  | Map (Ref u, Record v) -> sprintf "\n        [JsonConverter(typeof(XenRefObjectMapConverter<%s>))]" (exposed_class_name u)
  | Map (Ref u, Ref v)    -> sprintf "\n        [JsonConverter(typeof(XenRefXenRefMapConverter<%s, %s>))]" (exposed_class_name u) (exposed_class_name v)
  | Map (Ref u, String)   -> sprintf "\n        [JsonConverter(typeof(XenRefStringMapConverter<%s>))]" (exposed_class_name u)
  | Map (String, Ref v)   -> sprintf "\n        [JsonConverter(typeof(StringXenRefMapConverter<%s>))]" (exposed_class_name v)
  | _                     -> ""

and get_default_value_opt field =
  let rec get_default_value = function
    | VString y -> ["\"" ^ y ^ "\""]
    | VInt y -> [Int64.to_string y]
    | VFloat y -> [sprintf "%.3f" y]
    | VBool y -> [string_of_bool y]
    | VDateTime y -> [Printf.sprintf "DateTime.ParseExact(\"%s\", \"yyyyMMddTHH:mm:ssZ\", CultureInfo.InvariantCulture)" (Date.to_string y)]
    | VEnum y -> [enum_of_wire y]
    | VMap y -> List.map (fun (a, b) -> sprintf "{%s, %s}" (String.concat ", " (get_default_value a)) (String.concat ", " (get_default_value b))) y
    | VSet y -> List.map (fun x -> String.concat ", " (get_default_value x)) y
    | VRef y -> if y = "" then ["Helper.NullOpaqueRef"] else [sprintf "\"%s\"" y]
    | VCustom (_,y) -> get_default_value y
  in
  match field.default_value with
  | Some y -> get_default_value_per_type field.ty (get_default_value y)
  | None -> get_default_value_per_type field.ty []

and get_default_value_per_type ty thing =
  match ty with
  | DateTime
  | Int
  | Bool
  | Float          -> if thing = [] then "" else sprintf " = %s" (String.concat ", " thing)
  | Ref _          -> sprintf " = new %s(%s)" (exposed_type ty) (if thing = [] then "Helper.NullOpaqueRef" else (String.concat ", " thing))
  | String         -> sprintf " = %s" (if thing = [] then "\"\"" else (String.concat ", " thing))
  | Enum (name,_)  -> if thing = [] then "" else sprintf " = %s.%s" name (String.concat ", " thing)
  | Set(Int)
  | Set(String)    -> sprintf " = {%s}" (String.concat ", " thing)
  | Set (Ref name) -> sprintf " = new %s() {%s}" (exposed_type ty)
                        (if thing = [] then "" else String.concat ", " (List.map (fun x-> sprintf "new XenRef<%s>(%s)" (exposed_class_name name) x) thing))
  | Set _          -> sprintf " = new %s() {%s}" (exposed_type ty) (String.concat ", " thing)
  | Map(u, v)      -> sprintf " = new Dictionary<%s, %s>() {%s}" (exposed_type u) (exposed_type v) (String.concat ", " thing)
  | Record name    -> sprintf " = new %s()" (exposed_type ty)

and gen_i18n_errors () =
  Friendly_error_names.parse_sr_xml sr_xml;
  Friendly_error_names.parse_resx resx_file;
  let errors = Friendly_error_names.friendly_names_all Datamodel.errors in
  let json = `O [
      "i18n_errors", `A (List.map (fun (x, y) ->
          `O [
            "i18n_error_key", `String x;
            "i18n_error_description", `String y;
          ];) errors);
    ]
  in
  render_file ("FriendlyErrorNames.mustache", "FriendlyErrorNames.resx") json templdir destdir

let populate_releases ()=
  render_file ("ApiVersion.mustache", "ApiVersion.cs") json_releases templdir destdir

let _ =
  main();
  gen_i18n_errors();
  populate_releases()
