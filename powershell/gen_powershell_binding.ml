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

open Stdext
open Pervasiveext
open Printf
open Xstringext
open Datamodel
open Datamodel_types
open Datamodel_utils
open Dm_api
open Common_functions
open CommonFunctions

module DT = Datamodel_types
module DU = Datamodel_utils

module TypeSet = Set.Make(
  struct
    type t = DT.ty
    let compare = compare
  end)

let destdir' = ref ""

let _ =
  Arg.parse
    [
      "-d", Arg.Set_string destdir', "specifies the destination directory for the generated files";
    ]
    (fun x -> raise (Arg.Bad ("Found anonymous argument " ^ x)))
    ("Generates PowerShell bindings for the XenAPI. See -help.")

let destdir = !destdir'

let api =
  Datamodel_utils.named_self := true;
  let obj_filter _ = true in
  let field_filter field =
    (not field.internal_only) && (List.mem "closed" field.release.internal)
  in
  let message_filter msg =
    Datamodel_utils.on_client_side msg &&
    (not msg.msg_hide_from_docs) &&
    (not (List.mem msg.msg_name ["get_by_name_label"; "get_by_uuid";
                                 "get"; "get_all" ; "get_all_records";
                                 "get_all_records_where"; "get_record";])) &&
    (msg.msg_tag <> (FromObject GetAllRecords)) &&
    (List.mem "closed" msg.msg_release.internal)
  in
  filter obj_filter field_filter message_filter
    (Datamodel_utils.add_implicit_messages ~document_order:false
       (filter obj_filter field_filter message_filter Datamodel.all_api))

let classes_with_records =
  Datamodel_utils.add_implicit_messages ~document_order:false Datamodel.all_api
  |> objects_of_api
  |> List.filter (fun x -> List.exists (fun y-> y.msg_name = "get_all_records") x.messages)
  |> List.map (fun x-> x.name)

let classes = objects_of_api api
let maps = ref TypeSet.empty

let generated x =
  not (List.mem x.name ["blob"; "session"; "debug"; "event"; "vtpm"])

let rec main() =
  List.iter (fun x -> if generated x then gen_binding x) classes;
  gen_xenref_converters classes;
  List.iter gen_http_action
    (List.filter (fun (_, (_, _, sdk, _, _, _)) -> sdk) http_actions)

(****************)
(* Http actions *)
(****************)

and gen_http_action action =
  match action with (name, (meth, uri, _, args, _, _)) ->
    let commonVerb = get_http_action_verb name meth in
    let verbCategory = get_common_verb_category commonVerb in
    let stem = get_http_action_stem name in
    let content = sprintf "%s

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using System.Text;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(%s.%s, \"Xen%s\"%s)]
    [OutputType(typeof(void))]
    public class %sXen%sCommand : XenServerHttpCmdlet
    {
        #region Cmdlet Parameters
%s%s
        #endregion

        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();
%s
            RunApiCall(() => %s);
        }

        #endregion
    }
}\n" Licence.bsd_two_clause
        verbCategory commonVerb stem (gen_should_process_http_decl meth)
        commonVerb stem
        (gen_progress_tracker meth)
        (gen_arg_params args)
        (gen_should_process_http meth uri)
        (gen_http_action_call action)
    in
    write_file (sprintf "%s-Xen%s.cs" commonVerb stem) content

and gen_should_process_http_decl meth =
  match meth with
  | Put -> ", SupportsShouldProcess = true"
  | Get   -> ", SupportsShouldProcess = false"
  | _   -> assert false

and gen_should_process_http meth uri =
  match meth with
  | Put -> sprintf "
            if (!ShouldProcess(\"%s\"))
                return;\n" uri
  | _   -> ""

and gen_progress_tracker meth =
  match meth with
  | Get -> "
        [Parameter]
        public HTTP.DataCopiedDelegate DataCopiedDelegate { get; set; }\n"
  | Put -> "
        [Parameter]
        public HTTP.UpdateProgressDelegate ProgressDelegate { get; set; }\n"
  | _   -> assert false

and gen_arg_params args =
  match args with
  | []     -> ""
  | hd::tl -> sprintf "%s%s" (gen_arg_param hd) (gen_arg_params tl)

and gen_arg_param = function
  | String_query_arg x -> sprintf "
        [Parameter%s]
        public string %s { get; set; }\n"
                            (if (String.lowercase_ascii x) = "uuid" then
                               "(ValueFromPipelineByPropertyName = true)"
                             else "")
                            (pascal_case_ x)
  | Int64_query_arg x -> sprintf "
        [Parameter]
        public long %s { get; set; }\n" (pascal_case_ x)
  | Bool_query_arg x ->
    let y = if x = "host" then "is_host" else x in
    sprintf "
        [Parameter]
        public bool %s { get; set; }\n" (pascal_case_ y)
  | Varargs_query_arg -> sprintf "
        ///<summary>
        /// Alternate names & values
        ///</summary>
        [Parameter]
        public string[] Args { get; set; }\n"

and gen_http_action_call (name, (meth, _, _, args, _, _)) =
  let progressTracker = match meth with
    | Get -> "DataCopiedDelegate"
    | Put -> "ProgressDelegate"
    | _   -> assert false in
  sprintf "XenAPI.HTTP_actions.%s(%s,
                CancellingDelegate, TimeoutMs, XenHost, Proxy, Path, TaskRef,
                session.opaque_ref%s)"
    name progressTracker (gen_call_arg_params args)

and gen_call_arg_params args =
  match args with
  | []     -> ""
  | hd::tl -> sprintf "%s%s" (gen_call_arg_param hd) (gen_call_arg_params tl)

and gen_call_arg_param = function
  | String_query_arg x -> sprintf ", %s" (pascal_case_ x)
  | Int64_query_arg x -> sprintf ", %s" (pascal_case_ x)
  | Bool_query_arg x ->
    let y = if x = "host" then "is_host" else x in
    sprintf ", %s" (pascal_case_ y)
  | Varargs_query_arg -> sprintf ", Args"


(***********************************)
(* Utility cmdlet ConvertTo-XenRef *)
(***********************************)

and gen_xenref_converters classes =
  write_file "ConvertTo-XenRef.cs" (gen_body_xenref_converters classes)

and gen_body_xenref_converters classes =
  sprintf "%s

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using System.Text;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsData.ConvertTo, \"XenRef\")]
    [OutputType(typeof(IXenObject))]
    public class ConvertToXenRefCommand : PSCmdlet
    {
        #region Cmdlet Parameters

        [Parameter(Mandatory = true, ValueFromPipeline = true, Position = 0)]
        public IXenObject XenObject { get; set; }

        #endregion

        #region Cmdlet Methods

        protected override void ProcessRecord()
        {%s
        }

        #endregion

    }
}\n" Licence.bsd_two_clause (print_converters classes)

and print_converters classes =
  match classes with
  | []       -> ""
  | hd::tl -> sprintf "
            %s %s = XenObject as %s;
            if (%s != null)
            {
                WriteObject(new XenRef<%s>(%s));
                return;
            }%s"
                (qualified_class_name hd.name)
                (ocaml_class_to_csharp_local_var hd.name)
                (qualified_class_name hd.name)
                (ocaml_class_to_csharp_local_var hd.name)
                (qualified_class_name hd.name)
                (ocaml_class_to_csharp_local_var hd.name)
                (print_converters tl)

(*************************)
(* Autogenerated cmdlets *)
(*************************)

and gen_binding obj =
  match obj with
    {name=classname; description; messages; contents; gen_constructor_destructor=gen_const_dest} ->
    let partNew = List.partition (fun x -> is_constructor x) messages in
    let constructors = fst partNew in
    let partDest = List.partition (fun x -> is_destructor x) (snd partNew) in
    let destructors = fst partDest in
    let partRem = List.partition (fun x -> is_remover x) (snd partDest) in
    let removers = fst partRem in
    let partAdd = List.partition (fun x -> is_adder x) (snd partRem) in
    let adders = fst partAdd in
    let partSet = List.partition (fun x -> is_setter x) (snd partAdd) in
    let setters = fst partSet in
    let partGet = List.partition (fun x -> is_getter x) (snd partSet) in
    let getters = fst partGet in
    let invokers = List.filter (fun x -> is_invoke x) (snd partGet) in
    let stem = ocaml_class_to_csharp_class classname in
    begin
      if List.mem classname classes_with_records then
        write_file (sprintf "Get-Xen%s.cs" stem) (gen_class obj classname);
      if ((List.length constructors) > 0) then
        write_file (sprintf "New-Xen%s.cs" stem) (gen_constructor obj classname constructors);
      if ((List.length destructors) > 0) then
        write_file (sprintf "Remove-Xen%s.cs" stem) (gen_destructor obj classname destructors);
      if ((List.length removers) > 0) then
        write_file (sprintf "Remove-Xen%sProperty.cs" stem) (gen_remover obj classname removers);
      if ((List.length adders) > 0) then
        write_file (sprintf "Add-Xen%s.cs" stem) (gen_adder obj classname adders);
      if ((List.length setters) > 0) then
        write_file (sprintf "Set-Xen%s.cs" stem) (gen_setter obj classname setters);
      if ((List.length getters) > 0) then
        write_file (sprintf "Get-Xen%sProperty.cs" stem) (gen_getter obj classname getters);
      if ((List.length invokers) > 0) then
        write_file (sprintf "Invoke-Xen%s.cs" stem) (gen_invoker obj classname invokers);
    end

and write_file filename format =
  let out_chan = open_out (Filename.concat destdir filename) in
  finally
    (fun () -> fprintf out_chan "%s" format)
    (fun () -> close_out out_chan)

(*********************************)
(* Print function for Get-XenFoo *)
(*********************************)

and gen_class obj classname =
  (print_header_class classname)^
  (print_parameters_class obj classname)^
  (print_methods_class classname (has_uuid obj) (has_name obj))

and print_header_class classname =
  sprintf "%s

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using System.Text;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsCommon.Get, \"Xen%s\", DefaultParameterSetName = \"Ref\", SupportsShouldProcess = false)]
    [OutputType(typeof(%s[]))]
    public class GetXen%sCommand : XenServerCmdlet
    {\n"
    Licence.bsd_two_clause
    (ocaml_class_to_csharp_class classname)
    (qualified_class_name classname)
    (ocaml_class_to_csharp_class classname)

and print_parameters_class obj classname =
  sprintf
    "        #region Cmdlet Parameters
%s
        #endregion\n"
    (print_xenobject_params obj classname false false true)

and print_methods_class classname has_uuid has_name =
  let classType = (qualified_class_name classname) in
  sprintf "
        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();

            var records = %s.get_all_records(session);

            foreach (var record in records)
                record.Value.opaque_ref = record.Key;

            var results = new List<%s>();

            if (Ref != null)
            {
                foreach (var record in records)
                    if (Ref.opaque_ref == record.Key.opaque_ref)
                    {
                        results.Add(record.Value);
                        break;
                    }
            }%s%s
            else
            {
                results.AddRange(records.Values);
            }

            WriteObject(results, true);

            UpdateSessions();
        }

        #endregion
    }
}\n"
    classType
    classType
    (if has_name then sprintf "
            else if (Name != null)
            {
                var options = WildcardOptions.IgnoreCase
                              | WildcardOptions.Compiled
                              | WildcardOptions.CultureInvariant;
                var wildcard = new WildcardPattern(Name, options);

                foreach (var record in records)
                {
                    if (wildcard.IsMatch(record.Value.name_label))
                        results.Add(record.Value);
                }
            }"
     else "")
    (if has_uuid then sprintf "
            else if (Uuid != Guid.Empty)
            {
                foreach (var record in records)
                    if (Uuid.ToString() == record.Value.uuid)
                    {
                        results.Add(record.Value);
                        break;
                    }
            }"
     else "")


(*********************************)
(* Print function for New-XenFoo *)
(*********************************)

and gen_constructor obj classname messages =
  match messages with
  | []  -> ""
  | [x] -> (print_header_constructor x obj classname)^
           (print_params_constructor x obj classname)^
           (print_methods_constructor x obj classname)
  | _   -> assert false

and print_header_constructor message obj classname =
  sprintf "%s

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using System.Text;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsCommon.New, \"Xen%s\", DefaultParameterSetName = \"Hashtable\", SupportsShouldProcess = true)]
    [OutputType(typeof(%s))]%s
    [OutputType(typeof(void))]
    public class NewXen%sCommand : XenServerCmdlet
    {"
    Licence.bsd_two_clause
    (ocaml_class_to_csharp_class classname)
    (qualified_class_name classname)
    (if message.msg_async then "
    [OutputType(typeof(XenAPI.Task))]"
     else "")
    (ocaml_class_to_csharp_class classname)

and print_params_constructor message obj classname =
  sprintf "
        #region Cmdlet Parameters

        [Parameter]
        public SwitchParameter PassThru { get; set; }

        [Parameter(ParameterSetName = \"Hashtable\", Mandatory = true)]
        public Hashtable HashTable { get; set; }

        [Parameter(ParameterSetName = \"Record\", Mandatory = true)]
        public %s Record { get; set; }
%s%s
        #endregion\n"
    (qualified_class_name classname)
    (if is_real_constructor message then gen_fields (DU.fields_of_obj obj)
     else gen_constructor_params message.msg_params)
    (if message.msg_async then
       "
        protected override bool GenerateAsyncParam
        {
            get { return true; }
        }\n"
     else "")

and gen_constructor_params params =
  match params with
  |  []     -> ""
  |  hd::tl -> sprintf"%s\n%s"
                 (gen_constructor_param hd.param_name hd.param_type ["Fields"])
                 (gen_constructor_params tl)

and gen_fields fields =
  match fields with
  |  []     -> ""
  |  hd::tl -> match hd.qualifier with
    |  DynamicRO -> (gen_fields tl)
    |  _         -> sprintf "%s\n%s"
                      (gen_constructor_param (full_name hd) hd.ty ["Fields"])
                      (gen_fields tl)

and gen_constructor_param paramName paramType paramsets =
  let publicName = ocaml_class_to_csharp_property paramName in
  (*Do not add a Record parameter; it has already been added manually as all constructors need one*)
  if paramName = "record" then ""
  else sprintf "
        %s
        public %s %s { get; set; }" (print_parameter_sets paramsets)
      (obj_internal_type paramType) publicName

and print_methods_constructor message obj classname =
  sprintf "
        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();%s%s
            RunApiCall(()=>
            {%s
            });

            UpdateSessions();
        }

        #endregion
   }
}\n"
    (if is_real_constructor message then gen_make_record message obj classname
     else gen_make_fields message obj classname)
    (gen_shouldprocess "New" message classname)
    (gen_csharp_api_call message classname "New" "passthru")

and create_param_parse param paramName =
  match param.param_type with
  | Ref name -> sprintf "
            string %s = %s.opaque_ref;\n"
                  (String.lowercase_ascii param.param_name) paramName
  | _ -> ""

and gen_make_record message obj classname =
  sprintf "
            if (Record == null && HashTable == null)
            {
                Record = new %s();%s
            }
            else if (Record == null)
            {
                Record = new %s(HashTable);
            }\n"
    (qualified_class_name classname)
    (gen_record_fields (DU.fields_of_obj obj))
    (qualified_class_name classname)

and gen_record_fields fields =
  match fields with
  | [] -> ""
  | h::tl -> match h.qualifier with
    |  DynamicRO -> (gen_record_fields tl)
    |  _ -> sprintf "
                %s%s" (gen_record_field h) (gen_record_fields tl)

and gen_record_field field =
  match field.ty with
  |  Ref r     -> sprintf "Record.%s = %s == null ? null : new %s(%s.opaque_ref);"
                    (full_name field) (ocaml_field_to_csharp_property field)
                    (obj_internal_type field.ty) (ocaml_field_to_csharp_property field)
  |  Map(u, v) -> sprintf "Record.%s = CommonCmdletFunctions.ConvertHashTableToDictionary<%s, %s>(%s);"
                    (full_name field) (exposed_type u) (exposed_type v)
                    (pascal_case (full_name field))
  |  _         -> sprintf "Record.%s = %s;"
                    (full_name field) (ocaml_field_to_csharp_property field)

and gen_make_fields message obj classname =
  sprintf "
            if (Record != null)
            {%s
            }
            else if (HashTable != null)
            {%s
            }"
    (explode_record_fields message (DU.fields_of_obj obj))
    (explode_hashtable_fields message (DU.fields_of_obj obj))

and explode_record_fields message fields =
  let print_map tl hd =
    sprintf "
                %s = CommonCmdletFunctions.ConvertDictionaryToHashtable(Record.%s);%s"
      (ocaml_class_to_csharp_property (full_name hd))
      (full_name hd)
      (explode_record_fields message tl) in
  let print_record tl hd =
    sprintf "
                %s = Record.%s;%s"
      (ocaml_class_to_csharp_property (full_name hd))
      (full_name hd)
      (explode_record_fields message tl) in
  match fields with
  |  []     -> ""
  |  hd::tl ->
    if List.exists (fun x -> (full_name hd) = x.param_name) message.msg_params then
      match hd.ty with
      | Map(u, v) -> print_map tl hd
      | _ -> print_record tl hd
    else explode_record_fields message tl

and explode_hashtable_fields message fields =
  match fields with
  |  []     -> ""
  |  hd::tl ->
    if List.exists (fun x -> (full_name hd) = x.param_name) message.msg_params then
      sprintf "
                %s = %s;%s"
        (ocaml_class_to_csharp_property (full_name hd))
        (convert_from_hashtable (full_name hd) hd.ty)
        (explode_hashtable_fields message tl)
    else
      explode_hashtable_fields message tl

and convert_from_hashtable fname ty =
  let field = sprintf "\"%s\"" fname in
  match ty with
  | DateTime             -> sprintf "Marshalling.ParseDateTime(HashTable, %s)" field
  | Bool                 -> sprintf "Marshalling.ParseBool(HashTable, %s)" field
  | Float                -> sprintf "Marshalling.ParseDouble(HashTable, %s)" field
  | Int                  -> sprintf "Marshalling.ParseLong(HashTable, %s)" field
  | Ref name             -> sprintf "Marshalling.ParseRef<%s>(HashTable, %s)"
                              (exposed_class_name name) field
  | String               -> sprintf "Marshalling.ParseString(HashTable, %s)" field
  | Set(String)          -> sprintf "Marshalling.ParseStringArray(HashTable, %s)" field
  | Set(Ref x)           -> sprintf "Marshalling.ParseSetRef<%s>(HashTable, %s)"
                              (exposed_class_name x) field
  | Set(Enum(x, _))      -> sprintf "Helper.StringArrayToEnumList<%s>(Marshalling.ParseStringArray(HashTable, %s))" x field
  | Enum(x, _)           -> sprintf "(%s)CommonCmdletFunctions.EnumParseDefault(typeof(%s), Marshalling.ParseString(HashTable, %s))"
                              x x field
  | Map(Ref x, Record _) -> sprintf "Marshalling.ParseMapRefRecord<%s, Proxy_%s>(HashTable, %s)"
                              (exposed_class_name x) (exposed_class_name x) field
  | Map(u, v) as x       -> maps := TypeSet.add x !maps;
    sprintf "(Marshalling.ParseHashTable(HashTable, %s))" field
  | Record name          -> sprintf "new %s((Proxy_%s)HashTable[%s])"
                              (exposed_class_name name)
                              (exposed_class_name name) field
  | Set(Record name)     -> sprintf "Helper.Proxy_%sArrayTo%sList(Marshalling.ParseStringArray(%s))"
                              (exposed_class_name name)
                              (exposed_class_name name) field
  | _                   -> assert false



(************************************)
(* Print function for Remove-XenFoo *)
(************************************)

and gen_destructor obj classname messages =
  let cut_message_name = fun x-> cut_msg_name (pascal_case x.msg_name) "Remove" in
  let asyncMessages = List.map cut_message_name (List.filter (fun x -> x.msg_async) messages) in
  sprintf
    "%s

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using System.Text;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsCommon.Remove, \"Xen%s\", SupportsShouldProcess = true)]
    [OutputType(typeof(%s))]%s
    [OutputType(typeof(void))]
    public class RemoveXen%s : XenServerCmdlet
    {
        #region Cmdlet Parameters

        [Parameter]
        public SwitchParameter PassThru { get; set; }
%s%s
        #endregion

        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();

            string %s = Parse%s();

            %s

            UpdateSessions();
        }

        #endregion

        #region Private Methods
%s%s
        #endregion
    }
}\n"
    Licence.bsd_two_clause
    (ocaml_class_to_csharp_class classname)
    (qualified_class_name classname)
    (if (List.length asyncMessages) > 0 then "
    [OutputType(typeof(XenAPI.Task))]" else "")
    (ocaml_class_to_csharp_class classname)
    (print_xenobject_params obj classname true true true)
    (if (List.length asyncMessages) > 0 then sprintf "
        protected override bool GenerateAsyncParam
        {
            get { return true; }
        }\n" else "")
    (ocaml_class_to_csharp_local_var classname) (ocaml_class_to_csharp_property classname)
    (print_cmdlet_methods_remover classname messages)
    (print_parse_xenobject_private_method obj classname true)
    (print_process_record_private_methods classname messages "Remove" "asyncpassthru")

and print_cmdlet_methods_remover classname messages =
  let localVar = (ocaml_class_to_csharp_local_var classname) in
  let cut_message_name x = cut_msg_name (pascal_case x.msg_name) "Remove" in
  match messages with
  | [x] -> sprintf "ProcessRecord%s(%s);" (cut_message_name x) localVar
  | _   -> assert false


(*****************************************)
(* Print function for Remove-XenFoo -Bar *)
(*****************************************)

and gen_remover obj classname messages =
  let cut_message_name = fun x-> cut_msg_name (pascal_case x.msg_name) "Remove" in
  let asyncMessages = List.map cut_message_name (List.filter (fun x -> x.msg_async) messages) in
  sprintf
    "%s

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using System.Text;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsCommon.Remove, \"Xen%sProperty\", SupportsShouldProcess = true)]
    [OutputType(typeof(%s))]%s
    public class RemoveXen%sProperty : XenServerCmdlet
    {
        #region Cmdlet Parameters

        [Parameter]
        public SwitchParameter PassThru { get; set; }
%s%s%s
        #endregion

        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();

            string %s = Parse%s();

            %s

            %s

            UpdateSessions();
        }

        #endregion

        #region Private Methods
%s%s
        #endregion
    }
}\n"
    Licence.bsd_two_clause
    (ocaml_class_to_csharp_class classname)
    (qualified_class_name classname)
    (if (List.length asyncMessages) > 0 then "
    [OutputType(typeof(XenAPI.Task))]"
     else "")
    (ocaml_class_to_csharp_class classname)
    (print_xenobject_params obj classname true true true)
    (print_async_param asyncMessages)
    (gen_message_as_param classname "Remove" messages)
    (ocaml_class_to_csharp_local_var classname) (ocaml_class_to_csharp_property classname)
    (print_cmdlet_methods classname messages "Remove")
    (gen_passthru classname)
    (print_parse_xenobject_private_method obj classname true)
    (print_process_record_private_methods classname messages "Remove" "")


(**************************************)
(* Print function for Set-XenFoo -Bar *)
(**************************************)

and gen_setter obj classname messages =
  let cut_message_name = fun x-> cut_msg_name (pascal_case x.msg_name) "Set" in
  let asyncMessages = List.map cut_message_name (List.filter (fun x -> x.msg_async) messages) in
  sprintf
    "%s

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using System.Text;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsCommon.Set, \"Xen%s\", SupportsShouldProcess = true)]
    [OutputType(typeof(%s))]%s
    [OutputType(typeof(void))]
    public class SetXen%s : XenServerCmdlet
    {
        #region Cmdlet Parameters

        [Parameter]
        public SwitchParameter PassThru { get; set; }
%s%s%s
        #endregion

        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();

            string %s = Parse%s();

            %s

            %s

            UpdateSessions();
        }

        #endregion

        #region Private Methods
%s%s
        #endregion
    }
}\n"
    Licence.bsd_two_clause
    (ocaml_class_to_csharp_class classname)
    (qualified_class_name classname)
    (if (List.length asyncMessages) > 0 then "
    [OutputType(typeof(XenAPI.Task))]"
     else "")
    (ocaml_class_to_csharp_class classname)
    (print_xenobject_params obj classname true true true)
    (print_async_param asyncMessages)
    (gen_message_as_param classname "Set" messages)
    (ocaml_class_to_csharp_local_var classname) (ocaml_class_to_csharp_property classname)
    (print_cmdlet_methods classname messages "Set")
    (gen_passthru classname)
    (print_parse_xenobject_private_method obj classname true)
    (print_process_record_private_methods classname messages "Set" "")


(**************************************)
(* Print function for Add-XenFoo -Bar *)
(**************************************)

and gen_adder obj classname messages =
  let cut_message_name = fun x-> cut_msg_name (pascal_case x.msg_name) "Add" in
  let asyncMessages = List.map cut_message_name (List.filter (fun x -> x.msg_async) messages) in
  sprintf
    "%s

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using System.Text;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsCommon.Add, \"Xen%s\", SupportsShouldProcess = true)]
    [OutputType(typeof(%s))]%s
    [OutputType(typeof(void))]
    public class AddXen%s : XenServerCmdlet
    {
        #region Cmdlet Parameters

        [Parameter]
        public SwitchParameter PassThru { get; set; }
%s%s%s
        #endregion

        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();

            string %s = Parse%s();

            %s

            %s

            UpdateSessions();
        }

        #endregion

        #region Private Methods
%s%s
        #endregion
    }
}\n"
    Licence.bsd_two_clause
    (ocaml_class_to_csharp_class classname)
    (qualified_class_name classname)
    (if (List.length asyncMessages) > 0 then "
    [OutputType(typeof(XenAPI.Task))]"
     else "")
    (ocaml_class_to_csharp_class classname)
    (print_xenobject_params obj classname true true true)
    (print_async_param asyncMessages)
    (gen_message_as_param classname "Add" messages)
    (ocaml_class_to_csharp_local_var classname) (ocaml_class_to_csharp_property classname)
    (print_cmdlet_methods classname messages "Add")
    (gen_passthru classname)
    (print_parse_xenobject_private_method obj classname true)
    (print_process_record_private_methods classname messages "Add" "")


(*****************************************)
(* Print function for Invoke-XenFoo -Bar *)
(*****************************************)

and gen_invoker obj classname messages =
  let messagesWithParams = List.filter (is_message_with_dynamic_params classname) messages in
  sprintf
    "%s

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using System.Text;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsLifecycle.Invoke, \"Xen%s\", SupportsShouldProcess = true)]
    public class InvokeXen%s : XenServerCmdlet
    {
        #region Cmdlet Parameters

        [Parameter]
        public SwitchParameter PassThru { get; set; }
%s
        [Parameter(Mandatory = true)]
        public Xen%sAction XenAction { get; set; }

        #endregion
%s
        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();

            string %s = Parse%s();

            switch (XenAction)
            {%s
            }

            UpdateSessions();
        }

        #endregion

        #region Private Methods
%s%s
        #endregion
    }

    public enum Xen%sAction
    {%s
    }
%s
}\n"
    Licence.bsd_two_clause
    (ocaml_class_to_csharp_class classname)
    (ocaml_class_to_csharp_class classname)
    (print_xenobject_params obj classname true true true)
    (ocaml_class_to_csharp_class classname)
    (print_dynamic_generator classname "Action" "Invoke" messagesWithParams)
    (ocaml_class_to_csharp_local_var classname) (ocaml_class_to_csharp_property classname)
    (print_cmdlet_methods_dynamic classname messages "Action" "Invoke")
    (print_parse_xenobject_private_method obj classname true)
    (print_process_record_private_methods classname messages "Invoke" "passthru")
    (ocaml_class_to_csharp_class classname)
    (print_messages_as_enum "Invoke" messages)
    (print_dynamic_params classname "Action" "Invoke" messagesWithParams)


(**********************************************)
(* Print function for Get-XenFooProperty -Bar *)
(**********************************************)

and gen_getter obj classname messages =
  let messagesWithParams = List.filter (is_message_with_dynamic_params classname) messages in
  sprintf
    "%s

using System;
using System.Collections;
using System.Collections.Generic;
using System.Management.Automation;
using System.Text;

using XenAPI;

namespace Citrix.XenServer.Commands
{
    [Cmdlet(VerbsCommon.Get, \"Xen%sProperty\", SupportsShouldProcess = false)]
    public class GetXen%sProperty : XenServerCmdlet
    {
        #region Cmdlet Parameters
%s
        [Parameter(Mandatory = true)]
        public Xen%sProperty XenProperty { get; set; }

        #endregion
%s
        #region Cmdlet Methods

        protected override void ProcessRecord()
        {
            GetSession();

            string %s = Parse%s();

            switch (XenProperty)
            {%s
            }

            UpdateSessions();
        }

        #endregion

        #region Private Methods
%s%s
        #endregion
    }

    public enum Xen%sProperty
    {%s
    }
%s
}\n"
    Licence.bsd_two_clause
    (ocaml_class_to_csharp_class classname)
    (ocaml_class_to_csharp_class classname)
    (print_xenobject_params obj classname true true false)
    (ocaml_class_to_csharp_class classname)
    (print_dynamic_generator classname "Property" "Get" messagesWithParams)
    (ocaml_class_to_csharp_local_var classname) (ocaml_class_to_csharp_property classname)
    (print_cmdlet_methods_dynamic classname messages "Property" "Get")
    (print_parse_xenobject_private_method obj classname false)
    (print_process_record_private_methods classname messages "Get" "pipe")
    (ocaml_class_to_csharp_class classname)
    (print_messages_as_enum "Get" messages)
    (print_dynamic_params classname "Property" "Get" messagesWithParams)

and print_cmdlet_methods_dynamic classname messages enum commonVerb =
  let cut_message_name x = cut_msg_name (pascal_case x.msg_name) commonVerb in
  let localVar = (ocaml_class_to_csharp_local_var classname) in
  match messages with
  | []     -> ""
  | hd::tl -> sprintf "
                case Xen%s%s.%s:
                    ProcessRecord%s(%s);
                    break;%s"
                (ocaml_class_to_csharp_class classname) enum (cut_message_name hd)
                (cut_message_name hd) localVar
                (print_cmdlet_methods_dynamic classname tl enum commonVerb)

and print_async_param_getter classname asyncMessages =
  let properties = List.map
      (fun x -> sprintf "                    case Xen%sProperty.%s:"
          (ocaml_class_to_csharp_class classname) x) asyncMessages in
  match asyncMessages with
  | []          -> ""
  | _           -> sprintf "
        protected override bool GenerateAsyncParam
        {
            get
            {
                switch (XenProperty)
                {
%s
                        return true;
                    default:
                        return false;
                }
            }
        }\n" (String.concat "\n" properties)


(**************************************)
(* Common to more than one generators *)
(**************************************)

and gen_passthru classname =
  sprintf "if (!PassThru)
                return;

            RunApiCall(() =>
                {
                    var contxt = _context as XenServerCmdletDynamicParameters;

                    if (contxt != null && contxt.Async)
                    {
                        XenAPI.Task taskObj = null;
                        if (taskRef != null && taskRef != \"OpaqueRef:NULL\")
                        {
                            taskObj = XenAPI.Task.get_record(session, taskRef.opaque_ref);
                            taskObj.opaque_ref = taskRef.opaque_ref;
                        }

                        WriteObject(taskObj, true);
                    }
                    else
                    {

                        var obj = %s.get_record(session, %s);
                        if (obj != null)
                            obj.opaque_ref = %s;
                        WriteObject(obj, true);

                    }
                });"
    (qualified_class_name classname) (ocaml_class_to_csharp_local_var classname)
    (ocaml_class_to_csharp_local_var classname)

and is_message_with_dynamic_params classname message =
  let nonClassParams = List.filter (fun x -> not (is_class x classname)) message.msg_params in
  if ((List.length nonClassParams) > 0) || message.msg_async then true
  else false

and print_dynamic_generator classname enum commonVerb messagesWithParams =
  match messagesWithParams with
  | [] -> ""
  | _  -> sprintf "
        public override object GetDynamicParameters()
        {
            switch (Xen%s)
            {%s
                default:
                    return null;
            }
        }\n" enum (print_messages_with_params classname enum commonVerb messagesWithParams)

and print_messages_with_params classname enum commonVerb x =
  match x with
  | [] -> ""
  | hd::tl -> sprintf "
                case Xen%s%s.%s:
                    _context = new Xen%s%s%sDynamicParameters();
                    return _context;%s"
                (ocaml_class_to_csharp_class classname) enum
                (cut_msg_name (pascal_case hd.msg_name) commonVerb)
                (ocaml_class_to_csharp_class classname) enum
                (cut_msg_name (pascal_case hd.msg_name) commonVerb)
                (print_messages_with_params classname enum commonVerb tl)

and print_dynamic_params classname enum commonVerb messagesWithParams =
  match messagesWithParams with
  | []      -> ""
  | hd::tl  -> sprintf "
    public class Xen%s%s%sDynamicParameters : IXenServerDynamicParameter
    {%s%s
    }\n%s"
                 (ocaml_class_to_csharp_class classname) enum
                 (cut_msg_name (pascal_case hd.msg_name) commonVerb)
                 (if hd.msg_async then "
        [Parameter]
        public SwitchParameter Async { get; set; }\n"
                  else "")
                 (print_dynamic_param_members classname hd.msg_params commonVerb)
                 (print_dynamic_params classname enum commonVerb tl)

and print_dynamic_param_members classname params commonVerb =
  match params with
  | []     -> ""
  | hd::tl -> if is_class hd classname then
      print_dynamic_param_members classname tl commonVerb
    else (
      let publicProperty =
        (if commonVerb = "Invoke" && (List.mem (String.lowercase_ascii hd.param_name) ["name";"uuid"]) then
           (ocaml_class_to_csharp_property hd.param_name)^"Param"
         else ocaml_class_to_csharp_property hd.param_name) in
      let theType = obj_internal_type hd.param_type in
      sprintf "
        [Parameter]
        public %s %s { get; set; }\n%s "
        theType publicProperty (print_dynamic_param_members classname tl commonVerb))

and print_messages_as_enum commonVerb messages =
  let cut_message_name = fun x-> cut_msg_name (pascal_case x.msg_name) commonVerb in
  match messages with
  | []     -> ""
  | [x]    -> sprintf "\n        %s" (cut_message_name x)
  | hd::tl -> sprintf "\n        %s,%s" (cut_message_name hd) (print_messages_as_enum commonVerb tl)


and gen_message_as_param classname commonVerb messages =
  match messages with
  | []     -> ""
  | hd::tl ->
    let msgType = get_message_type hd classname commonVerb in
    let cutMessageName = cut_msg_name (pascal_case hd.msg_name) commonVerb in
    let msgName = if cutMessageName = "Host" then "XenHost" else cutMessageName in
    sprintf "
        [Parameter]
        public %s %s
        {
            get { return %s; }
            set
            {
                %s = value;
                %sIsSpecified = true;
            }
        }
        private %s %s;
        private bool %sIsSpecified;\n%s"
      msgType
      msgName
      (lower_and_underscore_first msgName)
      (lower_and_underscore_first msgName)
      (lower_and_underscore_first msgName)
      msgType (lower_and_underscore_first msgName)
      (lower_and_underscore_first msgName)
      (gen_message_as_param classname commonVerb tl)

and print_cmdlet_methods classname messages commonVerb =
  let cut_message_name x = cut_msg_name (pascal_case x.msg_name) commonVerb in
  let switch_name x = (if (cut_message_name x) = "Host" then "XenHost" else cut_message_name x) in
  let localVar = (ocaml_class_to_csharp_local_var classname) in
  match messages with
  | []     -> ""
  | [x]    -> sprintf "if (%sIsSpecified)
                ProcessRecord%s(%s);"
                (lower_and_underscore_first (switch_name x))
                (cut_message_name x) localVar
  | hd::tl -> sprintf
                "if (%sIsSpecified)
                ProcessRecord%s(%s);
            %s"
                (lower_and_underscore_first (switch_name hd))
                (cut_message_name hd)
                localVar
                (print_cmdlet_methods classname tl commonVerb)

and print_xenobject_params obj classname mandatoryRef includeXenObject includeUuidAndName =
  let publicName = ocaml_class_to_csharp_property classname in
  let privateName = lower_and_underscore_first publicName in
  sprintf "%s

        [Parameter(ParameterSetName = \"Ref\"%s, ValueFromPipelineByPropertyName = true, Position = 0)]
        [Alias(\"opaque_ref\")]
        public XenRef<%s> Ref { get; set; }
%s%s\n"
    (if includeXenObject then print_param_xen_object (qualified_class_name classname) publicName privateName
     else "")
    (if mandatoryRef then ", Mandatory = true" else "")
    (qualified_class_name classname)
    (print_param_uuid ((has_uuid obj) && includeUuidAndName))
    (print_param_name ((has_name obj) && includeUuidAndName))

and print_param_xen_object qualifiedClassName publicName privateName =
  sprintf "
        [Parameter(ParameterSetName = \"XenObject\", Mandatory = true, ValueFromPipeline = true, Position = 0)]
        public %s %s { get; set; }"
    qualifiedClassName publicName

and print_param_uuid hasUuid =
  if hasUuid then sprintf "
        [Parameter(ParameterSetName = \"Uuid\", Mandatory = true, ValueFromPipelineByPropertyName = true, Position = 0)]
        public Guid Uuid { get; set; }\n"
  else sprintf ""

and print_param_name hasName =
  if hasName then sprintf "
        [Parameter(ParameterSetName = \"Name\", Mandatory = true, ValueFromPipelineByPropertyName = true, Position = 0)]
        [Alias(\"name_label\")]
        public string Name { get; set; }\n"
  else sprintf ""

and gen_switch_params classname message commonVerb =
  let cutMessageName = cut_msg_name (pascal_case message.msg_name) commonVerb in
  let switchName = if cutMessageName = "Host" then "XenHost" else cutMessageName in
  sprintf "
        [Parameter(ParameterSetName = \"%s\", Mandatory = true)]
        public SwitchParameter %s
        {
            get { return %s; }
            set
            {
                %s = value;
                %sIsSpecified = true;
            }
        }
        private bool %s;
        private bool %sIsSpecified;\n"
    switchName
    switchName
    (lower_and_underscore_first switchName)
    (lower_and_underscore_first switchName)
    (lower_and_underscore_first switchName)
    (lower_and_underscore_first switchName)
    (lower_and_underscore_first switchName)

and print_async_param asyncMessages =
  match asyncMessages with
  | []          -> ""
  | _           -> sprintf "
        protected override bool GenerateAsyncParam
        {
            get
            {
                return %s;
            }
        }\n" (condition asyncMessages)

and condition messages =
  match messages with
  | []          -> ""
  | [x]         -> sprintf "%sIsSpecified" (lower_and_underscore_first x)
  | hd::tl      -> sprintf "%sIsSpecified
                       ^ %s" (lower_and_underscore_first hd) (condition tl)

and get_message_type message classname commonVerb =
  let messageParams = List.filter (fun x -> not (is_class x classname)) message.msg_params in
  match commonVerb with
  | "Remove" ->
    (match messageParams with
     | [x] -> obj_internal_type x.param_type
     | _   -> Printf.eprintf "%s" message.msg_name; assert false)
  | "Add"    ->
    (match messageParams with
     | [x]   -> obj_internal_type x.param_type
     | [x;y] -> sprintf "KeyValuePair<%s, %s>"
                  (obj_internal_type x.param_type) (obj_internal_type y.param_type)
     | _     -> Printf.eprintf "%s" message.msg_name; assert false)
  | "Set"    ->
    (match messageParams with
     | [x]    -> obj_internal_type x.param_type
     | [x;y] when not (obj_internal_type x.param_type =
                       obj_internal_type y.param_type) -> sprintf "KeyValuePair<%s, %s>"
                                                            (obj_internal_type x.param_type) (obj_internal_type y.param_type)
     | hd::tl -> let hdtype = obj_internal_type hd.param_type in
       if (List.for_all
             (fun x -> hdtype = (obj_internal_type x.param_type))
             tl) then sprintf "%s[]" hdtype
       else (Printf.eprintf "%s" message.msg_name; assert false)
     | _      -> Printf.eprintf "%s" message.msg_name; assert false)
  | "Get"    ->
    (match messageParams with
     | []  -> "SwitchParameter"
     | [x] -> obj_internal_type x.param_type
     | _   -> Printf.eprintf "%s" message.msg_name; assert false)
  | _        -> ""


and print_parameter_sets parameterSets =
  match parameterSets with
  | []     -> "[Parameter]"
  | [x]    -> sprintf "[Parameter(ParameterSetName = \"%s\")]" x
  | hd::tl -> sprintf "[Parameter(ParameterSetName = \"%s\")]\n        %s"
                hd (print_parameter_sets tl)

and print_parse_xenobject_private_method obj classname includeUuidAndName =
  let publicProperty = ocaml_class_to_csharp_property classname in
  let localVar = ocaml_class_to_csharp_local_var classname in
  sprintf "
        private string Parse%s()
        {
            string %s = null;

            if (%s != null)
                %s = (new XenRef<%s>(%s)).opaque_ref;%s%s
            else if (Ref != null)
                %s = Ref.opaque_ref;
            else
            {
                ThrowTerminatingError(new ErrorRecord(
                    new ArgumentException(\"At least one of the parameters '%s', 'Ref'%s must be set\"),
                    string.Empty,
                    ErrorCategory.InvalidArgument,
                    %s));
            }

            return %s;
        }\n"
    publicProperty
    localVar
    publicProperty
    localVar (qualified_class_name classname) publicProperty
    (if (has_uuid obj) && includeUuidAndName then
       sprintf "
            else if (Uuid != Guid.Empty)
            {
                var xenRef = %s.get_by_uuid(session, Uuid.ToString());
                if (xenRef != null)
                    %s = xenRef.opaque_ref;
            }"
         (qualified_class_name classname)
         localVar
     else sprintf "")
    (if (has_name obj) && includeUuidAndName then
       sprintf "
            else if (Name != null)
            {
                var xenRefs = %s.get_by_name_label(session, Name);
                if (xenRefs.Count == 1)
                    %s = xenRefs[0].opaque_ref;
                else if (xenRefs.Count > 1)
                    ThrowTerminatingError(new ErrorRecord(
                        new ArgumentException(string.Format(\"More than one %s with name label {0} exist\", Name)),
                        string.Empty,
                        ErrorCategory.InvalidArgument,
                        Name));
            }"
         (qualified_class_name classname)
         localVar
         (qualified_class_name classname)
     else sprintf "")
    localVar
    publicProperty
    (if (has_uuid obj) then sprintf ", 'Uuid'"
     else sprintf "")
    publicProperty
    localVar

and print_process_record_private_methods classname messages commonVerb switch =
  match messages with
  | []     -> sprintf ""
  | hd::tl -> let cutMessageName = cut_msg_name (pascal_case hd.msg_name) commonVerb in
    sprintf "
        private void ProcessRecord%s(string %s)
        {%s%s
            RunApiCall(()=>
            {%s
            });
        }\n%s"
      cutMessageName (ocaml_class_to_csharp_local_var classname) ""
      (gen_shouldprocess commonVerb hd classname)
      (gen_csharp_api_call hd classname commonVerb switch)
      (print_process_record_private_methods classname tl commonVerb switch)

and gen_shouldprocess commonVerb message classname =
  match commonVerb with
  | "Get" -> ""
  | _     -> let theObj =
               (if classname = "pool" || commonVerb = "New" then "session.Url"
                else ocaml_class_to_csharp_local_var classname) in
    sprintf "
            if (!ShouldProcess(%s, \"%s.%s\"))
                return;\n" theObj (exposed_class_name classname) message.msg_name

and gen_csharp_api_call message classname commonVerb switch =
  let asyncPipe = gen_csharp_api_call_async_pipe in
  let passThruTask =
    if switch = "pipe" then asyncPipe
    else if (switch = "passthru" || switch = "asyncpassthru") then print_pass_thru asyncPipe
    else "" in
  let syncPipe = gen_csharp_api_call_sync_pipe message classname in
  let passThruResult =
    if switch = "pipe" then syncPipe
    else if switch = "passthru" then print_pass_thru syncPipe
    else "" in
  if message.msg_async then
    sprintf "
                var contxt = _context as %s;

                if (contxt != null && contxt.Async)
                {%s%s
                }
                else
                {%s%s
                }\n"
      (if commonVerb = "Invoke" then sprintf "Xen%sAction%sDynamicParameters"
           (ocaml_class_to_csharp_class classname)
           (cut_msg_name (pascal_case message.msg_name) "Invoke")
       else if commonVerb = "Get" then sprintf "Xen%sProperty%sDynamicParameters"
           (ocaml_class_to_csharp_class classname)
           (cut_msg_name (pascal_case message.msg_name) "Get")
       else "XenServerCmdletDynamicParameters")
      (gen_csharp_api_call_async message classname commonVerb) passThruTask
      (gen_csharp_api_call_sync message classname commonVerb) passThruResult
  else sprintf "%s%s%s"
      (if (commonVerb = "Invoke") && (is_message_with_dynamic_params classname message) then
         sprintf "var contxt = _context as Xen%sAction%sDynamicParameters;\n"
           (ocaml_class_to_csharp_class classname)
           (cut_msg_name (pascal_case message.msg_name) "Invoke")
       else if (commonVerb = "Get") && (is_message_with_dynamic_params classname message) then
         sprintf "var contxt = _context as Xen%sProperty%sDynamicParameters;\n"
           (ocaml_class_to_csharp_class classname)
           (cut_msg_name (pascal_case message.msg_name) "Get")
       else "")
      (gen_csharp_api_call_sync message classname commonVerb) passThruResult

and print_pass_thru x = sprintf "
                    if (PassThru)
                    {%s
                    }" x

and gen_csharp_api_call_async message classname commonVerb =
  sprintf "
                    taskRef = %s.async_%s(%s);\n"
    (qualified_class_name classname)
    (message.msg_name)
    (gen_call_params classname message commonVerb)

and gen_csharp_api_call_async_pipe =
  sprintf "
                        XenAPI.Task taskObj = null;
                        if (taskRef != \"OpaqueRef:NULL\")
                        {
                            taskObj = XenAPI.Task.get_record(session, taskRef.opaque_ref);
                            taskObj.opaque_ref = taskRef.opaque_ref;
                        }

                        WriteObject(taskObj, true);"

and gen_csharp_api_call_sync message classname commonVerb =
  match message.msg_result with
  | None                  -> sprintf "
                    %s.%s(%s);\n"
                               (qualified_class_name classname) (message.msg_name)
                               (gen_call_params classname message commonVerb)
  | Some (Ref r, _)       -> sprintf "
                    string objRef = %s.%s(%s);\n"
                               (qualified_class_name classname) (message.msg_name)
                               (gen_call_params classname message commonVerb)
  | Some (Set (Ref r), _) -> sprintf "
                    var refs = %s.%s(%s);\n"
                               (qualified_class_name classname)
                               (message.msg_name) (gen_call_params classname message commonVerb)
  | Some (Map (u, v), _)  -> sprintf "
                    var dict = %s.%s(%s);\n"
                               (qualified_class_name classname) (message.msg_name)
                               (gen_call_params classname message commonVerb)
  | Some (x, _)           -> sprintf "
                    %s obj = %s.%s(%s);\n"
                               (exposed_type x) (qualified_class_name classname)
                               (message.msg_name)
                               (gen_call_params classname message commonVerb)

and gen_csharp_api_call_sync_pipe message classname =
  match message.msg_result with
  | None                  -> sprintf "
                        var obj = %s.get_record(session, %s);
                        if (obj != null)
                            obj.opaque_ref = %s;
                        WriteObject(obj, true);"
                               (qualified_class_name classname)
                               (ocaml_class_to_csharp_local_var classname)
                               (ocaml_class_to_csharp_local_var classname)
  | Some (Ref r, _)       -> sprintf "
                        %s obj = null;

                        if (objRef != \"OpaqueRef:NULL\")
                        {
                            obj = %s.get_record(session, objRef);
                            obj.opaque_ref = objRef;
                        }

                        WriteObject(obj, true);"
                               (qualified_class_name r) (qualified_class_name r)
  | Some (Set (Ref r), _) -> sprintf "
                        var records = new List<%s>();

                        foreach (var _ref in refs)
                        {
                            if (_ref.opaque_ref == \"OpaqueRef:NULL\")
                                continue;

                            var record = %s.get_record(session, _ref);
                            record.opaque_ref = _ref.opaque_ref;
                            records.Add(record);
                        }

                        WriteObject(records, true);"
                               (qualified_class_name r)
                               (qualified_class_name r)
  | Some (Map (u, v), _)  -> sprintf "
                        Hashtable ht = CommonCmdletFunctions.ConvertDictionaryToHashtable(dict);
                        WriteObject(ht, true);"
  | Some (x, _)           -> sprintf "
                        WriteObject(obj, true);"

and gen_call_params classname message commonVerb =
  (String.concat ", " ("session"::(gen_param_list classname message.msg_params message commonVerb)))

and gen_param_list classname params message commonVerb =
  let cutMessageName = cut_msg_name (ocaml_class_to_csharp_property message.msg_name) commonVerb in
  let get_param_name = fun x ->
    (if is_class x classname then
       ocaml_class_to_csharp_local_var classname
     else if (commonVerb = "Invoke") && (List.mem (String.lowercase_ascii x.param_name) ["name";"uuid"]) then
       sprintf "contxt.%s" (ocaml_class_to_csharp_property x.param_name)^"Param"
     else if commonVerb = "Invoke" then
       sprintf "contxt.%s" (ocaml_class_to_csharp_property x.param_name)
     else if commonVerb = "Get" then
       sprintf "contxt.%s" (ocaml_class_to_csharp_property x.param_name)
     else if not (commonVerb = "New") then
       cutMessageName
     else
       ocaml_class_to_csharp_property x.param_name) in
  let valueOfPair x =
    match x.param_type with
    | Map(u, v) -> sprintf "CommonCmdletFunctions.ConvertHashTableToDictionary<%s, %s>(%s.Value)"
                     (exposed_type u) (exposed_type v) cutMessageName
    | _         -> cutMessageName^".Value" in
  let api_call_param x =
    match x.param_type with
    |  Map(u, v) -> sprintf "CommonCmdletFunctions.ConvertHashTableToDictionary<%s, %s>(%s)"
                      (exposed_type u) (exposed_type v) (get_param_name x)
    | _          -> get_param_name x in
  let messageParams = List.filter (fun x -> not (is_class x classname)) message.msg_params in
  let restParams = List.filter (fun x -> is_class x classname) message.msg_params in
  let procParams = List.map get_param_name restParams in
  match commonVerb with
  | "Remove" ->
    (match messageParams with
     | []  -> procParams
     | [x] -> procParams@[api_call_param x]
     | _   -> Printf.eprintf "%s" message.msg_name; assert false)
  | "Add"    ->
    (match messageParams with
     | [x]   -> procParams@[api_call_param x]
     | [x;y] -> procParams@[cutMessageName^".Key"; valueOfPair y]
     | _     ->  Printf.eprintf "%s" message.msg_name; assert false)
  | "Set"    ->
    (match messageParams with
     | [x]    -> procParams@[api_call_param x]
     | [x;y] when not (obj_internal_type x.param_type =
                       obj_internal_type y.param_type) ->
       procParams@[cutMessageName^".Key"; valueOfPair y]
     | hd::tl -> let argList = ref [] in
       let hdtype = obj_internal_type hd.param_type in
       if (List.for_all (fun x ->
           hdtype = (obj_internal_type x.param_type)) tl) then
         (explode_array cutMessageName (List.length messageParams) argList;
          procParams@(!argList))
       else (Printf.eprintf "%s" message.msg_name; assert false)
     | _      -> Printf.eprintf "%s" message.msg_name; assert false)
  | _      -> (match params with
      |  []    -> []
      |  h::tl -> (api_call_param h)::(gen_param_list classname tl message commonVerb))

and explode_array name length result =
  for i = length -1 downto 0
  do
    result:= (sprintf "%s[%s]" name (string_of_int i))::!result
  done

and is_class param classname =
  (String.lowercase_ascii param.param_name) = "self" ||
  (String.lowercase_ascii param.param_name) = (String.lowercase_ascii classname)

let _ =
  main()
