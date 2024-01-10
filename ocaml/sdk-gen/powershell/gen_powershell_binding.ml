(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

open Printf
open Datamodel
open Datamodel_types
open Dm_api
open Common_functions
open CommonFunctions
module DT = Datamodel_types
module DU = Datamodel_utils

module TypeSet = Set.Make (struct
  type t = DT.ty

  let compare = compare
end)

let destdir = "autogen/src"

type cmdlet = {filename: string; content: string}

let api =
  Datamodel_utils.named_self := true ;
  let obj_filter _ = true in
  let field_filter field =
    (not field.internal_only) && List.mem "closed" field.release.internal
  in
  let message_filter msg =
    Datamodel_utils.on_client_side msg
    && (not msg.msg_hide_from_docs)
    && (not
          (List.mem msg.msg_name
             [
               "get_by_name_label"
             ; "get_by_uuid"
             ; "get"
             ; "get_all"
             ; "get_all_records"
             ; "get_all_records_where"
             ; "get_record"
             ]
          )
       )
    && msg.msg_tag <> FromObject GetAllRecords
    && List.mem "closed" msg.msg_release.internal
  in
  filter obj_filter field_filter message_filter
    (Datamodel_utils.add_implicit_messages ~document_order:false
       (filter obj_filter field_filter message_filter Datamodel.all_api)
    )

let classes_with_records =
  Datamodel_utils.add_implicit_messages ~document_order:false Datamodel.all_api
  |> objects_of_api
  |> List.filter (fun x ->
         List.exists (fun y -> y.msg_name = "get_all_records") x.messages
     )
  |> List.map (fun x -> x.name)

let classes = objects_of_api api

let maps = ref TypeSet.empty

let generated x =
  not (List.mem x.name ["blob"; "session"; "debug"; "event"; "vtpm"])

let rec main () =
  gen_xenref_converters classes ;
  let cmdlets =
    classes |> List.filter generated |> List.map gen_cmdlets |> List.concat
  in
  let http_cmdlets =
    http_actions
    |> List.filter (fun (_, (_, _, sdk, _, _, _)) -> sdk)
    |> List.map gen_http_action
  in
  let all_cmdlets = cmdlets @ http_cmdlets in
  List.iter (fun x -> write_file x.filename x.content) all_cmdlets

(****************)
(* Http actions *)
(****************)
and gen_http_action action =
  let name, (meth, uri, _, args, _, _) = action in
  let commonVerb = get_http_action_verb name meth in
  let verbCategory = get_common_verb_category commonVerb in
  let stem = get_http_action_stem name in
  let content =
    sprintf
      "%s\n\n\
       using System;\n\
       using System.Collections;\n\
       using System.Collections.Generic;\n\
       using System.Management.Automation;\n\
       using XenAPI;\n\n\
       namespace Citrix.XenServer.Commands\n\
       {\n\
      \    [Cmdlet(%s.%s, \"Xen%s\"%s)]\n\
      \    [OutputType(typeof(void))]\n\
      \    public class %sXen%sCommand : XenServerHttpCmdlet\n\
      \    {\n\
      \        #region Cmdlet Parameters\n\
       %s%s\n\
      \        #endregion\n\n\
      \        #region Cmdlet Methods\n\n\
      \        protected override void ProcessRecord()\n\
      \        {\n\
      \            GetSession();\n\
       %s\n\
      \            RunApiCall(() => %s);\n\
      \        }\n\n\
      \        #endregion\n\
      \    }\n\
       }\n"
      Licence.bsd_two_clause verbCategory commonVerb stem
      (gen_should_process_http_decl meth)
      commonVerb stem
      (gen_progress_tracker meth)
      (gen_arg_params args)
      (gen_should_process_http meth uri)
      (gen_http_action_call action)
  in
  {filename= sprintf "%s-Xen%s.cs" commonVerb stem; content}

and gen_should_process_http_decl meth =
  match meth with
  | Put ->
      ", SupportsShouldProcess = true"
  | Get ->
      ", SupportsShouldProcess = false"
  | _ ->
      assert false

and gen_should_process_http meth uri =
  match meth with
  | Put ->
      sprintf
        "\n            if (!ShouldProcess(\"%s\"))\n                return;\n"
        uri
  | _ ->
      ""

and gen_progress_tracker meth =
  match meth with
  | Get ->
      "\n\
      \        [Parameter]\n\
      \        public HTTP.DataCopiedDelegate DataCopiedDelegate { get; set; }\n"
  | Put ->
      "\n\
      \        [Parameter]\n\
      \        public HTTP.UpdateProgressDelegate ProgressDelegate { get; set; }\n"
  | _ ->
      assert false

and gen_arg_params args =
  match args with
  | [] ->
      ""
  | hd :: tl ->
      sprintf "%s%s" (gen_arg_param hd) (gen_arg_params tl)

and gen_arg_param = function
  | String_query_arg x ->
      sprintf
        "\n        [Parameter%s]\n        public string %s { get; set; }\n"
        ( if String.lowercase_ascii x = "uuid" then
            "(ValueFromPipelineByPropertyName = true)"
          else
            ""
        )
        (pascal_case_rec x)
  | Int64_query_arg x ->
      sprintf "\n        [Parameter]\n        public long? %s { get; set; }\n"
        (pascal_case_rec x)
  | Bool_query_arg x ->
      let y = if x = "host" then "is_host" else x in
      sprintf "\n        [Parameter]\n        public bool? %s { get; set; }\n"
        (pascal_case_rec y)
  | Varargs_query_arg ->
      sprintf
        "\n\
        \        ///<summary>\n\
        \        /// Alternate names and values\n\
        \        ///</summary>\n\
        \        [Parameter]\n\
        \        public string[] Args { get; set; }\n"

and gen_http_action_call (name, (meth, _, _, args, _, _)) =
  let progressTracker =
    match meth with
    | Get ->
        "DataCopiedDelegate"
    | Put ->
        "ProgressDelegate"
    | _ ->
        assert false
  in
  sprintf
    "XenAPI.HTTP_actions.%s(%s,\n\
    \                CancellingDelegate, TimeoutMs, XenHost, Proxy, Path, \
     TaskRef,\n\
    \                session.opaque_ref%s)" name progressTracker
    (gen_call_arg_params args)

and gen_call_arg_params args =
  match args with
  | [] ->
      ""
  | hd :: tl ->
      sprintf "%s%s" (gen_call_arg_param hd) (gen_call_arg_params tl)

and gen_call_arg_param = function
  | String_query_arg x ->
      sprintf ", %s" (pascal_case_rec x)
  | Int64_query_arg x ->
      sprintf ", %s" (pascal_case_rec x)
  | Bool_query_arg x ->
      let y = if x = "host" then "is_host" else x in
      sprintf ", %s" (pascal_case_rec y)
  | Varargs_query_arg ->
      sprintf ", Args"

(***********************************)
(* Utility cmdlet ConvertTo-XenRef *)
(***********************************)
and gen_xenref_converters classes =
  write_file "ConvertTo-XenRef.cs" (gen_body_xenref_converters classes)

and gen_body_xenref_converters classes =
  sprintf
    "%s\n\n\
     using System;\n\
     using System.Collections;\n\
     using System.Collections.Generic;\n\
     using System.Management.Automation;\n\
     using XenAPI;\n\n\
     namespace Citrix.XenServer.Commands\n\
     {\n\
    \    [Cmdlet(VerbsData.ConvertTo, \"XenRef\")]\n\
    \    [OutputType(typeof(IXenObject))]\n\
    \    public class ConvertToXenRefCommand : PSCmdlet\n\
    \    {\n\
    \        #region Cmdlet Parameters\n\n\
    \        [Parameter(Mandatory = true, ValueFromPipeline = true, Position = \
     0)]\n\
    \        public IXenObject XenObject { get; set; }\n\n\
    \        #endregion\n\n\
    \        #region Cmdlet Methods\n\n\
    \        protected override void ProcessRecord()\n\
    \        {%s\n\
    \        }\n\n\
    \        #endregion\n\n\
    \    }\n\
     }\n"
    Licence.bsd_two_clause (print_converters classes)

and print_converters classes =
  match classes with
  | [] ->
      ""
  | hd :: tl ->
      sprintf
        "\n\
        \            %s %s = XenObject as %s;\n\
        \            if (%s != null)\n\
        \            {\n\
        \                WriteObject(new XenRef<%s>(%s));\n\
        \                return;\n\
        \            }%s"
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
and gen_cmdlets obj =
  let {name= classname; messages; _} = obj in
  let stem = ocaml_class_to_csharp_class classname in

  let cmdlets =
    [
      {filename= sprintf "Get-Xen%s.cs" stem; content= gen_class obj classname}
    ; {
        filename= sprintf "New-Xen%s.cs" stem
      ; content=
          gen_constructor obj classname (List.filter is_constructor messages)
      }
    ; {
        filename= sprintf "Remove-Xen%s.cs" stem
      ; content=
          gen_destructor obj classname (List.filter is_destructor messages)
      }
    ; {
        filename= sprintf "Remove-Xen%sProperty.cs" stem
      ; content= gen_remover obj classname (List.filter is_remover messages)
      }
    ; {
        filename= sprintf "Add-Xen%s.cs" stem
      ; content= gen_adder obj classname (List.filter is_adder messages)
      }
    ; {
        filename= sprintf "Set-Xen%s.cs" stem
      ; content= gen_setter obj classname (List.filter is_setter messages)
      }
    ; {
        filename= sprintf "Get-Xen%sProperty.cs" stem
      ; content= gen_getter obj classname (List.filter is_getter messages)
      }
    ; {
        filename= sprintf "Invoke-Xen%s.cs" stem
      ; content= gen_invoker obj classname (List.filter is_invoke messages)
      }
    ]
  in

  cmdlets |> List.filter (fun x -> x.content <> "")

and write_file filename content =
  let fn = Filename.concat destdir filename in
  with_output fn (fun x -> output_string x content)

(*********************************)
(* Print function for Get-XenFoo *)
(*********************************)
and gen_class obj classname =
  if List.mem classname classes_with_records then
    print_header_class classname
    ^ print_parameters_class obj classname
    ^ print_methods_class classname (has_uuid obj) (has_name obj)
  else
    ""

and print_header_class classname =
  sprintf
    "%s\n\n\
     using System;\n\
     using System.Collections;\n\
     using System.Collections.Generic;\n\
     using System.Management.Automation;\n\
     using XenAPI;\n\n\
     namespace Citrix.XenServer.Commands\n\
     {\n\
    \    [Cmdlet(VerbsCommon.Get, \"Xen%s\", DefaultParameterSetName = \
     \"Ref\", SupportsShouldProcess = false)]\n\
    \    [OutputType(typeof(%s[]))]\n\
    \    public class GetXen%sCommand : XenServerCmdlet\n\
    \    {\n"
    Licence.bsd_two_clause
    (ocaml_class_to_csharp_class classname)
    (qualified_class_name classname)
    (ocaml_class_to_csharp_class classname)

and print_parameters_class obj classname =
  sprintf "        #region Cmdlet Parameters\n%s\n        #endregion\n"
    (print_xenobject_params obj classname false false true)

and print_methods_class classname has_uuid has_name =
  let classType = qualified_class_name classname in
  sprintf
    "\n\
    \        #region Cmdlet Methods\n\n\
    \        protected override void ProcessRecord()\n\
    \        {\n\
    \            GetSession();\n\n\
    \            var records = %s.get_all_records(session);\n\n\
    \            foreach (var record in records)\n\
    \                record.Value.opaque_ref = record.Key;\n\n\
    \            var results = new List<%s>();\n\n\
    \            if (Ref != null)\n\
    \            {\n\
    \                foreach (var record in records)\n\
    \                    if (Ref.opaque_ref == record.Key.opaque_ref)\n\
    \                    {\n\
    \                        results.Add(record.Value);\n\
    \                        break;\n\
    \                    }\n\
    \            }%s%s\n\
    \            else\n\
    \            {\n\
    \                results.AddRange(records.Values);\n\
    \            }\n\n\
    \            WriteObject(results, true);\n\n\
    \            UpdateSessions();\n\
    \        }\n\n\
    \        #endregion\n\
    \    }\n\
     }\n"
    classType classType
    ( if has_name then
        sprintf
          "\n\
          \            else if (Name != null)\n\
          \            {\n\
          \                var options = WildcardOptions.IgnoreCase\n\
          \                              | WildcardOptions.Compiled\n\
          \                              | WildcardOptions.CultureInvariant;\n\
          \                var wildcard = new WildcardPattern(Name, options);\n\n\
          \                foreach (var record in records)\n\
          \                {\n\
          \                    if (wildcard.IsMatch(record.Value.name_label))\n\
          \                        results.Add(record.Value);\n\
          \                }\n\
          \            }"
      else
        ""
    )
    ( if has_uuid then
        sprintf
          "\n\
          \            else if (Uuid != Guid.Empty)\n\
          \            {\n\
          \                foreach (var record in records)\n\
          \                    if (Uuid.ToString() == record.Value.uuid)\n\
          \                    {\n\
          \                        results.Add(record.Value);\n\
          \                        break;\n\
          \                    }\n\
          \            }"
      else
        ""
    )

(*********************************)
(* Print function for New-XenFoo *)
(*********************************)
and gen_constructor obj classname messages =
  match messages with
  | [] ->
      ""
  | [x] ->
      print_header_constructor x classname
      ^ print_params_constructor x obj classname
      ^ print_methods_constructor x obj classname
  | _ ->
      assert false

and print_header_constructor message classname =
  sprintf
    "%s\n\n\
     using System;\n\
     using System.Collections;\n\
     using System.Collections.Generic;\n\
     using System.Management.Automation;\n\
     using XenAPI;\n\n\
     namespace Citrix.XenServer.Commands\n\
     {\n\
    \    [Cmdlet(VerbsCommon.New, \"Xen%s\", DefaultParameterSetName = \
     \"Hashtable\", SupportsShouldProcess = true)]\n\
    \    [OutputType(typeof(%s))]%s\n\
    \    [OutputType(typeof(void))]\n\
    \    public class NewXen%sCommand : XenServerCmdlet\n\
    \    {" Licence.bsd_two_clause
    (ocaml_class_to_csharp_class classname)
    (qualified_class_name classname)
    ( if message.msg_async then
        "\n    [OutputType(typeof(XenAPI.Task))]"
      else
        ""
    )
    (ocaml_class_to_csharp_class classname)

and print_params_constructor message obj classname =
  sprintf
    "\n\
    \        #region Cmdlet Parameters\n\n\
    \        [Parameter]\n\
    \        public SwitchParameter PassThru { get; set; }\n\n\
    \        [Parameter(ParameterSetName = \"Hashtable\", Mandatory = true)]\n\
    \        public Hashtable HashTable { get; set; }\n\n\
    \        [Parameter(ParameterSetName = \"Record\", Mandatory = true)]\n\
    \        public %s Record { get; set; }\n\
     %s%s\n\
    \        #endregion\n"
    (qualified_class_name classname)
    ( if is_real_constructor message then
        gen_fields (DU.fields_of_obj obj)
      else
        gen_constructor_params message.msg_params
    )
    ( if message.msg_async then
        "\n\
        \        protected override bool GenerateAsyncParam\n\
        \        {\n\
        \            get { return true; }\n\
        \        }\n"
      else
        ""
    )

and gen_constructor_params params =
  match params with
  | [] ->
      ""
  | hd :: tl ->
      sprintf "%s\n%s"
        (gen_constructor_param hd.param_name hd.param_type ["Fields"])
        (gen_constructor_params tl)

and gen_fields fields =
  match fields with
  | [] ->
      ""
  | hd :: tl -> (
    match hd.qualifier with
    | DynamicRO ->
        gen_fields tl
    | _ ->
        sprintf "%s\n%s"
          (gen_constructor_param (full_name hd) hd.ty ["Fields"])
          (gen_fields tl)
  )

and gen_constructor_param paramName paramType paramsets =
  let publicName = ocaml_class_to_csharp_property paramName in
  (*Do not add a Record parameter; it has already been added manually as all constructors need one*)
  if paramName = "record" then
    ""
  else
    sprintf "\n        %s\n        public %s %s { get; set; }"
      (print_parameter_sets paramsets)
      (obj_internal_type paramType)
      publicName

and print_methods_constructor message obj classname =
  sprintf
    "\n\
    \        #region Cmdlet Methods\n\n\
    \        protected override void ProcessRecord()\n\
    \        {\n\
    \            GetSession();%s%s\n\
    \            RunApiCall(()=>\n\
    \            {%s\n\
    \            });\n\n\
    \            UpdateSessions();\n\
    \        }\n\n\
    \        #endregion\n\
    \   }\n\
     }\n"
    ( if is_real_constructor message then
        gen_make_record obj classname
      else
        gen_make_fields message obj
    )
    (gen_shouldprocess "New" message classname)
    (gen_csharp_api_call message classname "New" "passthru")

and gen_make_record obj classname =
  sprintf
    "\n\
    \            if (Record == null && HashTable == null)\n\
    \            {\n\
    \                Record = new %s();%s\n\
    \            }\n\
    \            else if (Record == null)\n\
    \            {\n\
    \                Record = new %s(HashTable);\n\
    \            }\n"
    (qualified_class_name classname)
    (gen_record_fields (DU.fields_of_obj obj))
    (qualified_class_name classname)

and gen_record_fields fields =
  match fields with
  | [] ->
      ""
  | h :: tl -> (
    match h.qualifier with
    | DynamicRO ->
        gen_record_fields tl
    | _ ->
        sprintf "\n                %s%s" (gen_record_field h)
          (gen_record_fields tl)
  )

and gen_record_field field =
  let chk =
    sprintf
      "if (MyInvocation.BoundParameters.ContainsKey(\"%s\"))\n                "
      (ocaml_field_to_csharp_property field)
  in
  let assignment =
    match field.ty with
    | Ref _ ->
        sprintf
          "    Record.%s = new %s(%s == null ? \"OpaqueRef:NULL\" : \
           %s.opaque_ref);"
          (full_name field)
          (obj_internal_type field.ty)
          (ocaml_field_to_csharp_property field)
          (ocaml_field_to_csharp_property field)
    | Map (u, v) ->
        sprintf
          "    Record.%s = \
           CommonCmdletFunctions.ConvertHashTableToDictionary<%s, %s>(%s);"
          (full_name field) (exposed_type u) (exposed_type v)
          (pascal_case (full_name field))
    | _ ->
        sprintf "    Record.%s = %s;" (full_name field)
          (ocaml_field_to_csharp_property field)
  in
  chk ^ assignment

and gen_make_fields message obj =
  sprintf
    "\n\
    \            if (Record != null)\n\
    \            {%s\n\
    \            }\n\
    \            else if (HashTable != null)\n\
    \            {%s\n\
    \            }"
    (explode_record_fields message (DU.fields_of_obj obj))
    (explode_hashtable_fields message (DU.fields_of_obj obj))

and explode_record_fields message fields =
  let print_map tl hd =
    sprintf
      "\n\
      \                %s = \
       CommonCmdletFunctions.ConvertDictionaryToHashtable(Record.%s);%s"
      (ocaml_class_to_csharp_property (full_name hd))
      (full_name hd)
      (explode_record_fields message tl)
  in
  let print_record tl hd =
    sprintf "\n                %s = Record.%s;%s"
      (ocaml_class_to_csharp_property (full_name hd))
      (full_name hd)
      (explode_record_fields message tl)
  in
  match fields with
  | [] ->
      ""
  | hd :: tl ->
      if List.exists (fun x -> full_name hd = x.param_name) message.msg_params
      then
        match hd.ty with
        | Map (_, _) ->
            print_map tl hd
        | _ ->
            print_record tl hd
      else
        explode_record_fields message tl

and explode_hashtable_fields message fields =
  match fields with
  | [] ->
      ""
  | hd :: tl ->
      if List.exists (fun x -> full_name hd = x.param_name) message.msg_params
      then
        sprintf "\n                %s = %s;%s"
          (ocaml_class_to_csharp_property (full_name hd))
          (convert_from_hashtable (full_name hd) hd.ty)
          (explode_hashtable_fields message tl)
      else
        explode_hashtable_fields message tl

and convert_from_hashtable fname ty =
  let field = sprintf "\"%s\"" fname in
  match ty with
  | DateTime ->
      sprintf "Marshalling.ParseDateTime(HashTable, %s)" field
  | Bool ->
      sprintf "Marshalling.ParseBool(HashTable, %s)" field
  | Float ->
      sprintf "Marshalling.ParseDouble(HashTable, %s)" field
  | Int ->
      sprintf "Marshalling.ParseLong(HashTable, %s)" field
  | Ref name ->
      sprintf "Marshalling.ParseRef<%s>(HashTable, %s)"
        (exposed_class_name name) field
  | SecretString | String ->
      sprintf "Marshalling.ParseString(HashTable, %s)" field
  | Set String ->
      sprintf "Marshalling.ParseStringArray(HashTable, %s)" field
  | Set (Ref x) ->
      sprintf "Marshalling.ParseSetRef<%s>(HashTable, %s)"
        (exposed_class_name x) field
  | Set (Enum (x, _)) ->
      sprintf
        "Helper.StringArrayToEnumList<%s>(Marshalling.ParseStringArray(HashTable, \
         %s))"
        x field
  | Enum (x, _) ->
      sprintf
        "(%s)CommonCmdletFunctions.EnumParseDefault(typeof(%s), \
         Marshalling.ParseString(HashTable, %s))"
        x x field
  | Map (Ref x, Record _) ->
      sprintf "Marshalling.ParseMapRefRecord<%s, Proxy_%s>(HashTable, %s)"
        (exposed_class_name x) (exposed_class_name x) field
  | Map (_, _) as x ->
      maps := TypeSet.add x !maps ;
      sprintf "(Marshalling.ParseHashTable(HashTable, %s))" field
  | Record name ->
      sprintf "new %s((Proxy_%s)HashTable[%s])" (exposed_class_name name)
        (exposed_class_name name) field
  | Set (Record name) ->
      sprintf "Helper.Proxy_%sArrayTo%sList(Marshalling.ParseStringArray(%s))"
        (exposed_class_name name) (exposed_class_name name) field
  | _ ->
      assert false

(************************************)
(* Print function for Remove-XenFoo *)
(************************************)
and gen_destructor obj classname messages =
  match messages with
  | [] ->
      ""
  | [x] ->
      let cut_message_name x = cut_msg_name (pascal_case x.msg_name) "Remove" in
      let asyncMessages =
        List.map cut_message_name (List.filter (fun x -> x.msg_async) messages)
      in
      sprintf
        "%s\n\n\
         using System;\n\
         using System.Collections;\n\
         using System.Collections.Generic;\n\
         using System.Management.Automation;\n\
         using XenAPI;\n\n\
         namespace Citrix.XenServer.Commands\n\
         {\n\
        \    [Cmdlet(VerbsCommon.Remove, \"Xen%s\", SupportsShouldProcess = \
         true)]\n\
        \    [OutputType(typeof(%s))]%s\n\
        \    [OutputType(typeof(void))]\n\
        \    public class RemoveXen%s : XenServerCmdlet\n\
        \    {\n\
        \        #region Cmdlet Parameters\n\n\
        \        [Parameter]\n\
        \        public SwitchParameter PassThru { get; set; }\n\
         %s%s\n\
        \        #endregion\n\n\
        \        #region Cmdlet Methods\n\n\
        \        protected override void ProcessRecord()\n\
        \        {\n\
        \            GetSession();\n\n\
        \            string %s = Parse%s();\n\n\
        \            %s\n\n\
        \            UpdateSessions();\n\
        \        }\n\n\
        \        #endregion\n\n\
        \        #region Private Methods\n\
         %s%s\n\
        \        #endregion\n\
        \    }\n\
         }\n"
        Licence.bsd_two_clause
        (ocaml_class_to_csharp_class classname)
        (qualified_class_name classname)
        ( if List.length asyncMessages > 0 then
            "\n    [OutputType(typeof(XenAPI.Task))]"
          else
            ""
        )
        (ocaml_class_to_csharp_class classname)
        (print_xenobject_params obj classname true true true)
        ( if List.length asyncMessages > 0 then
            sprintf
              "\n\
              \        protected override bool GenerateAsyncParam\n\
              \        {\n\
              \            get { return true; }\n\
              \        }\n"
          else
            ""
        )
        (ocaml_class_to_csharp_local_var classname)
        (ocaml_class_to_csharp_property classname)
        (print_cmdlet_methods_remover classname x)
        (print_parse_xenobject_private_method obj classname true)
        (print_process_record_private_methods classname messages "Remove"
           "asyncpassthru"
        )
  | _ ->
      assert false

and print_cmdlet_methods_remover classname message =
  let localVar = ocaml_class_to_csharp_local_var classname in
  let cut_message_name x = cut_msg_name (pascal_case x.msg_name) "Remove" in
  sprintf "ProcessRecord%s(%s);" (cut_message_name message) localVar

(*****************************************)
(* Print function for Remove-XenFoo -Bar *)
(*****************************************)
and gen_remover obj classname messages =
  match messages with
  | [] ->
      ""
  | _ ->
      let cut_message_name x = cut_msg_name (pascal_case x.msg_name) "Remove" in
      let asyncMessages =
        List.map cut_message_name (List.filter (fun x -> x.msg_async) messages)
      in
      sprintf
        "%s\n\n\
         using System;\n\
         using System.Collections;\n\
         using System.Collections.Generic;\n\
         using System.Management.Automation;\n\
         using XenAPI;\n\n\
         namespace Citrix.XenServer.Commands\n\
         {\n\
        \    [Cmdlet(VerbsCommon.Remove, \"Xen%sProperty\", \
         SupportsShouldProcess = true)]\n\
        \    [OutputType(typeof(%s))]%s\n\
        \    public class RemoveXen%sProperty : XenServerCmdlet\n\
        \    {\n\
        \        #region Cmdlet Parameters\n\n\
        \        [Parameter]\n\
        \        public SwitchParameter PassThru { get; set; }\n\
         %s%s%s\n\
        \        #endregion\n\n\
        \        #region Cmdlet Methods\n\n\
        \        protected override void ProcessRecord()\n\
        \        {\n\
        \            GetSession();\n\n\
        \            string %s = Parse%s();\n\n\
        \            %s\n\n\
        \            %s\n\n\
        \            UpdateSessions();\n\
        \        }\n\n\
        \        #endregion\n\n\
        \        #region Private Methods\n\
         %s%s\n\
        \        #endregion\n\
        \    }\n\
         }\n"
        Licence.bsd_two_clause
        (ocaml_class_to_csharp_class classname)
        (qualified_class_name classname)
        ( if List.length asyncMessages > 0 then
            "\n    [OutputType(typeof(XenAPI.Task))]"
          else
            ""
        )
        (ocaml_class_to_csharp_class classname)
        (print_xenobject_params obj classname true true true)
        (print_async_param asyncMessages)
        (gen_message_as_param classname "Remove" messages)
        (ocaml_class_to_csharp_local_var classname)
        (ocaml_class_to_csharp_property classname)
        (print_cmdlet_methods classname messages "Remove")
        (gen_passthru classname)
        (print_parse_xenobject_private_method obj classname true)
        (print_process_record_private_methods classname messages "Remove" "")

(**************************************)
(* Print function for Set-XenFoo -Bar *)
(**************************************)
and gen_setter obj classname messages =
  match messages with
  | [] ->
      ""
  | _ ->
      let cut_message_name x = cut_msg_name (pascal_case x.msg_name) "Set" in
      let asyncMessages =
        List.map cut_message_name (List.filter (fun x -> x.msg_async) messages)
      in
      sprintf
        "%s\n\n\
         using System;\n\
         using System.Collections;\n\
         using System.Collections.Generic;\n\
         using System.Management.Automation;\n\
         using XenAPI;\n\n\
         namespace Citrix.XenServer.Commands\n\
         {\n\
        \    [Cmdlet(VerbsCommon.Set, \"Xen%s\", SupportsShouldProcess = true)]\n\
        \    [OutputType(typeof(%s))]%s\n\
        \    [OutputType(typeof(void))]\n\
        \    public class SetXen%s : XenServerCmdlet\n\
        \    {\n\
        \        #region Cmdlet Parameters\n\n\
        \        [Parameter]\n\
        \        public SwitchParameter PassThru { get; set; }\n\
         %s%s%s\n\
        \        #endregion\n\n\
        \        #region Cmdlet Methods\n\n\
        \        protected override void ProcessRecord()\n\
        \        {\n\
        \            GetSession();\n\n\
        \            string %s = Parse%s();\n\n\
        \            %s\n\n\
        \            %s\n\n\
        \            UpdateSessions();\n\
        \        }\n\n\
        \        #endregion\n\n\
        \        #region Private Methods\n\
         %s%s\n\
        \        #endregion\n\
        \    }\n\
         }\n"
        Licence.bsd_two_clause
        (ocaml_class_to_csharp_class classname)
        (qualified_class_name classname)
        ( if List.length asyncMessages > 0 then
            "\n    [OutputType(typeof(XenAPI.Task))]"
          else
            ""
        )
        (ocaml_class_to_csharp_class classname)
        (print_xenobject_params obj classname true true true)
        (print_async_param asyncMessages)
        (gen_message_as_param classname "Set" messages)
        (ocaml_class_to_csharp_local_var classname)
        (ocaml_class_to_csharp_property classname)
        (print_cmdlet_methods classname messages "Set")
        (gen_passthru classname)
        (print_parse_xenobject_private_method obj classname true)
        (print_process_record_private_methods classname messages "Set" "")

(**************************************)
(* Print function for Add-XenFoo -Bar *)
(**************************************)
and gen_adder obj classname messages =
  match messages with
  | [] ->
      ""
  | _ ->
      let cut_message_name x = cut_msg_name (pascal_case x.msg_name) "Add" in
      let asyncMessages =
        List.map cut_message_name (List.filter (fun x -> x.msg_async) messages)
      in
      sprintf
        "%s\n\n\
         using System;\n\
         using System.Collections;\n\
         using System.Collections.Generic;\n\
         using System.Management.Automation;\n\
         using XenAPI;\n\n\
         namespace Citrix.XenServer.Commands\n\
         {\n\
        \    [Cmdlet(VerbsCommon.Add, \"Xen%s\", SupportsShouldProcess = true)]\n\
        \    [OutputType(typeof(%s))]%s\n\
        \    [OutputType(typeof(void))]\n\
        \    public class AddXen%s : XenServerCmdlet\n\
        \    {\n\
        \        #region Cmdlet Parameters\n\n\
        \        [Parameter]\n\
        \        public SwitchParameter PassThru { get; set; }\n\
         %s%s%s\n\
        \        #endregion\n\n\
        \        #region Cmdlet Methods\n\n\
        \        protected override void ProcessRecord()\n\
        \        {\n\
        \            GetSession();\n\n\
        \            string %s = Parse%s();\n\n\
        \            %s\n\n\
        \            %s\n\n\
        \            UpdateSessions();\n\
        \        }\n\n\
        \        #endregion\n\n\
        \        #region Private Methods\n\
         %s%s\n\
        \        #endregion\n\
        \    }\n\
         }\n"
        Licence.bsd_two_clause
        (ocaml_class_to_csharp_class classname)
        (qualified_class_name classname)
        ( if List.length asyncMessages > 0 then
            "\n    [OutputType(typeof(XenAPI.Task))]"
          else
            ""
        )
        (ocaml_class_to_csharp_class classname)
        (print_xenobject_params obj classname true true true)
        (print_async_param asyncMessages)
        (gen_message_as_param classname "Add" messages)
        (ocaml_class_to_csharp_local_var classname)
        (ocaml_class_to_csharp_property classname)
        (print_cmdlet_methods classname messages "Add")
        (gen_passthru classname)
        (print_parse_xenobject_private_method obj classname true)
        (print_process_record_private_methods classname messages "Add" "")

(*****************************************)
(* Print function for Invoke-XenFoo -Bar *)
(*****************************************)
and gen_invoker obj classname messages =
  match messages with
  | [] ->
      ""
  | _ ->
      let messagesWithParams =
        List.filter (is_message_with_dynamic_params classname) messages
      in
      sprintf
        "%s\n\n\
         using System;\n\
         using System.Collections;\n\
         using System.Collections.Generic;\n\
         using System.Management.Automation;\n\
         using XenAPI;\n\n\
         namespace Citrix.XenServer.Commands\n\
         {\n\
        \    [Cmdlet(VerbsLifecycle.Invoke, \"Xen%s\", SupportsShouldProcess = \
         true)]\n\
        \    public class InvokeXen%s : XenServerCmdlet\n\
        \    {\n\
        \        #region Cmdlet Parameters\n\n\
        \        [Parameter]\n\
        \        public SwitchParameter PassThru { get; set; }\n\
         %s\n\
        \        [Parameter(Mandatory = true)]\n\
        \        public Xen%sAction XenAction { get; set; }\n\n\
        \        #endregion\n\
         %s\n\
        \        #region Cmdlet Methods\n\n\
        \        protected override void ProcessRecord()\n\
        \        {\n\
        \            GetSession();\n\n\
        \            string %s = Parse%s();\n\n\
        \            switch (XenAction)\n\
        \            {%s\n\
        \            }\n\n\
        \            UpdateSessions();\n\
        \        }\n\n\
        \        #endregion\n\n\
        \        #region Private Methods\n\
         %s%s\n\
        \        #endregion\n\
        \    }\n\n\
        \    public enum Xen%sAction\n\
        \    {%s\n\
        \    }\n\
         %s\n\
         }\n"
        Licence.bsd_two_clause
        (ocaml_class_to_csharp_class classname)
        (ocaml_class_to_csharp_class classname)
        (print_xenobject_params obj classname true true true)
        (ocaml_class_to_csharp_class classname)
        (print_dynamic_generator classname "Action" "Invoke" messagesWithParams)
        (ocaml_class_to_csharp_local_var classname)
        (ocaml_class_to_csharp_property classname)
        (print_cmdlet_methods_dynamic classname messages "Action" "Invoke")
        (print_parse_xenobject_private_method obj classname true)
        (print_process_record_private_methods classname messages "Invoke"
           "passthru"
        )
        (ocaml_class_to_csharp_class classname)
        (print_messages_as_enum "Invoke" messages)
        (print_dynamic_params classname "Action" "Invoke" messagesWithParams)

(**********************************************)
(* Print function for Get-XenFooProperty -Bar *)
(**********************************************)
and gen_getter obj classname messages =
  match messages with
  | [] ->
      ""
  | _ ->
      let messagesWithParams =
        List.filter (is_message_with_dynamic_params classname) messages
      in
      sprintf
        "%s\n\n\
         using System;\n\
         using System.Collections;\n\
         using System.Collections.Generic;\n\
         using System.Management.Automation;\n\
         using XenAPI;\n\n\
         namespace Citrix.XenServer.Commands\n\
         {\n\
        \    [Cmdlet(VerbsCommon.Get, \"Xen%sProperty\", SupportsShouldProcess \
         = false)]\n\
        \    public class GetXen%sProperty : XenServerCmdlet\n\
        \    {\n\
        \        #region Cmdlet Parameters\n\
         %s\n\
        \        [Parameter(Mandatory = true)]\n\
        \        public Xen%sProperty XenProperty { get; set; }\n\n\
        \        #endregion\n\
         %s\n\
        \        #region Cmdlet Methods\n\n\
        \        protected override void ProcessRecord()\n\
        \        {\n\
        \            GetSession();\n\n\
        \            string %s = Parse%s();\n\n\
        \            switch (XenProperty)\n\
        \            {%s\n\
        \            }\n\n\
        \            UpdateSessions();\n\
        \        }\n\n\
        \        #endregion\n\n\
        \        #region Private Methods\n\
         %s%s\n\
        \        #endregion\n\
        \    }\n\n\
        \    public enum Xen%sProperty\n\
        \    {%s\n\
        \    }\n\
         %s\n\
         }\n"
        Licence.bsd_two_clause
        (ocaml_class_to_csharp_class classname)
        (ocaml_class_to_csharp_class classname)
        (print_xenobject_params obj classname true true false)
        (ocaml_class_to_csharp_class classname)
        (print_dynamic_generator classname "Property" "Get" messagesWithParams)
        (ocaml_class_to_csharp_local_var classname)
        (ocaml_class_to_csharp_property classname)
        (print_cmdlet_methods_dynamic classname messages "Property" "Get")
        (print_parse_xenobject_private_method obj classname false)
        (print_process_record_private_methods classname messages "Get" "pipe")
        (ocaml_class_to_csharp_class classname)
        (print_messages_as_enum "Get" messages)
        (print_dynamic_params classname "Property" "Get" messagesWithParams)

and print_cmdlet_methods_dynamic classname messages enum commonVerb =
  let cut_message_name x = cut_msg_name (pascal_case x.msg_name) commonVerb in
  let localVar = ocaml_class_to_csharp_local_var classname in
  match messages with
  | [] ->
      ""
  | hd :: tl ->
      sprintf
        "\n\
        \                case Xen%s%s.%s:\n\
        \                    ProcessRecord%s(%s);\n\
        \                    break;%s"
        (ocaml_class_to_csharp_class classname)
        enum (cut_message_name hd) (cut_message_name hd) localVar
        (print_cmdlet_methods_dynamic classname tl enum commonVerb)

(**************************************)
(* Common to more than one generators *)
(**************************************)
and gen_passthru classname =
  sprintf
    "if (!PassThru)\n\
    \                return;\n\n\
    \            RunApiCall(() =>\n\
    \                {\n\
    \                    var contxt = _context as \
     XenServerCmdletDynamicParameters;\n\n\
    \                    if (contxt != null && contxt.Async)\n\
    \                    {\n\
    \                        XenAPI.Task taskObj = null;\n\
    \                        if (taskRef != null && taskRef != \
     \"OpaqueRef:NULL\")\n\
    \                        {\n\
    \                            taskObj = XenAPI.Task.get_record(session, \
     taskRef.opaque_ref);\n\
    \                            taskObj.opaque_ref = taskRef.opaque_ref;\n\
    \                        }\n\n\
    \                        WriteObject(taskObj, true);\n\
    \                    }\n\
    \                    else\n\
    \                    {\n\n\
    \                        var obj = %s.get_record(session, %s);\n\
    \                        if (obj != null)\n\
    \                            obj.opaque_ref = %s;\n\
    \                        WriteObject(obj, true);\n\n\
    \                    }\n\
    \                });"
    (qualified_class_name classname)
    (ocaml_class_to_csharp_local_var classname)
    (ocaml_class_to_csharp_local_var classname)

and is_message_with_dynamic_params classname message =
  let nonClassParams =
    List.filter (fun x -> not (is_class x classname)) message.msg_params
  in
  if List.length nonClassParams > 0 || message.msg_async then
    true
  else
    false

and print_dynamic_generator classname enum commonVerb messagesWithParams =
  match messagesWithParams with
  | [] ->
      ""
  | _ ->
      sprintf
        "\n\
        \        public override object GetDynamicParameters()\n\
        \        {\n\
        \            switch (Xen%s)\n\
        \            {%s\n\
        \                default:\n\
        \                    return null;\n\
        \            }\n\
        \        }\n"
        enum
        (print_messages_with_params classname enum commonVerb messagesWithParams)

and print_messages_with_params classname enum commonVerb x =
  match x with
  | [] ->
      ""
  | hd :: tl ->
      sprintf
        "\n\
        \                case Xen%s%s.%s:\n\
        \                    _context = new Xen%s%s%sDynamicParameters();\n\
        \                    return _context;%s"
        (ocaml_class_to_csharp_class classname)
        enum
        (cut_msg_name (pascal_case hd.msg_name) commonVerb)
        (ocaml_class_to_csharp_class classname)
        enum
        (cut_msg_name (pascal_case hd.msg_name) commonVerb)
        (print_messages_with_params classname enum commonVerb tl)

and print_dynamic_params classname enum commonVerb messagesWithParams =
  match messagesWithParams with
  | [] ->
      ""
  | hd :: tl ->
      sprintf
        "\n\
        \    public class Xen%s%s%sDynamicParameters : \
         IXenServerDynamicParameter\n\
        \    {%s%s\n\
        \    }\n\
         %s"
        (ocaml_class_to_csharp_class classname)
        enum
        (cut_msg_name (pascal_case hd.msg_name) commonVerb)
        ( if hd.msg_async then
            "\n\
            \        [Parameter]\n\
            \        public SwitchParameter Async { get; set; }\n"
          else
            ""
        )
        (print_dynamic_param_members classname hd.msg_params commonVerb)
        (print_dynamic_params classname enum commonVerb tl)

and print_dynamic_param_members classname params commonVerb =
  match params with
  | [] ->
      ""
  | hd :: tl ->
      if is_class hd classname then
        print_dynamic_param_members classname tl commonVerb
      else
        let publicProperty =
          if
            commonVerb = "Invoke"
            && List.mem (String.lowercase_ascii hd.param_name) ["name"; "uuid"]
          then
            ocaml_class_to_csharp_property hd.param_name ^ "Param"
          else
            ocaml_class_to_csharp_property hd.param_name
        in
        let theType = obj_internal_type hd.param_type in
        sprintf "\n        [Parameter]\n        public %s %s { get; set; }\n%s "
          theType publicProperty
          (print_dynamic_param_members classname tl commonVerb)

and print_messages_as_enum commonVerb messages =
  let cut_message_name x = cut_msg_name (pascal_case x.msg_name) commonVerb in
  match messages with
  | [] ->
      ""
  | [x] ->
      sprintf "\n        %s" (cut_message_name x)
  | hd :: tl ->
      sprintf "\n        %s,%s" (cut_message_name hd)
        (print_messages_as_enum commonVerb tl)

and gen_message_as_param classname commonVerb messages =
  match messages with
  | [] ->
      ""
  | hd :: tl ->
      let msgType = get_message_type hd classname commonVerb in
      let cutMessageName = cut_msg_name (pascal_case hd.msg_name) commonVerb in
      let msgName =
        if cutMessageName = "Host" then "XenHost" else cutMessageName
      in
      sprintf
        "\n\
        \        [Parameter]\n\
        \        public %s %s\n\
        \        {\n\
        \            get { return %s; }\n\
        \            set\n\
        \            {\n\
        \                %s = value;\n\
        \                %sIsSpecified = true;\n\
        \            }\n\
        \        }\n\
        \        private %s %s;\n\
        \        private bool %sIsSpecified;\n\
         %s"
        msgType msgName
        (lower_and_underscore_first msgName)
        (lower_and_underscore_first msgName)
        (lower_and_underscore_first msgName)
        msgType
        (lower_and_underscore_first msgName)
        (lower_and_underscore_first msgName)
        (gen_message_as_param classname commonVerb tl)

and print_cmdlet_methods classname messages commonVerb =
  let cut_message_name x = cut_msg_name (pascal_case x.msg_name) commonVerb in
  let switch_name x =
    if cut_message_name x = "Host" then "XenHost" else cut_message_name x
  in
  let localVar = ocaml_class_to_csharp_local_var classname in
  match messages with
  | [] ->
      ""
  | [x] ->
      sprintf "if (%sIsSpecified)\n                ProcessRecord%s(%s);"
        (lower_and_underscore_first (switch_name x))
        (cut_message_name x) localVar
  | hd :: tl ->
      sprintf
        "if (%sIsSpecified)\n\
        \                ProcessRecord%s(%s);\n\
        \            %s"
        (lower_and_underscore_first (switch_name hd))
        (cut_message_name hd) localVar
        (print_cmdlet_methods classname tl commonVerb)

and print_xenobject_params obj classname mandatoryRef includeXenObject
    includeUuidAndName =
  let publicName = ocaml_class_to_csharp_property classname in
  sprintf
    "%s\n\n\
    \        [Parameter(ParameterSetName = \"Ref\"%s, \
     ValueFromPipelineByPropertyName = true, Position = 0)]\n\
    \        [Alias(\"opaque_ref\")]\n\
    \        public XenRef<%s> Ref { get; set; }\n\
     %s%s\n"
    ( if includeXenObject then
        print_param_xen_object (qualified_class_name classname) publicName
      else
        ""
    )
    (if mandatoryRef then ", Mandatory = true" else "")
    (qualified_class_name classname)
    (print_param_uuid (has_uuid obj && includeUuidAndName))
    (print_param_name (has_name obj && includeUuidAndName))

and print_param_xen_object qualifiedClassName publicName =
  sprintf
    "\n\
    \        [Parameter(ParameterSetName = \"XenObject\", Mandatory = true, \
     ValueFromPipeline = true, Position = 0)]\n\
    \        public %s %s { get; set; }" qualifiedClassName publicName

and print_param_uuid hasUuid =
  if hasUuid then
    sprintf
      "\n\
      \        [Parameter(ParameterSetName = \"Uuid\", Mandatory = true, \
       ValueFromPipelineByPropertyName = true, Position = 0)]\n\
      \        public Guid Uuid { get; set; }\n"
  else
    sprintf ""

and print_param_name hasName =
  if hasName then
    sprintf
      "\n\
      \        [Parameter(ParameterSetName = \"Name\", Mandatory = true, \
       ValueFromPipelineByPropertyName = true, Position = 0)]\n\
      \        [Alias(\"name_label\")]\n\
      \        public string Name { get; set; }\n"
  else
    sprintf ""

and print_async_param asyncMessages =
  match asyncMessages with
  | [] ->
      ""
  | _ ->
      sprintf
        "\n\
        \        protected override bool GenerateAsyncParam\n\
        \        {\n\
        \            get\n\
        \            {\n\
        \                return %s;\n\
        \            }\n\
        \        }\n"
        (condition asyncMessages)

and condition messages =
  match messages with
  | [] ->
      ""
  | [x] ->
      sprintf "%sIsSpecified" (lower_and_underscore_first x)
  | hd :: tl ->
      sprintf "%sIsSpecified\n                       ^ %s"
        (lower_and_underscore_first hd)
        (condition tl)

and get_message_type message classname commonVerb =
  let messageParams =
    List.filter (fun x -> not (is_class x classname)) message.msg_params
  in
  match commonVerb with
  | "Remove" -> (
    match messageParams with
    | [x] ->
        obj_internal_type x.param_type
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | "Add" -> (
    match messageParams with
    | [x] ->
        obj_internal_type x.param_type
    | [x; y] ->
        sprintf "KeyValuePair<%s, %s>"
          (obj_internal_type x.param_type)
          (obj_internal_type y.param_type)
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | "Set" -> (
    match messageParams with
    | [x] ->
        obj_internal_type x.param_type
    | [x; y]
      when not (obj_internal_type x.param_type = obj_internal_type y.param_type)
      ->
        sprintf "KeyValuePair<%s, %s>"
          (obj_internal_type x.param_type)
          (obj_internal_type y.param_type)
    | hd :: tl ->
        let hdtype = obj_internal_type hd.param_type in
        if List.for_all (fun x -> hdtype = obj_internal_type x.param_type) tl
        then
          sprintf "%s[]" hdtype
        else (
          Printf.eprintf "%s" message.msg_name ;
          assert false
        )
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | "Get" -> (
    match messageParams with
    | [] ->
        "SwitchParameter"
    | [x] ->
        obj_internal_type x.param_type
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | _ ->
      ""

and print_parameter_sets parameterSets =
  match parameterSets with
  | [] ->
      "[Parameter]"
  | [x] ->
      sprintf "[Parameter(ParameterSetName = \"%s\")]" x
  | hd :: tl ->
      sprintf "[Parameter(ParameterSetName = \"%s\")]\n        %s" hd
        (print_parameter_sets tl)

and print_parse_xenobject_private_method obj classname includeUuidAndName =
  let publicProperty = ocaml_class_to_csharp_property classname in
  let localVar = ocaml_class_to_csharp_local_var classname in
  sprintf
    "\n\
    \        private string Parse%s()\n\
    \        {\n\
    \            string %s = null;\n\n\
    \            if (%s != null)\n\
    \                %s = (new XenRef<%s>(%s)).opaque_ref;%s%s\n\
    \            else if (Ref != null)\n\
    \                %s = Ref.opaque_ref;\n\
    \            else\n\
    \            {\n\
    \                ThrowTerminatingError(new ErrorRecord(\n\
    \                    new ArgumentException(\"At least one of the \
     parameters '%s', 'Ref'%s must be set\"),\n\
    \                    string.Empty,\n\
    \                    ErrorCategory.InvalidArgument,\n\
    \                    %s));\n\
    \            }\n\n\
    \            return %s;\n\
    \        }\n"
    publicProperty localVar publicProperty localVar
    (qualified_class_name classname)
    publicProperty
    ( if has_uuid obj && includeUuidAndName then
        sprintf
          "\n\
          \            else if (Uuid != Guid.Empty)\n\
          \            {\n\
          \                var xenRef = %s.get_by_uuid(session, \
           Uuid.ToString());\n\
          \                if (xenRef != null)\n\
          \                    %s = xenRef.opaque_ref;\n\
          \            }"
          (qualified_class_name classname)
          localVar
      else
        sprintf ""
    )
    ( if has_name obj && includeUuidAndName then
        sprintf
          "\n\
          \            else if (Name != null)\n\
          \            {\n\
          \                var xenRefs = %s.get_by_name_label(session, Name);\n\
          \                if (xenRefs.Count == 1)\n\
          \                    %s = xenRefs[0].opaque_ref;\n\
          \                else if (xenRefs.Count > 1)\n\
          \                    ThrowTerminatingError(new ErrorRecord(\n\
          \                        new ArgumentException(string.Format(\"More \
           than one %s with name label {0} exist\", Name)),\n\
          \                        string.Empty,\n\
          \                        ErrorCategory.InvalidArgument,\n\
          \                        Name));\n\
          \            }"
          (qualified_class_name classname)
          localVar
          (qualified_class_name classname)
      else
        sprintf ""
    )
    localVar publicProperty
    ( if has_uuid obj then
        sprintf ", 'Uuid'"
      else
        sprintf ""
    )
    publicProperty localVar

and print_process_record_private_methods classname messages commonVerb switch =
  match messages with
  | [] ->
      sprintf ""
  | hd :: tl ->
      let cutMessageName = cut_msg_name (pascal_case hd.msg_name) commonVerb in
      sprintf
        "\n\
        \        private void ProcessRecord%s(string %s)\n\
        \        {%s%s\n\
        \            RunApiCall(()=>\n\
        \            {%s\n\
        \            });\n\
        \        }\n\
         %s"
        cutMessageName
        (ocaml_class_to_csharp_local_var classname)
        ""
        (gen_shouldprocess commonVerb hd classname)
        (gen_csharp_api_call hd classname commonVerb switch)
        (print_process_record_private_methods classname tl commonVerb switch)

and gen_shouldprocess commonVerb message classname =
  match commonVerb with
  | "Get" ->
      ""
  | _ ->
      let theObj =
        if classname = "pool" || commonVerb = "New" then
          "session.Url"
        else
          ocaml_class_to_csharp_local_var classname
      in
      sprintf
        "\n\
        \            if (!ShouldProcess(%s, \"%s.%s\"))\n\
        \                return;\n"
        theObj
        (exposed_class_name classname)
        message.msg_name

and gen_csharp_api_call message classname commonVerb switch =
  let asyncPipe = gen_csharp_api_call_async_pipe in
  let passThruTask =
    if switch = "pipe" then
      asyncPipe
    else if switch = "passthru" || switch = "asyncpassthru" then
      print_pass_thru asyncPipe
    else
      ""
  in
  let syncPipe = gen_csharp_api_call_sync_pipe message classname in
  let passThruResult =
    match (switch, message.msg_result) with
    | "pipe", _ ->
        syncPipe
    | "passthru", None ->
        "\n\
        \                    if (PassThru)\n\
        \                        WriteWarning(\"-PassThru can only be used \
         with -Async for this cmdlet. Ignoring.\");"
    | "passthru", _ ->
        print_pass_thru syncPipe
    | _ ->
        ""
  in
  if message.msg_async then
    sprintf
      "\n\
      \                var contxt = _context as %s;\n\n\
      \                if (contxt != null && contxt.Async)\n\
      \                {%s%s\n\
      \                }\n\
      \                else\n\
      \                {%s%s\n\
      \                }\n"
      ( if commonVerb = "Invoke" then
          sprintf "Xen%sAction%sDynamicParameters"
            (ocaml_class_to_csharp_class classname)
            (cut_msg_name (pascal_case message.msg_name) "Invoke")
        else if commonVerb = "Get" then
          sprintf "Xen%sProperty%sDynamicParameters"
            (ocaml_class_to_csharp_class classname)
            (cut_msg_name (pascal_case message.msg_name) "Get")
        else
          "XenServerCmdletDynamicParameters"
      )
      (gen_csharp_api_call_async message classname commonVerb)
      passThruTask
      (gen_csharp_api_call_sync message classname commonVerb)
      passThruResult
  else
    sprintf "%s%s%s"
      ( if
          commonVerb = "Invoke"
          && is_message_with_dynamic_params classname message
        then
          sprintf
            "\n\
            \                var contxt = _context as \
             Xen%sAction%sDynamicParameters;\n\
            \                if (contxt == null)\n\
            \                    return;"
            (ocaml_class_to_csharp_class classname)
            (cut_msg_name (pascal_case message.msg_name) "Invoke")
        else if
          commonVerb = "Get" && is_message_with_dynamic_params classname message
        then
          sprintf
            "\n\
            \                var contxt = _context as \
             Xen%sProperty%sDynamicParameters;\n\
            \                if (contxt == null)\n\
            \                    return;"
            (ocaml_class_to_csharp_class classname)
            (cut_msg_name (pascal_case message.msg_name) "Get")
        else
          ""
      )
      (gen_csharp_api_call_sync message classname commonVerb)
      passThruResult

and print_pass_thru x =
  sprintf
    "\n\
    \                    if (PassThru)\n\
    \                    {%s\n\
    \                    }" x

and gen_csharp_api_call_async message classname commonVerb =
  sprintf "\n                    taskRef = %s.async_%s(%s);\n"
    (qualified_class_name classname)
    message.msg_name
    (gen_call_params classname message commonVerb)

and gen_csharp_api_call_async_pipe =
  sprintf
    "\n\
    \                        XenAPI.Task taskObj = null;\n\
    \                        if (taskRef != \"OpaqueRef:NULL\")\n\
    \                        {\n\
    \                            taskObj = XenAPI.Task.get_record(session, \
     taskRef.opaque_ref);\n\
    \                            taskObj.opaque_ref = taskRef.opaque_ref;\n\
    \                        }\n\n\
    \                        WriteObject(taskObj, true);"

and gen_csharp_api_call_sync message classname commonVerb =
  match message.msg_result with
  | None ->
      sprintf "\n                    %s.%s(%s);\n"
        (qualified_class_name classname)
        message.msg_name
        (gen_call_params classname message commonVerb)
  | Some (Ref _, _) ->
      sprintf "\n                    string objRef = %s.%s(%s);\n"
        (qualified_class_name classname)
        message.msg_name
        (gen_call_params classname message commonVerb)
  | Some (Set (Ref _), _) ->
      sprintf "\n                    var refs = %s.%s(%s);\n"
        (qualified_class_name classname)
        message.msg_name
        (gen_call_params classname message commonVerb)
  | Some (Map (_, _), _) ->
      sprintf "\n                    var dict = %s.%s(%s);\n"
        (qualified_class_name classname)
        message.msg_name
        (gen_call_params classname message commonVerb)
  | Some (x, _) ->
      sprintf "\n                    %s obj = %s.%s(%s);\n" (exposed_type x)
        (qualified_class_name classname)
        message.msg_name
        (gen_call_params classname message commonVerb)

and gen_csharp_api_call_sync_pipe message classname =
  match message.msg_result with
  | None ->
      sprintf
        "\n\
        \                        var obj = %s.get_record(session, %s);\n\
        \                        if (obj != null)\n\
        \                            obj.opaque_ref = %s;\n\
        \                        WriteObject(obj, true);"
        (qualified_class_name classname)
        (ocaml_class_to_csharp_local_var classname)
        (ocaml_class_to_csharp_local_var classname)
  | Some (Ref r, _) ->
      sprintf
        "\n\
        \                        %s obj = null;\n\n\
        \                        if (objRef != \"OpaqueRef:NULL\")\n\
        \                        {\n\
        \                            obj = %s.get_record(session, objRef);\n\
        \                            obj.opaque_ref = objRef;\n\
        \                        }\n\n\
        \                        WriteObject(obj, true);"
        (qualified_class_name r) (qualified_class_name r)
  | Some (Set (Ref r), _) ->
      sprintf
        "\n\
        \                        var records = new List<%s>();\n\n\
        \                        foreach (var _ref in refs)\n\
        \                        {\n\
        \                            if (_ref.opaque_ref == \"OpaqueRef:NULL\")\n\
        \                                continue;\n\n\
        \                            var record = %s.get_record(session, _ref);\n\
        \                            record.opaque_ref = _ref.opaque_ref;\n\
        \                            records.Add(record);\n\
        \                        }\n\n\
        \                        WriteObject(records, true);"
        (qualified_class_name r) (qualified_class_name r)
  | Some (Map (_, _), _) ->
      sprintf
        "\n\
        \                        Hashtable ht = \
         CommonCmdletFunctions.ConvertDictionaryToHashtable(dict);\n\
        \                        WriteObject(ht, true);"
  | Some (_, _) ->
      sprintf "\n                        WriteObject(obj, true);"

and gen_call_params classname message commonVerb =
  String.concat ", "
    ("session" :: gen_param_list classname message.msg_params message commonVerb)

and gen_param_list classname params message commonVerb =
  let cutMessageName =
    cut_msg_name (ocaml_class_to_csharp_property message.msg_name) commonVerb
  in
  let get_param_name x =
    if is_class x classname then
      ocaml_class_to_csharp_local_var classname
    else if
      commonVerb = "Invoke"
      && List.mem (String.lowercase_ascii x.param_name) ["name"; "uuid"]
    then
      sprintf "contxt.%s" (ocaml_class_to_csharp_property x.param_name)
      ^ "Param"
    else if commonVerb = "Invoke" then
      sprintf "contxt.%s" (ocaml_class_to_csharp_property x.param_name)
    else if commonVerb = "Get" then
      sprintf "contxt.%s" (ocaml_class_to_csharp_property x.param_name)
    else if not (commonVerb = "New") then
      cutMessageName
    else
      ocaml_class_to_csharp_property x.param_name
  in
  let valueOfPair x =
    match x.param_type with
    | Map (u, v) ->
        sprintf
          "CommonCmdletFunctions.ConvertHashTableToDictionary<%s, %s>(%s.Value)"
          (exposed_type u) (exposed_type v) cutMessageName
    | _ ->
        cutMessageName ^ ".Value"
  in
  let api_call_param x =
    match x.param_type with
    | Map (u, v) ->
        sprintf "CommonCmdletFunctions.ConvertHashTableToDictionary<%s, %s>(%s)"
          (exposed_type u) (exposed_type v) (get_param_name x)
    | _ ->
        get_param_name x
  in
  let messageParams =
    List.filter (fun x -> not (is_class x classname)) message.msg_params
  in
  let restParams =
    List.filter (fun x -> is_class x classname) message.msg_params
  in
  let procParams = List.map get_param_name restParams in
  match commonVerb with
  | "Remove" -> (
    match messageParams with
    | [] ->
        procParams
    | [x] ->
        procParams @ [api_call_param x]
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | "Add" -> (
    match messageParams with
    | [x] ->
        procParams @ [api_call_param x]
    | [_; y] ->
        procParams @ [cutMessageName ^ ".Key"; valueOfPair y]
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | "Set" -> (
    match messageParams with
    | [x] ->
        procParams @ [api_call_param x]
    | [x; y]
      when not (obj_internal_type x.param_type = obj_internal_type y.param_type)
      ->
        procParams @ [cutMessageName ^ ".Key"; valueOfPair y]
    | hd :: tl ->
        let argList = ref [] in
        let hdtype = obj_internal_type hd.param_type in
        if List.for_all (fun x -> hdtype = obj_internal_type x.param_type) tl
        then (
          explode_array cutMessageName (List.length messageParams) argList ;
          procParams @ !argList
        ) else (
          Printf.eprintf "%s" message.msg_name ;
          assert false
        )
    | _ ->
        Printf.eprintf "%s" message.msg_name ;
        assert false
  )
  | _ -> (
    match params with
    | [] ->
        []
    | h :: tl ->
        api_call_param h :: gen_param_list classname tl message commonVerb
  )

and explode_array name length result =
  for i = length - 1 downto 0 do
    result := sprintf "%s[%s]" name (string_of_int i) :: !result
  done

and is_class param classname =
  String.lowercase_ascii param.param_name = "self"
  || String.lowercase_ascii param.param_name = String.lowercase_ascii classname

let _ = main ()
