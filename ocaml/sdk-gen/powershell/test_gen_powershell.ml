(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

(* Tests for the mustache templates used by gen_powershell_binding.
 *
 * Every cmdlet file the PowerShell generator emits is produced by rendering a
 * template from [templates/] against a JSON object. These tests pin that
 * rendering: each template is rendered against a representative fixture and
 * compared with a golden file in [test_data/].
 *
 * The fixtures deliberately mirror the JSON built by the corresponding
 * function in gen_powershell_binding.ml (gen_class, gen_constructor,
 * gen_message_family, gen_invoker), so a template that stops
 * agreeing with its caller shows up here.
 *
 * The large C# fragments (parameter blocks, method bodies) are injected by the
 * generator as pre-rendered strings via {{{...}}}. The fixtures use short
 * stand-ins for those: what matters is that they are interpolated verbatim and
 * in the right place, not what they contain.
 *
 * To regenerate the golden files after an intentional template change:
 *
 *   PS_GOLDEN_UPDATE=1 dune test ocaml/sdk-gen/powershell
 *   cp _build/default/ocaml/sdk-gen/powershell/test_data.new/* \
 *      ocaml/sdk-gen/powershell/test_data/
 *
 * Always read the resulting diff before committing it.
 *)

open CommonFunctions

let ( // ) = Filename.concat

let templates_dir = "templates"

let test_data_dir = "test_data"

(* Mirrors [render_to_string] in gen_powershell_binding.ml, so these tests go
   through the same rendering path as the generator itself -- including its use
   of [string_of_file], which is what drops the templates' final newline. *)
let render template_name json =
  let templ =
    string_of_file (templates_dir // template_name) |> Mustache.of_string
  in
  Mustache.render templ json

(* [CommonFunctions.string_of_file] joins the lines it reads with "\n" and so
   loses any trailing newline, which is exactly one of the things these tests
   need to pin down. Read the golden files verbatim instead, tolerating a CRLF
   checkout. *)
let read_golden path =
  let ic = open_in_bin path in
  Fun.protect
    (fun () ->
      let contents = really_input_string ic (in_channel_length ic) in
      String.concat "" (String.split_on_char '\r' contents)
    )
    ~finally:(fun () -> close_in ic)

let update_golden = Sys.getenv_opt "PS_GOLDEN_UPDATE" <> None

(* dune mounts the [test_data] dependency read-only, so update mode writes the
   candidates to a sibling directory for the caller to copy over. *)
let golden_out_dir = "test_data.new"

(* In update mode write the rendered output out and feed it back as the
   expectation, so the run doubles as a way to author the golden files. *)
let golden filename rendered =
  if update_golden then (
    if not (Sys.file_exists golden_out_dir) then Sys.mkdir golden_out_dir 0o755 ;
    let path = golden_out_dir // filename in
    let out = open_out_bin path in
    Fun.protect
      (fun () -> output_string out rendered)
      ~finally:(fun () -> close_out out) ;
    rendered
  ) else
    read_golden (test_data_dir // filename)

let str s = `String s

(* ------------------------------------------------------------------ *)
(* Fixtures                                                            *)
(* ------------------------------------------------------------------ *)

let licence = "// Copyright (c) Cloud Software Group, Inc."

let get_params =
  {|
        [Parameter(ParameterSetName = "Ref", Mandatory = true, ValueFromPipelineByPropertyName = true, Position = 0)]
        public XenRef<XenAPI.VM> Ref { get; set; }
|}

(* Get-XenObject.mustache, as built by [gen_class]. *)
let get_xen_object =
  `O
    [
      ("licence", str licence)
    ; ("class", str "VM")
    ; ("qualified_type", str "XenAPI.VM")
    ; ("params", str get_params)
    ; ("has_uuid", `Bool true)
    ; ("has_name", `Bool true)
    ]

(* Same template with both optional lookup branches switched off: several
   classes have neither a uuid nor a name_label field. *)
let get_xen_object_minimal =
  `O
    [
      ("licence", str licence)
    ; ("class", str "Blob")
    ; ("qualified_type", str "XenAPI.Blob")
    ; ("params", str get_params)
    ; ("has_uuid", `Bool false)
    ; ("has_name", `Bool false)
    ]

(* New-XenObject.mustache, as built by [gen_constructor]. *)
let new_xen_object =
  `O
    [
      ("licence", str licence)
    ; ("class", str "VM")
    ; ("qualified_type", str "XenAPI.VM")
    ; ("async_task", `Bool true)
    ; ( "fields_or_params"
      , str
          {|
        [Parameter(ParameterSetName = "Fields")]
        public string NameLabel { get; set; }
|}
      )
    ; ( "async_param_override"
      , str
          {|
        protected override bool GenerateAsyncParam
        {
            get { return true; }
        }
|}
      )
    ; ("make", str "\n\n            var record = new XenAPI.VM();")
    ; ( "shouldprocess"
      , str
          "\n\n\
          \            if (!ShouldProcess(\"VM\", \"New\"))\n\
          \                return;"
      )
    ; ("open_brace", str "{")
    ; ( "api_call"
      , str "\n                var obj = XenAPI.VM.create(session, record);"
      )
    ]

(* Set-XenObject.mustache, as built by [gen_message_family]. *)
let set_xen_object =
  `O
    [
      ("licence", str licence)
    ; ("verb", str "Set")
    ; ("noun", str "VM")
    ; ("qualified_type", str "XenAPI.VM")
    ; ("async_task", `Bool false)
    ; ("void_output", `Bool true)
    ; ("class_decl", str "SetXenVM")
    ; ("params", str get_params)
    ; ("async_param", str "")
    ; ( "message_params"
      , str
          {|
        [Parameter]
        public string NameLabel { get; set; }
|}
      )
    ; ("local_var", str "vm")
    ; ("property", str "VM")
    ; ("cmdlet_methods", str "ProcessRecordNameLabel(vm);")
    ; ("passthru", str "if (PassThru)\n                WriteObject(obj, true);")
    ; ( "parse_method"
      , str
          {|
        private string ParseVM()
        {
            return Ref.opaque_ref;
        }
|}
      )
    ; ( "process_methods"
      , str
          {|
        private void ProcessRecordNameLabel(string vm)
        {
            RunApiCall(() => XenAPI.VM.set_name_label(session, vm, NameLabel));
        }
|}
      )
    ]

(* Invoke-XenObject.mustache, as built by [gen_invoker]. This is the dynamic-
   parameter shape: the cmdlet carries an enum of actions plus a generated
   IXenServerDynamicParameter class. *)
let invoke_xen_object =
  `O
    [
      ("licence", str licence)
    ; ("verb_expr", str "VerbsLifecycle.Invoke")
    ; ("noun", str "VM")
    ; ("should_process", str "true")
    ; ("class_decl", str "InvokeXenVM")
    ; ("class", str "VM")
    ; ("enum_kind", str "Action")
    ; ("open_brace", str "{")
    ; ( "passthru_param"
      , str
          {|
        [Parameter]
        public SwitchParameter PassThru { get; set; }
|}
      )
    ; ("params", str get_params)
    ; ( "dynamic_generator"
      , str
          {|
        public override object GetDynamicParameters()
        {
            switch (XenAction)
            {
                case XenVMAction.Clone:
                    _context = new XenVMActionCloneDynamicParameters();
                    return _context;
                default:
                    return null;
            }
        }
|}
      )
    ; ("local_var", str "vm")
    ; ("property", str "VM")
    ; ( "cmdlet_methods_dynamic"
      , str
          {|
                case XenVMAction.Clone:
                    ProcessRecordClone(vm);
                    break;
|}
      )
    ; ( "parse_method"
      , str
          {|
        private string ParseVM()
        {
            return Ref.opaque_ref;
        }
|}
      )
    ; ( "process_methods"
      , str
          {|
        private void ProcessRecordClone(string vm)
        {
            RunApiCall(() => XenAPI.VM.clone(session, vm, NewName));
        }
|}
      )
    ; ("messages_enum", str "\n        Clone,\n        Destroy")
    ; ( "dynamic_params"
      , str
          {|
    public class XenVMActionCloneDynamicParameters : IXenServerDynamicParameter
    {
        [Parameter]
        public string NewName { get; set; }
    }
|}
      )
    ]

(* ------------------------------------------------------------------ *)
(* Golden-file tests                                                   *)
(* ------------------------------------------------------------------ *)

let cases =
  [
    ( "Get-XenObject.mustache, with uuid and name"
    , "Get-XenObject.mustache"
    , get_xen_object
    , "get_xen_object.cs"
    )
  ; ( "Get-XenObject.mustache, without uuid or name"
    , "Get-XenObject.mustache"
    , get_xen_object_minimal
    , "get_xen_object_minimal.cs"
    )
  ; ( "New-XenObject.mustache"
    , "New-XenObject.mustache"
    , new_xen_object
    , "new_xen_object.cs"
    )
  ; ( "Set-XenObject.mustache"
    , "Set-XenObject.mustache"
    , set_xen_object
    , "set_xen_object.cs"
    )
  ; ( "Invoke-XenObject.mustache"
    , "Invoke-XenObject.mustache"
    , invoke_xen_object
    , "invoke_xen_object.cs"
    )
  ]

module TemplatesTest = Test_highlevel.Generic.MakeStateless (struct
  module Io = struct
    type input_t = string * Mustache.Json.t

    type output_t = string

    let string_of_input_t (template, _) = "template " ^ template

    let string_of_output_t = Test_printers.string
  end

  let transform (template, json) = render template json

  let tests =
    `Documented
      (List.map
         (fun (doc, template, json, golden_file) ->
           let rendered = render template json in
           (doc, `Quick, (template, json), golden golden_file rendered)
         )
         cases
      )
end)

(* ------------------------------------------------------------------ *)
(* Invariants that the golden files alone would not pin down           *)
(* ------------------------------------------------------------------ *)

module InvariantsTest = struct
  let contains needle haystack =
    let nl = String.length needle and hl = String.length haystack in
    let rec go i =
      i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1))
    in
    nl = 0 || go 0

  (* The generator loads templates with [string_of_file], which joins lines
     with "\n" and so discards the final newline. Every template therefore has
     to end with two newlines for the generated file to end with one. Losing
     that shows up as a whole-file diff in the SDK output. *)
  let test_single_trailing_newline () =
    List.iter
      (fun (doc, template, json, _) ->
        let rendered = render template json in
        Alcotest.(check bool)
          (doc ^ ": ends with a newline")
          true
          (String.length rendered > 0
          && rendered.[String.length rendered - 1] = '\n'
          ) ;
        Alcotest.(check bool)
          (doc ^ ": does not end with a blank line")
          false
          (String.length rendered > 1
          && rendered.[String.length rendered - 2] = '\n'
          )
      )
      cases

  (* A literal '{' immediately before an interpolation collides with the
     '{{{' delimiter, so the templates pass the brace in as a variable. If that
     regresses, the brace or the interpolation is silently lost. *)
  let test_open_brace_interpolation () =
    let rendered = render "Invoke-XenObject.mustache" invoke_xen_object in
    (* Match the brace together with the fragment that must follow it: the
       injected fragments contain switch blocks of their own, so looking for
       the brace alone would match one of those instead. *)
    Alcotest.(check bool)
      "ProcessRecord switch opens with a brace, then the cases" true
      (contains
         "switch (XenAction)\n\
         \            {\n\
         \                case XenVMAction.Clone:\n\
         \                    ProcessRecordClone(vm);"
         rendered
      ) ;
    Alcotest.(check bool)
      "enum block opens with a brace, then the members" true
      (contains "public enum XenVMAction\n    {\n        Clone," rendered) ;
    Alcotest.(check bool)
      "no unrendered mustache tags remain" false (contains "{{" rendered)

  let tests =
    [
      ("single_trailing_newline", `Quick, test_single_trailing_newline)
    ; ("open_brace_interpolation", `Quick, test_open_brace_interpolation)
    ]
end

let tests =
  Test_highlevel.make_suite "gen_powershell_binding_"
    [("templates", TemplatesTest.tests); ("invariants", InvariantsTest.tests)]

let () = Alcotest.run "Gen PowerShell binding" tests
