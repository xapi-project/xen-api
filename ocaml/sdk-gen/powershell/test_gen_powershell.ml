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
 * gen_message_family, gen_invoker, gen_help), so a template that stops
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

(* PowerShellHelp.mustache, as built by [gen_help]. Two commands: a getter with
   two parameter sets and examples, and a dynamic-parameter cmdlet with an enum
   value group and a parameter that is documented but kept out of the syntax. *)
let help_param ?(required = "false") ?(pipeline = "false") ?(position = "Named")
    ?(switch = false) ?(values = []) ?(aliases = "") name typ description =
  `O
    [
      ("name", str name)
    ; ("type", str typ)
    ; ("description", str description)
    ; ("required", str required)
    ; ("pipeline", str pipeline)
    ; ("position", str position)
    ; ("switch", `Bool switch)
    ; ( "default_value"
      , str
          ( if switch then
              "False"
            else
              "None"
          )
      )
    ; ("aliases", str aliases)
    ; ("has_values", `Bool (values <> []))
    ; ("values", `A (List.map (fun v -> `O [("value", str v)]) values))
    ]

let syntax_item cmdlet set_name parameters =
  `O
    [
      ("cmdlet", str cmdlet)
    ; ("set_name", str set_name)
    ; ("parameters", `A parameters)
    ]

let p_ref =
  help_param ~pipeline:"true (ByPropertyName)" ~position:"0"
    ~aliases:"opaque_ref" "Ref" "XenRef&lt;XenAPI.VM&gt;"
    "The VM object to operate on, specified by its opaque reference."

let p_uuid =
  help_param ~required:"true" ~pipeline:"true (ByValue)" ~position:"0" "Uuid"
    "Guid" "The UUID of the VM to operate on."

let p_besteffort =
  help_param ~switch:true "BestEffort" "SwitchParameter"
    "If set, the cmdlet keeps going after a per-object failure."

let p_session =
  help_param "SessionOpaqueRef" "String"
    "The session object on which to run the cmdlet."

let p_action =
  help_param ~required:"true"
    ~values:["Snapshot"; "Clone"; "Destroy"]
    "XenAction" "XenVMAction" "Selects which action to use."

(* Documented, but deliberately absent from the syntax items below. *)
let p_dynamic =
  help_param "NewName" "string"
    "The name of the new VM. Accepted when -XenAction is Snapshot, Clone."

let powershell_help =
  `O
    [
      ( "commands"
      , `A
          [
            `O
              [
                ("name", str "Get-XenVM")
              ; ("verb", str "Get")
              ; ("noun", str "XenVM")
              ; ("synopsis", str "Gets the XenServer VM objects.")
              ; ( "description"
                , `A
                    [
                      `O [("para", str "Returns the VM objects on the server.")]
                    ; `O
                        [
                          ( "para"
                          , str
                              "A VM is a virtual machine, or a template from \
                               which one can be created."
                          )
                        ]
                    ]
                )
              ; ( "syntax"
                , `A
                    [
                      syntax_item "Get-XenVM" "Ref"
                        [p_ref; p_session; p_besteffort]
                    ; syntax_item "Get-XenVM" "Uuid"
                        [p_uuid; p_session; p_besteffort]
                    ]
                )
              ; ("parameters", `A [p_ref; p_uuid; p_session; p_besteffort])
              ; ("has_inputs", `Bool true)
              ; ( "inputs"
                , `A
                    [`O [("type", str "XenRef[VM]")]; `O [("type", str "guid")]]
                )
              ; ("has_outputs", `Bool true)
              ; ("outputs", `A [`O [("type", str "VM[]")]])
              ; ("has_examples", `Bool true)
              ; ( "examples"
                , `A
                    [
                      `O
                        [
                          ("title", str "-------- Example 1 --------")
                        ; ("code", str "Get-XenVM -Name &quot;my vm&quot;")
                        ; ( "remarks"
                          , str "Gets the VM whose name_label is 'my vm'."
                          )
                        ]
                    ]
                )
              ]
          ; `O
              [
                ("name", str "Invoke-XenVM")
              ; ("verb", str "Invoke")
              ; ("noun", str "XenVM")
              ; ("synopsis", str "Invokes an operation on a VM object.")
              ; ( "description"
                , `A [`O [("para", str "Invokes an operation on a VM object.")]]
                )
              ; ( "syntax"
                , `A [syntax_item "Invoke-XenVM" "Ref" [p_ref; p_action]]
                )
              ; ("parameters", `A [p_ref; p_action; p_dynamic])
                (* Declares no pipeline input, so the inputTypes block must be
                   gated off, but still declares an output type. *)
              ; ("has_inputs", `Bool false)
              ; ("inputs", `A [])
              ; ("has_outputs", `Bool true)
              ; ("outputs", `A [`O [("type", str "System.Object")]])
              ; ("has_examples", `Bool false)
              ; ("examples", `A [])
              ]
          ]
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
  ; ( "PowerShellHelp.mustache"
    , "PowerShellHelp.mustache"
    , powershell_help
    , "powershell_help.xml"
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

  let count_substring needle haystack =
    let nl = String.length needle and hl = String.length haystack in
    let rec go i acc =
      if i + nl > hl then
        acc
      else if String.sub haystack i nl = needle then
        go (i + nl) (acc + 1)
      else
        go (i + 1) acc
    in
    go 0 0

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

  (* Once external MAML help exists, Get-Help no longer derives the SYNTAX
     section by reflection: if the syntax block goes missing the section
     silently renders empty for every cmdlet. *)
  let test_maml_has_syntax_block () =
    let rendered = render "PowerShellHelp.mustache" powershell_help in
    Alcotest.(check int)
      "one syntax block per command" 2
      (count_substring "<command:syntax>" rendered) ;
    (* One syntaxItem per parameter set, not per command: Get-XenVM is callable
       by Ref or by Uuid, Invoke-XenVM only by Ref. Collapsing these into a
       single item makes Get-Help present mutually exclusive parameters as
       though they could be combined. *)
    Alcotest.(check int)
      "one syntaxItem per parameter set" 3
      (count_substring "<command:syntaxItem>" rendered) ;
    Alcotest.(check int)
      "parameters listed across the syntax items and parameter blocks" 15
      (count_substring "<command:parameter " rendered)

  (* An enum parameter must carry its permitted values, or the syntax degrades
     from "-XenAction {Snapshot | Clone | Destroy}" to a bare type name and the
     caller has no way to discover the actions. *)
  let test_maml_enum_value_group () =
    let rendered = render "PowerShellHelp.mustache" powershell_help in
    (* Once in the syntax item, once in the parameters block. *)
    Alcotest.(check int)
      "value group emitted in both places" 2
      (count_substring "<command:parameterValueGroup>" rendered) ;
    Alcotest.(check int)
      "every permitted value is listed in both" 6
      (count_substring "variableLength=\"false\"" rendered)

  (* Dynamic parameters are documented in the parameters block but left out of
     the syntax: a cmdlet like Invoke-XenVM has dozens across its actions, and
     listing them all would make the syntax unreadable. *)
  let test_maml_dynamic_params_documented_not_in_syntax () =
    let rendered = render "PowerShellHelp.mustache" powershell_help in
    (* Once only: a second occurrence would mean it had leaked into the
       syntax item as well. *)
    Alcotest.(check int)
      "the dynamic parameter is documented exactly once" 1
      (count_substring "<maml:name>NewName</maml:name>" rendered) ;
    Alcotest.(check bool)
      "the dynamic parameter carries the actions it applies to" true
      (contains "Accepted when -XenAction is Snapshot, Clone." rendered)

  (* A switch must not be given a parameter value in the syntax, or Get-Help
     renders it as "-BestEffort <SwitchParameter>", implying it takes an
     argument. *)
  let test_maml_switch_has_no_syntax_value () =
    let rendered = render "PowerShellHelp.mustache" powershell_help in
    Alcotest.(check int)
      "switch declared as SwitchParameter only in the parameters block" 1
      (count_substring
         "<command:parameterValue \
          required=\"false\">SwitchParameter</command:parameterValue>"
         rendered
      )

  (* Aliases survive only through the parameter's aliases attribute. Without
     it the Aliases row renders blank and -Ref stops advertising opaque_ref. *)
  let test_maml_aliases () =
    let rendered = render "PowerShellHelp.mustache" powershell_help in
    Alcotest.(check bool)
      "the declared alias reaches the parameter block" true
      (contains "aliases=\"opaque_ref\"" rendered) ;
    Alcotest.(check bool)
      "a parameter without aliases emits an empty attribute" true
      (contains "aliases=\"\"" rendered)

  (* External help replaces the reflected INPUTS and OUTPUTS too, so the file
     has to carry them: without inputTypes and returnValues both sections
     render empty, losing the pipeline types and the cmdlet's output type. *)
  let test_maml_inputs_and_outputs () =
    let rendered = render "PowerShellHelp.mustache" powershell_help in
    Alcotest.(check int)
      "inputTypes emitted for the command that declares them" 1
      (count_substring "<command:inputTypes>" rendered) ;
    Alcotest.(check int)
      "one inputType per pipeline type" 2
      (count_substring "<command:inputType>" rendered) ;
    Alcotest.(check int)
      "returnValues emitted for both commands" 2
      (count_substring "<command:returnValues>" rendered) ;
    Alcotest.(check bool)
      "the output type is carried through" true
      (contains "<maml:name>VM[]</maml:name>" rendered) ;
    (* Gated the same way the examples block is: the second command declares no
       pipeline input, and must not emit an empty inputTypes element. *)
    Alcotest.(check int)
      "no stray empty inputTypes" 1
      (count_substring "</command:inputTypes>" rendered)

  (* has_examples gates the examples block: cmdlets without examples must not
     emit an empty <command:examples/>, which some help renderers show as a
     stray empty EXAMPLES heading. *)
  let test_maml_examples_are_gated () =
    let rendered = render "PowerShellHelp.mustache" powershell_help in
    Alcotest.(check int)
      "only the command with examples emits the block" 1
      (count_substring "<command:examples>" rendered)

  (* Descriptions and parameter values reach the template already escaped by
     escape_xml; the template must interpolate with {{{ }}} so mustache does
     not escape them a second time. *)
  let test_maml_no_double_escaping () =
    let rendered = render "PowerShellHelp.mustache" powershell_help in
    Alcotest.(check bool)
      "generic parameter type survives intact" true
      (contains "XenRef&lt;XenAPI.VM&gt;" rendered) ;
    Alcotest.(check bool)
      "entities are not escaped twice" false
      (contains "&amp;lt;" rendered)

  let tests =
    [
      ("single_trailing_newline", `Quick, test_single_trailing_newline)
    ; ("open_brace_interpolation", `Quick, test_open_brace_interpolation)
    ; ("maml_has_syntax_block", `Quick, test_maml_has_syntax_block)
    ; ("maml_enum_value_group", `Quick, test_maml_enum_value_group)
    ; ( "maml_dynamic_params_documented_not_in_syntax"
      , `Quick
      , test_maml_dynamic_params_documented_not_in_syntax
      )
    ; ( "maml_switch_has_no_syntax_value"
      , `Quick
      , test_maml_switch_has_no_syntax_value
      )
    ; ("maml_aliases", `Quick, test_maml_aliases)
    ; ("maml_inputs_and_outputs", `Quick, test_maml_inputs_and_outputs)
    ; ("maml_examples_are_gated", `Quick, test_maml_examples_are_gated)
    ; ("maml_no_double_escaping", `Quick, test_maml_no_double_escaping)
    ]
end

(* The entries in curated_examples.ml are written by hand, so they get the same
   scrutiny as generated output.

   These checks need nothing but the module itself -- no compiled PowerShell
   module, no host to talk to -- so they run with the rest of the unit tests.
   They cover the part that can be settled from the file alone: that an entry
   is well formed, and that it is filed under the cmdlet it actually shows.
   Whether the cmdlet still exists is settled by gen_powershell_binding, which
   refuses to build on an orphaned entry; whether the parameters and enum
   values still exist is settled by verify-help.ps1 against the built module in
   CI. *)
module CuratedExamplesTest = struct
  let examples = Curated_examples.examples

  let contains needle haystack =
    let nl = String.length needle and hl = String.length haystack in
    let rec go i =
      i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1))
    in
    nl = 0 || go 0

  (* Walk every (cmdlet, title, code, explanation), tagging failures with the
     cmdlet so a broken entry is named rather than merely counted. *)
  let iter_examples f =
    List.iter
      (fun (cmdlet, es) ->
        List.iter (fun (title, code, expl) -> f cmdlet title code expl) es
      )
      examples

  let lines s =
    String.split_on_char '\n' s |> List.filter (fun l -> String.trim l <> "")

  let test_each_cmdlet_has_an_example () =
    List.iter
      (fun (cmdlet, es) ->
        Alcotest.(check bool)
          (cmdlet ^ ": listed, so must carry at least one example")
          true (es <> [])
      )
      examples

  let test_no_duplicate_cmdlets () =
    let names = List.map fst examples in
    let sorted = List.sort_uniq String.compare names in
    Alcotest.(check int)
      "each cmdlet appears once; a second entry would silently win"
      (List.length names) (List.length sorted)

  (* All three parts reach the reader: the title becomes the heading Get-Help
     prints, the code dev:code and the explanation dev:remarks. An empty one
     renders as a blank example. *)
  let test_all_parts_present () =
    iter_examples (fun cmdlet title code expl ->
        Alcotest.(check bool)
          (cmdlet ^ ": example has a title")
          true
          (String.trim title <> "") ;
        Alcotest.(check bool)
          (cmdlet ^ ": example code is not blank")
          true
          (String.trim code <> "") ;
        Alcotest.(check bool)
          (cmdlet ^ ": example has an explanation")
          true
          (String.trim expl <> "")
    )

  (* Titles are headings, not sentences: "Start a VM", not "Starts the VM." *)
  let test_titles_read_as_headings () =
    iter_examples (fun cmdlet title _ _ ->
        let t = String.trim title in
        Alcotest.(check bool)
          (cmdlet ^ ": title opens with a capital")
          true
          (t <> "" && t.[0] = Char.uppercase_ascii t.[0]) ;
        Alcotest.(check bool)
          (cmdlet ^ ": title does not end with a full stop")
          false
          (t <> "" && t.[String.length t - 1] = '.') ;
        (* Get-Help pads the heading with 26 dashes either side, so a long
           title wraps and the separator stops looking like one. Terse titles
           read better anyway. *)
        Alcotest.(check bool)
          (cmdlet ^ ": title is short enough not to wrap: " ^ t)
          true
          (String.length t <= 44)
    )

  (* Get-Help renders examples verbatim, so the layout has to carry the
     meaning: a prompt opens each statement, and a statement continued over
     several lines is indented instead. verify-help.ps1 strips the prompts on
     exactly that rule before parsing the example, so a continuation that
     wrongly carries one stops being part of the statement above it. *)
  let test_prompts_open_statements () =
    iter_examples (fun cmdlet _ code _ ->
        let ls = lines code in
        ( match ls with
        | first :: _ ->
            Alcotest.(check bool)
              (cmdlet ^ ": example opens at a PS> prompt")
              true
              (String.starts_with ~prefix:"PS> " first)
        | [] ->
            ()
        ) ;
        List.iter
          (fun line ->
            Alcotest.(check bool)
              (cmdlet
              ^ ": line is a prompt or an indented continuation: "
              ^ line
              )
              true
              (String.starts_with ~prefix:"PS> " line
              || String.starts_with ~prefix:"    " line
              )
          )
          ls
    )

  (* An example filed under the wrong cmdlet is worse than no example: it is
     shown under a heading it does not illustrate. *)
  let test_example_invokes_its_own_cmdlet () =
    iter_examples (fun cmdlet _ code _ ->
        Alcotest.(check bool)
          (cmdlet ^ ": example actually invokes " ^ cmdlet)
          true (contains cmdlet code)
    )

  (* Not a PowerShell parse -- that is verify-help.ps1's job against the built
     module -- but unbalanced delimiters are the way a hand-edited snippet
     usually breaks, and they are cheap to catch here. *)
  let test_snippets_are_balanced () =
    let count c s =
      String.fold_left
        (fun n ch ->
          if ch = c then
            n + 1
          else
            n
        )
        0 s
    in
    iter_examples (fun cmdlet _ code _ ->
        let balanced l r = count l code = count r code in
        Alcotest.(check bool)
          (cmdlet ^ ": double quotes are balanced")
          true
          (count '"' code mod 2 = 0) ;
        Alcotest.(check bool)
          (cmdlet ^ ": parentheses are balanced")
          true (balanced '(' ')') ;
        Alcotest.(check bool)
          (cmdlet ^ ": braces are balanced")
          true (balanced '{' '}') ;
        Alcotest.(check bool)
          (cmdlet ^ ": brackets are balanced")
          true (balanced '[' ']')
    )

  (* The explanations sit together in one EXAMPLES block, so they should read
     alike: a sentence, not a fragment. *)
  let test_explanations_read_as_sentences () =
    iter_examples (fun cmdlet _ _ expl ->
        let e = String.trim expl in
        Alcotest.(check bool)
          (cmdlet ^ ": explanation opens with a capital")
          true
          (e <> "" && e.[0] = Char.uppercase_ascii e.[0]) ;
        Alcotest.(check bool)
          (cmdlet ^ ": explanation ends with a full stop")
          true
          (e <> "" && e.[String.length e - 1] = '.')
    )

  (* The lookup the generator goes through, rather than the table behind it. *)
  let test_lookup_matches_the_table () =
    List.iter
      (fun (cmdlet, es) ->
        Alcotest.(check int)
          (cmdlet ^ ": for_cmdlet returns its examples")
          (List.length es)
          (List.length (Curated_examples.for_cmdlet cmdlet))
      )
      examples ;
    Alcotest.(check int)
      "an unknown cmdlet has no curated examples" 0
      (List.length (Curated_examples.for_cmdlet "Get-XenNoSuchThing"))

  let tests =
    [
      ("each_cmdlet_has_an_example", `Quick, test_each_cmdlet_has_an_example)
    ; ("no_duplicate_cmdlets", `Quick, test_no_duplicate_cmdlets)
    ; ("all_parts_present", `Quick, test_all_parts_present)
    ; ("titles_read_as_headings", `Quick, test_titles_read_as_headings)
    ; ("prompts_open_statements", `Quick, test_prompts_open_statements)
    ; ( "example_invokes_its_own_cmdlet"
      , `Quick
      , test_example_invokes_its_own_cmdlet
      )
    ; ("snippets_are_balanced", `Quick, test_snippets_are_balanced)
    ; ( "explanations_read_as_sentences"
      , `Quick
      , test_explanations_read_as_sentences
      )
    ; ("lookup_matches_the_table", `Quick, test_lookup_matches_the_table)
    ]
end

let tests =
  Test_highlevel.make_suite "gen_powershell_binding_"
    [
      ("templates", TemplatesTest.tests)
    ; ("invariants", InvariantsTest.tests)
    ; ("curated_examples", CuratedExamplesTest.tests)
    ]

let () = Alcotest.run "Gen PowerShell binding" tests
