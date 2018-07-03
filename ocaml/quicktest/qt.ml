
type rpc = Rpc.call -> Rpc.response

type sr_info =
  { sr: API.ref_SR
  ; allowed_operations: API.storage_operations_set
  ; capabilities: string list
  ; required_sm_api_version : string
  }

let init_session rpc username password =
  Client.Client.Session.login_with_password ~rpc ~uname:username ~pwd:password ~version:Datamodel_common.api_version_string ~originator:"quick_test"

let get_pool rpc session_id =
  match Client.Client.Pool.get_all rpc session_id with
  | [pool] -> pool
  | _ -> failwith "Number of pools isn't zero!"

let http request f =
  let open Xmlrpc_client in
  let transport =
    if !Quicktest_args.using_unix_domain_socket
    then Unix Xapi_globs.unix_domain_socket
    else SSL(SSL.make ~use_fork_exec_helper:false (), !Quicktest_args.host, 443) in
  with_transport transport (with_http request f)

let cli_cmd args =
  print_endline (String.concat " " ("$ xe" :: args));
  try
    let output = Xapi_stdext_std.Xstringext.String.rtrim (fst(Forkhelpers.execute_command_get_output !Quicktest_args.xe_path args)) in
    print_endline output;
    output
  with
  | Forkhelpers.Spawn_internal_error(log, output, Unix.WEXITED n) ->
    Alcotest.failf "CLI failed: exit code=%d output=[%s] log=[%s]" n output log
  | Forkhelpers.Spawn_internal_error(log, output, _) ->
    Alcotest.failf "CLI failed: exit code unkown; output=[%s] log=[%s]" output log
  | e ->
    Alcotest.fail ("CLI failed" ^ (Printexc.to_string e))

module Test = struct
  let assert_raises_match exception_match fn =
    try
      fn ();
      Alcotest.fail "assert_raises_match: failure expected"
    with failure ->
      if not (exception_match failure)
      then raise failure
      else ()

  (** Check that those fields of two records that are supposed to be the same are
      the same, and the ones that should be different are different. *)
  let compare_fields cls fields original_rec new_rec =
    let check (comparison, field_name, get_field) =
      let original_field = get_field original_rec in
      let new_field = get_field new_rec in
      match comparison with
      | `Same ->
        Alcotest.(check string)
          (Printf.sprintf "%s field %s should be the same" cls field_name)
          original_field new_field
      | `Different ->
        if new_field = original_field then
          Alcotest.failf
            "%s field %s should be different, but is the same: '%s'"
            cls field_name new_field
    in
    List.iter check fields
end

module Time = struct
  type t = float

  let now () = Unix.gettimeofday ()

  let of_field = Xapi_stdext_date.Date.to_float

  let pp t = Xapi_stdext_date.Date.of_float t |> Xapi_stdext_date.Date.to_string

  let check t ~after ~before =
    Alcotest.(check bool)
      (Printf.sprintf "Time %s should be between %s and %s (+-1s)" (pp t) (pp before) (pp after))
      true
      (t > (after -. 1.0) && t < (before +. 1.0))
end

module VM = struct
  module Template = struct
    exception Unable_to_find_suitable_vm_template

    let other = "Other install media"

    let find rpc session_id startswith =
      let vms = Client.Client.VM.get_all rpc session_id in
      match List.filter (fun self ->
          (Xapi_stdext_std.Xstringext.String.startswith startswith (Client.Client.VM.get_name_label rpc session_id self))
          && (Client.Client.VM.get_is_a_template rpc session_id self)
        ) vms with
      | [] -> raise Unable_to_find_suitable_vm_template
      | x :: _ ->
        Printf.printf "Choosing template with name: %s\n" (Client.Client.VM.get_name_label rpc session_id x);
        x
  end

  let install rpc session_id template name =
    let newvm_uuid = cli_cmd [ "vm-install"; "template-uuid=" ^ template; "new-name-label=" ^ name ] in
    Client.Client.VM.get_by_uuid rpc session_id newvm_uuid

  let dom0_of_host rpc session_id host =
    Client.Client.Host.get_control_domain rpc session_id host

  let get_dom0 rpc session_id =
    Xapi_inventory.inventory_filename := "/etc/xensource-inventory";
    let uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in
    Client.Client.VM.get_by_uuid ~rpc ~session_id ~uuid
end

module VDI = struct

  (* This naming is used to identify VDIs to destroy later on *)
  let test_vdi_name_label = "quicktest-vdi_16cd61e7-3f42-4ea7-a1d8-5e9d3f64f25d"
  let test_vdi_name_description = "VDI for storage quicktest"

  let make rpc session_id ?(virtual_size=4194304L) sR =
    Client.Client.VDI.create
      ~sR
      ~session_id
      ~rpc
      ~name_label:test_vdi_name_label
      ~name_description:test_vdi_name_description
      ~_type:`user
      ~sharable:false
      ~read_only:false
      ~virtual_size
      ~xenstore_data:[]
      ~other_config:[]
      ~tags:[]
      ~sm_config:[]

  let with_destroyed rpc session_id self f =
    Xapi_stdext_pervasives.Pervasiveext.finally
      f
      (fun () -> Client.Client.VDI.destroy ~rpc ~session_id ~self)

  let with_new rpc session_id ?(virtual_size=4194304L) sr f =
    let self = make rpc session_id ~virtual_size sr in
    with_destroyed rpc session_id self (fun () -> f self)

  let with_any rpc session_id sr_info f =
    if List.mem `vdi_create sr_info.allowed_operations then begin
      with_new rpc session_id sr_info.sr f
    end else begin
      let self = Client.Client.SR.get_VDIs ~rpc ~session_id ~self:sr_info.sr |> List.hd in
      f self
    end

  let with_attached rpc session_id vdi mode f =
    let dom0 = VM.get_dom0 rpc session_id in
    let vbd =
      Client.Client.VBD.create ~rpc ~session_id
        ~vM:dom0
        ~empty:false
        ~vDI:vdi
        ~userdevice:"autodetect"
        ~bootable:false
        ~mode
        ~_type:`Disk
        ~unpluggable:true
        ~qos_algorithm_type:""
        ~qos_algorithm_params:[]
        ~other_config:[]
    in
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () ->
         Client.Client.VBD.plug ~rpc ~session_id ~self:vbd;
         Xapi_stdext_pervasives.Pervasiveext.finally
           (fun () -> f ("/dev/" ^ (Client.Client.VBD.get_device ~rpc ~session_id ~self:vbd)))
           (fun () ->
              Client.Client.VBD.unplug ~rpc ~session_id ~self:vbd;
           )
      )
      (fun () -> Client.Client.VBD.destroy ~rpc ~session_id ~self:vbd)

  let with_open rpc session_id vdi mode f =
    with_attached rpc session_id vdi mode
      (fun path ->
         let mode' = match mode with
           | `RO -> [ Unix.O_RDONLY ]
           | `RW -> [ Unix.O_RDWR ] in
         let fd = Unix.openfile path mode' 0 in
         Xapi_stdext_pervasives.Pervasiveext.finally
           (fun () -> f fd)
           (fun () -> Unix.close fd)
      )

  let check_fields = Test.compare_fields "VDI"

  let test_update rpc session_id self =
    let original_vdi = Client.Client.VDI.get_record ~rpc ~session_id ~self in
    Client.Client.VDI.update ~rpc ~session_id ~vdi:self;
    let new_vdi = Client.Client.VDI.get_record ~rpc ~session_id ~self in
    let expected =
      [ `Same, "cbt_enabled", (fun vdi -> vdi.API.vDI_cbt_enabled |> string_of_bool)
      ; `Same, "is_a_snapshot", (fun vdi -> vdi.API.vDI_is_a_snapshot |> string_of_bool)
      ; `Same, "location", (fun vdi -> vdi.API.vDI_location)
      ; `Same, "managed", (fun vdi -> vdi.API.vDI_managed |> string_of_bool)
      ; `Same, "name_description", (fun vdi -> vdi.API.vDI_name_description)
      ; `Same, "name_label", (fun vdi -> vdi.API.vDI_name_label)
      ; `Same, "snapshot_of", (fun vdi -> vdi.API.vDI_snapshot_of |> API.Ref.string_of)
      ; `Same, "snapshot_time", (fun vdi -> vdi.API.vDI_snapshot_time |> Xapi_stdext_date.Date.to_string)
      ; `Same, "virtual_size", (fun vdi -> vdi.API.vDI_location)
      ]
    in
    check_fields expected original_vdi new_vdi
end

module SR = struct
  let check_fields = Test.compare_fields "SR"

  let test_update rpc session_id self =
    let original_sr = Client.Client.SR.get_record ~rpc ~session_id ~self in
    Client.Client.SR.update ~rpc ~session_id ~sr:self;
    let new_sr = Client.Client.SR.get_record ~rpc ~session_id ~self in
    let expected =
      [ `Same, "name_description", (fun sr -> sr.API.sR_name_description)
      ; `Same, "name_label", (fun sr -> sr.API.sR_name_label)
      ]
    in
    check_fields expected original_sr new_sr
end
