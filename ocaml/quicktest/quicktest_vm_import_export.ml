
let export_filename = "/tmp/quicktest-export"

let name_of_sr rpc session_id self =
  Client.Client.SR.get_name_label ~rpc ~session_id ~self

let vm_import ?(metadata_only=false) ?(preserve=false) ?sr rpc session_id filename =
  let sr_uuid = Xapi_stdext_monadic.Opt.map (fun sr -> Client.Client.SR.get_uuid rpc session_id sr) sr in
  let args = [ "vm-import"; "filename=" ^ filename ] in
  let args = args @ (Xapi_stdext_monadic.Opt.default [] (Xapi_stdext_monadic.Opt.map (fun x -> [ "sr-uuid=" ^ x ]) sr_uuid)) in
  let args = if metadata_only then args @ [ "metadata=true" ] else args in
  let args = if preserve then args @ [ "preserve=true" ] else args in
  let newvm_uuids = String.split_on_char ',' (Qt.cli_cmd args) in
  List.map (fun uuid -> Client.Client.VM.get_by_uuid rpc session_id uuid) newvm_uuids

let vm_export ?(metadata_only=false) rpc session_id vm filename =
  let uuid = Client.Client.VM.get_uuid rpc session_id vm in
  let args = [ "vm-export"; "vm=" ^ uuid; "filename=" ^ filename ] in
  let args = if metadata_only then args @ [ "metadata=true" ] else args in
  ignore(Qt.cli_cmd args)

let vm_uninstall rpc session_id vm =
  let uuid = Client.Client.VM.get_uuid rpc session_id vm in
  ignore(Qt.cli_cmd [ "vm-uninstall"; "uuid=" ^ uuid; "--force" ])

(** Set up export test: create a small VM with a selection of CDs, empty drives, "iso" Disks etc *)
let with_setup rpc session_id sr vm_template f =
  print_endline "Setting up test VM";
  let uuid = Client.Client.VM.get_uuid rpc session_id vm_template in
  print_endline (Printf.sprintf "Template has uuid: %s%!" uuid);
  Qt.VM.with_new rpc session_id ~template:vm_template
    (fun vm ->
      print_endline (Printf.sprintf "Installed new VM");
      let cd =
        let tools_iso_filter = "field \"is_tools_iso\"=\"true\"" in
        match Client.Client.VDI.get_all_records_where rpc session_id tools_iso_filter with
        | (vdi, _)::_ -> vdi
        | [] ->
          Alcotest.fail "setup_export_test_vm: Failed to find tools ISO VDI";
      in
      print_endline (Printf.sprintf "Using SR: %s" (name_of_sr rpc session_id sr));
      let vdi = Client.Client.VDI.create rpc session_id "small"
          "description" sr 4194304L `user false false [] [] [] [] in
      ignore(Client.Client.VBD.create ~rpc ~session_id ~vM:vm ~vDI:cd ~userdevice:"0" ~bootable:false
               ~mode:`RO ~_type:`CD ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[]);
      ignore(Client.Client.VBD.create ~rpc ~session_id ~vM:vm ~vDI:cd ~userdevice:"1" ~bootable:false
               ~mode:`RO ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[]);
      ignore(Client.Client.VBD.create ~rpc ~session_id ~vM:vm ~vDI:cd ~userdevice:"2" ~bootable:false
               ~mode:`RO ~_type:`CD ~unpluggable:true ~empty:true ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[]);
      ignore(Client.Client.VBD.create ~rpc ~session_id ~vM:vm ~vDI:vdi ~userdevice:"3" ~bootable:false
               ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[Xapi_globs.owner_key,""]
               ~qos_algorithm_type:"" ~qos_algorithm_params:[]);
      f vm
    )

let import_export_test rpc session_id sr_info vm_template () =
  let sr = sr_info.Qt.sr in
  with_setup rpc session_id sr vm_template
    (fun vm ->
       let by_device = List.map (fun vbd -> Client.Client.VBD.get_userdevice rpc session_id vbd, vbd) (Client.Client.VM.get_VBDs rpc session_id vm) in

       Xapi_stdext_unix.Unixext.unlink_safe export_filename;
       vm_export rpc session_id vm export_filename;
       let all_srs = Qt_filter.SR.(list_srs (all |> not_iso |> (allowed_operations [`vdi_create]))) in
       List.iter
         (fun sr_info ->
            let sr = sr_info.Qt.sr in
            print_endline (Printf.sprintf "Attempting import to SR: %s" (name_of_sr rpc session_id sr));
            let vm' = List.hd (vm_import ~sr rpc session_id export_filename) in
            let vbds = Client.Client.VM.get_VBDs rpc session_id vm' in

            if List.length vbds <> (List.length by_device) then Alcotest.fail "Wrong number of VBDs after import";
            List.iter (fun vbd ->
                let all = Client.Client.VBD.get_record rpc session_id vbd in
                let orig_vbd = List.assoc all.API.vBD_userdevice by_device in
                let orig_vbd = Client.Client.VBD.get_record rpc session_id orig_vbd in

                (* type, empty should match *)
                if all.API.vBD_type <> orig_vbd.API.vBD_type
                then Alcotest.fail (Printf.sprintf "Device %s varies in type" all.API.vBD_userdevice);
                if all.API.vBD_empty <> orig_vbd.API.vBD_empty
                then Alcotest.fail (Printf.sprintf "Device %s varies in emptiness" all.API.vBD_userdevice);
                match all.API.vBD_userdevice with
                | "0" | "1" | "2" ->
                  (* VDI should be the same *)
                  if all.API.vBD_VDI <> orig_vbd.API.vBD_VDI
                  then Alcotest.fail
                      (Printf.sprintf "Device %s varies in VDIness (original = %s; new = %s)"
                         all.API.vBD_userdevice
                         (Client.Client.VDI.get_uuid rpc session_id orig_vbd.API.vBD_VDI)
                         (Client.Client.VDI.get_uuid rpc session_id all.API.vBD_VDI)
                      )
                | "3" ->
                  (* VDI should be different *)
                  if all.API.vBD_VDI = orig_vbd.API.vBD_VDI
                  then Alcotest.fail (Printf.sprintf "Device %s should not vary in VDIness" all.API.vBD_userdevice)
                | _ -> Alcotest.fail (Printf.sprintf "Unhandled device number: %s" all.API.vBD_userdevice)) vbds;
            vm_uninstall rpc session_id vm'
         ) all_srs;
       Unix.unlink export_filename
    )

let tests () =
  let open Qt_filter in
  [ ["import_export_test", `Slow, import_export_test] |> conn |> sr SR.(all |> allowed_operations [`vdi_create]) |> vm_template Qt.VM.Template.other
  ]
  |> List.concat

