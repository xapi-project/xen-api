let local_vdi_migration rpc session_id vm_template
    ((src_sr_info, dst_sr_info) : Qt.sr_info * Qt.sr_info) () =
  let open Client in
  Printf.printf "Testing migration from %s to %s\n"
    (Client.SR.get_name_label ~rpc ~session_id ~self:src_sr_info.sr)
    (Client.SR.get_name_label ~rpc ~session_id ~self:dst_sr_info.sr) ;

  (* Create a VDI on src *)
  let vdi =
    Client.VDI.create ~rpc ~session_id ~name_label:"[QT] testing migration"
      ~name_description:__FILE__ ~sR:src_sr_info.sr ~virtual_size:2097152L
      ~_type:`user ~sharable:false ~read_only:false ~other_config:[]
      ~xenstore_data:[] ~sm_config:[] ~tags:[]
  in

  (* Track which VDI must be destroyed at cleanup time. After migration, ownership
     moves from the original VDI to the migrated one. *)
  let final_vdi = ref vdi in

  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      (* We can now create the VM and perform the migration *)
      Qt.VM.with_new rpc session_id ~template:vm_template (fun vm ->
          (* Attach the VDI *)
          Client.VBD.create ~rpc ~session_id ~vM:vm ~vDI:vdi ~device:""
            ~userdevice:"0" ~bootable:false ~mode:`RW ~_type:`Disk
            ~unpluggable:true ~empty:false ~other_config:[]
            ~qos_algorithm_type:"" ~qos_algorithm_params:[]
            ~currently_attached:true
          |> ignore ;

          let migrated_vdi =
            Client.VDI.pool_migrate ~rpc ~session_id ~vdi ~sr:dst_sr_info.sr
              ~dest_img_format:"" ~options:[]
          in

          final_vdi := migrated_vdi ;

          let new_sr = Client.VDI.get_SR ~rpc ~session_id ~self:migrated_vdi in
          let actual = Client.SR.get_uuid ~rpc ~session_id ~self:new_sr in
          let expected =
            Client.SR.get_uuid ~rpc ~session_id ~self:dst_sr_info.sr
          in

          Alcotest.(check string)
            "VDI migrated to destination SR" expected actual
      )
    )
    (fun () ->
      try Client.VDI.destroy ~rpc ~session_id ~self:!final_vdi with _ -> ()
    )

let tests () =
  let smapiv1_mig =
    Qt_filter.SR.(
      all
      |> not_iso
      |> smapiv1
      |> allowed_operations [`vdi_mirror; `vdi_snapshot]
    )
  in
  let smapiv3_mig =
    Qt_filter.SR.(
      all
      |> not_iso
      |> smapiv3
      |> allowed_operations [`vdi_mirror; `vdi_snapshot]
    )
  in

  let open Qt_filter in
  [
    [("SMAPIv1 migration test", `Slow, local_vdi_migration)]
    |> conn
    |> vm_template Qt.VM.Template.other
    |> migration_path smapiv1_mig
  ; [("SMAPIv3 migration test", `Slow, local_vdi_migration)]
    |> conn
    |> vm_template Qt.VM.Template.other
    |> migration_path smapiv3_mig
  ]
  |> List.concat
