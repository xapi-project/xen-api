let local_vdi_migration ?backing_format ~virtual_size ~prepare_vdi
    ~post_migration rpc session_id vm_template
    ((src_sr_info, dst_sr_info) : Qt.sr_info * Qt.sr_info) () =
  let open Client in
  Printf.printf "Testing migration from %s to %s\n"
    (Client.SR.get_name_label ~rpc ~session_id ~self:src_sr_info.sr)
    (Client.SR.get_name_label ~rpc ~session_id ~self:dst_sr_info.sr) ;

  (* Create a VDI on src *)
  Qt.VDI.with_new ~name_label:"[QT] testing migration"
    ~name_description:__FILE__ ~virtual_size ?backing_format rpc session_id
    src_sr_info.sr
  @@ fun vdi ->
  prepare_vdi vdi ;

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
              ~options:[("dest-img-format", "vhd")]
          in

          final_vdi := migrated_vdi ;

          let new_sr = Client.VDI.get_SR ~rpc ~session_id ~self:migrated_vdi in
          let actual = Client.SR.get_uuid ~rpc ~session_id ~self:new_sr in
          let expected =
            Client.SR.get_uuid ~rpc ~session_id ~self:dst_sr_info.sr
          in

          Alcotest.(check string)
            "VDI migrated to destination SR" expected actual ;

          post_migration migrated_vdi
      )
    )
    (fun () ->
      try Client.VDI.destroy ~rpc ~session_id ~self:!final_vdi with _ -> ()
    )

let tests () =
  let prepare_vdi = fun _ -> () in
  let post_migration = fun _ -> () in
  let open Qt_filter in
  [
    [
      ( "SMAPIv1 migration test"
      , `Slow
      , local_vdi_migration ~backing_format:"vhd" ~virtual_size:2097152L
          ~prepare_vdi ~post_migration
      )
    ]
    |> conn
    |> vm_template Qt.VM.Template.other
    |> migration_path SR.smapiv1_mig
  ; [
      ( "SMAPIv3 migration test"
      , `Slow
      , local_vdi_migration ~backing_format:"vhd" ~virtual_size:2097152L
          ~prepare_vdi ~post_migration
      )
    ]
    |> conn
    |> vm_template Qt.VM.Template.other
    |> migration_path SR.smapiv3_mig
  ]
  |> List.concat
