
(* ---------------- *
    Helper functions
 * ---------------- *)

let get_domain_zero rpc session_id =
  Xapi_inventory.inventory_filename := "/etc/xensource-inventory";
  let uuid = Xapi_inventory.lookup Xapi_inventory._control_domain_uuid in
  Client.Client.VM.get_by_uuid ~rpc ~session_id ~uuid

let with_attached_vdi rpc session_id vdi mode f =
  let dom0 = get_domain_zero rpc session_id in
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

let with_open_vdi rpc session_id vdi mode f =
  with_attached_vdi rpc session_id vdi mode
    (fun path ->
       let mode' = match mode with
         | `RO -> [ Unix.O_RDONLY ]
         | `RW -> [ Unix.O_RDWR ] in
       let fd = Unix.openfile path mode' 0 in
       Xapi_stdext_pervasives.Pervasiveext.finally
         (fun () -> f fd)
         (fun () -> Unix.close fd)
    )

(* ---------------- *
   Integrity tests
 * ---------------- *)

let random_bytes length =
  let buf = Bytes.create length in
  let f = open_in "/dev/urandom" in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
       really_input f buf 0 length;
       buf
    )
    (fun () -> close_in f)

let write_random_data session_id vdi =
  let rpc = !Quicktest_common.rpc in
  let size = Client.Client.VDI.get_virtual_size ~rpc ~session_id ~self:vdi in
  with_open_vdi rpc session_id vdi `RW
    (fun fd ->

       let max_writes = 100 in
       let max_write_length = 2048 in

       let write_random_block offset length =
         let buf = random_bytes length in
         assert (Unix.LargeFile.lseek fd offset Unix.SEEK_SET = offset);
         Unix.write fd buf 0 length |> ignore
       in

       let write_random_extent () =
         let offset = Random.int64 size in
         let to_end = Int64.sub size offset in
         let max_extent_length =
           min (Int64.of_int max_write_length) to_end |> Int64.to_int
         in
         let length = Random.int max_extent_length in

         write_random_block offset length
       in

       let num_extents = Random.int max_writes in
       for i = 1 to num_extents do
         write_random_extent ()
       done
    )

let fill session_id vdi =
  let rpc = !Quicktest_common.rpc in
  let size =
    Client.Client.VDI.get_virtual_size ~rpc ~session_id ~self:vdi
    |> Int64.to_int
  in
  with_open_vdi rpc session_id vdi `RW
    (fun fd ->
       let buf = random_bytes size in
       Unix.write fd buf 0 size
    )

let checksum rpc session_id vdi =
  with_attached_vdi rpc session_id vdi `RO (fun path ->
      Digest.to_hex (Digest.file path)
    )

let check_vdi_unchanged session_id ~prepare_vdi sr_info =
  let rpc = !Quicktest_common.rpc in
  let sR = sr_info.Storage_test.sr in
  let vdi = Client.Client.VDI.create ~rpc ~session_id ~name_label:"" ~name_description:"" ~sR ~virtual_size:4194304L ~_type:`user ~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in
  Storage_test.VDI.with_destroyed session_id vdi (fun () ->
      prepare_vdi vdi;
      let checksum_original = checksum rpc session_id vdi in
      let copy = Client.Client.VDI.copy ~rpc ~session_id
          ~vdi
          ~base_vdi:API.Ref.null
          ~into_vdi:API.Ref.null
          ~sr:sR
      in
      Storage_test.VDI.with_destroyed session_id copy (fun () ->
          let checksum_copy = checksum rpc session_id vdi in
          if (checksum_copy <> checksum_original) then
            failwith (Printf.sprintf "VDI copy (checksum: %s) has different data than original (checksum: %s)." checksum_copy checksum_original)
        )
    )

let tests session_id =
  let module F = Storage_test.Sr_filter in
  let filter = F.(allowed_operations [`vdi_create; `vdi_destroy] ||> not_iso) in
  [ "Copy of empty VDI", `Slow, check_vdi_unchanged ~prepare_vdi:(fun _vdi -> ()), filter
  ; "Copy of random VDI", `Slow, check_vdi_unchanged ~prepare_vdi:(write_random_data session_id), filter
  ; "Copy of full VDI", `Slow, check_vdi_unchanged ~prepare_vdi:(fill session_id), filter
  ]
  |> Storage_test.get_test_cases session_id
