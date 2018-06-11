
(* ---------------- *
    Helper functions
 * ---------------- *)

let with_open_vdi _rpc session_id vdi mode f =
  Storage_test.VDI.with_attached session_id vdi mode
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

let checksum _rpc session_id vdi =
  Storage_test.VDI.with_attached session_id vdi `RO (fun path ->
      Digest.to_hex (Digest.file path)
    )

let check_vdi_unchanged session_id ~prepare_vdi ~vdi_op sr_info =
  let rpc = !Quicktest_common.rpc in
  let sR = sr_info.Storage_test.sr in
  Storage_test.VDI.with_new session_id sR (fun vdi ->
      prepare_vdi vdi;
      let checksum_original = checksum rpc session_id vdi in
      let new_vdi = vdi_op rpc session_id sR vdi in
      Storage_test.VDI.with_destroyed session_id new_vdi (fun () ->
          let checksum_copy = checksum rpc session_id new_vdi in
          if (checksum_copy <> checksum_original) then
            failwith (Printf.sprintf "New VDI (checksum: %s) has different data than original (checksum: %s)." checksum_copy checksum_original)
        )
    )

let copy_vdi rpc session_id sr vdi =
  Client.Client.VDI.copy ~rpc ~session_id ~vdi ~base_vdi:API.Ref.null ~into_vdi:API.Ref.null ~sr

let export_import_vdi rpc session_id ~exportformat sR vdi =
  let vdi_uuid = Client.Client.VDI.get_uuid ~rpc ~session_id ~self:vdi in
  let file = "/tmp/quicktest_export_"^vdi_uuid in
  Quicktest_common.cli_cmd ["vdi-export"; "uuid="^vdi_uuid; "filename="^file; "format="^exportformat] |> ignore;
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
       let virtual_size = Client.Client.VDI.get_virtual_size ~rpc ~session_id ~self:vdi in
       let new_vdi = Client.Client.VDI.create ~rpc ~session_id ~name_label:"" ~name_description:"" ~sR ~virtual_size ~_type:`user ~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in
       let new_vdi_uuid = Client.Client.VDI.get_uuid ~rpc ~session_id ~self:new_vdi in
       Quicktest_common.cli_cmd ["vdi-import"; "uuid="^new_vdi_uuid; "filename="^file; "format="^exportformat] |> ignore;
       new_vdi
    )
    (fun () -> Sys.remove file)

let export_import_raw = export_import_vdi ~exportformat:"raw"
let export_import_vhd = export_import_vdi ~exportformat:"vhd"

let data_integrity_tests session_id vdi_op op_name =
  let module F = Storage_test.Sr_filter in
  let filter = F.(allowed_operations [`vdi_create; `vdi_destroy] ||> not_iso) in
  [ op_name^": empty VDI", `Slow, check_vdi_unchanged ~prepare_vdi:(fun _vdi -> ()) ~vdi_op, filter
  ; op_name^": random VDI", `Slow, check_vdi_unchanged ~prepare_vdi:(write_random_data session_id) ~vdi_op, filter
  ; op_name^": full VDI", `Slow, check_vdi_unchanged ~prepare_vdi:(fill session_id) ~vdi_op, filter
  ]

let tests session_id =
  (data_integrity_tests session_id copy_vdi "VDI.copy") @
  (data_integrity_tests session_id export_import_raw "VDI export/import to/from raw file") @
  (data_integrity_tests session_id export_import_vhd "VDI export/import to/from VHD file")
  |> Storage_test.get_test_cases session_id
