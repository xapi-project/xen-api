(* ---------------- *
   Integrity tests
 * ---------------- *)

let random_bytes length =
  let buf = Bytes.create length in
  let f = open_in "/dev/urandom" in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      really_input f buf 0 length ;
      buf
    )
    (fun () -> close_in f)

let write_random_data rpc session_id vdi =
  let size = Client.Client.VDI.get_virtual_size ~rpc ~session_id ~self:vdi in
  Qt.VDI.with_open rpc session_id vdi `RW (fun fd ->
      let max_writes = 100 in
      let max_write_length = 2048 in
      let write_random_block offset length =
        let buf = random_bytes length in
        assert (Unix.LargeFile.lseek fd offset Unix.SEEK_SET = offset) ;
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
      for _ = 1 to num_extents do
        write_random_extent ()
      done
  )

let fill rpc session_id vdi =
  let size =
    Client.Client.VDI.get_virtual_size ~rpc ~session_id ~self:vdi
    |> Int64.to_int
  in
  let (_ : int) =
    Qt.VDI.with_open rpc session_id vdi `RW (fun fd ->
        let buf = random_bytes size in
        Unix.write fd buf 0 size
    )
  in
  ()

let noop _rpc _session_id _vdi = ()

let checksum rpc session_id vdi =
  Qt.VDI.with_attached rpc session_id vdi `RO (fun path ->
      Digest.to_hex (Digest.file path)
  )

let check_vdi_unchanged rpc session_id ~vdi_size ~prepare_vdi ~vdi_op sr_info ()
    =
  let sR = sr_info.Qt.sr in
  Qt.VDI.with_new ~virtual_size:vdi_size rpc session_id sR (fun vdi ->
      prepare_vdi rpc session_id vdi ;
      let checksum_original = checksum rpc session_id vdi in
      let new_vdi = vdi_op rpc session_id sR vdi in
      Qt.VDI.with_destroyed rpc session_id new_vdi (fun () ->
          let checksum_copy = checksum rpc session_id new_vdi in
          if checksum_copy <> checksum_original then
            failwith
              (Printf.sprintf
                 "New VDI (checksum: %s) has different data than original \
                  (checksum: %s)."
                 checksum_copy checksum_original
              )
      )
  )

let check_vdi_delta rpc session_id ~vdi_size ~prepare_vdi ~prepare_vdi_base
    ~vdi_op sr_info () =
  let sR = sr_info.Qt.sr in
  Qt.VDI.with_new ~virtual_size:vdi_size rpc session_id sR
  @@ fun vdi_original ->
  Qt.VDI.with_new ~virtual_size:vdi_size rpc session_id sR @@ fun base_vdi ->
  prepare_vdi rpc session_id vdi_original ;
  let checksum_original = checksum rpc session_id vdi_original in
  prepare_vdi_base rpc session_id base_vdi ;

  vdi_op rpc session_id ~vdi:vdi_original ~base_vdi ;
  let checksum_copy = checksum rpc session_id base_vdi in
  if checksum_copy <> checksum_original then
    failwith
      (Printf.sprintf
         "New VDI (checksum: %s) has different data than original (checksum: \
          %s)."
         checksum_copy checksum_original
      )

let copy_vdi rpc session_id sr vdi =
  Client.Client.VDI.copy ~rpc ~session_id ~vdi ~base_vdi:API.Ref.null
    ~into_vdi:API.Ref.null ~sr

let export_vdi_to_file ~rpc ~session_id ~exportformat ?base_vdi ~vdi () =
  let get_uuid vdi = Client.Client.VDI.get_uuid ~rpc ~session_id ~self:vdi in
  let vdi_uuid = get_uuid vdi in
  let base_vdi_uuid = Option.map get_uuid base_vdi in
  let file = "/tmp/quicktest_export_" ^ vdi_uuid in
  Qt.cli_cmd
    ([
       "vdi-export"
     ; "uuid=" ^ vdi_uuid
     ; "filename=" ^ file
     ; "format=" ^ exportformat
     ]
    @ match base_vdi_uuid with None -> [] | Some x -> ["base=" ^ x]
    )
  |> ignore ;
  file

let create_new_vdi ~rpc ~session_id ~sR ~vdi =
  let virtual_size =
    Client.Client.VDI.get_virtual_size ~rpc ~session_id ~self:vdi
  in
  let new_vdi =
    Client.Client.VDI.create ~rpc ~session_id ~name_label:""
      ~name_description:"" ~sR ~virtual_size ~_type:`user ~sharable:false
      ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[]
  in
  let new_vdi_uuid =
    Client.Client.VDI.get_uuid ~rpc ~session_id ~self:new_vdi
  in
  (new_vdi_uuid, new_vdi)

let import_file_into_vdi ~file ~vdi_uuid ~exportformat =
  Qt.cli_cmd
    [
      "vdi-import"
    ; "uuid=" ^ vdi_uuid
    ; "filename=" ^ file
    ; "format=" ^ exportformat
    ]
  |> ignore

let export_import_vdi rpc session_id ~exportformat sR vdi =
  let file = export_vdi_to_file ~rpc ~session_id ~exportformat ~vdi () in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      let new_vdi_uuid, new_vdi = create_new_vdi ~rpc ~session_id ~sR ~vdi in
      import_file_into_vdi ~file ~vdi_uuid:new_vdi_uuid ~exportformat ;
      new_vdi
    )
    (fun () -> Sys.remove file)

let export_delta_import_vdi rpc session_id ~exportformat ~vdi ~base_vdi =
  let file =
    export_vdi_to_file ~rpc ~session_id ~exportformat ~vdi ~base_vdi ()
  in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      (* Import delta on top of base_vdi *)
      let base_uuid =
        Client.Client.VDI.get_uuid ~rpc ~session_id ~self:base_vdi
      in
      import_file_into_vdi ~file ~vdi_uuid:base_uuid ~exportformat
    )
    (fun () -> Sys.remove file)

let export_import_raw = export_import_vdi ~exportformat:"raw"

let export_import_vhd = export_import_vdi ~exportformat:"vhd"

let export_import_tar = export_import_vdi ~exportformat:"tar"

let export_import_qcow = export_import_vdi ~exportformat:"qcow2"

let delta_export_import_vhd = export_delta_import_vdi ~exportformat:"vhd"

let delta_export_import_qcow = export_delta_import_vdi ~exportformat:"qcow2"

let data_integrity_tests vdi_op op_name =
  [
    ( op_name ^ ": small empty VDI"
    , `Slow
    , check_vdi_unchanged ~vdi_size:Sizes.(4L ** mib) ~prepare_vdi:noop ~vdi_op
    )
  ; ( op_name ^ ": small random VDI"
    , `Slow
    , check_vdi_unchanged
        ~vdi_size:Sizes.(4L ** mib)
        ~prepare_vdi:write_random_data ~vdi_op
    )
  ; ( op_name ^ ": small full VDI"
    , `Slow
    , check_vdi_unchanged ~vdi_size:Sizes.(4L ** mib) ~prepare_vdi:fill ~vdi_op
    )
  ]

let delta_data_integrity_tests vdi_op op_name =
  [
    ( op_name ^ ": delta between empty & empty VDI"
    , `Slow
    , check_vdi_delta
        ~vdi_size:Sizes.(4L ** mib)
        ~prepare_vdi:noop ~prepare_vdi_base:noop ~vdi_op
    )
  ; ( op_name ^ ": delta between random & empty VDI"
    , `Slow
    , check_vdi_delta
        ~vdi_size:Sizes.(4L ** mib)
        ~prepare_vdi:write_random_data ~prepare_vdi_base:noop ~vdi_op
    )
  ; ( op_name ^ ": delta between random & random VDI"
    , `Slow
    , check_vdi_delta
        ~vdi_size:Sizes.(4L ** mib)
        ~prepare_vdi:write_random_data ~prepare_vdi_base:write_random_data
        ~vdi_op
    )
  ; ( op_name ^ ": delta between full and empty VDI"
    , `Slow
    , check_vdi_delta
        ~vdi_size:Sizes.(4L ** mib)
        ~prepare_vdi:fill ~prepare_vdi_base:noop ~vdi_op
    )
  ; ( op_name ^ ": delta between full and random VDI"
    , `Slow
    , check_vdi_delta
        ~vdi_size:Sizes.(4L ** mib)
        ~prepare_vdi:fill ~prepare_vdi_base:write_random_data ~vdi_op
    )
  ; ( op_name ^ ": delta between full and full VDI"
    , `Slow
    , check_vdi_delta
        ~vdi_size:Sizes.(4L ** mib)
        ~prepare_vdi:fill ~prepare_vdi_base:fill ~vdi_op
    )
  ]

let large_data_integrity_tests vdi_op op_name =
  let b = Random.int64 16L in
  [
    ( op_name ^ ": ~2GiB empty VDI"
    , `Slow
    , check_vdi_unchanged
        ~vdi_size:Sizes.((2L ** gib) +* b)
        ~prepare_vdi:noop ~vdi_op
    )
  ; ( op_name ^ ": ~2GiB random VDI"
    , `Slow
    , check_vdi_unchanged
        ~vdi_size:Sizes.((2L ** gib) +* b)
        ~prepare_vdi:write_random_data ~vdi_op
    )
  ]

let sr_with_vdi_create_destroy =
  Qt_filter.SR.(all |> allowed_operations [`vdi_create; `vdi_destroy] |> not_iso)

let supported_srs test_case =
  let open Qt_filter in
  test_case |> conn |> sr sr_with_vdi_create_destroy

(* XXX Currently only GFS2 SRs support sparse reading of VDIs exported as TAR *)
let supported_gfs2_srs test_case =
  let open Qt_filter in
  test_case |> conn |> sr (sr_with_vdi_create_destroy |> SR.has_type "gfs2")

let tests () =
  (data_integrity_tests copy_vdi "VDI.copy" |> supported_srs)
  @ (large_data_integrity_tests copy_vdi "VDI.copy" |> supported_srs)
  @ (data_integrity_tests export_import_raw "VDI export/import to/from raw file"
    |> supported_srs
    )
  @ (data_integrity_tests export_import_vhd "VDI export/import to/from VHD file"
    |> supported_srs
    )
  @ (delta_data_integrity_tests delta_export_import_vhd
       "VDI delta export/import to/from VHD file"
    |> supported_srs
    )
  @ (data_integrity_tests export_import_tar "VDI export/import to/from TAR file"
    |> supported_srs
    )
  @ (data_integrity_tests export_import_qcow
       "VDI export/import to/from QCOW file"
    |> supported_srs
    )
  @ (delta_data_integrity_tests delta_export_import_qcow
       "VDI delta export/import to/from QCOW file"
    |> supported_srs
    )
  @ (large_data_integrity_tests export_import_tar
       "VDI export/import to/from TAR file"
    |> supported_gfs2_srs
    )
