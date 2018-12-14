
(** From XenServer 7.5 Configuration Limits:
    https://docs.citrix.com/content/dam/docs/en-us/xenserver/current-release/downloads/xenserver-config-limits.pdf
    Using (2TiB - 4GiB), instead of the (2TB - 4GB) limit defined in the above
    document, does not work, we cannot create a VDI of that size. *)
let max_vdi_size = Sizes.((2L ** tb) -* (4L ** gb))

let with_max_vdi rpc session_id sr f =
  Qt.VDI.with_new rpc session_id ~virtual_size:max_vdi_size sr
    (fun vdi ->
       (* We write some data to the very end of the VDI to ensure the IO code
          gets tested with large offsets *)
       Qt.VDI.with_open rpc session_id vdi `RW
         (fun fd ->
            let data = Bytes.make 1024 't' in
            let offset = Int64.sub max_vdi_size 1024L in
            assert (Unix.LargeFile.lseek fd offset Unix.SEEK_SET = offset);
            assert (Unix.write fd data 0 1024 = 1024)
         );
       f rpc session_id sr vdi
    )

let test_copy rpc session_id sr_info () =
  let sr = sr_info.Qt.sr in
  with_max_vdi rpc session_id sr
    (fun rpc session_id sr vdi ->
       Qt.VDI.with_destroyed rpc session_id
         (Client.Client.VDI.copy ~rpc ~session_id ~vdi ~base_vdi:API.Ref.null ~into_vdi:API.Ref.null ~sr)
         (fun () -> ())
    )

(** This does not reproduce CA-292288 *)
let test_export_import rpc session_id sr_info () =
  (* We only want to export to a sparse VHD, not to a fully-inflated raw file *)
  let format = "vhd" in
  let sR = sr_info.Qt.sr in
  with_max_vdi rpc session_id sR
    (fun rpc session_id sr vdi ->
       let virtual_size = Client.Client.VDI.get_virtual_size ~rpc ~session_id ~self:vdi in
       let vdi_uuid = Client.Client.VDI.get_uuid ~rpc ~session_id ~self:vdi in
       let file = "/tmp/quicktest_export_"^vdi_uuid in
       Qt.cli_cmd ["vdi-export"; "uuid="^vdi_uuid; "filename="^file; "format="^format] |> ignore;
       Xapi_stdext_pervasives.Pervasiveext.finally
         (fun () ->
            Qt.VDI.with_new rpc session_id ~virtual_size sR (fun new_vdi ->
                let new_vdi_uuid = Client.Client.VDI.get_uuid ~rpc ~session_id ~self:new_vdi in
                Qt.cli_cmd ["vdi-import"; "uuid="^new_vdi_uuid; "filename="^file; "format="^format] |> ignore
              )
         )
         (fun () -> Sys.remove file)
    )

let tests () =
  let open Qt_filter in
  [ [ "VDI.copy", `Slow, test_copy ] |> conn |> sr SR.(all |> allowed_operations [`vdi_create; `vdi_destroy] |> thin_pro)
  ; ["VDI.export/import to/from VHD file", `Slow, test_export_import]
    (* GFS2 would export a fully-inflated VHD file, not a sparse one *)
    |> conn |> sr SR.(all |> allowed_operations [`vdi_create; `vdi_destroy] |> thin_pro |> not_type "gfs2")
  ]
  |> List.concat
