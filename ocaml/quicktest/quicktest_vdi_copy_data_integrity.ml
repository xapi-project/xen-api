
(* ---------------- *
    Helper functions
 * ---------------- *)

let with_test test f =
  Quicktest_common.start test;
  try
    f ();
    Quicktest_common.success test
  with e ->
    Quicktest_common.failed test (Printexc.to_string e);
    raise e

let finally f cleanup =
  let result =
    try
      f ()
    with e -> begin
        cleanup ();
        raise e
      end
  in
  cleanup ();
  result

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
  finally
    (fun () ->
       Client.Client.VBD.plug ~rpc ~session_id ~self:vbd;
       finally
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
       finally
         (fun () -> f fd)
         (fun () -> Unix.close fd)
    )

(* ---------------- *
   Integrity tests
 * ---------------- *)

let write_random_data rpc session_id vdi =
  let size = Client.Client.VDI.get_virtual_size ~rpc ~session_id ~self:vdi in
  with_open_vdi rpc session_id vdi `RW
    (fun fd ->

       let max_writes = 100 in
       let max_write_length = 2048 in

       let write_random_char_block offset length =
         let buf =
           let c = Random.int 256 |> Char.chr in
           Bytes.make length c
         in
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

         write_random_char_block offset length
       in

       let num_extents = Random.int max_writes in
       for i = 1 to num_extents do
         write_random_extent ()
       done
    )

let fill rpc session_id vdi =
  let size =
    Client.Client.VDI.get_virtual_size ~rpc ~session_id ~self:vdi
    |> Int64.to_int
  in
  with_open_vdi rpc session_id vdi `RW
    (fun fd ->
       let buf = Bytes.make size 'a' in
       Unix.write fd buf 0 size
    )

let checksum rpc session_id vdi =
  with_attached_vdi rpc session_id vdi `RO (fun path ->
      Digest.to_hex (Digest.file path)
    )

let check_vdi_unchanged test rpc session_id sR ~prepare_vdi =
  with_test test (fun () ->
      let vdi = Client.Client.VDI.create ~rpc ~session_id ~name_label:"" ~name_description:"" ~sR ~virtual_size:4194304L ~_type:`user ~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in
      finally
        (fun () ->
           prepare_vdi vdi;
           let checksum_original = checksum rpc session_id vdi in
           let copy = Client.Client.VDI.copy ~rpc ~session_id
               ~vdi
               ~base_vdi:API.Ref.null
               ~into_vdi:API.Ref.null
               ~sr:sR
           in
           finally
             (fun () ->
                let checksum_copy = checksum rpc session_id vdi in
                if (checksum_copy <> checksum_original) then
                  failwith (Printf.sprintf "VDI copy (checksum: %s) has different data than original (checksum: %s)." checksum_copy checksum_original)
             )
             (fun () -> Client.Client.VDI.destroy ~rpc ~session_id ~self:copy)
        )
        (fun () -> Client.Client.VDI.destroy ~rpc ~session_id ~self:vdi)
    )

let test_sr rpc session_id sr =
  let sr_name = Client.Client.SR.get_name_label ~rpc ~session_id ~self:sr in
  let test = Quicktest_common.make_test ("VDI.copy on SR [" ^ sr_name ^ "]") 4 in
  with_test test (fun () ->
      let test_nodata = Quicktest_common.make_test "Copy of empty VDI" 6 in
      check_vdi_unchanged test_nodata rpc session_id sr
        ~prepare_vdi:(fun _vdi -> ());

      let test_random = Quicktest_common.make_test "Copy of random VDI" 6 in
      check_vdi_unchanged test_random rpc session_id sr
        ~prepare_vdi:(write_random_data rpc session_id);

      let test_full = Quicktest_common.make_test "Copy of full VDI" 6 in
      check_vdi_unchanged test_full rpc session_id sr
        ~prepare_vdi:(fill rpc session_id)
    )

let test session_id =
  let test = Quicktest_common.make_test "Checking VDI.copy data integrity" 2 in
  with_test test (fun () ->
      let rpc = !Quicktest_common.rpc in

      Quicktest_storage.list_srs session_id
      |> List.filter
        (fun sR ->
           let ops = Client.Client.SR.get_allowed_operations ~session_id ~rpc ~self:sR in
           let required = [`vdi_create; `vdi_destroy] in
           List.for_all (fun op -> List.mem op ops) required
        )
      (* VDI creation on ISO SRs doesn't work in this test *)
      |> List.filter (fun self -> Client.Client.SR.get_type ~session_id ~rpc ~self <> "iso")
      |> List.iter (test_sr rpc session_id);
    )
